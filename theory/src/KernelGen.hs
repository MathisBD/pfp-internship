module KernelGen (
  KernelGen.generate, generateC, generateCB, generateCI, generateCBI,
  generateBmmcC
) where

import qualified Bmmc as B
import qualified Perm as P 
import Control.Monad ( msum )
import Data.Maybe ( fromMaybe )
import Data.List ( sort, sortOn, partition )
import Data.List.Unique ( sortUniq )
import Data.Function ( (&) )
import Data.Bits ( (.|.), shiftR, shiftL )
import Data.Vector (generate)
import Control.Exception ( assert )


tally :: [a] -> (a -> Bool) -> Int
tally xs f = sum $ map (b2i . f) xs
  where b2i False = 0
        b2i True  = 1
        
comment :: String -> String
comment str = "// " ++ str

indent :: String -> String
indent str = "    " ++ str

removeIndices :: [Int] -> [a] -> [a]
removeIndices is xs = go 0 (sortUniq is) xs
  where go k [] xs = xs
        go k (i:is) (x:xs) 
          | i < k     = undefined
          | i == k    = go (k+1) is xs
          | otherwise = x : go (k+1) (i:is) xs

mergeAssigns :: [(String, Int, String, Int, [Int])] -> [(String, Int, String, Int, [Int])]
mergeAssigns xs = maybe xs mergeAssigns (msum $ map (uncurry $ tryMerge xs) ijs)
                & sortOn (\(v_out, out_idx, v_in, in_idx, offsets) -> (v_in, in_idx))
  where ijs = do 
          i <- [0..length xs - 1]
          j <- [0..length xs - 1]
          pure (i, j)
        tryMerge xs i j
          | v_out1 == v_out2 && v_in1 == v_in2 && delta_in == delta_out && delta_in > 0 =
              Just $ x : removeIndices [i, j] xs
          | otherwise = Nothing
          where (v_out1, out_idx1, v_in1, in_idx1, offsets1) = xs !! i
                (v_out2, out_idx2, v_in2, in_idx2, offsets2) = xs !! j
                delta_out = out_idx2 - out_idx1
                delta_in = in_idx2 - in_idx1
                x = (v_out1, out_idx1, v_in1, in_idx1, offsets1 ++ map (+ delta_in) offsets2)

generateAssign :: (String, Int, String, Int, [Int]) -> String
generateAssign (v_out, out_idx, v_in, in_idx, offsets) = 
  v_out <> " |= (" <> v_in <> " & " <> show mask <> ")" <> shift_code <> ";" 
  where mask = bitIdxsToInteger offsets `shiftL` in_idx
        shift_code 
          | in_idx < out_idx  = " << " <> show (out_idx - in_idx)
          | in_idx == out_idx = ""
          | in_idx > out_idx  = " >> " <> show (in_idx - out_idx)
        
bitIdxsToInteger :: [Int] -> Integer
bitIdxsToInteger is = foldl (.|.) 0 $ map (shiftL 1) is

genOutAddrNaive n perm v_out_addr v_in_addr = go 0
  where go idx
          | idx >= n = []
          | otherwise =
              (v_out_addr, P.apply perm idx, v_in_addr, idx, [0]) : go (idx+1)

genInAddrBasic n p q cols v_in_addr v_i v_j v_g = go 0 0 0 0
  -- Stitch the input bits together 
  where go addr_idx i_idx j_idx g_idx
          | addr_idx >= n = []
          -- Take bit from j
          | addr_idx < p =
              (v_in_addr, addr_idx, v_j, j_idx, [0]) : go (addr_idx+1) i_idx (j_idx+1) g_idx
          -- Take bit from i
          | addr_idx `elem` cols = 
              (v_in_addr, addr_idx, v_i, i_idx, [0]) : go (addr_idx+1) (i_idx + 1) j_idx g_idx
          -- Take bit from g
          | otherwise =
              (v_in_addr, addr_idx, v_g, g_idx, [0]) : go (addr_idx + 1) i_idx j_idx (g_idx+1)

genInAddrIter n p q iters cols v_in_addr v_i v_j v_g v_iter = go 0 0 0 0 0
  -- Stitch the input bits together 
  -- Compared to the basic version, here the bits of iter take 
  -- the place of the lower bits of g.
  where go addr_idx i_idx j_idx g_idx iter_idx
          | addr_idx >= n = []
          -- Take bit from j
          | addr_idx < p =
              (v_in_addr, addr_idx, v_j, j_idx, [0]) : go (addr_idx+1) i_idx (j_idx+1) g_idx iter_idx
          -- Take bit from i
          | addr_idx `elem` cols = 
              (v_in_addr, addr_idx, v_i, i_idx, [0]) : go (addr_idx+1) (i_idx + 1) j_idx g_idx iter_idx
          -- Take bit from iter
          | iter_idx < iters =
              (v_in_addr, addr_idx, v_iter, iter_idx, [0]) : go (addr_idx + 1) i_idx j_idx g_idx (iter_idx+1)
          -- Take bit from g
          | otherwise =
              (v_in_addr, addr_idx, v_g, g_idx, [0]) : go (addr_idx + 1) i_idx j_idx (g_idx+1) iter_idx

genOutAddrBasic n p q cols v_out_addr v_i v_j v_g = go 0 0 0 0 
  where -- Stitch the output bits together 
        go addr_idx i_idx j_idx g_idx
          | addr_idx >= n = []
          -- take bit from j
          | addr_idx `elem` cols = 
              (v_out_addr, addr_idx, v_j, j_idx, [0]) : go (addr_idx + 1) i_idx (j_idx + 1) g_idx
          -- Take bit from i
          | addr_idx < p =
              (v_out_addr, addr_idx, v_i, i_idx, [0]) : go (addr_idx + 1) (i_idx + 1) j_idx g_idx
          -- Take bit from g
          | otherwise =
              (v_out_addr, addr_idx, v_g, g_idx, [0]) : go (addr_idx + 1) i_idx j_idx (g_idx + 1)

genOutAddrIter n p q iters cols v_out_addr v_i v_j v_g v_iter = go 0 0 0 0 0
  where -- Stitch the output bits together 
        go addr_idx i_idx j_idx g_idx iter_idx
          | addr_idx >= n = []
          -- take bit from j
          | addr_idx `elem` cols = 
              (v_out_addr, addr_idx, v_j, j_idx, [0]) : go (addr_idx + 1) i_idx (j_idx + 1) g_idx iter_idx
          -- Take bit from i
          | addr_idx < p =
              (v_out_addr, addr_idx, v_i, i_idx, [0]) : go (addr_idx + 1) (i_idx + 1) j_idx g_idx iter_idx
          -- Take bit from iter
          | iter_idx < iters =
              (v_out_addr, addr_idx, v_iter, iter_idx, [0]) : go (addr_idx + 1) i_idx j_idx g_idx (iter_idx+1)
          -- Take bit from g
          | otherwise =
              (v_out_addr, addr_idx, v_g, g_idx, [0]) : go (addr_idx + 1) i_idx j_idx (g_idx + 1) iter_idx


genIBlockAddrBasic n p q cols v_iblock_addr v_i v_j = go 0 0 0
  where go addr_idx i_idx j_idx 
          | addr_idx >= p + q = []
          -- Take bit from j
          | addr_idx < p = 
              (v_iblock_addr, addr_idx, v_j, j_idx, [0]) : go (addr_idx + 1) i_idx (j_idx+1)
          -- Take bit from i
          | otherwise = 
              (v_iblock_addr, addr_idx, v_i, i_idx, [0]) : go (addr_idx + 1) (i_idx+1) j_idx

genOBlockAddrBasic n p q cols v_oblock_addr v_i v_j = go 0 0 0
  where go addr_idx i_idx j_idx 
          | addr_idx >= p + q = []
          -- Take bit from j
          | addr_idx `elem` cols || addr_idx >= p = 
              (v_oblock_addr, addr_idx, v_j, j_idx, [0]) : go (addr_idx + 1) i_idx (j_idx+1)
          -- Take bit from i
          | otherwise = 
              (v_oblock_addr, addr_idx, v_i, i_idx, [0]) : go (addr_idx + 1) (i_idx+1) j_idx

genShift n p q cols v_shift v_block_addr = go 0 0
  where go addr_idx row_idx
          | row_idx >= q = []
          | addr_idx `elem` cols = 
              go (addr_idx+1) row_idx
          | otherwise = 
              (v_shift, addr_idx, v_block_addr, row_idx + p, [0]) : go (addr_idx+1) (row_idx+1)

-- Variable names used in every kernel
v_in, v_out, v_block, v_g, v_i, v_j, v_in_addr, v_out_addr :: String
v_iblock_addr, v_oblock_addr :: String
v_in = "in_vect"
v_out = "out_vect"
v_block = "block"
v_g = "g"
v_i = "i"
v_j = "j"
v_in_addr = "in_addr"
v_out_addr = "out_addr"
v_iblock_addr = "iblock_addr"
v_oblock_addr = "oblock_addr"

-- This is the most naive kernel.
generate :: String -> P.Perm -> String
generate name perm = unlines $
  [ comment $ "size of input and output arrays = 2^n"
  , comment $ "thread group count = (2^n / group_size, 1, 1)  group size = (group_size, 1, 1)"
  , comment $ "n = " <> show n  
  , comment $ "permutation = " <> show perm
  , "__global__ void " <> kernel_name <> "(const int* " <> v_in <> ", int* " <> v_out <> ")"
  , "{"
  ] ++
  map indent body_lines
  ++
  [ "}" ]
  where n = P.size perm 
        p = 10
        kernel_name = "generate_n" <> show n <> "_" <> name
        body_lines =
          [ "size_t " <> v_in_addr <> " = blockIdx.x * blockDim.x + threadIdx.x;"
          , ""
          , comment "Compute the output address"
          , "size_t " <> v_out_addr <> " = 0;"
          ] ++
          map generateAssign (mergeAssigns $ genOutAddrNaive n perm v_out_addr v_in_addr) ++
          [ v_out <> "[" <> v_out_addr <> "] = " <> v_in <> "[" <> v_in_addr <> "];" ]

-- Generate the CUDA code for a kernel that performs the given bit-permutation.
-- The parameters are :
-- --^ p : (log) size we need for coalescing, typically 4 or 5.
generateC :: String -> P.Perm -> Int -> String
generateC name perm p = unlines $
  [ comment $ "size of input and output arrays = 2^n"
  , comment $ "thread group count = (2^(n-p-q), 1, 1)  group size = (2^p, 2^q, 1)"
  , comment $ "n = " <> show n <> "  p = " <> show p <> "  q = " <> show q
  , comment $ "permutation = " <> show perm
  , comment $ "cols = " <> show cols
  , "__global__ void " <> kernel_name <> "(const int* " <> v_in <> ", int* " <> v_out <> ")"
  , "{"
  ] ++
  map indent body_lines
  ++
  [ "}" ]
  where n = P.size perm 
        cols = filter (\j -> P.apply perm j < p) [0..n-1]
        q = tally cols $ \j -> j >= p
        kernel_name = "generateC_n" <> show n <> "_" <> name
        body_lines =
          [ "__shared__ int " <> v_block <> "[" <> show (2^p * 2^q) <> "];"
          , "size_t " <> v_g <> " = blockIdx.x;"
          , "size_t " <> v_i <> " = threadIdx.y;"
          , "size_t " <> v_j <> " = threadIdx.x;"
          , ""
          ] ++
          input_lines ++
          [ ""
          , comment "Synchronize"
          , "__syncthreads();"
          , ""
          ] ++
          output_lines
        input_lines =  
          [ comment "Read the input block"
          , "size_t " <> v_in_addr <> " = 0;"
          , "size_t " <> v_iblock_addr <> " = 0;"
          ] ++ 
          map generateAssign (mergeAssigns $ genInAddrBasic n p q cols v_in_addr v_i v_j v_g) ++
          map generateAssign (mergeAssigns $ genIBlockAddrBasic n p q cols v_iblock_addr v_i v_j) ++
          [ v_block <> "[" <> v_iblock_addr <> "] = " <> v_in <> "[" <> v_in_addr <> "];"
          ]
        output_lines = 
          [ comment "Write the output block"
          , "size_t " <> v_out_addr <> " = 0;"
          , "size_t " <> v_oblock_addr <> " = 0;"
          ] ++
          map generateAssign (mergeAssigns $ 
            map (\(v_out, out_idx, v_in, in_idx, ofs) -> (v_out, P.apply perm out_idx, v_in, in_idx, ofs)) $
            genOutAddrBasic n p q cols v_out_addr v_i v_j v_g) ++
          map generateAssign (mergeAssigns $ genOBlockAddrBasic n p q cols v_oblock_addr v_i v_j) ++
          [ v_out <> "[" <> v_out_addr <> "] = " <> v_block <> "[" <> v_oblock_addr <> "];"
          ]

-- Generate the CUDA code for a kernel that performs the given bit-permutation.
-- The parameters are :
-- --^ p : (log) size we need for coalescing, typically 4 or 5.
-- This version should avoid bank conflicts
generateCB :: String -> P.Perm -> Int -> String
generateCB name perm p = unlines $
  [ comment $ "size of input and output arrays = 2^n"
  , comment $ "thread group count = (2^(n-p-q), 1, 1)  group size = (2^p, 2^q, 1)"
  , comment $ "n = " <> show n <> "  p = " <> show p <> "  q = " <> show q
  , comment $ "permutation = " <> show perm
  , "__global__ void " <> kernel_name <> "(const int* " <> v_in <> ", int* " <> v_out <> ")"
  , "{"
  ] ++
  map indent body_lines
  ++
  [ "}" ]
  where n = P.size perm 
        cols = filter (\j -> P.apply perm j < p) [0..n-1]
        q = tally cols $ \j -> j >= p
        v_ishift = "ishift"
        v_oshift = "oshift"
        kernel_name = "generateCB_n" <> show n <> "_" <> name
        body_lines =
          [ "__shared__ int " <> v_block <> "[" <> show (2^p * 2^q) <> "];"
          , "size_t " <> v_g <> " = blockIdx.x;"
          , "size_t " <> v_i <> " = threadIdx.y;"
          , "size_t " <> v_j <> " = threadIdx.x;"
          , ""
          ] ++
          input_lines ++
          [ ""
          , comment "Synchronize"
          , "__syncthreads();"
          , ""
          ] ++
          output_lines
        input_lines =  
          [ comment "Read the input block"
          , "size_t " <> v_in_addr <> " = 0;"
          , "size_t " <> v_iblock_addr <> " = 0;"
          , "size_t " <> v_ishift <> " = 0;"
          ] ++ 
          map generateAssign (mergeAssigns $ genInAddrBasic n p q cols v_in_addr v_i v_j v_g) ++
          map generateAssign (mergeAssigns $ genIBlockAddrBasic n p q cols v_iblock_addr v_i v_j) ++
          map generateAssign (mergeAssigns $ genShift n p q cols v_ishift v_iblock_addr) ++
          [ v_block <> "[" <> 
              "(" <> v_iblock_addr <> " & " <> show ((2^q-1) * 2^p) <> ") + " <> 
              "((" <> v_ishift <> " + " <> v_iblock_addr <> ") & " <> show (2^p-1) <> ")" <>
            "] = " <> v_in <> "[" <> v_in_addr <> "];"
          ]
        output_lines = 
          [ comment "Write the output block"
          , "size_t " <> v_out_addr <> " = 0;"
          , "size_t " <> v_oblock_addr <> " = 0;"
          , "size_t " <> v_oshift <> " = 0;"
          ] ++
          map generateAssign (mergeAssigns $ 
            map (\(v_out, out_idx, v_in, in_idx, ofs) -> (v_out, P.apply perm out_idx, v_in, in_idx, ofs)) $            
            genOutAddrBasic n p q cols v_out_addr v_i v_j v_g) ++
          map generateAssign (mergeAssigns $ genOBlockAddrBasic n p q cols v_oblock_addr v_i v_j) ++
          map generateAssign (mergeAssigns $ genShift n p q cols v_oshift v_oblock_addr) ++
          [ v_out <> "[" <> v_out_addr <> "] = " <> v_block <> "[" <> 
              "(" <> v_oblock_addr <> " & " <> show ((2^q-1) * 2^p) <> ") + " <> 
              "((" <> v_oshift <> " + " <> v_oblock_addr <> ") & " <> show (2^p-1) <> ")"
            <> "];"
          ]
-- Generate the CUDA code for a kernel that performs the given bit-permutation.
-- The parameters are :
-- --^ p : (log) size we need for coalescing, typically 4 or 5.
-- --^ iters : (log) number of iterations each thread will execute, typically between 0 and 4.
-- In this version the iter bits are taken from the lower bits of g. 
generateCI :: String -> P.Perm -> Int -> Int -> String
generateCI name perm p iters0 = unlines $
  [ comment $ "size of input and output arrays = 2^n"
  , comment $ "thread group count = (2^(n-p-q-iters), 1, 1)  group size = (2^p, 2^q, 1)"
  , comment $ "n = " <> show n <> "  p = " <> show p <> "  q = " <> show q <> "  iters = " <> show iters
  , comment $ "permutation = " <> show perm
  , "__global__ void " <> kernel_name <> "(const int* " <> v_in <> ", int* " <> v_out <> ")"
  , "{"
  ] ++
  map indent body_lines
  ++
  [ "}" ]
  where n = P.size perm 
        cols = filter (\j -> P.apply perm j < p) [0..n-1]
        q = tally cols $ \j -> j >= p
        -- The iteration bits have to fit into the original bits for g
        iters = min iters0 (n-p-q)
        v_iter = "iter"
        kernel_name = "generateCI_n" <> show n <> "_" <> name
        body_lines =
          [ "__shared__ int " <> v_block <> "[" <> show (2^iters * 2^p * 2^q) <> "];"
          , "size_t " <> v_g <> " = blockIdx.x;"
          , "size_t " <> v_i <> " = threadIdx.y;"
          , "size_t " <> v_j <> " = threadIdx.x;"
          , ""
          ] ++
          input_lines ++
          [ ""
          , comment "Synchronize"
          , "__syncthreads();"
          , ""
          ] ++
          output_lines
        input_lines =  
          [ comment "Read the input block"
          , "size_t " <> v_in_addr <> " = 0;"
          , "size_t " <> v_iblock_addr <> " = 0;"
          ] ++ 
          map generateAssign (mergeAssigns $ genIBlockAddrBasic n p q perm v_iblock_addr v_i v_j) ++
          map generateAssign outer_input_assigns ++
          [ "for (size_t " <> v_iter <> " = 0; " <> v_iter <> " < " <> show (2^iters) <> "; " <> v_iter <> "++) {" 
          , indent $ v_in_addr <> " &= ~" <> show in_addr_mask <> "ULL;"
          ] ++
          map (indent . generateAssign) inner_input_assigns ++
          [ indent $ v_block <> "[" <> 
              "(" <> v_iter <> " << " <> show (p + q) <> ") + " <> v_iblock_addr <>
            "] = " <> v_in <> "[" <> v_in_addr <> "];"
          , "}"
          ]
        (inner_input_assigns, outer_input_assigns) = 
          partition (\(_, _, v_in, _, _) -> v_in == v_iter) 
            (mergeAssigns $ genInAddrIter n p q iters cols v_in_addr v_i v_j v_g v_iter)
        in_addr_mask = concatMap (\(_, out_idx, _, _, offsets) -> map (+ out_idx) offsets) inner_input_assigns
                     & bitIdxsToInteger
        output_lines = 
          [ comment "Write the output block"
          , "size_t " <> v_out_addr <> " = 0;"
          , "size_t " <> v_oblock_addr <> " = 0;"
          ] ++
          map generateAssign (mergeAssigns $ genOBlockAddrBasic n p q cols v_oblock_addr v_i v_j) ++
          map generateAssign outer_output_assigns ++
          [ "for (size_t " <> v_iter <> " = 0; " <> v_iter <> " < " <> show (2^iters) <> "; " <> v_iter <> "++) {" 
          , indent $ v_out_addr <> " &= ~" <> show out_addr_mask <> "ULL;"
          ] ++
          map (indent . generateAssign) inner_output_assigns ++
          [ indent $ v_out <> "[" <> v_out_addr <> "] = " <> v_block <> "[" <> 
              "(" <> v_iter <> " << " <> show (p + q) <> ") + " <> v_oblock_addr
            <> "];"
          , "}"
          ]
        (inner_output_assigns, outer_output_assigns) = 
          partition (\(_, _, v_in, _, _) -> v_in == v_iter) 
            (mergeAssigns $ 
             map (\(v_out, out_idx, v_in, in_idx, ofs) -> (v_out, P.apply perm out_idx, v_in, in_idx, ofs)) $            
             genOutAddrIter n p q iters cols v_out_addr v_i v_j v_g v_iter)
        out_addr_mask = concatMap (\(_, out_idx, _, _, offsets) -> map (+ out_idx) offsets) inner_output_assigns
                     & bitIdxsToInteger

              

-- Generate the CUDA code for a kernel that performs the given bit-permutation.
-- The parameters are :
-- --^ p : (log) size we need for coalescing, typically 4 or 5.
-- --^ iters : (log) number of iterations each thread will execute, typically between 0 and 4.
-- In this version the iter bits are taken from the lower bits of g. 
generateCBI :: String -> P.Perm -> Int -> Int -> String
generateCBI name perm p iters0 = unlines $
  [ comment $ "size of input and output arrays = 2^n"
  , comment $ "thread group count = (2^(n-p-q-iters), 1, 1)  group size = (2^p, 2^q, 1)"
  , comment $ "n = " <> show n <> "  p = " <> show p <> "  q = " <> show q <> "  iters = " <> show iters
  , comment $ "permutation = " <> show perm
  , "__global__ void " <> kernel_name <> "(const int* " <> v_in <> ", int* " <> v_out <> ")"
  , "{"
  ] ++
  map indent body_lines
  ++
  [ "}" ]
  where n = P.size perm 
        cols = filter (\j -> P.apply perm j < p) [0..n-1]
        q = tally cols $ \j -> j >= p
        -- The iteration bits have to fit into the original bits for g
        iters = min iters0 (n-p-q)
        v_ishift = "ishift"
        v_oshift = "oshift"
        v_iter = "iter"
        kernel_name = "generateCBI_n" <> show n <> "_" <> name
        body_lines =
          [ "__shared__ int " <> v_block <> "[" <> show (2^iters * 2^p * 2^q) <> "];"
          , "size_t " <> v_g <> " = blockIdx.x;"
          , "size_t " <> v_i <> " = threadIdx.y;"
          , "size_t " <> v_j <> " = threadIdx.x;"
          , ""
          ] ++
          input_lines ++
          [ ""
          , comment "Synchronize"
          , "__syncthreads();"
          , ""
          ] ++
          output_lines
        input_lines =  
          [ comment "Read the input block"
          , "size_t " <> v_in_addr <> " = 0;"
          , "size_t " <> v_iblock_addr <> " = 0;"
          , "size_t " <> v_ishift <> " = 0;"
          ] ++ 
          map generateAssign (mergeAssigns $ genIBlockAddrBasic n p q cols v_iblock_addr v_i v_j) ++
          map generateAssign (mergeAssigns $ genShift n p q cols v_ishift v_iblock_addr) ++
          map generateAssign outer_input_assigns ++
          [ "for (size_t " <> v_iter <> " = 0; " <> v_iter <> " < " <> show (2^iters) <> "; " <> v_iter <> "++) {" 
          , indent $ v_in_addr <> " &= ~" <> show in_addr_mask <> "ULL;"
          ] ++
          map (indent . generateAssign) inner_input_assigns ++
          [ indent $ v_block <> "[" <> 
              "(" <> v_iter <> " << " <> show (p + q) <> ") + " <>
              "(" <> v_iblock_addr <> " & " <> show ((2^q-1) * 2^p) <> ") + " <> 
              "((" <> v_ishift <> " + " <> v_iblock_addr <> ") & " <> show (2^p-1) <> ")" <>
            "] = " <> v_in <> "[" <> v_in_addr <> "];"
          , "}"
          ]
        (inner_input_assigns, outer_input_assigns) = 
          partition (\(_, _, v_in, _, _) -> v_in == v_iter) 
            (mergeAssigns $ genInAddrIter n p q iters cols v_in_addr v_i v_j v_g v_iter)
        in_addr_mask = concatMap (\(_, out_idx, _, _, offsets) -> map (+ out_idx) offsets) inner_input_assigns
                     & bitIdxsToInteger
        output_lines = 
          [ comment "Write the output block"
          , "size_t " <> v_out_addr <> " = 0;"
          , "size_t " <> v_oblock_addr <> " = 0;"
          , "size_t " <> v_oshift <> " = 0;"
          ] ++
          map generateAssign (mergeAssigns $ genOBlockAddrBasic n p q cols v_oblock_addr v_i v_j) ++
          map generateAssign (mergeAssigns $ genShift n p q cols v_oshift v_oblock_addr) ++
          map generateAssign outer_output_assigns ++
          [ "for (size_t " <> v_iter <> " = 0; " <> v_iter <> " < " <> show (2^iters) <> "; " <> v_iter <> "++) {" 
          , indent $ v_out_addr <> " &= ~" <> show out_addr_mask <> "ULL;"
          ] ++
          map (indent . generateAssign) inner_output_assigns ++
          [ indent $ v_out <> "[" <> v_out_addr <> "] = " <> v_block <> "[" <> 
              "(" <> v_iter <> " << " <> show (p + q) <> ") + " <>
              "(" <> v_oblock_addr <> " & " <> show ((2^q-1) * 2^p) <> ") + " <> 
              "((" <> v_oshift <> " + " <> v_oblock_addr <> ") & " <> show (2^p-1) <> ")"
            <> "];"
          , "}"
          ]
        (inner_output_assigns, outer_output_assigns) = 
          partition (\(_, _, v_in, _, _) -> v_in == v_iter) 
            (mergeAssigns $
             map (\(v_out, out_idx, v_in, in_idx, ofs) -> (v_out, P.apply perm out_idx, v_in, in_idx, ofs)) $            
             genOutAddrIter n p q iters cols v_out_addr v_i v_j v_g v_iter)
        out_addr_mask = concatMap (\(_, out_idx, _, _, offsets) -> map (+ out_idx) offsets) inner_output_assigns
                     & bitIdxsToInteger
        
-- The parameters are :
-- --^ p : (log) size we need for coalescing, typically 4 or 5.
generateBmmcC :: String -> B.BMatrix -> Int -> String
generateBmmcC name mat p = unlines $
  [ comment $ "size of input and output arrays = 2^n"
  , comment $ "thread group count = (2^(n-p-q), 1, 1)  group size = (2^p, 2^q, 1)"
  , comment $ "n = " <> show n <> "  p = " <> show p <> "  q = " <> show q
  , comment $ "cols = " <> show cols
  --, comment $ "permutation = " <> show perm
  , "__global__ void " <> kernel_name <> "(const int* " <> v_in <> ", int* " <> v_out <> ")"
  , "{"
  ] ++
  map indent body_lines
  ++
  [ "}" ]
  where n = B.rows mat 
        -- The columns that are 'sparse'
        cols = assert (length cols0 >= p) $ take p cols0
          where cols0 = filter (\j -> all (\i -> not (B.get mat i j)) [p..n-1]) [0..n-1]
        q = tally cols $ \j -> j >= p
        kernel_name = "generateBmmcC_n" <> show n <> "_" <> name
        v_out_tmp = "out_tmp"
        body_lines =
          [ "__shared__ int " <> v_block <> "[" <> show (2^p * 2^q) <> "];"
          , "size_t " <> v_g <> " = blockIdx.x;"
          , "size_t " <> v_i <> " = threadIdx.y;"
          , "size_t " <> v_j <> " = threadIdx.x;"
          , ""
          ] ++
          input_lines ++
          [ ""
          , comment "Synchronize"
          , "__syncthreads();"
          , ""
          ] ++
          output_lines
        input_lines =  
          [ comment "Read the input block"
          , "size_t " <> v_in_addr <> " = 0;"
          , "size_t " <> v_iblock_addr <> " = 0;"
          ] ++ 
          map generateAssign (mergeAssigns $ genInAddrBasic n p q cols v_in_addr v_i v_j v_g) ++
          map generateAssign (mergeAssigns $ genIBlockAddrBasic n p q cols v_iblock_addr v_i v_j) ++
          [ v_block <> "[" <> v_iblock_addr <> "] = " <> v_in <> "[" <> v_in_addr <> "];"
          ]
        output_lines = 
          [ comment "Write the output block"
          , "size_t " <> v_out_tmp <> " = 0;"
          , "size_t " <> v_out_addr <> " = 0;"
          , "size_t " <> v_oblock_addr <> " = 0;"
          ] ++
          map generateAssign (mergeAssigns $ genOBlockAddrBasic n p q cols v_oblock_addr v_i v_j) ++
          map generateAssign (mergeAssigns $ genOutAddrBasic n p q cols v_out_tmp v_i v_j v_g) ++
          map (\i -> v_out_addr <> " |= (__popcll(" <> 
            show (B.rowToInt $ B.getRow mat i) <> " & " <> v_out_tmp <> ") & 1) << " <> show i <> ";") 
            [0..n-1] ++
          [ v_out <> "[" <> v_out_addr <> "] = " <> v_block <> "[" <> v_oblock_addr <> "];"
          ]
