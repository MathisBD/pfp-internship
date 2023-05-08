module KernelGen (
  generateBP, generateBPIter
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

-- Generate the CUDA code for a kernel that performs the given bit-permutation.
-- The parameters are :
-- --^ p : (log) size we need for coalescing, typically 4 or 5.
generateBP :: P.Perm -> Int -> String
generateBP perm p = unlines $
  [ comment $ "size of input and output arrays = 2^n"
  , comment $ "thread group count = (2^(n-p-q), 1, 1)  group size = (2^p, 2^q, 1)"
  , comment $ "n = " <> show n <> "  p = " <> show p <> "  q = " <> show q
  , comment $ "permutation = " <> show perm
  , "__global__ void BP_kernel(const int* " <> v_in <> ", int* " <> v_out <> ")"
  , "{"
  ] ++
  map indent body_lines
  ++
  [ "}" ]
  where n = P.size perm 
        q = tally [p..n-1] $ \i -> P.apply perm i < p
        v_in = "in_vect"
        v_out = "out_vect"
        v_block = "block"
        v_g = "g"
        v_i = "i"
        v_j = "j"
        v_in_addr = "in_addr"
        v_out_addr = "out_addr"
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
          [ comment "Read the input block" ] ++ 
          input_addr_lines ++
          [ v_block <> "[" <> v_i <> " * " <> show (2^p) <> " + " <> v_j <> "] = " <> 
              v_in <> "[" <> v_in_addr <> "];"
          ]
        output_lines = 
          [ comment "Write the output block"
          ] ++
          output_addr_code ++
          [ v_out <> "[" <> v_out_addr <> "] = " <> 
              v_block <> "[" <> v_j <> " * " <> show (2^q) <> " + " <> v_i <> "];"
          ]
        input_addr_lines = 
          ("size_t " <> v_in_addr <> " = " <> v_j <> ";") : 
          map generateAssign (mergeAssigns $ go p 0 0)
          where -- Stitch the input bits together 
                go addr_idx i_idx g_idx
                  | addr_idx >= n = []
                  | P.apply perm addr_idx < p = 
                      (v_in_addr, addr_idx, v_i, i_idx, [0]) : go (addr_idx + 1) (i_idx + 1) g_idx
                  | otherwise =
                      (v_in_addr, addr_idx, v_g, g_idx, [0]) : go (addr_idx + 1) i_idx (g_idx + 1)
        output_addr_code = 
          ("size_t " <> v_out_addr <> " = 0;") : 
          map generateAssign (mergeAssigns $ go 0 0 0 0)
          where -- Stitch the output bits together 
                go addr_idx i_idx j_idx g_idx
                  | addr_idx >= n = []
                  | addr_idx < q =
                      (v_out_addr, P.apply perm addr_idx, v_i, i_idx, [0]) : go (addr_idx + 1) (i_idx + 1) j_idx g_idx
                  | P.apply perm addr_idx < p = 
                      (v_out_addr, P.apply perm addr_idx, v_j, j_idx, [0]) : go (addr_idx + 1) i_idx (j_idx + 1) g_idx
                  | otherwise =
                      (v_out_addr, P.apply perm addr_idx, v_g, g_idx, [0]) : go (addr_idx + 1) i_idx j_idx (g_idx + 1)
        
bitIdxsToInteger :: [Int] -> Integer
bitIdxsToInteger is = foldl (.|.) 0 $ map (shiftL 1) is


-- Generate the CUDA code for a kernel that performs the given bit-permutation.
-- The parameters are :
-- --^ p : (log) size we need for coalescing, typically 4 or 5.
-- --^ iters : (log) number of iterations each thread will execute. 
generateBPIter :: P.Perm -> Int -> Int -> String
generateBPIter perm p iters0 = unlines $
  [ comment $ "size of input and output arrays = 2^n"
  , comment $ "thread group count = (2^(n-p-q+iters), 1, 1)  group size = (2^p, 2^(q-iters), 1)"
  , comment $ "n = " <> show n <> "  p = " <> show p <> "  q = " <> show q <> "  iters = " <> show iters
  , comment $ "permutation = " <> show perm
  , "__global__ void BP_kernel(const int* " <> v_in <> ", int* " <> v_out <> ")"
  , "{"
  ] ++
  map indent body_lines
  ++
  [ "}" ]
  where n = P.size perm 
        q = tally [p..n-1] $ \i -> P.apply perm i < p
        iters = min iters0 q
        v_in = "in_vect"
        v_out = "out_vect"
        v_block = "block"
        v_g = "g"
        v_i = "i"
        v_iter = "iter"
        v_j = "j"
        v_in_addr = "in_addr"
        v_out_addr = "out_addr"
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
          , "size_t " <> v_in_addr <> " = " <> v_j <> ";"
          ] ++
          map generateAssign outer_input_assigns ++
          [ "for (int " <> v_iter <> " = 0; " <> v_iter <> " < " <> show (2^iters) <> "; " <> v_iter <> "++) {" 
          , indent $ v_in_addr <> " &= ~" <> show in_addr_mask <> "ULL;"
          ] ++
          map (indent . generateAssign) inner_input_assigns ++
          [ indent $ v_block <> "[" <> 
              v_i <> " * " <> show (2^(p+iters)) <> " + " <> v_iter <> " * " <> show (2^p) <> " + " <> v_j <> 
              "] = " <> v_in <> "[" <> v_in_addr <> "];"
          , "}"
          ]
        output_lines = 
          [ comment "Write the output block"
          , "size_t " <> v_out_addr <> " = 0;"
          ] ++
          map generateAssign outer_output_assigns ++
          [ "for (int " <> v_iter <> " = 0; " <> v_iter <> " < " <> show (2^iters) <> "; " <> v_iter <> "++) {" 
          , indent $ v_out_addr <> " &= ~" <> show out_addr_mask <> "ULL;"
          ] ++
          map (indent . generateAssign) inner_output_assigns ++
          [ indent $ v_out <> "[" <> v_out_addr <> "] = " <> v_block <> "[" <> 
              v_j <> " * " <> show (2^q) <> " + " <> v_i <> " * " <> show (2^iters) <> " + " <> v_iter <> "];"
          , "}"
          ]
        input_assigns = mergeAssigns $ go p 0 0
          where -- Stitch the input bits together 
                go addr_idx i_idx g_idx
                  | addr_idx >= n = []
                  | P.apply perm addr_idx < p && i_idx < iters = 
                      (v_in_addr, addr_idx, v_iter, i_idx, [0]) : go (addr_idx + 1) (i_idx + 1) g_idx
                  | P.apply perm addr_idx < p && i_idx >= iters = 
                      (v_in_addr, addr_idx, v_i, i_idx - iters, [0]) : go (addr_idx + 1) (i_idx + 1) g_idx
                  | otherwise =
                      (v_in_addr, addr_idx, v_g, g_idx, [0]) : go (addr_idx + 1) i_idx (g_idx + 1)
        (inner_input_assigns, outer_input_assigns) = 
          partition (\(_, _, v_in, _, _) -> v_in == v_iter) input_assigns
        in_addr_mask = concatMap (\(_, out_idx, _, _, offsets) -> map (+ out_idx) offsets) inner_input_assigns
                     & bitIdxsToInteger
        output_assigns = mergeAssigns $ go 0 0 0 0
          where -- Stitch the output bits together 
                go addr_idx i_idx j_idx g_idx
                  | addr_idx >= n = []
                  | addr_idx < q && i_idx < iters =
                      (v_out_addr, P.apply perm addr_idx, v_iter, i_idx, [0]) : go (addr_idx + 1) (i_idx + 1) j_idx g_idx
                  | addr_idx < q && i_idx >= iters =
                      (v_out_addr, P.apply perm addr_idx, v_i, i_idx - iters, [0]) : go (addr_idx + 1) (i_idx + 1) j_idx g_idx
                  | P.apply perm addr_idx < p = 
                      (v_out_addr, P.apply perm addr_idx, v_j, j_idx, [0]) : go (addr_idx + 1) i_idx (j_idx + 1) g_idx
                  | otherwise =
                      (v_out_addr, P.apply perm addr_idx, v_g, g_idx, [0]) : go (addr_idx + 1) i_idx j_idx (g_idx + 1)
        (inner_output_assigns, outer_output_assigns) = 
          partition (\(_, _, v_in, _, _) -> v_in == v_iter) output_assigns
        out_addr_mask = concatMap (\(_, out_idx, _, _, offsets) -> map (+ out_idx) offsets) inner_output_assigns
                     & bitIdxsToInteger
        