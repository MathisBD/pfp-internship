module Main ( main ) where 

import Data.List ( sort, transpose, intersperse, intercalate )
import Data.List.Unique ( count )
import Control.Monad ( forM_, liftM2, replicateM )
import Data.Traversable ( for )
import System.Random ( randomIO, randomRIO )
import qualified Bmmc as B
import qualified Data.Vector.Unboxed as U
import qualified Perm as P
import KernelGen
import qualified Control.Category as P
import Text.Printf ( printf )


-- An arbitrary upper invertible matrix
randUpperInvMatrix :: Int -> IO B.BMatrix
randUpperInvMatrix n = do
  contents <- replicateM (n*n) (randomIO :: IO Bool)
  pure $ B.make n n $ \i j -> 
                if i == j then True 
                else if i < j then contents !! (i * n + j)
                else False

showBmmcCLike :: B.BMatrix -> String
showBmmcCLike a = "{ " ++ intercalate "\n, " rows ++ "\n};"
  where rows = map (\i -> "{ " ++ show (B.rowToInt $ B.getRow a i) ++ " }") [0..B.rows a - 1]

tally :: [a] -> (a -> Bool) -> Int
tally xs f = sum $ map (b2i . f) xs
  where b2i False = 0
        b2i True  = 1

avgLength :: [[a]] -> Double
avgLength [] = 0
avgLength xss = fromIntegral (sum $ map length xss) / fromIntegral (length xss)

--main :: IO ()
--main = do
--  let n = 30
--      p = 5
--      iters = 3
--      count = 10000
--      --perms = [P.reverse n]
--  perms <- replicateM count (P.generateRandom n)
--  let outAddrNaive = flip map perms $ \perm -> 
--        genOutAddrNaive n perm "v_out_addr" "v_in_addr"    
--      inAddrBasic = flip map perms $ \perm -> 
--          let cols = filter (\j -> P.apply perm j < p) [0..n-1]
--              q = tally cols $ \j -> j >= p
--          in genInAddrBasic n p q cols "v_in_addr" "v_i" "v_j" "v_g"
--      outAddrBasic = flip map perms $ \perm -> 
--          let cols = filter (\j -> P.apply perm j < p) [0..n-1]
--              q = tally cols $ \j -> j >= p
--          in map (\(v_out, out_idx, v_in, in_idx, ofs) -> (v_out, P.apply perm out_idx, v_in, in_idx, ofs)) $           
--             genOutAddrBasic n p q cols "v_out_addr" "v_i" "v_j" "v_g"
--      iblockAddrBasic = flip map perms $ \perm -> 
--          let cols = filter (\j -> P.apply perm j < p) [0..n-1]
--              q = tally cols $ \j -> j >= p
--          in genIBlockAddrBasic n p q cols "v_iblock_addr" "v_i" "v_j"
--      oblockAddrBasic = flip map perms $ \perm -> 
--          let cols = filter (\j -> P.apply perm j < p) [0..n-1]
--              q = tally cols $ \j -> j >= p
--          in genOBlockAddrBasic n p q cols "v_oblock_addr" "v_i" "v_j"
--      inAddrIter = flip map perms $ \perm -> 
--          let cols = filter (\j -> P.apply perm j < p) [0..n-1]
--              q = tally cols $ \j -> j >= p
--          in genInAddrIter n p q iters cols "v_in_addr" "v_i" "v_j" "v_g" "v_iter"
--      outAddrIter = flip map perms $ \perm -> 
--          let cols = filter (\j -> P.apply perm j < p) [0..n-1]
--              q = tally cols $ \j -> j >= p
--          in map (\(v_out, out_idx, v_in, in_idx, ofs) -> (v_out, P.apply perm out_idx, v_in, in_idx, ofs)) $           
--             genOutAddrIter n p q iters cols "v_out_addr" "v_i" "v_j" "v_g" "v_iter"
--      shift = flip map perms $ \perm -> 
--          let cols = filter (\j -> P.apply perm j < p) [0..n-1]
--              q = tally cols $ \j -> j >= p
--          in genShift n p q cols "v_shift" "v_block_addr"
--  printf "Out addr naive : unmerged=%.1g   merged=%.1g\n" 
--    (avgLength outAddrNaive) 
--    (avgLength $ map mergeAssigns outAddrNaive)
--  putStrLn ""
--  printf "In addr basic : unmerged=%.1g   merged=%.1g\n" 
--    (avgLength inAddrBasic) 
--    (avgLength $ map mergeAssigns inAddrBasic)
--  printf "Out addr basic : unmerged=%.1g   merged=%.1g\n" 
--    (avgLength outAddrBasic) 
--    (avgLength $ map mergeAssigns outAddrBasic)
--  printf "In block addr basic : unmerged=%.1g   merged=%.1g\n" 
--    (avgLength iblockAddrBasic) 
--    (avgLength $ map mergeAssigns iblockAddrBasic)
--  printf "Out block addr basic : unmerged=%.1g   merged=%.1g\n" 
--    (avgLength oblockAddrBasic) 
--    (avgLength $ map mergeAssigns oblockAddrBasic)
--  printf "Total basic : unmerged=%.1g   merged=%.1g\n" 
--    (avgLength inAddrBasic + avgLength outAddrBasic + avgLength iblockAddrBasic + avgLength oblockAddrBasic) 
--    (avgLength (map mergeAssigns inAddrBasic) + avgLength (map mergeAssigns outAddrBasic) +
--     avgLength (map mergeAssigns iblockAddrBasic) + avgLength (map mergeAssigns oblockAddrBasic))
--  putStrLn ""
--  printf "In addr iter : unmerged=%.1g   merged=%.1g\n" 
--    (avgLength inAddrIter) 
--    (avgLength $ map mergeAssigns inAddrIter)
--  printf "Out addr iter : unmerged=%.1g   merged=%.1g\n" 
--    (avgLength outAddrIter) 
--    (avgLength $ map mergeAssigns outAddrIter)
--  printf "Shift : unmerged=%.1g   merged=%.1g\n" 
--    (avgLength shift) 
--    (avgLength $ map mergeAssigns shift)
--  printf "Total iter + shift : unmerged=%.1g   merged=%.1g\n" 
--    (avgLength inAddrIter + avgLength outAddrIter + avgLength iblockAddrBasic + avgLength oblockAddrBasic + avgLength shift) 
--    (avgLength (map mergeAssigns inAddrIter) + avgLength (map mergeAssigns outAddrIter) +
--     avgLength (map mergeAssigns iblockAddrBasic) + avgLength (map mergeAssigns oblockAddrBasic) +
--     avgLength (map mergeAssigns shift))
  
  
main :: IO ()
main = do 
  let n = 15
      p = 5
      iters = 3
      perm = P.reverse n
  --putStrLn $ showBmmcCLike mat
  putStrLn $ generate "0" perm <> "\n"
  putStrLn $ generateC "0" perm p <> "\n"
  putStrLn $ generateCB "0" perm p <> "\n"
  putStrLn $ generateCI "0" perm p iters <> "\n"
  --putStrLn $ generateBmmcC "0" mat p <> "\n"
  --putStrLn $ generateBmmcCB "0" mat p <> "\n