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


isConsecutive :: [Int] -> Bool
isConsecutive xs = sort xs == [minimum xs..maximum xs]
  
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

main :: IO ()
main = do 
  let n = 30
      n2 = 15
      p = 5
      perm = P.fromList $ [n2..n-1] ++ [0..n2-1] 
      mat = B.fromPerm perm
  print mat
  --putStrLn $ showBmmcCLike mat
  putStrLn $ generate "0" perm <> "\n"
  putStrLn $ generateC "0" perm p <> "\n"
  --putStrLn $ generateCBI "0" perm p iters <> "\n"
  --putStrLn $ generateBmmcC "0" mat p <> "\n"
  --putStrLn $ generateBmmcCB "0" mat p <> "\n"
