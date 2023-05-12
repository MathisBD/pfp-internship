module Main ( main ) where 

import Data.Bits
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


-- Seperate a list in two.
-- The list of booleans tells whether each element should go left or right.
seperate :: [Bool] -> [a] -> ([a], [a])
seperate [] [] = ([], [])
seperate (b:bs) (x:xs) = if b then (ls', x:rs') else (x:ls', rs')
  where (ls', rs') = seperate bs xs

-- Merge two lists into one. 
-- The list of booleans tells whether each element should be taken from left or right 
merge :: [Bool] -> [a] -> [a] -> [a]
merge [] [] [] = []
merge (False:bs) (l:ls) rs = l : merge bs ls rs
merge (True:bs) ls (r:rs) = r : merge bs ls rs

-- The parm combinator.
parm :: Int -> ([a] -> [a]) -> ([a] -> [a])
parm mask _ _ | mask <= 0 = undefined
parm mask f xs = merge groups (f ls) (f rs)
  where (ls, rs) = seperate groups xs
        groups = [ odd $ popCount (mask .&. i) `mod` 2 | i <- [0..length xs - 1] ]
        
ilv = parm 1
que = parm 2
vee = parm 3

-- The two combinator. 
two :: ([a] -> [a]) -> ([a] -> [a])
two f xs = f ls ++ f rs
  where ls = take (length xs `div` 2) xs
        rs = drop (length xs `div` 2) xs

-- Make a k-regular column out of two binary functions
col :: Int -> (a -> a -> a) -> (a -> a -> a) -> [a] -> [a]
col k f g xs = [ res i | i <- [0..length xs - 1] ]
  where res i = if i < (i `xor` k) 
                then f (xs !! i) (xs !! (i `xor` k))
                else g (xs !! i) (xs !! (i `xor` k))

removeBit :: Int -> Int -> Int 
removeBit i x = ((x .&. high) `shiftR` 1) .|. (x .&. low)
  where high = (complement 0) `shiftL` (i+1)
        low  = (1 `shiftL` i) - 1

-- Make a k-regular column, but build it using parm.
colWithParm :: Int -> (a -> a -> a) -> (a -> a -> a) -> [a] -> [a]
colWithParm k _ _ _ | k <= 0 = undefined
colWithParm k f g [x0, x1] = [f x0 x1, g x0 x1]
colWithParm k f g xs
  | k `mod` 4 == 0 = ilv (col (removeBit 0 k) f g) xs
  | k `mod` 4 == 1 = que (col (removeBit 1 k) f g) xs
  | k `mod` 4 == 2 = ilv (col (removeBit 0 k) f g) xs
  | k `mod` 4 == 3 = vee (col (removeBit 0 k) f g) xs


replicateMin :: [Int] -> [Int]
replicateMin xs = replicate (length xs) (minimum xs)

--permuteBMMC :: B.BMatrix -> [a] -> [a]
--permuteBMMC a xs = map (\i -> xs !! src i) [0..length xs - 1]
--  where src i = B.unsafeInverse a `B.transformInt` i
--
-- The index of the least significant bit in a positive integer
--lsb :: Int -> Int
--lsb x 
--  | x <= 0    = error "lsb: needs a positive argument"
--  | odd x     = 0
--  | otherwise = 1 + lsb (x `div` 2)
--
---- Calculate the matrix corresponding to a parm mask.
--parmToMatrix :: Int -> Int -> B.BMatrix
--parmToMatrix n mask = top `B.vstack` B.rowFromInt n mask
--  where top = B.make (n-1) n $ \i j -> case compare j (lsb mask) of 
--                LT -> i == j
--                EQ -> False
--                GT -> j == i + 1
--
---- The parm combinator, implemented using BMMCs and 'two'.
--parmWithBmmc :: Int -> ([a] -> [a]) -> ([a] -> [a])
--parmWithBmmc mask _ xs | mask <= 0 = undefined
--parmWithBmmc mask f xs = permuteBMMC (B.unsafeInverse a) $ two f $ permuteBMMC a xs
--  where a = parmToMatrix n mask
--        n = log2 $ length xs
--        log2 1 = 0
--        log2 i = 1 + log2 (i `div` 2)
--
--parmNestToMatrix :: Int -> [Int] -> B.BMatrix
--parmNestToMatrix n [] = B.identity n
--parmNestToMatrix n (m:ms) = 
--  B.blockDiag [parmNestToMatrix (n-1) ms, B.identity 1] `B.mult` parmToMatrix n m 
--
---- Get the BMMC matrix corresponding to a column
--colToMatrix :: Int -> Int -> B.BMatrix
--colToMatrix n k | k <= 0 || n <= 0 || k >= 2^n = undefined
--colToMatrix 1 k = B.identity 1
--colToMatrix n k = innerM `B.mult` outerM 
--  where outerM = parmToMatrix n mask
--        innerM = B.blockDiag [colToMatrix (n-1) newK, B.identity 1]
--        mask
--          | k `mod` 4 == 0 = 1
--          | k `mod` 4 == 1 = 2
--          | k `mod` 4 == 2 = 1
--          | k `mod` 4 == 3 = 3
--        newK
--          | k `mod` 4 == 1 = removeBit 1 k
--          | otherwise      = removeBit 0 k


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
      p = 5
      iters = 3
      perm = P.identity n 
      mat = B.fromPerm perm
  print mat
  putStrLn $ showBmmcCLike mat
  putStrLn $ generate "0" perm <> "\n"
  putStrLn $ generateC "0" perm p <> "\n"
  putStrLn $ generateCB "0" perm p <> "\n"
  putStrLn $ generateCI "0" perm p iters <> "\n"
  putStrLn $ generateCBI "0" perm p iters <> "\n"
  putStrLn $ generateBmmcC "0" mat p <> "\n"
  putStrLn $ generateBmmcCB "0" mat p <> "\n"

--main :: IO ()
--main = do 
--  let n = 10
--      p = 4
--  perm <- P.generateRandom n
--  let q = tally (\i -> P.apply perm i < p) [p..n-1]
--      xs = [0..2^n - 1]
--      ys1 = doPermNaive perm xs
--      ys2 = doPerm perm p q xs
--  print $ P.toMatrix perm
--  putStrLn $ "n = " <> show n 
--  putStrLn $ "p = " <> show p 
--  putStrLn $ "q = " <> show q 
--  --print xs
--  --print ys1 
--  --print ys2
--  print $ ys1 == ys2

--main :: IO ()
--main = do
--  let n = 10
--      perm = (n-1) : [0..n-2]
--      p = 4
--      q = tally (\i -> perm !! i < p) [p..n-1]
--  g <- randomRIO (0, 2^(n-p-q) - 1)
--  i <- randomRIO (0, 2^q - 1)
--  let accesses = map (generateAccesses perm p q g i) [0..2^p-1]
--      reads = sort $ map fst accesses
--      writes = sort $ map snd accesses
--  print $ permToMatrix perm
--  putStrLn $ "n = " <> show n
--  putStrLn $ "p = " <> show p
--  putStrLn $ "q = " <> show q
--  putStrLn $ "group = " <> show g
--  putStrLn $ "i = " <> show i
--  putStrLn $ show reads <> " " <> show (isConsecutive reads)
--  putStrLn $ show writes <> " " <> show (isConsecutive writes)