module Main where 

import Data.Bits
import Data.List ( sort )
import Data.List.Unique ( count )
import Control.Monad ( forM_ )

-- Seperate a list in two.
-- The list of booleans tells whether each element should go left or right.
seperate :: [Bool] -> [a] -> ([a], [a])
seperate [] [] = ([], [])
seperate (b:bs) (x:xs) = if b then (ls', x:rs') else (x:ls', rs')
  where (ls', rs') = seperate bs xs

-- Merge to lists into one. 
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
colWithPar :: Int -> (a -> a -> a) -> (a -> a -> a) -> [a] -> [a]
colWithPar k _ _ _ | k <= 0 = undefined
colWithPar k f g [x0, x1] = [f x0 x1, g x0 x1]
colWithPar k f g xs
  | k `mod` 4 == 0 = ilv (col (removeBit 0 k) f g) xs
  | k `mod` 4 == 1 = que (col (removeBit 1 k) f g) xs
  | k `mod` 4 == 2 = ilv (col (removeBit 0 k) f g) xs
  | k `mod` 4 == 3 = vee (col (removeBit 0 k) f g) xs


replicateMin :: [Int] -> [Int]
replicateMin xs = replicate (length xs) (minimum xs)

--main :: IO ()
--main = do 
--  let xs = [0..7]
--      ys = [0, 1, 1, 0, 1, 0, 0, 1]
--  print $ parm 7 replicateMin xs
--  print ys

-- The permutation is defined as the mapping between the indices
-- and the corresponding elements.
type Perm = [Int]

-- Generate all permutations on n elements.
genPerms :: Int -> [Perm]
genPerms 0 = [[]]
genPerms n = do
  -- Choose the image of the first element
  first <- [0..n-1]
  -- Choose the image of the other n-1 elements
  rest <- genPerms (n-1)
  -- Combine them 
  pure $ first : map (\x -> if x < first then x else x + 1) rest

-- The k-cross-rank of a permutation is the number of indices that cross 
-- a line between k-1 and k, in one direction.
permRank :: Int -> Perm -> Int
permRank k perm = tally [perm !! i >= k | i <- [0..k-1]]
  where tally = foldl (\acc b -> if b then acc + 1 else acc) 0

binom :: Integer -> Integer -> Integer
binom n 0 = 1
binom 0 k = 0
binom n k = (binom (n-1) (k-1) * n) `div` k

fact :: Integer -> Integer 
fact 0 = 1
fact n = n * fact (n-1)

main :: IO ()
main = do 
  let n = 18
      k = 10
      --perms = genPerms (fromInteger n)
      --hist = count $ map (permRank (fromInteger k)) perms
      hist = map (\m -> (m, binom k m * binom (n-k) m * fact k * fact (n-k))) [0..k]
      total = sum (map snd hist)
  forM_ hist $ \(m, c) -> do
    let p = 100 * (fromInteger c / fromInteger total)
    putStrLn $ "  " <> show m <> " --> " <> show (round p) <> "%"