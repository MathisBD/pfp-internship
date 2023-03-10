module Main ( main ) where 

import Data.Bits


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

-- Make a k-regular column, but build it using parm.
colWithPar :: Int -> (a -> a -> a) -> (a -> a -> a) -> [a] -> [a]
colWithPar k f g
  | k <= 0         = undefined
  | k == 1         = \[x0, x1] -> [f x0 x1, g x0 x1]
  | k `mod` 4 == 0 = ilv $ col (k `div` 2) f g
  | k `mod` 4 == 1 = que $ col (k `div` 2) f g
  | k `mod` 4 == 2 = ilv $ col (k `div` 2) f g
  | k `mod` 4 == 3 = vee $ col (k `div` 2) f g


replicateMin :: [Int] -> [Int]
replicateMin xs = replicate (length xs) (minimum xs)

main :: IO ()
main = do 
  let xs = [0..15]
      f1 = (ilv . ilv . ilv) replicateMin
      f2 = col 8 min min
      f3 = colWithPar 8 min min
  print $ f1 xs
  print $ f2 xs  
  print $ f3 xs      