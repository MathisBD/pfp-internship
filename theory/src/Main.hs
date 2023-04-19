module Main ( main ) where 

import Data.Bits
import Data.List ( sort, transpose )
import Data.List.Unique ( count )
import Control.Monad ( forM_, liftM2 )
import Data.Traversable ( for )
import System.Random ( randomIO )
import qualified Bmmc as B


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

permuteBMMC :: B.BMatrix -> [a] -> [a]
permuteBMMC a xs = map (\i -> xs !! src i) [0..length xs - 1]
  where src i = B.unsafeInverse a `B.transformInt` i

-- The index of the least significant bit in a positive integer
lsb :: Int -> Int
lsb x 
  | x <= 0    = error "lsb: needs a positive argument"
  | odd x     = 0
  | otherwise = 1 + lsb (x `div` 2)

-- Calculate the matrix corresponding to a parm mask.
parmToMatrix :: Int -> Int -> B.BMatrix
parmToMatrix n mask = top `B.vstack` B.rowFromInt n mask
  where top = B.make (n-1) n $ \i j -> case compare j (lsb mask) of 
                LT -> i == j
                EQ -> False
                GT -> j == i + 1

-- The parm combinator, implemented using BMMCs and 'two'.
parmWithBmmc :: Int -> ([a] -> [a]) -> ([a] -> [a])
parmWithBmmc mask _ xs | mask <= 0 = undefined
parmWithBmmc mask f xs = permuteBMMC (B.unsafeInverse a) $ two f $ permuteBMMC a xs
  where a = parmToMatrix n mask
        n = log2 $ length xs
        log2 1 = 0
        log2 i = 1 + log2 (i `div` 2)

parmNestToMatrix :: Int -> [Int] -> B.BMatrix
parmNestToMatrix n [] = B.identity n
parmNestToMatrix n (m:ms) = 
  B.blockDiag [parmNestToMatrix (n-1) ms, B.identity 1] `B.mult` parmToMatrix n m 

-- Get the BMMC matrix corresponding to a column
colToMatrix :: Int -> Int -> B.BMatrix
colToMatrix n k | k <= 0 || n <= 0 || k >= 2^n = undefined
colToMatrix 1 k = B.identity 1
colToMatrix n k = innerM `B.mult` outerM 
  where outerM = parmToMatrix n mask
        innerM = B.blockDiag [colToMatrix (n-1) newK, B.identity 1]
        mask
          | k `mod` 4 == 0 = 1
          | k `mod` 4 == 1 = 2
          | k `mod` 4 == 2 = 1
          | k `mod` 4 == 3 = 3
        newK
          | k `mod` 4 == 1 = removeBit 1 k
          | otherwise      = removeBit 0 k

isGood :: Int -> B.BMatrix -> Bool
isGood k a = B.rank tr <= 3 && B.rank bl <= 3 && B.rank br >= k-3
  where (t, b) = B.vsplit (B.rows a - k) a
        (tl, tr) = B.hsplit (B.cols a - k) t
        (bl, br) = B.hsplit (B.cols a - k) b

-- Generate a list of random integers with a high and low bound
randomList :: Int -> Int -> Int -> IO [Int]
randomList 0 low high = pure []
randomList n low high = do
  x <- randomIO :: IO Int
  xs <- randomList (n-1) low high
  let x'  = if x < 0 then -x else x
      x'' = low + x' `mod` (high - low)
  pure $ x'' : xs 


main :: IO ()
main = do 
  let n = 16
      maskCount = 13
  mss <- mapM (\i -> randomList 100 1 (2^(n-i)-1)) [0..maskCount - 1]
  let as = map (parmNestToMatrix n) $ transpose mss
      correct = tally (\(a1, a2) -> isGood maskCount $ a1 `B.mult` B.unsafeInverse a2)
        $ liftM2 (,) as as
      percent = (fromInteger correct / fromInteger (toInteger (length as * length as))) * 100
  putStrLn $ "Correct : " <> show correct <> " (" <> show percent <> "%)"
  where tally f [] = 0
        tally f (x:xs) = if f x then 1 + tally f xs else tally f xs      
        

-- The permutation is defined as the mapping between the indices
-- and the corresponding elements.
--type Perm = [Int]
--
---- Generate all permutations on n elements.
--genPerms :: Int -> [Perm]
--genPerms 0 = [[]]
--genPerms n = do
--  -- Choose the image of the first element
--  first <- [0..n-1]
--  -- Choose the image of the other n-1 elements
--  rest <- genPerms (n-1)
--  -- Combine them 
--  pure $ first : map (\x -> if x < first then x else x + 1) rest
--
---- The k-cross-rank of a permutation is the number of indices that cross 
---- a line between k-1 and k, in one direction.
--permRank :: Int -> Perm -> Int
--permRank k perm = tally [perm !! i >= k | i <- [0..k-1]]
--  where tally = foldl (\acc b -> if b then acc + 1 else acc) 0
--
--binom :: Integer -> Integer -> Integer
--binom n 0 = 1
--binom 0 k = 0
--binom n k = (binom (n-1) (k-1) * n) `div` k
--
--fact :: Integer -> Integer 
--fact 0 = 1
--fact n = n * fact (n-1)
--
--main :: IO ()
--main = do 
--  let n = 20
--      k = 10
--      --perms = genPerms (fromInteger n)
--      --hist = count $ map (permRank (fromInteger k)) perms
--      hist = map (\m -> (m, binom k m * binom (n-k) m * fact k * fact (n-k))) [0..k]
--      total = sum (map snd hist)
--  forM_ hist $ \(m, c) -> do
--    let p = 100 * (fromInteger c / fromInteger total)
--    putStrLn $ "  " <> show m <> " --> " <> show (round p) <> "%"