module Bmmc (
  BMatrix, rows, cols, isSquare, 
  make, makeCol, makeRow, get,
  mult, transpose, identity,
  colToInt, rowToInt, colFromInt, rowFromInt,
  vstack, hstack, vsplit, hsplit,
  reducedRowEchelon, inverse, isInvertible, unsafeInverse,
  blockDiag, rank,
  transformInt
) where

import Data.Bits
import Data.List ( intercalate, find )
import Data.Maybe ( isJust )
import qualified Data.Vector.Unboxed as U


-- Rectangular boolean matrices. The first two arguments are the number of rows and columns.
data BMatrix = BMatrix Int Int (U.Vector Bool)
                   deriving (Eq)

rows :: BMatrix -> Int
rows (BMatrix r c v) = r

cols :: BMatrix -> Int
cols (BMatrix r c v) = c

get :: BMatrix -> Int -> Int -> Bool
get (BMatrix r c v) i j = v U.! (i * c + j)

instance Show BMatrix where
  show a = intercalate "\n" $ map (prettyRow a) [0..rows a - 1]
    where prettyRow a i = concatMap (\j -> if get a i j then "1" else ".") [0..cols a - 1]

isSquare :: BMatrix -> Bool
isSquare (BMatrix r c _) = r == c

make :: Int -> Int -> (Int -> Int -> Bool) -> BMatrix
make r c f = BMatrix r c v 
  where v = U.generate (r * c) $ \idx -> f (idx `div` c) (idx `mod` c)

makeCol :: Int -> (Int -> Bool) -> BMatrix
makeCol n f = make n 1 $ \i j -> f i

makeRow :: Int -> (Int -> Bool) -> BMatrix
makeRow n f = make 1 n $ \i j -> f j

mult :: BMatrix -> BMatrix -> BMatrix
a `mult` b | cols a /= rows b = error "mult: inner dimensions do not match"
a `mult` b = make (rows a) (cols b) $ \i j -> 
  foldr xor False [ get a i k && get b k j | k <- [0..cols a - 1] ]

transpose :: BMatrix -> BMatrix
transpose a = make (cols a) (rows a) $ \i j -> get a j i

-- The empty (0 * 0) matrix
empty :: BMatrix 
empty = BMatrix 0 0 U.empty

-- The square identity matrix
identity :: Int -> BMatrix
identity n = make n n $ \i j -> i == j

-- Horizontal stacking
hstack :: BMatrix -> BMatrix -> BMatrix 
hstack a b | rows a /= rows b = error "hstack: rows do not match"
hstack a b = make (rows a) (cols a + cols b) $ \i j -> 
  if j < cols a then get a i j else get b i (j - cols a) 

-- Vertical stacking
vstack :: BMatrix -> BMatrix -> BMatrix 
vstack a b | cols a /= cols b = error "vstack: columns do not match"
vstack a b = make (rows a + rows b) (cols a) $ \i j ->
  if i < rows a then get a i j else get b (i - rows a) j

-- Horizontal splitting
hsplit :: Int -> BMatrix -> (BMatrix, BMatrix)
hsplit k a | k < 0 || k > cols a = error "hsplit: invalid split index"
hsplit k a = (left, right)
  where left  = make (rows a) k $ \i j -> get a i j
        right = make (rows a) (cols a - k) $ \i j -> get a i (j + k)

-- Vertical splitting
vsplit :: Int -> BMatrix -> (BMatrix, BMatrix)
vsplit k a | k < 0 || k > rows a = error "vsplit: invalid split index"
vsplit k a = (top, bottom)
  where top    = make k (cols a) $ \i j -> get a i j
        bottom = make (rows a - k) (cols a) $ \i j -> get a (i + k) j

-- Compute the reduced row echelon form of a matrix.
-- This is very inefficient : I should probably use mutable vectors
-- in this function.
reducedRowEchelon :: BMatrix -> BMatrix 
reducedRowEchelon a0 = go a0 0 0
  where go a i j | i >= rows a || j >= cols a = a 
        go a i j = case getPivotRow a i j of 
          Nothing -> go a i (j+1)
          Just iPivot -> 
            let a1 = swapRows a i iPivot in
            let a2 = clearPivotCol a1 i j in
              go a2 (i+1) (j+1) 
        -- Get the row of the pivot for index (i, j)
        getPivotRow a i j = find (\i' -> get a i' j) [i..rows a - 1]
        -- Clear the column of the pivot at index (i, j)
        clearPivotCol a i j = foldr 
          (\i' a' -> if get a' i' j then addRow a' i i' else a') 
          a 
          ([0..i-1] ++ [i+1..rows a - 1])
        -- Swap rows i1 and i2 
        swapRows a i1 i2 = make (rows a) (cols a) $ \i j ->
          if i == i1 then get a i2 j 
          else if i == i2 then get a i1 j
          else get a i j
        -- Add row i1 to row i2
        addRow a i1 i2 = make (rows a) (cols a) $ \i j ->
          if i == i2 then get a i1 j `xor` get a i2 j 
          else get a i j 
        
-- Compute the inverse of a matrix.
inverse :: BMatrix -> Maybe BMatrix 
inverse a | not (isSquare a) = Nothing
inverse a = if left == identity n then Just right else Nothing
  where (left, right) = hsplit n $ reducedRowEchelon $ a `hstack` identity n
        n = rows a

-- For convenience
unsafeInverse :: BMatrix -> BMatrix 
unsafeInverse a = case inverse a of 
  Nothing -> error "unsafeInverse: the input matrix is not invertible"
  Just b -> b

isInvertible :: BMatrix -> Bool 
isInvertible = isJust . inverse

rank :: BMatrix -> Int
rank a0 = tally (isRowNonZero $ reducedRowEchelon a0) [0..rows a0 - 1]
  where tally f [] = 0
        tally f (x:xs) = if f x then 1 + tally f xs else tally f xs
        isRowNonZero a i = any (get a i) [0..cols a - 1]

-- This is inefficent : I could use mutable vectors here.
blockDiag :: [BMatrix] -> BMatrix 
blockDiag [] = empty
blockDiag (b1:bs) = make (rows b1 + rows b2) (cols b1 + cols b2) $ \i j ->
                     if i < rows b1 && j < cols b1 then get b1 i j 
                     else if i >= rows b1 && j >= cols b1 then get b2 (i - rows b1) (j - cols b1)
                     else False
  where b2 = blockDiag bs

-- Viewing integers as vectors of bits

colToInt :: BMatrix -> Int 
colToInt v | cols v /= 1 = error "colsToInt: argument is not a single column"
colToInt v = foldr (.|.) 0 $ map (1 `shiftL`) $ filter (\i -> get v i 0) [0..rows v - 1]

rowToInt :: BMatrix -> Int 
rowToInt v | rows v /= 1 = error "rowToInt: argument is not a single row"
rowToInt v = colToInt $ transpose v

colFromInt :: Int -> Int -> BMatrix
colFromInt n x | n < 0 || x < 0 = error "colFromInt: invalid arguments"
colFromInt n x = makeCol n $ \i -> odd (x `shiftR` i)

rowFromInt :: Int -> Int -> BMatrix
rowFromInt n x | n < 0 || x < 0 = error "rowFromInt: invalid arguments"
rowFromInt n x = transpose $ colFromInt n x

transformInt :: BMatrix -> Int -> Int
a `transformInt` x = colToInt $ a `mult` colFromInt (cols a) x

