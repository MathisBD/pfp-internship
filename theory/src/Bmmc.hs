module Bmmc (
  BMatrix, rows, cols, isSquare, 
  make, makeCol, makeRow, get,
  mult, transpose, empty, identity,
  colToInt, rowToInt, colFromInt, rowFromInt,
  vstack, hstack, vsplit, hsplit,
  reducedRowEchelon, inverse, isInvertible, unsafeInverse,
  blockDiag, rank,
  transformInt
) where

import Data.Bits ( Bits(shiftR, xor, (.|.), shiftL) )
import Data.List ( intercalate, find )
import Data.Maybe ( isJust )
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Control.Monad.ST ( ST )
import Control.Monad.Extra ( findM )
import Control.Monad ( when )
import Data.Foldable ( forM_ )


-- Rectangular boolean matrices. The first two arguments are the number of rows and columns.
data BMatrix = BMatrix Int Int (U.Vector Bool)
                   deriving (Eq)

rows :: BMatrix -> Int
rows (BMatrix r c v) = r

cols :: BMatrix -> Int
cols (BMatrix r c v) = c

get :: BMatrix -> Int -> Int -> Bool
get (BMatrix r c v) i j 
  | 0 <= i && i < r && 0 <= j && j < c = v U.! (i * c + j)
  | otherwise = error "BMatrix.get: out of bounds access"

instance Show BMatrix where
  show a = intercalate "\n" $ map (prettyRow a) [0..rows a - 1]
    where prettyRow a i = concatMap (\j -> if get a i j then "1" else ".") [0..cols a - 1]

isSquare :: BMatrix -> Bool
isSquare (BMatrix r c _) = r == c

make :: Int -> Int -> (Int -> Int -> Bool) -> BMatrix
make r c f = BMatrix r c (U.force v) 
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
reducedRowEchelon :: BMatrix -> BMatrix
reducedRowEchelon (BMatrix r c bs) = BMatrix r c (U.modify (reducedRowEchelonAux r c) bs)

-- We do this operation on a mutable vector for better performance
reducedRowEchelonAux :: Int -> Int -> MU.MVector s Bool -> ST s ()
reducedRowEchelonAux rows cols a = go 0 0
  where go i j | i >= rows || j >= cols = pure ()
        go i j = do
          pivot <- getPivotRow i j
          case pivot of  
            Nothing -> go i (j+1)
            Just iPivot -> do
              swapRows i iPivot
              clearPivotCol i j
              go (i+1) (j+1) 
        -- Get the row of the pivot for index (i, j)
        getPivotRow i j = findM (\i' -> get i' j) [i..rows-1]
        -- Clear the column of the pivot at index (i, j)
        clearPivotCol i j = forM_ ([0..i-1] ++ [i+1..rows-1]) $ \i' ->
          do b <- get i' j 
             when b $ addRow i i'
        -- Swap rows i1 and i2 
        swapRows i1 i2 = forM_ [0..cols-1] $ \j ->
          do b1 <- get i1 j 
             b2 <- get i2 j
             set i1 j b2
             set i2 j b1
        -- Add row i1 to row i2
        addRow i1 i2 = forM_ [0..cols-1] $ \j ->
          do b1 <- get i1 j
             b2 <- get i2 j
             set i2 j (b1 `xor` b2)
        get i j = MU.read a (i * cols + j)
        set i j = MU.write a (i * cols + j)

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

colToInt :: BMatrix -> Integer
colToInt v | cols v /= 1 = error "colToInt: argument is not a single column"
colToInt v = foldr (.|.) 0 $ map (1 `shiftL`) $ filter (\i -> get v i 0) [0..rows v - 1]

rowToInt :: BMatrix -> Integer
rowToInt v | rows v /= 1 = error "rowToInt: argument is not a single row"
rowToInt v = colToInt $ transpose v

colFromInt :: Int -> Integer -> BMatrix
colFromInt n x | n < 0 || x < 0 = error "colFromInt: invalid arguments"
colFromInt n x = makeCol n $ \i -> odd (x `shiftR` i)

rowFromInt :: Int -> Integer -> BMatrix
rowFromInt n x | n < 0 || x < 0 = error "rowFromInt: invalid arguments"
rowFromInt n x = transpose $ colFromInt n x

transformInt :: BMatrix -> Integer -> Integer
a `transformInt` x = colToInt $ a `mult` colFromInt (cols a) x

