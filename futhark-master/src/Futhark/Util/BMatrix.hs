-- This module implements binary matrices of rank 2.

module Futhark.Util.BMatrix (
  BMatrix, rows, cols, isSquare, 
  make, makeCol, makeRow, get, getCol, getRow,
  add, mult, transpose, empty, identity, zeros, ones,
  colToInt, rowToInt, colFromInt, rowFromInt,
  vstack, hstack, vsplit, hsplit,
  fromPerm,
  reducedRowEchelon, inverse, isInvertible, unsafeInverse,
  blockDiag, rank,
  transformInt,
  decomposeLUP, decomposeULP
) where

import Data.Bits ( Bits(shiftR, xor, (.|.), shiftL) )
import Data.List ( intercalate, find )
import Data.Maybe ( isJust )
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Control.Monad.ST ( ST )
import Control.Monad ( when )
import Data.Foldable ( forM_ )
import Futhark.Util.Perm qualified as P
import Futhark.Util.Pretty


-- Rectangular boolean matrices. The first two arguments are the number of rows and columns.
data BMatrix = BMatrix Int Int (U.Vector Bool)
                   deriving (Eq, Ord)

rows :: BMatrix -> Int
rows (BMatrix r _ _) = r

cols :: BMatrix -> Int
cols (BMatrix _ c _) = c

get :: BMatrix -> Int -> Int -> Bool
get (BMatrix r c v) i j 
  | 0 <= i && i < r && 0 <= j && j < c = v U.! (i * c + j)
  | otherwise = error "BMatrix.get: out of bounds access"

-- Extract the given row from a matrix
getRow :: BMatrix -> Int -> BMatrix
getRow a i 
  | 0 <= i && i < rows a = makeRow (cols a) $ \j -> get a i j
  | otherwise = error "BMatrix.getRow: out of bounds access"

-- Extract the given cplumn from a matrix
getCol :: BMatrix -> Int -> BMatrix
getCol a j 
  | 0 <= j && j < cols a = makeCol (rows a) $ \i -> get a i j
  | otherwise = error "BMatrix.getCol: out of bounds access"

-- The Show instance prints out the entries of the matrix.
instance Show BMatrix where
  show a = intercalate "\n" $ map prettyRow [0..rows a - 1]
    where prettyRow i = concatMap (\j -> if get a i j then "1" else ".") [0..cols a - 1]

-- The pretty instance only prints out the shape of the matrix.
instance Pretty BMatrix where
  --pretty a = apply $ map prettyRow [0..rows a - 1]
  --  where prettyRow i = pretty $ concatMap (\j -> if get a i j then "1" else ".") [0..cols a - 1]
  pretty a = "bmatrix" <> apply [ pretty (rows a), pretty (cols a) ] 

isSquare :: BMatrix -> Bool
isSquare (BMatrix r c _) = r == c

make :: Int -> Int -> (Int -> Int -> Bool) -> BMatrix
make r c f = BMatrix r c (U.force v) 
  where v = U.generate (r * c) $ \idx -> f (idx `div` c) (idx `mod` c)        

makeCol :: Int -> (Int -> Bool) -> BMatrix
makeCol n f = make n 1 $ \i _ -> f i

makeRow :: Int -> (Int -> Bool) -> BMatrix
makeRow n f = make 1 n $ \_ j -> f j

mult :: BMatrix -> BMatrix -> BMatrix
a `mult` b | cols a /= rows b = error "Bmmc.mult: inner dimensions do not match"
a `mult` b = make (rows a) (cols b) $ \i j -> 
  foldr xor False [ get a i k && get b k j | k <- [0..cols a - 1] ]

add :: BMatrix -> BMatrix -> BMatrix
a `add` b | rows a /= rows b || cols a /= cols b = error "Bmmc.add: dimensions do not match"
a `add` b = make (rows a) (cols a) $ \i j ->
  get a i j `xor` get b i j

transpose :: BMatrix -> BMatrix
transpose a = make (cols a) (rows a) $ \i j -> get a j i

-- The empty (0 * 0) matrix
empty :: BMatrix 
empty = BMatrix 0 0 U.empty

-- The square identity matrix
identity :: Int -> BMatrix
identity n = make n n $ \i j -> i == j

-- A matrix filled with 0 entries
zeros :: Int -> Int -> BMatrix
zeros r c = make r c $ \_ _ -> False

-- A matrix filled with 1 entries
ones :: Int -> Int -> BMatrix
ones r c = make r c $ \_ _ -> True

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
reducedRowEchelonAux r c a = go 0 0
  where go i j | i >= r || j >= c = pure ()
        go i j = do
          pivot <- getPivotRow i j
          case pivot of  
            Nothing -> go i (j+1)
            Just iPivot -> do
              swapRows i iPivot
              clearPivotCol i j
              go (i+1) (j+1) 
        -- Get the row of the pivot for index (i, j)
        getPivotRow i j = findM (\i' -> get i' j) [i..r-1]
        -- Clear the column of the pivot at index (i, j)
        clearPivotCol i j = forM_ ([0..i-1] ++ [i+1..r-1]) $ \i' ->
          do b <- get i' j 
             when b $ addRow i i'
        -- Swap rows i1 and i2 
        swapRows i1 i2 = forM_ [0..c-1] $ \j ->
          do b1 <- get i1 j 
             b2 <- get i2 j
             set i1 j b2
             set i2 j b1
        -- Add row i1 to row i2
        addRow i1 i2 = forM_ [0..c-1] $ \j ->
          do b1 <- get i1 j
             b2 <- get i2 j
             set i2 j (b1 `xor` b2)
        get i j = MU.read a (i * c + j)
        set i j = MU.write a (i * c + j)

-- | Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b' <- b; if b' then t else f

-- | Like 'find', but where the test can be monadic.
--
-- > findM (Just . isUpper) "teST"             == Just (Just 'S')
-- > findM (Just . isUpper) "test"             == Just Nothing
-- > findM (Just . const True) ["x",undefined] == Just (Just "x")
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)


fromPerm :: P.Perm -> BMatrix 
fromPerm perm = make n n $ \i j -> i == P.apply perm j
  where n = P.size perm


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
  where tally _ [] = 0
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

-- Decompose a matrix A as A = L*U*P
-- where P is a permutation matrix
--       L is a lower triangular matrix
--       U is an upper triangular matrix
decomposeLUP :: BMatrix -> (BMatrix, BMatrix, P.Perm)
decomposeLUP a 
  | not (isSquare a) = error "Upper/Lower definition is only defined for square matrices"
  | n <= 1 = (identity n, a, P.identity n)
  | otherwise = (l, u, p)
  where l = block (ones 1 1) (zeros 1 (n-1)) v l'
        u = block (ones 1 1) (w `mult` fromPerm (P.inverse p')) (zeros (n-1) 1) u'
        p = shift p' `P.compose` pswap
          where shift p0 = P.fromList $ 0 : map (+1) (P.toList p0)
        (l', u', p') = decomposeLUP (a' `add` (v `mult` w))
        (_, w, v, a') = unblock (a `mult` fromPerm pswap)
        pswap = P.swap n 0 j
          where Just j = find (get a 0) [0..n-1]
        block x1 x2 x3 x4 = (x1 `hstack` x2) `vstack` (x3 `hstack` x4)
        unblock x = (x1, x2, x3, x4)
          where (x12, x34) = vsplit 1 x
                (x1, x2) = hsplit 1 x12
                (x3, x4) = hsplit 1 x34
        n = rows a
        
-- Decompose a matrix A as A = U*L*P
-- where P is a permutation matrix
--       L is a lower triangular matrix
--       U is an upper triangular matrix
decomposeULP :: BMatrix -> (BMatrix, BMatrix, P.Perm)
decomposeULP a 
  | not (isSquare a) = error "Upper/Lower definition is only defined for square matrices"
  | otherwise = (u, l, p)
  where u = r `mult` l' `mult` r
        l = r `mult` u' `mult` r
        p = P.reverse n `P.compose` p' `P.compose` P.reverse n
        (l', u', p') = decomposeLUP (r `mult` a `mult` r)
        r = fromPerm $ P.reverse n
        n = rows a
