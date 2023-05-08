module Perm (
  Perm, size, identity, Perm.reverse, make,
  inverse, apply, toMatrix, permute,
  generateAll, generateRandom
) where 

import System.Random ( randomIO, randomRIO )
import qualified Data.Vector.Unboxed as U
import qualified Bmmc as B

-- The permutation is defined as the mapping between the indices
-- and the corresponding elements.
type Perm = U.Vector Int

-- The number of elements this permutation acts on.
size :: Perm -> Int
size = U.length

-- The image of an index through a permutation.
apply :: Perm -> Int -> Int
apply perm i | i < 0 || i >= size perm = error "out of bounds index when applying permutation"
apply perm i = perm U.! i

-- Make a permutation given a mapping.
make :: Int -> (Int -> Int) -> Perm
make n f = U.generate n f

-- The identity permutation on n elements.
identity :: Int -> Perm 
identity n = make n id

-- The reverse permutation on n elements.
reverse :: Int -> Perm 
reverse n = make n $ \i -> n - 1 - i

toMatrix :: Perm -> B.BMatrix 
toMatrix perm = B.make n n $ \i j -> i == apply perm j
  where n = size perm

-- Generate all permutations on n elements.
generateAll :: Int -> [Perm]
generateAll 0 = [U.empty]
generateAll n = do
  -- Choose the image of the first element
  first <- [0..n-1]
  -- Choose the image of the other n-1 elements
  rest <- generateAll (n-1)
  -- Combine them 
  pure $ make n $ \i -> 
    if i == 0 then first else 
    let x = apply rest i in if x < first then x else x + 1

generateRandom :: Int -> IO Perm
generateRandom 0 = pure U.empty
generateRandom n = do
  -- Choose the image of the first element
  first <- randomRIO (0, n-1)
  -- Choose the image of the other n-1 elements
  rest <- generateRandom (n-1)
  -- Combine them 
  pure $ make n $ \i -> 
    if i == 0 then first else 
    let x = apply rest (i-1) in if x < first then x else x + 1

-- Compute the inverse of a permutation.
inverse :: Perm -> Perm 
inverse perm = make n $ \i -> indexOf i (U.toList perm)
  where n = size perm
        indexOf i [] = undefined
        indexOf i (x:xs) 
          | i == x    = 0
          | otherwise = 1 + indexOf i xs

-- Apply a permutation to a list of arbitrary elements.
permute :: Perm -> [a] -> [a]
permute perm xs = [ xs !! apply inv i | i <- [0..n-1] ]
  where n = size perm
        inv = inverse perm
