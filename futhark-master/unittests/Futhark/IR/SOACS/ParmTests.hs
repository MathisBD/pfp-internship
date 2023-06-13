module Futhark.IR.SOACS.ParmTests ( tests ) where

import Data.Bits ( popCount, (.&.) )
import Futhark.IR.SOACS.Parm
import Futhark.Util.BMatrix qualified as B
import Test.Tasty
import Test.Tasty.QuickCheck hiding ( (.&.) )


tests :: TestTree
tests = testGroup "Parm Bmmc Tests" 
  [ testProperty "parm-single" $ 
      forAllShrink (choose (0, 10)) shrink $ \n -> 
        parmSingleProp n
  , testProperty "parm-nested" $ 
      forAllShrink (choose (0, 10)) shrink $ \n -> 
      forAllShrink (resize n $ listOf arbitrary) shrink $ \masks ->
        parmNestedProp n masks
  ]

parmSingleProp :: Int -> Integer -> Property 
parmSingleProp n mask = 
  0 < n ==> 
  0 < mask ==> 
  mask < 2^n ==> 
    forAllShrink (choose (0, 2^(n-1)-1)) shrink $ \k ->
      parm mask (replicateKth k) xs == parmWithBmmc mask (replicateKth k) xs
  where xs :: [Integer] = [0..2^n - 1]

parmNestedProp :: Int -> [Integer] -> Property
parmNestedProp n masks = 
  0 < n ==> 
  length masks <= n ==>
  all (0 <) masks ==>
  all (\(m, i) -> m < 2^(n-i)) (zip masks [0..]) ==>
    forAllShrink (choose (0, 2^(n-length masks) - 1)) shrink $ \k ->
      parmNest masks (replicateKth k) xs == parmNestWithBmmc masks (replicateKth k) xs 
  where xs :: [Integer] = [0..2^n - 1]

replicateKth :: Int -> [a] -> [a]
replicateKth k xs0 = replicate (length xs0) (xs0 !! k)


-- Seperate a list in two.
-- The list of booleans tells whether each element should go left or right.
seperate :: [Bool] -> [a] -> ([a], [a])
seperate [] [] = ([], [])
seperate (b:bs) (x:xs) = if b then (ls', x:rs') else (x:ls', rs')
  where (ls', rs') = seperate bs xs
seperate _ _ = undefined 

-- Merge two lists into one. 
-- The list of booleans tells whether each element should be taken from left or right 
merge :: [Bool] -> [a] -> [a] -> [a]
merge [] [] [] = []
merge (False:bs) (l:ls) rs = l : merge bs ls rs
merge (True:bs) ls (r:rs) = r : merge bs ls rs
merge _ _ _ = undefined

-- The parm combinator.
parm :: Integer -> ([a] -> [a]) -> ([a] -> [a])
parm mask _ _ | mask <= 0 = undefined
parm mask f xs = merge groups (f ls) (f rs)
  where (ls, rs) = seperate groups xs
        groups = [ odd $ popCount (mask .&. toInteger i) `mod` 2 | i <- [0..length xs - 1] ]

parmNest :: [Integer] -> ([a] -> [a]) -> [a] -> [a]
parmNest [] f xs = f xs
parmNest (m:ms) f xs = parm m (parmNest ms f) xs

-- The two combinator. 
two :: Int -> ([a] -> [a]) -> ([a] -> [a])
two 0 f xs = f xs 
two k f xs = two (k-1) f ls ++ two (k-1) f rs
  where ls = take (length xs `div` 2) xs
        rs = drop (length xs `div` 2) xs

permuteBMMC :: B.BMatrix -> [a] -> [a]
permuteBMMC a xs = map (\i -> xs !! src i) [0..length xs - 1]
  where src i = fromInteger $ B.unsafeInverse a `B.transformInt` toInteger i

-- The parm combinator, implemented using BMMCs and 'two'.
parmWithBmmc :: Integer -> ([a] -> [a]) -> ([a] -> [a])
parmWithBmmc mask f xs = 
  permuteBMMC (B.unsafeInverse a) $ two 1 f $ permuteBMMC a xs
  where a = parmMatrix n mask
        n = log2 $ length xs
        log2 1 = 0
        log2 i = 1 + log2 (i `div` 2)

parmNestWithBmmc :: [Integer] -> ([a] -> [a]) -> ([a] -> [a])
parmNestWithBmmc masks f xs = 
  permuteBMMC (B.unsafeInverse a) $ two (length masks) f $ permuteBMMC a xs
  where a = parmNestMatrix n masks
        n = log2 $ length xs
        log2 1 = 0
        log2 i = 1 + log2 (i `div` 2)

