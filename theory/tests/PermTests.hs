module PermTests ( permTests ) where 

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Perm as P
import qualified Bmmc as B
import Data.List ( sort )


instance Arbitrary P.Perm where
  arbitrary = sized $ \n -> 
    P.fromList <$> shuffle [0..n-1] 

permTests :: TestTree
permTests = testGroup "Permutation Tests" 
  [ testProperty "id-size" idSizeProp
  , testProperty "rev-size" revSizeProp
  , testProperty "to-from-list" toFromListProp
  , testProperty "id-valid" idValidProp
  , testProperty "rev-valid" revValidProp
  , testProperty "inv-valid" invValidProp
  , testProperty "inv-inv" invInvProp
  , testProperty "to-matrix-invertible" toMatrixInvProp
  , testProperty "to-matrix-permute" toMatrixPermuteProp
  , testProperty "inv-to-matrix" invToMatrixProp
  ]

isPermValid :: P.Perm -> Bool
isPermValid perm = sort (P.toList perm) == [0..n-1]
  where n = P.size perm

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation xs ys = sort xs == sort ys

idSizeProp :: Int -> Property
idSizeProp n = n >= 0 ==> P.size (P.identity n) == n

revSizeProp :: Int -> Property
revSizeProp n = n >= 0 ==> P.size (P.reverse n) == n

toFromListProp :: P.Perm -> Property 
toFromListProp perm = P.fromList (P.toList perm) === perm

idValidProp :: Int -> Property
idValidProp n = n >= 0 ==> isPermValid (P.identity n)

revValidProp :: Int -> Property
revValidProp n = n >= 0 ==> isPermValid (P.reverse n)

invValidProp :: P.Perm -> Property
invValidProp perm = property $ isPermValid $ P.inverse perm

invInvProp :: P.Perm -> Property
invInvProp perm = P.inverse (P.inverse perm) === perm

permuteProp :: P.Perm -> [Int] -> Property
permuteProp perm xs = property $ P.permute perm xs `isPermutation` xs

toMatrixInvProp :: P.Perm -> Property
toMatrixInvProp perm = property $ B.isInvertible (P.toMatrix perm)

-- LSB is first
fromBits :: [Bool] -> Integer
fromBits [] = 0
fromBits (True:bs) = 1 + 2 * fromBits bs
fromBits (False:bs) = 2 * fromBits bs

-- LSB is first
toBits :: Int -> Integer -> [Bool]
toBits 0 x = []
toBits n x = odd x : toBits (n-1) (x `div` 2)

-- The precondition is pretty sparse, we use custom generators when running this property
toMatrixPermuteProp :: P.Perm -> Integer -> Property
toMatrixPermuteProp perm i =
  0 <= i ==> i < 2^n ==>  
    fromBits (P.permute perm (toBits n i)) == B.transformInt (P.toMatrix perm) i
  where n = P.size perm
  
invToMatrixProp :: P.Perm -> Property
invToMatrixProp perm = B.inverse (P.toMatrix perm) === Just (P.toMatrix (P.inverse perm))