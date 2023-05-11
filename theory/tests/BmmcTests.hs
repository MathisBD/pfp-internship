module BmmcTests ( bmmcTests ) where 

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Perm as P
import qualified Bmmc as B
import Data.List ( sort, isPrefixOf, find, isSubsequenceOf )
import Control.Monad ( liftM2 )


-- An arbitrary matrix
randMatrix :: Int -> Int -> Gen B.BMatrix
randMatrix rows cols = do 
  contents <- vectorOf (rows * cols) arbitrary
  pure $ B.make rows cols $ \i j -> contents !! (i * cols + j)         

-- An arbitrary upper invertible matrix
randUpperInvMatrix :: Int -> Gen B.BMatrix
randUpperInvMatrix n = do
  a <- randMatrix n n
  pure $ B.make n n $ \i j -> 
                if i == j then True 
                else if i < j then B.get a i j
                else False

-- An arbitrary lower invertible matrix
randLowerInvMatrix :: Int -> Gen B.BMatrix
randLowerInvMatrix n = B.transpose <$> randUpperInvMatrix n

-- An arbitrary permutation matrix
randPermMatrix :: Int -> Gen B.BMatrix
randPermMatrix n = P.toMatrix . P.fromList <$> shuffle [0..n-1]
  
-- An arbitrary invertible matrix
randInvMatrix :: Int -> Gen B.BMatrix
randInvMatrix n = do
  upper <- randUpperInvMatrix n
  lower <- randLowerInvMatrix n
  perm <- randPermMatrix n
  pure $ upper `B.mult` lower `B.mult` perm


instance Arbitrary B.BMatrix where
  arbitrary = frequency 
    [ (1, pure B.empty)
    , -- We have to hack around the limitation that there can only be 
      -- a single size parameter for a generator
      (5, sized $ \s ->
        do let s' = max s 2
           rows <- choose (1, s'-1)
           randMatrix rows (s' - rows))
    , (1, sized $ \n -> randPermMatrix n)  
    , (5, sized $ \n -> randInvMatrix n)
    ]

  shrink a = (if c >= 2 then pairToList (B.hsplit (c `div` 2) a) else [])
           ++ (if r >= 2 then pairToList (B.vsplit (r `div` 2) a) else [])
    where pairToList (x, y) = [x, y]
          r = B.rows a
          c = B.cols a
    
bmmcTests :: TestTree
bmmcTests = testGroup "BMMC Tests" 
  [ testProperty "empty-size" emptySizeProp
  , testProperty "id-size" idSizeProp
  , testProperty "transpose-size" transposeSizeProp
  , testProperty "echelon-size" echelonSizeProp
  , testProperty "inv-size" invSizeProp
  , testProperty "hstack-size" hstackSizeProp
  , testProperty "vstack-size" vstackSizeProp
  , testProperty "hsplit-size" hsplitSizeProp
  , testProperty "vsplit-size" vsplitSizeProp
  , testProperty "row-from-to-int" rowFromToInt
  , testProperty "row-to-from-int" $
      forAllShrink (sized $ \n -> randMatrix 1 n) shrink rowToFromInt
  , testProperty "col-from-to-int" colFromToInt
  , testProperty "col-to-from-int" $
      forAllShrink (sized $ \n -> randMatrix n 1) shrink colToFromInt
  , testProperty "mult-size" multSizeProp
  , testProperty "inv" invProp
  , testProperty "inv-inv" invInvProp
  , testProperty "inv-mult" invMultProp
  , testProperty "rank-bounds" rankBoundsProp
  , testProperty "rank-inv" rankInvProp
  , testProperty "transform-range" transformRangeProp
  ]

emptySizeProp :: Property
emptySizeProp = B.rows B.empty == 0 .&&. B.cols B.empty == 0

idSizeProp :: Int -> Property 
idSizeProp n = 0 <= n ==> B.rows (B.identity n) == n && B.cols (B.identity n) == n

transposeSizeProp :: B.BMatrix -> Property
transposeSizeProp a = B.rows (B.transpose a) == B.cols a .&&. B.cols (B.transpose a) == B.rows a

echelonSizeProp :: B.BMatrix -> Property
echelonSizeProp a = B.rows b == B.rows a .&&. B.cols b == B.cols a
  where b = B.reducedRowEchelon a

invSizeProp :: B.BMatrix -> Property
invSizeProp a = 
  B.isInvertible a ==>
    B.rows b == B.rows a .&&. B.cols b == B.cols a
  where b = B.unsafeInverse a

hstackSizeProp :: B.BMatrix -> B.BMatrix -> Property
hstackSizeProp a b = 
  B.rows a == B.rows b ==> 
    B.rows c == B.rows a && B.cols c == B.cols a + B.cols b
  where c = B.hstack a b

vstackSizeProp :: B.BMatrix -> B.BMatrix -> Property
vstackSizeProp a b = 
  B.cols a == B.cols b ==> 
    B.cols c == B.cols a && B.rows c == B.rows a + B.rows b
  where c = B.vstack a b
  
hsplitSizeProp :: B.BMatrix -> Int -> Property
hsplitSizeProp c k =
  0 <= k ==> k <= B.cols c ==>
    B.rows a == B.rows c && B.rows b == B.rows c && B.cols a == k && B.cols b == B.cols c - k
  where (a, b) = B.hsplit k c

vsplitSizeProp :: B.BMatrix -> Int -> Property
vsplitSizeProp c k =
  0 <= k ==> k <= B.rows c ==>
    B.cols a == B.cols c && B.cols b == B.cols c && B.rows a == k && B.rows b == B.rows c - k
  where (a, b) = B.vsplit k c

rowToFromInt :: B.BMatrix -> Property
rowToFromInt r = 
  B.rows r == 1 ==>
    B.rowFromInt (B.cols r) (B.rowToInt r) === r

rowFromToInt :: Integer -> Int -> Property
rowFromToInt x n = 
  0 < n ==> 0 <= x ==> x < 2^n ==>
    B.rowToInt (B.rowFromInt n x) === x

colToFromInt :: B.BMatrix -> Property
colToFromInt c = 
  B.cols c == 1 ==>
    B.colFromInt (B.rows c) (B.colToInt c) === c

colFromToInt :: Integer -> Int -> Property
colFromToInt x n = 
  0 < n ==> 0 <= x ==> x < 2^n ==>
    B.colToInt (B.colFromInt n x) === x

multSizeProp :: B.BMatrix -> B.BMatrix -> Property
multSizeProp a b =
  B.cols a == B.rows b ==>
    B.rows c == B.rows a && B.cols c == B.cols b
  where c = a `B.mult` b

isSorted :: Ord a => [a] -> Bool
isSorted xs = xs == sort xs

invProp :: B.BMatrix -> Property
invProp a = 
  B.isInvertible a ==> 
    b `B.mult` a == B.identity n && a `B.mult` b == B.identity n
  where n = B.rows a
        b = B.unsafeInverse a

invInvProp :: B.BMatrix -> Property
invInvProp a = 
  B.isInvertible a ==> 
    (B.inverse <$> B.inverse a) == Just (Just a)

invMultProp :: B.BMatrix -> B.BMatrix -> Property
invMultProp a b =
  B.isInvertible a ==> B.isInvertible b ==> B.cols a == B.rows b ==>
    B.inverse (a `B.mult` b) == liftM2 B.mult (B.inverse b) (B.inverse a)

rankBoundsProp :: B.BMatrix -> Property
rankBoundsProp a = 0 <= B.rank a .&&. B.rank a <= B.rows a && B.rank a <= B.cols a

rankInvProp :: B.BMatrix -> Property
rankInvProp a = B.isInvertible a === (B.rank a == B.rows a && B.isSquare a)

transformRangeProp :: B.BMatrix -> Integer -> Property
transformRangeProp a i =
  B.isSquare a ==> 0 <= i ==> i < 2^n ==> 
    0 <= B.transformInt a i && B.transformInt a i < 2^n
  where n = B.rows a