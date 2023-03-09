module Main ( main ) where 

import Data.Bits
import Text.Printf

--msbOnly :: Int -> Int 
--msbOnly n = n .&. (n-1)

parmGroups :: Int -> Int -> [Bool]
parmGroups mask n | mask <= 0 || n <= 0 = undefined
parmGroups mask n = [ odd $ popCount (mask .&. i) `mod` 2 | i <- [0..n-1] ]

groupIndex :: [Bool] -> [Int]
groupIndex = go 0 0 
  where go i j [] = []
        go i j (False:xs) = i : go (i+1) j xs
        go i j (True:xs)  = j : go i (j+1) xs


main :: IO ()
main = do 
  let mask = 12
      n = 16
      groups = parmGroups mask n
      indices = groupIndex groups
  putStr $ concat $ zipWith prettyGroup [(0::Int)..] groups
  putStr $ concat $ zipWith prettyIndex [(0::Int)..] indices
    where prettyGroup i False = printf "%d   \n" i
          prettyGroup i True  = printf "   %d\n" i
          prettyIndex i idx   = printf "%4b |---> %4b\n" i idx
          