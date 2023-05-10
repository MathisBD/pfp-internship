module Main ( main ) where

import Test.Tasty
import PermTests
import BmmcTests


main :: IO ()
main = defaultMain $ testGroup "All Tests" 
  [ permTests
  , bmmcTests
  --, kernelTests
  ]

