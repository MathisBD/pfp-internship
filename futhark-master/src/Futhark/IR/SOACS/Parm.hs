-- This module implements some helper functions 
-- to convert parm into bmmc . two . bmmc 

module Futhark.IR.SOACS.Parm ( 
  parmMatrix, 
  parmNestMatrix 
) where

import Data.Bits ( Bits(shiftR) ) 
import Futhark.Util.BMatrix qualified as B

-- Calculate the matrix A such that for any function f :
-- parm [mask] f = bmmc A^-1 . two 1 f . bmmc A
parmMatrix :: Int -> Integer -> B.BMatrix
parmMatrix n mask = B.make n n idxfun
  where idxfun i j 
          | i < lsb mask             = j == i
          | lsb mask <= i && i < n-1 = j == i + 1
          | i == n-1                 = mask `getBit` j
          | otherwise                = undefined

-- Same as parmMatrix, but works with several masks.
parmNestMatrix :: Int -> [Integer] -> B.BMatrix
parmNestMatrix n [] = B.identity n
parmNestMatrix n (mask:masks) = 
  B.blockDiag [parmNestMatrix (n-1) masks, B.identity 1] `B.mult` parmMatrix n mask 

-- The index of the least significant bit in a positive integer
lsb :: Integer -> Int
lsb x 
  | x <= 0    = error "lsb: needs a positive argument"
  | odd x     = 0
  | otherwise = 1 + lsb (x `div` 2)

-- Get the i-th bit from an integer
getBit :: Integer -> Int -> Bool
getBit x i = odd (x `shiftR` i)
