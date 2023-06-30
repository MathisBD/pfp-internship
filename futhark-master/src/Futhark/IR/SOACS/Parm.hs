-- This module implements some helper functions 
-- to convert parm into bmmc . two . bmmc 
-- and to manipulate bmmcs.

module Futhark.IR.SOACS.Parm ( 
  parmMatrix, 
  parmNestMatrix,
  composeBmmcs,
  invertBmmc
) where

import Data.Bits ( Bits(shiftR) ) 
import Futhark.Util.BMatrix qualified as B

-- Composition is done right to left.
composeBmmcs :: (B.BMatrix, B.BMatrix) -> (B.BMatrix, B.BMatrix) -> (B.BMatrix, B.BMatrix)
composeBmmcs (mat2, c2) (mat1, c1)
  | B.rows c1 == n && B.cols c1 == 1 && 
    B.rows c2 == n && B.cols c2 == 1 && 
    B.rows mat1 == n && B.cols mat1 == n &&
    B.rows mat2 == n && B.cols mat2 == n =
      (mat2 `B.mult` mat1, (mat2 `B.mult` c1) `B.add` c2)
  | otherwise = error "composeBmmcs: dimensions do not match."
  where n = B.rows mat1
    
-- Compute the BMMC that corresponds to the inverse permutation.
invertBmmc :: (B.BMatrix, B.BMatrix) -> (B.BMatrix, B.BMatrix)
invertBmmc (mat, c) 
  | B.rows c == n && B.cols c == 1 && 
    B.rows mat == n && B.cols mat == n =
      (mat_inv, mat_inv `B.mult` c)
  | otherwise = error "invertBmmc: dimensions do not match."
  where n = B.rows mat
        mat_inv = B.unsafeInverse mat

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
