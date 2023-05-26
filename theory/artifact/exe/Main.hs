module Main ( main ) where 

import Data.List ( sort, transpose, intersperse, intercalate )
import Data.List.Unique ( count )
import Control.Monad ( forM_, liftM2, replicateM )
import Data.Traversable ( for )
import System.Random ( randomIO, randomRIO )
import qualified Bmmc as B
import qualified Data.Vector.Unboxed as U
import qualified Perm as P
import KernelGen
import qualified Control.Category as P
import Text.Printf ( printf )
import Control.Exception ( assert )
import Data.Bits ( Bits(shiftR) ) 
import qualified Test.Tasty.QuickCheck as QC


-- This example shows how to generate the various
-- kernels for a random BPC permutation.
exampleBPC :: IO ()
exampleBPC = do 
  let n = 25
      p = 5
      iters = 3
  -- Generate a random BP permutation.
  perm <- QC.generate $ P.randPerm n
  putStrLn $ "\nThe permutation is : " <> show perm <> "\n"
  -- BPC kernels.
  putStrLn $ generate       "BPC_naive_kernel"            perm
  putStrLn $ generateC      "BPC_tile_kernel"             perm p
  putStrLn $ generateCB     "BPC_tile_banks_kernel"       perm p
  putStrLn $ generateCI     "BPC_tile_iters_kernel"       perm p iters
  putStrLn $ generateCBI    "BPC_tile_banks_iters_kernel" perm p iters
  -- Tiled BMMC kernels.
  putStrLn $ generateBmmc   "BMMC_naive_kernel"           (B.fromPerm perm)
  putStrLn $ generateBmmcC  "BMMC_tile_kernel"            (B.fromPerm perm) p
  putStrLn $ generateBmmcCB "BMMC_tile_banks_kernel"      (B.fromPerm perm) p

-- This example shows how to generate the two tiled BMMC kernels
-- corresponding to a random BMMC permutation.
exampleBMMC :: IO ()
exampleBMMC = do 
  let n = 25
      p = 5
  -- Generate a random BMMC permutation.
  mat <- QC.generate $ B.randInvMatrix n
  --putStrLn $ "\nThe BMMC matrix is :\n" <> show mat <> "\n"
  -- Factorize the BMMC matrix.
  let (upper, lower, perm) = B.decomposeULP mat
      rev = B.fromPerm (P.reverse n)
      tiled1 = rev `B.mult` lower `B.mult` B.fromPerm perm
      tiled2 = upper `B.mult` rev 
  putStrLn $ "\nIs the decomposition correct ? " <> show (mat == tiled2 `B.mult` tiled1) <> "\n"
  -- Tiled BMMC kernels.
  putStrLn $ generateBmmcC "BMMC_kernel_first" tiled1 p
  putStrLn $ generateBmmcC "BMMC_kernel_second" tiled2 p
  
-- Change this to use the example you want.
main :: IO ()
main = exampleBPC
