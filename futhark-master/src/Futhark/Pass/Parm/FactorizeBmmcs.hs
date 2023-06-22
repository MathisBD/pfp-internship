-- This pass factorizes every BMMC into a product of two tiled BMMCs.
-- These are tiled regardless of the tile bit count we actually use later on (in ImpGen).
module Futhark.Pass.Parm.FactorizeBmmcs ( factorizeBmmcs ) where

import Data.List ( sort )
import Futhark.IR.GPU
import Futhark.IR.GPU.Simplify 
import Futhark.Pass
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify as Simplify
import Futhark.Util.BMatrix qualified as B
import Futhark.Util.Perm qualified as P
import Futhark.Construct


-- This is a standard GPU simplification pass, with one rule extra.
factorizeBmmcs :: Pass GPU GPU
factorizeBmmcs = 
  Pass "factorize BMMCs" "Factorize every non-tiled BMMC into a product of two tiled BMMCs" $
    Simplify.simplifyProg 
      simpleGPU 
      (kernelRules <> ruleBook [RuleBasicOp elimRule] mempty) 
      Simplify.noExtraHoistBlockers

elimRule :: TopDownRuleBasicOp (Wise GPU)
elimRule _ (Pat [pe]) aux (Bmmc mat compl v) 
  | not (isPseudoTriangular mat) = Simplify $ auxing aux $ do
      v' <- letExp "bmmc_tmp" $
        BasicOp $ Bmmc tiled1 (B.zeros n 1) v
      letBind (Pat [pe]) $
        BasicOp $ Bmmc tiled2 compl v'
    where (upper, lower, perm) = B.decomposeULP mat
          rev = B.fromPerm (P.reverse n)
          tiled1 = rev `B.mult` lower `B.mult` B.fromPerm perm
          tiled2 = upper `B.mult` rev 
          n = B.rows mat
elimRule _ _ _ _ = Skip

-- Can we permute the columns of this (square) matrix so that it is upper triangular ?
isPseudoTriangular :: B.BMatrix -> Bool
isPseudoTriangular mat = 
  isSubLinear $ sort $ map colSize [0..n-1]
  where isSubLinear xs = 
          and $ zipWith (<=) xs [1..]
        colSize j = case filter (\i -> B.get mat i j) [0..n-1] of 
          [] -> 0
          idxs -> 1 + maximum idxs
        n = B.rows mat