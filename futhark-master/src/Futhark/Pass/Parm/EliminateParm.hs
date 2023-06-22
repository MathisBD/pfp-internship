-- This pass replaces uses of Parm using Bmmc and Two.
-- This is a simplification pass that does at least as much simplication 
-- as Futhark.Pass.Simplify.simplifySOACS does. 
-- There is no guarantee that all occurences of Parm can be replaced :
-- if there are some left the compiler will raise an error later on.
module Futhark.Pass.Parm.EliminateParm ( eliminateParm ) where

import Control.Monad
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.IR.SOACS
import Futhark.IR.SOACS.Parm
import Futhark.Construct
import Futhark.Pass
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify as Simplify
import Futhark.IR.SOACS.Simplify ( simpleSOACS, soacRules, asSOAC )
import Futhark.Util.BMatrix qualified as B


-- This is a standard SOACS simplification pass, with one rule extra.
eliminateParm :: Pass SOACS SOACS
eliminateParm = 
  Pass "eliminate Parm" "Replace every occurence of Parm by Bmmc . Two . Bmmc" $
    Simplify.simplifyProg 
      simpleSOACS 
      (soacRules <> ruleBook [RuleOp elimRule] mempty) 
      Simplify.noExtraHoistBlockers

elimRule :: TopDownRuleOp (Wise SOACS)
elimRule vtable pat aux op
  | Just (Parm masks_len masks arrs_len arrs lam) <- asSOAC op,
    -- We need the array length to be known at compile time.
    Just arrs_len_const <- compileTimeInt64 vtable arrs_len,
    -- We need the masks array to be known at compile time.
    Just masks_const <- compileTimeInt64Array vtable (Var masks),
    -- The length of input arrays must be a power of two.
    Just n <- log2 arrs_len_const = Simplify $ auxing aux $ do
      -- Compute the BMMC matrices we will need.
      let mat = parmNestMatrix (fromIntegral n) (map toInteger masks_const)
          Just mat_inv = B.inverse mat
          compl = B.zeros (fromIntegral n) 1

      -- Permute the arrays using mat.
      arrs1 <- forM arrs $ \arr -> do 
        letExp "parm_bmmc" $ BasicOp $ Bmmc mat compl arr
         
      -- Apply the Two combinator.
      let nest = masks_len
      arrs2 <- letTupExp "parm_two" $ Op $ Two nest arrs_len arrs1 lam

      -- Permute the arrays using mat_inv.
      forM_ (zip (patElems pat) arrs2) $ \(p, arr) -> do
        letBind (Pat [p]) $ BasicOp $ Bmmc mat_inv compl arr
      
    where -- The logarithm of a power of two.
          log2 :: (Integral a) => a -> Maybe a
          log2 n = go 0
            where go i | 2^i < n   = go (i+1)
                       | 2^i == n  = Just i
                       | otherwise = Nothing
elimRule _ _ _ _ = Skip

compileTimeInt64 :: ST.SymbolTable rep -> SubExp -> Maybe Int64
compileTimeInt64 vtable (Var v) = case ST.lookupExp v vtable of 
  Just (BasicOp (SubExp (Constant (IntValue (Int64Value val)))), _) -> Just val
  _ -> Nothing
compileTimeInt64 _ (Constant (IntValue (Int64Value val))) = Just val
compileTimeInt64 _ _ = Nothing

compileTimeInt64Array :: ST.SymbolTable rep -> SubExp -> Maybe [Int64]
compileTimeInt64Array vtable (Var v) = case ST.lookupExp v vtable of 
  Just (BasicOp (ArrayLit arr _), _) -> 
    traverse (compileTimeInt64 vtable) arr
  -- The simplify engine will sometimes replace array literals with replicate.
  Just (BasicOp (Replicate (Shape [count]) x), _) -> do
    count' <- compileTimeInt64 vtable count 
    x' <- compileTimeInt64 vtable x
    pure $ replicate (fromIntegral count') x'
  _ -> Nothing
compileTimeInt64Array _ _ = Nothing
           
