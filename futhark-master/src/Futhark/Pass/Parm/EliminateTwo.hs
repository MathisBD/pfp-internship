-- This pass replaces uses of Two using Reshape and Map.
-- This is a simplification pass that does at least as much simplication 
-- as Futhark.Pass.Simplify.simplifySOACS does. 
module Futhark.Pass.Parm.EliminateTwo ( eliminateTwo ) where

import Control.Monad
import Futhark.IR.SOACS
import Futhark.Construct
import Futhark.Pass
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify as Simplify
import Futhark.IR.SOACS.Simplify ( simpleSOACS, soacRules, asSOAC )

-- This is a standard SOACS simplification pass, with one rule extra.
eliminateTwo :: Pass SOACS SOACS
eliminateTwo = 
  Pass "eliminate Two" "Replace every occurence of Two by Reshape . Map . Reshape" $
    Simplify.simplifyProg 
      simpleSOACS 
      (soacRules <> ruleBook [RuleOp elimRule] mempty) 
      Simplify.noExtraHoistBlockers

elimRule :: TopDownRuleOp (Wise SOACS)
elimRule _ pat aux op 
  | Just (Two nest arrs_len arrs lam) <- asSOAC op = Simplify $ auxing aux $ do
      chunk_size <- letSubExp "chunk_count" =<< 
        eBinOp (LShr Int64) (eSubExp arrs_len) (eSubExp nest)
      chunk_count <- letSubExp "chunk_size" =<<
        eBinOp (Shl Int64) (eSubExp $ intConst Int64 1) (eSubExp nest)

      -- Reshape the input arrays.
      arrs' <- forM arrs $ \arr -> 
        letExp "two" $ 
          BasicOp $ Reshape ReshapeArbitrary (Shape [chunk_count, chunk_size]) arr

      -- Map the lambda.
      arrs'' <- letTupExp "two" $ 
        Op $ Screma chunk_count arrs' (ScremaForm [] [] lam)

      -- Flatten the arrays.
      forM_ (zip (patElems pat) arrs'') $ \(pe, arr) ->
        letBind (Pat [pe]) $ 
          BasicOp $ Reshape ReshapeArbitrary (Shape [arrs_len]) arr
elimRule _ _ _ _ = Skip 
