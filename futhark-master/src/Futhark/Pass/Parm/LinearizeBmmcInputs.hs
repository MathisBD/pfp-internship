-- This pass ensures that the inputs to Bmmc permutations are 
-- linear (contiguous) in memory.
module Futhark.Pass.Parm.LinearizeBmmcInputs ( linearizeBmmcInputs ) where 

import Control.Applicative
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Analysis.UsageTable as UT
import Futhark.IR.GPUMem
import Futhark.IR.Mem.Simplify ( blockers )
import Futhark.Construct
import Futhark.Pass
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify as Simplify
import Futhark.Optimise.Simplify.Engine as Engine
import Futhark.IR.Mem.IxFun ( linearWithOffset )


linearizeBmmcInputs :: Pass GPUMem GPUMem
linearizeBmmcInputs = 
  Pass "linearize BMMC inputs" "Make sure the input to every BMMC permutation is linear in memory." $
    Simplify.simplifyProg 
      simpleGPUMem 
      (ruleBook [RuleBasicOp elimRule] mempty) 
      (blockers { Engine.blockHoistBranch = blockAllocs })
  where 
    blockAllocs :: ST.SymbolTable (Wise GPUMem) -> UT.UsageTable -> Stm (Wise GPUMem) -> Bool
    blockAllocs vtable _ (Let _ _ (Op Alloc {})) =
      not $ ST.simplifyMemory vtable
    -- Do not hoist statements that produce arrays.  This is
    -- because in the KernelsMem representation, multiple
    -- arrays can be located in the same memory block, and moving
    -- their creation out of a branch can thus cause memory
    -- corruption.  At this point in the compiler we have probably
    -- already moved all the array creations that matter.
    blockAllocs _ _ (Let pat _ _) =
      not $ all primType $ patTypes pat

elimRule :: TopDownRuleBasicOp (Wise GPUMem)
elimRule vtable (Pat [pe]) aux (Bmmc mat compl v) 
  | Just entry <- ST.lookup v vtable,
    Just (t, ixfun) <- letBoundIxFun entry <|> fparamIxFun entry <|> lparamIxFun entry,
    Nothing <- linearWithOffset ixfun (primByteSize t) =
      Simplify $ auxing aux $ do
        tmp <- letExp "bmmc_manifest" $ BasicOp $ Manifest [0] v
        letBind (Pat [pe]) $ BasicOp $ Bmmc mat compl tmp
    where 
      letBoundIxFun :: ST.Entry (Wise GPUMem) -> Maybe (PrimType, IxFun)
      letBoundIxFun entry = do 
        (_, MemArray t _ _ (ArrayIn _ ixfun)) <- ST.entryLetBoundDec entry
        pure (t, ixfun)
      fparamIxFun :: ST.Entry (Wise GPUMem) -> Maybe (PrimType, IxFun)
      fparamIxFun entry = do 
        MemArray t _ _ (ArrayIn _ ixfun) <- ST.entryFParam entry
        pure (t, ixfun)
      lparamIxFun :: ST.Entry (Wise GPUMem) -> Maybe (PrimType, IxFun)
      lparamIxFun entry = do 
        MemArray t _ _ (ArrayIn _ ixfun) <- ST.entryLParam entry
        pure (t, ixfun)
           
elimRule _ _ _ _ = Skip
