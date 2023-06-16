module Futhark.CodeGen.ImpGen.GPU.Bmmc ( compileBmmc ) where

import Futhark.IR.GPUMem
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.CodeGen.ImpCode.GPU qualified as Imp
import Futhark.Util.BMatrix qualified as B


compileBmmc :: PatElem LetDecMem -> B.BMatrix -> B.BMatrix -> VName -> ImpM GPUMem HostEnv Imp.HostOp ()
compileBmmc = error "compileBmmc: not implemented yet."