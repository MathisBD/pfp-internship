module Futhark.CodeGen.ImpGen.GPU.Bmmc ( compileBmmc ) where

import Control.Monad ( unless )
import Data.Maybe
import Futhark.IR.Prop.Types
import Futhark.MonadFreshNames
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.GPU.Base
import Futhark.CodeGen.ImpCode.GPU
import Futhark.IR.Mem.IxFun qualified as IxFun
import Futhark.Util.BMatrix qualified as B
import Futhark.Util.Perm qualified as P
import Futhark.Util.IntegralExp qualified as IntExp


-- Depending on the size of the input and the Bmmc matrix, we use different BMMC kernels.
data BmmcType 
  -- Args : matrix size / column indices / matrix / complement
  = BmmcTiled Int [Int] B.BMatrix B.BMatrix
  -- Args : matrix size / column indices / permutation / complement
  | BmmcBpc Int [Int] P.Perm B.BMatrix
  -- Args : matrix size / matrix / complement
  -- We use the same kernel for small Bmmcs regardless of the shape of the matrix.
  | BmmcSmall Int B.BMatrix B.BMatrix
  
nTile, nIter :: Int
nTile = 5
-- Each thread processes 3 input elements. 
nIter = 3

tally :: (a -> Bool) -> [a] -> Int
tally f = length . filter f

-- We generate a seperate kernel for each BMMC statement.
-- The code depends heavily on the parameters, so we can hardly 
-- reuse a kernel for several BMMC statements 
-- (unless maybe they involved arrays of the same size and with the same BMMC matrices).
compileBmmc :: VName -> B.BMatrix -> B.BMatrix -> VName -> CallKernelGen ()
compileBmmc dest mat compl src = do
  ArrayEntry (MemLoc dest_mem _ dest_ixfun) dest_t <- lookupArray dest
  ArrayEntry (MemLoc src_mem _ src_ixfun) src_t <- lookupArray src

  unless (dest_t == src_t) $ error "compileBmmc: src and dest element types should match."

  -- Choose a unique name for the kernel.
  fname <- askFunction
  tag <- baseTag <$> newVName ""
  let name = keyWithEntryPoint fname 
           $ nameFromString 
           $ "bmmc_" <> prettyString dest_t <> "_" <> prettyString tag

  -- The generated kernel is only used in this statement.
  emit $ Op $ CallKernel $ 
    bmmcKernel 
      name
      dest_mem 
      (elements $ ixfunOffset dest_ixfun dest_t) 
      src_mem 
      (elements $ ixfunOffset src_ixfun src_t) 
      dest_t 
      kind

  where ixfunOffset ixfun t
          | Just ofs <- IxFun.linearWithOffset ixfun (primByteSize t) = ofs `IntExp.quot` primByteSize t
          | otherwise = error "compileBmmc: expected a linear array in memory."
        kind  
          | Just perm <- B.isPerm mat, 
            nTile + tally (>= nTile) cols + nIter <= n = BmmcBpc n cols perm compl
          | nTile + tally (>= nTile) cols <= n         = BmmcTiled n cols mat compl
          | otherwise                                  = BmmcSmall n mat compl
        cols = fromMaybe 
          (error "compileBmmc: general Bmmcs should be factored in tiled Bmmcs.")
          (isTiled mat)
        n = B.rows mat 

-- Assumes the matrix is invertible.
isTiled :: B.BMatrix -> Maybe [Int]
isTiled mat 
  | length cols == nTile && B.isInvertible subMat = Just cols
  | otherwise = Nothing
  where subMat = B.make nTile nTile $ \i j -> B.get mat i (cols !! j) 
        -- This should always have length at most equal to tileBits (because mat is invertible).
        cols = filter (\j -> all (\i -> not $ B.get mat i j) [nTile..n-1]) [0..n-1]
        n = B.rows mat

bmmcKernel :: 
  Name ->
  VName -> 
  Count Elements (TExp Int64) -> 
  VName -> 
  Count Elements (TExp Int64) -> 
  PrimType -> 
  BmmcType -> 
  Kernel
bmmcKernel name dest_mem dest_ofs src_mem src_ofs t kind =
  Kernel 
    { kernelBody = bmmcKernelCode dest_mem dest_ofs src_mem src_ofs t kind,
      kernelUses = uses,
      Futhark.CodeGen.ImpCode.GPU.kernelNumGroups = map untyped num_groups,
      Futhark.CodeGen.ImpCode.GPU.kernelGroupSize = map (Left . untyped) group_size,
      kernelName = name,
      kernelFailureTolerant = True,
      kernelCheckLocalMemory = False
    }
  where     
    num_groups, group_size :: [TPrimExp Int64 VName]
    (num_groups, group_size) = case kind of
      BmmcSmall n _ _ -> ( [1],  [2^n] )
      BmmcBpc n cols _ _ -> ( [2^(n - nTile - q - nIter)], [2^nTile, 2^q] )
        where q = tally (>= nTile) cols
      BmmcTiled n cols _ _ -> ( [2^(n - nTile - q)], [2^nTile, 2^q] )
        where q = tally (>= nTile) cols
    
    uses =
      map (`ScalarUse` IntType Int32)
        ( namesToList 
        $ mconcat
        $ map freeIn [ dest_ofs, src_ofs]
        )
      ++ map MemoryUse [dest_mem, src_mem]


bmmcKernelCode :: 
  VName -> 
  Count Elements (TExp Int64) -> 
  VName -> 
  Count Elements (TExp Int64) -> 
  PrimType -> 
  BmmcType -> 
  KernelCode 
bmmcKernelCode dest_mem dest_ofs src_mem src_ofs t kind = case kind of 
  BmmcSmall n mat compl -> mkBmmc $ mconcat
    [ 
    ]
  BmmcBpc n cols perm compl -> mkBmmc $ mconcat 
    [
    ]
  BmmcTiled n cols mat compl -> mkBmmc $ mconcat
    [ dec in_idx $ ve64 0
    , dec in_tile_idx $ ve64 0
    -- TODO : compute the indices.
    , Read val src_mem (elements $ unCount src_ofs + le64 in_idx) t (Space "global") Nonvolatile   
    , Write tile (elements $ le64 in_tile_idx) t (Space "local") Nonvolatile (var val t)     
    , Op $ Barrier FenceLocal
    , dec out_idx $ ve64 0
    , dec out_tile_idx $ ve64 0
    -- TODO : compute the indices.
    , Read val tile (elements $ le64 out_tile_idx) t (Space "local") Nonvolatile
    , Write dest_mem (elements $ unCount dest_ofs + le64 out_idx) t (Space "global") Nonvolatile (var val t)
    ]
  where 
    mkBmmc body = mconcat 
      [ DeclareMem tile (Space "local")
      , Op (LocalAlloc tile tile_bytes)
      , get_ids 
      , body
      ]

    dec v (TPrimExp e) =
      DeclareScalar v Nonvolatile (primExpType e) <> SetScalar v e

    ve64 :: (Integral t) => t -> TPrimExp Int64 v
    ve64 x = TPrimExp $ ValueExp $ IntValue $ intValue Int64 x

    tile_bytes = bytes $ case kind of
      BmmcSmall {} -> 1 :: TExp Int64
                      -- Not used, but AMD's OpenCL
                      -- does not like zero-size local memory.
      BmmcBpc _ cols _ _ -> fromInteger $ 2^(nTile + q + nIter) * primByteSize t
        where q = tally (>= nTile) cols
      BmmcTiled _ cols _ _ -> fromInteger $ 2^(nTile + q) * primByteSize t
        where q = tally (>= nTile) cols
    
    get_ids =
      mconcat
        [ DeclareScalar thread_id Nonvolatile int32
        , Op $ GetLocalId thread_id 0
        , DeclareScalar warp_id Nonvolatile int32
        , Op $ GetLocalId warp_id 1
        , DeclareScalar block_id Nonvolatile int32
        , Op $ GetGroupId block_id 0
        ]

    -- Be extremely careful when editing this list to ensure that
    -- the names match up. Also, be careful that the tags on
    -- these names do not conflict with the tags of the
    -- surrounding code.  We accomplish the latter by using very
    -- low tags (normal variables start at least in the low
    -- hundreds).
    [  tile
     , val
     , block_id
     , warp_id
     , thread_id
     , in_idx
     , out_idx
     , in_tile_idx
     , out_tile_idx
     , in_shift
     , out_shift
     , iter
     ] =
        zipWith (flip VName) [30 ..] $
          map
            nameFromString
            [ "tile"
            , "val"
            , "block_id"
            , "warp_id"
            , "thread_id"
            , "in_idx"
            , "out_idx"
            , "in_tile_idx"
            , "out_tile_idx"
            , "in_shift"
            , "out_shift"
            , "iter"
            ]

