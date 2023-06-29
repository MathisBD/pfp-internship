{-# LANGUAGE LambdaCase #-}

module Futhark.CodeGen.ImpGen.GPU.Bmmc ( compileBmmc ) where

import Debug.Trace ( trace ) 
import Control.Monad
import Control.Monad.Reader
import Data.Bits qualified as Bits
import Data.List ( sort, sortOn, partition )
import Data.Function ( (&) )
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
  -- Args : matrix / complement
  = BmmcTiled B.BMatrix B.BMatrix
  -- Args : permutation / complement
  | BmmcBpc P.Perm B.BMatrix
  -- Args : matrix / complement
  -- We use the same kernel for small Bmmcs regardless of the shape of the matrix.
  | BmmcSmall B.BMatrix B.BMatrix
  
data BmmcEnv = BmmcEnv
  { nBits  :: Int -- Total bits count.
  , nIter  :: Int -- Iteration bits count.
  , nTile  :: Int -- Tile bits count. 
  , nOver  :: Int -- Tile overlap bits count.
  , cols   :: [Int] -- This has length nTile.
  , kind   :: BmmcType
  }

-- Shorthand for nTile - nOver.
nTO :: BmmcEnv -> Int
nTO env = nTile env - nOver env

-- Thread block bits count.
nBlock :: BmmcEnv -> Int
nBlock env = case kind env of 
  BmmcSmall {} -> 0
  BmmcBpc {}   -> nBits env - nIter env - nTile env - nTO env
  BmmcTiled {} -> nBits env - nTile env - nTO env

type BmmcM = Reader BmmcEnv

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

  -- The parameters used to generate the kernel.
  let nTile = 5
      nIter = 3

  -- The generated kernel is only used... 
  let kernel = 
        flip runReader (makeEnv nTile nIter mat compl) $ 
          bmmcKernel 
            name
            dest_mem 
            (elements $ ixfunOffset dest_ixfun dest_t) 
            src_mem 
            (elements $ ixfunOffset src_ixfun src_t) 
            dest_t 
  -- ...in this statement.
  emit $ Op $ CallKernel kernel

  where ixfunOffset ixfun t
          | Just ofs <- IxFun.linearWithOffset ixfun (primByteSize t) = ofs `IntExp.quot` primByteSize t
          | otherwise = error $ 
              "compileBmmc: expected a linear array in memory."
              
makeEnv :: Int -> Int -> B.BMatrix -> B.BMatrix -> BmmcEnv
makeEnv nTile nIter mat compl =
  BmmcEnv 
    { nBits = n
    , nIter = nIter
    , nTile = nTile
    , nOver = tally (< nTile) cols
    , cols  = cols 
    , kind  = kind 
    }
  where kind
          | Just perm <- B.isPerm mat, 
            nTile + tally (>= nTile) cols + nIter <= n = trace ("bpc:\n" <> show mat <> "\n")   $ BmmcBpc perm compl
          | nTile + tally (>= nTile) cols <= n         = trace ("tiled:\n" <> show mat <> "\n") $ BmmcTiled mat compl
          | otherwise                                  = trace "small" $ BmmcSmall mat compl
        cols
          | Just cols0 <- isTiled nTile mat = cols0
          | otherwise = error $ "makeEnv: the Bmmc matrix must be tiled.\n" <> show mat
        n = B.rows mat

-- Assumes the matrix is invertible.
isTiled :: Int -> B.BMatrix -> Maybe [Int]
isTiled nTile mat 
  | n <= nTile = Just [0..n-1]
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
  BmmcM Kernel
bmmcKernel name dest_mem dest_ofs src_mem src_ofs t = do 
  num_groups <- asks $ \env -> [ ve64 $ 2^nBlock env ] 
  group_size <- asks $ \env -> 
    case kind env of
      BmmcSmall _ _ -> [ ve64 $ 2^(nBits env)]
      BmmcBpc _ _ -> [2^(nTile env + nTO env)]
      BmmcTiled _ _ -> [2^(nTile env + nTO env)]
  
  body <- bmmcKernelCode dest_mem dest_ofs src_mem src_ofs t

  pure $ Kernel 
    { kernelBody = body,
      kernelUses = uses,
      Futhark.CodeGen.ImpCode.GPU.kernelNumGroups = map untyped num_groups,
      Futhark.CodeGen.ImpCode.GPU.kernelGroupSize = map (Left . untyped) group_size,
      kernelName = name,
      kernelFailureTolerant = True,
      kernelCheckLocalMemory = False
    }
  where     
    uses =
      map (`ScalarUse` IntType Int32)
        ( namesToList 
        $ mconcat
        $ map freeIn [ dest_ofs, src_ofs]
        )
      ++ map MemoryUse [dest_mem, src_mem]

ve64 :: Int64 -> TPrimExp Int64 v
ve64 x = TPrimExp $ ValueExp $ IntValue $ intValue Int64 x

bmmcKernelCode :: 
  VName -> 
  Count Elements (TExp Int64) -> 
  VName -> 
  Count Elements (TExp Int64) -> 
  PrimType -> 
  BmmcM KernelCode 
bmmcKernelCode dest_mem dest_ofs src_mem src_ofs t = do
  tile_bytes <- asks $ \env -> 
    bytes $ case kind env of
      BmmcSmall {} -> 1 :: TExp Int64
                      -- Not used, but AMD's OpenCL
                      -- does not like zero-size local memory.
      BmmcBpc {} -> fromInteger $ 2^(nTile env + nTO env + nIter env) * primByteSize t
      BmmcTiled {} -> fromInteger $ 2^(nTile env + nTO env) * primByteSize t
  
  -- Two masks used for address computations in the bank-conflict free versions.
  tile_mask_low <- asks $ \env -> 
    2^(nTile env) - 1 :: TExp Int64  
  tile_mask_high <- asks $ \env -> 
    (2^(nTO env) - 1) * 2^(nTile env) :: TExp Int64
  
  asks kind >>= \case  
    -- This version is uncoalesced and has only one thread group.
    BmmcSmall mat compl -> pure $ mkBmmc tile_bytes $ mconcat
      [ dec in_idx $ le64 get_local_id_0
      , dec out_idx $ ve64 0
      , DeclareScalar val Nonvolatile t
      , genMatMul mat compl out_idx in_idx 
      , Read val src_mem (elements $ unCount src_ofs + le64 in_idx) t (Space "global") Nonvolatile   
      , Write dest_mem (elements $ unCount dest_ofs + le64 out_idx) t (Space "global") Nonvolatile (var val t)
      ]
    -- This version uses tiling, iterations and has no bank conflicts.
    BmmcBpc perm compl -> do
      env <- ask
      in_idx_copies       <- genInAddrIter   in_idx       warp_id      thread_id block_id iter
      in_tile_idx_copies  <- genInBlockAddr  in_tile_idx  warp_id      thread_id
      in_shift_copies     <- genShift        in_shift     in_tile_idx
      out_idx_copies      <- genOutAddrIter  out_idx      warp_id      thread_id block_id iter
      out_tile_idx_copies <- genOutBlockAddr out_tile_idx warp_id      thread_id
      out_shift_copies    <- genShift        out_shift    out_tile_idx
      
      let (inner_in_idx_copies, outer_in_idx_copies) = 
            partition (\(_, _, v_in, _, _) -> v_in == iter) 
              in_idx_copies 
          in_idx_mask = concatMap (\(_, i_out, _, _, offsets) -> map (+ i_out) offsets) inner_in_idx_copies          
                      & bitIdxsToInteger
                      & fromInteger 
                      & Bits.complement
                      & ve64
          (inner_out_idx_copies, outer_out_idx_copies) = 
            partition (\(_, _, v_in, _, _) -> v_in == iter) $
              map (\(v_out, i_out, v_in, i_in, offsets) -> (v_out, P.apply perm i_out, v_in, i_in, offsets)) 
                out_idx_copies
          out_idx_mask = concatMap (\(_, i_out, _, _, offsets) -> map (+ i_out) offsets) inner_out_idx_copies          
                       & bitIdxsToInteger
                       & fromInteger 
                       & Bits.complement
                       & ve64
          
      pure $ mkBmmc tile_bytes $ mconcat
        [ dec block_id $ le64 get_group_id_0
        , dec warp_id $ le64 get_local_id_0 .>>. fromIntegral (nTile env)
        , dec thread_id $ le64 get_local_id_0 .&. (2^(nTile env)-1)
        , DeclareScalar val Nonvolatile t
        -- Read in the tile.
        , dec in_idx $ ve64 0
        , dec in_tile_idx $ ve64 0
        , dec in_shift $ ve64 0
        , genBitCopies in_tile_idx_copies
        , genBitCopies in_shift_copies
        , genBitCopies outer_in_idx_copies
        , For iter (ValueExp $ IntValue $ Int64Value $ 2^(nIter env)) $ mconcat 
          [ SetScalar in_idx $ untyped $ le64 in_idx .&. in_idx_mask
          , genBitCopies inner_in_idx_copies
          , Read val src_mem 
              (elements $ unCount src_ofs + le64 in_idx) 
              t (Space "global") Nonvolatile   
          , Write tile 
              (elements $ 
                (le64 iter .<<. fromIntegral (nTile env + nTO env)) +
                (le64 in_tile_idx .&. tile_mask_high) +
                ((le64 in_tile_idx + le64 in_shift) .&. tile_mask_low)) 
              t (Space "local") Nonvolatile (var val t)     
          ]
        -- Synchronize.
        , Op $ Barrier FenceLocal
        -- Write out the tile.
        , dec out_idx $ ve64 0
        , dec out_tile_idx $ ve64 0
        , dec out_shift $ ve64 0
        , genBitCopies out_tile_idx_copies
        , genBitCopies out_shift_copies
        , genBitCopies outer_out_idx_copies
        , For iter (ValueExp $ IntValue $ Int64Value $ 2^(nIter env)) $ mconcat 
          [ SetScalar out_idx $ untyped $ le64 out_idx .&. out_idx_mask
          , genBitCopies inner_out_idx_copies
          , Read val tile 
              (elements $ 
                (le64 iter .<<. fromIntegral (nTile env + nTO env)) +
                (le64 out_tile_idx .&. tile_mask_high) +
                ((le64 out_tile_idx + le64 out_shift) .&. tile_mask_low)) 
              t (Space "local") Nonvolatile
          , Write dest_mem 
              (elements $ unCount dest_ofs + (le64 out_idx .^. fromInteger (B.colToInt compl))) 
              t (Space "global") Nonvolatile (var val t)
          ]
        ]
    -- This version uses tiling and has no bank conflicts, but does not use iterations.
    -- On average this version is as fast as the BPC version, but it might be slower for edge cases.
    BmmcTiled mat compl -> do
      env <- ask
      in_idx_copies       <- genInAddrBasic  in_idx       warp_id      thread_id block_id
      in_tile_idx_copies  <- genInBlockAddr  in_tile_idx  warp_id      thread_id
      in_shift_copies     <- genShift        in_shift     in_tile_idx
      out_idx_copies      <- genOutAddrBasic out_idx_tmp  warp_id      thread_id block_id
      out_tile_idx_copies <- genOutBlockAddr out_tile_idx warp_id      thread_id
      out_shift_copies    <- genShift        out_shift    out_tile_idx
      pure $ mkBmmc tile_bytes $ mconcat
        [ dec block_id $ le64 get_group_id_0
        , dec warp_id $ le64 get_local_id_0 .>>. fromIntegral (nTile env)
        , dec thread_id $ le64 get_local_id_0 .&. (2^(nTile env)-1)
        , DeclareScalar val Nonvolatile t
        -- Read in the tile.
        , dec in_idx $ ve64 0
        , dec in_tile_idx $ ve64 0
        , dec in_shift $ ve64 0
        , genBitCopies in_idx_copies
        , genBitCopies in_tile_idx_copies
        , genBitCopies in_shift_copies
        , Read val src_mem (elements $ unCount src_ofs + le64 in_idx) t (Space "global") Nonvolatile   
        , Write tile 
            (elements $ 
              (le64 in_tile_idx .&. tile_mask_high) +
              ((le64 in_tile_idx + le64 in_shift) .&. tile_mask_low)) 
            t (Space "local") Nonvolatile (var val t)     
        -- Synchronize.
        , Op $ Barrier FenceLocal
        -- Write out the tile.
        , dec out_idx $ ve64 0
        , dec out_idx_tmp $ ve64 0
        , dec out_tile_idx $ ve64 0
        , dec out_shift $ ve64 0
        , genBitCopies out_idx_copies
        , genBitCopies out_tile_idx_copies
        , genBitCopies out_shift_copies
        , genMatMul mat compl out_idx out_idx_tmp 
        , Read val tile 
            (elements $ 
              (le64 out_tile_idx .&. tile_mask_high) +
              ((le64 out_tile_idx + le64 out_shift) .&. tile_mask_low)) 
            t (Space "local") Nonvolatile
        , Write dest_mem (elements $ unCount dest_ofs + le64 out_idx) t (Space "global") Nonvolatile (var val t)
        ]
  where 
    mkBmmc tile_bytes body = mconcat 
      [ DeclareMem tile (Space "local")
      , Op (LocalAlloc tile tile_bytes)
      , get_ids 
      , body
      ]

    dec v (TPrimExp e) =
      DeclareScalar v Nonvolatile (primExpType e) <> SetScalar v e

    get_ids =
      mconcat
        [ DeclareScalar get_local_id_0 Nonvolatile int32
        , Op $ GetLocalId get_local_id_0 0
        , DeclareScalar get_local_id_1 Nonvolatile int32
        , Op $ GetLocalId get_local_id_1 1
        , DeclareScalar get_group_id_0 Nonvolatile int32
        , Op $ GetGroupId get_group_id_0 0
        , DeclareScalar get_local_size_0 Nonvolatile int32
        , Op $ GetLocalSize get_local_size_0 0
        , DeclareScalar get_global_id_0 Nonvolatile int32
        , SetScalar get_global_id_0 $ untyped $ le32 get_group_id_0 * le32 get_local_size_0 + le32 get_local_id_0
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
     , out_idx_tmp
     , in_shift
     , out_shift
     , iter
     , get_local_id_0
     , get_local_id_1
     , get_group_id_0
     , get_global_id_0
     , get_local_size_0
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
            , "out_idx_tmp"
            , "in_shift"
            , "out_shift"
            , "iter"
            , "get_local_id_0"
            , "get_local_id_1"
            , "get_group_id_0"
            , "get_global_id_0"
            , "get_local_size_0"
            ]

-- Data to generate code that copies one or several bits
-- between two variables.
-- This : 
--     (v_out, out_idx, v_in, in_idx, offsets)
-- Corresponds to the instruction :
--     forall ofs in offsets, bit(v_out, out_idx + ofs) <- bit(v_in, in_idx + ofs)
type BitCopy = (VName, Int, VName, Int, [Int])

mergeBitCopies :: [BitCopy] -> [BitCopy]
mergeBitCopies xs = maybe xs mergeBitCopies (msum $ map (tryMerge xs) ijs)
                  & sortOn (\(_, _, v_in, in_idx, _) -> (v_in, in_idx))
  where ijs = do 
          i <- [0..length xs - 1]
          j <- [0..length xs - 1]
          pure (i, j)
        tryMerge xs' (i, j)
          | v_out1 == v_out2 && v_in1 == v_in2 && delta_in == delta_out && delta_in > 0 =
              Just $ x : removeIndices [i, j] xs'
          | otherwise = Nothing
          where (v_out1, out_idx1, v_in1, in_idx1, offsets1) = xs' !! i
                (v_out2, out_idx2, v_in2, in_idx2, offsets2) = xs' !! j
                delta_out = out_idx2 - out_idx1
                delta_in = in_idx2 - in_idx1
                x = (v_out1, out_idx1, v_in1, in_idx1, offsets1 ++ map (+ delta_in) offsets2)

genBitCopies :: [BitCopy] -> KernelCode 
genBitCopies = mconcat . map genBitCopy . mergeBitCopies

genBitCopy :: BitCopy -> KernelCode
genBitCopy (v_out, out_idx, v_in, in_idx, offsets) = 
  SetScalar v_out $ untyped $ 
    le64 v_out .|. shift (le64 v_in .&. fromInteger mask)
  where mask = bitIdxsToInteger offsets `Bits.shiftL` in_idx
        shift x
          | in_idx < out_idx  = x .<<. fromIntegral (out_idx - in_idx)
          | in_idx == out_idx = x
          | otherwise         = x .>>. fromIntegral (in_idx - out_idx)

genInAddrBasic :: VName -> VName -> VName -> VName -> BmmcM [BitCopy]
genInAddrBasic v_in_addr v_i v_j v_g = asks $ go 0 0 0 0
  -- Stitch the input bits together 
  where go addr_idx i_idx j_idx g_idx env
          | addr_idx >= nBits env = []
          -- Take bit from j
          | addr_idx < nTile env =
              (v_in_addr, addr_idx, v_j, j_idx, [0]) : go (addr_idx+1) i_idx (j_idx+1) g_idx env
          -- Take bit from i
          | addr_idx `elem` cols env = 
              (v_in_addr, addr_idx, v_i, i_idx, [0]) : go (addr_idx+1) (i_idx + 1) j_idx g_idx env
          -- Take bit from g
          | otherwise =
              (v_in_addr, addr_idx, v_g, g_idx, [0]) : go (addr_idx + 1) i_idx j_idx (g_idx+1) env

genInAddrIter :: VName -> VName -> VName -> VName -> VName -> BmmcM [BitCopy]
genInAddrIter v_in_addr v_i v_j v_g v_iter = asks $ go 0 0 0 0 0
  -- Stitch the input bits together.
  where go addr_idx i_idx j_idx g_idx iter_idx env
          | addr_idx >= nBits env = []
          -- Take bit from j
          | addr_idx < nTile env =
              (v_in_addr, addr_idx, v_j, j_idx, [0]) : go (addr_idx+1) i_idx (j_idx+1) g_idx iter_idx env
          -- Take bit from i
          | addr_idx `elem` cols env = 
              (v_in_addr, addr_idx, v_i, i_idx, [0]) : go (addr_idx+1) (i_idx + 1) j_idx g_idx iter_idx env
          -- Take bit from iter
          | iter_idx < nIter env =
              (v_in_addr, addr_idx, v_iter, iter_idx, [0]) : go (addr_idx + 1) i_idx j_idx g_idx (iter_idx+1) env
          -- Take bit from g
          | otherwise =
              (v_in_addr, addr_idx, v_g, g_idx, [0]) : go (addr_idx + 1) i_idx j_idx (g_idx+1) iter_idx env

genOutAddrBasic :: VName -> VName -> VName -> VName -> BmmcM [BitCopy]
genOutAddrBasic v_out_addr v_i v_j v_g = asks $ go 0 0 0 0 
  where -- Stitch the output bits together 
        go addr_idx i_idx j_idx g_idx env
          | addr_idx >= nBits env = []
          -- take bit from j
          | addr_idx `elem` cols env = 
              (v_out_addr, addr_idx, v_j, j_idx, [0]) : go (addr_idx + 1) i_idx (j_idx + 1) g_idx env
          -- Take bit from i
          | addr_idx < nTile env =
              (v_out_addr, addr_idx, v_i, i_idx, [0]) : go (addr_idx + 1) (i_idx + 1) j_idx g_idx env
          -- Take bit from g
          | otherwise =
              (v_out_addr, addr_idx, v_g, g_idx, [0]) : go (addr_idx + 1) i_idx j_idx (g_idx + 1) env
 
genOutAddrIter :: VName -> VName -> VName -> VName -> VName -> BmmcM [BitCopy]
genOutAddrIter v_out_addr v_i v_j v_g v_iter = asks $ go 0 0 0 0 0
  where -- Stitch the output bits together 
        go addr_idx i_idx j_idx g_idx iter_idx env
          | addr_idx >= nBits env = []
          -- take bit from j
          | addr_idx `elem` cols env = 
              (v_out_addr, addr_idx, v_j, j_idx, [0]) : go (addr_idx + 1) i_idx (j_idx + 1) g_idx iter_idx env
          -- Take bit from i
          | addr_idx < nTile env =
              (v_out_addr, addr_idx, v_i, i_idx, [0]) : go (addr_idx + 1) (i_idx + 1) j_idx g_idx iter_idx env
          -- Take bit from iter
          | iter_idx < nIter env =
              (v_out_addr, addr_idx, v_iter, iter_idx, [0]) : go (addr_idx + 1) i_idx j_idx g_idx (iter_idx+1) env
          -- Take bit from g
          | otherwise =
              (v_out_addr, addr_idx, v_g, g_idx, [0]) : go (addr_idx + 1) i_idx j_idx (g_idx + 1) iter_idx env


genInBlockAddr :: VName -> VName -> VName -> BmmcM [BitCopy]
genInBlockAddr v_iblock_addr v_i v_j = asks $ go 0 0 0
  where go addr_idx i_idx j_idx env
          | addr_idx >= nTile env + nTO env = []
          -- Take bit from j
          | addr_idx < nTile env = 
              (v_iblock_addr, addr_idx, v_j, j_idx, [0]) : go (addr_idx + 1) i_idx (j_idx+1) env
          -- Take bit from i
          | otherwise = 
              (v_iblock_addr, addr_idx, v_i, i_idx, [0]) : go (addr_idx + 1) (i_idx+1) j_idx env

genOutBlockAddr :: VName -> VName -> VName -> BmmcM [BitCopy]
genOutBlockAddr v_oblock_addr v_i v_j = asks $ go 0 0 0
  where go addr_idx i_idx j_idx env
          | addr_idx >= nTile env + nTO env = []
          -- Take bit from j
          | addr_idx `elem` cols env || addr_idx >= nTile env = 
              (v_oblock_addr, addr_idx, v_j, j_idx, [0]) : go (addr_idx + 1) i_idx (j_idx+1) env
          -- Take bit from i
          | otherwise = 
              (v_oblock_addr, addr_idx, v_i, i_idx, [0]) : go (addr_idx + 1) (i_idx+1) j_idx env

genShift :: VName -> VName -> BmmcM [BitCopy]
genShift v_shift v_block_addr = asks $ go 0 0
  where go addr_idx row_idx env
          | row_idx >= nTO env = []
          | addr_idx `elem` cols env = 
              go (addr_idx+1) row_idx env
          | otherwise = 
              (v_shift, addr_idx, v_block_addr, row_idx + nTile env, [0]) : go (addr_idx+1) (row_idx+1) env

-- Generate the code to do a BMMC matrix multiplication on indices.
-- Assumes the out variable is initialized to zero.
genMatMul :: B.BMatrix -> B.BMatrix -> VName -> VName -> KernelCode
genMatMul mat compl v_out v_in = do
  mconcat (map genRow [0..n-1]) <> genXor 
  where 
    genRow i = 
      SetScalar v_out $ untyped $ 
        le64 v_out .|. (row i .<<. fromIntegral i)
    row i =
      1 .&. popc64 (le64 v_in .&. fromInteger (B.rowToInt $ B.getRow mat i))
    popc64 te =
      isInt64 $ FunExp "popc64" [untyped te] (IntType Int64)
    genXor 
      | B.null compl = mempty
      | otherwise = 
          SetScalar v_out $ untyped $ 
            le64 v_out .^. fromInteger (B.colToInt compl)  
    n = B.rows mat        

-- Delete the elements at a the given indices in a list. 
removeIndices :: [Int] -> [a] -> [a]
removeIndices is0 xs0 = go 0 (sortUniq is0) xs0
  where go _ [] xs = xs
        go k (i:is) (x:xs) 
          | i < k     = undefined
          | i == k    = go (k+1) is xs
          | otherwise = x : go (k+1) (i:is) xs
        go _ _ _ = undefined

-- Sort a list and remove the duplicates.
sortUniq :: (Ord a) => [a] -> [a]
sortUniq xs0 = go $ sort xs0
  where go [] = []
        go [x] = [x]
        go (x1:x2:xs) 
          | x1 == x2  = go (x2:xs)            
          | otherwise = x1 : go (x2:xs)

bitIdxsToInteger :: [Int] -> Integer
bitIdxsToInteger is = foldl (Bits..|.) 0 $ map (Bits.shiftL 1) is

tally :: (a -> Bool) -> [a] -> Int
tally f = length . filter f

