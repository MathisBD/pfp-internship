

makeName :: Int -> String
makeName i = "kernel" <> show i
  
genTable :: [a] -> (a -> (Int, Int, Int)) -> (a -> (Int, Int, Int)) -> String
genTable xs blockCount blockSize = unlines $ 
  [ "std::vector<void*> kernels = {" ] ++
  zipWith genKernel xs [0..] ++
  [ "};\nstd::vector<dim3> block_counts = {" ] ++
  zipWith (genDim3 blockCount) xs [0..] ++
  [ "};\nstd::vector<dim3> block_sizes = {" ] ++
  zipWith (genDim3 blockSize) xs [0..] ++
  [ "};\n" ]
  where genKernel x i = "    (void*)" <> makeName i <> coma 
          where coma | i < length xs - 1 = ","
                     | otherwise = ""
        genDim3 dim x i = "    { " <> show dx <> ", " <> show dy <> ", " <> show dz <> " }" <> coma
          where (dx, dy, dz) = dim x
                coma | i < length xs - 1 = ","
                     | otherwise = ""

genKernels :: [a] -> (a -> String -> String) -> String
genKernels xs gen =
  unlines $ zipWith (\x i -> gen x (makeName i)) xs [0..]  

main :: IO ()
main = do 
  let n = 30
      p = 5
      --iters = 3
      count = 1000
      cols mat = assert (length cols0 >= p) $ take p cols0
        where cols0 = filter (\j -> all (\i -> not (B.get mat i j)) [p..n-1]) [0..n-1]
      q mat = tally (cols  mat) $ \j -> j >= p
  mats <- replicateM count (Test.Tasty.QuickCheck.generate $ randInvMatrix n)
  let tiledMats = mats
        --flip concatMap mats $ \mat -> 
        --  let (upper, lower, perm) = B.decomposeULP mat 
        --      rev = B.fromPerm (P.reverse n)
        --  in [ upper `B.mult` rev, rev `B.mult` lower `B.mult` B.fromPerm perm ]
      kernels = genKernels tiledMats $ \mat name -> generateBmmc name mat
      table = genTable tiledMats (\mat -> (2^(n - 10), 1, 1)) (\mat -> (2^10, 1, 1)) 
  putStrLn kernels
  putStrLn table 
