-- Copy benchmark
-- == 
-- entry: test_copy
-- random input { [33554432]i32 } 

entry test_copy (arr : []i32) = copy arr

-- Transposition benchmarks
-- ==
-- entry: test_transpose
-- random input { [4194304][8]i32 }
-- random input { [2097152][16]i32 }
-- random input { [1048576][32]i32 }
-- random input { [524288][64]i32 }
-- random input { [262144][128]i32 }
-- random input { [131072][256]i32 }
-- random input { [65536][512]i32 }
-- random input { [32768][1024]i32 }
-- random input { [16384][2048]i32 }
-- random input { [8192][4096]i32 }
-- random input { [4096][8192]i32 }
-- random input { [2048][16384]i32 }
-- random input { [1024][32768]i32 }
-- random input { [512][65536]i32 }
-- random input { [256][131072]i32 }
-- random input { [128][262144]i32 }
-- random input { [64][524288]i32 }
-- random input { [32][1048576]i32 }
-- random input { [16][2097152]i32 }
-- random input { [8][4194304]i32 }

entry test_transpose (arr : [][]i32) = transpose arr

