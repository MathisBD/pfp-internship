-- Test unary operators for u64.

-- ==
-- entry: negateu64
-- input { [0u64, 1u64, 18446744073709551615u64, 8u64, 18446744073709551608u64] }
-- output { [0u64, 18446744073709551615u64, 1u64, 18446744073709551608u64, 8u64] }

-- ==
-- entry: absu64
-- input { [0u64, 1u64, 18446744073709551615u64, 8u64, 18446744073709551608u64] }
-- output { [0u64, 1u64, 18446744073709551615u64, 8u64, 18446744073709551608u64] }

-- ==
-- entry: sgnu64
-- input { [0u64, 1u64, 18446744073709551615u64, 8u64, 18446744073709551608u64] }
-- output { [0u64, 1u64, 1u64, 1u64, 1u64] }

entry negateu64 = map (\x : u64 -> -x)
entry absu64 = map (u64.abs)
entry sgnu64 = map (u64.sgn)

