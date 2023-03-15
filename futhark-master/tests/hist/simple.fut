-- Test genred in simple cases with addition operator
-- ==
--
-- input  {
--   [0, 0, 0, 0, 0]
--   [1, 1, 1, 1, 1]
-- }
-- output {
--   [0, 5, 0, 0, 0]
-- }
--
-- input  {
--   [0, 0, 0, 0, 0]
--   [1, 1, 4, 4, 4]
-- }
-- output {
--   [0, 2, 0, 0, 12]
-- }
--
-- input  {
--   [1, 2, 3, 4, 5]
--   [1, 1, 4, 4, 4]
-- }
-- output {
--   [1, 4, 3, 4, 17]
-- }
--
-- input {
--   [0, 0, 0, 0, 0]
--   [10000000]
-- }
-- output {
--   [0, 0, 0, 0, 0]
-- }
--
-- input {
--   [0, 0, 0, 0, 0]
--   empty([0]i32)
-- }
-- output {
--   [0, 0, 0, 0, 0]
-- }
--
-- input {
--   empty([0]i32)
--   empty([0]i32)
-- }
-- output {
--   empty([0]i32)
-- }

def main [m][n] (hist : *[n]i32) (image : [m]i32) : [n]i32 =
  reduce_by_index hist (+) 0 (map i64.i32 image) image
