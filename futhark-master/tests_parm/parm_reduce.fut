
def parm 'a 'b [n] [m] [k] (masks : [k]i64) (f : [m]a -> [m]b) (xs : [n]a) : *[n]b =
  intrinsics.parm masks f xs

entry main (xs : [64]i32) =
  let ys = parm [ 3, 2 ] (map (+2) : [16]i32 -> [16]i32) xs
  in reduce_comm (+) 0 ys
  