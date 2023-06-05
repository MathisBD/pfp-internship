


def parm 'a 'b [n] [m] [k] (masks : [k]i64) (f : [m]a -> [m]b) (xs : [n]a) : *[n]b =
  intrinsics.parm masks f xs

entry main (xs : [128]i32) =
  parm [ 1, 3 ] 
    (map f32.i32 : [32]i32 -> [32]f32) 
    xs
