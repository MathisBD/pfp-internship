


def parm 'a 'b [n] [m] [k] (masks : [k]i64) (f : [m]a -> [m]b) (xs : [n]a) : *[n]b =
  intrinsics.parm masks f xs

entry main (xs : [128]i32) =
  parm [ 3 ] 
    (reverse : [64]i32 -> [64]i32) 
    xs
