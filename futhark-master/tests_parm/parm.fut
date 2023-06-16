


def parm 'a 'b [n] [m] [k] (masks : [k]i64) (f : [m]a -> [m]b) (xs : [n]a) : *[n]b =
  intrinsics.parm masks f xs

entry main (xs : [16]i32) =
  parm [ 1 ] 
    (reverse : [8]i32 -> [8]i32) 
    xs
