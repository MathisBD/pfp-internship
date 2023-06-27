


def parm 'a 'b [n] [m] [k] (masks : [k]i64) (f : [m]a -> [m]b) (xs : [n]a) : *[n]b =
  intrinsics.parm masks f xs

def replicateMin [n] (xs : [n]i32) : [n]i32 =
  replicate n (reduce i32.min i32.highest xs)

entry main (x : i64) =
  parm [ 4, 1, 1 ] 
    (map (+0) : [256]i64 -> [256]i64) 
    (map (+x) (iota 2048))
