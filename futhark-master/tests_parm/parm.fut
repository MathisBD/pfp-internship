


def parm 'a 'b [n] [m] [k] (masks : [k]i64) (f : [m]a -> [m]b) (xs : [n]a) : *[n]b =
  intrinsics.parm masks f xs

def replicateMin [n] (xs : [n]i32) : [n]i32 =
  replicate n (reduce i32.min i32.highest xs)

entry main (xs : [64]i64) =
  parm [ 3 ] 
    (\(xs' : [32](i64, i64)) : [32]i64 -> map (uncurry (+)) xs')
    (zip xs1 xs2)
