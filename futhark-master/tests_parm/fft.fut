
import "complex"

module cf32 = mk_complex f32

def log2 (_ : i64) : i64 = ???

-- length masks > 0
-- the array of masks is known at compile time
-- m == n / 2^(length masks)
def parm 'a 'b [n] [m] : []i64 -> ([m]a -> [m]b) -> [n]a -> [n]b = ???

def col 'a 'b [n] (cmask : i64) (f : a -> a -> b) (g : a -> a -> b) (xs : [n]a) : [n]b =
  let k = log2 n
  let (pmasks, _) = 
    let pmasks0 = replicate (k-1) 0 in
    loop (pmasks, cmask) = (pmasks0, cmask) for i < k-1 do
      let pmasks' = pmasks with [i] = ??? cmask
      let cmask' = ??? cmask
      in (pmasks', cmask') 
  in parm pmasks (\(ys : [2]a) -> [ f ys[0] ys[1], g ys[0] ys[1] ]) xs

def bit_reverse 'a [n] (xs : [n]a) : [n]a =
  let k = log2 n 
  let matrix = tabulate_2d k k (\i j -> if i + j == k - 1 then 1 else 0)
  in bmmc matrix xs

def fft [n] (xs : [n]cf32.t) : [n]cf32.t =
  let k = log2 n in
  loop xs = bit_reverse xs for i < k do 
    let twiddle : cf32.t = ??? i
    in col (1 << i) (\x y -> cf32.(x + twiddle * y)) (\x y -> cf32.(x - twiddle * y)) xs

def main (xs : [8]cf32.t) : [8]cf32.t =
  fft xs