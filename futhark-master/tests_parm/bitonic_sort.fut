

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
  
-- m == n / 2^nest
def nested_two 'a 'b [n] [m] (nest : i64) (f : [m]a -> [m]b) (xs : [n]a) : [n]b =
  let k = log2 n
  let pmasks = 
    loop pmasks = replicate nest 0 for i < nest do
      pmasks with [i] = 1 << (k-1-i)
  in parm pmasks f xs

def bmerge [n] (xs : [n]i32) : [n]i32 =
  let k = log2 n in
  -- the first column
  let xs = col ((1 << k) - 1) i32.min i32.max xs in
  -- the remaining columns
  loop xs = xs for i < k-1 do 
    col (1 << (k - i - 2)) i32.min i32.max xs

def bsort [n] (xs : [n]i32) : [n]i32 =
  let k = log2 n in
  loop xs = xs for i < k do
    let n2 = n >> (k-i-1)
    in nested_two (k-i-1) (bmerge : [n2]i32 -> [n2]i32) xs
  
entry main (xs : [64]i32) : [64]i32 =
  bsort xs