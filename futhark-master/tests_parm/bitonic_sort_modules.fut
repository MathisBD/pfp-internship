
def log2 (n : i64) : i64 = 
  if      n == 1 then 0
  else if n == 2 then 1
  else if n == 4 then 2
  else if n == 8 then 3
  else if n == 16 then 4
  else if n == 32 then 5
  else 0

-- m == n / 2
def parm 'a 'b [n] [m] : i64 -> ([m]a -> [m]b) -> [n]a -> [n]b = ???

module type col = {
  val n : i64
  val col 'a 'b : i64 -> (a -> a -> b) -> (a -> a -> b) -> [n]a -> [n]b
}

module col_1 : col = {
  def n = 2i64
  def col 'a 'b (_ : i64) f g (xs : [n]a) : [n]b = 
    [ f xs[0] xs[1], g xs[0] xs[1] ] :> [n]b
}

module col_double (C : col) : col = {
  def n = 2 * C.n
  def col 'a 'b (_ : i64) f g (xs : [n]a) : [n]b =
    let m1 = ???
    let m2 = ???
    in parm m1 (C.col m2 f g) xs
}

module col_2 = col_double col_1
module col_3 = col_double col_2
module col_4 = col_double col_3
module col_5 = col_double col_4
module col_6 = col_double col_5

module type two = {
  -- how many iterations to do
  val k : i64
  -- m = n / 2^k
  val two [n] [m] 'a 'b : ([m]a -> [m]b) -> [n]a -> [n]b
}

module two_0 : two = {
  def k = 0i64
  def two [n] [m] 'a 'b (f : [m]a -> [m]b) (xs : [n]a) : [n]b = 
    f (xs :> [m]a) :> [n]b
}

module two_incr (T : two) : two = {
  def k = 1 + T.k
  def two [n] [m] 'a 'b (f : [m]a -> [m]b) (xs : [n]a) : [n]b = 
    let k = log2 n
    let n2 = n / 2
    in parm (1 << (k-1)) (T.two f : [n2]a -> [n2]b) xs
}


module type bmerge = {
  val n : i64 -- the size of the input array
  val merge : [n]i32 -> [n]i32
}

module bmerge_0 : bmerge = {
  def n = 1i64
  def merge (xs : [n]i32) : [n]i32 = xs
}

-- C.n == 2 * BM.n
module bmerge_double (BM : bmerge) (C : col) : bmerge = {
  def n = 2 * BM.n
  def merge (xs : [n]i32) : [n]i32 =
    let k = log2 n in
    -- the first column
    let xs = C.col ((1 << k) - 1) i32.min i32.max (xs :> [C.n]i32) in
    -- the remaining columns
    let xs = loop xs = xs for i < k-1 do 
      C.col (1 << (k - i - 2)) i32.min i32.max xs
    in xs :> [n]i32
}

module bmerge_1 = bmerge_double bmerge_0 col_1
module bmerge_2 = bmerge_double bmerge_1 col_2
module bmerge_3 = bmerge_double bmerge_2 col_3
module bmerge_4 = bmerge_double bmerge_3 col_4
module bmerge_5 = bmerge_double bmerge_4 col_5
module bmerge_6 = bmerge_double bmerge_5 col_6

entry main (xs : [2]i32) : [2]i32 =
  col_1.col 1i64 i32.min i32.max (xs :> [col_1.n]i32) :> [2]i32