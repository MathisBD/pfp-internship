

def g [n] (xs : [n]i32) : i32 =
  reduce (+) 0 xs
  
entry f [n] (xs : [n]i32) : [n]i32 = 
  map (\x -> x + (1 + 3)) (map (\x -> g xs * x) xs)