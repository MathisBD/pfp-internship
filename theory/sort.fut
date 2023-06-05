-- We assume the length of xs is even.
def even_odd 't (xs : []t) = 
  let n = length xs
  in map 
    (\i -> if 2*i < n then xs[2*i] else xs[2*i+1 - n]) 
    (iota n)







    