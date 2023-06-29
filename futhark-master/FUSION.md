# Bmmc / Two fusion rules :

let ys = bmmc Id 0 xs 
==> simplification
let ys = xs

let ys = bmmc A c xs
let zs = bmmc A' c' ys
==> simplification
let zs = bmmc (A' * A) (A' * c + c') xs

let (..., ys, ...) = two k w lam1 xs
let zs = two k w lam2 ys
==> fusion
let (..., zs, ...) = two k w lam' xs
-- Only when all the inputs to the second 'two' are outputs of the first 'two'.

let (..., ys, ...) = two k w lam1 xs
let zs = map w lam2 ys
==> fusion
let (..., zs, ...) = two k w lam' xs
-- Only when all the inputs to the 'map' are outputs of the 'two'.
-- Similar rule for map ->- two.

let ys = two k1 w1 (\xs' -> two k2 w2 lam2 xs') xs
==> simplification
let ys = two (k1 + k2) w1 lam2 xs

let ys = two k w (\xs' -> ...1... let zs' = bmmc A c ys' ...2...) xs
==> simplification
let (ys1, ys2, ...) = two k w (\xs' -> ...1... return (ys1', ys2', ...)) xs
let ysk = bmmc A' c' ys1
let ys = two k w (\ys1' ys2' ... ysk' -> ...2...) ys1 ys2 ... ysk
-- We have to add results to the first 'two' (...1...) so that it returns the variables 
-- that are needed for the bmmc and the second 'two' (...2...). 
-- These are exactly the variables that are free in ...2... (and also ys') but
-- computed in ...1...

let ys = replicate w x
let zs = bmmc A c ys
==> simplification
let zs = replicate w x

let ys = bmmc A c xs
let zs = reduce_comm w op ys
==> simplification
let zs = reduce_comm w op xs


