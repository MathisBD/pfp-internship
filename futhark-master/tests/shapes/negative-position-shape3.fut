-- Entry points may not be return-polymorphic.
-- ==
-- error: Entry point

entry main [n] (x: i32) : [n]i32 = replicate n x
