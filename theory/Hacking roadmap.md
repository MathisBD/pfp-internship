1. Enforce the simplifications into the compiler :
- Arrays have only one dimension
- Array sizes must be known at compile time
- Array sizes must be powers of two if used as input to a parm / col / etc.
- Remove LMADs --> probably don't do this before when we need to deal with index functions.

2. Add the new combinators in the compiler front-end :
- the 'parm' combinator
We can implement 'col' directly in futhark, by using nested applications of 'parm'.
Do we expose 'bmmc' as a user-level primitive ? --> probably not, CHECK WITH MARY

3. Add the following to the SOACS IR :
- 'bmmc' permutations --> these actually would be a BasicOp, so not just for SOACS.
- the 'two k f' combinator, that divides the input array into 2^k contiguous subarrays and applies f to each one

The 'parm' combinator then gets compiled to bmmc ->- two ->- bmmc. Using fusion rules we can simplify nested applications of parm (after expansion).

4. Add fusion rules at the SOACS level.

5. Handle flattening of 'two k' into a SegOp (extractKernel pass :: SOACS -> GPU). The way we would do this : 
-> first flatten f to a SegOp
-> add a dimension to the SegSpace (with size 2^k).
-> most likely modify the body of f to use correct indexing into the SegOp array inputs.

6. Modify the index functions to use BMMC permutations instead of LMADs. 
