# Parm identities : 

1. How does 'parm' change indices ? i.e. if a value is at index x in an array, at what index will it be in each of the two arrays created by parm ? 

Answer : for a mask M, a value at index x will be at index x' in its subarray, and x' = x where we removed (deleted, not set to 0) the bit at position lsb(M).

Example : if the mask M is odd, then x is mapped to x/2.

2. Can we exchange two parms, as in parm A . parm B ?

Answer : yes in some cases.

Define A' = A but we remove the bit at position lsb(B')
       B' = B but we add a 0 at position lsb(A)

If A_{lsb(B')} = 0, then parm A . parm B = parm B' . parm A'

Special cases : 
  - when A & B' = 0 we can exchange.
  - when A and B are powers of 2 we can exchange and :
    parm 2^i . parm 2^j = parm 2^(j+1) . parm 2^i   (for i <= j)
    Fun fact : this is the relation that degeneracy maps satisfy in simplicial sets.


3. Can we push a BMMC through a single parm M ? 

4. How to decompose parm M using two and BMMC permutations ? 

Answer : we have parm M f = bmmc A ->- two f ->- bmmc A^-1
  where A = (1-0----)
            (-10----)
            (--01---)
            (--0-1--)
            (--0--1-)
            (--0---1)
            (---M---)
          i.e. M is on the last line (lsb to the left) and the column of 0s is at index lsb(M)

(I can also give an explicit form for A^-1)

To make ilv the matrix is 
  (-I)
  (1-) where I is the identity matrix

5. We also have the identity : 
  two (bmmc A) = (A 0)
                 (0 1)

This allows us to express nested applications of parm : 
  parm M $ parm N f = bmmc (M_A * M_B') ->- two (two f) ->- bmmc (M_A * M_B')^-1
    where M_B' is (M_B 0)
                  ( 0  1)

This could be good for GPU compilation, as applying a function to a contiguous (sub)array is better.
This could allow for efficient compilation of these weaving-like function applications, if we can figure out how to implement bmmc permutations efficiently.

Indeed weaving f on a big array can make it so that each application of f has very bad coallescing behaviour. It could be worth it to perform a permutation of the big array between each column of two^k.
The performance implications would be :
- A potential order of magnitude speedup for the application of f if it is memory bound.
- A single additional bmmc perm (and thus memory accesses) between each column. Note that consecutive perms are easily fused. 

6. How to decompose a k-regular column into a composition of parm ? 
- Choose a mask M such that k xor M has an even number of set bits
- Set k' = k but we remove the bit at index lsb(M)
- We have col k = parm M . col k'

Note that we can always choose M to be 1, 2 or 3 (depending on the value of k mod 4) yielding a decomposition using only ilv, que and vee.

Also if we have two columns k1 and k2 such that k1 and k2 have at least 3 bits each (in total), we can make sure that the first mask M is the same for both. 
This makes it so that we can always write, for any 2-functions f and g :
  col k1 f ->- col k2 g =
    parm M_1 $ ... $ parm M_n $ (parm A f ->- parm B g)
    for some masks M_i, A, B

(And in 75% of cases we can choose A = B)
