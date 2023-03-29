Notation : N = 2^n is a vector length.
           G = 2^g is a kernel group size.
           K = 2^k is a block size.

We will generally have g <= k <= n.

1. How to perfom the following BMMC perm (MRC, memory rearrange complement) on the GPU :
  (A B)
  (0 C) where size(A) = k*k and size(C) = (n-k)*(n-k)
      
kernel: (requires G = K)
  gid <- group id
  lid <- local id
  x <- read(gid*G + lid)
  shared[mm(A,lid) + mm(B,gid)] <- x
  sync()
  y <- shared[lid]
  y -> write(mm(C,gid)*G + lid)

This requires that K fit in a single group, i.e. K <= 1024 on most GPUs.
It also requires that K is big enough to fill a warp, i.e. 32 <= K.

2. How to perform the following permutation on the GPU :
  (Id 0)
  (0  X) where size(Id) = k*k and size(X) = (n-k)*(n-k)

kernel: (requires G <= K)
  gid <- group id
  lid <- local id
  gid_low, gid_high = (k-g) lowest and highest (n-k) bits of gid
  x <- read(gid*G + lid)
  x -> write(mm(X, gid_high)*K + gid_low*G + lid)

This only requires that 32 <= K.

3. How to perform any BPC permutation P on the GPU :

Sort the m first bits of P.
Repeat until done:
  Sort the (n-b) last bits of P.
  Sort the m first bits of P.

We need to have b < m for this to work.
Sorting the m first bits can be done with a perm of the form of 1 (with k=m).
Sorting the (n-b) last bits can be done with a perm of the form of 2 (with k=b).

Cormen's thesis (section 2.2) shows that the required number of iterations is :
  ceil(max(rank_b(P), rank_m(P)) / (m-b))
    where rank_k(P) is the number of indices that cross a line between positions k-1 and k in one direction

We of course want to minimize b and maximize m. On the GPU the best values for m and b seem to be :
  m = 10
  b = 5
Which gives the trivial upper bound of 2 iterations, thus 5 passes.
However in some cases we can expect that rank_m(P) <= 5, yielding a bound of 1 iteration and 3 passes. TODO : how likely is this, accross all permutation P ?

4. How to perform any BMMC permutation A :
Using the LU factorization, we can always write A as :
  A = P*L*U
    where P is a permutation matrix
          L is a lower triangular matrix
          U is an upper triangular matrix

We can perform P in 5 passes using the method in point 3.
We can perform U in 1 pass using the method in point 1.
We can perform L in 3 passes by decomposing :
  L = T1*U'*T2
    where U' is a matrix of the form 1.
          T1=T2 is a matrix that transposes the first k bits and the last n-k bits of its vector input (T is a standard transposition that takes one pass to perform)

We can do better : can we perform U'*T2 in a single pass ?
--> Yes when K is small : we use a tiling method. Each thread group will have size K*K : it reads a tile of size K*K, performs some stuff in shared memory, and then writes back each vector of K elements (but not in the same K*K tile anymore). This would require K*K to fit in a single group, and that K is larger than a warp, so K=32.

We can also fuse T1 into P, eliminating the pass for T1.

This brings us down to 7 passes to perform an arbitrary BMMC, and hopefully 5 passes in most cases.

If we can also factorize A as A = U*L*P, then we can remove another pass :
  A = U*T1*U'*P'
    where L = T1*U'*T2
          P' = T2*P
We can perform U*T1 in a single pass.
We can fuse U' into the last pass of P' (which is also upper triangular, and we can choose K to match both), to perform U'*P' in 5 passes.

This brings us to 6 passes (and hopefully 4 in most cases).

5. Could we replace LMADs with BMMC perms ? 
This would require :
A. All arrays to one-dimensional.
B. All array sizes to be powers of 2.
C. All array sizes to be known at compile time (if we want the BMMC perms to be known at compile time).
D. Array slices to be powers of two (size, stride, maybe even offset).

- B Could be fixed by having the compiler automatically pad arrays to a power of 2.
Is it worth it ? E.g. mapping and reducing an array becomes tricky... more bookkeeping to perform.

- Of these restrictions, C seems to be the most annoying. 
We could maybe fix this by using matrices that have blocks (filled with e.g. 0s or an identity matrix) with existential sizes. We would calculate the exact matrices at execution time, using the sizes of the actual arrays. This would also mean we would have to do the matrix multiplications and factorizations at execution time. How fast is it to factorize a e.g. 30*30 binary matrix on the GPU ? Negligible compared to the global memory accesses ?
We could also have matrices that have entries that are expressions (as for LMADs). The issue is that the size of the expression for a given entry would grow exponentially with the number of matrices in e.g. a chained matrix multiplication (it grows linearly for LMAD operations).
Another option would be to calculate the explicit matrix for every size (1 to 64) at compile time, and choose the correct one at run time --> how to deal with bmmc slices ? 



It would allow us :
- To introduce the parm combinator.
- To fuse nested applications of parm.

Do we introduce the col combinator as a primitive ? It can be built using parm, but the number of such parms depends on the size of the input array.
This means that we can likely not detect compositions of parm that form regular columns at compile time. This is bad because sequential composition of two regular columns can be fused into a single column in many instances, and always into a single kernel. 

Another issue is : do we introduce the BMMC perms at the memory level or before ? The LMADs are at the memory level, but the BMMC perms would benefit from having fusion rules apply to them.

Do we even need parm at all in the IR ? Would it be enough to have BMMC and col ?
The fusion algebra would definitely be simpler.

6. How to integrate BMMCs into the IR :
- At the SOACS level : BMMCs as an index space transformation.
- At the memory level (GPUMem) : BMMCs replace LMADs, i.e. each array is assigned some memory block and a BMMC on this block, and also the array size (initial segment of the memory that has been permuted).

Fusion at the SOACS level :
- parm M f becomes bmmc A ->- two f ->- bmmc A^-1
  --> Issue : how to represent A ? While M is a compile time constant, the size of the input array could be arbitrary big...
- bmmc A ->- bmmc B becomes bmmc (B*A)
- col k1 f ->- col k2 g becomes col k3 (f ->- g) when possible
- two (bmmc A ->- f) becomes bmmc A' ->- two f (to enable further bmmc fusion)
- two (f ->- bmmc A) becomes two f ->- bmmc A' 
- two (col k1 f) becomes col k2 f

Other nice simplifications :
- replicate n x ->- bmmc A becomes replicate n x
- bmmc A ->- reduce_comm becomes reduce_comm

We can also push BMMCs through col and map. Would it be worth it ?
Yes if it can enable further fusion of BMMCs. Push forwards or backwards ?

Then at the memory level we can compile BMMC permutations and slicing as simply multiplying the array's BMMC. We can also implement coalescing as manifesting the array with its BMMC multiplied by the coalescing BMMC.
