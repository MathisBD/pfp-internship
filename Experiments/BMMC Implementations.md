Notation : N = 2^n is a vector length.
           G = 2^g is a kernel group size.
           K = 2^k is a block size.

We will generally have g <= k <= n.

1. How to perfom the following BMMC perm on the GPU :
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