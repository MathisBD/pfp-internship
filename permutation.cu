

// We launch 2**n threads, with a thread group of size 256.
// The 'in_vect' and 'out_vect' arrays have 2**n elements.
// The 'bmmc' array is a binary matrix, with n rows and n columns : e
// each element of the array corresponds to a single row. This matrix is 
// what defines the permutation.

// A block permutation would have a matrix of the following form (? is either 0 or 1).
// The LSB it at the top left.
// 1.....
// .1....
// ..1...
// ...???
// ...???
// ...???

// A segmented permutation would have a matrix of the following form (? is either 0 or 1).
// The LSB is at the top left. 
// ???...
// ???...
// ???...
// ...1..
// ....1.
// .....1
void kernel permutation(
    int n, 
    constant ulong* bmmc, 
    global const int* in_vect, 
    global int* out_vect) 
{
    size_t id = get_global_id(0);

    // Compute the matrix multiplication 'bmmc*id'
    // Here 'bmmc' is viewed as an n*n binary matrix, 
    // and 'id' is viewed as an n*1 column vector (LSB at the top and MSB at the bottom).
    ulong out_idx = 0;
    for (int i = 0; i < n; i++) {
        out_idx |= (popcount(bmmc[i] & id) & 1) << i;
    }
    // Do the actual copy
    out_vect[out_idx] = in_vect[id];
}
