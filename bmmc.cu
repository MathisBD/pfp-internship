
void kernel copy(
    global const int* in_vect,
    global int* out_vect)
{
    size_t id = get_global_id(0);
    out_vect[id] = in_vect[id];
}

void kernel scatter(
    global const int* in_vals,
    global const int* in_idxs,
    global int* out_vect)
{
    size_t id = get_global_id(0);
    out_vect[in_idxs[id]] = in_vals[id];
}

// n is the number of bits in each index.
// The bmmc is defined as an array of n ulong, i.e. one ulong for each row.
void kernel naive_bmmc(
    int n, 
    constant ulong* bmmc, 
    global const int* in_vect, 
    global int* out_vect) 
{
    size_t id = get_global_id(0);
    size_t lid = get_local_id(0);
    size_t gid = get_group_id(0);

    // Do the matrix multiplication
    ulong out_idx = 0;
    for (int i = 0; i < n; i++) {
        out_idx |= (popcount(bmmc[i] & id) & 1) << i;
    }
    // Do the actual copy
    out_vect[out_idx] = in_vect[id];
}


// k is the size of the lower block of the bmmc. The higher block is of size n - k.
void kernel mrc(
    int n, 
    int k, 
    constant ulong* bmmc, 
    global const int* in_vect, 
    global int* out_vect,
    local int* buffer) 
{
    size_t id = get_global_id(0);
    size_t lid = get_local_id(0);
    size_t gid = get_group_id(0);

    // Read to the buffer
    ulong buffer_idx = 0;
    for (int i = 0; i < k; i++) {
        buffer_idx |= (popcount(bmmc[i] & id) & 1) << i;
    }
    buffer[buffer_idx] = in_vect[id];
    
    // Synchronize
    barrier(CLK_LOCAL_MEM_FENCE);
    
    // Write the output
    ulong out_idx = lid;
    for (int i = k; i < n; i++) {
        out_idx |= (popcount(bmmc[i] & id) & 1) << i;
    }
    out_vect[out_idx] = buffer[lid];
}


// This implements a special class of index bit permutations,
// that swaps the 'low' lowest bits and the 'high' highest bits of the index.
// The block should have (1 << block_size) rows and ((1 << block_size) + 1) columns : 
// the +1 is to avoid bank conflicts.
// We should also have block_size <= min(low, high).
// The input vector is viewed as a matrix with (1 << high) rows and (1 << low) columns.
// When calling this kernel, there should be (1 << low, 1 << high) threads in total 
// and each thread group should have (1 << block_size, 1 << block_size) treads.
void kernel block_swap(
    int low,   // the number of low bits
    int high,  // the number of high bits
    int block_size,
    global const int* in_vect,
    global int* out_vect,
    local int* block)
{
    size_t gi = get_group_id(1);
    size_t gj = get_group_id(0);
    size_t li = get_local_id(1);
    size_t lj = get_local_id(0);

    // Read the block and transpose it in shared memory
    int in_i = (gi << block_size) + li;
    int in_j = (gj << block_size) + lj;
    block[li * ((1 << block_size) + 1) + lj] = in_vect[(in_i << low) + in_j];
    
    // Synchronize
    barrier(CLK_LOCAL_MEM_FENCE);

    // Write out the block (but not at the same place)
    int out_i = (gj << block_size) + li;
    int out_j = (gi << block_size) + lj;
    out_vect[(out_i << high) + out_j] = block[lj * ((1 << block_size) + 1) + li];
}

//void kernel block_swap_REV(
//    int low,   // the number of low bits
//    int high,  // the number of high bits
//    int block_size,
//    global const int* in_vect,
//    global int* out_vect,
//    local int* block)
//{
//    size_t gi = get_group_id(1);
//    size_t gj = get_group_id(0);
//    size_t li = get_local_id(1);
//    size_t lj = get_local_id(0);
//
//    // Read the block and transpose it in shared memory
//    int in_i = (gi << block_size) + li;
//    int in_j = (gj << block_size) + lj;
//    block[li * (1 << block_size) + lj] = in_vect[(in_i << low) + in_j];
//    
//    // Synchronize
//    barrier(CLK_LOCAL_MEM_FENCE);
//
//    // Write out the block (but not at the same place)
//    int out_i = (gj << block_size) + li;
//    int out_j = (gi << block_size) + lj;
//    out_vect[(out_i << high) + out_j] = block[lj * (1 << block_size) + li];
//}