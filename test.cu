
// n is the number of bits in each index.
// The permutation is defined as an array of n ulong, i.e. one ulong for each row.
void kernel naive_bmmc(
    int n, 
    global const ulong* bmmc, 
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
    global const ulong* bmmc, 
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