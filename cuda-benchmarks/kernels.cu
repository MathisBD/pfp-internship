	
// The input and output arrays have size 1 << n. 
// We launch exactly the right amount of threads.
__global__ void copy_kernel(int n, const int* input, int* output)
{
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    output[i] = input[i];
}


// This implements a special class of index bit permutations,
// that swap the 'low' lowest bits and the 'high' highest bits of the index.
// The block should have (1 << block_size) rows and ((1 << block_size) + 1) columns : 
// the +1 is to avoid bank conflicts.
// We should also have block_size <= min(low, high).
// The input vector is viewed as a matrix with (1 << high) rows and (1 << low) columns.
// When calling this kernel, there should be (1 << low, 1 << high) threads in total 
// and each thread group should have (1 << block_size, 1 << block_size) treads.

#define BLOCK_SIZE 5

__global__ void block_swap_kernel(
    int low,   // the number of low bits
    int high,  // the number of high bits
    const int* in_vect,
    int* out_vect)
{
    __shared__ int block[((1 << BLOCK_SIZE) + 1) * (1 << BLOCK_SIZE)];

    size_t gi = blockIdx.x / ((1 << low) / (1 << BLOCK_SIZE));
    size_t gj = blockIdx.x % ((1 << low) / (1 << BLOCK_SIZE));
    //size_t gi = blockIdx.y;
    //size_t gj = blockIdx.x;
    size_t li = threadIdx.y;
    size_t lj = threadIdx.x;

    // Read the block and transpose it in shared memory
    int in_i = (gi << BLOCK_SIZE) + li;
    int in_j = (gj << BLOCK_SIZE) + lj;
    block[li * ((1 << BLOCK_SIZE) + 1) + lj] = in_vect[(in_i << low) + in_j];
    
    // Synchronize
    __syncthreads();

    // Write out the block (but not at the same place)
    int out_i = (gj << BLOCK_SIZE) + li;
    int out_j = (gi << BLOCK_SIZE) + lj;
    out_vect[(out_i << high) + out_j] = block[lj * ((1 << BLOCK_SIZE) + 1) + li];
}

__global__ void bit_reverse_kernel(
	int n,
	const int* in_vect,
	int* out_vect)
{
	__shared__ int block[((1 << BLOCK_SIZE) + 1) * (1 << BLOCK_SIZE)];

	size_t g = blockIdx.x;
	size_t i = threadIdx.y;
	size_t j = threadIdx.x;

	// Read the input block
	size_t in_addr = (i << (n - BLOCK_SIZE)) | (g << BLOCK_SIZE) | j;
	block[i * ((1 << BLOCK_SIZE) + 1) + j] = in_vect[in_addr];

	// Synchronize
	__syncthreads();

	// Write the ouput block
	size_t out_addr = (j << (n - BLOCK_SIZE)) | (g << BLOCK_SIZE) | i;
   	out_addr = __brevll(out_addr) >> (64 - n);
	out_vect[out_addr] = block[j * ((1 << BLOCK_SIZE) + 1) + i];
}


