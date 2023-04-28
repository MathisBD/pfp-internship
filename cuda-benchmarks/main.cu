#include <cassert>
#include <iostream>
#include <vector>
#include <functional>
#include <cstdlib>

#include "perm.h"
#include "kernels.cu"
	

using events_t = std::vector<std::pair<cudaEvent_t, cudaEvent_t>>;

std::vector<int> random_vector(int size)
{
    std::vector<int> vect;
    for (int i = 0; i < size; i++) {
        vect.push_back(rand());
    }
    return vect;
}

template <typename T>
void test(
    std::function<T()> random_state,
    std::function<events_t(T state, int*, int*)> f_test,
    std::function<std::vector<int>(T state, std::vector<int>)> f_expected,    
    int input_size,
    int iterations)
{   
    int *input_d, *output_d;
    cudaMalloc(&input_d, input_size * sizeof(int));
    cudaMalloc(&output_d, input_size * sizeof(int));
 
    // Do the GPU computation
    for (int it = 0; it < iterations; it++) {
        if (it % ((iterations / 10) + 1) == 0) {
            std::cout << ".";
            std::cout.flush();
        }
        // Generate the input and the state
        std::vector<int> input = random_vector(input_size);
        std::vector<int> output(input.size(), 0);
        T state = random_state();
            
        // Do the computation on the GPU
        cudaMemcpy(input_d, input.data(), input_size * sizeof(int), cudaMemcpyHostToDevice);
        const events_t& events = f_test(state, input_d, output_d); 
        cudaMemcpy(output.data(), output_d, input_size * sizeof(int), cudaMemcpyDeviceToHost);

        // Compare the result to the expected one
        assert(output == f_expected(state, input));
    }
    std::cout << std::endl;
}


// The times are in milliseconds
template<typename T>
float benchmark(
     std::function<T()> random_state,
     std::function<events_t(T, int*, int*)> f,
     int input_size,
     int iterations,
     bool verbose = true) 
{
    // Generate the input array
    std::vector<int> array = random_vector(input_size);

    // Copy the input to the GPU
    int *input, *output;
    cudaMalloc(&input, input_size * sizeof(int));
    cudaMalloc(&output, input_size * sizeof(int));
    cudaMemcpy(input, array.data(), input_size * sizeof(int), cudaMemcpyHostToDevice);

    // Get the time measurements
    std::vector<std::vector<float>> times(iterations, std::vector<float>());
    for (int it = 0; it < iterations; it++) {
        if (verbose && (it % ((iterations / 10) + 1) == 0)) {
            std::cout << ".";
            std::cout.flush();
        }

        const events_t& events = f(random_state(), input, output);
         
        for (int i = 0; i < events.size(); i++) {
	    cudaEventSynchronize(events[i].second);
	    float milliseconds = 0;
	    cudaEventElapsedTime(&milliseconds, events[i].first, events[i].second);
	    times[it].push_back(milliseconds);
    	}  
    }

    cudaFree(input);
    cudaFree(output);

    if (verbose) { 
        std::cout << "\n";
            
        // Display the time measurements, divided by event count
        int max_event_count = 0;
        for (const auto& ts : times) {
            max_event_count = std::max(max_event_count, (int)ts.size());
        }
        for (int event_count = 1; event_count <= max_event_count; event_count++) {
            // How many measurements we have for this count
            int trace_count = 0;
            std::vector<float> avg(event_count, 0);
            for (const auto& ts : times) {
                if (ts.size() == event_count) {
                    trace_count++;
                    for (int i = 0; i < event_count; i++) {
                        avg[i] += ts[i];
                    }
                }
            }
            for (int i = 0; i < event_count; i++) {
                avg[i] /= trace_count;
            }
            if (trace_count > 0) {
                std::cout << "  avg time for " << event_count << " events:\n    ";
                for (int i = 0; i < event_count; i++) {
                    std::cout << avg[i] << "ms  ";
                }
                std::cout << "\n";
            }
        }
    }
    // Compute the average time per iteration
    float total = 0;
    for (const auto& ts : times) {
        for (float t : ts) {
            total += t;
	}
    }
    return total / iterations;
}

events_t copy(int n, int* input_d, int* output_d)
{
    cudaEvent_t start, stop;
    cudaEventCreate(&start);
    cudaEventCreate(&stop);

    cudaEventRecord(start);
    copy_kernel<<<(1<<n) / 1024, 1024>>>(n, input_d, output_d);
    cudaEventRecord(stop);

    return { { start, stop } };
}

events_t block_swap(int low, int high, int* input_d, int* output_d)
{
    assert(BLOCK_SIZE <= low && BLOCK_SIZE <= high);

    cudaEvent_t start, stop;
    cudaEventCreate(&start);
    cudaEventCreate(&stop);

    dim3 blockCount(((1 << low) / (1 << BLOCK_SIZE)) * ((1 << high) / (1 << BLOCK_SIZE)));
    dim3 blockSize((1 << BLOCK_SIZE), (1 << BLOCK_SIZE));

    cudaEventRecord(start); 
    block_swap_kernel<<<blockCount, blockSize>>>(low, high, input_d, output_d);
    cudaEventRecord(stop);

    return { { start, stop } };
}

events_t bit_reverse(int n, int* input_d, int* output_d)
{
    assert(2*BLOCK_SIZE <= n);

    cudaEvent_t start, stop;
    cudaEventCreate(&start);
    cudaEventCreate(&stop);

    dim3 blockSize((1 << BLOCK_SIZE), (1 << BLOCK_SIZE));
    dim3 blockCount((1 << n) / (blockSize.x * blockSize.y));

    cudaEventRecord(start); 
    bit_reverse_kernel<<<blockCount, blockSize>>>(n, input_d, output_d);
    cudaEventRecord(stop);

    return { { start, stop } };
}

std::vector<int> cpu_bmmc(const BMMC& bmmc, const std::vector<int>& input)
{
    std::vector<int> output(input.size(), 0);
    for (int i = 0; i < input.size(); i++) {
        output[bmmc_mult_vect(bmmc, i)] = input[i];
    }
    return output;
}


int main()
{
	int n = 15;
    
    test<int>(
        [&]() -> int { return 0; },
        [&](int unused, int* input_d, int* output_d) -> events_t { return copy(n, input_d, output_d); },
        [&](int unused, std::vector<int> input) -> std::vector<int> { return input; },
        1 << n,
        100); 

	test<std::pair<int, int>>(
        [&]() -> std::pair<int, int> { return { 10, n - 10 }; },
        [&](std::pair<int, int> lh, int* input_d, int* output_d) -> events_t { return block_swap(lh.first, lh.second, input_d, output_d); },
        [&](std::pair<int, int> lh, std::vector<int> input) -> std::vector<int> { return cpu_bmmc(perm_to_bmmc(rotate_perm(lh.second, lh.first + lh.second)), input); },
        1 << n,
        100); 
        
    test<int>(
        [&]() -> int { return n; },
        [&](int n, int* input_d, int* output_d) -> events_t { return bit_reverse(n, input_d, output_d); },
        [&](int n, std::vector<int> input) -> std::vector<int> { return cpu_bmmc(perm_to_bmmc(reverse_perm(n)), input); },
        1 << n,
        100); 

    n = 27;

    double time = benchmark<int>(
        [&]() -> int { return 0; },
        [&](int unused, int* input_d, int* output_d) -> events_t
          { return bit_reverse(n, input_d, output_d); },
        1 << n,
        100, 
        false);
    printf("Bit reverse: n=%d  time=%fms\n", n, time); 
    
	time = benchmark<int>(
        [&]() -> int { return 0; },
        [&](int unused, int* input_d, int* output_d) -> events_t
          { return copy(n, input_d, output_d); },
        1 << n,
        100, 
        false);
    printf("Copy: n=%d  time=%fms\n", n, time); 
    
    for (int low = BLOCK_SIZE; low <= n - BLOCK_SIZE; low++) {
        double time = benchmark<std::pair<int, int>>(
            [&]() -> std::pair<int, int> { return { low, n - low }; },
            [&](std::pair<int, int> lh, int* input_d, int* output_d) -> events_t
              { return block_swap(lh.first, lh.second, input_d, output_d); },
            1 << n,
            100, 
            false);
        printf("Block swap: n=%d k=%d  time=%fms\n", n, low, time); 
    }

} 
