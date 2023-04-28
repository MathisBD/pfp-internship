#include <cassert>
#include <iostream>
#include <vector>
#include <functional>
#include <cstdlib>

#include "kernels.cu"
	

std::vector<int> random_vector(int size)
{
    std::vector<int> vect;
    for (int i = 0; i < size; i++) {
        vect.push_back(rand());
    }
    return vect;
}

// The times are in milliseconds
template<typename T>
float benchmark(
     std::function<T()> random_state,
     std::function<std::vector<std::pair<cudaEvent_t, cudaEvent_t>>(T, int*, int*)> f,
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

        std::vector<std::pair<cudaEvent_t, cudaEvent_t>> events = f(random_state(), input, output);
         
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

std::vector<std::pair<cudaEvent_t, cudaEvent_t>> copy(int n, int* input_d, int* output_d)
{
    cudaEvent_t start, stop;
    cudaEventCreate(&start);
    cudaEventCreate(&stop);

    cudaEventRecord(start);
    copy_kernel<<<(1<<n) / 1024, 1024>>>(n, input_d, output_d);
    cudaEventRecord(stop);

    return { { start, stop } };
}

std::vector<std::pair<cudaEvent_t, cudaEvent_t>> block_swap(int low, int high, int* input_d, int* output_d)
{
    assert(BLOCK_SIZE <= low && BLOCK_SIZE <= high);

    cudaEvent_t start, stop;
    cudaEventCreate(&start);
    cudaEventCreate(&stop);

    dim3 blockCount((1 << low) / (1 << BLOCK_SIZE), (1 << high) / (1 << BLOCK_SIZE));
    dim3 blockSize(1 << BLOCK_SIZE, 1 << BLOCK_SIZE);

    cudaEventRecord(start); 
    block_swap_kernel<<<blockCount, blockSize>>>(low, high, input_d, output_d);
    cudaEventRecord(stop);

    return { { start, stop } };
}

int main() 
{
    int n = 27;
    
    double time = benchmark<int>(
        [&]() -> int { return 0; },
        [&](int unused, int* input_d, int* output_d) -> std::vector<std::pair<cudaEvent_t, cudaEvent_t>>
          { return copy(n, input_d, output_d); },
        1 << n,
        100, 
        false);
    printf("Copy: n=%d  time=%fms\n", n, time); 
    
    for (int low = BLOCK_SIZE; low <= n - BLOCK_SIZE; low++) {
        double time = benchmark<std::pair<int, int>>(
            [&]() -> std::pair<int, int> { return { low, n - low }; },
            [&](std::pair<int, int> lh, int* input_d, int* output_d) -> std::vector<std::pair<cudaEvent_t, cudaEvent_t>>
              { return block_swap(lh.first, lh.second, input_d, output_d); },
            1 << n,
            100, 
            false);
        printf("Block swap: n=%d k=%d  time=%fms\n", n, low, time); 
    }

} 
