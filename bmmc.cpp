#define CL_TARGET_OPENCL_VERSION 210
#include <CL/cl.hpp>
#include <iostream>
#include <fstream>
#include <sstream>
#include <assert.h>
#include <ctime>

#include "perm.h"

std::string read_file(const std::string& file_name)
{
    std::ifstream file(file_name);
    std::stringstream buffer;
    if (file.is_open()) {
        buffer << file.rdbuf();
    }
    else {
        std::cout << "Could not open file: " << file_name << std::endl;
        exit(1);
    }
    return buffer.str();
}

inline void CLguard(cl_int err)
{
    if (err != CL_SUCCESS) {
        std::cout << "Aborting: CL error " << err << "\n";
        exit(1);
    }
}

class App
{
private:
    cl::Platform m_platform;
    cl::Device m_device;
    cl::Context m_context;
    cl::CommandQueue m_queue;
    cl::Program m_program;

public:
    App(bool profiling = false)
    {
        // Platform
        std::vector<cl::Platform> all_platforms;
        CLguard(cl::Platform::get(&all_platforms));
        if(all_platforms.size()==0){
            std::cout << "No platforms found. Check OpenCL installation!\n";
            exit(1);
        }
        m_platform = all_platforms[0];
        std::cout << "Using platform: " << m_platform.getInfo<CL_PLATFORM_NAME>() << "\n";
        
        // Device
        std::vector<cl::Device> all_devices;
        CLguard(m_platform.getDevices(CL_DEVICE_TYPE_ALL, &all_devices));
        std::cout << "Found " << all_devices.size() << " devices:\n";
        for (auto& device : all_devices) {
            std::cout << "\t" << device.getInfo<CL_DEVICE_NAME>() << "\n";
        }
        if (all_devices.size() == 0) {
            exit(1);
        }
        m_device = all_devices[0];
        std::cout << "Using device: " << m_device.getInfo<CL_DEVICE_NAME>() << "\n";

        // Context
        m_context = cl::Context({ m_device });
        cl_command_queue_properties props = 0;
        if (profiling) {
            props |= CL_QUEUE_PROFILING_ENABLE;
        }
        m_queue = cl::CommandQueue(m_context, m_device, props);
        
        // Program 
        std::string kernel_code = read_file("bmmc.cu");
        cl::Program::Sources sources;
        sources.push_back({ kernel_code.c_str(), kernel_code.length() });
        m_program = cl::Program(m_context, sources);
        if (m_program.build({ m_device }) != CL_SUCCESS) {
            std::cout << "Error building: " << m_program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(m_device) << "\n";
            exit(1);
        }
    }
    
    std::pair<std::vector<int>, double> slow_bpc(const permutation& perm, const std::vector<int>& input_h, int iterations = 1)
    {
        // Compute the number of bits to encode indices.
        int BITS = 0;
        while ((1 << BITS) < input_h.size()) {
            BITS++;
        }
        assert((1 << BITS) == input_h.size());
        assert(perm.size() == BITS);

        // Allocate the buffers
        BMMC bmmc_h = perm_to_bmmc(perm);
        std::vector<int> output_h(1 << BITS, 0);
        cl::Buffer bmmc_d(m_context, CL_MEM_READ_ONLY, sizeof(uint64_t) * 64);
        cl::Buffer input_d(m_context, CL_MEM_READ_ONLY, sizeof(int) * (1 << BITS));
        cl::Buffer output_d(m_context, CL_MEM_READ_WRITE, sizeof(int) * (1 << BITS));
        
        // Copy data to the GPU
        CLguard(m_queue.enqueueWriteBuffer(bmmc_d, CL_TRUE, 0, sizeof(uint64_t) * 64, bmmc_h.data()));
        CLguard(m_queue.enqueueWriteBuffer(input_d, CL_TRUE, 0, sizeof(int) * (1 << BITS), input_h.data()));
        
        // Call the kernel
        const int GROUP_BITS = 8;
        cl::Kernel naive_bmmc = cl::Kernel(m_program, "naive_bmmc");
        naive_bmmc.setArg(0, BITS);
        naive_bmmc.setArg(1, bmmc_d);
        naive_bmmc.setArg(2, input_d);
        naive_bmmc.setArg(3, output_d);

        uint64_t nanoseconds = 0;
        for (int it = 0; it < iterations; it++) {
            cl::Event event;
            CLguard(m_queue.enqueueNDRangeKernel(naive_bmmc, 
                cl::NullRange, cl::NDRange(1 << BITS), cl::NDRange(1 << GROUP_BITS), nullptr, &event));
            uint64_t start, end;
            CLguard(m_queue.finish());
            CLguard(event.getProfilingInfo(CL_PROFILING_COMMAND_START, &start));
            CLguard(event.getProfilingInfo(CL_PROFILING_COMMAND_END, &end));
            nanoseconds += end - start;
        }
        double NANOSECONDS_PER_SECOND = 1000000000;
        double time = nanoseconds / ((double)iterations * NANOSECONDS_PER_SECOND); 

        // Copy data back to the CPU
        CLguard(m_queue.enqueueReadBuffer(output_d, CL_TRUE, 0, sizeof(int) * (1 << BITS), output_h.data()));
        return { output_h, time };
    }  

    std::pair<std::vector<int>, double> fast_bpc(
        const permutation& perm, const std::vector<int>& input_h, int iterations = 1)
    {
        // Compute the number of bits to encode indices.
        int BITS = 0;
        while ((1 << BITS) < input_h.size()) {
            BITS++;
        }
        assert((1 << BITS) == input_h.size());
        assert(perm.size() == BITS);

        // Calculate the permutations
        permutation remaining = perm;
        std::vector<permutation> factors;
        auto add_factor = [&factors, &remaining](const permutation& f) {
            factors.push_back(f);
            remaining = compose_perm(remaining, inverse_perm(factors.back()));
            //std::cout << "factor: " << show_perm(factors.back()) << "\nremaining: " << show_perm(remaining) << "\n\n";
        };
        
        const int SEG_INIT = 8;
        const int SEG_FIN = 6;
        while (!is_perm_identity(remaining)) {
            if ((factors.size() & 1) == 0) {
                add_factor(sort_perm_range(remaining, 0, SEG_INIT));
            }
            else {
                add_factor(sort_perm_range(remaining, SEG_FIN, BITS - SEG_FIN));
            }
        }

        std::cout << "Num factors: " << factors.size() << "\n";

        // Allocate the buffers
        std::vector<int> output_h(1 << BITS, 0);
        cl::Buffer bmmc_d(m_context, CL_MEM_READ_ONLY, sizeof(uint64_t) * 64);
        cl::Buffer buffer1_d(m_context, CL_MEM_READ_WRITE, sizeof(int) * (1 << BITS));
        cl::Buffer buffer2_d(m_context, CL_MEM_READ_WRITE, sizeof(int) * (1 << BITS));

        // Copy the input vector to the GPU
        CLguard(m_queue.enqueueWriteBuffer(buffer1_d, CL_TRUE, 0, sizeof(int) * (1 << BITS), input_h.data()));
        
        uint64_t nanoseconds = 0;    
        for (int it = 0; it < iterations; it++) {
            std::vector<cl::Event> events;
            // Apply each factor
            for (int i = 0; i < factors.size(); i++) {
                // Upload the bmmc to the GPU
                BMMC bmmc_h = perm_to_bmmc(factors[i]);
                CLguard(m_queue.enqueueWriteBuffer(bmmc_d, CL_TRUE, 0, sizeof(uint64_t) * 64, bmmc_h.data()));

                // Choose which input and output to use
                const auto& source_d = (i & 1) ? buffer2_d : buffer1_d;
                const auto& dest_d   = (i & 1) ? buffer1_d : buffer2_d;
                
                cl::Event ev;
                // This is a lower sort, use the MRC kernel.
                if ((i & 1) == 0) {
                    const int GROUP_BITS = SEG_INIT;
                    cl::Kernel kernel = cl::Kernel(m_program, "mrc");
                    kernel.setArg(0, BITS);
                    kernel.setArg(1, GROUP_BITS);
                    kernel.setArg(2, bmmc_d);
                    kernel.setArg(3, source_d);
                    kernel.setArg(4, dest_d);
                    kernel.setArg(5, (1 << GROUP_BITS) * sizeof(uint64_t), NULL);
                    CLguard(m_queue.enqueueNDRangeKernel(kernel, 
                        cl::NullRange, cl::NDRange(1 << BITS), cl::NDRange(1 << GROUP_BITS), nullptr, &ev));
                }
                // This is a higher sort, use the naive BMMC kernel.
                else {
                    const int GROUP_BITS = SEG_FIN;
                    cl::Kernel kernel = cl::Kernel(m_program, "naive_bmmc");
                    kernel.setArg(0, BITS);
                    kernel.setArg(1, bmmc_d);
                    kernel.setArg(2, source_d);
                    kernel.setArg(3, dest_d);
                    CLguard(m_queue.enqueueNDRangeKernel(kernel, 
                        cl::NullRange, cl::NDRange(1 << BITS), cl::NDRange(1 << GROUP_BITS), nullptr, &ev));
                }    
                events.push_back(ev);
            }

            // Compute time measurements
            m_queue.finish();
            for (const auto& ev : events) {
                uint64_t start, end;
                CLguard(ev.getProfilingInfo(CL_PROFILING_COMMAND_START, &start));
                CLguard(ev.getProfilingInfo(CL_PROFILING_COMMAND_END, &end));
                nanoseconds += end - start;
            }
        }
        double NANOSECONDS_PER_SECOND = 1000000000;
        double time = nanoseconds / ((double)iterations * NANOSECONDS_PER_SECOND); 

        // Copy the data back to the CPU
        CLguard(m_queue.enqueueReadBuffer((factors.size() & 1) ? buffer2_d : buffer1_d, CL_TRUE, 0, sizeof(int) * (1 << BITS), output_h.data()));
        return { output_h, time };
    }

    std::pair<std::vector<int>, double> scatter_bpc(
        const permutation& perm, const std::vector<int>& input_h, int iterations = 1)
    {
        // Compute the number of bits to encode indices.
        int BITS = 0;
        while ((1 << BITS) < input_h.size()) {
            BITS++;
        }
        assert((1 << BITS) == input_h.size());
        assert(perm.size() == BITS);

        // Allocate the buffers
        std::vector<int> output_h(1 << BITS, 0);
        std::vector<int> idxs_h(1 << BITS, 0);
        for (int i = 0; i < (1 << BITS); i++) {
            idxs_h[i] = permute_bits(perm, i);
        }
        cl::Buffer bmmc_d(m_context, CL_MEM_READ_ONLY, sizeof(uint64_t) * 64);
        cl::Buffer vals_d(m_context, CL_MEM_READ_ONLY, sizeof(int) * (1 << BITS));
        cl::Buffer idxs_d(m_context, CL_MEM_READ_ONLY, sizeof(int) * (1 << BITS));
        cl::Buffer output_d(m_context, CL_MEM_READ_WRITE, sizeof(int) * (1 << BITS));
        
        // Copy data to the GPU
        CLguard(m_queue.enqueueWriteBuffer(vals_d, CL_TRUE, 0, sizeof(int) * (1 << BITS), input_h.data()));
        CLguard(m_queue.enqueueWriteBuffer(idxs_d, CL_TRUE, 0, sizeof(int) * (1 << BITS), idxs_h.data()));
        
        // Call the kernel
        const int GROUP_BITS = 5;
        cl::Kernel kernel = cl::Kernel(m_program, "scatter");
        kernel.setArg(0, vals_d);
        kernel.setArg(1, idxs_d);
        kernel.setArg(2, output_d);
            
        uint64_t nanoseconds = 0;
        for (int it = 0; it < iterations; it++) {
            cl::Event event;
            CLguard(m_queue.enqueueNDRangeKernel(kernel, 
                cl::NullRange, cl::NDRange(1 << BITS), cl::NDRange(1 << GROUP_BITS), nullptr, &event));
            
            uint64_t start, end;
            CLguard(m_queue.finish());
            CLguard(event.getProfilingInfo(CL_PROFILING_COMMAND_START, &start));
            CLguard(event.getProfilingInfo(CL_PROFILING_COMMAND_END, &end));
            nanoseconds += end - start;
        }
        double NANOSECONDS_PER_SECOND = 1000000000;
        double time = nanoseconds / ((double)iterations * NANOSECONDS_PER_SECOND); 

        // Copy data back to the CPU
        CLguard(m_queue.enqueueReadBuffer(output_d, CL_TRUE, 0, sizeof(int) * (1 << BITS), output_h.data()));
        
        return { output_h, time };
    }
};

std::vector<int> cpu_bpc(const permutation& perm, const std::vector<int>& input)
{
    std::vector<int> output(input.size(), 0);
    for (int i = 0; i < input.size(); i++) {
        output[permute_bits(perm, i)] = input[i];
    }

    return output;
}

bool vector_equal(const std::vector<int>& a, const std::vector<int>& b)
{
    if (a.size() != b.size()) {
        return false;
    }
    for (int i = 0; i < a.size(); i++) {
        if (a[i] != b[i]) {
            return false;
        }
    }
    return true;
}

int main() 
{
    App app = App(true);

    //for (int bits = 15; bits <= 27; bits++) {
    //    std::vector<int> arr = random_array(1 << bits);
    //    auto [res1, time1] = app.slow_bpc(reverse_perm(bits), arr, 10);
    //    auto [res2, time2] = app.fast_bpc(reverse_perm(bits), arr, 10);
    //
    //    std::cout << "Bits: " << bits << "\n";
    //    std::cout << "Slow time avg:" << time1 * 1000 << "ms\n";
    //    std::cout << "Fast time avg:" << time2 * 1000 << "ms\n";
    //    std::cout << "    " << time1 / time2 << " speedup\n";
    //}

    // Test the BMMC LU decompositions 
    int n = 32;
    BMMC A = perm_to_bmmc(random_perm(n));
    bmmc_set(A, 3, 5, 1);
    bmmc_set(A, 6, 5, 1);
    auto [U, L, p] = bmmc_A_ULP_decomp(A);
    
    std::cout << show_bmmc(A) << std::endl;
    std::cout << show_bmmc(U) << std::endl;
    std::cout << show_bmmc(L) << std::endl;
    std::cout << show_bmmc(perm_to_bmmc(p)) << std::endl;
    
    assert(A == bmmc_mult_bmmc(bmmc_mult_bmmc(U, L), perm_to_bmmc(p)));

    return 0;
}