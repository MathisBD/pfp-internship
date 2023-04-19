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
    
    // n is the number of bits to encode indices
    std::vector<cl::Event> slow_bmmc(
        int n, const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d)
    {
        // Create the BMMC on the GPU
        assert(bmmc.size() == n);
        cl::Buffer bmmc_d(m_context, CL_MEM_READ_ONLY, sizeof(uint64_t) * n);
        CLguard(m_queue.enqueueWriteBuffer(bmmc_d, CL_TRUE, 0, sizeof(uint64_t) * n, bmmc.data()));
        
        // Call the kernel
        const int GROUP_BITS = 8;
        cl::Kernel naive_bmmc = cl::Kernel(m_program, "naive_bmmc");
        naive_bmmc.setArg(0, n);
        naive_bmmc.setArg(1, bmmc_d);
        naive_bmmc.setArg(2, input_d);
        naive_bmmc.setArg(3, output_d);
        cl::Event event;
        CLguard(m_queue.enqueueNDRangeKernel(naive_bmmc, 
            cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &event));

        return { event };
    }  

    // n is the number of bits to encode indices
    std::vector<cl::Event> mrc_bmmc(
        int n, int k, const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d)
    {
        // Create the BMMC on the GPU
        assert(bmmc.size() == n);
        cl::Buffer bmmc_d(m_context, CL_MEM_READ_ONLY, sizeof(uint64_t) * n);
        CLguard(m_queue.enqueueWriteBuffer(bmmc_d, CL_TRUE, 0, sizeof(uint64_t) * n, bmmc.data()));
        
        // Call the kernel
        const int GROUP_BITS = 8;
        cl::Kernel kernel = cl::Kernel(m_program, "mrc");
        kernel.setArg(0, n);
        kernel.setArg(1, k);
        kernel.setArg(2, bmmc_d);
        kernel.setArg(3, input_d);
        kernel.setArg(4, output_d);
        kernel.setArg(5, (1 << GROUP_BITS) * sizeof(int), NULL);
        cl::Event event;
        CLguard(m_queue.enqueueNDRangeKernel(kernel, 
            cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &event));

        return { event };
    }  

    //std::vector<cl::Event> fast_bpc(
    //    int n, const permutation& perm, const cl::Buffer& input_d, const cl::Buffer& output_d)
    //{
    //    assert(perm.size() == n);
    //    
    //    // Factorize the permutation
    //    const int SEG_INIT = std::min(8, n);
    //    const int SEG_FIN = std::min(5, n);
    //    std::vector<permutation> factors = factorize_perm_init_fin(perm, SEG_INIT, n - SEG_FIN);
//
    //    // Allocate the buffers
    //    cl::Buffer bmmc_d(m_context, CL_MEM_READ_ONLY, sizeof(uint64_t) * n);
    //    std::vector<cl::Event> events;
    //    
    //    // Apply each factor
    //    for (int i = 0; i < factors.size(); i++) {
    //        // Upload the bmmc to the GPU
    //        BMMC bmmc_h = perm_to_bmmc(factors[i]);
    //        CLguard(m_queue.enqueueWriteBuffer(bmmc_d, CL_TRUE, 0, sizeof(uint64_t) * n, bmmc_h.data()));
//
    //        cl::Event ev;
    //        // This is a lower sort, use the MRC kernel.
    //        if ((i & 1) == 0) {
    //            const int GROUP_BITS = SEG_INIT;
    //            cl::Kernel kernel = cl::Kernel(m_program, "mrc");
    //            kernel.setArg(0, n);
    //            kernel.setArg(1, GROUP_BITS);
    //            kernel.setArg(2, bmmc_d);
    //            kernel.setArg(3, input_d);
    //            kernel.setArg(4, output_d);
    //            kernel.setArg(5, (1 << GROUP_BITS) * sizeof(int), NULL);
    //            CLguard(m_queue.enqueueNDRangeKernel(kernel, 
    //                cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &ev));
    //        }
    //        // This is a higher sort, use the naive BMMC kernel.
    //        else {
    //            const int GROUP_BITS = SEG_FIN;
    //            cl::Kernel kernel = cl::Kernel(m_program, "naive_bmmc");
    //            kernel.setArg(0, n);
    //            kernel.setArg(1, bmmc_d);
    //            kernel.setArg(2, input_d);
    //            kernel.setArg(3, output_d);
    //            CLguard(m_queue.enqueueNDRangeKernel(kernel, 
    //                cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &ev));
    //        }    
    //        events.push_back(ev);
//
    //        // Copy the output back to the input. We will likely be smarter in the actual Futhark implementation.
    //        CLguard(m_queue.enqueueCopyBuffer(output_d, input_d, 0, 0, (1 << n) * sizeof(int)));
    //    }
//
    //    return events;
    //}
    
    std::vector<cl::Event> fast_bpc(
        int n, const permutation& perm, const cl::Buffer& input_d, const cl::Buffer& output_d)
    {
        assert(perm.size() == n);
        
        // Factorize the permutation
        const int SEG_INIT = std::min(17, n);
        const int SEG_FIN = std::max(n - 9, 0);
        std::vector<permutation> factors = factorize_perm_init_fin(perm, SEG_INIT, SEG_FIN);

        // Allocate the buffers
        cl::Buffer bmmc_d(m_context, CL_MEM_READ_ONLY, sizeof(uint64_t) * n);
        std::vector<cl::Event> events;
        
        // Apply each factor
        for (int i = 0; i < factors.size(); i++) {
            // Upload the bmmc to the GPU
            BMMC bmmc_h = perm_to_bmmc(factors[i]);
            CLguard(m_queue.enqueueWriteBuffer(bmmc_d, CL_TRUE, 0, sizeof(uint64_t) * n, bmmc_h.data()));

            cl::Event ev;
            const int GROUP_BITS = 8;
            cl::Kernel kernel = cl::Kernel(m_program, "naive_bmmc");
            kernel.setArg(0, n);
            kernel.setArg(1, bmmc_d);
            kernel.setArg(2, input_d);
            kernel.setArg(3, output_d);
            CLguard(m_queue.enqueueNDRangeKernel(kernel, 
                cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &ev));
            events.push_back(ev);

            // Copy the output back to the input. We will likely be smarter in the actual Futhark implementation.
            CLguard(m_queue.enqueueCopyBuffer(output_d, input_d, 0, 0, (1 << n) * sizeof(int)));
        }

        return events;
    }

    //std::vector<cl::Event> fast_bmmc(
    //    int n, const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d)
    //{
    //    // Calculate the LU decomposition
    //    assert(bmmc.size() == n);
    //    auto [U, L, perm] = bmmc_A_ULP_decomp(bmmc);
    //    assert(bmmc == bmmc_mult_bmmc(bmmc_mult_bmmc(U, L), perm_to_bmmc(perm)));
//
    //    // Factorize the permutation
    //    const int K = std::min(5, n); // This is the size of the lsb block for the block swap
    //    const int SEG_INIT = std::min(8, n);
    //    const int SEG_FIN = std::min(5, n);
    //    // We fuse the first block swap into the permutation
    //    std::vector<permutation> factors = factorize_perm_init_fin(
    //        //compose_perm(rotate_perm(K, n), perm), SEG_INIT, n - SEG_FIN);
    //        perm, SEG_INIT, n - SEG_FIN);
    //    std::vector<BMMC> bmmcs;
    //    for (int i = 0; i < factors.size(); i++) {
    //        bmmcs.push_back(perm_to_bmmc(factors[i]));
    //    }
    //    // Try to fuse L2 with the last factor
    //    BMMC L2 = bmmc_rotate_cols(bmmc_rotate_rows(L, K), K);   
    //    //bool fuse_L2 = (factors.size() & 1) != 0; 
    //    bool fuse_L2 = false;
    //    if (fuse_L2) {
    //        bmmcs[bmmcs.size()-1] = bmmc_mult_bmmc(L2, bmmcs.back());
    //    }
    //    
    //    // Allocate the buffers
    //    cl::Buffer bmmc_d(m_context, CL_MEM_READ_ONLY, sizeof(uint64_t) * n);
    //    std::vector<cl::Event> events;
    //    cl::Event ev;
//
    //    // Apply each factor
    //    for (int i = 0; i < factors.size(); i++) {
    //        // Upload the bmmc to the GPU
    //        CLguard(m_queue.enqueueWriteBuffer(bmmc_d, CL_TRUE, 0, sizeof(uint64_t) * n, bmmcs[i].data()));
//
    //        // This is a lower sort, use the MRC kernel.
    //        if ((i & 1) == 0) {
    //            const int GROUP_BITS = SEG_INIT;
    //            cl::Kernel kernel = cl::Kernel(m_program, "mrc");
    //            kernel.setArg(0, n);
    //            kernel.setArg(1, GROUP_BITS);
    //            kernel.setArg(2, bmmc_d);
    //            kernel.setArg(3, input_d);
    //            kernel.setArg(4, output_d);
    //            kernel.setArg(5, (1 << GROUP_BITS) * sizeof(int), NULL);
    //            CLguard(m_queue.enqueueNDRangeKernel(kernel, 
    //                cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &ev));
    //        }
    //        // This is a higher sort, use the naive BMMC kernel.
    //        else {
    //            const int GROUP_BITS = SEG_FIN;
    //            cl::Kernel kernel = cl::Kernel(m_program, "naive_bmmc");
    //            kernel.setArg(0, n);
    //            kernel.setArg(1, bmmc_d);
    //            kernel.setArg(2, input_d);
    //            kernel.setArg(3, output_d);
    //            CLguard(m_queue.enqueueNDRangeKernel(kernel, 
    //                cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &ev));
    //        }   
    //        // Bookkeeping 
    //        events.push_back(ev);
    //        CLguard(m_queue.enqueueCopyBuffer(output_d, input_d, 0, 0, (1 << n) * sizeof(int)));
    //    }
//
    //    // Block swap
    //    {
    //        // Ideally we want block_size >= 5, but since the group size is 2**(2*block_size), 
    //        // the maximum is block_size = 4 on AMD GPUs.
    //        // We also need to have block_size <= low and high. Otherwise we have some freedom in the choice of k.
    //        const int LOW = n - K;
    //        const int HIGH = K;
    //        const int block_size = std::min(4, std::min(LOW, HIGH));
    //        assert(0 <= LOW && 0 <= HIGH && LOW + HIGH == n);
//
    //        cl::Kernel kernel = cl::Kernel(m_program, "block_swap");
    //        kernel.setArg(0, LOW);
    //        kernel.setArg(1, HIGH);
    //        kernel.setArg(2, block_size);
    //        kernel.setArg(3, input_d);
    //        kernel.setArg(4, output_d);
    //        kernel.setArg(5, sizeof(int) * ((1 << block_size) * ((1 << block_size) + 1)), nullptr);
    //        CLguard(m_queue.enqueueNDRangeKernel(kernel, 
    //            cl::NullRange, cl::NDRange(1 << LOW, 1 << HIGH), cl::NDRange(1 << block_size, 1 << block_size), nullptr, &ev));    
    //        events.push_back(ev);
    //        CLguard(m_queue.enqueueCopyBuffer(output_d, input_d, 0, 0, (1 << n) * sizeof(int)));
    //    }
    //    
    //    // Perform L2 if needed
    //    if (!fuse_L2) {
    //        CLguard(m_queue.enqueueWriteBuffer(bmmc_d, CL_TRUE, 0, sizeof(uint64_t) * n, L2.data()));
    //        
    //        // This is the same k as in the transposition   
    //        const int GROUP_BITS = K;
    //        cl::Kernel kernel = cl::Kernel(m_program, "mrc");
    //        kernel.setArg(0, n);
    //        kernel.setArg(1, GROUP_BITS);
    //        kernel.setArg(2, bmmc_d);
    //        kernel.setArg(3, input_d);
    //        kernel.setArg(4, output_d);
    //        kernel.setArg(5, (1 << GROUP_BITS) * sizeof(int), NULL);
    //        CLguard(m_queue.enqueueNDRangeKernel(kernel, 
    //            cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &ev));
    //        events.push_back(ev);
    //        CLguard(m_queue.enqueueCopyBuffer(output_d, input_d, 0, 0, (1 << n) * sizeof(int)));
    //    }
//
    //    // Block swap
    //    {
    //        // Ideally we want block_size >= 5, but since the group size is 2**(2*block_size), 
    //        // the maximum is block_size = 4 on AMD GPUs.
    //        // We also need to have block_size <= low and high. Otherwise we have some freedom in the choice of k.
    //        const int LOW = K;
    //        const int HIGH = n - K;
    //        int block_size = std::min(4, std::min(LOW, HIGH));
    //        assert(0 <= LOW && 0 <= HIGH && LOW + HIGH == n);
    //        
    //        cl::Kernel kernel = cl::Kernel(m_program, "block_swap");
    //        kernel.setArg(0, LOW);
    //        kernel.setArg(1, HIGH);
    //        kernel.setArg(2, block_size);
    //        kernel.setArg(3, input_d);
    //        kernel.setArg(4, output_d);
    //        kernel.setArg(5, sizeof(int) * ((1 << block_size) * ((1 << block_size) + 1)), nullptr);
    //        CLguard(m_queue.enqueueNDRangeKernel(kernel, 
    //            cl::NullRange, cl::NDRange(1 << LOW, 1 << HIGH), cl::NDRange(1 << block_size, 1 << block_size), nullptr, &ev));    
    //        events.push_back(ev);
    //        CLguard(m_queue.enqueueCopyBuffer(output_d, input_d, 0, 0, (1 << n) * sizeof(int)));
    //    }
    //    
    //    // Perform U
    //    {
    //        CLguard(m_queue.enqueueWriteBuffer(bmmc_d, CL_TRUE, 0, sizeof(uint64_t) * n, U.data()));
    //        // This is the same k as in the block swap   
    //        const int GROUP_BITS = K;
    //        cl::Kernel kernel = cl::Kernel(m_program, "mrc");
    //        kernel.setArg(0, n);
    //        kernel.setArg(1, GROUP_BITS);
    //        kernel.setArg(2, bmmc_d);
    //        kernel.setArg(3, input_d);
    //        kernel.setArg(4, output_d);
    //        kernel.setArg(5, (1 << GROUP_BITS) * sizeof(int), NULL);
    //        CLguard(m_queue.enqueueNDRangeKernel(kernel, 
    //            cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &ev));
    //        events.push_back(ev);
    //        CLguard(m_queue.enqueueCopyBuffer(output_d, input_d, 0, 0, (1 << n) * sizeof(int)));
    //    }
//
    //    return events;
    //}

    std::vector<cl::Event> fast_bmmc(
        int n, const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d)
    {
        // Calculate the LU decomposition
        assert(bmmc.size() == n);
        auto [U, L, perm] = bmmc_A_ULP_decomp(bmmc);
        assert(bmmc == bmmc_mult_bmmc(bmmc_mult_bmmc(U, L), perm_to_bmmc(perm)));

        // Factorize the permutation
        const int K = std::min(5, n); // This is the size of the lsb block for the block swap
        const int SEG_INIT = std::min(17, n);
        const int SEG_FIN = std::max(0, n - 9);
        // We fuse the first block swap into the permutation
        std::vector<permutation> factors = factorize_perm_init_fin(
            compose_perm(rotate_perm(K, n), perm), SEG_INIT, SEG_FIN);
        std::vector<BMMC> bmmcs;
        for (int i = 0; i < factors.size(); i++) {
            bmmcs.push_back(perm_to_bmmc(factors[i]));
        }
        // Fuse L2 with the last factor.
        // It seems that fusing L2 gives little to no overhead to the last factor,
        // independently of whether it is an initial or a final sort.
        BMMC L2 = bmmc_rotate_cols(bmmc_rotate_rows(L, K), K);   
        bool fuse_L2 = true;
        if (fuse_L2) {
            bmmcs[bmmcs.size()-1] = bmmc_mult_bmmc(L2, bmmcs.back());
        }

        // Allocate the buffers
        cl::Buffer bmmc_d(m_context, CL_MEM_READ_ONLY, sizeof(uint64_t) * n);
        std::vector<cl::Event> events;
        cl::Event ev;

        // Apply each factor
        for (int i = 0; i < factors.size(); i++) {
            // Upload the bmmc to the GPU
            CLguard(m_queue.enqueueWriteBuffer(bmmc_d, CL_TRUE, 0, sizeof(uint64_t) * n, bmmcs[i].data()));

            const int GROUP_BITS = 8;
            cl::Kernel kernel = cl::Kernel(m_program, "naive_bmmc");
            kernel.setArg(0, n);
            kernel.setArg(1, bmmc_d);
            kernel.setArg(2, input_d);
            kernel.setArg(3, output_d);
            CLguard(m_queue.enqueueNDRangeKernel(kernel, 
                cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &ev));
            // Bookkeeping 
            events.push_back(ev);
            CLguard(m_queue.enqueueCopyBuffer(output_d, input_d, 0, 0, (1 << n) * sizeof(int)));
        }

        // Perform L2 if needed
        if (!fuse_L2) {
            BMMC L2 = bmmc_rotate_cols(bmmc_rotate_rows(L, K), K);
            CLguard(m_queue.enqueueWriteBuffer(bmmc_d, CL_TRUE, 0, sizeof(uint64_t) * n, L2.data()));
            
            // This is the same k as in the transposition   
            const int GROUP_BITS = K;
            cl::Kernel kernel = cl::Kernel(m_program, "mrc");
            kernel.setArg(0, n);
            kernel.setArg(1, GROUP_BITS);
            kernel.setArg(2, bmmc_d);
            kernel.setArg(3, input_d);
            kernel.setArg(4, output_d);
            kernel.setArg(5, (1 << GROUP_BITS) * sizeof(int), NULL);
            CLguard(m_queue.enqueueNDRangeKernel(kernel, 
                cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &ev));
            events.push_back(ev);
            CLguard(m_queue.enqueueCopyBuffer(output_d, input_d, 0, 0, (1 << n) * sizeof(int)));
        }

        // Block swap
        {
            // Ideally we want block_size >= 5, but since the group size is 2**(2*block_size), 
            // the maximum is block_size = 4 on AMD GPUs.
            // We also need to have block_size <= low and high. Otherwise we have some freedom in the choice of k.
            const int LOW = K;
            const int HIGH = n - K;
            int block_size = std::min(4, std::min(LOW, HIGH));
            assert(0 <= LOW && 0 <= HIGH && LOW + HIGH == n);
            
            cl::Kernel kernel = cl::Kernel(m_program, "block_swap");
            kernel.setArg(0, LOW);
            kernel.setArg(1, HIGH);
            kernel.setArg(2, block_size);
            kernel.setArg(3, input_d);
            kernel.setArg(4, output_d);
            kernel.setArg(5, sizeof(int) * ((1 << block_size) * ((1 << block_size) + 1)), nullptr);
            CLguard(m_queue.enqueueNDRangeKernel(kernel, 
                cl::NullRange, cl::NDRange(1 << LOW, 1 << HIGH), cl::NDRange(1 << block_size, 1 << block_size), nullptr, &ev));    
            events.push_back(ev);
            CLguard(m_queue.enqueueCopyBuffer(output_d, input_d, 0, 0, (1 << n) * sizeof(int)));
        }
        
        // Perform U
        {
            CLguard(m_queue.enqueueWriteBuffer(bmmc_d, CL_TRUE, 0, sizeof(uint64_t) * n, U.data()));
            // This is the same k as in the block swap   
            const int GROUP_BITS = K;
            cl::Kernel kernel = cl::Kernel(m_program, "mrc");
            kernel.setArg(0, n);
            kernel.setArg(1, GROUP_BITS);
            kernel.setArg(2, bmmc_d);
            kernel.setArg(3, input_d);
            kernel.setArg(4, output_d);
            kernel.setArg(5, (1 << GROUP_BITS) * sizeof(int), NULL);
            CLguard(m_queue.enqueueNDRangeKernel(kernel, 
                cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &ev));
            events.push_back(ev);
            CLguard(m_queue.enqueueCopyBuffer(output_d, input_d, 0, 0, (1 << n) * sizeof(int)));
        }

        return events;
    }


    std::vector<cl::Event> scatter_bmmc(
        int n, const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d)
    {
        // Allocate the buffers
        assert(bmmc.size() == n);
        std::vector<int> idxs_h(1 << n, 0);
        for (int i = 0; i < (1 << n); i++) {
            idxs_h[i] = bmmc_mult_vect(bmmc, i);
        }
        cl::Buffer idxs_d(m_context, CL_MEM_READ_ONLY, sizeof(int) * (1 << n));
        
        // Copy data to the GPU
        CLguard(m_queue.enqueueWriteBuffer(idxs_d, CL_TRUE, 0, sizeof(int) * (1 << n), idxs_h.data()));
        
        // Call the kernel
        const int GROUP_BITS = 5;
        cl::Kernel kernel = cl::Kernel(m_program, "scatter");
        kernel.setArg(0, input_d);
        kernel.setArg(1, idxs_d);
        kernel.setArg(2, output_d);
        cl::Event event;
        CLguard(m_queue.enqueueNDRangeKernel(kernel, 
            cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &event));
        
        return { event };
    }

    std::vector<cl::Event> dummy(
        int n, int k, const cl::Buffer& input_d, const cl::Buffer& output_d)
    {
        const int GROUP_BITS = std::min(k, 8);
        
        cl::Kernel kernel = cl::Kernel(m_program, "dummy");
        kernel.setArg(0, n);
        kernel.setArg(1, k);
        kernel.setArg(2, input_d);
        kernel.setArg(3, output_d);
        cl::Event event;
        CLguard(m_queue.enqueueNDRangeKernel(kernel, 
                cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &event));
        
        return { event };
    }

    
    std::vector<cl::Event> copy(
        int n, const cl::Buffer& input_d, const cl::Buffer& output_d)
    {
        const int GROUP_BITS = std::min(6, n);
        
        cl::Kernel kernel = cl::Kernel(m_program, "copy");
        kernel.setArg(0, input_d);
        kernel.setArg(1, output_d);
        cl::Event event;
        CLguard(m_queue.enqueueNDRangeKernel(kernel, 
                cl::NullRange, cl::NDRange(1 << n), cl::NDRange(1 << GROUP_BITS), nullptr, &event));
        
        return { event };
    }

    std::vector<cl::Event> block_swap(
        int low, int high, const cl::Buffer& input_d, const cl::Buffer& output_d)
    {
        int n = low + high;
        int block_size = std::min(4, std::min(low, high));
        
        cl::Kernel kernel = cl::Kernel(m_program, "block_swap");
        kernel.setArg(0, low);
        kernel.setArg(1, high);
        kernel.setArg(2, block_size);
        kernel.setArg(3, input_d);
        kernel.setArg(4, output_d);
        kernel.setArg(5, sizeof(int) * ((1 << block_size) * ((1 << block_size) + 1)), nullptr);
        cl::Event event;
        CLguard(m_queue.enqueueNDRangeKernel(kernel, 
                cl::NullRange, cl::NDRange(1 << low, 1 << high), cl::NDRange(1 << block_size, 1 << block_size), nullptr, &event));
        
        return { event };
    }
    
    template<typename T>
    double benchmark(
        std::function<T()> random_state,
        std::function<std::vector<cl::Event>(const T&, const cl::Buffer&, const cl::Buffer&)> f,
        int input_size,
        int iterations,
        bool verbose = true) 
    {
        // Generate the input
        std::vector<int> input = random_array(input_size);

        // Copy the input to the GPU
        cl::Buffer input_d(m_context, CL_MEM_READ_WRITE, sizeof(int) * input.size());
        cl::Buffer output_d(m_context, CL_MEM_READ_WRITE, sizeof(int) * input.size());
        CLguard(m_queue.enqueueWriteBuffer(input_d, CL_TRUE, 0, sizeof(int) * input.size(), input.data()));

        // Get the time measurements
        std::vector<std::vector<double>> times(iterations, std::vector<double>());
        for (int it = 0; it < iterations; it++) {
            if (verbose && (it % ((iterations / 10) + 1) == 0)) {
                std::cout << ".";
                std::cout.flush();
            }

            std::vector<cl::Event> events = f(random_state(), input_d, output_d);

            CLguard(m_queue.finish());
            for (int i = 0; i < events.size(); i++) {
                uint64_t start, end;
                CLguard(events[i].getProfilingInfo(CL_PROFILING_COMMAND_START, &start));
                CLguard(events[i].getProfilingInfo(CL_PROFILING_COMMAND_END, &end));
                double delta = (end - start) / (double)1000000000;
                times[it].push_back(delta);
            } 
        }
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
                std::vector<double> avg(event_count, 0);
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
                        std::cout << avg[i] * 1000 << "ms  ";
                    }
                    std::cout << "\n";
                }
            }
        }
        // Compute the average time per iteration
        float total = 0;
        for (const auto& ts : times) {
            for (double t : ts) {
                total += t;
            }
        }
        return total / iterations;
    }

    template <typename T>
    void test(
        std::function<T()> random_state,
        std::function<std::vector<cl::Event>(const T& state, const cl::Buffer&, const cl::Buffer&)> f_test,
        std::function<std::vector<int>(T& state, const std::vector<int>&)> f_expected,    
        int input_size,
        int iterations)
    {   
        cl::Buffer input_d(m_context, CL_MEM_READ_ONLY, sizeof(int) * input_size);
        cl::Buffer output_d(m_context, CL_MEM_READ_WRITE, sizeof(int) * input_size);
        
        // Do the GPU computation
        for (int it = 0; it < iterations; it++) {
            if (it % ((iterations / 10) + 1) == 0) {
                std::cout << ".";
                std::cout.flush();
            }
            // Generate the input and the state
            std::vector<int> input = random_array(input_size);
            std::vector<int> output(input.size(), 0);
            T state = random_state();
            
            // Do the computation on the GPU
            CLguard(m_queue.enqueueWriteBuffer(input_d, CL_TRUE, 0, sizeof(int) * input.size(), input.data()));
            std::vector<cl::Event> events = f_test(state, input_d, output_d);
            CLguard(m_queue.enqueueReadBuffer(output_d, CL_TRUE, 0, sizeof(int) * input.size(), output.data()));
    
            // Compare the result to the expected one
            assert(output == f_expected(state, input));
        }
        std::cout << std::endl;
    }
};

std::vector<int> cpu_bmmc(const BMMC& bmmc, const std::vector<int>& input)
{
    std::vector<int> output(input.size(), 0);
    for (int i = 0; i < input.size(); i++) {
        output[bmmc_mult_vect(bmmc, i)] = input[i];
    }
    return output;
}

void benchmark_all(App& app, int bits, int iterations) 
{  
    std::vector<int> arr = random_array(1 << bits);
    double time;

    //assert(bits >= 10);
    //time = app.benchmark<std::pair<int, int>>(
    //    [&]() -> std::pair<int, int> 
    //      { int low = std::clamp(rand() % bits, 5, bits-5); return { low, bits - low }; },
    //    [&](const std::pair<int, int>& lh, const cl::Buffer& input_d, const cl::Buffer& output_d) -> std::vector<cl::Event>
    //      { return app.block_swap(lh.first, lh.second, input_d, output_d); },
    //    1 << bits,
    //    iterations);
    //std::cout << "Block swap avg: " << time * 1000 << "ms\n";
    
    time = app.benchmark<int>(
        [&]() -> int { return 0; },
        [&](const int& unused, const cl::Buffer& input_d, const cl::Buffer& output_d) -> std::vector<cl::Event>
          { return app.copy(bits, input_d, output_d); },
        1 << bits,
        iterations);
    std::cout << "Copy avg: " << time * 1000 << "ms\n";

    time = app.benchmark<BMMC>(
        [&]() -> BMMC { return random_invertible_bmmc(bits); },
        [&](const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d) -> std::vector<cl::Event>
          { return app.scatter_bmmc(bits, bmmc, input_d, output_d); },
        1 << bits, 
        iterations);
    std::cout << "Scatter bmmc avg: " << time * 1000 << "ms\n";

    time = app.benchmark<BMMC>(
        [&]() -> BMMC { return random_invertible_bmmc(bits); },
        [&](const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d) -> std::vector<cl::Event>
          { return app.slow_bmmc(bits, bmmc, input_d, output_d); },
        1 << bits, 
        iterations);
    std::cout << "Slow bmmc avg: " << time * 1000 << "ms\n";
        
    time = app.benchmark<permutation>(
        [&]() -> permutation { return random_perm(bits); },
        [&](const permutation& perm, const cl::Buffer& input_d, const cl::Buffer& output_d) -> std::vector<cl::Event>
          { return app.fast_bpc(bits, perm, input_d, output_d); },
        1 << bits, 
        100);
    std::cout << "Fast bpc avg: " << time * 1000 << "ms\n";

    time = app.benchmark<BMMC>(
        [&]() -> BMMC { return random_invertible_bmmc(bits); },
        [&](const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d) -> std::vector<cl::Event>
          { return app.fast_bmmc(bits, bmmc, input_d, output_d); },
        1 << bits, 
        iterations);
    std::cout << "Fast bmmc avg: " << time * 1000 << "ms\n";
}

void test_all(App& app, int bits, int iterations)
{
    std::cout << "Testing slow bmmc\n  ";
    app.test<permutation>(
        [&]() -> permutation { return random_perm(bits); },
        [&](const permutation& perm, const cl::Buffer& input_d, const cl::Buffer& output_d) -> std::vector<cl::Event>
            { return app.slow_bmmc(bits, perm_to_bmmc(perm), input_d, output_d); },
        [&](const permutation& perm, const std::vector<int>& input) -> std::vector<int> 
            { return cpu_bmmc(perm_to_bmmc(perm), input); },
        1 << bits,
        iterations);
    
    std::cout << "Testing scatter bmmc\n  ";
    app.test<permutation>(
        [&]() -> permutation { return random_perm(bits); },
        [&](const permutation& perm, const cl::Buffer& input_d, const cl::Buffer& output_d) -> std::vector<cl::Event>
            { return app.scatter_bmmc(bits, perm_to_bmmc(perm), input_d, output_d); },
        [&](const permutation& perm, const std::vector<int>& input) -> std::vector<int> 
            { return cpu_bmmc(perm_to_bmmc(perm), input); },
        1 << bits,
        iterations);
    
    std::cout << "Testing block swap\n  ";
    app.test<std::pair<int, int>>(
        [&]() -> std::pair<int, int> 
          { int low = rand() % bits; return { low, bits - low }; },
        [&](const std::pair<int, int>& lh, const cl::Buffer& input_d, const cl::Buffer& output_d) -> std::vector<cl::Event>
            { return app.block_swap(lh.first, lh.second, input_d, output_d); },
        [&](const std::pair<int, int>& lh, const std::vector<int>& input) -> std::vector<int>
            { return cpu_bmmc(perm_to_bmmc(rotate_perm(lh.second, lh.first + lh.second)), input); },
        1 << bits,
        iterations);
    
    std::cout << "Testing fast bpc\n  ";
    app.test<permutation>(
        [&]() -> permutation { return random_perm(bits); },
        [&](const permutation& perm, const cl::Buffer& input_d, const cl::Buffer& output_d) -> std::vector<cl::Event>
            { return app.fast_bpc(bits, perm, input_d, output_d); },
        [&](const permutation& perm, const std::vector<int>& input) -> std::vector<int> 
            { return cpu_bmmc(perm_to_bmmc(perm), input); },
        1 << bits,
        iterations);

    std::cout << "Testing fast bmmc\n  ";
    app.test<BMMC>(
        [&]() -> BMMC { return perm_to_bmmc(random_perm(bits)); },
        [&](const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d) -> std::vector<cl::Event>
        { return app.fast_bmmc(bits, bmmc, input_d, output_d); },
        [&](const BMMC& bmmc, const std::vector<int>& input) -> std::vector<int> 
        { return cpu_bmmc(bmmc, input); },
        1 << bits,
        iterations);
}   

void measure_varying_n(int n_min, int n_max, std::function<double(int)> measure)
{
    std::vector<std::tuple<int, double>> times;
    for (int n = n_min; n <= n_max; n++) {
        double t = measure(n);
        times.push_back({ n, t });
    }
    printf("Size of input array = 2**n\n");
    for (int i = 0; i < times.size(); i++) {
        auto [n, t] = times[i];
        auto [n_prev, t_prev] = i > 0 ? times[i-1] : std::tuple<int, double>({ 1, t });
        printf("n=%2d  time=%3.2fms  scale:%.2f\n", n, 1000 * t, t / t_prev);
    }
}

int main()
{
    App app = App(true);

    int n = 27;
    double time = app.benchmark<BMMC>(
        [&]() -> BMMC { return perm_to_bmmc(reverse_perm(n)); },
        [&](const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d) -> std::vector<cl::Event>
          { return app.slow_bmmc(n, bmmc, input_d, output_d); },
        1 << n, 
        10);
    std::cout << "Slow bmmc avg: " << time * 1000 << "ms\n";
}

// Copy VS block bit-reversal permutation, varying array sizes.
//int main()
//{
//    App app = App(true);
//
//    // Measure the copy for different array sizes.
//    printf("Simple copy\n");
//    measure_varying_n(18, 28, [&](int n) -> double { 
//        return app.benchmark<int>(
//            [&]() -> int { return 0; },
//            [&](const int& unused, const cl::Buffer& input_d, const cl::Buffer& output_d) -> std::vector<cl::Event>
//              { return app.copy(n, input_d, output_d); },
//            1 << n, 100, false); 
//    });
//
//    printf("\n");
//
//    // Measure the block bit-reversal permutation for different array sizes, fixed block size.
//    int k = 5;
//    printf("Block bit-reversal permutation\n");
//    printf("Block size = 2**k where k=%d\n", k);
//    measure_varying_n(18, 28, [&](int n) -> double {
//        return app.benchmark<BMMC>(
//            [&]() -> BMMC 
//              { return diag_block_bmmc({ perm_to_bmmc(identity_perm(k)), perm_to_bmmc(reverse_perm(n-k)) }); },
//            [&](const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d) -> std::vector<cl::Event>
//              { return app.slow_bmmc(n, bmmc, input_d, output_d); },
//            1 << n, 100, false);
//    });
//    return 0;
//}  


// Block permutation and segmented permutation, both random and bit-reverse,
// fixed array size but varying block size.
//int main()
//{
//    App app = App(true);
//
//    //for (int n = 5; n < 18; n++) {
//    //    test_all(app, n, 10);
//    //}
//    //benchmark_all(app, 27, 100);
//
//    int n = 27;
//    std::vector<std::tuple<int, double, double, double, double>> times;
//    for (int k = 0; k <= n; k++) {
//        double t_seg_rand = app.benchmark<BMMC>(
//            [&]() -> BMMC { 
//            return diag_block_bmmc({ perm_to_bmmc(random_perm(k)), perm_to_bmmc(identity_perm(n-k)) }); },
//            [&](const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d)
//            { return app.slow_bmmc(n, bmmc, input_d, output_d); },
//            1 << n,
//            100);
//        double t_block_rand = app.benchmark<BMMC>(
//            [&]() -> BMMC { 
//            return diag_block_bmmc({ perm_to_bmmc(identity_perm(k)), perm_to_bmmc(random_perm(n-k)) }); },
//            [&](const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d)
//            { return app.slow_bmmc(n, bmmc, input_d, output_d); },
//            1 << n,
//            100);
//        double t_seg_rev = app.benchmark<BMMC>(
//            [&]() -> BMMC { 
//            return diag_block_bmmc({ perm_to_bmmc(reverse_perm(k)), perm_to_bmmc(identity_perm(n-k)) }); },
//            [&](const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d)
//            { return app.slow_bmmc(n, bmmc, input_d, output_d); },
//            1 << n,
//            10);
//        double t_block_rev = app.benchmark<BMMC>(
//            [&]() -> BMMC { 
//            return diag_block_bmmc({ perm_to_bmmc(identity_perm(k)), perm_to_bmmc(reverse_perm(n-k)) }); },
//            [&](const BMMC& bmmc, const cl::Buffer& input_d, const cl::Buffer& output_d)
//            { return app.slow_bmmc(n, bmmc, input_d, output_d); },
//            1 << n,
//            10);
//        times.push_back({ k, t_seg_rand, t_block_rand, t_seg_rev, t_block_rev });
//    }
//    for (auto [k, t_seg_rand, t_block_rand, t_seg_rev, t_block_rev] : times) {
//        printf("k=%2d | t_seg_rand=%.2fms  t_seg_rev=%.2fms | t_block_rand=%.2fms  t_block_rev=%.2fms\n", 
//          k, 1000 * t_seg_rand, 1000 * t_seg_rev, 1000 * t_block_rand, 1000 * t_block_rev);
//    }
//    std::cout << "\n";
//    return 0;
//}

//int main()
//{
    // Test the BMMC LU decompositions 
    //int n = 32;
    //BMMC A = perm_to_bmmc(random_perm(n));
    //bmmc_set(A, 3, 5, 1);
    //bmmc_set(A, 6, 5, 1);
    //auto [U, L, p] = bmmc_A_ULP_decomp(A);
    //
    //std::cout << show_bmmc(A) << std::endl;
    //std::cout << show_bmmc(U) << std::endl;
    //std::cout << show_bmmc(L) << std::endl;
    //std::cout << show_bmmc(perm_to_bmmc(p)) << std::endl;
    //
    //assert(A == bmmc_mult_bmmc(bmmc_mult_bmmc(U, L), perm_to_bmmc(p)));
    //
    //return 0;
//}