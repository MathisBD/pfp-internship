#define CL_TARGET_OPENCL_VERSION 210
#include <CL/cl.hpp>
#include <iostream>
#include <fstream>
#include <sstream>
#include <assert.h>


std::vector<int> inverse_perm(const std::vector<int>& perm)
{
    std::vector<int> inv_perm(perm.size(), 0);
    for (int i = 0; i < perm.size(); i++) {
        assert(0 <= perm[i] && perm[i] < perm.size());
        inv_perm[perm[i]] = i;
    }
    return inv_perm;
}

std::vector<uint64_t> perm_to_bmmc(const std::vector<int>& perm)
{
    std::vector<uint64_t> bmmc;
    std::vector inv_perm = inverse_perm(perm);
    for (int i = 0; i < 64; i++) {
        if (i < perm.size()) {
            bmmc.push_back(1 << inv_perm[i]);
        }
        else {
            bmmc.push_back(0);
        }
    }
    return bmmc;
}

std::string show_bmmc(const std::vector<uint64_t>& bmmc, int n = 64)
{
    std::stringstream buffer;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            buffer << ((bmmc[i] >> j) & 1) << " ";
        }
        buffer << "\n";
    }
    return buffer.str();
}

std::string show_perm(const std::vector<int>& perm)
{
    std::stringstream buffer;
    buffer << "{ ";
    for (int i = 0; i < perm.size(); i++) {
        buffer << perm[i];
        if (i + 1 < perm.size()) {
            buffer << ", ";
        }
    }
    buffer << " }";
    return buffer.str();
}

std::string read_file(const std::string& file_name)
{
    std::ifstream file(file_name);
    std::stringstream buffer;
    if (file.is_open()) {
        buffer << file.rdbuf();
    }
    return buffer.str();
}

std::vector<int> iota(int n)
{
    std::vector<int> res;
    for (int i = 0; i < n; i++) {
        res.push_back(i);
    }
    return res;
}

// Compose two permutations on the same size.
std::vector<int> compose_perm(const std::vector<int>& second, const std::vector<int>& first)
{
    assert(first.size() == second.size());

    std::vector<int> perm;
    for (int i = 0; i < first.size(); i++) {
        perm.push_back(second[first[i]]);
    }
    return perm;
}

// Returns a permutation of the same size as the input, that sorts the given contiguous range,
// and fixes the other elements.
std::vector<int> sort_perm_range(const std::vector<int>& perm, int start, int count)
{
    assert(0 <= start && start < perm.size());
    assert(0 <= count && start + count <= perm.size());

    std::vector<int> res;
    for (int i = 0; i < perm.size(); i++) {
        if (start <= i && i < start + count) {
            int idx = start;
            for (int j = start; j < start + count; j++) {
                if (perm[j] < perm[i]) idx++;
            }
            res.push_back(idx);
        }
        else {
            res.push_back(i);
        }
    }
    return res;
}

bool is_perm_identity(const std::vector<int>& perm) 
{
    for (int i = 0; i < perm.size(); i++) {
        if (perm[i] != i) {
            return false;
        }
    }
    return true;
}

std::vector<int> slow_bpc(
    cl::Context& context, cl::CommandQueue& queue, cl::Program& program,
    const std::vector<int>& perm, const std::vector<int>& input_h)
{
    // Compute the number of bits to encode indices.
    int BITS = 0;
    while ((1 << BITS) < input_h.size()) {
      BITS++;
    }
    assert((1 << BITS) == input_h.size());
    assert(perm.size() == BITS);

    // Allocate the buffers
    std::vector<uint64_t> bmmc_h = perm_to_bmmc(perm);
    std::vector<int> output_h(1 << BITS, 0);
    cl::Buffer bmmc_d(context, CL_MEM_READ_ONLY, sizeof(uint64_t) * 64);
    cl::Buffer input_d(context, CL_MEM_READ_ONLY, sizeof(int) * (1 << BITS));
    cl::Buffer output_d(context, CL_MEM_READ_WRITE, sizeof(int) * (1 << BITS));
    
    // Copy data to the GPU
    queue.enqueueWriteBuffer(bmmc_d, CL_TRUE, 0, sizeof(uint64_t) * 64, bmmc_h.data());
    queue.enqueueWriteBuffer(input_d, CL_TRUE, 0, sizeof(int) * (1 << BITS), input_h.data());
    
    // Call the kernels
    const int GROUP_BITS = 3;
    cl::Kernel naive_bmmc = cl::Kernel(program, "naive_bmmc");
    naive_bmmc.setArg(0, BITS);
    naive_bmmc.setArg(1, bmmc_d);
    naive_bmmc.setArg(2, input_d);
    naive_bmmc.setArg(3, output_d);
    queue.enqueueNDRangeKernel(naive_bmmc, cl::NullRange, cl::NDRange(1 << BITS), cl::NDRange(1 << GROUP_BITS));
    
    // Copy data back to the CPU
    queue.enqueueReadBuffer(output_d, CL_TRUE, 0, sizeof(int) * (1 << BITS), output_h.data());
    return output_h;
}

std::vector<int> fast_bpc(
    cl::Context& context, cl::CommandQueue& queue, cl::Program& program,
    const std::vector<int>& perm, const std::vector<int>& input_h)
{
    cl::Kernel mrc = cl::Kernel(program, "mrc");
    cl::Kernel naive_bmmc = cl::Kernel(program, "naive_bmmc");

    // Compute the number of bits to encode indices.
    int BITS = 0;
    while ((1 << BITS) < input_h.size()) {
      BITS++;
    }
    assert((1 << BITS) == input_h.size());
    assert(perm.size() == BITS);

    // Calculate the permutations
    std::vector<int> remaining = perm;
    std::vector<std::vector<int>> factors;
    auto add_factor = [&factors, &remaining](const std::vector<int>& f) {
        factors.push_back(f);
        remaining = compose_perm(remaining, inverse_perm(factors.back()));
        std::cout << "factor: " << show_perm(factors.back()) << "\nremaining: " << show_perm(remaining) << "\n\n";
    };
    
    const int SEG_INIT = 4;
    const int SEG_FIN = 2;
    while (!is_perm_identity(remaining)) {
        if ((factors.size() & 1) == 0) {
            add_factor(sort_perm_range(remaining, 0, SEG_INIT));
        }
        else {
            add_factor(sort_perm_range(remaining, SEG_FIN, BITS - SEG_FIN));
        }
    }

    // Allocate the buffers
    std::vector<int> output_h(1 << BITS, 0);
    cl::Buffer bmmc_d(context, CL_MEM_READ_ONLY, sizeof(uint64_t) * 64);
    cl::Buffer buffer1_d(context, CL_MEM_READ_WRITE, sizeof(int) * (1 << BITS));
    cl::Buffer buffer2_d(context, CL_MEM_READ_WRITE, sizeof(int) * (1 << BITS));

    // Copy the input vector to the GPU
    queue.enqueueWriteBuffer(buffer1_d, CL_TRUE, 0, sizeof(int) * (1 << BITS), input_h.data());
    
    // Apply each factor
    for (int i = 0; i < factors.size(); i++) {
        // Upload the bmmc to the GPU
        std::vector<uint64_t> bmmc_h = perm_to_bmmc(factors[i]);
        queue.enqueueWriteBuffer(bmmc_d, CL_TRUE, 0, sizeof(uint64_t) * 64, bmmc_h.data());

        // Choose which input and output to use
        const auto& source_d = (i & 1) ? buffer2_d : buffer1_d;
        const auto& dest_d   = (i & 1) ? buffer1_d : buffer2_d;
        std::cout << "source: " << ((i & 1) ? "buffer2_d" : "buffer1_d") << " "
                  << "dest: " << ((i & 1) ? "buffer1_d" : "buffer2_d") << "\n";

        // This is a lower sort, use the MRC kernel.
        if ((i & 1) == 0) {
            const int GROUP_BITS = SEG_INIT;
            cl::Kernel kernel = cl::Kernel(program, "mrc");
            kernel.setArg(0, BITS);
            kernel.setArg(1, GROUP_BITS);
            kernel.setArg(2, bmmc_d);
            kernel.setArg(3, source_d);
            kernel.setArg(4, dest_d);
            kernel.setArg(5, (1 << GROUP_BITS) * sizeof(uint64_t), NULL);
            queue.enqueueNDRangeKernel(kernel, cl::NullRange, cl::NDRange(1 << BITS), cl::NDRange(1 << GROUP_BITS));
        }
        // This is a higher sort, use the naive BMMC kernel.
        else {
            const int GROUP_BITS = SEG_FIN;
            cl::Kernel kernel = cl::Kernel(program, "naive_bmmc");
            kernel.setArg(0, BITS);
            kernel.setArg(1, bmmc_d);
            kernel.setArg(2, source_d);
            kernel.setArg(3, dest_d);
            queue.enqueueNDRangeKernel(kernel, cl::NullRange, cl::NDRange(1 << BITS), cl::NDRange(1 << GROUP_BITS));
        }    
    }

    // Copy the data back to the CPU
    std::cout << "result: " << ((factors.size() & 1) ? "buffer2_d" : "buffer1_d") << "\n";
    queue.enqueueReadBuffer((factors.size() & 1) ? buffer2_d : buffer1_d, CL_TRUE, 0, sizeof(int) * (1 << BITS), output_h.data());
    return output_h;
}

int main() 
{
    // Platform
    std::vector<cl::Platform> all_platforms;
    cl::Platform::get(&all_platforms);
    if(all_platforms.size()==0){
        std::cout << " No platforms found. Check OpenCL installation!\n";
        exit(1);
    }
    cl::Platform platform = all_platforms[0];
    std::cout << "Using platform: " << platform.getInfo<CL_PLATFORM_NAME>() << "\n";
    
    // Device
    std::vector<cl::Device> all_devices;
    platform.getDevices(CL_DEVICE_TYPE_ALL, &all_devices);
    std::cout << "Found " << all_devices.size() << " devices:\n";
    for (auto& device : all_devices) {
        std::cout << "\t" << device.getInfo<CL_DEVICE_NAME>() << "\n";
    }
    if (all_devices.size() == 0) {
        exit(1);
    }
    cl::Device device = all_devices[0];
    std::cout << "Using device: " << device.getInfo<CL_DEVICE_NAME>() << "\n";

    // Context
    cl::Context context({ device });
    cl::CommandQueue queue(context, device);
    
    // Program 
    std::string kernel_code = read_file("test.cu");
    cl::Program::Sources sources;
    sources.push_back({ kernel_code.c_str(), kernel_code.length() });
    cl::Program program(context, sources);
    if (program.build({ device }) != CL_SUCCESS) {
        std::cout << " Error building: " << program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(device) << "\n";
        exit(1);
    }
    
    // Actual computations
    std::vector<int> arr = iota(1 << 6);
    std::vector<int> perm({ 1, 0, 2, 3, 5, 4 });
    std::vector<int> res = fast_bpc(context, queue, program, perm, arr);

    std::cout << "result: ";
    for (int i = 0; i < res.size(); i++) {
        std::cout << res[i] << " ";
    }
    std::cout << "\n";

    return 0;
}