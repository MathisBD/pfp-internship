#include <vector>
#include <chrono>
#include <cstdio>
#include <cstdlib>


using clk = std::chrono::system_clock;


int main()
{
    int N = 1 << 12;

    std::vector<int> xs(N*N, 0);
    std::vector<int> ys(N*N, 0);
    for (int i = 0; i < N*N; i++) {
        xs[i] = rand();
    }

    int runs = 10;
    double time;
    for (int r = 0; r < runs; r++) {
        auto start = clk::now();
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                ys[i * N + j] = xs[i * N + j];
            }
        }
        auto end = clk::now();
        time += std::chrono::duration<double>(end - start).count();
    }
    time /= runs;

    printf("Elapsed time : %.2gs\n", time);

    return 0;
}