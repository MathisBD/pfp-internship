
.PHONY: all run clean

all: test.cpp test.cu
	g++ -I/opt/rocm-5.3.3/include -L/opt/rocm-5.3.3/lib test.cpp -lOpenCL -o test

run: all
	./test

clean: 
	rm test