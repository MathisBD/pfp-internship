
.PHONY: all run clean

all: bmmc.cpp bmmc.cu perm.h
	g++ -Wno-deprecated-declarations -I/opt/rocm-5.3.3/include -L/opt/rocm-5.3.3/lib bmmc.cpp -lOpenCL -o a.out

run: all
	./a.out

clean: 
	rm a.out