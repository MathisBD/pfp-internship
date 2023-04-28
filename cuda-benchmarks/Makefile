
.PHONY: all run clean

all: main.cu kernels.cu perm.h
	nvcc -o test main.cu

run: all
	./test

clean:
	rm test
