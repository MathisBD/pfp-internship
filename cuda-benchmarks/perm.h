#pragma once

#include <vector>
#include <stdlib.h>
#include <algorithm>
#include <sstream>


// A permutation is interpreted as the mapping from indices to 
// values in the vector.
using permutation = std::vector<int>;
// A BMMC is always a square matrix. 
// We don't care about complement vectors for now.
using BMMC = std::vector<uint64_t>;

permutation inverse_perm(const permutation& perm)
{
    permutation inv_perm(perm.size(), 0);
    for (int i = 0; i < perm.size(); i++) {
        assert(0 <= perm[i] && perm[i] < perm.size());
        inv_perm[perm[i]] = i;
    }
    return inv_perm;
}

BMMC perm_to_bmmc(const permutation& perm)
{
    BMMC bmmc;
    permutation inv_perm = inverse_perm(perm);
    for (int i = 0; i < perm.size(); i++) {
        bmmc.push_back(1 << inv_perm[i]);
    }
    return bmmc;
}

std::string show_bmmc(const BMMC& bmmc)
{
    std::stringstream buffer;
    for (int i = 0; i < bmmc.size(); i++) {
        for (int j = 0; j < bmmc.size(); j++) {
            if ((bmmc[i] >> j) & 1) {
                buffer << "1";
            }
            else {
                buffer << ".";
            }
        }
        buffer << "\n";
    }
    return buffer.str();
}

std::string show_perm(const permutation& perm)
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


std::vector<int> iota(int n)
{
    std::vector<int> res;
    for (int i = 0; i < n; i++) {
        res.push_back(i);
    }
    return res;
}

// Compose two permutations of the same size.
permutation compose_perm(const permutation& second, const permutation& first)
{
    assert(first.size() == second.size());

    permutation perm;
    for (int i = 0; i < first.size(); i++) {
        perm.push_back(second[first[i]]);
    }
    return perm;
}

// Returns a permutation of the same size as the input, that sorts the given contiguous range,
// and fixes the other elements.
permutation sort_perm_range(const permutation& perm, int start, int count)
{
    assert(0 <= start && start < perm.size());
    assert(0 <= count && start + count <= perm.size());

    permutation res;
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

bool is_perm_identity(const permutation& perm) 
{
    for (int i = 0; i < perm.size(); i++) {
        if (perm[i] != i) {
            return false;
        }
    }
    return true;
}

int permute_bits(const permutation& perm, int idx)
{
    int res = 0;
    for (int i = 0; i < perm.size(); i++) {
        int bit = (idx >> i) & 1;
        res |= bit << perm[i];
    }
    return res;
}

// This should return every permutation with the same probability
permutation random_perm(int size)
{
    if (size == 0) {
        return std::vector<int>();
    }

    // Choose the image of the last element
    int last = rand() % size;
    // Choose the image of the other elements
    permutation rest = random_perm(size - 1);
    // Combine them
    permutation perm(size, 0);
    for (int i = 0; i < size-1; i++) {
        perm[i] = rest[i] < last ? rest[i] : rest[i] + 1;
    }
    perm[size-1] = last;
    return perm;
}


bool is_perm_valid(const permutation& perm)
{
    permutation perm_copy = perm;
    std::sort(perm_copy.begin(), perm_copy.end());
    return is_perm_identity(perm_copy);
}

std::vector<int> random_array(int size)
{
    std::vector<int> arr;
    for (int i = 0; i < size; i++) {
        arr.push_back(rand());
    }
    return arr;
}

inline permutation identity_perm(int size)
{
    return iota(size);
}

inline permutation transpose_perm(int i, int j, int size)
{
    permutation perm = identity_perm(size);
    perm[i] = j;
    perm[j] = i;
    return perm;
}

inline permutation rotate_perm(int k, int size)
{
    permutation perm;
    for (int i = 0; i < size; i++) {
        perm.push_back((i + k) % size);
    }
    return perm;
}

permutation reverse_perm(int size)
{
    permutation perm;
    for (int i = 0; i < size; i++) {
        perm.push_back(size - 1 - i);
    }
    return perm;
}

void bmmc_set(BMMC& bmmc, int i, int j, int bit)
{
    // Set the bit
    if (bit & 1) {
        bmmc[i] |= 1 << j;
    }
    // Clear the bit
    else {
        bmmc[i] &= ~(1 << j);
    }
}

int bmmc_get(const BMMC& bmmc, int i, int j)
{
    return (bmmc[i] >> j) & 1;
}

inline BMMC bmmc_zero(int size)
{
    return std::vector<uint64_t>(size, 0);
}

BMMC bmmc_identity(int size)
{
    assert(0 <= size && size < 64);
    BMMC bmmc = bmmc_zero(size);
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            if (i == j) {
                bmmc_set(bmmc, i, j, 1);
            }
            else {
                bmmc_set(bmmc, i, j, 0);
            }
        }
    }
    return bmmc;
}

BMMC bmmc_mult_bmmc(const BMMC& A, const BMMC& B)
{
    assert(A.size() == B.size());
    int n = A.size();
    
    BMMC C = bmmc_zero(n);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            int bit = 0;
            for (int k = 0; k < n; k++) {
                bit ^= bmmc_get(A, i, k) & bmmc_get(B, k, j);
            }
            bmmc_set(C, i, j, bit);
        }
    }
    return C;
}

uint64_t bmmc_mult_vect(const BMMC& A, uint64_t v)
{
    uint64_t res = 0;
    for (int i = 0; i < A.size(); i++) {
        int bit = 0;
        for (int j = 0; j < A.size(); j++) {
            bit ^= bmmc_get(A, i, j) & ((v >> j) & 1);
        }
        res |= bit << i;
    }
    return res;
}

// Transpose a BMMC matrix.
BMMC bmmc_transpose(const BMMC& A)
{
    BMMC B = bmmc_zero(A.size());
    for (int i = 0; i < A.size(); i++) {
        for (int j = 0; j < A.size(); j++) {
            bmmc_set(B, i, j, bmmc_get(A, j, i));
        }
    }
    return B;
}

// Point-wise addition (i.e. XOR) of two BMMC matrices.
BMMC bmmc_add(const BMMC& A, const BMMC& B)
{
    assert(A.size() == B.size());
    BMMC C;
    for (int i = 0; i < A.size(); i++) {
        C.push_back(A[i] ^ B[i]);
    }
    return C;
}

// Perform the column-row matrix product col^T*row
BMMC bmmc_col_row(uint64_t col, uint64_t row, int size)
{
    BMMC A = bmmc_zero(size);
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            int bit = ((col >> i) & 1) & ((row >> j) & 1);
            bmmc_set(A, i, j, bit);
        }
    }
    return A;
}


// Factorize a permutation into a sequence of permutations that act alternatively 
// only on the initial (resp. final) bits of the input.
// If factorize(p, k_init, k_fin) = [p0, p1, p2, p3, ...] then :
//   p = ... . p3 . p2 . p1 . p0
//   p0, p2, p4, ... act only on the k_init initial bits (lsbs)
//   p1, p3, p5, ... act only on the k_fin final bits (msbs) 
// It must hold that k_init + k_fin > p.size() for the method to work.
// 'start_with_init' controls whether p0 is an initial permutation or a final permutation.
std::vector<permutation> factorize_perm_init_fin(
    const permutation& perm, int k_init, int k_fin, bool start_with_init = true)
{
    int n = perm.size();
    assert(0 <= k_init && k_init <= n);
    assert(0 <= k_fin && k_fin <= n);
    
    if (k_init == n) {
        return { perm };
    }
    assert(n < k_init + k_fin);

    permutation remaining = perm;
    std::vector<permutation> factors;
    auto add_factor = [&factors, &remaining](const permutation& f) {
        factors.push_back(f);
        remaining = compose_perm(remaining, inverse_perm(f));
    };
    
    while (!is_perm_identity(remaining)) {
        if ((start_with_init && factors.size() % 2 == 0) || (!start_with_init && factors.size() % 2 == 1)) {
            add_factor(sort_perm_range(remaining, 0, k_init));
        }
        else {
            add_factor(sort_perm_range(remaining, n - k_fin, k_fin));
        }
    }

    // Check the result
    permutation test = identity_perm(n);
    for (const permutation& f : factors) {
        test = compose_perm(f, test);
    }
    assert(test == perm);

    return factors;
}

// We are only allowed to sort the k_fin last bits of perm.
std::vector<permutation> factorize_perm_fin(const permutation& perm, int k_fin)
{
    int n = perm.size();
    assert(0 <= k_fin && k_fin <= n);
    
    if (k_fin == n) {
        return { perm };
    }
    assert(n < 2 * k_fin);

    permutation remaining = perm;
    std::vector<permutation> factors;
    auto add_factor = [&factors, &remaining](const permutation& f) {
        factors.push_back(f);
        remaining = compose_perm(remaining, inverse_perm(f));
    };
    

    while (!is_perm_identity(remaining)) {
        // sort the end
        if ((factors.size() & 1) == 0) {
            add_factor(sort_perm_range(remaining, n - k_fin, k_fin));
        }
        // sort the start
        else {
            add_factor(rotate_perm(n - k_fin, n));
            add_factor(sort_perm_range(remaining, n - k_fin, k_fin));
            add_factor(rotate_perm(k_fin, n));
        }
    }

    // Check the result
    permutation test = identity_perm(n);
    for (const permutation& f : factors) {
        test = compose_perm(f, test);
    }
    assert(test == perm);

    return factors;
}


// Rotate the rows k steps towards the bottom
BMMC bmmc_rotate_rows(const BMMC& A, int k)
{
    BMMC B = bmmc_zero(A.size());
    for (int i = 0; i < A.size(); i++) {
        for (int j = 0; j < A.size(); j++) {
            // Pay attention to the (int) conversion for the modulo.
            int i2 = (i - k) % (int)A.size();
            if (i2 < 0) { 
                i2 += A.size();
            }
            assert(0 <= i2 && i2 < A.size());
            bmmc_set(B, i, j, bmmc_get(A, i2, j));
        }
    }
    return B;
}

// Rotate the columns k steps towards the right
BMMC bmmc_rotate_cols(const BMMC& A, int k)
{
    BMMC B = bmmc_zero(A.size());
    for (int i = 0; i < A.size(); i++) {
        for (int j = 0; j < A.size(); j++) {
            // Pay attention to the (int) conversion for the modulo.
            int j2 = (j - k) % (int)A.size();
            if (j2 < 0) { 
                j2 += A.size();
            }
            assert(0 <= j2 && j2 < A.size());
            bmmc_set(B, i, j, bmmc_get(A, i, j2));
        
        }
    }
    return B;
}



BMMC diag_block_bmmc(const std::vector<BMMC>& bmmcs)
{
    int n = 0;
    for (const BMMC& A : bmmcs) {
        n += A.size();
    }

    BMMC B = bmmc_zero(n);
    int ofs = 0;
    for (const BMMC& A : bmmcs) {
        for (int i = 0; i < A.size(); i++) {
            for (int j = 0; j < A.size(); j++) {
                bmmc_set(B, ofs + i, ofs + j, bmmc_get(A, i, j));
            }
        }
        ofs += A.size();
    }
    return B;
}

// if perm == rotate_perm(k, n) then return k else return -1
int is_rotate_perm(const permutation& perm)
{
    int k = perm[0];
    for (int i = 0; i < perm.size(); i++) {
        if (perm[i] != (i+k) % perm.size()) {
            return -1;
        }
    }
    return k;
}
