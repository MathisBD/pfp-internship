#pragma once

#include <vector>
#include <stdlib.h>
#include <algorithm>


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

// Decompose a matrix A as P*A = L*U
// where P is a permutation matrix
//       L is a lower triangular matrix
//       U is an upper triangular matrix 
std::tuple<permutation, BMMC, BMMC> bmmc_PA_LU_decomp(const BMMC& A)
{
    if (A.size() <= 1) {
        return { identity_perm(A.size()), bmmc_identity(A.size()), A };
    }

    auto block = [](uint64_t a, uint64_t r, uint64_t c, const BMMC& X) -> BMMC {
        BMMC Y;
        Y.push_back((a & 1) | (r << 1));
        for (int i = 0; i < X.size(); i++) {
            Y.push_back(((c >> i) & 1) | (X[i] << 1));
        }
        return Y;
    };

    auto unblock = [](const BMMC& X) -> std::tuple<uint64_t, uint64_t, uint64_t, BMMC> {
        uint64_t a = bmmc_get(X, 0, 0);
        uint64_t r = X[0] >> 1;
        uint64_t c = 0;
        BMMC Y;
        for (int i = 1; i < X.size(); i++) {
            c |= (X[i] & 1) << (i-1);
            Y.push_back(X[i] >> 1);
        }
        return { a, r, c, Y };
    };

    int n = A.size();
    int i = 0;
    while (i < n && bmmc_get(A, i, 0) == 0) {
        i++;
    }
    if (i >= n) {
        throw std::invalid_argument("bmmc_PA_LU_decomp : the input matrix is not invertible");
    }
    permutation p = transpose_perm(0, i, n);
    
    BMMC pA = bmmc_mult_bmmc(perm_to_bmmc(p), A);
    auto [a1, r1, c1, A1] = unblock(pA);
    BMMC A1_cr = bmmc_add(A1, bmmc_col_row(c1, r1, n-1));
    auto [p1, L1, U1] = bmmc_PA_LU_decomp(A1_cr);

    permutation p1_shift;
    p1_shift.push_back(0);
    for (int x : p1) {
        p1_shift.push_back(x + 1);
    }

    permutation p2 = compose_perm(p1_shift, p);
    BMMC L2 = block(1, 0, bmmc_mult_vect(perm_to_bmmc(p1), c1), L1);
    BMMC U2 = block(1, r1, 0, U1);

    // Check L2 and U2 are of the correct form
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i == j && (bmmc_get(L2, i, j) != 1 || bmmc_get(U2, i, j) != 1)) {
                throw std::invalid_argument("bmmc_PA_LU_decomp : the input matrix is not invertible"); 
            }
            if (i < j && bmmc_get(L2, i, j) != 0) {
                throw std::invalid_argument("bmmc_PA_LU_decomp : the input matrix is not invertible"); 
            }
            if (i > j && bmmc_get(U2, i, j) != 0) {
                throw std::invalid_argument("bmmc_PA_LU_decomp : the input matrix is not invertible"); 
            }
        } 
    }

    return { p2, L2, U2 };
}

// Decompose a matrix A as A = U*L*P
// where P is a permutation matrix
//       L is a lower triangular matrix
//       U is an upper triangular matrix
std::tuple<BMMC, BMMC, permutation> bmmc_A_ULP_decomp(const BMMC& A)
{
    // Reverse the rows and columns of a BMMC.
    auto reverse = [](const BMMC& X) -> BMMC {
        BMMC Y = bmmc_zero(X.size());
        for (int i = 0; i < X.size(); i++) {
            for (int j = 0; j < X.size(); j++) {
                int bit = bmmc_get(X, i, j);
                bmmc_set(Y, X.size()-1-i, X.size()-1-j, bit);
            }
        }
        return Y;
    };

    // Same as reverse, but for a permutation.
    auto reverse_perm = [](const permutation& perm) -> permutation {
        permutation res = identity_perm(perm.size());
        for (int i = 0; i < perm.size(); i++) {
            res[perm.size()-1-i] = perm.size()-1-perm[i];
        }
        return res;
    };

    // p1 * R*A^T*R = L1 * U1
    auto [p1, L1, U1] = bmmc_PA_LU_decomp(reverse(bmmc_transpose(A)));
    // A^T = R*p1^-1*R * R*L1*R * R*U1*R
    //     = p2 * L2 * U2
    permutation p2 = reverse_perm(inverse_perm(p1));
    BMMC L2 = reverse(L1);
    BMMC U2 = reverse(U1);
    // A = U2^T * L2^T * p2^T
    //   = U3 * L3 * p3
    // For a permutation p, p^T = p^-1
    permutation p3 = inverse_perm(p2);
    BMMC L3 = bmmc_transpose(L2);
    BMMC U3 = bmmc_transpose(U2);
    return { U3, L3, p3 };
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

// This does not return every invertible bmmc : it is based on some 
// crude heuristics.
BMMC random_invertible_bmmc(int size)
{
    int random_bits = rand() % (2 * size);
    while (random_bits > 0) {
        BMMC bmmc = perm_to_bmmc(random_perm(size));
        for (int i = 0; i < random_bits; i++) { 
            bmmc_set(bmmc, rand() % size, rand() % size, 1); 
        }
        // Test if the matrix is invertible
        try {
            auto [U, L, p] = bmmc_A_ULP_decomp(bmmc);
            assert(bmmc == bmmc_mult_bmmc(bmmc_mult_bmmc(U, L), perm_to_bmmc(p)));
            return bmmc;
        }
        // The matrix was not invertible : try again with less random bits
        catch (...) { 
            random_bits--;
            continue; 
        }
    }
    return perm_to_bmmc(random_perm(size));
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