/// NTL Wrappers
///
/// The functions in this module wrap a select few functions for lattice
/// reduction in NTL. They take matrices with 'int64_t' elements, but use
/// unlimited precision internally.

#include <climits>
#include <string>
#include <fstream>
#include <stdexcept>

#include <boost/numeric/ublas/matrix.hpp>
#include "NTL/matrix.h"
#include "NTL/ZZ.h"
#include "NTL/LLL.h"

#include "debug.h"
#include "types.h"

#include "ntl_wrapper.h"

using std::string;
using std::ifstream;
using std::runtime_error;
using boost::numeric::ublas::matrix;
using NTL::Mat;
using NTL::Vec;
using NTL::ZZ;


/////////////////////////////////////////////////////////////////////
/// LLL reduction

/// LLL reduction using exact arithmetic
///
/// This is the default used in BLT.
int64 LLL(matrix<int64>& B, matrix<int64>& U) {
    Mat<ZZ> ntlB;
    Mat<ZZ> ntlU;
    ZZ det2;

    matrix_to_ntl(B, ntlB);
    TRACE("lattice reduction method: LLL");
    int64 rank = NTL::LLL(det2, ntlB, ntlU);

    ZZ det = SqrRoot(det2);
    TRACE("reduced lattice det: " << det);

    ntl_to_matrix(ntlB, B);
    ntl_to_matrix(ntlU, U);
    return rank;
}

/// LLL_FP wrapper
///
/// This method is generally faster than 'LLL', but uses floating point
/// arithmetic.
int64 LLL_FP(matrix<int64>& B, matrix<int64>& U) {
    Mat<ZZ> ntlB;
    Mat<ZZ> ntlU;

    matrix_to_ntl(B, ntlB);
    TRACE("lattice reduction method: LLL_FP");
    int64 rank = NTL::LLL_FP(ntlB, ntlU);

    ZZ det;
    determinant(det, ntlB);
    TRACE("reduced lattice det: " << det);

    ntl_to_matrix(ntlB, B);
    ntl_to_matrix(ntlU, U);
    return rank;
}

/// G_LLL_FP wrapper
///
/// This method is more stable than 'LLL_FP', but may take longer.
int64 G_LLL_FP(matrix<int64>& B, matrix<int64>& U) {
    Mat<ZZ> ntlB;
    Mat<ZZ> ntlU;

    matrix_to_ntl(B, ntlB);
    TRACE("lattice reduction method: G_LLL_FP");
    int64 rank = NTL::G_LLL_FP(ntlB, ntlU);

    ZZ det;
    determinant(det, ntlB);
    TRACE("reduced lattice det: " << det);

    ntl_to_matrix(ntlB, B);
    ntl_to_matrix(ntlU, U);
    return rank;
}

/////////////////////////////////////////////////////////////////////
/// Utilities

/// Estimate the number of solutions of a given lattice constraint pair.
///
/// If 'inv == true' then return value is ~1/expected_solns.
long expected_solns(const matrix<int64>& L, const matrix<int64>& C, bool& inv) {
    Mat<ZZ> ntlL;
    ZZ det;
    matrix_to_ntl(L, ntlL);

    // det computation must be done using unlimited precision
    determinant(det, ntlL);
    det = abs(det);

    // volume computation must be done using unlimited precision
    int64 r, maxr = 0;
    ZZ vol(1L);
    for (size_t i = 0; i < C.size1(); ++i) {
        r = C(i,1) - C(i,0);
        if (r <= 0)
            TRACE("i=" << i << " C(i,0)=" << C(i,0) << " C(i,1)=" << C(i,1));
        assert(r > 0 && "a constraint is inconsistent");
        vol *= r;
        if (r > maxr)
            maxr = r;
    }
    TRACE("orthotope radius: " << (double)maxr/2.0);

    ZZ q;
    if (det <= vol) {
        inv = false;
        q = vol / det;
    } else {
        inv = true;
        q = det / vol;
    }
    TRACE("NTL expected solutions = " << (inv ? "1 / " : "") << q);
    if (q >= LONG_MAX)
        return LONG_MAX;
    return to_long(q);
}

/////////////////////////////////////////////////////////////////////
// Conversion

// copy a matrix m into a mat_ZZ `n`; `n` is resized.
void matrix_to_ntl(const matrix<int64>& m, Mat<ZZ>& n) {
    size_t r = m.size1();  // num rows
    size_t c = m.size2();  // num columns
    if (r == 0 || c == 0) {
        n.SetDims(0,0);
        return;
    }
    n.SetDims(r, c);
    for (size_t i=0; (int)i != n.NumRows(); i++) {
        for (size_t j=0; (int)j != n.NumCols(); j++) {
            n[i][j] = m(i, j);  // int64 -> ZZ coercion
        }
    }
}

// vice-versa: copy a Mat<ZZ> to a matrix<int64> with possible
// loss of precision
void ntl_to_matrix(const Mat<ZZ>& N, matrix<int64>& M) {
    size_t r = N.NumRows();
    size_t c = N.NumCols();
    M.resize(r, c);
    for (size_t i=0; i != M.size1(); i++) {
        for (size_t j=0; j != M.size2(); j++) {
            M(i, j) = to_long(N[i][j]);  // ZZ -> int64 coercion
        }
    }
}


/////////////////////////////////////////////////////////////////////
// Misc


// Return a view of n consisting of the first c columns
Mat<ZZ> take_cols(const Mat<ZZ>& n, size_t c) {
    if ((int) c > n.NumCols())
        return n;
    size_t r = n.NumRows();
    Mat<ZZ> ret;
    ret.SetDims(r, c);
    for (size_t i=0; i != r; i++) {
        Vec<ZZ> t;
        t.SetLength(c);
        for (size_t j=0; j != c; j++) {
            t[j] = n[i][j];
        }
        ret[i] = t;
    }
    return ret;
}

// Read a lattice from file into a Mat<ZZ> using NTLs >> operators
Mat<ZZ> read_lattice_ntl(string infile) {
    ifstream in(infile.c_str());
    if (!in)
        throw runtime_error("could not open input file: " + infile);
    Mat<ZZ> N;
    in >> N;
    if (!in)
        throw runtime_error("could not read matrix from input file: " + infile);
    return N;
}
