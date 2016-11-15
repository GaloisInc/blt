//
// Test module for ntl_wrapper.cpp
//
#include <cstdlib>
#include <string>
#include <fstream>
#include <stdexcept>

#define BOOST_TEST_MODULE ntl_wrapper
#include <boost/test/included/unit_test.hpp>
#include <boost/numeric/ublas/matrix.hpp>

#include <NTL/matrix.h>
#include <NTL/ZZ.h>

#include "types.h"

/* module being tested */
#include "ntl_wrapper.h"

using std::ifstream;
using std::string;
using std::runtime_error;

using boost::numeric::ublas::matrix;
using NTL::ZZ;
using NTL::Mat;

static const string LATTICE_95 = "../test/data/lattice_95.tst";

// rank of the 64-dim'l lattice after LLL is still 64
BOOST_AUTO_TEST_CASE( test_rk_LLL_lattice_95 ) {
    Mat<ZZ> N = read_lattice_ntl(LATTICE_95);
    ZZ det2;
    BOOST_CHECK( NTL::LLL(det2, N) == 64 );
}

// rank of half the 64-dim'l lattice is 32
BOOST_AUTO_TEST_CASE( test_rk_LLL_sublattice_95 ) {
    Mat<ZZ> N = read_lattice_ntl(LATTICE_95);
    Mat<ZZ> SN = take_cols(N, 32);
    ZZ det2;
    BOOST_CHECK( NTL::LLL(det2, SN) == 32 );
}

long populate(size_t, size_t);
long populate(size_t i, size_t j) { return (long)(3*i + 2*j + 1); }

// Fill a 3x3 boost matrix, convert to NTL, then test elements
BOOST_AUTO_TEST_CASE( test_matrix_to_ntl ) {
    matrix<int64> M (3, 3);
    for (size_t i=0; i != M.size1(); i++) {
        for (size_t j=0; j != M.size2(); j++) {
            M(i, j) = populate(i, j);
        }
    }
    Mat<ZZ> N;
    matrix_to_ntl(M, N);
    for (size_t i=0; i != (size_t)N.NumRows(); i++) {
        for (size_t j=0; j != (size_t)N.NumCols(); j++) {
            BOOST_CHECK( to_long(N[i][j]) == populate(i, j) );
        }
    }
}
