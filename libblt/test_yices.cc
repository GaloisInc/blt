//
// Test module for yices_wrapper.cc
//

#include <iostream>

#define BOOST_TEST_MODULE yices_wrapper
#include "boost/test/included/unit_test.hpp"
#include "boost/numeric/ublas/assignment.hpp"
#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/vector.hpp"
#include "boost/numeric/ublas/io.hpp"

#include "types.h"
#include "codes.h"

// module being tested
#include "yices_wrapper.h"
#include "yices_test_fixture.h"

using std::cout;
using std::endl;
using boost::numeric::ublas::matrix;
using boost::numeric::ublas::vector;


BOOST_GLOBAL_FIXTURE(CmdLine);


// Testing prune/no-prune decision on a very simple 2d example.
//
// Note: 'yprune' returns BLT_CHECK_SAT when it can prove that there is
// a point on the layer whose L_inf distance from 'p' is strictly larger than
// 'b'.
//
// See Ben's Galois Notebook, Vol. 3, pg. 43.
BOOST_AUTO_TEST_CASE( test_yprune_2d ) {
    if (!test_yices) {
        BOOST_TEST_MESSAGE("Yices not enabled, skipping test_yprune_2d");
        return;
    }

    matrix<int64> B(2, 2);   // Lattice basis
    B <<= 8, 4, 4, 8;

    vector<rat64> p (2);     // center point vector
    p <<= 4, 4;

    vector<int64> u(2);      // layer index vector
    u <<= 0, 1;

    size_t k = 1;            // 1d layer
    rat64 b;                 // L_inf distance
    int rc;

    int dec;

    // is the closest (real) point on the layer farther than 2 units from p?
    // (YES)
    b = 2;
    rc = yprune(B, u, k, p, b, false, nullptr, dec);
    BOOST_CHECK(rc == LP_OK && dec == BLT_CHECK_UNSAT);

    // is the closest (real) point on the layer farther than 3 units from p?
    // (NO, just barely)
    b = 3;
    rc = yprune(B, u, k, p, b, false, nullptr, dec);
    BOOST_CHECK(rc == LP_OK && dec == BLT_CHECK_SAT);

    // is the closest (real) point on the layer farther than 1000 units from p?
    // (NO)
    b = 1000;
    rc = yprune(B, u, k, p, b, false, nullptr, dec);
    BOOST_CHECK(rc == LP_OK && dec == BLT_CHECK_SAT);

    // loop over a consequtive range of layers and check that only two of them
    // should be explored (not pruned)
    b = 4;
    for (u(1) = -10; u(1) <= 10; ++u(1)) {  // range of u(1) values to try
        rc = yprune(B, u, k, p, b, false, nullptr, dec);
        if (u(1) == 0 || u(1) == 1)
            BOOST_CHECK(rc == LP_OK && dec == BLT_CHECK_SAT);
        else
            BOOST_CHECK(rc == LP_OK && dec == BLT_CHECK_UNSAT);
    }
}


// Testing prune/no-prune decision on a simple 3d example.
//
// Note: 'yprune' returns BLT_CHECK_SAT when it can prove that there is
// a point on the layer whose L_inf distance from 'p' is strictly larger than
// 'b'.
//
// See Ben's Galois Notebook, Vol. 3, pg. 46.
BOOST_AUTO_TEST_CASE( test_yprune_3d ) {
    if (!test_yices) {
        BOOST_TEST_MESSAGE("Yices not enabled, skipping test_yprune_2d");
        return;
    }

    matrix<int64> B(3, 3);
    vector<int64> p(3), u(3);

    B <<= 1, 0, 3, 0, 2, 1, 4, 4, -4;  // lattice basis
    p <<= 0, 0, 0;                     // center
    u <<= 0, 0, 0;                     // layer index

    size_t k = 2;                      // layer dim
    rat64 b = 4;                      // constraint radius

    int dec;

    // loop over a consequtive range of layers and check that only three of them
    // should be explored (not pruned)
    for (u(2) = -10; u(2) <= 10; ++u(2)) {
        int rc = yprune(B, u, k, p, b, false, nullptr, dec);
        if (-1 <= u(2) && u(2) <= 1)
            BOOST_CHECK(rc == LP_OK && dec == BLT_CHECK_SAT);
        else
            BOOST_CHECK(rc == LP_OK && dec == BLT_CHECK_UNSAT);
    }
}


// TODO add tests that get a model and verify it
