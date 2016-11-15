//
// Test module for glpk_wrapper.cpp
//
#include <cstdlib>
#include <cstdio>
#include <cmath>

#define BOOST_TEST_MODULE glpk_wrapper
#include "boost/test/included/unit_test.hpp"
#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/vector.hpp"
#include "boost/numeric/ublas/io.hpp"

#include "types.h"
#include "codes.h"
#include "util.h"

// module being tested
#include "glpk_wrapper.h"

using boost::numeric::ublas::matrix;
using boost::numeric::ublas::vector;

static const int mem_size = (2+1)*2*2 + 1;  // n = m = 2

// "Tilted example" from the python tests
//
// >>> L = [[2.0, 1.0], [1.0, 2.0]]
// >>> p = [1.0, 1.0]
// >>> u = [0.0, 1.0]
// >>> k = 1
// >>> abs(dlayer_lp(L, u, k, p) - 0.6666666635162583) < 1e-14
// True
//
BOOST_AUTO_TEST_CASE( test_glpk_tilted ) {
    matrix<int64> B(2, 2);
    B(0,0)=2;  B(0, 1)=1;
    B(1,0)=1;  B(1, 1)=2;

    vector<int64> p (2);            // <1, 1> vector
    p(0) = 1; p(1) = 1;

    vector<int64> u(2);             // <0, 1> vector
    u(0) = 0; u(1) = 1;

    vector<double> x(2);
    uint64 k = 1;
    int rc = 0;

    SM* mem = newSM(mem_size);
    double d = infdist(B, u, k, p, mem, x, rc);
    freeSM(mem);
    printf("rc %d LP_OK %d d %f approxEq %d\n", rc, LP_OK, d, approximatelyEqual(d, 0.66667, DIST_EPS));
    BOOST_CHECK(rc == LP_OK && approximatelyEqual(d, 0.66667, DIST_EPS));
}

// "Horizontal example"
// >>> L = [[3.0, 0.0], [0.0, 3.0]]
// >>> p = [1.0, 0.0]
// >>> u = [0.0, 1.0]
// >>> k = 1
// >>> (dlayer_lp(L, u, k, p) - 3.0) < EPS
// True
//
// >>> p = [0.0, 0.0]
// >>> (dlayer_lp(L, u, k, p) - 3.0) < EPS
// True
//
// >>> p = [0.0, 3.0]
// >>> (dlayer_lp(L, u, k, p) - 0.0) < EPS
// True
BOOST_AUTO_TEST_CASE( test_glpk_horiz ) {
    matrix<int64> B(2, 2);
    B(0,0)=3;  B(0, 1)=0;
    B(1,0)=0;  B(1, 1)=3;

    vector<double> p(2);
    p(0) = 1; p(1) = 0;

    vector<int64> u(2);
    u(0) = 0; u(1) = 1;

    vector<double> x(2);
    uint64 k = 1;
    int rc = 0;

    std::cout << "SIZES " << p.size() << u.size() << x.size() << std::endl;

    SM* mem = newSM(mem_size);
    double d = infdist(B, u, k, p, mem, x, rc);
    BOOST_CHECK(rc == LP_OK && approximatelyEqual(d, 3.0, DIST_EPS));

    // change p, doesn't change distance
    p(0) = 0; p(1) = 0;
    d = infdist(B, u, k, p, mem, x, rc);
    BOOST_CHECK(rc == LP_OK && approximatelyEqual(d, 3.0, DIST_EPS));

    // change p again, does change distance
    p(0) = 0; p(1) = 3;
    d = infdist(B, u, k, p, mem, x, rc);
    freeSM(mem);

    BOOST_CHECK(rc == LP_OK && approximatelyEqual(d, 0.0, DIST_EPS));
}

BOOST_AUTO_TEST_CASE(test_glpk_return) {
    matrix<int64> A(2, 2);
    A(0,0) = 3;  A(0,1) = 0;
    A(1,0) = -1; A(1,1) = 2;

    // first let's test matrix coercion
    matrix<double> D = A;
    BOOST_CHECK(D(0,0) == 3.0 && D(0,1) == 0.0 && D(1,0) == -1.0 && D(1,1) == 2);
    A(0,0) = 1;
    BOOST_CHECK(D(0,0) == 3.0); // still 3!

    vector<double> p(2);
    p(0) = 2.5; p(1) = 2.5;
    vector<int64> u(2);  // intentionally blank
    vector<double> x(2);  // intentionally blank
    vector<double> true_x(2);
    true_x(0) = 3.75; true_x(1) = 1.25;
    uint64 k = 2;
    int rc = 0;
    SM* mem = newSM(mem_size);
    double d = infdist(A, u, k, p, mem, x, rc);
    BOOST_CHECK(rc == LP_OK && approximatelyEqual(d, 0.0, DIST_EPS));
    freeSM(mem);

    // test return vector
    BOOST_CHECK(equal(x.begin(), x.end(), true_x.begin(),
      [](double x, double y) { return approximatelyEqual(x, y, DIST_EPS); })); // because we can
}

// test initialization for a square lattice
BOOST_AUTO_TEST_CASE(test_closest_init_square_lattice) {
    size_t N=2, M=2;
    matrix<int64> A(N, M);
    A(0,0) = 3; A(0,1) = 0;
    A(1,0) = 0; A(1,1) = 3;

    vector<int64>  u(N);
    vector<double> p(N); p(0) = 0.0;
    vector<double> x(N);
    int rc = 0;
    SM* mem = newSM(mem_size);

    // positive fraction
    p(1) = 6.25;
    double d = infdist(A, u, N, p, mem, x, rc);
    printf("rc %d LP_OK %d d %f approxEq %d x(1) %d\n", rc, LP_OK, d, approximatelyEqual(d, 0.66667, DIST_EPS), (int)round(x(1)));
    BOOST_CHECK(rc == LP_OK && approximatelyEqual(d, 0.0, DIST_EPS) &&
                (int)round(x(1)) == 2);

    // negative fraction
    p(1) = -3.25;
    d = infdist(A, u, N, p, mem, x, rc);
    printf("rc %d LP_OK %d d %f approxEq %d x(1) %d\n", rc, LP_OK, d, approximatelyEqual(d, 0.66667, DIST_EPS), (int)round(x(1)));
    BOOST_CHECK(rc == LP_OK && approximatelyEqual(d, 0.0, DIST_EPS) &&
                (int)round(x(1)) == -1);

    // in this test, we see negative fractions rounded away from zero in a tie
    p(1) = -4.50;
    d = infdist(A, u, N, p, mem, x, rc);
    printf("rc %d LP_OK %d d %f approxEq %d x(1) %d\n", rc, LP_OK, d, approximatelyEqual(d, 0.66667, DIST_EPS), (int)round(x(1)));
    BOOST_CHECK(rc == LP_OK && approximatelyEqual(d, 0.0, DIST_EPS) &&
                (int)round(x(1)) == -2);
    freeSM(mem);
}
