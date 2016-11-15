//
// Test module for check.cc and associated modules
//

#include <algorithm>
#include <cmath>
#include <iostream>
#include <memory>

#define BOOST_TEST_MODULE closest
#include "boost/numeric/ublas/assignment.hpp"
#include "boost/numeric/ublas/io.hpp"
#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/vector.hpp"
#include "boost/test/included/unit_test.hpp"

#include "codes.h"
#include "types.h"
#include "parser.h"
#include "yices_test_fixture.h"

// module(s) being tested
#include "closest.h"

using namespace std;
namespace ublas = boost::numeric::ublas;


// initialize the 'test_yices' parameter from the command line
BOOST_GLOBAL_FIXTURE(CmdLine);

// test input sanity checking
BOOST_AUTO_TEST_CASE(test_closest_bad_input) {
    ublas::matrix<int64> A(2, 2);
    ublas::matrix<int64> B(1, 2);  // should be 2 x 2
    ublas::vector<int64> sol;
    SolverStats stats;
    BOOST_CHECK(closest(A, B, false, sol, stats) == BLT_STATUS_INPUT_ERROR);
    if (test_yices)
        BOOST_CHECK(closest(A, B, true, sol, stats) == BLT_STATUS_INPUT_ERROR);
    ublas::matrix<int64> C(2, 3);  // should be 2 x 2
    BOOST_CHECK(closest(A, C, false, sol, stats) == BLT_STATUS_INPUT_ERROR);
    if (test_yices)
        BOOST_CHECK(closest(A, C, true, sol, stats) == BLT_STATUS_INPUT_ERROR);
}

// test that closest initializes OK
//
// \   *
//  \_
BOOST_AUTO_TEST_CASE(test_closest_state_init) {
    ublas::matrix<int64> A(2, 2);
    A <<= 1, 0, -1, 2;
    cout << setw(10) << "lattice" << A << endl;
    ublas::matrix<int64> C(2, 2);
    C <<= 2, 3, 2, 3;
    cout << setw(10) << "constr " << C << endl;
    ublas::vector<int64> sol;
    SolverStats stats;
    BOOST_CHECK(closest(A, C, false, sol, stats) == BLT_STATUS_OK);
    if (test_yices)
        BOOST_CHECK(closest(A, C, true, sol, stats) == BLT_STATUS_OK);
}

// Functionality for finding *true* closest vectors has been removed
// as of 2015-10-30.
//
// test some simple return values
// BOOST_AUTO_TEST_CASE(test_closest_2x2) {
//     ublas::matrix<int64> A(2, 2);
//     A(0,0) = 1;  A(0,1) = 0;
//     A(1,0) = -1; A(1,1) = 2;
//     ublas::matrix<int64> C(2, 2);
//     C(0,0) = 2;  C(0,1) = 3;
//     C(1,0) = 2;  C(1,1) = 3;
//     ublas::vector<int64> sol;
//     SolverStats stats;
// 
//     int ret = closest(A, C, false, sol, stats);  // find true closest vector
//     BOOST_CHECK(ret == BLT_STATUS_OK);
// 
//     std::cout << "test_closest_2x2: sol = " << sol << std::endl;
//     int oracle1[2] = {3, 1};
//     int oracle2[2] = {4, 1};
//     BOOST_REQUIRE(sol.size() == 2);
//     BOOST_CHECK(equal(sol.begin(), sol.end(), oracle1) ||
//                 equal(sol.begin(), sol.end(), oracle2));
// }

BOOST_AUTO_TEST_CASE(test_check_soln) {
    ublas::matrix<int64> A(2, 2);
    A(0,0) = 1;  A(0,1) = 0;
    A(1,0) = -1; A(1,1) = 2;
    ublas::matrix<int64> C(2, 2);
    C(0,0) = 2;  C(0,1) = 3;
    C(1,0) = 2;  C(1,1) = 3;
    ublas::vector<int64> u(2);

    // clearly out of bounds
    u(0) = 15; u(1) = -5;
    BOOST_CHECK(!check_soln(A, C, u));

    // just in bounds
    u(0) = 3; u(1) = 1;
    BOOST_CHECK(check_soln(A, C, u));

    // just in bounds
    u(0) = 4; u(1) = 1;
    BOOST_CHECK(check_soln(A, C, u));

    // just outside bounds
    u(0) = 5; u(1) = 1;
    BOOST_CHECK(!check_soln(A, C, u));
}

// TODO add a test of 'read_bilp' here
// test that we can read in matrices from disk
BOOST_AUTO_TEST_CASE(test_read_lattice) {
    ublas::matrix<int64> M;
    int rc = read_lattice("../test/data/test_95.lat", M);
    BOOST_CHECK(rc == 0 && M.size1() == 64 && M.size2() == 64);
    // checksum it
    int64 sum = 0;
    int64 mod = pow(2, 16);
    for (size_t i = 0; i < M.size1(); ++i) {
        for (size_t j = 0; j < M.size2(); ++j) {
            sum = (sum + M(i, j)) % mod;
        }
    }
    cout << "test_95.lat chksum " << sum << std::endl;
    BOOST_CHECK(sum == 61856);  // checked against lattice-ilp implementation

    ublas::matrix<int64> C;
    rc = read_lattice("../test/data/test_95.con", C);
    BOOST_CHECK(rc == 0 && C.size1() == 64 && C.size2() == 2);
}

// Test solving for the closest vector in a 4x4 dct example
BOOST_AUTO_TEST_CASE(test_closest_lattice_dct4x4) {
    ublas::matrix<int64> M, C;
    int rc;
    rc = read_lattice("../test/data/test_4x4.lat", M);
    BOOST_CHECK(rc == 0);
    rc = read_lattice("../test/data/test_4x4.con", C);
    BOOST_CHECK(rc == 0);

    cout << setw(10) << "lattice" << M << endl;
    cout << setw(10) << "constr"  << C << endl;

    ublas::vector<int64> sol(4);
    SolverStats stats;

    rc = closest(M, C, false, sol, stats);  // search until we find the first solution
    BOOST_CHECK(rc == BLT_STATUS_OK);
    cout << setw(10) << "sol" << sol << endl;
    BOOST_CHECK(check_soln(M, C, sol));
    if (test_yices) {  // repeat test using Yices
        rc = closest(M, C, true, sol, stats);
        BOOST_CHECK(rc == BLT_STATUS_OK);
        cout << setw(10) << "[yices] sol" << sol << endl;
        BOOST_CHECK(check_soln(M, C, sol));
    }

    // TODO replace with a solution iterator test
    // rc = closest(M, C, false, sol, stats);  // search until we find THE closest vector
    // BOOST_CHECK(rc == BLT_STATUS_OK);
    // std::cout << "test_closest_lattice_dct4x4: sol = " << sol << std::endl;
    // BOOST_CHECK(check_soln(M, C, sol));
}
