/// Regression test module
///
/// Apply a quick and dirty solving method (based on internal functions
/// to BLT) to several test cases.

#include <algorithm>
#include <cmath>
#include <iostream>
#include <memory>

#define BOOST_TEST_MODULE regression
#include "boost/test/included/unit_test.hpp"
#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/vector.hpp"
#include "boost/numeric/ublas/io.hpp"

#include "codes.h"
#include "ntl_wrapper.h"
#include "types.h"
#include "parser.h"
#include "closest.h"
#include "yices_test_fixture.h"

using std::equal;
using std::shared_ptr;
using boost::numeric::ublas::matrix;
using boost::numeric::ublas::vector;

int solve_raw(const string, const string);

/// Quick and dirty closest vector solver
///
/// This function attempts to solve the problem specified by the dirname and
/// name given. Unlike the member functions in 'Context', it does not scale
/// the constraint set to a hypercube. It exists only for testing purposes.
int solve_raw(const string dir, const string name, bool enable_yices) {
    matrix<int64> M, C;
    int rc;
    (void) read_lattice(dir + "/" + name + ".lat", M);
    (void) read_lattice(dir + "/" + name + ".con", C);
    std::cout << "lattice size " << M.size1() << "x" << M.size2() << std::endl;
    std::cout << "constr size "  << C.size1() << "x" << C.size2() << std::endl;

    std::cout << "LLL reducing... ";
    matrix<int64> U(M.size1(), M.size2());  // U is unused
    (void) LLL(M, U);
    std::cout << "done." << std::endl;

    std::cout << "Solving... (may take a few minutes)" << std::endl;
    vector<int64> sol;
    SolverStats stats;
    rc = closest(M, C, enable_yices, sol, stats);
    std::cout << "rc " << rc;
    bool chk = check_soln(M, C, sol);
    std::cout << " chk " << chk << std::endl;

    if (rc == BLT_STATUS_OK)
       return chk ? BLT_CHECK_SAT : BLT_CHECK_UNSAT;
    else
        return rc;
}


BOOST_GLOBAL_FIXTURE(CmdLine);

/// Test basic closest vector solving
///
/// Answer from lattice-ilp (which uses a slightly different descent heuristic) is
///
/// [ 672, -42, -105, 739, 2032, -469, -929, 976, 427, 1164, 257, -187, -2, -146,
///   325, -234, -129, -702, 276, 688, -478, -2, -1052, -297, -594, 0, 0, 728,
///   721, -4, -50, -411, 172, 289, -133, -289, -117, 0, 0, -636, 0, 2, -53,
///   474, -560, 270, 477, 23, -150, 9, -125, 32, -186, 33, 219, 34, -124, -45,
///   32, 149, -124, 46, 0, -1 ]
///
/// To compare, our initial solution is:
///
/// ( 680, -43, -106, 739, 2030, -467, -928, 978, 427, 1164, 256, -187, -1, -147,
///   326, -235, -129, -701, 278, 689, -477, -2, -1053, -296, -593, -2, 1, 729,
///   722, -4, -50, -412, 173, 290, -133, -289, -117, -1, 0, -636, 0, 2, -53, 474,
///  -560, 270, 477, 23, -151, 9, -125, 33, -186, 33, 219, 34, -124, -45, 32, 149,
///  -124, 46, 0, -1)
///
BOOST_AUTO_TEST_CASE(test_closest_lattice_95) {
    BOOST_CHECK(BLT_CHECK_SAT == solve_raw("../test/data", "test_95", false));
    if (test_yices)
        BOOST_CHECK(BLT_CHECK_SAT == solve_raw("../test/data", "test_95", true));
}

/// Solve test_50
/// Test a more difficult large problem because the lattice data LLL
/// reduced
BOOST_AUTO_TEST_CASE(test_closest_lattice_50) {
    BOOST_CHECK(BLT_CHECK_SAT == solve_raw("../test/data", "test_50", false));
    if (test_yices)
        BOOST_CHECK(BLT_CHECK_SAT == solve_raw("../test/data", "test_50", true));
}

/// Solve test_50_8
///
/// Test another more difficult large problem
BOOST_AUTO_TEST_CASE(test_closest_lattice_50_8) {
    BOOST_CHECK(BLT_CHECK_SAT == solve_raw("../test/data", "test_50_8", false));
    if (test_yices)
        BOOST_CHECK(BLT_CHECK_SAT == solve_raw("../test/data", "test_50_8", true));
}
