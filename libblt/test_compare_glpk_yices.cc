//
// Test module for comparing GLPK and Yices at layer pruning
//

#include <iostream>
#include <random>

#define BOOST_TEST_MODULE yices_wrapper
#include "boost/test/included/unit_test.hpp"
#include "boost/numeric/ublas/assignment.hpp"
#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/vector.hpp"
#include "boost/numeric/ublas/io.hpp"

#include "types.h"
#include "codes.h"
#include "util.h"
#include "yices_test_fixture.h"

// modules being tested
#include "glpk_wrapper.h"
#include "yices_wrapper.h"

using std::cout;
using std::endl;
using std::default_random_engine;
using std::uniform_int_distribution;
using boost::numeric::ublas::matrix;
using boost::numeric::ublas::vector;


BOOST_GLOBAL_FIXTURE(CmdLine);


// Number of random 2d tests to perform
static const int NUM_TEST_2D = 1000;


// Testing prune/no-prune decision on random 2d instances
//
// We generate a problem (lattice, layer, point, cube radius) and ask Yices if
// the layer has any real point in the cube. We then ask GLPK to compute the
// L_inf distance of the layer from the center point. We check that there is a
// layer point in the cube iff. the L_inf distance is less or equal to the
// cube radius.
//
BOOST_AUTO_TEST_CASE( test_glpk_v_yices_2d ) {
    if (!test_yices) {
        BOOST_TEST_MESSAGE("Yices not enabled, skipping test_yprune_2d");
        return;
    }

    matrix<int64> B(2, 2);   // Lattice basis
    vector<int64> u(2);      // layer index
    vector<rat64> p(2);     // center point
    vector<double> p_fp(2);     // center point
    int mem_size = (2+1)*2*2 + 1;  // n = m = 2
    SM* mem = newSM(mem_size);

    // for generating test vectors
    default_random_engine gen;
    uniform_int_distribution<int> dist(1, 1000);
    uniform_int_distribution<int> idist(-1000, 1000);

    vector<double> model(2);
    int dec;

    for (size_t i = 0; i < NUM_TEST_2D; ++i) {

        B <<= 2, idist(gen), 0, 5;     // generate an invertible integer matrix
        p <<= idist(gen), idist(gen);  // generate an arbitrary integer point
        p_fp <<= boost::rational_cast<double>(p(0))
               , boost::rational_cast<double>(p(1));
        u <<= 0, dist(gen);            // random layer index
        rat64 b = dist(gen);           // L_inf distance
        double b_fp = boost::rational_cast<double>(b);

        size_t k = 1;                  // 1d layer
        int yrc, grc;

        // is the closest (real) point on the layer closer or farther than 'b' units
        // from p?
        yrc = yprune(B, u, k, p, b, true, &model, dec);

        // compare to GLPK decision
        double d = infdist(B, u, k, p_fp, mem, model, grc);

        BOOST_CHECK(yrc == LP_OK);
        BOOST_CHECK(grc == LP_OK);
        switch (dec) {
            case BLT_CHECK_UNSAT:
                BOOST_CHECK(definitelyLess(b_fp, d, DIST_EPS));
                break;
            case BLT_CHECK_SAT:
                BOOST_CHECK(approximatelyLE(d, b_fp, DIST_EPS));
                break;
            default:
                BOOST_CHECK(false && "error or unknown in yprune");
        }

    }
    freeSM(mem);

}
