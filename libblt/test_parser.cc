//
// Test module for parser.cc
//

#include <iostream>
#include <sstream>

#define BOOST_TEST_MODULE util
#include "boost/test/included/unit_test.hpp"
#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/io.hpp"

#include "parser.h"
#include "types.h"

using std::cout;
using std::endl;
using boost::numeric::ublas::matrix;

// test vectors
static const char* bilp1 = \
    " 1 <= 2 <= 5";
static const char* bilp2 = \
    " 1 <= 2  3 4  <=   5\n"
    "-7 <= 8 -9 10 <= -11";
static const char* bilp3 = \
    " 1 <= 2  3 4  <=   5\n"
    "-7 <= 8 -9 10 <= -11\n"
    "0  <= 1  0  0 <=  33";

// test read_bilp
BOOST_AUTO_TEST_CASE(test_read_bilp) {
    std::istringstream in(bilp1);
    matrix<int64> C,L;
    BOOST_CHECK(read_bilp(in, L, C) == 0);
    cout << "C: " << C << endl;
    cout << "L: " << L << endl;
    BOOST_CHECK(C.size1() == 1 && C.size2() == 2);
    BOOST_CHECK(L.size1() == 1 && L.size2() == 1);
    //
    std::istringstream in2(bilp2);
    BOOST_CHECK(read_bilp(in2, L, C) == 0);
    cout << "C: " << C << endl;
    cout << "L: " << L << endl;
    BOOST_CHECK(C.size1() == 2 && C.size2() == 2);
    BOOST_CHECK(L.size1() == 2 && L.size2() == 3);
    //
    std::istringstream in3(bilp3);
    BOOST_CHECK(read_bilp(in3, L, C) == 0);
    cout << "C: " << C << endl;
    cout << "L: " << L << endl;
    BOOST_CHECK(C.size1() == 3 && C.size2() == 2);
    BOOST_CHECK(L.size1() == 3 && L.size2() == 3);
}
