//
// Test module for expr.cc
//

#include <algorithm>
#include <cmath>
#include <iostream>
#include <memory>

#define BOOST_TEST_MODULE expr
#include "boost/test/included/unit_test.hpp"
#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/vector.hpp"
#include "boost/numeric/ublas/io.hpp"

#include "codes.h"
#include "types.h"
#include "util.h"

// module(s) being tested
#include "context.h"
#include "expr.h"

using std::cout;
using std::endl;
using std::equal;
using std::shared_ptr;
using boost::numeric::ublas::matrix;
using boost::numeric::ublas::vector;
using boost::rational;

// test Expr ctr
BOOST_AUTO_TEST_CASE(test_expr_ctr) {
    Expr e;
    Expr e1 = ExprVar(1), e2 = ExprVar(2);

    BOOST_CHECK(e.nsummands() == 0);
    BOOST_CHECK(e1.nsummands() == 1);

    e = 3*e1 - e2;
    BOOST_CHECK(e.nsummands() == 2);

    Expr c = ExprConst(rational<int64>(1,3));
    e += c;
    BOOST_CHECK(e.nsummands() == 3);
    BOOST_CHECK(e.get_const() == rational<int64>(1,3));
}

BOOST_AUTO_TEST_CASE(test_expr_round) {
    Expr x(1), y(2);
    Expr c = ExprConst(rational<int64>(2,7));
    Expr e = 4*x + y + c;

    e *= rational<int64>(2,7);
    e = round(e);
    BOOST_CHECK(e[1] == 1);
    cout << "e[2] = " << e[2] << endl;
    BOOST_CHECK(e[2] == 0);
    BOOST_CHECK(e.get_const() == 0);
}

// test construction and arithmetic with expressions
BOOST_AUTO_TEST_CASE(test_expr) {
    Context ctx;
    Expr x = ctx.mkVar("x");
    Expr y = ctx.mkVar("y");
    int i, j;

    (void) ctx.getVarIdx("x", i);
    std::set<int> idxx{i};
    BOOST_CHECK(equal(idxx.begin(), idxx.end(), x.indices().begin()));

    (void) ctx.getVarIdx("y", i);
    std::set<int> idxy{i};
    BOOST_CHECK(equal(idxy.begin(), idxy.end(), y.indices().begin()));

    Expr s = x + y;
    (void) ctx.getVarIdx("x", i);
    (void) ctx.getVarIdx("y", j);
    std::set<int> idxs{i, j};
    BOOST_CHECK(equal(idxs.begin(), idxs.end(), s.indices().begin()));

    s += x;
    // indices shouldn't change
    (void) ctx.getVarIdx("x", i);
    (void) ctx.getVarIdx("y", j);
    BOOST_CHECK(equal(idxs.begin(), idxs.end(), s.indices().begin()));
    BOOST_CHECK(s[i] == 2 && s[j] == 1);
    s -= y;
    BOOST_CHECK(s[i] == 2 && s[j] == 0);
    s += -3*x + 5*y;
    BOOST_CHECK(s[i] == -1 && s[j] == 5);

    Expr v1 = x;
    Expr v2 = y;
    Expr w  = v1*13 - v2*11;
    BOOST_CHECK(w[0] == 13 && w[1] == -11);
}
