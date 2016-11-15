//
// Test module for context.cc and associated modules
//

#include <algorithm>
#include <iostream>
#include <set>
#include <memory>

#define BOOST_TEST_MODULE context
#include "boost/test/included/unit_test.hpp"

#include "codes.h"
#include "types.h"
#include "util.h"
#include "yices_test_fixture.h"

// module(s) being tested
#include "context.h"

using std::equal;
using std::string;
using boost::rational;


BOOST_GLOBAL_FIXTURE(CmdLine);


// test Context ctr
BOOST_AUTO_TEST_CASE(test_context_ctr) {
    Context ctx;
    //auto x = ctx.mkVar("x");
    BOOST_CHECK(ctx.nvars() == 0);
}

// test Atom ctr
BOOST_AUTO_TEST_CASE(test_atom_ctr) {
    Expr e(0);
    Atom a(-10, e, 10);
    BOOST_CHECK(a.lower() == -10 && a.upper() == 10 && a.expr().nsummands() == 1);
}

// test mkVar
BOOST_AUTO_TEST_CASE(test_mkVar) {
    Context ctx;
    BOOST_CHECK(ctx.nvars() == 0);

    Expr x = ctx.mkVar("x");
    string name;
    BOOST_CHECK(x.nsummands() == 1);
    BOOST_CHECK(ctx.nvars() == 0 && ctx.getVarName(0, name) == 0 && name == "x");

    Expr y = ctx.mkVar("y");
    int idx;
    BOOST_CHECK(ctx.nvars() == 0);
    BOOST_CHECK(ctx.getVarIdx("x", idx) == 0 && idx == 0);
    BOOST_CHECK(ctx.getVarIdx("y", idx) == 0 && idx == 1);
    BOOST_CHECK(ctx.getVarName(1, name) == 0 && name == "y");

    // check out of range indices
    BOOST_CHECK(ctx.getVarName(3, name) != 0);
    BOOST_CHECK(ctx.getVarName(-1, name) != 0);
    BOOST_CHECK(ctx.getVarIdx("foo", idx) != 0);
}

// test saving and backtracking
BOOST_AUTO_TEST_CASE(test_save_backtrack) {
    Context ctx;
    Expr x = ctx.mkVar("x");
    Expr y = ctx.mkVar("y");
    BOOST_CHECK(!ctx.varActive("x") && !ctx.varActive("y")); // x,y not active
    BOOST_CHECK(ctx.nvars() == 0);  // no active variables yet

    Expr s = x + y;
    // assume 1
    ctx.assume(0, s, 1);
    BOOST_CHECK(ctx.varActive("x") && ctx.varActive("y")); // x,y active
    BOOST_CHECK(ctx.nvars() == 2);
    SolvState save1 = ctx.save_state();

    Expr z = ctx.mkVar("z");
    // assume 2
    ctx.assume(3, z, 5);
    BOOST_CHECK(ctx.varActive("z")); // z active
    BOOST_CHECK(ctx.nassumpts() == 2);
    BOOST_CHECK(ctx.nvars() == 3);
    SolvState save2 = ctx.save_state();

    // backtrack to 1
    ctx.backtrack(save1);

    BOOST_CHECK(!ctx.varActive("z")); // z not active
    BOOST_CHECK(ctx.nvars() == 2);
    BOOST_CHECK(ctx.nassumpts() == 1);
    BOOST_CHECK(ctx.backtrack(save2) == BLT_STATUS_ERROR); // no backtracking forward

    // assumes 4 & 5
    ctx.assume(-1, 3*x + 4*z, 1);
    ctx.assume(-2048, -1*y + 1001*z, 2048);
    BOOST_CHECK(ctx.nvars() == 3);
    BOOST_CHECK(ctx.nassumpts() == 3);
    // x, y, and z active
    BOOST_CHECK(ctx.varActive("x") && ctx.varActive("y") && ctx.varActive("z"));
}

// test checking SAT
BOOST_AUTO_TEST_CASE(test_check_sat_2d) {
    // 2d SAT example
    Context ctx(test_yices == 1 ? true : false);
    Expr x = ctx.mkVar("x");
    Expr y = ctx.mkVar("y");
    ctx.assume(0, x, 3);
    ctx.assume(3, y, 5);  // diagonal matrix
    BOOST_CHECK(ctx.check() == BLT_CHECK_SAT);
    // manually check the solution
    int64 x0, y0;
    BOOST_REQUIRE(BLT_STATUS_OK == ctx.get_model("x", &x0) &&
                  BLT_STATUS_OK == ctx.get_model("y", &y0));
    BOOST_CHECK(0 <= x0 && x0 <= 3);
    BOOST_CHECK(3 <= y0 && y0 <= 5);
}

BOOST_AUTO_TEST_CASE(test_check_sat_3d) {
    // 3d SAT example
    Context ctx(test_yices == 1 ? true : false);
    Expr u = ctx.mkVar("u");
    Expr v = ctx.mkVar("v");
    Expr z = ctx.mkVar("z");
    ctx.assume(-1, u + z, 1);
    ctx.assume(-3, v - z, 5);  // non-square matrix [[1 0 1][0 1 -1]]
    BOOST_CHECK(ctx.check() == BLT_CHECK_SAT);
    // add one more row
    ctx.assume(-1, u + 13*v + 101*z, 2);
    BOOST_CHECK(ctx.check() == BLT_CHECK_SAT);
    // manually check the solution
    int64 u0, v0, z0;
    BOOST_REQUIRE(BLT_STATUS_OK == ctx.get_model("u", &u0) &&
                  BLT_STATUS_OK == ctx.get_model("v", &v0) &&
                  BLT_STATUS_OK == ctx.get_model("z", &z0));
    BOOST_CHECK(-1 <= u0 + z0 && u0 + z0 <= 1);
    BOOST_CHECK(-3 <= v0 - z0 && v0 - z0 <= 5);
    BOOST_CHECK(-1 <= u0 + 13*v0 + 101*z0 && u0 + 13*v0 + 101*z0 <= 2);
}

BOOST_AUTO_TEST_CASE(test_check_unsat) {
    // a toy UNSAT example
    Context ctx(test_yices == 1 ? true : false);
    Expr a = ctx.mkVar("a");
    Expr b = ctx.mkVar("b");
    ctx.assume(1, 3*a, 2);
    ctx.assume(3, 5*b, 4);
    BOOST_CHECK(ctx.check() == BLT_CHECK_UNSAT);
}

// check SAT of a rational system
BOOST_AUTO_TEST_CASE(test_check_sat_rat) {
    rat64 one_two(1, 2);
    rat64 one_three(1, 3);
    rat64 one_five(1, 5);
    Context ctx(test_yices == 1 ? true : false);
    Expr u = ctx.mkVar("u");
    Expr v = ctx.mkVar("v");
    Expr z = ctx.mkVar("z");
    // scale each row by a different fraction
    ctx.assume(-one_two, one_two*(u + z), one_two);
    ctx.assume(-3*one_three, one_three*(v - z), one_three*5);
    ctx.assume(-1*one_five, one_five*(u + 13*v + 101*z), one_five*2);
    BOOST_CHECK(ctx.check() == BLT_CHECK_SAT);
    // manually check the solution
    int64 u0, v0, z0;
    BOOST_REQUIRE(BLT_STATUS_OK == ctx.get_model("u", &u0) &&
                  BLT_STATUS_OK == ctx.get_model("v", &v0) &&
                  BLT_STATUS_OK == ctx.get_model("z", &z0));
    // same conditions as integral system in `test_check_sat`
    BOOST_CHECK(-1 <= u0 + z0 && u0 + z0 <= 1);
    BOOST_CHECK(-3 <= v0 - z0 && v0 - z0 <= 5);
    BOOST_CHECK(-1 <= u0 + 13*v0 + 101*z0 && u0 + 13*v0 + 101*z0 <= 2);
}


// Test solving overconstrained problems: pentagon in a 2d integer lattice
//
//    0 <=       y  <= 2
// -inf <=   x      <= 3
// -inf <=   x - y  <= 2
// -inf <= -2x + y  <= 0
BOOST_AUTO_TEST_CASE(test_check_2ip) {
    Context ctx(test_yices == 1 ? true : false);
    Expr x = ctx.mkVar("x");
    Expr y = ctx.mkVar("y");

    const int64 LB = -5; // safe, arbitrary lower bound
    ctx.assume(0,         y , 2);
    ctx.assume(LB,    x     , 3);
    ctx.assume(LB,    x - y , 2);
    ctx.assume(LB, -2*x + y , 0);

    printf("test_check_2ip: solving...\n");
    int rc = ctx.check();
    BOOST_CHECK(rc == BLT_CHECK_SAT);

    int64 x0, y0;
    BOOST_REQUIRE(ctx.get_model("x", &x0) == BLT_STATUS_OK);
    BOOST_REQUIRE(ctx.get_model("y", &y0) == BLT_STATUS_OK);
    printf("test_check_2ip: solution = (%ld, %ld)\n", (long) x0, (long) y0);

    // There are 9 solutions to this system
}

// Test solving overconstrained problems: pentagon in a pentagon in a 2d
// integer lattice. This is a math olympiad problem.
//
// 0  <= 3x - 4y
//        x - 4y <= 0
//      -2x -  y <= -4
// -5 <= -x - 2y
//       2x -  y <= 4
//
BOOST_AUTO_TEST_CASE(test_check_2ip_hard) {
    Context ctx(test_yices == 1 ? true : false);
    Expr x = ctx.mkVar("x");
    Expr y = ctx.mkVar("y");

    const int64 BND = 20; // safe, arbitrary bound

    ctx.assume(0   , 3*x-4*y    , BND);
    ctx.assume(-BND, x - 4*y    , 0);
    ctx.assume(-BND, -2*x - y   , -4);
    ctx.assume(-5  , -1*x - 2*y , BND);
    ctx.assume(-BND, 2*x - y    , 4);

    printf("test_check_2ip_hard: solving...\n");
    int rc = ctx.check();
    BOOST_CHECK(rc == BLT_CHECK_SAT);

    int64 x0, y0;
    BOOST_REQUIRE(ctx.get_model("x", &x0) == BLT_STATUS_OK);
    BOOST_REQUIRE(ctx.get_model("y", &y0) == BLT_STATUS_OK);
    printf("test_check_2ip: solution = (%ld, %ld)\n", (long) x0, (long) y0);

    BOOST_CHECK(x0 == 2 && y0 == 1);  // the unique solution
}

BOOST_AUTO_TEST_CASE(test_check_affine) {
    const rational<int64> CONST(3, 1);
    Context ctx(test_yices == 1 ? true : false);
    Expr x = ctx.mkVar("x");
    Expr y = ctx.mkVar("y");
    Expr c = ctx.mkConst(CONST);
    // system with two different constants
    ctx.assume(1, x + c, 2);
    ctx.assume(100, x + y + 2*c, 125);
    int rc = ctx.check();
    BOOST_CHECK(rc == BLT_CHECK_SAT);
    // validate the solution
    int64 x0, y0;
    BOOST_REQUIRE(ctx.get_model("x", &x0) == BLT_STATUS_OK
               && ctx.get_model("y", &y0) == BLT_STATUS_OK);
    BOOST_CHECK(1 <= x0 + CONST && x0 + CONST <= 2);
    BOOST_CHECK(100 <= x0 + y0 + 2*CONST && x0 + y0 + 2*CONST <= 125);
}
