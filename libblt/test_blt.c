#include <stdio.h>

#include "blt.h"
#include "codes.h"

#include "test_blt.h"

/* Test driver */
int main(int argc, char* argv[]) {
    int rc;

    rc = test_2d_problem();
    if (rc) return rc;

#ifdef ENVIRONMENT_64
    rc = test_tight_parallelogram();
    if (rc) return rc;
#endif

    rc = test_tight_parallelogram_simple();
    if (rc) return rc;

    printf("Tests pass!\n");
    return 0;
}

/* A simple 2d ILP problem:
 *
 *     2 <= x - y <= 3
 *     2 <=   2*y <= 3
*/
int test_2d_problem() {
    CContext ctx = blt_init(0);
    CExpr x = blt_var(ctx, "x");
    CExpr y = blt_var(ctx, "y");
    CExpr z = blt_add(ctx, x, blt_smul(ctx, -1, 1, y));
    blt_assume(ctx, 2, 1, z, 3, 1);
    CExpr w = blt_smul(ctx, 2, 1, y);
    blt_assume(ctx, 2, 1, w, 3, 1);

    /* check satisfiability of the problem and print return code */
    int rc = blt_check(ctx);
    printf("check: %d\n", rc);

    /* extract and print the satisfying model */
    int64 x0, y0;
    if (BLT_STATUS_OK != blt_model(ctx, "x", &x0)) {
        printf("ERROR getting model for x\n");
        return 1;
    }
    if (BLT_STATUS_OK != blt_model(ctx, "y", &y0)) {
        printf("ERROR getting model for y\n");
        return 1;
    }
    printf("model: x = %d, y = %d\n", (int)x0, (int)y0);

    /* free memory allocated by the library */
    blt_free(ctx);
    printf("Done!\n");
    return 0;
}

/*
   A test problem that is UNSAT from the Yices 2 examples. This problem
   causes overflow problems leading to spurious solutions on some 32-bit
   platforms.

   Tight parallelogram with no integer solution
   by Jochen Hoenicke

   :status unsat
   :difficulty { 0 }
   :logic QF_LIA
   :extrafuns ((x Int))
   :extrafuns ((y Int))
   :formula
   (and (<= 0 (- (* 4283000000 x) (* 3245000001 y)))
        (<= (- (* 4283000000 x) (* 3245000001 y)) 999999)
        (<= 1 (- (* 4283000001 x) (* 3245000000 y)))
        (<= (- (* 4283000001 x) (* 3245000000 y)) 1000000))
*/
int test_tight_parallelogram() {
    CContext ctx = blt_init(0);
    CExpr x = blt_var(ctx, "x");
    CExpr y = blt_var(ctx, "y");

    CExpr e1 = blt_add(ctx, blt_smul(ctx, 4283000000, 1, x),
                            blt_smul(ctx, -3245000001, 1, y));
    blt_assume(ctx, 0, 1, e1, 999999, 1);

    CExpr e2 = blt_add(ctx, blt_smul(ctx, 4283000001, 1, x),
                            blt_smul(ctx, -3245000000, 1, y));
    blt_assume(ctx, 1, 1, e2, 1000000, 1);

    /* check satisfiability of the problem and print return code */
    int rc = blt_check(ctx);

    if (rc != BLT_CHECK_UNSAT) {
        printf("test error: expected BLT_CHECK_UNSAT, got rc %d\n", rc);
        return 1;
    }

    /* free memory allocated by the library */
    blt_free(ctx);
    return 0;
}

/*
   Another UNSAT tight parallelogram example from Yices' test suite. This
   one features smaller coefficients.

   Tight parallelogram with no integer solution
   by Jochen Hoenicke

   :status unsat
   :difficulty { 0 }
   :logic QF_LIA
   :extrafuns ((x Int))
   :extrafuns ((y Int))
   :formula
   (and (<= 0 (- (* 428300 x) (* 324501 y)))
        (<= (- (* 428300 x) (* 324501 y)) 99)
        (<= 1 (- (* 428301 x) (* 324500 y)))
        (<= (- (* 428301 x) (* 324500 y)) 100))
*/
int test_tight_parallelogram_simple() {
    CContext ctx = blt_init(0);
    CExpr x = blt_var(ctx, "x");
    CExpr y = blt_var(ctx, "y");

    CExpr e1 = blt_add(ctx, blt_smul(ctx, 428300, 1, x),
                            blt_smul(ctx, -324501, 1, y));
    blt_assume(ctx, 0, 1, e1, 99, 1);

    CExpr e2 = blt_add(ctx, blt_smul(ctx, 428301, 1, x),
                            blt_smul(ctx, -324500, 1, y));
    blt_assume(ctx, 1, 1, e2, 100, 1);

    /* check satisfiability of the problem and print return code */
    int rc = blt_check(ctx);

    if (rc != BLT_CHECK_UNSAT) {
        printf("test_tight_parallelogram_simple: expected BLT_CHECK_UNSAT, got rc %d\n", rc);
        return 1;
    }

    /* free memory allocated by the library */
    blt_free(ctx);
    return 0;
}

