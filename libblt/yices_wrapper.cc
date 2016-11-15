#include <iostream>
#include <cstdio>
#include <cassert>

#ifdef DEBUG
  #include <ctime>
  #include <iomanip>
#endif

#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/vector.hpp"

#include "types.h"
#include "codes.h"
#include "debug.h"
#include "error.h"
#include "util.h"

#include "yices_wrapper.h"
#include "yices_stub.h"

using std::cout;
using std::endl;
using boost::numeric::ublas::matrix;
using boost::numeric::ublas::vector;


/// Decide if the given layer can be pruned using Yices decision procedure for
/// linear, real arithmetic.
///
/// input: @param B lattice basis (integral)
///        @param u vector indexing the layer (integral)
///        @param k dimension of the layer
///        @param p space coordinates of the point (rational)
///        @param l L_inf distance to prune at (rational)
///        @param ret_model flag to enable returning a model
///        @param model [out] solution vector of size 'n', the first 'k'
///                 elements are modified by the call
///        @param decision [out] satisfiability decision:
///                 - BLT_CHECK_SAT     (don't prune)
///                 - BLT_CHECK_UNSAT   (prune)
///                 - BLT_CHECK_UNKNOWN (error with problem, or solver failure)
///
/// @return: LP_OK (solver success), or LP_ERROR (problem error or solver
///          failure)
///
int yprune(const matrix<int64>& B, const vector<int64>& u, size_t k,
           const vector<rat64>& p, rat64 l, bool ret_model,
           vector<double> *model, int& decision) {
    // validate input
    const size_t n = B.size1();  // must match size of u, x, and k <= n
    const size_t m = B.size2();  // must match size of p
    assert(k <= n);
    assert(n == u.size());
    assert(!ret_model || n == model->size());
    assert(m == p.size());

    (void) yices_init();
    context_t *ctx = yices_new_context(NULL);
    const type_t yices_real = yices_real_type();

    // declare real variables x[0] ... x[k-1]
    term_t *x = new term_t[k];
    char buf[BLT_YICES_MAX_NAME_SIZE];
    for (size_t i = 0; i < k; ++i) {
        x[i] = yices_new_uninterpreted_term(yices_real);
        snprintf(buf, BLT_YICES_MAX_NAME_SIZE, "x%zu", i);
        yices_set_term_name(x[i], buf);
    }

    term_t formula = yices_true();  // initialize: formula = true
    // construct terms 'f_i(x)'
    term_t b = yices_rational64(l.numerator(), l.denominator());  // cutoff dist
    for (size_t i = 0; i < m; ++i) {
        term_t fi = yices_zero();
        // summands depending on x[i]
        for (size_t j = 0; j < k; ++j) {
            term_t m = yices_mul(x[j], yices_int64(B(j, i)));
            fi = yices_add(fi, yices_neg(m));
        }
        // constant subterms
        for (size_t j = k; j < n; ++j) {
            term_t m = yices_mul(yices_int64(u(j)), yices_int64(B(j, i)));
            fi = yices_add(fi, yices_neg(m));
        }
        fi = yices_add(fi,
                yices_rational64(p(i).numerator(), p(i).denominator()));

        // build the formula:
        // \exists x. \forall i. \pm f_i(x) \leq b
        // i.e., is there a point on the layer that is inside the cube?
        term_t conjs[3] = { formula,
                            yices_arith_leq_atom(fi, b),  //  +- f_i(x) <= b
                            yices_arith_leq_atom(yices_neg(fi), b) };
        formula = yices_and(3, conjs);
    }

    yices_assert_formula(ctx, formula);

#ifdef DEBUG_YICES
    cout << "  yices formula:" << endl;
    yices_pp_term(stdout, formula, 70, 2, 2);
    cout << endl;
#endif

    //
    // Call the decision procedure
    //
    int ret;
    switch(yices_check_context(ctx, NULL)) {

      case STATUS_SAT:
        // extract a model
        if (ret_model) {
            assert(nullptr != model);
            model_t *mdl = yices_get_model(ctx, 1 /* keep elim'd vars */);
#ifdef DEBUG_YICES
            cout << "yices model:" << endl;
            yices_pp_model(stdout, mdl, 70, 10, 0);
            cout << endl;
#endif
            double v;
            for (size_t i=0; i < k; ++i) {
                if (0 != yices_get_double_value(mdl, x[i], &v)) {
                    fprintf(stderr, "Model failure: ");
                    yices_print_error(stderr);
                    decision = BLT_CHECK_UNKNOWN;
                    ret = LP_ERROR;
                    goto common_cleanup;
                }
                (*model)(i) = v;
            }
            yices_free_model(mdl);
        }

        decision = BLT_CHECK_SAT;
        ret = LP_OK;
        break;

      case STATUS_UNSAT:
        decision = BLT_CHECK_UNSAT;
        ret = LP_OK;
        break;

      case STATUS_UNKNOWN:
        // fall through
      default:
        decision = BLT_CHECK_UNKNOWN;
        ret = LP_ERROR;
    }

common_cleanup:  // cleanup Yices session
    yices_free_context(ctx);
    yices_exit();
    delete[] x;
    return ret;
}


/// pretty prints the given Yices term with a given string prefix.
void print_term(const char *name, term_t t) {
    printf("%s: ", name);
    if (yices_pp_term(stdout, t, 80, 20, 0) != 0) {
        printf("error occured in term");
        yices_print_error(stderr);
    } else {
        printf("\n");
    }
}
