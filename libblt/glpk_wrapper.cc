#include <iostream>
#include <cassert>
#include <cmath>
#include <ctime>

#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/vector.hpp"

#include "types.h"
#include "codes.h"
#include "debug.h"
#include "error.h"
#include "glpk.h"
#include "util.h"

#include "glpk_wrapper.h"

using std::cout;
using std::endl;
using boost::numeric::ublas::matrix;
using boost::numeric::ublas::vector;


/// Number of times to try rescaling if errors are encountered in the distance
/// calculation
///
/// TODO - make this a parameter
static const int ITER_MAX = 12800;


/// Driver for 'glpk_infdist' which handles rescaling upon numerical
/// error.
double infdist(const matrix<double>& B, const vector<int64>& u, uint64 k,
               const vector<double>& p, SM* mem,
               // outputs
               vector<double>& x, int& rc) {

    // persistent scaling parameters
    static unsigned iter = 0;

    rc = LP_ERROR;
    double obj;
    unsigned i = 0;  // local iteration count
    while (rc != LP_OK && iter < ITER_MAX) {

        int se = scale_exp(iter);

        // set the scale and try to compute distance
        // scale_t starts at 2^0 and increases by powers of 2 until 2^64
        // where is wraps to 2^-64 and then increases back to 1
        double scale_t = pow(2, se);
        obj = glpk_infdist(B, u, k, p, mem, scale_t, x, rc);

        if (rc != LP_OK) {
#ifdef DEBUG_GLPK
            TRACE("infdist: LP solver failed, scaling level: 2^" << se);
#endif
            iter++;
        }

        i++;
    }

    if (rc != LP_OK) {
        TRACE("LP SOLVER FAILURE -- automatic scaling failed!");
        return 0.0;
    }

    return obj;
}


inline
int scale_exp(unsigned i) {
    return ((i+64) % 128) - 64;
}

/// Compute L_infinity distance from lattice layer to point
///
/// input: @param B lattice basis
///        @param u vector of integers indexing the layer
///        @param k dimension of the layer
///        @param p space coordinates of the point
///        @param t_scale scale coefficient of aux variable t (see below)
///        @param x [out] solution vector, first 'k' elements are
///               modified by the call
///        @param rc [out] return code starting with `LP_'
///
/// @return: distance
///
/// The lattice basis B, vector u, and dimension k determine the layer
/// in question. The layer consists of all real-linear combinations of the
/// first `k` lattice vectors, plus the vector offset obtained by weighting
/// the last (n-k) lattice vectors according to the last (n-k) entries of `u`.
///
/// Warning: math follows.
///
/// The distance is computed by solving the following linear programming
/// problem. Let x_1, ..., x_k, t be real variables. An assignment to the x_i
/// determines a point in the sub-lattice (as in the previous paragraph). In
/// ambient space coordinates this is the point:
///
///     x_1 B_1 + ... + x_k B_k + u_{k+1} B_{k+1} + ... + u_n B_n
///
/// Subtracting the point 'p' we have the relative vector. The L_infinity
/// distance is then the maximum absolute value of the coordinates of the
/// relative vector:
///
///     distance = max_j abs(\sum_{i=1}^k x_i B_{ij} + \sum_{i=k+1}^n u_i B_ij - p_j)
///
/// Denote the argument to abs(..) by f_j(x). Then, the maximum is obtained
/// by the smallest value of t such that:
///
///       f_j(x) <= t,  and
///     - f_j(x) <= t   for all components j
///
/// In the code below we setup this problem and call GLPK to approximate a
/// solution which we return through the return value 't' and the parameter 'x'.
///
double glpk_infdist(const matrix<double>& B, const vector<int64>& u, uint64 k,
                    const vector<double>& p, SM* mem, double t_scale,
                    // outputs
                    vector<double>& x, int& rc) {

    const size_t n = B.size1();  // must match size of u, x, and k <= n
    const size_t m = B.size2();  // must match size of p
    assert(k <= n);
    assert(n == u.size());
    assert(n == x.size());
    assert(m == p.size());

    // memory for constraint matrix:
    // 2*m rows (inequalities)
    // k+1 cols (variables: x_1, ..., x_k, and t)
    // at most n+1 cols
    // value at index 0 is used by the library
    const size_t constr_size = (k + 1)*2*m + 1;
    assert(NULL != mem && "null pointer mem");
    assert(mem->size >= constr_size && "not enough mem");

    glp_prob *lp = glp_create_prob();  // problem environment
    glp_set_obj_dir(lp, GLP_MIN);      // not stricly neccessary

    glp_add_rows(lp, 2*m);    // rows correspond to inequality constraints
    glp_add_cols(lp, k+1);    // cols correspond to free vars x_i & t
    for (size_t i = 0; i < k; ++i) {
        glp_set_col_bnds(lp, i+1, GLP_FR, 0.0, 0.0);  // initial k cols are free vars
        glp_set_obj_coef(lp, i+1, 0.0);               // obj fcn is indep of x_i
    }

    // setup aux variable t and the obj fcn
    glp_set_col_bnds(lp, k+1, GLP_LO, 0.0, 0.0);  // 0 <= t < inf
    glp_set_obj_coef(lp, k+1, 1.0);               // obj fcn =  t

    // for each pair of constraints
    for (size_t i = 0; i < m; ++i) {
        double rhs1=0.0, rhs2=0.0;  // upper bounds on inequality constraints
        for (size_t j = k; j < n; ++j) {
            rhs1 += -u(j) * B(j, i);
            rhs2 +=  u(j) * B(j, i);
        }
        rhs1 += p(i);
        rhs2 += -p(i);
        glp_set_row_bnds(lp, 2*i+1, GLP_UP, 0.0, rhs1);
        glp_set_row_bnds(lp, 2*i+2, GLP_UP, 0.0, rhs2);
    }

    // compute coefficients to fill the problem matrix
    int c = 1;  // index into the memory area
    for (size_t i = 0; i < m; ++i) {
        for (size_t j = 0; j < k; ++j) {
            // matrix[2*i+1][j+1] = B(j, i)
            mem->ia[c] = 2*i+1; mem->ja[c] = j+1; mem->ar[c] = B(j, i);
            ++c;
            // matrix[2*i+2][j+1] = -B(j, i)
            mem->ia[c] = 2*i+2; mem->ja[c] = j+1; mem->ar[c] = -B(j, i);
            ++c;
        }
        // matrix[2*i+1][k+1] = -1.0 * ave;
        mem->ia[c] = 2*i+1; mem->ja[c] = k+1; mem->ar[c] = -1.0 * t_scale;
        ++c;
        mem->ia[c] = 2*i+2; mem->ja[c] = k+1; mem->ar[c] = -1.0 * t_scale;
        ++c;
    }
    assert(c == (int)constr_size && "memory filled == memory declared to glpk");
    glp_load_matrix(lp, constr_size-1, mem->ia, mem->ja, mem->ar);

    // init solver
    glp_smcp param;
    glp_init_smcp(&param);
    // rescale the problem by powers of 2 if needed
    //glp_scale_prob(lp, GLP_SF_SKIP | GLP_SF_2N);
    //
    // set verbosity
    #ifdef DEBUG_GLPK
      param.msg_lev = GLP_MSG_ON;
      glp_term_out(1);
    #else
      param.msg_lev = GLP_MSG_OFF;
      glp_term_out(0);
    #endif

    // solve!
    int r = glp_simplex(lp, &param);  // returns 0 if successful
    int s = glp_get_status(lp);       // returns GLP_OPT if solution is optimal
    double obj;                       // objecive function value at min

    // check the LP solver status
    if (r == 0 && s == GLP_OPT) {
        rc = LP_OK;

        // fill the column solution vector, values of x(i) for i >= k
        // are intentionally left alone (they should not be used by the caller)
        for (size_t i = 0; i < k; ++i) {
            x(i) = glp_get_col_prim(lp, i+1);
        }

        // copy and scale the objective value
        obj = t_scale * glp_get_obj_val(lp);
    }
    else {
#ifdef DEBUG_GLPK
        TRACE("GLPK error: glp_simplex    -> " << r);
        TRACE("            glp_get_status -> " << s);
#endif
        rc = LP_ERROR;
        obj = 0.0;  // bogus
    }

    glp_delete_prob(lp);
    return obj;
}


/// Allocate memory for a sparse matrix with at most 'size' non-zero
/// elements.
SM* newSM(size_t size) {
    SM *r = new SM;
    if (r == NULL) {
        ERROR("out of memory");
        return NULL;
    }
    r->size = size;
    r->ia = new int[size];
    r->ja = new int[size];
    r->ar = new double[size];
    if (r->ia == NULL || r->ja == NULL || r->ar == NULL) {
        ERROR("out of memory");
        return NULL;
    }
    return r;
}


void freeSM(SM* p) {
    assert(NULL != p);
    delete [] p->ia;
    delete [] p->ja;
    delete [] p->ar;
    delete p;
}

