#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <map>
#include <memory>
#include <new>
#include <stdexcept>
#include <vector>
#include <set>

#include "boost/math/common_factor.hpp"
#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/vector.hpp"
#include "boost/numeric/ublas/io.hpp"
#include "boost/rational.hpp"

#include "closest.h"
#include "codes.h"
#include "debug.h"
#include "error.h"
#include "expr.h"
#include "ntl_wrapper.h"
#include "types.h"
#include "yices_stub.h"

#include "context.h"

using std::abs;
using std::cerr;
using std::endl;
using std::find;
using std::make_shared;
using std::map;
using std::max;
using std::set;
using std::shared_ptr;
using std::string;

using boost::math::lcm;
using boost::math::gcd;
using boost::numeric::ublas::matrix;
using boost::numeric::ublas::vector;  // we're using both std::vector and boost vector
using boost::rational;

/// Context::mkVar
///
/// Create a fresh variable in the context with a given name. If the variable
/// already exists in the context, return a new 'Expr' representing it.
///
Expr Context::mkVar(const string &name) {
    int idx;
    auto it = find(names.begin(), names.end(), name);
    if (it == names.end()) {        // new variable
        idx = names.size();
        names.push_back(name);      // STL containers may throw std::bad_alloc
        indices[name] = idx;
        mask.resize(names.size());  // update the mask size to match `names`
    } else {                        // existing variable
        idx = indices[name];
    }
    return Expr(idx);
}

Expr Context::mkConst(rational<int64> c) {
    return ExprConst(c);
}


/// Return (in 'idx') the index associated with a variable (name) if it exists. If it
/// doesn't, return error code 1.
int Context::getVarIdx(const string &name, int& idx) const {
    auto it = indices.find(name);
    if (it == indices.end()) {
        return 1;
    } else {
        idx = it->second;
        return 0;
    }
}


/// Return the name of the variable whose index is given. If the index
/// is out of range, throw an exception.
int Context::getVarName(size_t idx, string& name) const {
    if (idx < names.size()) {
        name = names[idx];
        return 0;
    } else {
        return 1;
    }
}


/// Predicate on whether the named variable appears in any current assumption.
/// Note: a variable may be known in the Context but not be active due to
/// backtracking.
bool Context::varActive(const string &name) const {
    int idx;
    int rc = getVarIdx(name, idx);
    if (rc == 0)
        return mask[idx];
    else
        return false;
}


/// Add an atom to the current context.
///
/// A trivial consistency check is performed in order to determine if the
/// atom by itself is trivial UNSAT. Using const we garauntee that the
/// caller's Expr object won't be modified by the solver.
int Context::assume(const rational<int64> lower, Expr expr, const rational<int64> upper) {
    // consistency check
    if (lower <= upper) {
        try {
            Atom a(lower, expr, upper);
            assumptions.push_back(a);
            // update bitmask keeping track of variable indices
            const set<int> &idxs = expr.indices();
            for (auto i : idxs) {
                mask[i] = true;
            }
            return BLT_STATUS_OK;
        } catch (std::bad_alloc &e) {
            ERROR("Context::assume: Out of memory" << std::endl << e.what());
            return BLT_STATUS_ERROR;
        }
    }
    return BLT_CHECK_UNSAT;
}


/// Check the satisfiability of the current set of assumptions.
///
/// If the current formula is satisfiable, return BLT_CHECK_SAT. If the
/// current formula is unsatisfiable, return BLT_CHECK_UNSAT. The function
/// can also return BLT_STATUS_INPUT_ERROR, and BLT_STATUS_ERROR.
int Context::check() {

    //--------------------------------------------------
    // 0. convert assertion stack to pair of matrices (lattice, constr)
    //    note: rows of `lattice` are the lattice vectors
    matrix<rational<int64>> L0 = compute_lattice();
    size_t m = L0.size1();  // num lattice vectors (# rows of lattice matrix)
    size_t n = L0.size2();  // num of constraints (dim of lattice's ambient
                            // space)
    if (n == 0 || m == 0) {
        WARN("Context::check: zero-dimensional input lattice or constraint set");
        return BLT_STATUS_INPUT_ERROR;
    }
    matrix<rational<int64>> C0  = compute_constr();
    assert(C0.size1() == n);
    TRACE("Context::check: original lattice: " << L0);
    TRACE("Context::check: original constrs: " << C0);

    // Convert the rational system to an integral one
    matrix<int64> L, C;
    int rc = convert_to_integral(L0, C0, L, C);
    if (rc != BLT_STATUS_OK)
        return rc;
    TRACE("Context::check: integral lattice: " << L);

    //--------------------------------------------------
    // 1. scale to make a hypercube

    rc = stretch_to_cube(L, C);
    if (rc != BLT_STATUS_OK)
        return rc;

    //--------------------------------------------------
    // 2. reduce and LLL the assertion stack to square lattice

    matrix<int64> U;       // inverse of LLL transform

    int rank = LLL(L, U);  // LLL reduction in-place.
                           // If L is m.x.n and has rank r, (r <= m)
                           // then resulting lattice has last r rows
                           // non-zero and U is m.x.m that U * oldL = newL
                           // and the first (m-r) rows of U generate the
                           // kernel of oldL.

    TRACE("Context::check: rank of LLL lattice: " << rank);
    if (rank == 0) {
        TRACE("Context::check: zero-rank input lattice");
        return BLT_STATUS_INPUT_ERROR;
    }

    matrix<int64> newL(rank, n);
    // copy last (m-rank) rows of L to newL
    for (size_t i = 0; (int)i < rank; ++i)
        for (size_t j = 0; j < n; ++j)
            newL(i, j) = L(i + m - rank, j);

#ifdef DEBUG
    if (rank == n) {
        int64 est;
        bool inv = false;
        est = expected_solns(newL, C, inv);
        TRACE("Context::check: solution density: " \
              << (inv ? "1 / " : "") << est);
    }
#endif

    //--------------------------------------------------
    // 3. call closest()  --> soln vector or err code

    // Determine if Yices has been located and loaded in memory already, set
    // 'yok' appropriately as (user wants yices) \cap (yices is available).
    bool yok = false;
    if (is_yices_enabled()) {
        TRACE("Context::check: trying to load libyices.so");
        if (load_yices_c > 0) {  // yices has already been loaded
           TRACE("Context::check: libyices.so was already loaded");
           yok = true;
        } else {                 // attempt to load yices symbols dynamically
            int rc = load_yices();
            if (rc == 0) {
                TRACE("Context::check: successfully loaded libyices.so");
                yok = true;
            } else {
                ERROR("Context::check: ERROR Yices support was requested, " \
                      "but libyices.so failed to load!");
                return BLT_STATUS_ERROR;
            }
        }
    }
    TRACE("Context::check: yok = " << (yok ? "true" : "false"));

    vector<int64> presoln_a(m-rank);
    for (size_t i = 0; i < m-rank; ++i)  // pad presoln with zeros
        presoln_a(i) = 0;

    vector<int64> presoln_b(rank);
    SolverStats stats = new_stats();
    rc = closest(newL, C, yok, presoln_b, stats);
    TRACE("Context::check: number of iterations: " << stats.niter+1);
    TRACE("Context::check: number of prunes:     " << stats.nprune);
    if (rc != BLT_STATUS_OK) {  // pass the error up
        return rc;
    }

    //---------------------------------------------------------------
    // 4. convert solution from closest() back to original variables

    vector<int64> presoln(m);
    // join presoln_a and presoln_b
    for (size_t i = 0; i < m; ++i) {
        if (i < m-rank)
            presoln(i) = presoln_a(i);
        else
            presoln(i) = presoln_b(i-m+rank);
    }
    vector<int64> soln = prod(presoln, U);

    //--------------------------------------------------
    // 5. populate model

    model.clear();

    // loop over all known variables, filling in values for only the ones
    // which are active.
    for (size_t i = 0, j = 0; i < mask.size(); ++i) {
        if (mask[i]) {
            string name;
            (void) getVarName(i, name);  // index 'i' is in the name map by construction
            model[name] = soln[j++];
        }
    }

    //--------------------------------------------------------
    // 6. return status code BLT_CHECK_SAT or BLT_CHECK_UNSAT

    TRACE("Context::check: BLT checkSat completed");

    if (check_soln(newL, C, presoln_b))
        return BLT_CHECK_SAT;
    else
        return BLT_CHECK_UNSAT;
}


/// Returns the value currently assigned to the variable with given name.
/// This overwrites the value of the second argument. If there is no such
/// variable, or no value, return an error code.
int Context::get_model(const string &name, int64* val) const {
    auto it = model.find(name);
    if (it != model.end()) {
        *val = it->second;
        return BLT_STATUS_OK;
    }
    return BLT_STATUS_INPUT_ERROR;
}

/// Same as above but takes a variable index instead of a name.
int Context::get_model(size_t idx, int64* val) const {
    string name;
    int rc = getVarName(idx, name);
    if (rc) {
        cerr << "invalid variable index " << idx << endl;
        return BLT_STATUS_INPUT_ERROR;
    } else
        return get_model(name, val);
}

/// Return a saved state that let's us recreate the current context at a later
/// time by backtracking.
SolvState Context::save_state() const {
    return SolvState(assumptions.size());
}

/// Return true is the given state can be jumped to. There is no backtracking
/// forward.
bool Context::valid_state(SolvState st) const {
    return st.pos() <= assumptions.size();
}

/// Recreate the Context pointed to by the given Context. Returns a success
/// or failure code.
int Context::backtrack(SolvState st) {
    if (valid_state(st)) {
        // pop off the assumptions stack until we're at st.pos()
        assumptions.resize(st.pos());
        update_mask();  // update the variable bitmask for the new set of
                        // assumptions
        return BLT_STATUS_OK;
    } else {
        return BLT_STATUS_ERROR;
    }
}

// Private Members #############################################################

/// Returns a matrix representing the rows of expressions in the assumption
/// stack of the Context.
matrix<rational<int64>> Context::compute_lattice() {
    size_t m = nvars();             // number of rows
    size_t n = assumptions.size();  // number of cols
    TRACE("Context::compute_lattice:");
    TRACE("    no. vars        = " << m);
    TRACE("    no. assumptions = " << n);
    matrix<rational<int64>> mat(m, n);
    // fill mat
    // each row is an Atom
    auto col_start = assumptions.begin();
    for (auto col = col_start; col != assumptions.end(); ++col) {
        Expr expr = col->expr();
        // loop over all known variables in the context, but we only record
        // coefficients of those which are active globally.
        for (size_t idx = 0, row = 0; idx < mask.size(); ++idx) {
            // row is the variable's row index in the returned matrix,
            // in general `row` and `idx` will differ for a given variable
            if (mask[idx]) {
                assert(row <= idx && row < m);
                mat(row, col - col_start) = expr[idx];
                ++row;
            }
        }
    }
    return mat;
}

/// Returns a matrix representing the rows of lower/upper bounds in the
/// assumption stack of the Context. We also take care of subtracting constants
/// from all sides.
matrix<rational<int64>> Context::compute_constr() {
    int m = assumptions.size();         // number of rows
    matrix<rational<int64>> mat(m, 2);  // columns are [lower, upper]
    rational<int64> c;
    // fill mat
    for (auto row = assumptions.begin(); row != assumptions.end(); ++row) {
        c = row->expr().get_const();
        mat(row-assumptions.begin(), 0) = row->lower() - c;
        mat(row-assumptions.begin(), 1) = row->upper() - c;
    }
    return mat;
}

void Context::update_mask() {
    // replace old mask with the zero vector
    mask.clear();
    mask.resize(names.size(), false);
    // mark the variables that occur in each assumption
    for (auto i = assumptions.begin(); i != assumptions.end(); ++i) {
        const set<int> idxs = i->expr().indices();
        for (size_t vi : idxs) {
            assert(vi < mask.size() &&
                   "Context::update_mask: FATAL ERROR: \
                   assumptions and names are out of sync!");
            mask[vi] = true;
        }
    }
}


////////////////////////
/// Non-class functions

/// Convert a rational system to an integal system
///
/// Scale each inequality of the BILP system by the least common multiplier of
/// the set of denominators. An attempt is made to detect integer overflow.
///
/// TODO: use an lcm function with sound overflow detection
int convert_to_integral( matrix<rational<int64>> L0
                        , matrix<rational<int64>> C0
                        , matrix<int64> &L
                        , matrix<int64> &C ) {
    size_t nrows = L0.size1();
    size_t ncols = L0.size2();  // same as number of contraint pairs
    assert(ncols == C0.size1());

    L.resize(nrows, ncols);
    C.resize(ncols, 2);

    for (size_t c = 0; c < ncols; ++c) {
        // find lcm of denominators row by row
        int64 mult = 1;
        for (size_t r = 0; r < nrows; ++r) {
            int64 t_mult = lcm(mult, L0(r, c).denominator());
            // all denominators are positive, so lcm should be non-decreasing
            // at each step
            if (t_mult < mult) {
                // At this point, undefined behavior has occured
                ERROR("integer overflow detected!");
                return BLT_STATUS_ERROR;
            }
            mult = t_mult;
        }
        mult = lcm(mult, C0(c, 0).denominator());
        mult = lcm(mult, C0(c, 1).denominator());

        TRACE("row " << c << " lcm " << mult);  // yes, row 'c' is correct

        // clear denominators and assign to the integer matrices
        // TODO: detect overflow
        for (size_t r = 0; r < nrows; ++r) {
            L(r, c) = (mult / L0(r, c).denominator()) * L0(r, c).numerator();
        }
        C(c, 0) = (mult / C0(c, 0).denominator()) * C0(c, 0).numerator();
        C(c, 1) = (mult / C0(c, 1).denominator()) * C0(c, 1).numerator();
    }
    return BLT_STATUS_OK;
}

/// Scale the system so that the constraint set is a hypercube
///
/// Individually scale the rows of C and L by a positive integer so that
///  forall i. C(i,1)-C(i,0) = H for some fixed H.
///
/// L is an n.x.m matrix
/// C is an n.x.2 matrix
///
/// TODO: use an lcm function with sound overflow detection
int stretch_to_cube(matrix<int64> &L, matrix<int64> &C) {
    size_t m = L.size1();
    size_t n = L.size2();
    assert(n == C.size1());

    // find the lcm of all the constraint sizes
    int64 l = 1;
    for (size_t i = 0; i < n; ++i) {
        int64 r = C(i,1)-C(i,0);
        if (r <= 0) {
            ERROR("upper and lower bound coincide or conflict");
            return BLT_STATUS_INPUT_ERROR;
        }
        int64 tmpl = lcm(l, r);
        if (tmpl % l != 0 || tmpl % r != 0) {
            // At this point, undefined behavior has occured
            ERROR("integer overflow in lcm detected");
            return BLT_STATUS_INPUT_ERROR;
        }
        l = tmpl;
    }
    TRACE("stretch: lcm = " << l);

    // scale each row so that C(i,1)-C(i,0) == lcm
    int64 max_C = 0, max_L = 0;  // maximum constraint coefficients and lattice
                                 // coefficients, for DEBUG purposes only
    for (size_t i = 0; i < n; ++i) {
        int64 mult = l / (C(i,1)-C(i,0));
        // scale the constraints
        // TODO add a check for overflow here
        C(i,0) *= mult;
        C(i,1) *= mult;
        max_C = max(abs(C(i, 0)), max_C);
        max_C = max(abs(C(i, 1)), max_C);
        // scale the lattice
        // TODO add a check for overflow here
        for (size_t j = 0; j < m; ++j) {
            L(j, i) *= mult;
            max_L = max(abs(L(j,i)), max_L);
        }
    }
    TRACE("stretch: max_C = " << max_C);
    TRACE("         max_L = " << max_L);

    return BLT_STATUS_OK;
}
