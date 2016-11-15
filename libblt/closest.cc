/// Closest vector search in the L_infinity metric
///
/// Author: Benjamin Jones <bjones@galois.com>
/// Copyright: 2014, 2015 Galois, Inc.
///
/// See header file closest.h for high level documentation.

#include <algorithm>
#include <cassert>
#include <iomanip>
#include <iostream>
#include <ostream>
#ifdef PROGRESS
  #include <string>
#endif

#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/vector.hpp"
#include "boost/numeric/ublas/io.hpp"

#include "codes.h"
#include "debug.h"
#include "error.h"
#include "glpk_wrapper.h"
#include "types.h"
#include "util.h"
#include "yices_wrapper.h"

#include "closest.h"

using std::left;
using std::ostream;
using std::copy;
using std::setw;
using boost::numeric::ublas::matrix;
using boost::numeric::ublas::vector;

/// Search state in the Schnorr-Euchner algorithm.
/// Contains the important bits of state that are updated during the search.
struct SearchState {
    uint64 dim;            ///< dimension of ambient space
    uint64 k;              ///< dimension of layer
    uint64 old_k;
    vector<int64> u;       ///< layer index
    vector<int64> uhat;    ///< copy of best layer index so far
    vector<double> x;      ///< approx. coordinates of the current *real* solution
    vector<int64> step;    ///< step vector controls direction of search
    vector<bool>  pruned;  ///< to determine if we've pruned anything on each layer

    /// Initialize SearchState object for an `n` dimensional lattice search
    explicit SearchState(uint64 n) : dim(n), k(n), old_k(-1), u(n), uhat(n), x(n),
                                     step(n), pruned(n) {
        for (size_t i = 0; i < n; ++i) {  // initialize the vectors
               u(i)      = 0;
               uhat(i)   = 0;
               x(i)      = 0.0;
               step(i)   = 0;
               pruned(i) = false;
        }
    };

    // State transition functions
    void set_index(uint64);
    void descend();
    void explore();
    void ascend();
    void record();

    std::string to_str();
};


// Initialize 'u', and 'step' using the last coordinate of 'x' to
void SearchState::set_index(uint64 n) {
    // Set 'u' to represent the closest hyperplane to 'x' which contains an
    // (n-1)-dimensional sublattice.
    u(n-1) = static_cast<int64>(round(x(n-1)));

    // 'step' always points towards 'x' from 'u', so if we rounded up in last
    // line, we'll step down past x next time.
    step(n-1) = sgn(x(n-1) - u(n-1));
}


/// `descend` lowers the layer dimension k by 1, sets the corresponding
/// coefficient of the layer index `u` using results of the previous call
/// to `infdist`, and sets the step to point in the correct direction.
void SearchState::descend() {
    assert(k != 0);
    old_k = k;
    k--;
    u(k-1) = static_cast<int64>(round(x(k-1)));
    step(k-1) = sgn(x(k-1) - u(k-1));
    pruned(k-1) = false;
}

/// `explore` updates the layer index to point to the next closest point on
/// the current 1-dimensional layer.
void SearchState::explore() {
    old_k = k;
    k++;
    u(k-1) += step(k-1);
    step(k-1) = -step(k-1) - sgn(step(k-1));
}

/// `ascend` is called when the search bottoms out at a 1-dimensional layer
/// and has exhausted all fruitful points there. In general, `ascend` raises
/// the dimension by 1 and sets the layer index at that dimension according to
/// the next step value for that dimension.
void SearchState::ascend() {
    old_k = k;
    if (pruned(k-1)) {
        // ascend
        k++;
    } else {
        // wait to ascend until two attempts are made in a row (one on either
        // side of the target)
        pruned(k-1) = true;
    }
    u(k-1) += step(k-1);
    step(k-1) = -step(k-1) - sgn(step(k-1));
}

/// 'record' write the value of the current layer index ('u') into a temporary
/// location in order to save if for possible future return value.
void SearchState::record() {
    // copy the value of 'u' over 'uhat'
    copy(u.begin(), u.end(), uhat.begin());
}

/// Return the contents of a SearchState object as a string
std::string SearchState::to_str() {
    std::ostringstream out;
    out << "k "    << std::setw(4) << k          << " ";
    out << "u "    << u;
    return out.str();
}


// ----------------------------------------------------------------------
//
// The Main Search Routine
//
// ----------------------------------------------------------------------


/// Carries out the Schnorr-Euchner closest vector algorithm using the
/// L_infinity metric.
///
/// The lattice is searched for a vector that is closest to the center of the
/// (n-dimensional) box whose opposite corners are given in 'con'. When
/// 'exit_early' is true, the search terminates as soon as a lattice vector is
/// found *inside* the box.
///
/// Computation of L_infinity distance for sublayers of the lattice is handled
/// by general purpose linear programming (GLPK). If BLT is compiled with
/// Yices support, the pruning decision can be perfromed in a completely sound
/// manner, rather than relying on floating point operations.
///
/// Input and output arguments are described inline below, but note the
/// following assumptions:
///
/// 1) the rows of 'lat' are linearly independent over QQ
/// 2) for each index 'i', con[i][0] < con[i][1]
///
int closest(const ibmatrix &lat,       ///< [in]  n x m lattice matrix, vectors are rows
            const ibmatrix &con,       ///< [in]  m x 2 constraint matrix
            const bool enable_yices,   ///< [in]  flag to enable sound pruning via yices
            vector<int64> &sol,        ///< [out] solution vector or closest vector
            SolverStats &stats) {      ///< [out] solver stats

    // input shape sanity check
    size_t n = lat.size1();
    size_t m = lat.size2();
    if (m != con.size1() || 2 != con.size2() || m < n || n == 0 || m == 0) {
        return BLT_STATUS_INPUT_ERROR;
    }

    // compute the target point, center of the constraint cube
    vector<rat64>  p(m);
    vector<double> p_fp(m);  // floating point approx of 'p'
    for (size_t i = 0; i < m; ++i) {
        p(i) = rat64((con(i, 1) + con(i, 0)), 2);
        p_fp(i) = boost::rational_cast<double>(p(i));
    }

    TRACE("Starting lattice search (Yices?  "
      << (enable_yices ? "ON" : "OFF") << ")");

    // initialize search state
    SearchState state(n);
    // L_inf radius of the constraint set
    const rat64 radius = rat64(con(0, 1) - con(0, 0), 2);
    const double radius_fp = boost::rational_cast<double>(radius);


    // Compute the closest real point in the ambent space in terms of lattice
    // basis coordinates. This is equivalent to computing 'x' such that
    // 'B^Tr * x = p'. We discard the distance and use the value of 'x' as
    // a heuristic for assigning to 'u' in the search loop.
    //
    int rc;                             // used throughout the function as a return code
    const matrix<double> lat_fp = lat;  // floating point copy of lat for 'infdist'
    SM* mem = newSM((n+1)*2*m + 1);     // size determined by size of GLPK problem
    double new_dist;                    // L_inf distance from current sublayer to 'p'
    new_dist = infdist(lat_fp, state.u, state.k, p_fp, mem, state.x, rc);
    TRACE("  initial infdist = " << new_dist);
    TRACE("  initial model x = " << state.x);
    if (rc != LP_OK) {
        ERROR("LP backend failure (pre-iteration): retcode " << rc);
        freeSM(mem);
        return BLT_STATUS_LP_FAIL;
    }


    // ------------------------------------------------------------
    // The Main Loop

    state.set_index(n);  // initialize state.u & state.step
    uint64 counter = 0;  // count number of iterations
    uint64 nprune  = 0;  // count number of branches pruned
    PROG(">");           // output progress indicator.
                         // TODO: output at verbosity level set at runtime
    while (true) {

        TRACE("\nloop " << counter
           << ", k "  << state.k
           << ", u "  << state.u);

        // Step 1. compute the new distance, best sublayer to explore
        new_dist = infdist(lat_fp, state.u, state.k-1, p_fp, mem, state.x, rc);
        TRACE("  newdist " << new_dist
           << ", radius " << radius_fp);
        if (UNLIKELY(rc != LP_OK)) {
            ERROR("LP backend failure: retcode " << rc);
            ERROR("  -- iteration number " << counter);
            rc = BLT_STATUS_LP_FAIL;
            goto return_closest;
        }

        // Step 2. Make a pruning decision.
        //
        // Compare 'new_dist' to the constraint radius.
        // If smaller, we have more to explore in this layer.
        //
        // If "sound pruning" is enabled, we call out to Yices for the
        // decision iff the LP solver heuristic suggests pruning.
        //
        int explore = approximatelyLE(new_dist, radius_fp, DIST_EPS) ? 1 : 0;

        // Case: there are no feasible RR points on this layer
        // (according to the LP solver)
        if (explore == 0) {
            if (LIKELY(state.k < n)) {

                // If sound pruning is enabled, we re-evaluate the decision
                // using Yices
                if (enable_yices) {
                    TRACE("closest: checking numerical prune decision with Yices");
                    int d;
                    int rc1 = yprune(lat, state.u, state.k-1, p, radius, false, nullptr, d);
                    if (UNLIKELY(rc1 != LP_OK)) {
                        rc = BLT_STATUS_LP_FAIL;
                        goto return_closest;
                    }
                    explore = (d == BLT_CHECK_SAT) ? 1 : 0;
                }

                if (explore == 0) {
                    TRACE("  --> prune");
                    PROG(((state.old_k > state.k) ?
                             (std::string("(") + std::string(std::to_string(state.k)) +
                              std::string(")^")) :
                             std::string("^")));
                    state.ascend();
                    nprune++;
                    continue;
                }

            } else {
                // the vector returned in 'sol' at this point will be the
                // closest one seen
                PROG("U\n");
                rc = BLT_STATUS_OK;
                goto return_closest;
            }
        }

        // Case: there are RR feasible points on this layer
        if (explore == 1) {

            TRACE("  --> explore");
            // descent case: move down one layer
            if (state.k > 1) {
                PROG(".");
                state.descend();
            }

            // when k == 1, record a new solution vector
            else {
                state.record();  // record current vector
                // 'check_soln' verifies the current vector
                if (check_soln(lat, con, state.u)) {
                    PROG("S\n");
                    rc = BLT_STATUS_OK;
                    goto return_closest;
                }
                // continue exploration deeper in the lattice
                else {
                    PROG("v");
                    state.explore();
                }
            }
        }
        counter++;
    }  // end main loop

    // common cleanup:
    // copy solution to the output and return
return_closest:
    sol.resize(n);
    copy(state.uhat.begin(), state.uhat.end(), sol.begin());
    stats.niter  = counter;
    stats.nprune = nprune;
    freeSM(mem);
    return rc;
}

/// Check if the vector `u` represents a solution to the ILP problem
bool check_soln( const matrix<int64>& lat, const matrix<int64>& con
               , const vector<int64>& u) {
    // compute space coordinates of the lattice vector `u` represents
    vector<int64> z = prod(u, lat);
    size_t m = z.size();
    for (size_t i = 0; i < m; ++i) {
        if (con(i, 0) > z(i) || z(i) > con(i, 1))
            return false;
    }
    return true;
}

/// Custom sgn function that returns -1 at z=0
inline
int sgn(double z) {
    return z > 0 ? 1 : -1;
}

/// Create a fresh set of stats
SolverStats new_stats() {
    SolverStats r;
    r.niter = 0;
    r.nprune = 0;
    return r;
}
