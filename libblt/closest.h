#pragma once

#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/vector.hpp"

#include "types.h"


// Inform compiler to tell the branch predictor a thing -JED
#define LIKELY(x)    (__builtin_expect((x), 1))
#define UNLIKELY(x)  (__builtin_expect((x), 0))

/// Data structure for collecting solver stats
struct SolverStats {
    uint64 niter;   ///< number of iterations done
    uint64 nprune;  ///< number of layers pruned
};

/// Constructor for 'SolverStats'
SolverStats new_stats();


/// Closest vector search in the L_infinity metric
///
/// Inputs:
///
///     * `lat` -- The lattice matrix whose rows are lattice vectors. This must
///       be a square matrix, `n x n`.
///     * `con` -- The constraint matrix, representing a hypercube. This must
///       be an `n x 2` matrix. Row `i` corresponds to the lower and upper bounds
///       set on coordinate `i` of the solution.
///     * `enable_yices` -- enable sound pruning via Yices
///
/// Outputs:
///
///     * `sol` -- If the return value is STATUS_SAT, `sol` contains the solution
///       in lattice vector coordinates. If STATUS_UNSAT, `sol` contains the
///       lattice coordinates of the closest vector found.
///
///     * `stats` -- Collects solver statistics for the run (see SolverStats
///       type)
///
///     * Return value: status indicator
///
///     CHECK_SAT
///     CHECK_UNSAT
///     STATUS_LP_FAIL
///     STATUS_ERROR
///
int closest(const boost::numeric::ublas::matrix<int64>&,
            const boost::numeric::ublas::matrix<int64>&,
            const bool,
            boost::numeric::ublas::vector<int64>&,
            SolverStats&);

// Private functions

struct SearchState;
bool check_soln(const boost::numeric::ublas::matrix<int64>&,
                const boost::numeric::ublas::matrix<int64>&,
                const boost::numeric::ublas::vector<int64>&);
int sgn(double);
