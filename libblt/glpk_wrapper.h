#pragma once

#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/vector.hpp"

#include "types.h"

// An estimate of relative numerical inaccuracy in calculating L_infinity distances.
// This number is based on average LP solver performance for 64-dimensional
// DCT based inputs.
//
// TODO: explore setting DIST_EPS dynamically based on reported accuracy
static const double DIST_EPS = 1.0e-4;

/// working memory for a sequence of distance calculations; this is
/// essentially a sparse matrix
struct SM {
    size_t size;
    int *ia;
    int *ja;
    double *ar;
};
typedef struct SM SM;

/// A driver for 'glpk_infdist' which handles rescaling upon failure
double
infdist(const boost::numeric::ublas::matrix<double>&,
        const boost::numeric::ublas::vector<int64>&,
        uint64,
        const boost::numeric::ublas::vector<double>&,
        SM*,
        // outputs
        boost::numeric::ublas::vector<double>&,
        int&);

/// helper function to cycle through the range of scaling
/// exponents: [-64 .. 64]
int scale_exp(unsigned);

/// Compute L_infinity distance between a lattice layer and a point
/// using GLPK.
double
glpk_infdist(const boost::numeric::ublas::matrix<double>&,
             const boost::numeric::ublas::vector<int64>&,
             uint64,
             const boost::numeric::ublas::vector<double>&,
             SM*,
             double,
             // outputs
             boost::numeric::ublas::vector<double>&,
             int&);

SM* newSM(size_t);
void freeSM(SM*);

// Bob's your uncle
