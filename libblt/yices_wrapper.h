/// yprune.h
///
/// Author: Benjamin Jones <bjones@galois.com>
/// Copyright: Galois, Inc. 2015
///
/// 'yprune' provides an interface between BLT and the SMT solver Yices. This
/// can be used in the main search search procedure

#pragma once

#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/vector.hpp"

#include "types.h"
#include "yices_stub.h"


/// Maximum size of names for variables declared to Yices
#define BLT_YICES_MAX_NAME_SIZE 256

/// Pretty print yices terms
void print_term(const char *name, term_t);

/// Decides whether or not to prune a given sublattice from the search
/// space. This method is both sound and complete.
int yprune( const boost::numeric::ublas::matrix<int64>&,
            const boost::numeric::ublas::vector<int64>&,
            size_t,
            const boost::numeric::ublas::vector<rat64>&,
            rat64,
            bool,
            boost::numeric::ublas::vector<double>*,
            int&
          );
