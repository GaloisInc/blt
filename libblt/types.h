#pragma once

#include <cstdint>
#include "boost/rational.hpp"
#include "boost/numeric/ublas/matrix.hpp"

typedef int64_t int64;                                                  ///< standard 64-bit integer type
typedef uint64_t uint64;                                                ///< unsigned version, used for Nat
typedef boost::rational<int64_t> rat64;                                 ///< rational over int64
typedef boost::numeric::ublas::matrix<boost::rational<int64>> rbmatrix; ///< rational matrix
typedef boost::numeric::ublas::matrix<int64> ibmatrix;                  ///< integer matrix
