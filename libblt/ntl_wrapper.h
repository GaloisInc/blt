// ntl_wrapper
//
// Functions for converting to and from boost::numeric::ublas::matrix<T> and
// NTL::Mat<T> as well as functions for calling NTL routines on boost types.
//
#include <string>
#include <boost/numeric/ublas/matrix.hpp>

#include "NTL/matrix.h"
#include "NTL/ZZ.h"
#include "NTL/LLL.h"

#include "types.h"

using std::string;
using boost::numeric::ublas::matrix;
using NTL::Mat;
using NTL::ZZ;

int64 LLL(matrix<int64>&, matrix<int64>&);
int64 LLL_FP(matrix<int64>&, matrix<int64>&);
int64 G_LLL_FP(matrix<int64>&, matrix<int64>&);

long expected_solns(const matrix<int64>&, const matrix<int64>&, bool&);

void matrix_to_ntl(const matrix<int64>&, Mat<ZZ>&);
void ntl_to_matrix(const Mat<ZZ>&, matrix<int64>&);

Mat<ZZ> take_cols(const Mat<ZZ>&, size_t);
Mat<ZZ> read_lattice_ntl(const string);
