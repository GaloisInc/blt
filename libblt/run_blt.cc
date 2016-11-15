/*
 * Standalone solver program using libblt
 *
 * INPUT:
 *
 * Expected input is a text file describing a system of integer linear
 * two-sided inequalities. For example:
 *
 * 1 <= 2 3 -1 <= 8
 * 0 <= 0 0 1  <= 2
 *
 * represents the pair of two-sided inequalities: 1 <= 2x + 3y - z <= 8
 * and 0 <= z <= 2.
 *
 * In general, `run_blt` decides the satisfiability of a systems of inqualities:
 *
 *     L <= A*x <= U
 *
 * where `x` is an n-vector of integer variables, A is an integer matrix,
 * and L, U are integer vectors.
 *
 * OUTPUT:
 *
 * The output consists of a single line on stdout of the form:
 *
 * PROBLEM <name> RUNTIME <r> niter <i> npru <p> rc <s> check <c>
 *
 * where:
 *
 *   <name> |  provided on the command line
 *      <r> |  runtime in seconds
 *      <i> |  number of search iterations
 *      <p> |  number of layers pruned during search
 *      <s> |  return status, e.g. OK, BLT_STATUS_INPUT_ERROR, BLT_STATUS_LP_ERROR
 *      <c> |  decision: SAT or UNSAT
 *
 */

#include <algorithm>
#include <cmath>
#include <cstring>
#include <ctime>
#include <fstream>
#include <iostream>
#include <memory>
// POSIX only
#include <unistd.h>

#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/vector.hpp"
#include "boost/numeric/ublas/io.hpp"

#include "blt.h"
#include "codes.h"
#include "debug.h"
#include "error.h"
#include "parser.h"
#include "yices_wrapper.h"

using std::cout;
using std::endl;
using std::equal;
using std::ifstream;
using std::shared_ptr;
using std::string;
using std::vector;
using boost::numeric::ublas::matrix;


void print_usage(int argc, char *argv[]) {
    cout << "Usage: " << argv[0] << " [options]  <input>\n";
    cout << "\n";
    cout << "  [options]\n";
    cout << "  -v          print version information\n";
    cout << "  -h          print this help message\n";
    cout << "  -m          print out a model if SAT\n";
    cout << "  -y          enable \"sound and complete mode\" using the Yices SMT solver\n";
    cout << "              (libyices shared library must be in the runtime linker's search path)\n";
    cout << "\n";
    cout << "  <input>     input filename (BILP format)\n";
    cout << endl;
}


int main(int argc, char* argv[]) {
    int rc;

    // Parse command-line args using getopt
    bool do_model   = false;
    bool do_version = false;
    bool do_yices   = false;
    char options[]  = "mhvy";
    int opt;
    while ((opt = getopt(argc, argv, options)) != -1) {
        switch (opt) {
            case 'm':
                do_model = true;
                break;
            case 'v':
                do_version = true;
                break;
            case 'y':
                do_yices = true;
                break;
            case '?':
                ERROR("Unknown option: " << optopt);
                // fall through
            case 'h':
                // fall through
            default:
                print_usage(argc, argv);
                return 1;
        }
    }

    if (do_version) {
        cout << "BLT version " << __BLT_VERSION << endl;
        return 0;
    }

    // ensure we have an input file
    if (argc - optind != 1) {
        ERROR("Invalid numer of non-option arguments!");
        print_usage(argc, argv);
        return 1;
    }

    // 0. read input matrices
    string fn(argv[optind++]);
    ifstream infile(fn);
    matrix<int64> M, C;
    rc = read_bilp(infile, M, C);
    if (rc != 0) {
        ERROR("cannot parse input file: " << fn);
        return 1;
    }
    TRACE("DEBUG: lattice size " << M.size1() << "x" << M.size2());
    TRACE("DEBUG: constr  size "  << C.size1() << "x" << C.size2());

    size_t n = M.size2(); // num lattice vectors = num variables
    size_t m = M.size1(); // dim of ambient space = num inequality pairs

    // setup problem
    // 1. create fresh variables
    CContext ctx = blt_init(do_yices);
    if (!ctx) {
        ERROR("BLT initialization failed");
        return BLT_STATUS_ERROR;
    }

    size_t var_name_size = 2 + lround(log10(n) + 0.5);
    char *var_name = new char[var_name_size];
    CExpr *var = new CExpr[n];
    for (size_t i=0; i<n; i++) {
        snprintf(var_name, var_name_size, "%zu", i);
        var[i] = blt_var(ctx, var_name);
    }

    // 2. assert linear inequalities
    for (size_t i=0; i<m; i++) {
        CExpr e = blt_const(ctx, 0, 1);  // e <- 0
        for (size_t j=0; j<n; j++) {
            e = blt_add(ctx, e, blt_smul(ctx, M(i,j), 1, var[j]));
        }
        (void) blt_assume(ctx, C(i,0), 1, e, C(i,1), 1);
    }

    // 3. run solver
    clock_t start = clock();
    rc = blt_check(ctx);
    double elapsed = (1.0*(clock() - start))/CLOCKS_PER_SEC;

    cout << "PROBLEM " << fn << " RUNTIME " << elapsed << " ";
    cout << "niter " << "XXX" << " npru " << "XXX" << " ";
    cout << "rc " << "XXX" << " check "
         << (rc == BLT_CHECK_SAT
                 ? " SAT "
                 : (rc == BLT_CHECK_UNSAT
                     ? " UNSAT "
                     : "INPUT_ERROR"))
         << endl;

    // 4. print a model
    if (do_model && rc == BLT_CHECK_SAT) {
        cout << "PROBLEM " << fn << " MODEL ";
        int64 x;
        cout << "(";
        for (size_t i=0; i<n; i++) {
            snprintf(var_name, var_name_size, "%zu", i);
            blt_model(ctx, var_name, &x);
            cout << x << (i<n-1 ? ", " : ")\n");
        }
    }

    delete[] var;
    delete[] var_name;
    return rc;
}
