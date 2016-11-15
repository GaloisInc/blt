/*
 * Converter from the old lat/con 2-file format to the new "bilp" format
 */

#include <cstring>
#include <fstream>
#include <iostream>
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

using std::cout;
using std::endl;
using std::equal;
using std::ifstream;
using std::string;
using boost::numeric::ublas::matrix;


void print_usage(int argc, char *argv[]) {
    cout << "Usage: " << argv[0] << " <lat> <con>\n";
    cout << endl;
    cout << "  Converts the lat/con pair of files into 'bilp' format data\n";
    cout << "  that is sent to stdout.\n";
    cout << endl;
}


int main(int argc, char* argv[]) {
    int rc;

    // Parse command-line args using getopt
    char options[] = "h";
    int opt;
    while ((opt = getopt(argc, argv, options)) != -1) {
        switch (opt) {
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

    if (argc - optind != 2) {
        ERROR("invalid number of non-option arguments!");
        print_usage(argc, argv);
        return 1;
    }

    // read input matrices
    string latfn(argv[optind++]);
    string confn(argv[optind++]);
    matrix<int64> M, C;
    rc =  read_lattice(latfn, M);
    rc += read_lattice(confn, C);
    if (rc != 0) {
        ERROR("could not read/parse input files: " << latfn << ", " << confn);
        return 1;
    }
    TRACE("lattice size " << M.size1() << "x" << M.size2());
    TRACE("constr size "  << C.size1() << "x" << C.size2());

    // M: rows are lattice vectors, cols are coefficients for each variable
    matrix<int64> L(M.size2(), M.size1());  // allocate transpose of M
    for (size_t i = 0; i < M.size1(); i++)
        for (size_t j = 0; j < M.size2(); j++)
            L(j, i) = M(i, j);
    size_t n = L.size1(); // dim of ambient space = num inequalities
    size_t m = L.size2(); // num lattice vectors = num variables

    // print out bilp formatted data to stdout
    for (size_t i = 0; i < n; i++) {
        cout << C(i, 0);
        cout << " <=";
        for (size_t j = 0; j < m; j++) {
            cout << " " << L(i, j);
        }
        cout << " <= ";
        cout << C(i, 1);
        cout << endl;
    }

    return 0;
}
