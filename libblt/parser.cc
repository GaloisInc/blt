/// Parser for the Bounded Integer Linear Programming (BILP) Format
///
/// See the 'read_bilp' function and the 'read_lattice' function for
/// description of the input formats supported.

#include <iostream>
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include "boost/numeric/ublas/matrix.hpp"
#include "boost/numeric/ublas/io.hpp"

#include "error.h"
#include "parser.h"
#include "types.h"

using std::cout;
using std::endl;
using std::ifstream;
using std::istream;
using std::string;

using boost::numeric::ublas::matrix;


/// DEPRECATED - use read_bilp and the "BILP" format instead!
///
/// Read a lattice in "Boost" format from file into a matrix<int64>
///
/// The result is stored in the output parameter 'm' passed by
/// reference. Returns 0 on success, or 1 on read or parse error.
///
/// The matrix format is e.g.,
///
///     [2,2]
///     ((1,0), (0,1))
///
/// See documentation in boost/numeric/ublas/io.hpp for more details.
///
int read_lattice(const string &infile, matrix<int64> &m) {
    std::ifstream in(infile);
    if (!in) {
        ERROR("could not open input file: " << infile);
        return 1;
    }
    in >> m;
    if (!in) {
        ERROR("boost matrix parser failed on input file: " << infile);
        return 1;
    }
    return 0;
}

/// Read a (B)ounded (I)nteger (L)inear (P)rogramming problem
///
/// The format of a BILP is a sequence of lines of the form:
///
///     l <= a_1 a_2 ... a_n <= u
///
/// where l, u, a_1, a_2, ..., and a_n are integers that are separated by
/// whitespace.
///
/// TODO: support rational numbers
///
/// in     - (input)  input stream to parse
/// L      - (output) lattice matrix where rows are lattice vectors
/// C      - (output) constraint matrix
///
int read_bilp(istream &in, matrix<int64> &L, matrix<int64> &C) {
    if (!in) {
        ERROR("could not open input stream for parsing");
        return 1;
    }

    size_t nc=0, nr=0;   // number of cols, rows
    string line;

    C.resize(0, 2);
    L.resize(0, 0);

    // parse each line of the input into corresponding rows of the C and L
    // matrices
    while (getline(in, line) /* the <string> version */) {
        std::vector<int64> row;         // temp row, row[0] is lower bound,
                                        // row[end] is upper bound
        std::istringstream iss(line);

        // if parsing row fails, or successfully parsed row has an unexpected
        // # of columns, then return failure
        if (parse_row(iss, row) != 0 ||
           (nc != 0 && row.size()-2 != nc)) {
            ERROR("parse error on input line " << nr+1);
            return 1;
        }

        // record length of the initial row
        if (nc == 0) {
            nc = row.size()-2;
            L.resize(0, nc);
        }

        // copy parsed data into the output arrays
        C.resize(nr+1, 2, true);
        C(nr, 0) = row.front();
        C(nr, 1) = row.back();
        L.resize(nr+1, nc, true);
        for (size_t i=0; i<nc; i++) {
            L(nr, i) = row[i+1];
        }

        nr++;
    }

    if (nr == 0 || nc == 0) {
       ERROR("input stream is empty");
       return 1;
    }

    assert(nr == L.size1() && nc == L.size2());
    assert(nr == C.size1() && 2  == C.size2());
    return 0;
}


/// Parse a row of BILP input
///
/// Parse a line of the form: 'l <= a_1 a_2 ... a_n <= u' where all the
/// symbols represent integers and '<=' represents itself. The tokens parsed
/// as integers are returned in order through the second parameter.
int parse_row(istream &in, std::vector<int64> &row) {
    std::vector<string> toks;

    // split input into words
    string w;
    //while (parse_word(in, w) == 0) {
    while (in >> w) toks.push_back(w);

    // the smallest valid row is 5 tokens long: a <= b <= c
    size_t nt = toks.size();
    if (nt < 5 || toks[1] != "<=" || toks[nt-2] != "<=") {
        ERROR("malformed row: " << w);
        return 1;
    }

    // parse tokens into integers, skip '<=' symbols
    try {
        row.resize(nt-2);
        row[0]    = stoll(toks[0]);      // first and last tokens are the bounds
        row[nt-3] = stoll(toks[nt-1]);
        for (size_t i=2; i<nt-2; i++) {  // middle tokens are the variable coefficients
            row[i-1] = stoll(toks[i]);
        }
    // catch: std::invalid_argument
    //        std::out_of_range
    } catch (const std::exception &e) {
        // TODO: add row, column numbers
        ERROR("parser failed, expecting integer");
        ERROR(e.what());
        return 1;
    }

    return 0;
}
