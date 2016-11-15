#pragma once

#include <istream>
#include <string>
#include <vector>

#include "boost/numeric/ublas/matrix.hpp"

#include "types.h"

// Parsing
int read_lattice ( const std::string& infile
                 , boost::numeric::ublas::matrix<int64>& M);

int read_bilp    ( std::istream &in
                 , boost::numeric::ublas::matrix<int64> &L
                 , boost::numeric::ublas::matrix<int64> &C);

// General parsing helper functions
int parse_row    (std::istream &in, std::vector<int64> &row);

//int parse_word      ( std::istream &in, std::string &word );
//void skip_whitespace ( std::istream &in);
