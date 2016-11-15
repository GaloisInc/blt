#include <cmath>
#include <iostream>
#include <map>
#include <memory>
#include <stdexcept>

#include "boost/rational.hpp"

#include "types.h"
#include "expr.h"

using std::map;
using std::ostream;
using std::shared_ptr;
using boost::rational;

/// Construct a variable term
const Expr ExprVar(int idx) {
    return Expr(idx);
}

/// Construct a constant term
const Expr ExprConst(rational<int64> c) {
    Expr r;
    r.set_const(c);
    return r;
}

/// Return coefficient of given variable. If the variable isn't
//  represented in the expression, return 0.
rational<int64> Expr::operator[](int idx) const {
    auto it = coeff_map_.find(idx);
    if (it != coeff_map_.end()) {
        return it->second;
    } else {
        return 0;
    }
}

void Expr::set_coeff(int idx, rational<int64> c) {
    coeff_map_[idx] = c;
}

void Expr::set_const(rational<int64> c) {
    const_ = c;
}


// #############################################################################
// Arithmetic on Expr

Expr & Expr::operator+=(const Expr &rhs) {
    for (auto it  = rhs.coeff_map_.begin();
              it != rhs.coeff_map_.end();
              ++it) {
        coeff_map_[it->first] += it->second;
        indices_.insert(it->first);
    }
    const_ += rhs.const_;
    return *this;
}

Expr & Expr::operator*=(const rational<int64> &s) {
    for (auto it = coeff_map_.begin();
              it != coeff_map_.end();
              ++it) {
        coeff_map_[it->first] *= s;
        indices_.insert(it->first);
    }
    const_ *= s;
    return *this;
}

Expr & Expr::operator-=(const Expr &t) {
    return *this += (t * -1);
}

ostream & operator<<(ostream& os, const Expr &e) {
    std::set<int> idxs = e.indices();
    rational<int64> c;
    os << "<";
    for (auto it = idxs.begin(); it != idxs.end(); ++it) {
        c = e[*it];
        os << (it == idxs.begin() ? "" : " ") << c << "*" << "x" << *it;
    }
    os << ">";
    // print constant term if present
    if (e.const_ != 0) {
        os << " + " << e.const_;
    }
    return os;
}

////////////////////////////////////////////////////////////////
// Related functions

const Expr Expr::operator+(const Expr &rhs) const {
    Expr r = *this;
    r += rhs;
    return r;
}

const Expr Expr::operator-(const Expr &rhs) const {
    Expr r = *this;
    r -= rhs;
    return r;
}

const Expr operator*(const Expr &e, rational<int64> s) {
    Expr r = e;
    r *= s;
    return r;
}

const Expr operator*(rational<int64> s, const Expr &e) {
    Expr r = e;
    r *= s;
    return r;
}

/// Round all coefficients of the Expr to the nearest integer
const Expr round(const Expr &e) {
    Expr r = e;
    std::set<int> idxs = r.indices();
    double t;
    for (auto it = idxs.begin(); it != idxs.end(); ++it) {
        if (r[*it].denominator() == 1)
            continue;
        t = boost::rational_cast<double>(r[*it]);
        r.set_coeff(*it, static_cast<int64>(std::round(t)));
    }
    // round the constant term
    t = boost::rational_cast<double>(r.get_const());
    r.set_const(static_cast<int64>(std::round(t)));
    return r;
}
