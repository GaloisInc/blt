#pragma once

#include <map>
#include <memory>
#include <set>

#include "boost/rational.hpp"

#include "types.h"

/// Expr represents an integer (affine) linear combination of variables, i.e.
/// an expression of the form a1 x1 + ... + an xn + C, where the ai are
/// integers, the xi are variables, and C is an integer constant.
class Expr {
    /// Coeffifient map. Keys: variable indices, Values: variable coefficients
    std::map<int, boost::rational<int64>> coeff_map_;
    /// Constant coefficient is kept separately
    boost::rational<int64> const_;
    /// Convenience: keeps a list of keys in coeff_map_ so we don't have to
    /// compute it repeatedly.
    std::set<int> indices_;

private:
    /// Apply rounding to all coefficients in the expression
    friend const Expr round(const Expr &);
    void set_coeff(int, boost::rational<int64>);
    void set_const(boost::rational<int64>);

public:
    /// Represents the empty expression, i.e. 0
    Expr () : const_(0) { };

    /// Basic copy constructor
    Expr (const Expr & e) : coeff_map_(e.coeff_map_),
                            const_(e.const_),
                            indices_(e.indices_) { };

    /// Single variable constructor representing the variable with index `idx`.
    /// Note that automatic coercion from int is disallowed.
    explicit Expr (int idx) {
        coeff_map_[idx] = 1;
        const_ = 0;
        indices_.insert(idx);
    }

    /// Return a set of variable indices which occur in this Expr.
    const std::set<int> & indices() const { return indices_; }
    /// Return number of terms represented, including constant (if non-zero)
    int nsummands() const { return coeff_map_.size() + (const_ == 0 ? 0 : 1); }
    bool is_zero() const { return coeff_map_.empty() && const_ == 0; }

    /// Getters for the coefficients
    boost::rational<int64> operator[](int) const;
    boost::rational<int64> get_const() const { return const_; };

    /// Arithmetic operations
    const Expr operator+(const Expr &) const;
    const Expr operator-(const Expr &) const;
    Expr & operator+=(const Expr &);
    Expr & operator-=(const Expr &);
    Expr & operator*=(const boost::rational<int64> &);

    /// Auxilliary constructors
    friend const Expr ExprVar(int);
    friend const Expr ExprConst(boost::rational<int64>);

    friend std::ostream & operator<<(std::ostream &, const Expr &);
};

const Expr ExprVar(int);
const Expr ExprConst(boost::rational<int64>);
const Expr round(const Expr &);
const Expr operator*(const Expr &, boost::rational<int64>);
const Expr operator*(boost::rational<int64>, const Expr &);
