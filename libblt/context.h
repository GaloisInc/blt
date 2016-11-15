#pragma once

#include <map>
#include <memory>
#include <vector>

#include "boost/numeric/ublas/matrix.hpp"
#include "boost/rational.hpp"

#include "expr.h"
#include "types.h"


/// Atom represents a double inequality lower <= linear expr <= upper. This
/// object is immutable.
class Atom {
    boost::rational<int64> lower_;
    boost::rational<int64> upper_;
    Expr  expr_;

public:
    Atom() : lower_(0), upper_(0), expr_() { };
    Atom(boost::rational<int64> l, Expr e, boost::rational<int64> u) : lower_(l), upper_(u), expr_(e) { };

    boost::rational<int64> lower() const { return lower_; };
    boost::rational<int64> upper() const { return upper_; };
    Expr  expr()  const { return expr_;  };
};

/// Immutable object representing the solver state.
class SolvState {
    size_t pos_; // position in the assumption list

public:
    /// Construct a solver state at assumption position `a`.
    explicit SolvState(size_t a) : pos_(a) { };

    size_t pos() const { return pos_; };
};

/// Context for the problem being solved; It records variables and expressions
/// and provides an API for enable DPLL-style search.
///
/// Context objects are only created using the default initializer. They are
/// updated using `mkVar`, `assume`, `check`, and `backtrack`.
///
class Context {
    /// at each index, the name of the corresponding variable
    std::vector<std::string>      names;
    /// Bitmask of "active" variables, those occuring in a current assumption.
    /// Updated on `mkVar`, `assume`, and `backtrack`.
    std::vector<bool>             mask;
    /// reverse name lookup
    std::map<std::string, int>   indices;
    /// stack of row assumptions
    std::vector<Atom>            assumptions;
    /// place to store a model
    std::map<std::string, int64> model;
    /// indicate if yices support is to be enabled
    bool enable_yices_;

    ///< set `mask` to reflect the current set of assumptions
    void update_mask();

    rbmatrix compute_lattice();
    rbmatrix compute_constr();

public:

    /// Construct a default 'Context' where Yices is disabled
    Context() : enable_yices_(false) { };
    /// Construct a 'Context' using flag to enable/disable Yices
    explicit Context(int flag) : enable_yices_(flag) { };

    /// Return the number of active variables: popcount(mask)
    int nvars() const {
        return count(mask.begin(), mask.end(), true);
    }

    int nassumpts() const { return assumptions.size(); };

    Expr mkVar(const std::string &);
    Expr mkConst(boost::rational<int64>);
    int getVarIdx(const std::string &, int &) const;
    int getVarName(size_t, std::string &) const;
    bool varActive(const std::string &) const;

    int assume(boost::rational<int64>, Expr, boost::rational<int64>);
    SolvState save_state() const;
    bool valid_state(SolvState) const;
    int backtrack(SolvState);

    int get_model(const std::string &, int64*) const;
    int get_model(size_t, int64*) const;

    int check();

    bool is_yices_enabled() { return enable_yices_; };
};


/// Convert a given rational system into an integral one.
int convert_to_integral( boost::numeric::ublas::matrix<boost::rational<int64>>
                       , boost::numeric::ublas::matrix<boost::rational<int64>>
                       , boost::numeric::ublas::matrix<int64> &
                       , boost::numeric::ublas::matrix<int64> & );

/// Stretch a lattice and constraints into a hypercube
int stretch_to_cube( boost::numeric::ublas::matrix<int64> &
                   , boost::numeric::ublas::matrix<int64> &);
