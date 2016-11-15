/// \file blt.cc
///
/// C-style API wrappers for the BLT C++ library
///

#include <vector>

#include "boost/rational.hpp"

#include "blt.h"
#include "context.h"
#include "types.h"

#include "debug.h"

using boost::rational;
using std::vector;
using std::map;


/// Global pools that store references to expressions and solver states
static map<CContext, vector<Expr*> > e_pool;
static map<CContext, vector<SolvState*> > s_pool;

CContext blt_init(int enable_yices) {
    Context* cp = new Context(enable_yices);
    return reinterpret_cast<CContext>(cp);
}

CExpr blt_var(CContext ctx, const char* name) {
    Context* cp = reinterpret_cast<Context*>(ctx);
    Expr* ep = new Expr(cp->mkVar(name));
    e_pool[ctx].push_back(ep);
    return reinterpret_cast<CExpr>(ep);
}

CExpr blt_const(CContext ctx, int64 n, int64 d) {
    Context* cp = reinterpret_cast<Context*>(ctx);
    Expr* ep = new Expr(cp->mkConst(rational<int64>(n, d)));
    e_pool[ctx].push_back(ep);
    return reinterpret_cast<CExpr>(ep);
}

CExpr blt_add(CContext ctx, const CExpr lhs, const CExpr rhs) {
    assert(lhs != nullptr && "null lhs expression passed to blt_add");
    assert(rhs != nullptr && "null rhs expression passed to blt_add");
    Expr* lhsp = reinterpret_cast<Expr*>(lhs);
    Expr* rhsp = reinterpret_cast<Expr*>(rhs);
    Expr* ep = new Expr(*lhsp + *rhsp);
    e_pool[ctx].push_back(ep);
    return reinterpret_cast<CExpr>(ep);
}

CExpr blt_smul(CContext ctx, int64 n, int64 d, CExpr ce) {
    if (d != 0) {
        rational<int64> r(n, d);
        assert(ce != nullptr && "null expression passed to blt_smul");
        Expr* e = reinterpret_cast<Expr*>(ce);
        Expr* ep = new Expr(r * (*e));
        e_pool[ctx].push_back(ep);
        return reinterpret_cast<CExpr>(ep);
    } else {
        return nullptr;
    }
}

CExpr blt_tof(CContext ctx, int64 s, CExpr ce) {
    assert(ce != nullptr && "null expression passed to blt_tof");
    Expr* e = reinterpret_cast<Expr*>(ce);
    Expr* ep = new Expr(round(s * (*e)));
    e_pool[ctx].push_back(ep);
    TRACE("expr " << *e << " tof " << *ep);
    return reinterpret_cast<CExpr>(ep);
}

int blt_assume(CContext ctx, int64 ln, int64 ld, const CExpr ce, int64 un, int64 ud) {
    rational<int64> l(ln, ld);
    rational<int64> u(un, ud);
    assert(ce != nullptr && "null expression passed to blt_assume");
    Context* cp = reinterpret_cast<Context*>(ctx);
    Expr* e = reinterpret_cast<Expr*>(ce);
    return cp->assume(l, *e, u);
}

int blt_check(CContext ctx) {
    Context* cp = reinterpret_cast<Context*>(ctx);
    return cp->check();
}

int blt_model(const CContext ctx, const char* name, int64* val) {
    Context* cp = reinterpret_cast<Context*>(ctx);
    std::string sname(name);
    return cp->get_model(sname, val);
}

CSolvState blt_save(CContext ctx) {
    Context* cp = reinterpret_cast<Context*>(ctx);
    return reinterpret_cast<CSolvState>(new SolvState(cp->save_state()));
}

int blt_backtrack(CContext ctx, CSolvState sav) {
    Context* cp = reinterpret_cast<Context*>(ctx);
    SolvState* ss = reinterpret_cast<SolvState*>(sav);
    return cp->backtrack(*ss);
}


/// Free memory associated with the given Context
void blt_free(CContext ctx) {
    for (auto it = e_pool[ctx].begin(); it != e_pool[ctx].end(); ++it)
        delete *it;
    e_pool[ctx].clear();

    for (auto it = s_pool[ctx].begin(); it != s_pool[ctx].end(); ++it)
        delete *it;
    s_pool[ctx].clear();

    Context* cp = reinterpret_cast<Context*>(ctx);
    delete cp;
}

