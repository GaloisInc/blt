/*! \file blt.h - This file defines the BLT C-API
 *
 * The API is primarilty used in the stand-alone solver 'run_blt' and to
 * define high level haskell bindings to libblt (part of the 'blt' Haskell
 * package).
 *
 */

#ifndef BLT_H
#define BLT_H

#include <stdint.h>


/*! __BLT_VERSION should match the version in 'blt.cabal' */
#define __BLT_VERSION "1.0.0"

/*! 'ENVIRONMENT_XX' defines the machine word size */
#if INTPTR_MAX == INT32_MAX
    #define ENVIRONMENT_32
#elif INTPTR_MAX == INT64_MAX
    #define ENVIRONMENT_64
#else
    #error "Platform is not 32 or 64-bit."
#endif


/*! Type synonym for the standard 64-bit integer */
typedef int64_t int64;

/*! 'CContext', 'CExpr', and 'CSolveState' are type wrappers for C++ objects
 *  that are maninpulated by the C-API.  */
typedef struct Context_internal_*   CContext;    /* a problem context */
typedef struct Expr_internal_*      CExpr;       /* an expression objext */
typedef struct SolvState_internal_* CSolvState;  /* a solver state bookmark */

#ifdef __cplusplus
extern "C" {
#endif


/*! Initialize a new problem context.
 *
 * If 'enable_yices' is non-zero, then the Yices library will be loaded
 * dynamically as needed. The client is responsible for calling 'blt_free' when
 * the context is no longer needed. */
CContext    blt_init      ( int enable_yices );


/*! Create a fresh variable with the given name.
 *
 * A symbolic expression "CExpr" is returned. If the name is already taken in
 * the given context, NULL is returned. */
CExpr       blt_var       ( CContext, const char* );


/*! Create a new rational constant term 'n'/'d' */
CExpr       blt_const     ( CContext, int64 n, int64 d);


/*! Add the two given expressions */
CExpr       blt_add       ( CContext, const CExpr, const CExpr );


/*! Multiply expression by the rational number 'n'/'d'.
 *  If d == 0, then NULL is returned. */
CExpr       blt_smul      ( CContext, int64 n, int64 d, CExpr expr);


/*! Convert the coefficients of the expression to a fixed point
 * representation.
 *
 * This is done by first scaling all cofficients by the given integer and
 * then rounding them to the nearest integer. */
CExpr       blt_tof       ( CContext, int64 s, CExpr expr);


/*! Add the assumption l <= 'expr' <= u to the assumption stack.
 *
 * In the assumption, l = 'n1'/'d1' and u = 'n2'/'d2'. A basic satisfiabilty
 * check is done to ensure that l <= u.
 *
 * Return values:
 *
 *     STATUS_OK
 *     STATUS_ERROR (out of memory)
 *     CHECK_UNSAT  (assumption is trivially UNSAT)
 */
int         blt_assume    ( CContext, int64 n1, int64 d1, const CExpr expr,
                            int64 n2, int64 d2);


/*! Check the satisfiability of the current set of assumptions.
 *
 * Returns values:
 *
 *     BLT_SAT    -- problem is SAT
 *     BLT_UNSAT  -- problem is UNSAT
 *     STATUS_INPUT_ERROR
 *                -- input data is not well formed
 */
int         blt_check     ( CContext );


/*! Get the model assigned value of the variable 'name' into 'x0'.
 *
 * Return values:
 *
 *     STATUS_OK
 *     STATUS_INPUT_ERROR  (a value for 'name' hasn't been assigned)
 */
int         blt_model     ( const CContext, const char* name, int64* x0);


/*! Return a solver state that the client can use to backtrack. */
CSolvState  blt_save      ( CContext );


/*! Perform simple backtracking on the assumption stack.
 *
 * Backtracking pops assumptions from the context's assumption stack. New
 * variables created after a save-point will still be in scope after
 * backtracking.
 */
int         blt_backtrack ( CContext, CSolvState );


/*! Free memory associated with the given context.
 *
 * It is the client's responsibility to eventually call 'blt_free' after
 * each invocation of 'blt_init' to properly free memory used in building
 * expressions and assumptions.
 */
void        blt_free      ( CContext );

#ifdef __cplusplus
}
#endif

#endif
