#ifndef YICES_STUB_H
#define YICES_STUB_H

/*
 * yices_stub.h
 *
 * This header contains function pointer declarations for a subset of the
 * Yices API. The pointers are initialized using dlopen/dlsym at runtime.
 */

#include "stdio.h"
#include "stdint.h"


#if defined(__APPLE__)
#   define LIB_YICES_NAME "libyices.dylib"
#elif defined(__linux__)
#   define LIB_YICES_NAME "libyices.so"
#else
#   error "platform not supported"
#endif


#ifdef __cplusplus
extern "C" {
#endif

/* Yices specific types that occur in the API */
typedef int32_t             term_t;
typedef int32_t             type_t;
typedef struct context_s    context_t;
typedef struct model_s      model_t;
typedef struct param_s      param_t;
typedef struct ctx_config_s ctx_config_t;
typedef enum smt_status {
  STATUS_IDLE,
  STATUS_SEARCHING,
  STATUS_UNKNOWN,
  STATUS_SAT,
  STATUS_UNSAT,
  STATUS_INTERRUPTED,
  STATUS_ERROR
} smt_status_t;


/* Declare function pointer types for a subset of the Yices API that is used
 * by yices_wrapper.cc, the pointers are declared and initialized in
 * 'yices_stub.c'
 */

typedef  term_t       (*yices_add_t)                     (term_t t1, term_t t2);
typedef  term_t       (*yices_neg_t)                     (term_t t1);
typedef  term_t       (*yices_mul_t)                     (term_t t1, term_t t2);
typedef  term_t       (*yices_and_t)                     (uint32_t n, term_t arg[]);
typedef  term_t       (*yices_arith_leq_atom_t)          (term_t t1, term_t t2);
typedef  int32_t      (*yices_assert_formula_t)          (context_t *ctx, term_t t);
typedef  smt_status_t (*yices_check_context_t)           (context_t *ctx, const param_t *params);
typedef  void         (*yices_exit_t)                    (void);
typedef  void         (*yices_free_context_t)            (context_t *ctx);
typedef  void         (*yices_free_model_t)              (model_t *mdl);
typedef  int32_t      (*yices_get_double_value_t)        (model_t *mdl, term_t t, double *val);
typedef  model_t*     (*yices_get_model_t)               (context_t *ctx, int32_t keep_subst);
typedef  void         (*yices_init_t)                    (void);
typedef  term_t       (*yices_int64_t)                   (int64_t val);
typedef  context_t*   (*yices_new_context_t)             (const ctx_config_t *config);
typedef  term_t       (*yices_new_uninterpreted_term_t)  (type_t tau);
typedef  int32_t      (*yices_pp_model_t)                (FILE *f, model_t *mdl, uint32_t width, uint32_t height, uint32_t offset);
typedef  int32_t      (*yices_pp_term_t)                 (FILE *f, term_t t, uint32_t width, uint32_t height, uint32_t offset);
typedef  int32_t      (*yices_print_error_t)             (FILE *f);
typedef  term_t       (*yices_rational64_t)              (int64_t num, uint64_t den);
typedef  type_t       (*yices_real_type_t)               (void);
typedef  int32_t      (*yices_set_term_name_t)           (term_t t, const char *name);
typedef  term_t       (*yices_true_t)                    (void);
typedef  term_t       (*yices_zero_t)                    (void);

/* Extern'd pointers to functions from the Yices API */
extern yices_add_t                    yices_add;
extern yices_neg_t                    yices_neg;
extern yices_mul_t                    yices_mul;
extern yices_and_t                    yices_and;
extern yices_arith_leq_atom_t         yices_arith_leq_atom;
extern yices_assert_formula_t         yices_assert_formula;
extern yices_check_context_t          yices_check_context;
extern yices_exit_t                   yices_exit;
extern yices_free_context_t           yices_free_context;
extern yices_free_model_t             yices_free_model;
extern yices_get_double_value_t       yices_get_double_value;
extern yices_get_model_t              yices_get_model;
extern yices_init_t                   yices_init;
extern yices_int64_t                  yices_int64;
extern yices_new_context_t            yices_new_context;
extern yices_new_uninterpreted_term_t yices_new_uninterpreted_term;
extern yices_pp_model_t               yices_pp_model;
extern yices_pp_term_t                yices_pp_term;
extern yices_print_error_t            yices_print_error;
extern yices_rational64_t             yices_rational64;
extern yices_real_type_t              yices_real_type;
extern yices_set_term_name_t          yices_set_term_name;
extern yices_true_t                   yices_true;
extern yices_zero_t                   yices_zero;

/* Reference counter for the dynamic library handle */
extern int load_yices_c;

/* Load Yices shared library and resolve function symbols */
int   load_yices();
int   unload_yices();
void* load_sym(const char*, int*);


#ifdef __cplusplus
}
#endif

#endif
