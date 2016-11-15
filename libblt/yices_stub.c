#include "dlfcn.h"
#include "stdio.h"
#include "stdlib.h"

#include "yices_stub.h"


/* Pointers to functions from the Yices API, they are initialized by
 * 'load_yices'
 */
yices_add_t                    yices_add = NULL;
yices_neg_t                    yices_neg = NULL;
yices_mul_t                    yices_mul = NULL;
yices_and_t                    yices_and = NULL;
yices_arith_leq_atom_t         yices_arith_leq_atom = NULL;
yices_assert_formula_t         yices_assert_formula = NULL;
yices_check_context_t          yices_check_context = NULL;
yices_exit_t                   yices_exit = NULL;
yices_free_context_t           yices_free_context = NULL;
yices_free_model_t             yices_free_model = NULL;
yices_get_double_value_t       yices_get_double_value = NULL;
yices_get_model_t              yices_get_model = NULL;
yices_init_t                   yices_init = NULL;
yices_int64_t                  yices_int64 = NULL;
yices_new_context_t            yices_new_context = NULL;
yices_new_uninterpreted_term_t yices_new_uninterpreted_term = NULL;
yices_pp_model_t               yices_pp_model = NULL;
yices_pp_term_t                yices_pp_term = NULL;
yices_print_error_t            yices_print_error = NULL;
yices_rational64_t             yices_rational64 = NULL;
yices_real_type_t              yices_real_type = NULL;
yices_set_term_name_t          yices_set_term_name = NULL;
yices_true_t                   yices_true = NULL;
yices_zero_t                   yices_zero = NULL;

/* libdl handle for libyices */
static void *h;


/** A reference counter indicating whether yices shared library has been
 *  loaded or not and by how many clients.
 * 
 * load_yices_c == -1:  loading yices failed for some reason
 * load_yices_c ==  0:  yices is not currently loaded
 * load_yices_c >=  1:   yices has been successfully loaded this number of
 *                       times
 */
int load_yices_c = 0;


/**
 * Load Yices shared library using 'dlopen' and resolve all the function
 * symbols declared above.
 *
 * @return: upon load error return 1, and upon resolution error, return 2. If otherwise
 * successfull, return 0.
 */
int load_yices() {

#ifdef DEBUG
    printf("[yices_stub]: attempting to load %s\n", LIB_YICES_NAME);
#endif

    (void) dlerror();  /* clear error state */
    h = dlopen(LIB_YICES_NAME, RTLD_NOW);

    const char *err;
    if ((err = dlerror()) != NULL) {
        fprintf(stderr, "error loading: %s\ndlerror() reports: %s\n",
                LIB_YICES_NAME, err);
        load_yices_c = -1;
        return 1;
    }

    /* load symbols */
    int rc = 0;
    yices_add                    = (yices_add_t) load_sym("yices_add", &rc);
    yices_neg                    = (yices_neg_t) load_sym("yices_neg", &rc);
    yices_mul                    = (yices_mul_t) load_sym("yices_mul", &rc);
    yices_and                    = (yices_and_t) load_sym("yices_and", &rc);
    yices_arith_leq_atom         = (yices_arith_leq_atom_t) load_sym("yices_arith_leq_atom", &rc);
    yices_assert_formula         = (yices_assert_formula_t) load_sym("yices_assert_formula", &rc);
    yices_check_context          = (yices_check_context_t) load_sym("yices_check_context", &rc);
    yices_exit                   = (yices_exit_t) load_sym("yices_exit", &rc);
    yices_free_context           = (yices_free_context_t) load_sym("yices_free_context", &rc);
    yices_free_model             = (yices_free_model_t) load_sym("yices_free_model", &rc);
    yices_get_double_value       = (yices_get_double_value_t) load_sym("yices_get_double_value", &rc);
    yices_get_model              = (yices_get_model_t) load_sym("yices_get_model", &rc);
    yices_init                   = (yices_init_t) load_sym("yices_init", &rc);
    yices_int64                  = (yices_int64_t) load_sym("yices_int64", &rc);
    yices_new_context            = (yices_new_context_t) load_sym("yices_new_context", &rc);
    yices_new_uninterpreted_term = (yices_new_uninterpreted_term_t) load_sym("yices_new_uninterpreted_term", &rc);
    yices_pp_model               = (yices_pp_model_t) load_sym("yices_pp_model", &rc);
    yices_pp_term                = (yices_pp_term_t) load_sym("yices_pp_term", &rc);
    yices_print_error            = (yices_print_error_t) load_sym("yices_print_error", &rc);
    yices_rational64             = (yices_rational64_t) load_sym("yices_rational64", &rc);
    yices_real_type              = (yices_real_type_t) load_sym("yices_real_type", &rc);
    yices_set_term_name          = (yices_set_term_name_t) load_sym("yices_set_term_name", &rc);
    yices_true                   = (yices_true_t) load_sym("yices_true", &rc);
    yices_zero                   = (yices_zero_t) load_sym("yices_zero", &rc);

    if (rc == 0)
        load_yices_c++;
    else
        load_yices_c = -1;

    return rc;
}


/**
 * Load symbol by 'name' using the global handle 'h'. Update 'rc' if there is
 * an error.
 */
void* load_sym(const char *name, int *rc) {
    void *p = dlsym(h, name);
    if (p == NULL) {
        *rc = 2;
    }
    return p;
}


/**
 * If loaded, unload the yices library and decrement the reference counter.
 * Return non-zero value iff 'dlclose' fails. Otherwise return 0.
 */
int unload_yices() {
    if (load_yices_c > 0) {
        load_yices_c--;
        return dlclose(h);
    } else {
        return 0;
    }
}
