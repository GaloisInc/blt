#ifndef TEST_BLT_H
#define TEST_BLT_H

/* simple two dimensional test problem */
int test_2d_problem();

/*
 * UNSAT problems taken from the Yices test suite; results for the
 * non-simple version are incorrect due to overflow on 32-bit platforms.
 */
int test_tight_parallelogram();
int test_tight_parallelogram_simple();

#endif
