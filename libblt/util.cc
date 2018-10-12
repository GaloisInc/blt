// Utility functions for BLT
//
// Author: Benjamin Jones <bjones@galois.com>
// Copyright 2014 Galois, Inc.

#include <cmath>

#include "util.h"


// ===----------------------------------------------------------------
//    Arithmetic
// ===----------------------------------------------------------------

/// Return true if `a` and `b` are within epsilon times the larger of the two.
/// From Knuth's TAoCP.
bool approximatelyEqual(double a, double b, double epsilon) {
    return fabs(a - b) <= ((fabs(a) < fabs(b) ? fabs(b) : fabs(a)) * epsilon);
}

/// Compute a < b up to epsilon tolerance. Technically, returns true if `b-a`
/// is greater than epsilon times the larger of the two.
/// From Knuth's TAoCP.
bool definitelyLess(double a, double b, double epsilon) {
    return (b - a) > ((fabs(a) < fabs(b) ? fabs(b) : fabs(a)) * epsilon);
}

/// Disjunction of 'approximatelyEqual' and 'definitelyLess'.
bool approximatelyLE(double a, double b, double epsilon) {
    return definitelyLess(a, b, epsilon) || approximatelyEqual(a, b, epsilon);
}
