#ifndef CODES_H
#define CODES_H

/* Function return codes */
enum return_codes {
    BLT_STATUS_OK          = 0,  /* call succeeded */
    BLT_STATUS_ERROR       = 1,  /* call failed */
    BLT_STATUS_LP_FAIL     = 2,  /* call failed due to LP failure */
    BLT_STATUS_INPUT_ERROR = 3   /* call failed because input was bad */
};

/* SAT return codes */
enum sat_codes {
    BLT_CHECK_SAT     = 10,      /* input or current state is SAT */
    BLT_CHECK_UNSAT   = 11,      /* input or current state is UNSAT */
    BLT_CHECK_UNKNOWN = 12
};

/* Linear programming return codes */
/* TODO: ellaborate these based on backend codes */
enum lp_codes {
    LP_OK    = 20,
    LP_ERROR = 21
};

#endif
