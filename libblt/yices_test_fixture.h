#pragma once

#include <string>
#include <cstring>
#include "boost/test/included/unit_test.hpp"

#include "yices_stub.h"


using std::string;
using std::to_string;
using boost::unit_test::framework::master_test_suite;

static int test_yices;

/// Global Boost UTF Fixture for enabling/disabling the testing of Yices
/// based on command line parameters to the test runner.
///
/// Enable Yices testing using:
///
///     $ test_runner -y
///
struct CmdLine {
    CmdLine() {
        if (cmd_line_has("-y")) {
            BOOST_TEST_MESSAGE("[yices_test_fixture]: "
                               "Testing Yices support is enabled");
            int rc = load_yices();
            if (rc == 0) {
                test_yices = 1;
                BOOST_TEST_MESSAGE("[yices_test_fixture]: "
                                   "Yices was loaded successfully");
            } else {
                string emsg("[yices_test_fixture]: Yices support was requested, "
                            "but it failed to load (rc = ");
                emsg += to_string(rc) + ")";
                throw std::runtime_error(emsg);
            }
        } else {
            test_yices = 0;
        }
    }

    ~CmdLine() { };

    /// Check if a string appears as a command line argument
    static int cmd_line_has(const string s) {
        int argc = master_test_suite().argc;
        if (argc <= 1)
            return 0;
        for (size_t i = 1; i < argc; i++) {
            if (string(master_test_suite().argv[i]) == s)
                return 1;
        }
        return 0;
    }
};
