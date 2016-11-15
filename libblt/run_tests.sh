#!/bin/bash
#
# Usage: run_tests [-y] <TESTS>
#
#          -y       (optional) enable yices support in tests
#          <TESTS>  list of test executables to run
#
# If -y is given, the user must specify the location of libyices.{so,dylib}
# in the environment variable YICES_LIB_PATH, e.g.
#
# % YICES_LIB_PATH=/my/path/to/yices ./run_tests -y test_foobar
#

# comsume the optional -y argument which enables Yices support
enable_yices=""
if [ "$1" == "-y" ]; then
    enable_yices=$1
    shift
fi


# detect platform
UNAME_S=$(uname -s)
if [ $UNAME_S == "Linux" ]; then
    SYSTEM_LD_PATH="LD_LIBRARY_PATH"
elif [ $UNAME_S == "Darwin" ]; then
    SYSTEM_LD_PATH="DYLD_LIBRARY_PATH"
else
    printf "$0 Error: platform $UNAME_S is not supported\n"
    exit 1
fi


for t in $@; do
    printf "\n"
    printf "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
    printf "~~  Running test: $t"
    if [ -n "$enable_yices" ]; then printf " (Yices enabled)"; fi
    printf "\n\n"
    if [ -n "$enable_yices" ]; then
        OLD_SYSTEM_LD_PATH=$(eval "echo \$$SYSTEM_LD_PATH")
        env $SYSTEM_LD_PATH="$YICES_LIB_PATH:$OLD_SYSTEM_LD_PATH" \
          ./$t --report_level=detailed --log_level=message -y
    else
        ./$t --report_level=detailed --log_level=message
    fi
    rc=$?
    if [ $rc -ne 0 ]; then
        printf "ERROR: test $t failed!\n"
        exit 1
    fi
done

printf "All tests passed!\n"
