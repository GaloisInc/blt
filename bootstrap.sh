#!/bin/bash

############################################################################
#
# Run this script to install all dependencies for BLT in
# $PREFIX (or $PWD/env by default).
#
# The script produces another shell script with configuration meant to
# be sourced before building BLT.
#
# Options:
#
#   * PREFIX    -- prefix to install dependency libs and include files to
#   * GET_YICES -- download and install Yices when this variable set to any
#                  non-empty string AND YICES_URL is specified.
#
#
# Prerequisites: (Ubuntu)
#
#     $ apt-get install git curl build-essential -y
#
# Prerequisites: (Mac OSX)
#
#     * OSX Developer Tools
#
############################################################################

set -eu

# prefix to install 3rd party libraries to
PREFIX="${PREFIX:-$PWD/env}"
OS=$(uname)

### 3rd party libraries, versions and locations
#
# both VERSION and URL must be specified together in the calling environment,
# or the defaults below will be used

NTL_VER=${NTL_VER:-"9.4.0"}
NTL_URL=${NTL_URL:-"http://www.shoup.net/ntl/ntl-${NTL_VER}.tar.gz"}
NTL_OPTS=${NTL_OPTS:-"WIZARD=off"}

GLPK_VER=${GLPK_VER:-"4.55"}
GLPK_URL=${GLPK_URL:-"http://ftp.gnu.org/gnu/glpk/glpk-${GLPK_VER}.tar.gz"}
GLPK_OPTS=${GLPK_OPTS:-"--disable-shared"}

BOOST_VER=${BOOST_VER:-"1.59.0"}
BOOST_VER_UND=$(echo $BOOST_VER | sed 's/\./_/g')
BOOST_URL=${BOOST_URL:-"http://sourceforge.net/projects/boost/files/boost/${BOOST_VER}/boost_${BOOST_VER_UND}.tar.gz/download"}

YICES_VER=${YICES_VER:-"2.4.0"}
YICES_URL=${YICES_URL:-""}
YICES_OPTS=${YICES_OPTS:-""}


### Print the environment ###

printf "%s" "Environment:\n"
printf "%s" "------------\n"
printf "%15s%s\n" "PREFIX:"    "${PREFIX:-}"
printf "%15s%s\n" "CPPFLAGS:"  "${CPPFLAGS:-}"
printf "%15s%s\n" "LDFLAGS:"   "${LDFLAGS:-}"
printf "%15s%s\n" "GET_YICES:" "${GET_YICES:-}"
printf "%15s%s\n" "OS:" "${OS}"


### Auxilliary Functions

# $1 = filepath to test
assert_exists() {
    if [ -f "$1" ]; then
        return
    else
        echo "build error: artifact $1 not found" 2>&1
        exit 1
    fi
}

# $1 = dirpath to test
assert_dir() {
    if [ -d "$1" ]; then
        return
    else
        echo "build error: directory $1 not found" 2>&1
        exit 1
    fi
}


### create PREFIX directories

mkdir -p "$PREFIX"
mkdir -p "$PREFIX/include"
mkdir -p "$PREFIX/lib"
mkdir -p "$PREFIX/src"

# place tarballs here
pushd "$PREFIX/src"

# Build & install ntl
# We require objects are compiled with -fPIC so they can be included in
# haskell .so libs.
if [ -f "$PREFIX/lib/libntl.a" ]; then
    echo "Skipping NTL (found $PREFIX/lib/libntl.a)"
else
    echo "Getting NTL... $NTL_URL"
    curl -L "$NTL_URL" | tar xz
    pushd "ntl-${NTL_VER}"/src
    # set -e explicitly b/c configure does not return proper exit status codes
    bash -e configure PREFIX="$PREFIX" CXXFLAGS="-O2 -fPIC" $NTL_OPTS
    make && make install
    popd
fi

# Build & install glpk
# We require objects are compiled with -fPIC so they can be included in
# haskell .so libs
if [ -f "$PREFIX/lib/libglpk.a" ]; then
    echo "Skipping GLPK (found $PREFIX/lib/libglpk.a)"
else
    echo "Getting GLPK... $GLPK_URL"
    curl "${GLPK_URL}" | tar xz
    pushd "glpk-${GLPK_VER}"
    CFLAGS="-fPIC" ./configure --prefix="$PREFIX" $GLPK_OPTS
    make && make install
    popd
fi

# Install boost
if [ -d $PREFIX/include/boost ]; then
    echo "Skipping boost (found $PREFIX/include/boost)"
else
    echo "Getting boost... $BOOST_URL"
    curl -L $BOOST_URL | tar xz
    cp -r "boost_${BOOST_VER_UND}/boost" "$PREFIX/include/boost"
fi

# Install Yices
if [ -n "${GET_YICES:-}" ]; then
    if [ -f "$PREFIX/lib/libyices.so"    -o \
         -f "$PREFIX/lib/libyices.dylib" -o \
         "$OS" == "Unknown" ]; then
        echo "Skipping Yices..."
        unset USE_YICES
    else
        if [ -z "$YICES_URL" ]; then
            echo "No YICES_URL set, skipping Yices..."
        else
            echo "Getting Yices... $YICES_URL"
            curl "${YICES_URL}" | tar xz
            pushd "yices-${YICES_VER}"
            ./configure --prefix="$PREFIX" $YICES_OPTS
            make
            make install
            if [ "$OS" == "Darwin" ]; then
                YICES_LIB="libyices.dylib"
            elif [ "$OS" == "Linux" ]; then
                YICES_LIB="libyices.so"
            fi
            popd
        fi
    fi
fi

popd

assert_exists "$PREFIX/lib/libglpk.a"
assert_exists "$PREFIX/lib/libntl.a"
[ -z "${GET_YICES:-}" ] || assert_exists "$PREFIX/lib/$YICES_LIB"
assert_dir    "$PREFIX/include/boost"

# populate setup_env.sh script
cat << EOF > setup_env.sh
export CPPFLAGS="-isystem $PREFIX/include/boost -I$PREFIX/include"
export LDFLAGS="-L$PREFIX/lib"
export YICES_LIB_PATH="$PREFIX/lib"
EOF

# Message to the user

echo "Dependencies are installed."
echo ""
echo "Invoke the following to build and test the BLT library:"
echo ""
echo "    % source ./setup_env.sh"
echo "    % cd libblt"
echo "    % make && make test"
echo ""
echo "If you have built with Yices support, you may want to run the test"
echo "suite with Yices support enabled:"
echo ""
echo "    % make test-yices"
echo ""
