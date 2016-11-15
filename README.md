![BLT logo](doc/blt_logo.png)

# BLT - A Library for Lattice Based Integer Linear Programming  {#mainpage}

## Introduction

BLT is a C/C++ library for solving certain integer linear programming (ILP)
problems using techniques that come from the theory of lattices. It is
complementary to many existing, traditional ILP solvers in that there are
problems it solves very well and very quickly which traditional solvers do not
(and vice-versa).

For a theoretical discussion, see our conference [paper] from the 2015 SMT
Workshop.

This project also provides a higher level set of Haskell bindings to BLT
through a Haskell package called "blt". See "Installation and Usage" below for
details.

Finally, there is a stand-alone ILP solver included called `run_blt`. See
the "Stand-alone Solver" section.

The current version of BLT is 1.0.0, dated 2016-11-15.


## Documentation

Documentation (HTML) for the C/C++ library and the Haskell binding API can
generated from source using [doxygen](http://www.stack.nl/~dimitri/doxygen/)
(for the source code documentation of the C/C++ library) and by using `cabal
haddock` to generate the Haskell source documentation.

The C/C++ library API is documented in the file `blt.h`. For the Haskell API,
see the BLT.Bindings module.


## Installation and Usage

The C/C++ library may be built using standard Unix build system tools,
`gcc` or `clang` and `make`. The Haskell bindings may be built using standard
Haskell ecosystems tools such as either [cabal] or [stack].


### Dependencies

BLT depends on the following 3rd party software:

  * C++ compiler supporting the c++0x standard
  * [GLPK]  (>= 4.53 && <= 4.55) -- provides real-valued linear programming
            (note v4.56 & 4.57 are known to be incompatible with BLT)
  * [NTL]   (>= 6.2.1 && <7.0 or >= 9.0.0) -- provides lattice reduction
            algorithms
  * [Boost] C++ headers (>= 1.55.0) -- provides matrices and vectors, rational
            numbers, and the unit test framework. Only a subset of the boost
            headers are needed

and optionally,

  * [GHC]   for Haskell support (>= 7.6)
  * [Yices] SMT solver (>= 2.3.0, see the section "Sound Mode" for more details).


A bootstrapping script `bootstrap.sh` is provided and may be used to
automatically download and install most of these (not including the compiler).


### Building & Installing 'libblt'

'libblt' is the C/C++ library implementing the core of BLT. The source code
and build system for libblt is located in the 'libblt' directory. The library
is built using GNU make as follows. First, set the environment variables
listed in the `config.mk.example` file and rename it to `config.mk`. The
variables should be set so that the dependencies listed above (GLPK, etc..)
can be found by the C++ compiler. This can also be done in the shell, at the
command line, or in the `Makefile` itself. Then invoke:

    % cd libblt
    % make

It is recommended to test your build by running:

    % make test
    % make longtest

The second command may take from several minutes to several hours depending on
how you have chosen to configure the build. 

Testing with Yices support enabled is done using:

    % make test-yices
    % make longteset-yices

You can expect the 'longtest-yices' target to take more than 1 hour.


The optional `PREFIX` environment variable specifies the installation prefix.
BLT may now be installed by invoking:

    % make install

or

    % PREFIX=/usr/local make install

or similar.

For example, if the dependency libraries are installed in $HOME/local/lib and
the Boost headers are installed in $HOME/local/include, then the following
commands will build and install BLT to `/usr/local/{lib,bin}`.

    % export LDFLAGS=-L$HOME/local/lib CPPFLAGS=-I$HOME/local/include
    % make
    % PREFIX=/usr/local make install


#### Troubleshooting

If you have installed GLPK and NTL into a local prefix (as opposed to a
system-wide location like `/usr/local/lib`) it is necessary to tell the
Makefile in `libblt` where to find them. This can be done by
explicitly setting the environment variables `LDFLAGS` and `CPPFLAGS`.

For example, if your local prefix is `$HOME/local`, one might use:

    % export BLT_LIB_DEST=$HOME/local/lib
    % export LDFLAGS=-L$HOME/local/lib
    % export CPPFLAGS=-I$HOME/local/include


### Sound Mode

Including the Yices SMT solver and enabling it at runtime allows BLT to
operate as a sound and complete decision procedure for bounded integer linear
programming problems. However, it comes at a large performance cost. In dense,
high dimensional problems the cost has been observed as high as a factor of
1000 in total runtime.


### Stand-alone Solver

The included program `run_blt` is a standalone ILP (actually, Bounded ILP)
solver using the BLT library. It takes input in the form described below, and
returns a SAT or UNSAT decision, optionally a model, and some limited runtime
information.


#### INPUT

The expected input is a single file, `<name>.bilp` where `<name>` is any valid
filename prefix. The file is expected to contain a system of two-sided
inequalities representing a matrix system

    L <= A*x <= U,

where L, U are integer column vectors, A is an integer matrix, and x is a
column vector of integer unknowns (variables). `run_blt` decides the
satisfiability of such a system of inequalities. The input `.bilp` file should
be in the form:

    l0 <= a11 a12 ... a2n <= u0
    .      .                 .
    .      .                 .
    .      .                 .
    lm <= am1 am2 ... amn <= um

with all the a's, l's and u's being integer constants. In this case, 'x' is a
vector of 'n' unknowns. In this format, each line represents a single
two-sided inequality and excess whitespace is ignored. There are several
example `.bilp` files in the `test/data` directory for reference.

`run_blt` supports a few optional flags:

 * `-m` print out a satisfying model if the problem is SAT
 * `-v` print out version and compile-time information
 * `-h` print out program usage

#### OUTPUT

The output consists of a single line on `stdout` of the form:

    PROBLEM <name> RUNTIME <r> niter <i> npru <p> rc <s> check <c>

where:

    <name> |  provided on the command line
       <r> |  runtime in seconds
       <i> |  number of search iterations
       <p> |  number of layers pruned during search
       <s> |  return status, e.g. OK, STATUS_INPUT_ERROR, STATUS_LP_ERROR
       <c> |  decision: SAT or UNSAT

If the `-m` flag is given and the problem is SAT, then an additional line is
printed that includes the model:

    PROBLEM <name> MODEL <vector>

where:

      <name> |  provided on the command line
    <vector> |  vector of integers representing the solution 'x'



## Haskell Bindings to BLT

Building the bindings requires the Haskell compiler GHC version 7.6 or
greater and the cabal-install tool, preferably version 1.18 or greater, but
older versions may also work (untested).

To install the bindings to your user packages:

    % cabal install --extra-lib-dirs=<path to deps>

where <path to deps> is a directory containing the dependency libraries
`libglpk.*`, and `libntl.*`. The flag is not neccesary if GLPK and NTL are
installed system wide. See also the sections "Dependencies" and "Troubleshooting" 

The cabal configure process automatically builds and installs (locally) the
C/C++ library contained in 'libblt', so it's important that the compiler and
linker environment flags are set before running `cabal install`.  The BLT
library can be used in any Haskell project by adding `blt` to your build
dependencies.

See the Haskell documentation listed in the "Documentation" section for
details about the API.



[paper]: http://smt2015.csl.sri.com/wp-content/uploads/2015/06/2015-Hendrix-Jones-Bounded-Integer-Linear-Constraint-Solving-via-Lattice-Search.pdf
[GHC]: https://www.haskell.org/ghc/
[GLPK]: https://www.gnu.org/software/glpk/
[NTL]: http://www.shoup.net/ntl/
[Boost]: http://www.boost.org/
[Cabal]: https://www.haskell.org/cabal/
[stack]: http://docs.haskellstack.org/en/stable/README
[Yices]: http://yices.csl.sri.com/
