# Copyright 2015 Galois, Inc.
# Benjamin Jones <bjones@galois.com>

#
# ~~~ IMPORTANT ~~~
#
# The user is in charge of setting the following variables
# in such a way that the external dependencies (GLPK, NTL, Boost)
# can be found:
#
# CXXFLAGS - C++ compiler flags
# CPPFLAGS - preprocessor flags (e.g., -I<dir>)
# LDFLAGS  - linker flags       (e.g., -L<dir>)
#
# On some systems, the ntl and glpk libraries may be compiled with gmp support
# enabled. In this case your LDFLAGS should contain "-lgmp".
#
# Defaults for these flags are set in the config.mk.example file based
# on values for the "build options".
#
# The user should copy the included 'config.mk.example' file to 'config.mk'
# and edit the values there before invoking make.
#
#
# Code Coverage Report:
#
# To produce a code coverage report, add '--coverage' to CXXFLAGS, use '-O0'
# for OPT_FLAGS, and '-g' for DEBUG_FLAGS. Then run 'make && make test' to
# produce raw code coverage data (*.gcno, *.gcda). Finally, a report can be
# generated using the 'gcovr' tool, e.g.
#
#     $ gcovr --html -r $PWD -o coverage_20151002.html
#

-include config.mk

# standard toolchain defaults
CC      ?= gcc
CXX     ?= gcc
AR      ?= ar
INSTALL ?= install

# other optional tools
CPP_CHECK ?= cppcheck
CTAGS     ?= ctags


####################################################
# Platform Specific Configuration

# We assume a POSIX system with the 'uname' command
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    CFLAGS += -DLINUX
    CXXFLAGS += -DLINUX
    LIBS += -ldl
endif
ifeq ($(UNAME_S),Darwin)
    CFLAGS += -DOSX
    CXXFLAGS += -DOSX
endif


####################################################
# Main Targets


all: libblt.a run_blt latcon2bilp

# target prints out the important environment variables
print:
	@echo ""
	@echo "BLT Build Environment:"
	@echo "---------------------"
	@echo "CFLAGS      = $(CFLAGS)"
	@echo "CXXFLAGS    = $(CXXFLAGS)"
	@echo "CPPFLAGS    = $(CPPFLAGS)"
	@echo "LDFLAGS     = $(LDFLAGS)"
	@echo "LIBS        = $(LIBS)"
	@echo ""
	@echo "DEBUG_FLAGS = $(DEBUG_FLAGS)"
	@echo "OPT_FLAGS   = $(OPT_FLAGS)"
	@echo ""
	@echo "PREFIX      = $(PREFIX)"
	@echo ""

# sources
CXXSOURCES = $(shell echo *.cc)
CSOURCES   = $(shell echo *.c)

BLT_OBJS =          \
	blt.o           \
	closest.o       \
	context.o       \
	expr.o          \
	glpk_wrapper.o  \
	ntl_wrapper.o   \
	parser.o        \
	util.o          \
	yices_wrapper.o \
	yices_stub.o

EXT_LIBS = -lglpk -lntl -lgmp

EXES = run_blt latcon2bilp


libblt.a: $(BLT_OBJS)
	@echo "AR $@"
	@$(AR) crs $@ $(BLT_OBJS)


$(EXES): CXXFLAGS += -std=c++0x
$(EXES): LDFLAGS += -L.
$(EXES): %: %.o libblt.a
	@echo "LD $@"
	@$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $@.o -lblt $(EXT_LIBS) $(LIBS)


install: libblt.a blt.h run_blt latcon2bilp
	@echo Installing to PREFIX: $(PREFIX)
	mkdir -p $(PREFIX)/bin
	mkdir -p $(PREFIX)/include
	mkdir -p $(PREFIX)/lib
	$(INSTALL) libblt.a $(PREFIX)/lib
	$(INSTALL) -m 0755 run_blt $(PREFIX)/bin
	$(INSTALL) -m 0755 latcon2bilp $(PREFIX)/bin
	$(INSTALL) -m 0644 blt.h $(PREFIX)/include


uninstall:
	echo "To uninstall manually, do this:"
	echo ""
	echo "    $(RM) $(PREFIX)/lib/libblt.*"
	echo "    $(RM) $(PREFIX)/include/blt.h"
	echo "    $(RM) $(PREFIX)/bin/run_blt"
	echo "    $(RM) $(PREFIX)/bin/latcon2bilp"
	echo ""


####################################################
# Source targets: generate deps first, then compile

# Build C++ files
%.o: CXXFLAGS += -std=c++0x -fPIC
%.o: %.cc
	@echo "CXX $<"
	@set -e; $(CXX) $(CXXFLAGS) -M $(CPPFLAGS) -c $< \
	        | sed 's/\($*\)\.o[ :]*/\1.o \1.dep : /g' > $(@:.o=.dep); \
	        [ -s $(@:.o=.dep) ] || rm -f $(@:.o=.dep)
	@$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $<

# Build C files
%.o: CFLAGS += -std=c99 -fPIC
%.o: %.c
	@echo "CC $<"
	@set -e; $(CC) $(CFLAGS) -M $(CPPFLAGS) -c $< \
	        | sed 's/\($*\)\.o[ :]*/\1.o \1.dep : /g' > $(@:.o=.dep); \
	        [ -s $(@:.o=.dep) ] || rm -f $(@:.o=.dep)
	@$(CC) $(CFLAGS) $(CPPFLAGS) -c $<

-include $(CXXSOURCES:.cc=.dep)
-include $(CSOURCES:.c=.dep)


####################################################
# Test Programs


# test executables
TEST_EXES=       \
	test_ntl     \
	test_glpk    \
	test_closest \
	test_context \
	test_expr    \
	test_blt     \
	test_parser  \
	test_yices   \
	test_compare_glpk_yices

# tests that take more than 1 minute
TEST_LONG_EXES=test_regression

TEST_ALL=$(TEST_EXES) $(TEST_LONG_EXES)

$(TEST_ALL): CXXFLAGS += -std=c++0x
$(TEST_ALL): LDFLAGS += -L.
$(TEST_ALL): %: %.o libblt.a
	@echo "LD $@"
	@$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $@.o -lblt $(EXT_LIBS) $(LIBS)


####################################################
# Utilities


clean:
	@echo "Cleaning.."
	@rm -f *.o *.dep *.gcov *.gcno *.gcda libblt.*
	@rm -f run_blt latcon2bilp $(DEP_FILES)
	@rm -f $(TEST_ALL) $(YICES_TESTS)

test: $(TEST_EXES)
	@echo "Testing.."
	@./run_tests.sh $(TEST_EXES)

test-yices: $(TEST_EXES)
	@echo "Testing Yices support.."
	@NOT_DYLD_LIBRARY_PATH=$(PREFIX)/lib  \
		NOT_LD_LIBRARY_PATH=$(PREFIX)/lib \
		./run_tests.sh -y $(TEST_EXES)

longtest: $(TEST_LONG_EXES)
	@echo "Long Testing.."
	@./run_tests.sh $(TEST_LONG_EXES)

longtest-yices: $(TEST_LONG_EXES)
	@echo "Long Testing Yices support.."
	@NOT_DYLD_LIBRARY_PATH=$(PREFIX)/lib  \
		NOT_LD_LIBRARY_PATH=$(PREFIX)/lib \
		./run_tests.sh -y $(TEST_LONG_EXES)

check: $(CXXSOURCES)
	@echo "Linting.."
	@$(CPP_CHECK) --enable=all --std=c++11 $(CXXSOURCES)

tags: $(CSOURCES) $(CXXSOURCES)
	@echo "TAGS"
	@$(CTAGS) $^

# all the phonies
.PHONY: check test clean uninstall install all print
