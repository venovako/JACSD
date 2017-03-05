SHELL=/bin/bash
ARCH=$(shell uname)
RM=rm -rfv
AR=ar
ifndef WP
WP=16
endif # !WP
ARFLAGS=rsv
FC=gfortran
CC=gcc
FORFLAGS=-cpp -DUSE_GNU -DUSE_PWR8 -fdefault-integer-8 -fopenmp -fstack-arrays
C11FLAGS=-DUSE_GNU -DUSE_PWR8 -DVN_INTEGER_KIND=8 -std=gnu11
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -mcpu=power8 -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
OPTCFLAGS=-O$(NDEBUG) -mcpu=power8 -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
DBGFLAGS=-DNDEBUG -fopt-info-optimized-vec -pedantic -Wall -Wextra -Wno-compare-reals -Warray-temporaries -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
DBGCFLAGS=-DNDEBUG -fopt-info-optimized-vec
FPUFLAGS=-ffp-contract=fast
FPUCFLAGS=-ffp-contract=fast -fno-math-errno
else # DEBUG
OPTFLAGS=-Og -mcpu=power8
OPTCFLAGS=-Og -mcpu=power8
DBGFLAGS=-g -fcheck=all -finit-local-zero -finit-real=snan -finit-derived -pedantic -Wall -Wextra -Wno-compare-reals -Warray-temporaries -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
DBGCFLAGS=-g
FPUFLAGS=-ffpe-trap=invalid,zero,overflow
FPUCFLAGS=-ffp-contract=fast
endif # ?NDEBUG
