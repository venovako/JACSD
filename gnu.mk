SHELL=/bin/bash
ARCH=$(shell uname)
RM=rm -rfv
AR=ar
ifndef WP
WP=16
endif # !WP
ARFLAGS=rsv
ifdef USE_MPI
FC=mpifort
FORFLAGS=-cpp -DUSE_GNU -DUSE_X64 -DUSE_MPI -fdefault-integer-8 -fopenmp -fstack-arrays -std=f2008ts
else # no MPI
ifeq ($(ARCH),Darwin)
FC=gfortran-6
else # Linux
FC=gfortran
endif # ?Darwin
FORFLAGS=-cpp -DUSE_GNU -DUSE_X64 -fdefault-integer-8 -fopenmp -fstack-arrays
endif # ?USE_MPI
ifeq ($(ARCH),Darwin)
CC=clang #gcc-6
C11FLAGS=-DUSE_CLANG -DUSE_X64 -DVN_INTEGER_KIND=8 -std=gnu11
else # Linux
CC=gcc
C11FLAGS=-DUSE_GNU -DUSE_X64 -DVN_INTEGER_KIND=8 -std=gnu11
endif # ?Darwin
ifdef NDEBUG
ifeq ($(ARCH),Darwin)
OPTFLAGS=-O$(NDEBUG) -march=native -Wa,-q -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
OPTCFLAGS=-O$(NDEBUG) -march=native -integrated-as
DBGFLAGS=-DNDEBUG -fopt-info-optimized-vec -pedantic -Wall -Wextra -Wno-compare-reals -Warray-temporaries -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
DBGCFLAGS=-DNDEBUG
FPUFLAGS=
FPUCFLAGS=-ffp-contract=on
else # Linux
OPTFLAGS=-O$(NDEBUG) -march=native -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
OPTCFLAGS=-O$(NDEBUG) -march=native -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
DBGFLAGS=-DNDEBUG -fopt-info-optimized-vec -pedantic -Wall -Wextra -Wno-compare-reals -Warray-temporaries -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
DBGCFLAGS=-DNDEBUG -fopt-info-optimized-vec
FPUFLAGS=
FPUCFLAGS=-ffp-contract=fast -fno-math-errno
endif # ?Darwin
else # DEBUG
ifeq ($(ARCH),Darwin)
OPTFLAGS=-O0 -march=native -Wa,-q
OPTCFLAGS=-O0 -march=native -integrated-as
DBGFLAGS=-g -fcheck=all -finit-local-zero -finit-real=snan -pedantic -Wall -Wextra -Wno-compare-reals -Warray-temporaries -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
DBGCFLAGS=-g -ftrapv
FPUFLAGS=-ffpe-trap=invalid,zero,overflow
FPUCFLAGS=-ffp-contract=on
else # Linux
OPTFLAGS=-O0 -march=native
OPTCFLAGS=-Og -march=native
DBGFLAGS=-g -fcheck=all -finit-local-zero -finit-real=snan -finit-derived -pedantic -Wall -Wextra -Wno-compare-reals -Warray-temporaries -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
DBGCFLAGS=-g -ftrapv
FPUFLAGS=-ffpe-trap=invalid,zero,overflow
FPUCFLAGS=-ffp-contract=fast
endif # ?Darwin
endif # ?NDEBUG
