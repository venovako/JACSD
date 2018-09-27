SHELL=/bin/bash
ARCH=$(shell uname)
RM=rm -rfv
AR=ar
ifndef LP64
ILP64=1
endif # !LP64
ifndef WP
WP=10
endif # !WP
ARFLAGS=rsv
CPUFLAGS=-DUSE_GNU -DUSE_X64
FORFLAGS=-cpp $(CPUFLAGS) -ffree-line-length-none -fopenmp -fstack-arrays
ifdef USE_MPI
FC=mpifort
FORFLAGS += -DUSE_MPI
else # no MPI
ifeq ($(ARCH),Darwin)
FC=gfortran-8
else # Linux
FC=gfortran
endif # ?Darwin
endif # ?USE_MPI
ifdef ILP64
FORFLAGS += -fdefault-integer-8
endif # ILP64
ifeq ($(ARCH),Darwin)
CC=clang
C11FLAGS=$(CPUFLAGS) -std=gnu17 -pthread
else # Linux
CC=gcc
C11FLAGS=$(CPUFLAGS) -std=gnu11 -fopenmp
endif # ?Darwin
ifdef ILP64
C11FLAGS += -DVN_INTEGER_KIND=8
endif # ILP64
ifdef NDEBUG
ifeq ($(ARCH),Darwin)
OPTFLAGS=-O$(NDEBUG) -march=native -Wa,-q -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
OPTCFLAGS=-O$(NDEBUG) -march=native -integrated-as
DBGCFLAGS=-DNDEBUG
else # Linux
OPTFLAGS=-O$(NDEBUG) -march=native -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
OPTCFLAGS=-O$(NDEBUG) -march=native -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
DBGCFLAGS=-DNDEBUG -fopt-info-optimized-vec
endif # ?Darwin
DBGFLAGS=-DNDEBUG -fopt-info-optimized-vec -pedantic -Wall -Wextra -Wno-compare-reals -Warray-temporaries -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
FPUFLAGS=-ffp-contract=fast
FPUCFLAGS=-ffp-contract=fast -fno-math-errno
OPTFLAGS += -DMKL_DIRECT_CALL
else # DEBUG
ifeq ($(ARCH),Darwin)
OPTFLAGS=-Og -march=native -Wa,-q
OPTCFLAGS=-Og -march=native -integrated-as
else # Linux
OPTFLAGS=-Og -march=native
OPTCFLAGS=-Og -march=native
endif # ?Darwin
DBGFLAGS=-g -fcheck=all -finit-local-zero -finit-real=snan -finit-derived -pedantic -Wall -Wextra -Wno-compare-reals -Warray-temporaries -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
DBGCFLAGS=-g -ftrapv
FPUFLAGS=-ffp-contract=fast -ffpe-trap=invalid,zero,overflow
FPUCFLAGS=-ffp-contract=fast
endif # ?NDEBUG
LIBFLAGS=-DUSE_MKL -I.
ifdef ILP64
LIBFLAGS += -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include
else # LP64
LIBFLAGS += -I${MKLROOT}/include
endif # ?ILP64
ifeq ($(ARCH),Darwin)
LDFLAGS=-L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -L${MKLROOT}/../compiler/lib -Wl,-rpath,${MKLROOT}/../compiler/lib
ifdef ILP64
LDFLAGS += -lmkl_intel_ilp64
else # LP64
LDFLAGS += -lmkl_intel_lp64
endif # ?ILP64
LDFLAGS += -lmkl_core -lmkl_intel_thread -liomp5 -lpthread -lm -ldl
else # Linux
LIBFLAGS += -D_GNU_SOURCE
LDFLAGS=-L${MKLROOT}/lib/intel64 -Wl,-rpath=${MKLROOT}/lib/intel64 -L${MKLROOT}/../compiler/lib/intel64 -Wl,-rpath=${MKLROOT}/../compiler/lib/intel64 -Wl,--no-as-needed
ifdef ILP64
LDFLAGS += -lmkl_gf_ilp64
else # LP64
LDFLAGS += -lmkl_gf_lp64
endif # ?ILP64
LDFLAGS += -lmkl_core -lmkl_gnu_thread -lpthread -lm -ldl
endif # ?Darwin
FCFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFLAGS)
CFLAGS=$(OPTCFLAGS) $(DBGCFLAGS) $(LIBFLAGS) $(C11FLAGS) $(FPUCFLAGS)
