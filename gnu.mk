SHELL=/bin/bash
ifndef WP
WP=10
endif # !WP
ifdef NDEBUG
DEBUG=
else # DEBUG
DEBUG=g
endif # ?NDEBUG
ARCH=$(shell uname)
RM=rm -rfv
AR=ar
ARFLAGS=rsv
CPUFLAGS=-DUSE_GNU -DUSE_X64 -fexceptions
ifdef PROFILE
CPUFLAGS += -DVN_PROFILE=$(PROFILE) -fPIC -fno-inline -fno-omit-frame-pointer -finstrument-functions -rdynamic
endif # PROFILE
FORFLAGS=-cpp $(CPUFLAGS) -fdefault-integer-8 -ffree-line-length-none -fopenmp -fstack-arrays
C11FLAGS=$(CPUFLAGS) #-std=gnu17
ifeq ($(ARCH),Darwin)
CC=clang
FC=gfortran-8
C11FLAGS += -pthread
else # Linux
CC=gcc
FC=gfortran
C11FLAGS += -fopenmp
endif # ?Darwin
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -march=native
DBGFLAGS=-DNDEBUG
ifeq ($(ARCH),Darwin)
OPTFFLAGS=$(OPTFLAGS) -Wa,-q -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
OPTCFLAGS=$(OPTFLAGS) -integrated-as
DBGFFLAGS=$(DBGFLAGS) -fopt-info-optimized-vec -pedantic -Wall -Wextra -Wno-compare-reals -Warray-temporaries -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
DBGCFLAGS=$(DBGFLAGS)
else # Linux
OPTFLAGS += -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
OPTFFLAGS=$(OPTFLAGS)
OPTCFLAGS=$(OPTFLAGS)
DBGFLAGS += -fopt-info-optimized-vec
DBGFFLAGS=$(DBGFLAGS) -pedantic -Wall -Wextra -Wno-compare-reals -Warray-temporaries -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
DBGCFLAGS=$(DBGFLAGS)
endif # ?Darwin
FPUFLAGS=-ffp-contract=fast
FPUFFLAGS=$(FPUFLAGS)
FPUCFLAGS=$(FPUFLAGS) -fno-math-errno
OPTFFLAGS += -DMKL_DIRECT_CALL
else # DEBUG
OPTFLAGS=-O$(DEBUG) -march=native
ifeq ($(ARCH),Darwin)
OPTFFLAGS=$(OPTFLAGS) -Wa,-q
OPTCFLAGS=$(OPTFLAGS) -integrated-as
else # Linux
OPTFFLAGS=$(OPTFLAGS)
OPTCFLAGS=$(OPTFLAGS)
endif # ?Darwin
DBGFLAGS=-$(DEBUG)
DBGFFLAGS=$(DBGFLAGS) -fcheck=all -finit-local-zero -finit-real=snan -finit-derived -pedantic -Wall -Wextra -Wno-compare-reals -Warray-temporaries -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
DBGCFLAGS=$(DBGFLAGS) -ftrapv
FPUFLAGS=-ffp-contract=fast
FPUFFLAGS=$(FPUFLAGS) -ffpe-trap=invalid,zero,overflow
FPUCFLAGS=$(FPUFLAGS)
endif # ?NDEBUG
LIBFLAGS=-DUSE_MKL -DMKL_ILP64 -I. -I../vn -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include
LDFLAGS=-L.. -lvn$(PROFILE)
ifeq ($(ARCH),Darwin)
LDFLAGS += -L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -L${MKLROOT}/../compiler/lib -Wl,-rpath,${MKLROOT}/../compiler/lib -lmkl_intel_ilp64 -lmkl_intel_thread -lmkl_core -liomp5
else # Linux
LIBFLAGS += -D_GNU_SOURCE
LDFLAGS += -L${MKLROOT}/lib/intel64 -Wl,-rpath=${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_gf_ilp64 -lmkl_gnu_thread -lmkl_core -lgomp
endif # ?Darwin
LDFLAGS += -lpthread -lm -ldl
FFLAGS=$(OPTFFLAGS) $(DBGFFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFFLAGS)
CFLAGS=$(OPTCFLAGS) $(DBGCFLAGS) $(LIBFLAGS) $(C11FLAGS) $(FPUCFLAGS)
