ifndef WP
WP=16
endif # !WP
SHELL=/bin/bash
ARCH=$(shell uname)
RM=rm -rfv
AR=xiar
ARFLAGS=-qnoipo -lib rsv
CPUFLAGS=-DUSE_INTEL -DUSE_X200
FORFLAGS=$(CPUFLAGS) -i8 -fexceptions -standard-semantics
C11FLAGS=$(CPUFLAGS) -DVN_INTEGER_KIND=8 -std=c11 -fexceptions
CC=icc
FC=ifort
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -xHost
OPTFFLAGS=$(OPTFLAGS) -DMKL_DIRECT_CALL
OPTCFLAGS=$(OPTFLAGS)
DBGFLAGS=-DNDEBUG -qopt-report=5 -traceback -diag-disable=10397
DBGFFLAGS=$(DBGFLAGS)
DBGCFLAGS=$(DBGFLAGS) -w3 -diag-disable=1572,2547
FPUFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt -fimf-precision=high -fimf-use-svml=true
FPUFFLAGS=$(FPUFLAGS)
FPUCFLAGS=$(FPUFLAGS)
else # DEBUG
OPTFLAGS=-O0 -xHost
OPTFFLAGS=$(OPTFLAGS)
OPTCFLAGS=$(OPTFLAGS)
DBGFLAGS=-g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -traceback -diag-disable=10397
DBGFFLAGS=$(DBGFLAGS) -debug-parameters all -check all -warn all
DBGCFLAGS=$(DBGFLAGS) -check=stack,uninit -w3 -diag-disable=1572,2547
FPUFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt -fimf-precision=high
FPUFFLAGS=$(FPUFLAGS) #-fp-model strict -assume ieee_fpe_flags -fp-stack-check
FPUCFLAGS=$(FPUFLAGS) #-fp-model strict -fp-stack-check
endif # ?NDEBUG
LIBFLAGS=-D_GNU_SOURCE -DUSE_MKL -DMKL_ILP64 -I. -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include -qopenmp
LDFLAGS=-L${MKLROOT}/lib/intel64 -Wl,-rpath=${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_intel_thread -lmkl_core -lpthread -lm -ldl -lmemkind
FFLAGS=$(OPTFFLAGS) $(DBGFFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFFLAGS)
CFLAGS=$(OPTCFLAGS) $(DBGCFLAGS) $(LIBFLAGS) $(C11FLAGS) $(FPUCFLAGS)
