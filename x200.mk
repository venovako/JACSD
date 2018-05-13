SHELL=/bin/bash
ARCH=$(shell uname)
RM=rm -rfv
AR=xiar
ifndef WP
WP=16
endif # !WP
ARFLAGS=-qnoipo -lib rsv
ifdef USE_MPI
ifdef NDEBUG
FC=mpiifort -ilp64
else # DEBUG
FC=mpiifort -ilp64 -trace # -tcollect
endif # ?NDEBUG
FORFLAGS=-DUSE_INTEL -DUSE_X200 -DUSE_MPI -i8 -fexceptions -standard-semantics
else # no MPI
FC=ifort
FORFLAGS=-DUSE_INTEL -DUSE_X200 -i8 -fexceptions -standard-semantics
endif # ?USE_MPI
#-prof-gen=srcpos,globdata,threadsafe
CC=icc
C11FLAGS=-DUSE_INTEL -DUSE_X200 -DVN_INTEGER_KIND=8 -std=c11 -fexceptions
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -xHost -DMKL_DIRECT_CALL #-xMIC-AVX512
OPTCFLAGS=-O$(NDEBUG) -xHost #-xMIC-AVX512
DBGFLAGS=-DNDEBUG -qopt-report=5 -traceback -diag-disable=10397
DBGCFLAGS=-DNDEBUG -qopt-report=5 -traceback -w3 -diag-disable=1572,2547,10397
FPUFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
FPUCFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0 -xHost #-xMIC-AVX512
OPTCFLAGS=-O0 -xHost #-xMIC-AVX512
DBGFLAGS=-g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
DBGCFLAGS=-g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -check=stack,uninit -traceback -w3 -diag-disable=1572,2547,10397
FPUFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt #-fp-model strict -assume ieee_fpe_flags -fp-stack-check
FPUCFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt #-fp-model strict -fp-stack-check
endif # ?NDEBUG
LIBFLAGS=-D_GNU_SOURCE -DUSE_MKL -I. -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include -qopenmp
LDFLAGS=-L${MKLROOT}/lib/intel64 -Wl,-rpath=${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm -ldl -lmemkind
FCFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFLAGS)
CFLAGS=$(OPTCFLAGS) $(DBGCFLAGS) $(LIBFLAGS) $(C11FLAGS) $(FPUCFLAGS)
