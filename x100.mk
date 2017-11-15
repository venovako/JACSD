SHELL=/bin/bash
ARCH=$(shell uname)
RM=rm -rfv
AR=xiar
ifndef WP
WP=16
endif # !WP
ifdef NDEBUG
ARFLAGS=-lib rsv
else # DEBUG
ARFLAGS=-qnoipo -lib rsv
endif # ?NDEBUG
ifdef USE_MPI
ifdef NDEBUG
FC=mpiifort -ilp64
else # DEBUG
FC=mpiifort -ilp64 -trace # -tcollect
endif # ?NDEBUG
FORFLAGS=-DUSE_INTEL -DUSE_X100 -DUSE_MPI -mmic -i8 -fexceptions -standard-semantics
else # no MPI
FC=ifort
FORFLAGS=-DUSE_INTEL -DUSE_X100 -mmic -i8 -fexceptions -standard-semantics
endif # ?USE_MPI
CC=icc
C11FLAGS=-DUSE_INTEL -DUSE_X100 -DVN_INTEGER_KIND=8 -std=c11 -mmic -fexceptions
ifdef NDEBUG
OPTFLAGS=-fast #-O$(NDEBUG)
OPTCFLAGS=-fast #-O$(NDEBUG)
DBGFLAGS=-DNDEBUG -qopt-report=5 -traceback -diag-disable=10397
DBGCFLAGS=-DNDEBUG -qopt-report=5 -traceback -diag-disable=1572,2547,10397
FPUFLAGS=-fma #-fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
FPUCFLAGS=-fma #-fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0
OPTCFLAGS=-O0
DBGFLAGS=-g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
DBGCFLAGS=-g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -check=stack,uninit -w3 -traceback -diag-disable=1572,2547,10397
FPUFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt #-fp-model strict -assume ieee_fpe_flags -fp-stack-check
FPUCFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt #-fp-model strict -fp-stack-check
endif # ?NDEBUG
LIBFLAGS=-DUSE_MKL -DMKL_DIRECT_CALL -I. -I${MKLROOT}/include/mic/ilp64 -I${MKLROOT}/include -qopenmp
LDFLAGS=-L${MKLROOT}/lib/mic -Wl,-rpath=${MKLROOT}/lib/mic -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm -ldl
FCFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFLAGS)
CFLAGS=$(OPTCFLAGS) $(DBGCFLAGS) $(LIBFLAGS) $(C11FLAGS) $(FPUCFLAGS)
