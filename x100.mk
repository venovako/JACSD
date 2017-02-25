AR=xiar
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
FORFLAGS=-DUSE_INTEL -DUSE_X100 -DUSE_MPI -mmic -i8 -qopenmp -fexceptions -standard-semantics
else # no MPI
FC=ifort
FORFLAGS=-DUSE_INTEL -DUSE_X100 -mmic -i8 -qopenmp -fexceptions -standard-semantics
endif # ?USE_MPI
ifdef NDEBUG
OPTFLAGS=-fast #-O$(NDEBUG)
DBGFLAGS=-DNDEBUG -qopt-report=5 -traceback -diag-disable=10397
FPUFLAGS=-fma #-fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0
DBGFLAGS=-g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
FPUFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt #-fp-model strict -assume ieee_fpe_flags -fp-stack-check
endif # ?NDEBUG
LIBFLAGS=-DUSE_MKL -DMKL_DIRECT_CALL -I. -I${MKLROOT}/include/mic/ilp64 -I${MKLROOT}/include -threads
LDFLAGS=-L. -ljstrat -lqxblas -lvn -L${MKLROOT}/lib/mic -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm -ldl
