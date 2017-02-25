AR=xiar
ARFLAGS=-qnoipo -lib rsv
ifdef USE_MPI
ifdef NDEBUG
FC=mpiifort -ilp64
else # DEBUG
FC=mpiifort -ilp64 -trace # -tcollect
endif # ?NDEBUG
FORFLAGS=-DUSE_INTEL -DUSE_X200 -DUSE_MPI -i8 -qopenmp -fexceptions -standard-semantics
else # no MPI
FC=ifort
FORFLAGS=-DUSE_INTEL -DUSE_X200 -i8 -qopenmp -fexceptions -standard-semantics
endif # ?USE_MPI
#-prof-gen=srcpos,globdata,threadsafe
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -xHost #-xMIC-AVX512
DBGFLAGS=-DNDEBUG -qopt-report=5 -traceback -diag-disable=10397
FPUFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0 -xHost #-xMIC-AVX512
DBGFLAGS=-g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
FPUFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt #-fp-model strict -assume ieee_fpe_flags -fp-stack-check
endif # ?NDEBUG
LIBFLAGS=-DUSE_MKL -DMKL_DIRECT_CALL -I. -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include -threads
LDFLAGS=-L. -ljstrat -lqxblas -lvn -L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm -ldl -lmemkind
