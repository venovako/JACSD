AR=ar
ARFLAGS=rsv
ifdef USE_MPI
FC=mpifort
FORFLAGS=-DUSE_GNU -DUSE_X64 -DUSE_MPI -fdefault-integer-8 -fopenmp -fstack-arrays
else # no MPI
ifeq ($(ARCH),Darwin)
FC=gfortran-6
else # Linux
FC=gfortran
endif # ?Darwin
FORFLAGS=-DUSE_GNU -DUSE_X64 -fdefault-integer-8 -fopenmp -fstack-arrays
endif # ?USE_MPI
ifdef NDEBUG
ifeq ($(ARCH),Darwin)
OPTFLAGS=-O$(NDEBUG) -march=native -Wa,-q -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
else # Linux
OPTFLAGS=-O$(NDEBUG) -march=native -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
endif # ?Darwin
DBGFLAGS=-DNDEBUG -fopt-info-optimized-vec
FPUFLAGS=
else # DEBUG
ifeq ($(ARCH),Darwin)
OPTFLAGS=-O0 -march=native -Wa,-q
else # Linux
OPTFLAGS=-O0 -march=native
endif # ?Darwin
DBGFLAGS=-g -fcheck=all -finit-local-zero -finit-real=snan
FPUFLAGS=-ffpe-trap=invalid,zero,overflow
endif # ?NDEBUG
LIBFLAGS=-I.
LDFLAGS=-L. -ljstrat -lqxblas -lvn -L$(HOME)/OpenBLAS/lib -lopenblas_omp #-ltmglib -llapack -lrefblas
