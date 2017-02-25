AR=ar
ARFLAGS=rsv
FC=gfortran
FORFLAGS=-DUSE_GNU -DUSE_PWR8 -fdefault-integer-8 -fopenmp -fstack-arrays
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -mcpu=power8 -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
DBGFLAGS=-DNDEBUG -fopt-info-optimized-vec
FPUFLAGS=
else # DEBUG
OPTFLAGS=-Og -mcpu=power8
DBGFLAGS=-g -fcheck=all -finit-local-zero -finit-real=snan
FPUFLAGS=-ffpe-trap=invalid,zero,overflow
endif # ?NDEBUG
LIBFLAGS=-I. -DUSE_ATLAS
ifeq ($(USE_ATLAS),pt)
LDFLAGS=-L. -ljstrat -lqxblas -L$(HOME)/atlas/lib -lptlapack -llapack -lptf77blas -lptcblas -latlas
else # sequential ATLAS
LDFLAGS=-L. -ljstrat -lqxblas -L$(HOME)/atlas/lib -lptlapack -llapack -lf77blas -lcblas -latlas
endif # ?parallel ATLAS
