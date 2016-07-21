SHELL=/bin/bash
RM=rm -fv
ifndef WP
WP=16
endif # !WP
ifndef ARCH
ARCH=none
endif # !ARCH
LIBS=libjstrat.a libqxblas.a
ifeq ($(CPU),x64) # Xeon
AR=xiar
ARFLAGS=-qnoipo -lib rsv
ifdef USE_MPI
FC=mpiifort
else # no MPI
FC=ifort
endif # ?USE_MPI
FORFLAGS=-DUSE_INTEL -i8 -qopenmp -fexceptions -standard-semantics
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -xHost
DBGFLAGS=-DNDEBUG -fno-omit-frame-pointer -qopt-report=5 -traceback -diag-disable=10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0 -xHost
ifeq ($(ARCH),MACINT)
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
else # Linux
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
endif # ?MACINT
FPUFLAGS=-fp-model strict -assume ieee_fpe_flags -fma -fp-stack-check -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
endif # ?NDEBUG
LIBFLAGS=-DUSE_MKL -I. -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include -threads
ifeq ($(ARCH),MACINT)
LDFLAGS=-L. -ljstrat -lqxblas -L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm -ldl
else # Linux
LDFLAGS=-L. -ljstrat -lqxblas -L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm -ldl
endif # ?MACINT
else ifeq ($(CPU),x100) # Knights Corner
AR=xiar
ARFLAGS=-qnoipo -lib rsv
ifdef USE_MPI
FC=mpiifort
else # no MPI
FC=ifort
endif # ?USE_MPI
FORFLAGS=-DUSE_INTEL -DUSE_KNC -mmic -i8 -qopenmp -fexceptions -standard-semantics
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG)
DBGFLAGS=-DNDEBUG -fno-omit-frame-pointer -qopt-report=5 -traceback -diag-disable=10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
FPUFLAGS=-fp-model strict -assume ieee_fpe_flags -fma -fp-stack-check -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
endif # ?NDEBUG
LIBFLAGS=-DUSE_MKL -I. -I${MKLROOT}/include/mic/ilp64 -I${MKLROOT}/include -threads
LDFLAGS=-L. -ljstrat -lqxblas -L${MKLROOT}/lib/mic -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm -ldl
else ifeq ($(CPU),pwr8) # Power8
AR=ar
ARFLAGS=rsv
ifdef USE_MPI
FC=mpfort
else # no MPI
FC=xlf2008_r
endif # ?USE_MPI
FORFLAGS=-WF,-DUSE_IBM -qintsize=8 -qnosave -qsclk=micro -qsmp=omp # -WF,-qfpp
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -qmaxmem=-1 -qtune=pwr8:smt8
DBGFLAGS=-WF,-DNDEBUG
FPUFLAGS=-qfloat=nans:subnormals
else # DEBUG
OPTFLAGS=-O0 -qmaxmem=-1 -qtune=pwr8:smt8
DBGFLAGS=-g
FPUFLAGS=-qfloat=nans:subnormals
endif # ?NDEBUG
LIBFLAGS=-I.
LDFLAGS=-L. -ljstrat -lqxblas -L$(HOME)/lapack -ltmglib -llapack -lrefblas
else # GNU Fortran
AR=ar
ARFLAGS=rsv
ifdef USE_MPI
FC=mpifort
else # no MPI
FC=gfortran
endif # ?USE_MPI
FORFLAGS=-DUSE_GNU -fdefault-integer-8 -fopenmp -fexceptions -frecursive
ifdef NDEBUG
ifeq ($(ARCH),MACINT)
OPTFLAGS=-O$(NDEBUG) -march=native -Wa,-q
else # Linux
OPTFLAGS=-O$(NDEBUG) -march=native
endif # ?MACINT
DBGFLAGS=-DNDEBUG
FPUFLAGS=
else # DEBUG
ifeq ($(ARCH),MACINT)
OPTFLAGS=-O0 -march=native -Wa,-q
else # Linux
OPTFLAGS=-O0 -march=native
endif # ?MACINT
DBGFLAGS=-g -fcheck=all -finit-local-zero -finit-real=snan
FPUFLAGS=-ffpe-trap=invalid,zero,overflow
endif # ?NDEBUG
LIBFLAGS=-I.
LDFLAGS=-L. -ljstrat -lqxblas -L$(HOME)/lapack -ltmglib -llapack -lrefblas
endif # ?CPU
FCFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFLAGS)

all: xDJAC0.exe xDJAC1.exe xDJAC2.exe

help:
	@echo "make [WP=4|8|10|16] [CPU=x64|x100|pwr8] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

xDJAC0.exe: xDJAC0.o CSD.o $(LIBS) GNUmakefile
	$(FC) $(FCFLAGS) xDJAC0.o CSD.o -o$@ $(LDFLAGS)

xDJAC0.o: xDJAC0.F90 csd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c xDJAC0.F90

xDJAC1.exe: xDJAC1.o CSD.o $(LIBS) GNUmakefile
	$(FC) $(FCFLAGS) xDJAC1.o CSD.o -o$@ $(LDFLAGS)

xDJAC1.o: xDJAC1.F90 csd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c xDJAC1.F90

xDJAC2.exe: xDJAC2.o CSD.o $(LIBS) GNUmakefile
	$(FC) $(FCFLAGS) xDJAC2.o CSD.o -o$@ $(LDFLAGS)

xDJAC2.o: xDJAC2.F90 csd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c xDJAC2.F90

libjstrat.a: GNUmakefile
ifdef NDEBUG
	pushd jstrat && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) ARCH=$(ARCH) && popd
else # DEBUG
	pushd jstrat && $(MAKE) CPU=$(CPU) ARCH=$(ARCH) && popd
endif # ?NDEBUG

qx_wp.fi libqxblas.a: GNUmakefile
ifdef NDEBUG
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) NDEBUG=$(NDEBUG) ARCH=$(ARCH) && popd
else # DEBUG
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) ARCH=$(ARCH) && popd
endif # ?NDEBUG

CSD.o csd.mod: CSD.F90 BIN_IO.F90 BLAS.F90 CONSTANTS.F90 GET_IOUNIT.F90 GET_NTHR.F90 IFACES_IMPL.F90 INTERFACES.F90 JAC0.F90 JAC1.F90 JAC2.F90 KIND_PARAMS.F90 TIMER.F90 USE_MODULES.F90 VEC_PARAMS.F90 GNUmakefile
	$(FC) $(FCFLAGS) -c CSD.F90

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *__genmod.f90
	-$(RM) *__genmod.mod
