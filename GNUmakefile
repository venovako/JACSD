ifndef SHELL
SHELL=/bin/bash
endif # !SHELL
ifndef ARCH
ARCH=$(shell uname)
endif # !ARCH
ifndef WP
WP=16
endif # !WP
ifndef RM
RM=rm -fv
endif # !RM
LIBS=libjstrat.a libqxblas.a # libvn.a
ifeq ($(CPU),x64) # Xeon
AR=xiar
ARFLAGS=-qnoipo -lib rsv
ifdef USE_MPI
FC=mpiifort -ilp64 -trace # -tcollect
FORFLAGS=-DUSE_INTEL -DUSE_X64 -DUSE_MPI -i8 -qopenmp -fexceptions -standard-semantics
else # no MPI
FC=ifort
FORFLAGS=-DUSE_INTEL -DUSE_X64 -i8 -qopenmp -fexceptions -standard-semantics
endif # ?USE_MPI
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -xHost
DBGFLAGS=-DNDEBUG -fno-omit-frame-pointer -qopt-report=5 -traceback -diag-disable=10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0 -xHost
ifeq ($(ARCH),Darwin)
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
else # Linux
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
endif # ?Darwin
FPUFLAGS=-fp-model strict -assume ieee_fpe_flags -fma -fp-stack-check -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
endif # ?NDEBUG
LIBFLAGS=-DUSE_MKL -DMKL_DIRECT_CALL -I. -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include -threads
ifeq ($(ARCH),Darwin)
LDFLAGS=-L. -ljstrat -lqxblas -L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm -ldl # -lvn after -lqxblas
else # Linux
LDFLAGS=-L. -ljstrat -lqxblas -L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm -ldl # -lvn after -lqxblas
endif # ?Darwin
else ifeq ($(CPU),x100) # Knights Corner
AR=xiar
ARFLAGS=-qnoipo -lib rsv
ifdef USE_MPI
FC=mpiifort -ilp64 -trace # -tcollect
FORFLAGS=-DUSE_INTEL -DUSE_KNC -DUSE_MPI -mmic -i8 -qopenmp -fexceptions -standard-semantics
else # no MPI
FC=ifort
FORFLAGS=-DUSE_INTEL -DUSE_KNC -mmic -i8 -qopenmp -fexceptions -standard-semantics
endif # ?USE_MPI
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG)
DBGFLAGS=-DNDEBUG -fno-omit-frame-pointer -qopt-report=5 -traceback -diag-disable=10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt # -fp-model strict -assume ieee_fpe_flags -fp-stack-check
endif # ?NDEBUG
LIBFLAGS=-DUSE_MKL -DMKL_DIRECT_CALL -I. -I${MKLROOT}/include/mic/ilp64 -I${MKLROOT}/include -threads
LDFLAGS=-L. -ljstrat -lqxblas -L${MKLROOT}/lib/mic -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm -ldl # -lvn after -lqxblas
else ifeq ($(CPU),power8) # IBM POWER8LE
AR=ar
ARFLAGS=rsv
FC=gfortran
FORFLAGS=-DUSE_GNU -DUSE_PWR8 -fdefault-integer-8 -frecursive -fstack-arrays -fopenmp
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -mcpu=power8 -fgcse-sm -fgcse-las -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
DBGFLAGS=-DNDEBUG -fopt-info-optimized-vec
FPUFLAGS=
else # DEBUG
OPTFLAGS=-Og -mcpu=power8
DBGFLAGS=-g -fcheck=all -finit-local-zero -finit-real=snan
FPUFLAGS=-ffpe-trap=invalid,zero,overflow
endif # ?NDEBUG
LIBFLAGS=-DUSE_OPENBLAS -I.
LDFLAGS=-L. -ljstrat -lqxblas -L$(HOME)/OpenBLAS-gcc/lib -lopenblas_omp
else ifeq ($(CPU),pwr8) # IBM POWER8LE / XL
AR=ar
ARFLAGS=rsv
ifdef USE_MPI
FC=mpfort -WF,-qfpp
FORFLAGS=-WF,-DUSE_IBM -WF,-DUSE_PWR8 -WF,-DUSE_MPI -qintsize=8 -qnosave -qsclk=micro -qsmp=omp -qreport=smplist -qlanglvl=extended -qassert=contig -k -qxlf90=signedzero -qxlf2003=nooldnaninf:signdzerointr
else # no MPI
FC=xlf2008_r
FORFLAGS=-WF,-DUSE_IBM -WF,-DUSE_PWR8 -qintsize=8 -qnosave -qsclk=micro -qsmp=omp -qreport=smplist -qlanglvl=extended -qassert=contig
endif # ?USE_MPI
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -qmaxmem=-1 -qarch=auto -qtune=pwr8:smt8 -qhot=level=2:vector -qprefetch=aggressive
DBGFLAGS=-WF,-DNDEBUG -qinfo=mt #:unset
FPUFLAGS=-qfloat=nans:subnormals -qstrict=nans:infinities:subnormals:zerosigns:operationprecision
else # DEBUG
OPTFLAGS=-O0 -qmaxmem=-1 -qarch=auto -qtune=pwr8:smt8
DBGFLAGS=-g -C -qinfo=mt #:unset
FPUFLAGS=-qfloat=nans:subnormals
endif # ?NDEBUG
LIBFLAGS=-qessl -WF,-DUSE_ESSL -I. #-WF,-DUSE_OPENBLAS -I.
LDFLAGS=-L. -ljstrat -lqxblas -L/usr/lib64 -lesslsmp6464 -lessl6464 #-L$(HOME)/OpenBLAS-ibm/lib -lopenblas_omp
#-L$(HOME)/lapack -ltmglib -llapack -lrefblas # -lvn after -lqxblas
else # GNU Fortran
AR=ar
ARFLAGS=rsv
ifdef USE_MPI
FC=mpifort
FORFLAGS=-DUSE_GNU -DUSE_X64 -DUSE_MPI -fdefault-integer-8 -frecursive -fstack-arrays -fopenmp
else # no MPI
FC=gfortran
FORFLAGS=-DUSE_GNU -DUSE_X64 -fdefault-integer-8 -frecursive -fstack-arrays -fopenmp
endif # ?USE_MPI
ifdef NDEBUG
ifeq ($(ARCH),Darwin)
OPTFLAGS=-O$(NDEBUG) -march=native -Wa,-q -fgcse-sm -fgcse-las -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
else # Linux
OPTFLAGS=-O$(NDEBUG) -march=native -fgcse-sm -fgcse-las -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
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
LDFLAGS=-L. -ljstrat -lqxblas -L$(HOME)/lapack -ltmglib -llapack -lrefblas # -lvn after -lqxblas
endif # ?CPU
FCFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFLAGS)

ifeq ($(CPU),pwr8)
all: xDJAC0.exe xDJAC1.exe xDJAC2.exe xDGESVD.exe xCSGEN.exe
else
all: xDJAC0.exe xDJAC1.exe xDJAC2.exe xDGESVD.exe xCSGEN.exe xLACSD.exe # xJCSD.exe
endif

help:
	@echo "make [WP=4|8|10|16] [CPU=x64|x100|power8|pwr8] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

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

xDGESVD.exe: xDGESVD.o CSD.o $(LIBS) GNUmakefile
	$(FC) $(FCFLAGS) xDGESVD.o CSD.o -o$@ $(LDFLAGS)

xDGESVD.o: xDGESVD.F90 csd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c xDGESVD.F90

xCSGEN.exe: xCSGEN.o CSD.o $(LIBS) GNUmakefile
	$(FC) $(FCFLAGS) xCSGEN.o CSD.o -o$@ $(LDFLAGS)

xCSGEN.o: xCSGEN.F90 qx_wp.fi csd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c xCSGEN.F90

xLACSD.exe: xLACSD.o BSCSD.o CSD.o $(LIBS) GNUmakefile
	$(FC) $(FCFLAGS) xLACSD.o BSCSD.o CSD.o -o$@ $(LDFLAGS)

xLACSD.o: xLACSD.F90 bscsd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c xLACSD.F90

xJCSD.exe: xJCSD.o JCSD.o CSD.o $(LIBS) GNUmakefile
	$(FC) $(FCFLAGS) xJCSD.o JCSD.o CSD.o -o$@ $(LDFLAGS)

xJCSD.o: xJCSD.F90 jcsd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c xJCSD.F90

libjstrat.a: GNUmakefile
ifdef NDEBUG
	pushd jstrat && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
else # DEBUG
	pushd jstrat && $(MAKE) CPU=$(CPU) && popd
endif # ?NDEBUG

qx_wp.fi libqxblas.a: GNUmakefile
ifdef NDEBUG
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
else # DEBUG
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) && popd
endif # ?NDEBUG

libvn.a: GNUmakefile
ifdef NDEBUG
	pushd vn && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
else # DEBUG
	pushd vn && $(MAKE) CPU=$(CPU) && popd
endif # ?NDEBUG

BSCSD.o bscsd.mod: BSCSD.F90 csd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c BSCSD.F90

JCSD.o jcsd.mod: JCSD.F90 csd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c JCSD.F90

CSD.o csd.mod: CSD.F90 BIN_IO.F90 BLAS.F90 CONSTANTS.F90 GET_IOUNIT.F90 GET_NTHR.F90 IFACES_IMPL.F90 INTERFACES.F90 JAC0.F90 JAC1.F90 JAC2.F90 KIND_PARAMS.F90 TIMER.F90 USE_MODULES.F90 VEC_PARAMS.F90 GNUmakefile
	$(FC) $(FCFLAGS) -c CSD.F90

clean:
ifdef NDEBUG
	pushd vn && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) clean && popd
	pushd jstrat && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) NDEBUG=$(NDEBUG) clean && popd
else # DEBUG
	pushd vn && $(MAKE) CPU=$(CPU) clean && popd
	pushd jstrat && $(MAKE) CPU=$(CPU) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) clean && popd
endif # ?NDEBUG
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *__genmod.f90
	-$(RM) *__genmod.mod
