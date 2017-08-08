ifeq ($(CPU),x64) # Xeon / Intel Fortran
include x64.mk
MKFS=GNUmakefile x64.mk
LIBFLAGS=-DUSE_MKL -DMKL_DIRECT_CALL -I. -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include -threads
ifeq ($(ARCH),Darwin)
LDFLAGS=-L. -ljstrat -lqxblas -lvn -L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm -ldl
else # Linux
LDFLAGS=-L. -ljstrat -lqxblas -lvn -L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm -ldl
endif # ?Darwin
else ifeq ($(CPU),x100) # Knights Corner / Intel Fortran
include x100.mk
MKFS=GNUmakefile x100.mk
LIBFLAGS=-DUSE_MKL -DMKL_DIRECT_CALL -I. -I${MKLROOT}/include/mic/ilp64 -I${MKLROOT}/include -threads
LDFLAGS=-L. -ljstrat -lqxblas -lvn -L${MKLROOT}/lib/mic -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm -ldl
else ifeq ($(CPU),x200) # Knights Landing / Intel Fortran
include x200.mk
MKFS=GNUmakefile x200.mk
LIBFLAGS=-DUSE_MKL -DMKL_DIRECT_CALL -I. -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include -threads
LDFLAGS=-L. -ljstrat -lqxblas -lvn -L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm -ldl -lmemkind
else ifeq ($(CPU),power8) # IBM POWER8LE / GNU Fortran
include power8.mk
MKFS=GNUmakefile power8.mk
LIBFLAGS=-I. -DUSE_ATLAS
ifeq ($(USE_ATLAS),pt)
LDFLAGS=-L. -ljstrat -lqxblas -L$(HOME)/atlas/lib -lptlapack -llapack -lptf77blas -lptcblas -latlas
else # sequential ATLAS
LDFLAGS=-L. -ljstrat -lqxblas -L$(HOME)/atlas/lib -lptlapack -llapack -lf77blas -lcblas -latlas
endif # ?parallel ATLAS
else ifeq ($(CPU),pwr8) # IBM POWER8LE / XL Fortran
include pwr8.mk
MKFS=GNUmakefile pwr8.mk
LIBFLAGS=-I. -WF,-DUSE_ESSL #-qessl
LDFLAGS=-L. -ljstrat -lqxblas -lvn -L/usr/lib64 -lesslsmp6464 -lessl6464
else # GNU Fortran
include gnu.mk
MKFS=GNUmakefile gnu.mk
LIBFLAGS=-I.
LDFLAGS=-L. -ljstrat -lqxblas -lvn -L$(HOME)/OpenBLAS-seq/lib -lopenblas #_omp #-ltmglib -llapack -lrefblas
endif # ?CPU
FCFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFLAGS)

LIBS=libjstrat.a libqxblas.a libvn.a

.PHONY: all help clean

all: xVJAC0.exe xVJAC1.exe xVJAC2.exe xDGESVD.exe xCSGEN.exe xLACSD.exe # xDJAC0.exe xDJAC1.exe xDJAC2.exe xJCSD.exe

help:
	@echo "make [WP=4|8|10|16] [CPU=x64|x100|power8|pwr8] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

xDJAC0.exe: xDJAC0.o CSD.o $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) xDJAC0.o CSD.o -o$@ $(LDFLAGS)

xDJAC0.o: xDJAC0.F90 csd.mod $(MKFS)
	$(FC) $(FCFLAGS) -c xDJAC0.F90

xVJAC0.exe: xVJAC0.o CSD.o $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) xVJAC0.o CSD.o -o$@ $(LDFLAGS)

xVJAC0.o: xVJAC0.F90 csd.mod $(MKFS)
	$(FC) $(FCFLAGS) -c xVJAC0.F90

xDJAC1.exe: xDJAC1.o CSD.o $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) xDJAC1.o CSD.o -o$@ $(LDFLAGS)

xDJAC1.o: xDJAC1.F90 csd.mod $(MKFS)
	$(FC) $(FCFLAGS) -c xDJAC1.F90

xVJAC1.exe: xVJAC1.o CSD.o $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) xVJAC1.o CSD.o -o$@ $(LDFLAGS)

xVJAC1.o: xVJAC1.F90 csd.mod $(MKFS)
	$(FC) $(FCFLAGS) -c xVJAC1.F90

xDJAC2.exe: xDJAC2.o CSD.o $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) xDJAC2.o CSD.o -o$@ $(LDFLAGS)

xDJAC2.o: xDJAC2.F90 csd.mod $(MKFS)
	$(FC) $(FCFLAGS) -c xDJAC2.F90

xVJAC2.exe: xVJAC2.o CSD.o $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) xVJAC2.o CSD.o -o$@ $(LDFLAGS)

xVJAC2.o: xVJAC2.F90 csd.mod $(MKFS)
	$(FC) $(FCFLAGS) -c xVJAC2.F90

xDGESVD.exe: xDGESVD.o CSD.o $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) xDGESVD.o CSD.o -o$@ $(LDFLAGS)

xDGESVD.o: xDGESVD.F90 csd.mod $(MKFS)
	$(FC) $(FCFLAGS) -c xDGESVD.F90

xCSGEN.exe: xCSGEN.o CSD.o $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) xCSGEN.o CSD.o -o$@ $(LDFLAGS)

xCSGEN.o: xCSGEN.F90 qx_wp.fi csd.mod $(MKFS)
	$(FC) $(FCFLAGS) -c xCSGEN.F90

xLACSD.exe: xLACSD.o BSCSD.o CSD.o $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) xLACSD.o BSCSD.o CSD.o -o$@ $(LDFLAGS)

xLACSD.o: xLACSD.F90 bscsd.mod $(MKFS)
	$(FC) $(FCFLAGS) -c xLACSD.F90

xJCSD.exe: xJCSD.o JCSD.o CSD.o $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) xJCSD.o JCSD.o CSD.o -o$@ $(LDFLAGS)

xJCSD.o: xJCSD.F90 jcsd.mod $(MKFS)
	$(FC) $(FCFLAGS) -c xJCSD.F90

libjstrat.a: $(MKFS)
ifdef NDEBUG
	pushd jstrat && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
else # DEBUG
	pushd jstrat && $(MAKE) CPU=$(CPU) && popd
endif # ?NDEBUG

qx_wp.fi libqxblas.a: $(MKFS)
ifdef NDEBUG
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
else # DEBUG
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) && popd
endif # ?NDEBUG

libvn.a: $(MKFS)
ifdef NDEBUG
	pushd vn && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
else # DEBUG
	pushd vn && $(MAKE) CPU=$(CPU) && popd
endif # ?NDEBUG

BSCSD.o bscsd.mod: BSCSD.F90 csd.mod $(MKFS)
	$(FC) $(FCFLAGS) -c BSCSD.F90

JCSD.o jcsd.mod: JCSD.F90 csd.mod $(MKFS)
	$(FC) $(FCFLAGS) -c JCSD.F90

CSD.o csd.mod: CSD.F90 BIN_IO.F90 BLAS.F90 CONSTANTS.F90 GET_IOUNIT.F90 GET_NTHR.F90 IFACES_ESSL.F90 IFACES_IMPL.F90 INTERFACES.F90 JAC0.F90 JAC1.F90 JAC2.F90 KIND_PARAMS.F90 TIMER.F90 USE_MODULES.F90 VEC_PARAMS.F90 VJAC0.F90 VJAC1.F90 VJAC2.F90 $(MKFS)
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
	-$(RM) *.dSYM
