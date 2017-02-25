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
LIBS=libjstrat.a libqxblas.a libvn.a
ifeq ($(CPU),x64) # Xeon / Intel Fortran
include x64.mk
else ifeq ($(CPU),x100) # Knights Corner / Intel Fortran
include x100.mk
else ifeq ($(CPU),x200) # Knights Landing / Intel Fortran
include x200.mk
else ifeq ($(CPU),power8) # IBM POWER8LE / GNU Fortran
include power8.mk
else ifeq ($(CPU),pwr8) # IBM POWER8LE / XL Fortran
include pwr8.mk
else # GNU Fortran
include gnu.mk
endif # ?CPU
FCFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFLAGS)

all: xVJAC0.exe xVJAC1.exe xVJAC2.exe xDGESVD.exe xCSGEN.exe xLACSD.exe # xDJAC0.exe xDJAC1.exe xDJAC2.exe xJCSD.exe

help:
	@echo "make [WP=4|8|10|16] [CPU=x64|x100|power8|pwr8] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

xDJAC0.exe: xDJAC0.o CSD.o $(LIBS) GNUmakefile
	$(FC) $(FCFLAGS) xDJAC0.o CSD.o -o$@ $(LDFLAGS)

xDJAC0.o: xDJAC0.F90 csd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c xDJAC0.F90

xVJAC0.exe: xVJAC0.o CSD.o $(LIBS) GNUmakefile
	$(FC) $(FCFLAGS) xVJAC0.o CSD.o -o$@ $(LDFLAGS)

xVJAC0.o: xVJAC0.F90 csd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c xVJAC0.F90

xDJAC1.exe: xDJAC1.o CSD.o $(LIBS) GNUmakefile
	$(FC) $(FCFLAGS) xDJAC1.o CSD.o -o$@ $(LDFLAGS)

xDJAC1.o: xDJAC1.F90 csd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c xDJAC1.F90

xVJAC1.exe: xVJAC1.o CSD.o $(LIBS) GNUmakefile
	$(FC) $(FCFLAGS) xVJAC1.o CSD.o -o$@ $(LDFLAGS)

xVJAC1.o: xVJAC1.F90 csd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c xVJAC1.F90

xDJAC2.exe: xDJAC2.o CSD.o $(LIBS) GNUmakefile
	$(FC) $(FCFLAGS) xDJAC2.o CSD.o -o$@ $(LDFLAGS)

xDJAC2.o: xDJAC2.F90 csd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c xDJAC2.F90

xVJAC2.exe: xVJAC2.o CSD.o $(LIBS) GNUmakefile
	$(FC) $(FCFLAGS) xVJAC2.o CSD.o -o$@ $(LDFLAGS)

xVJAC2.o: xVJAC2.F90 csd.mod GNUmakefile
	$(FC) $(FCFLAGS) -c xVJAC2.F90

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

CSD.o csd.mod: CSD.F90 BIN_IO.F90 BLAS.F90 CONSTANTS.F90 GET_IOUNIT.F90 GET_NTHR.F90 IFACES_ESSL.F90 IFACES_IMPL.F90 INTERFACES.F90 JAC0.F90 JAC1.F90 JAC2.F90 KIND_PARAMS.F90 TIMER.F90 USE_MODULES.F90 VEC_PARAMS.F90 VJAC0.F90 VJAC1.F90 VJAC2.F90 GNUmakefile
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
