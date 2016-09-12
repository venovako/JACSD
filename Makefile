!IFNDEF WP
WP=16
!ENDIF # !WP

!IFNDEF RM
RM=del /F
!ENDIF # !RM

AR=xilib.exe
ARFLAGS=-qnoipo -lib /NOLOGO /VERBOSE

!IFDEF USE_MPI
!IFDEF NDEBUG
FC=mpiifort.bat
!ELSE # DEBUG
FC=mpiifort.bat -check_mpi -debug
!ENDIF # ?NDEBUG
!ELSE # no MPI
FC=ifort.exe
!ENDIF # ?USE_MPI

FORFLAGS=/nologo /fpp /DUSE_INTEL /DUSE_X64 /4I8 /Qopenmp /standard-semantics
LIBS=jstrat.lib qxblas.lib
MKLLIBS=mkl_intel_ilp64_dll.lib mkl_core_dll.lib mkl_intel_thread_dll.lib
!IFDEF NDEBUG
OPTFLAGS=/O$(NDEBUG) /QxHost
DBGFLAGS=/DNDEBUG /Qopt-report:5 /traceback
FPUFLAGS=/fp:source /Qfma /Qftz- /Qcomplex-limited-range- /Qfast-transcendentals- /Qprec-div /Qprec-sqrt
LIBFLAGS=/DUSE_MKL /I. /I"%MKLROOT%\include\intel64\ilp64" /I"%MKLROOT%\include" /libs:dll /threads
# /STACK:8388608 8 MiB stack
LDFLAGS=/link /RELEASE /LIBPATH:. $(LIBS) /LIBPATH:"%MKLROOT%\lib\intel64_win" $(MKLLIBS)
!ELSE # DEBUG
OPTFLAGS=/Od /QxHost
DBGFLAGS=/debug:full /debug:inline-debug-info /debug-parameters:all /check:all /warn:all /traceback
FPUFLAGS=/fp:strict /assume:ieee_fpe_flags /Qfma /Qfp-stack-check /Qftz- /Qcomplex-limited-range- /Qfast-transcendentals- /Qprec-div /Qprec-sqrt
LIBFLAGS=/DUSE_MKL /I. /I"%MKLROOT%\include\intel64\ilp64" /I"%MKLROOT%\include" /libs:dll /threads /dbglibs
# /STACK:8388608 8 MiB stack
LDFLAGS=/link /DEBUG /LIBPATH:. $(LIBS) /LIBPATH:"%MKLROOT%\lib\intel64_win" $(MKLLIBS)
!ENDIF # ?NDEBUG
FCFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFLAGS)

all: xDJAC0.exe xDJAC1.exe xDJAC2.exe xCSGEN.exe xLACSD.exe # xJCSD.exe

help:
	@echo "nmake.exe [WP=4|8|16] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

xDJAC0.exe: xDJAC0.obj CSD.obj $(LIBS) Makefile
	$(FC) $(FCFLAGS) xDJAC0.obj CSD.obj /exe:$@ $(LDFLAGS)

xDJAC0.obj: xDJAC0.F90 csd.mod Makefile
	$(FC) $(FCFLAGS) /c xDJAC0.F90

xDJAC1.exe: xDJAC1.obj CSD.obj $(LIBS) Makefile
	$(FC) $(FCFLAGS) xDJAC1.obj CSD.obj /exe:$@ $(LDFLAGS)

xDJAC1.obj: xDJAC1.F90 csd.mod Makefile
	$(FC) $(FCFLAGS) /c xDJAC1.F90

xDJAC2.exe: xDJAC2.obj CSD.obj $(LIBS) Makefile
	$(FC) $(FCFLAGS) xDJAC2.obj CSD.obj /exe:$@ $(LDFLAGS)

xDJAC2.obj: xDJAC2.F90 csd.mod Makefile
	$(FC) $(FCFLAGS) /c xDJAC2.F90

xCSGEN.exe: xCSGEN.obj CSD.obj $(LIBS) Makefile
	$(FC) $(FCFLAGS) xCSGEN.obj CSD.obj /exe:$@ $(LDFLAGS)

xCSGEN.obj: xCSGEN.F90 qx_wp.fi csd.mod Makefile
	$(FC) $(FCFLAGS) /c xCSGEN.F90

xLACSD.exe: xLACSD.obj BSCSD.obj CSD.obj $(LIBS) Makefile
	$(FC) $(FCFLAGS) xLACSD.obj BSCSD.obj CSD.obj /exe:$@ $(LDFLAGS)

xLACSD.obj: xLACSD.F90 bscsd.mod Makefile
	$(FC) $(FCFLAGS) /c xLACSD.F90

xJCSD.exe: xJCSD.obj JCSD.obj CSD.obj $(LIBS) Makefile
	$(FC) $(FCFLAGS) xJCSD.obj JCSD.obj CSD.obj /exe:$@ $(LDFLAGS)

xJCSD.obj: xJCSD.F90 jcsd.mod Makefile
	$(FC) $(FCFLAGS) /c xJCSD.F90

jstrat.lib: Makefile
!IFDEF NDEBUG
	pushd jstrat && $(MAKE) NDEBUG=$(NDEBUG) && popd
!ELSE # DEBUG
	pushd jstrat && $(MAKE) && popd
!ENDIF # ?NDEBUG

qx_wp.fi qxblas.lib: Makefile
!IFDEF NDEBUG
	pushd qxblas && $(MAKE) WP=$(WP) NDEBUG=$(NDEBUG) && popd
!ELSE # DEBUG
	pushd qxblas && $(MAKE) WP=$(WP) && popd
!ENDIF # ?NDEBUG

BSCSD.obj bscsd.mod: BSCSD.F90 csd.mod Makefile
	$(FC) $(FCFLAGS) /c BSCSD.F90

JCSD.obj jcsd.mod: JCSD.F90 csd.mod Makefile
	$(FC) $(FCFLAGS) /c JCSD.F90

CSD.obj csd.mod: CSD.F90 BIN_IO.F90 BLAS.F90 CONSTANTS.F90 GET_IOUNIT.F90 GET_NTHR.F90 IFACES_IMPL.F90 INTERFACES.F90 JAC0.F90 JAC1.F90 JAC2.F90 KIND_PARAMS.F90 TIMER.F90 USE_MODULES.F90 VEC_PARAMS.F90 Makefile
	$(FC) $(FCFLAGS) /c CSD.F90

clean:
!IFDEF NDEBUG
	pushd jstrat && $(MAKE) NDEBUG=$(NDEBUG) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) NDEBUG=$(NDEBUG) clean && popd
!ELSE # DEBUG
	pushd jstrat && $(MAKE) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) clean && popd
!ENDIF # ?NDEBUG
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.obj
	-$(RM) *.optrpt
	-$(RM) *__genmod.f90
	-$(RM) *__genmod.mod
