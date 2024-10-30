!IFNDEF ABI
ABI=lp64
!ENDIF # !ABI
!IFNDEF WP
WP=16
!ENDIF # !WP
!IFNDEF RM
RM=del /F
!ENDIF # !RM
!IFDEF NDEBUG
DEBUG=
!ELSE # DEBUG
DEBUG=d
!ENDIF # ?NDEBUG
AR=lib.exe
ARFLAGS=/NOLOGO /VERBOSE
FORT=ifx
FC=$(FORT).exe
FORFLAGS=/nologo /fpp /DUSE_INTEL /DUSE_X64 /DQX_WP=$(WP) /Qopenmp /Qvec-threshold:0 /standard-semantics /traceback
LIBFLAGS=/I"$(MKLROOT)\include\intel64\$(ABI)" /I"$(MKLROOT)\include" /libs:dll /threads
!IF "$(ABI)"=="ilp64"
FORFLAGS=$(FORFLAGS) /4I8
LIBFLAGS=$(LIBFLAGS) /DMKL_ILP64
!ENDIF # ilp64
LIBS=/LIBPATH:.. qxblas$(WP)$(ABI)$(DEBUG).lib /LIBPATH:"$(MKLROOT)\lib\intel64_win" mkl_intel_$(ABI)_dll.lib mkl_intel_thread_dll.lib mkl_core_dll.lib
!IFNDEF MARCH
MARCH=Host
!ENDIF # !MARCH
!IFDEF NDEBUG
OPTFLAGS=/O$(NDEBUG) /Qx$(MARCH)
DBGFLAGS=/DNDEBUG
FPUFLAGS=/fp:precise /Qprotect-parens /Qfma /Qftz-
DBGFLAGS=$(DBGFLAGS) /Qopt-report:3
LDFLAGS=/link /RELEASE $(LIBS)
!ELSE # DEBUG
OPTFLAGS=/O$(DEBUG) /Qx$(MARCH)
DBGFLAGS=/debug:full /debug:inline-debug-info /debug-parameters:all /check:all /warn:all
FPUFLAGS=/fp:strict /Qprotect-parens /Qfma /Qftz- /assume:ieee_fpe_flags
LIBFLAGS=$(LIBFLAGS) /dbglibs
LDFLAGS=/link /DEBUG $(LIBS)
!ENDIF # ?NDEBUG
FFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFLAGS)

all: dgenskew.exe zgenskew.exe

help:
	@echo "nmake.exe [NDEBUG=0|1|2|3|4|5] [ABI=lp64|ilp64] [all|clean|help]"

dgenskew.exe: dgenskew.F90 bio.mod seed.mod lamgen.mod datgen.mod Makefile
	$(FC) $(FFLAGS) dgenskew.F90 bio.obj seed.obj lamgen.obj datgen.obj /Fe$@ $(LDFLAGS)

zgenskew.exe: zgenskew.F90 bio.mod seed.mod lamgen.mod datgen.mod Makefile
	$(FC) $(FFLAGS) zgenskew.F90 bio.obj seed.obj lamgen.obj datgen.obj /Fe$@ $(LDFLAGS)

bio.obj bio.mod: bio.F90 Makefile
	$(FC) $(FFLAGS) /c bio.F90

seed.obj seed.mod: seed.F90 Makefile
	$(FC) $(FFLAGS) /c seed.F90

lamgen.obj lamgen.mod: lamgen.F90 seed.mod Makefile
	$(FC) $(FFLAGS) /c lamgen.F90

datgen.obj datgen.mod: datgen.F90 seed.mod Makefile
	$(FC) $(FFLAGS) /c datgen.F90

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.obj
	-$(RM) *.pdb
	-$(RM) *.optrpt
	-$(RM) *__genmod.f90
	-$(RM) *__genmod.mod
