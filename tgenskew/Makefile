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

LIBS=qxblas$(WP)$(DEBUG).lib

AR=xilib.exe
ARFLAGS=-qnoipo -lib /NOLOGO /VERBOSE
FC=ifort

FORFLAGS=/nologo /fpp /DUSE_INTEL /DUSE_X64 /DQX_WP=$(WP) /Qopenmp /standard-semantics #/4I8
!IFDEF NDEBUG
OPTFLAGS=/O$(NDEBUG) /QxHost /Qopt-multi-version-aggressive
DBGFLAGS=/DNDEBUG /Qopt-report:5 /traceback
FPUFLAGS=/fp:source /Qfp-stack-check /Qimf-precision:high /Qfma /Qftz- /Qcomplex-limited-range- /Qfast-transcendentals- /Qprec-div /Qprec-sqrt
LIBFLAGS=/Qmkl /libs:dll /threads
LDFLAGS=/link /RELEASE /LIBPATH:.. $(LIBS)
!ELSE # DEBUG
OPTFLAGS=/O$(DEBUG) /QxHost /Qopt-multi-version-aggressive
DBGFLAGS=/debug:full /debug:inline-debug-info /debug-parameters:all /check:all /warn:all /traceback
FPUFLAGS=/fp:strict /assume:ieee_fpe_flags /Qfp-stack-check /Qimf-precision:high /Qfma /Qftz- /Qcomplex-limited-range- /Qfast-transcendentals- /Qprec-div /Qprec-sqrt
LIBFLAGS=/Qmkl /libs:dll /threads /dbglibs
LDFLAGS=/link /DEBUG /LIBPATH:.. $(LIBS)
!ENDIF # ?NDEBUG
FFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFLAGS)

all: dgenskew.exe zgenskew.exe

help:
	@echo "nmake.exe [NDEBUG=0|1|2|3|4|5] [WP=4|8|16] [all|clean|help]"

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
