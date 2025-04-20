RM=del /F
AR=lib.exe
ARFLAGS=/NOLOGO /VERBOSE
FC=ifx.exe
!IFNDEF MARCH
MARCH=Host
!ENDIF # !MARCH
!IFNDEF NDEBUG
NDEBUG=d
!ENDIF # !NDEBUG
!IFNDEF WP
WP=16
!ENDIF # !WP
!IFNDEF ABI
ABI=ilp64
!ENDIF # !ABI
FCFLAGS=/nologo /fpp /recursive /standard-semantics /traceback /DNDEBUG=$(NDEBUG) /DQX_WP=$(WP) /I. /MT /O$(NDEBUG) /Qx$(MARCH) /fp:precise /Qfma /Qftz- /Qprec-div /Qprotect-parens /Qopt-report:3 /Qvec-threshold:0
!IF "$(ABI)"=="ilp64"
FCFLAGS=$(FCFLAGS) /4I8 /DMKL_ILP64 /Qmkl-ilp64:sequential
!ELSE # lp64
FCFLAGS=$(FCFLAGS) /Qmkl:sequential
!ENDIF # ilp64
LDFLAGS=/link /RELEASE /LIBPATH:.. qxblas$(WP)$(ABI).lib /LIBPATH:..\..\libpvn\src pvn.lib

all: dgenhsvd.exe zgenhsvd.exe svalserr.exe gentxt.exe

help:
	@echo "nmake.exe [MARCH=Host|...] [NDEBUG=d|1|2|3...] [ABI=ilp64|lp64] [WP=16|...] [all|clean|help]"

dgenhsvd.exe: dgenhsvd.F90 bio.mod seed.mod lamgen.mod indeff.mod datgen.mod Makefile
	$(FC) $(FCFLAGS) dgenhsvd.F90 bio.obj seed.obj lamgen.obj indeff.obj datgen.obj /Fe$@ $(LDFLAGS)

zgenhsvd.exe: zgenhsvd.F90 bio.mod seed.mod lamgen.mod indeff.mod datgen.mod Makefile
	$(FC) $(FCFLAGS) zgenhsvd.F90 bio.obj seed.obj lamgen.obj indeff.obj datgen.obj /Fe$@ $(LDFLAGS)

svalserr.exe: svalserr.F90 bio.mod Makefile
	$(FC) $(FCFLAGS) svalserr.F90 bio.obj /Fe$@ $(LDFLAGS)

gentxt.exe: gentxt.F90 Makefile
	$(FC) $(FCFLAGS) gentxt.F90 /Fe$@ $(LDFLAGS)

bio.obj bio.mod: bio.F90 Makefile
	$(FC) $(FCFLAGS) /c bio.F90

seed.obj seed.mod: seed.F90 Makefile
	$(FC) $(FCFLAGS) /c seed.F90

lamgen.obj lamgen.mod: lamgen.F90 seed.mod Makefile
	$(FC) $(FCFLAGS) /c lamgen.F90

indeff.obj indeff.mod: indeff.F90 Makefile
	$(FC) $(FCFLAGS) /c indeff.F90

datgen.obj datgen.mod: datgen.F90 seed.mod indeff.mod Makefile
	$(FC) $(FCFLAGS) /c datgen.F90

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.obj
	-$(RM) *.optrpt
	-$(RM) *.pdb
