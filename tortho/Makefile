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
FCFLAGS=$(FCFLAGS) /4I8
!ENDIF # ilp64
LDFLAGS=/link /RELEASE

all: dortho.exe zortho.exe sortho.exe cortho.exe

help:
	@echo "nmake.exe [MARCH=Host|...] [NDEBUG=d|1|2|3|...] [ABI=ilp64|lp64] [WP=16|...] [all|clean|help]"

dortho.exe: dortho.F90 bio.mod $(MKFS)
	$(FC) $(FCFLAGS) dortho.F90 bio.obj /Fe$@ $(LDFLAGS)

zortho.exe: zortho.F90 bio.mod $(MKFS)
	$(FC) $(FCFLAGS) zortho.F90 bio.obj /Fe$@ $(LDFLAGS)

sortho.exe: sortho.F90 bio.mod $(MKFS)
	$(FC) $(FCFLAGS) sortho.F90 bio.obj /Fe$@ $(LDFLAGS)

cortho.exe: cortho.F90 bio.mod $(MKFS)
	$(FC) $(FCFLAGS) cortho.F90 bio.obj /Fe$@ $(LDFLAGS)

bio.obj bio.mod: bio.F90 $(MKFS)
	$(FC) $(FCFLAGS) /c bio.F90

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.obj
	-$(RM) *.optrpt
	-$(RM) *.pdb
