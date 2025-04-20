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

all: dgentxt.exe sgentxt.exe dgensvd.exe sgensvd.exe zgensvd.exe cgensvd.exe derrsvd.exe serrsvd.exe zerrsvd.exe cerrsvd.exe dsverr.exe ssverr.exe

help:
	@echo "nmake.exe [MARCH=Host|...] [NDEBUG=d|1|2|3|...] [ABI=ilp64|lp64] [WP=16|...] [all|clean|help]"

dgentxt.exe: dgentxt.F90 Makefile
	$(FC) $(FCFLAGS) dgentxt.F90 /Fe$@ $(LDFLAGS)

sgentxt.exe: sgentxt.F90 Makefile
	$(FC) $(FCFLAGS) sgentxt.F90 /Fe$@ $(LDFLAGS)

dgensvd.exe: dgensvd.F90 bio.mod seed.mod svgen.mod datgen.mod Makefile
	$(FC) $(FCFLAGS) dgensvd.F90 bio.obj seed.obj svgen.obj datgen.obj /Fe$@ $(LDFLAGS)

sgensvd.exe: sgensvd.F90 bio.mod seed.mod svgen.mod datgen.mod Makefile
	$(FC) $(FCFLAGS) sgensvd.F90 bio.obj seed.obj svgen.obj datgen.obj /Fe$@ $(LDFLAGS)

zgensvd.exe: zgensvd.F90 bio.mod seed.mod svgen.mod datgen.mod Makefile
	$(FC) $(FCFLAGS) zgensvd.F90 bio.obj seed.obj svgen.obj datgen.obj /Fe$@ $(LDFLAGS)

cgensvd.exe: cgensvd.F90 bio.mod seed.mod svgen.mod datgen.mod Makefile
	$(FC) $(FCFLAGS) cgensvd.F90 bio.obj seed.obj svgen.obj datgen.obj /Fe$@ $(LDFLAGS)

derrsvd.exe: derrsvd.F90 bio.mod Makefile
	$(FC) $(FCFLAGS) derrsvd.F90 bio.obj /Fe$@ $(LDFLAGS)

serrsvd.exe: serrsvd.F90 bio.mod Makefile
	$(FC) $(FCFLAGS) serrsvd.F90 bio.obj /Fe$@ $(LDFLAGS)

zerrsvd.exe: zerrsvd.F90 bio.mod Makefile
	$(FC) $(FCFLAGS) zerrsvd.F90 bio.obj /Fe$@ $(LDFLAGS)

cerrsvd.exe: cerrsvd.F90 bio.mod Makefile
	$(FC) $(FCFLAGS) cerrsvd.F90 bio.obj /Fe$@ $(LDFLAGS)

dsverr.exe: dsverr.F90 bio.mod qccmp.obj Makefile
	$(FC) $(FCFLAGS) dsverr.F90 bio.obj qccmp.obj /Fe$@ $(LDFLAGS)

ssverr.exe: ssverr.F90 bio.mod qccmp.obj Makefile
	$(FC) $(FCFLAGS) ssverr.F90 bio.obj qccmp.obj /Fe$@ $(LDFLAGS)

bio.obj bio.mod: bio.F90 Makefile
	$(FC) $(FCFLAGS) /c bio.F90

seed.obj seed.mod: seed.F90 Makefile
	$(FC) $(FCFLAGS) /c seed.F90

svgen.obj svgen.mod: svgen.F90 seed.mod Makefile
	$(FC) $(FCFLAGS) /c svgen.F90

datgen.obj datgen.mod: datgen.F90 seed.mod Makefile
	$(FC) $(FCFLAGS) /c datgen.F90

qccmp.o: qccmp.F90 Makefile
	$(FC) $(FCFLAGS) /c qccmp.F90

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.obj
	-$(RM) *.optrpt
	-$(RM) *.pdb
