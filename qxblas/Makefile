!IFNDEF ABI
ABI=ilp64
!ENDIF # !ABI
!IFNDEF WP
WP=16
!ENDIF # !WP
!IFDEF NDEBUG
DEBUG=
!ELSE # DEBUG
DEBUG=d
!ENDIF # ?NDEBUG
RM=del /F
LIBS=qxblas$(WP)$(ABI)$(DEBUG).lib
AR=lib.exe
ARFLAGS=/NOLOGO /VERBOSE
FORT=ifx
FC=$(FORT).exe
FORFLAGS=/nologo /fpp /DUSE_INTEL /DUSE_X64 /DQX_WP=$(WP) /recursive /standard-semantics /traceback /Qvec-threshold:0
!IFNDEF MARCH
MARCH=Host
!ENDIF # !MARCH
!IFDEF NDEBUG
OPTFLAGS=/O$(NDEBUG) /Qx$(MARCH)
DBGFLAGS=/DNDEBUG
FPUFLAGS=/fp:precise /Qprotect-parens /Qfma /Qftz- /Qprec-div
DBGFLAGS=$(DBGFLAGS) /Qopt-report:3
LIBFLAGS=/I.. /MT
LDFLAGS=/link /RELEASE /LIBPATH:.. $(LIBS)
!ELSE # DEBUG
OPTFLAGS=/O$(DEBUG) /Qx$(MARCH)
DBGFLAGS=/debug:full /debug:inline-debug-info /debug-parameters:all /check:all /warn:all
FPUFLAGS=/fp:strict /Qprotect-parens /Qfma /Qftz- /Qprec-div /assume:ieee_fpe_flags
LIBFLAGS=/I.. /MTd
LDFLAGS=/link /DEBUG /LIBPATH:.. $(LIBS)
!ENDIF # ?NDEBUG
FFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFLAGS)

SOURCES=     \
iqamax.F90   \
ixamax.F90   \
qasum.F90    \
qaxpy.F90    \
qcopy.F90    \
qdot.F90     \
qgbmv.F90    \
qgemm.F90    \
qgemv.F90    \
qger.F90     \
qisnan.F90   \
qlacpy.F90   \
qlaev2.F90   \
qlagsy.F90   \
qlaisnan.F90 \
qlamch.F90   \
qlangb.F90   \
qlange.F90   \
qlansb.F90   \
qlansp.F90   \
qlansy.F90   \
qlapy2.F90   \
qlaran.F90   \
qlarfg.F90   \
qlarge.F90   \
qlarnd.F90   \
qlarnv.F90   \
qlaror.F90   \
qlartg.F90   \
qlaruv.F90   \
qlaset.F90   \
qlassq.F90   \
qnrm2.F90    \
qrot.F90     \
qrotg.F90    \
qrotm.F90    \
qrotmg.F90   \
qsbmv.F90    \
qscal.F90    \
qspmv.F90    \
qspr.F90     \
qspr2.F90    \
qswap.F90    \
qsymm.F90    \
qsymv.F90    \
qsyr.F90     \
qsyr2.F90    \
qsyr2k.F90   \
qsyrk.F90    \
qtbmv.F90    \
qtbsv.F90    \
qtpmv.F90    \
qtpsv.F90    \
qtrmm.F90    \
qtrmv.F90    \
qtrsm.F90    \
qtrsv.F90    \
qxabs1.F90   \
qxasum.F90   \
qxnrm2.F90   \
xaxpy.F90    \
xcopy.F90    \
xdotc.F90    \
xdotu.F90    \
xgbmv.F90    \
xgemm.F90    \
xgemv.F90    \
xgerc.F90    \
xgeru.F90    \
xhbmv.F90    \
xhemm.F90    \
xhemv.F90    \
xher.F90     \
xher2.F90    \
xher2k.F90   \
xherk.F90    \
xhpmv.F90    \
xhpr.F90     \
xhpr2.F90    \
xlacgv.F90   \
xlacpy.F90   \
xlaev2.F90   \
xlaghe.F90   \
xlarge.F90   \
xlarnd.F90   \
xlarnv.F90   \
xlaror.F90   \
xlaset.F90   \
xqrot.F90    \
xqscal.F90   \
xrotg.F90    \
xscal.F90    \
xswap.F90    \
xsymm.F90    \
xsyr2k.F90   \
xsyrk.F90    \
xtbmv.F90    \
xtbsv.F90    \
xtpmv.F90    \
xtpsv.F90    \
xtrmm.F90    \
xtrmv.F90    \
xtrsm.F90    \
xtrsv.F90

OBJS=        \
iqamax.obj   \
ixamax.obj   \
qasum.obj    \
qaxpy.obj    \
qcopy.obj    \
qdot.obj     \
qgbmv.obj    \
qgemm.obj    \
qgemv.obj    \
qger.obj     \
qisnan.obj   \
qlacpy.obj   \
qlaev2.obj   \
qlagsy.obj   \
qlaisnan.obj \
qlamch.obj   \
qlangb.obj   \
qlange.obj   \
qlansb.obj   \
qlansp.obj   \
qlansy.obj   \
qlapy2.obj   \
qlaran.obj   \
qlarfg.obj   \
qlarge.obj   \
qlarnd.obj   \
qlarnv.obj   \
qlaror.obj   \
qlartg.obj   \
qlaruv.obj   \
qlaset.obj   \
qlassq.obj   \
qnrm2.obj    \
qrot.obj     \
qrotg.obj    \
qrotm.obj    \
qrotmg.obj   \
qsbmv.obj    \
qscal.obj    \
qspmv.obj    \
qspr.obj     \
qspr2.obj    \
qswap.obj    \
qsymm.obj    \
qsymv.obj    \
qsyr.obj     \
qsyr2.obj    \
qsyr2k.obj   \
qsyrk.obj    \
qtbmv.obj    \
qtbsv.obj    \
qtpmv.obj    \
qtpsv.obj    \
qtrmm.obj    \
qtrmv.obj    \
qtrsm.obj    \
qtrsv.obj    \
qxabs1.obj   \
qxasum.obj   \
qxnrm2.obj   \
xaxpy.obj    \
xcopy.obj    \
xdotc.obj    \
xdotu.obj    \
xgbmv.obj    \
xgemm.obj    \
xgemv.obj    \
xgerc.obj    \
xgeru.obj    \
xhbmv.obj    \
xhemm.obj    \
xhemv.obj    \
xher.obj     \
xher2.obj    \
xher2k.obj   \
xherk.obj    \
xhpmv.obj    \
xhpr.obj     \
xhpr2.obj    \
xlacgv.obj   \
xlacpy.obj   \
xlaev2.obj   \
xlaghe.obj   \
xlarge.obj   \
xlarnd.obj   \
xlarnv.obj   \
xlaror.obj   \
xlaset.obj   \
xqrot.obj    \
xqscal.obj   \
xrotg.obj    \
xscal.obj    \
xswap.obj    \
xsymm.obj    \
xsyr2k.obj   \
xsyrk.obj    \
xtbmv.obj    \
xtbsv.obj    \
xtpmv.obj    \
xtpsv.obj    \
xtrmm.obj    \
xtrmv.obj    \
xtrsm.obj    \
xtrsv.obj

!IF "$(ABI)"=="ilp64"
FORFLAGS=$(FORFLAGS) /4I8
SOURCES=$(SOURCES) lsame.F90 xerbla.F90
OBJS=$(OBJS) lsame.obj xerbla.obj
!ENDIF # ilp64

all: ..\$(LIBS)

help:
	@echo "nmake.exe [MARCH=Host|...] [NDEBUG=d|1|2|3...] [ABI=ilp64|lp64] [WP=16|...] [all|clean|help]"

..\$(LIBS): $(OBJS) Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS)

.F90.obj:
	$(FC) $(FFLAGS) /c $<

clean:
	-$(RM) *.pdb
	-$(RM) *.optrpt
	-$(RM) *__genmod.f90
	-$(RM) *__genmod.mod
	-$(RM) $(OBJS)
	-$(RM) ..\$(LIBS)
