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
LIBS=qxblas.lib
!IFDEF NDEBUG
OPTFLAGS=/O$(NDEBUG) /QxHost
DBGFLAGS=/DNDEBUG /Qopt-report:5 /traceback
FPUFLAGS=/fp:source /Qfma /Qftz- /Qcomplex-limited-range- /Qfast-transcendentals- /Qprec-div /Qprec-sqrt
LIBFLAGS=/I.. /libs:dll /threads
LDFLAGS=/link /RELEASE /LIBPATH:.. $(LIBS)
!ELSE # DEBUG
OPTFLAGS=/Od /QxHost
DBGFLAGS=/debug:full /debug:inline-debug-info /debug-parameters:all /check:all /warn:all /traceback
FPUFLAGS=/fp:strict /assume:ieee_fpe_flags /Qfma /Qfp-stack-check /Qftz- /Qcomplex-limited-range- /Qfast-transcendentals- /Qprec-div /Qprec-sqrt
LIBFLAGS=/I.. /libs:dll /threads /dbglibs
LDFLAGS=/link /DEBUG /LIBPATH:.. $(LIBS)
!ENDIF # ?NDEBUG
FCFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFLAGS)

SOURCES=     \
iqamax.f90   \
ixamax.f90   \
qasum.f90    \
qaxpy.f90    \
qcopy.f90    \
qdot.f90     \
qgbmv.f90    \
qgemm.f90    \
qgemv.f90    \
qger.f90     \
qisnan.f90   \
qlacpy.f90   \
qlaisnan.f90 \
qlamch.f90   \
qlangb.f90   \
qlange.f90   \
qlansb.f90   \
qlansp.f90   \
qlansy.f90   \
qlapy2.f90   \
qlaran.f90   \
qlarfg.f90   \
qlarnd.f90   \
qlaror.f90   \
qlartg.f90   \
qlaset.f90   \
qlassq.f90   \
qnrm2.f90    \
qrot.f90     \
qrotg.f90    \
qrotm.f90    \
qrotmg.f90   \
qsbmv.f90    \
qscal.f90    \
qspmv.f90    \
qspr.f90     \
qspr2.f90    \
qswap.f90    \
qsymm.f90    \
qsymv.f90    \
qsyr.f90     \
qsyr2.f90    \
qsyr2k.f90   \
qsyrk.f90    \
qtbmv.f90    \
qtbsv.f90    \
qtpmv.f90    \
qtpsv.f90    \
qtrmm.f90    \
qtrmv.f90    \
qtrsm.f90    \
qtrsv.f90    \
qxabs1.f90   \
qxasum.f90   \
qxnrm2.f90   \
xaxpy.f90    \
xcopy.f90    \
xdotc.f90    \
xdotu.f90    \
xgbmv.f90    \
xgemm.f90    \
xgemv.f90    \
xgerc.f90    \
xgeru.f90    \
xhbmv.f90    \
xhemm.f90    \
xhemv.f90    \
xher.f90     \
xher2.f90    \
xher2k.f90   \
xherk.f90    \
xhpmv.f90    \
xhpr.f90     \
xhpr2.f90    \
xqrot.f90    \
xqscal.f90   \
xrotg.f90    \
xscal.f90    \
xswap.f90    \
xsymm.f90    \
xsyr2k.f90   \
xsyrk.f90    \
xtbmv.f90    \
xtbsv.f90    \
xtpmv.f90    \
xtpsv.f90    \
xtrmm.f90    \
xtrmv.f90    \
xtrsm.f90    \
xtrsv.f90

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
qlarnd.obj   \
qlaror.obj   \
qlartg.obj   \
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

all: ..\$(LIBS)

help:
	@echo "nmake.exe [WP=4|8|16] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

prepare:
	echo INTEGER, PARAMETER :: WP = $(WP) > ..\qx_wp.fi

..\$(LIBS): prepare $(OBJS) Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS)

.f90.obj:
	$(FC) $(FCFLAGS) /c $<

clean:
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) ..\$(LIBS)
	-$(RM) ..\qx_wp.fi
