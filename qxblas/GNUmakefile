SHELL=/bin/bash
RM=rm -fv
ifndef WP
WP=16
endif # !WP
ifndef ARCH
ARCH=none
endif # !ARCH
ifeq ($(CPU),x64) # Xeon
AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=ifort
FORFLAGS=-DUSE_INTEL -i8 -fexceptions -standard-semantics
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -xHost
DBGFLAGS=-DNDEBUG -fno-omit-frame-pointer -qopt-report=5 -traceback -diag-disable=10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0 -xHost
ifeq ($(ARCH),MACINT)
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
else # Linux
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
endif # ?MACINT
FPUFLAGS=-fp-model strict -assume ieee_fpe_flags -fma -fp-stack-check -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
endif # ?NDEBUG
LIBFLAGS=-threads -I..
else ifeq ($(CPU),x100) # Knights Corner
AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=ifort
FORFLAGS=-DUSE_INTEL -DUSE_KNC -mmic -i8 -fexceptions -standard-semantics
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG)
DBGFLAGS=-DNDEBUG -fno-omit-frame-pointer -qopt-report=5 -traceback -diag-disable=10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
FPUFLAGS=-fp-model strict -assume ieee_fpe_flags -fma -fp-stack-check -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
endif # ?NDEBUG
LIBFLAGS=-threads -I..
else ifeq ($(CPU),pwr8) # Power8
AR=ar
ARFLAGS=rsv
FC=xlf2008_r
FORFLAGS=-WF,-DUSE_IBM -qintsize=8 -qnosave -qsclk=micro -qsmp=omp -qlanglvl=extended -qassert=contig
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -qmaxmem=-1 -qtune=pwr8:smt8 -qhot=level=2:vector -qipa=level=2:partition=large -qprefetch=aggressive
DBGFLAGS=-WF,-DNDEBUG -qinfo=mt:unset
FPUFLAGS=-qfloat=nans:subnormals -qstrict=nans:infinities:subnormals:zerosigns:operationprecision
else # DEBUG
OPTFLAGS=-O0 -qmaxmem=-1 -qtune=pwr8:smt8
DBGFLAGS=-g -C -qinfo=mt:unset
FPUFLAGS=-qfloat=nans:subnormals
endif # ?NDEBUG
LIBFLAGS=-I..
else # GNU Fortran
AR=ar
ARFLAGS=rsv
FC=gfortran
FORFLAGS=-DUSE_GNU -fdefault-integer-8 -fexceptions -frecursive
ifdef NDEBUG
ifeq ($(ARCH),MACINT)
OPTFLAGS=-O$(NDEBUG) -march=native -Wa,-q
else # Linux
OPTFLAGS=-O$(NDEBUG) -march=native
endif # ?MACINT
DBGFLAGS=-DNDEBUG
FPUFLAGS=
else # DEBUG
ifeq ($(ARCH),MACINT)
OPTFLAGS=-O0 -march=native -Wa,-q
else # Linux
OPTFLAGS=-O0 -march=native
endif # ?MACINT
DBGFLAGS=-g -fcheck=all -finit-local-zero -finit-real=snan
FPUFLAGS=-ffpe-trap=invalid,zero,overflow
endif # ?NDEBUG
LIBFLAGS=-I..
endif # ?CPU
FCFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFLAGS)
LIBS=libqxblas.a
LDFLAGS=-L.. -lqxblas

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
OBJS=$(SOURCES:.f90=.o)

.PHONY: all help prepare clean

all: ../$(LIBS)

help:
	@echo "make [WP=4|8|10|16] [CPU=x64|x100|pwr8] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

prepare:
	echo INTEGER, PARAMETER :: WP = $(WP) > ../qx_wp.fi

../$(LIBS): prepare $(OBJS) GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS)

%.o: %.f90 GNUmakefile
	$(FC) $(FCFLAGS) -c $< -o $@

clean:
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) ../$(LIBS)
	-$(RM) ../qx_wp.fi
	-$(RM) *__genmod.mod
	-$(RM) *__genmod.f90
