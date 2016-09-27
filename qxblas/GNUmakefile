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
ifeq ($(CPU),x64) # Xeon
AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=ifort
FORFLAGS=-DUSE_INTEL -DUSE_X64 -i8 -fexceptions -standard-semantics
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -xHost
DBGFLAGS=-DNDEBUG -fno-omit-frame-pointer -qopt-report=5 -traceback -diag-disable=10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0 -xHost
ifeq ($(ARCH),Darwin)
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
else # Linux
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
endif # ?Darwin
FPUFLAGS=-fp-model strict -assume ieee_fpe_flags -fma -fp-stack-check -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
endif # ?NDEBUG
LIBFLAGS=-threads -I..
else ifeq ($(CPU),x100) # Knights Corner
AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=ifort
FORFLAGS=-DUSE_INTEL -DUSE_KNL -mmic -i8 -fexceptions -standard-semantics
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG)
DBGFLAGS=-DNDEBUG -fno-omit-frame-pointer -qopt-report=5 -traceback -diag-disable=10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt # -fp-model strict -assume ieee_fpe_flags -fp-stack-check
endif # ?NDEBUG
LIBFLAGS=-threads -I..
else ifeq ($(CPU),x200) # Knights Landing
AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=ifort
FORFLAGS=-DUSE_INTEL -DUSE_X200 -i8 -fexceptions -standard-semantics
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -xMIC-AVX512 #-xHost
DBGFLAGS=-DNDEBUG -fno-omit-frame-pointer -qopt-report=5 -traceback -diag-disable=10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0 -xMIC-AVX512 #-xHost
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -debug-parameters all -check all -warn all -traceback -diag-disable=10397
FPUFLAGS=-fp-model strict -assume ieee_fpe_flags -fma -fp-stack-check -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
endif # ?NDEBUG
LIBFLAGS=-threads -I..
else ifeq ($(CPU),power8) # IBM POWER8LE
AR=ar
ARFLAGS=rsv
FC=gfortran
FORFLAGS=-DUSE_GNU -DUSE_PWR8 -fdefault-integer-8 -frecursive -fstack-arrays -fopenmp
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -mcpu=power8 -fgcse-sm -fgcse-las -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
DBGFLAGS=-DNDEBUG -fopt-info-optimized-vec
FPUFLAGS=
else # DEBUG
OPTFLAGS=-Og -mcpu=power8
DBGFLAGS=-g -fcheck=all -finit-local-zero -finit-real=snan
FPUFLAGS=-ffpe-trap=invalid,zero,overflow
endif # ?NDEBUG
LIBFLAGS=-I..
else ifeq ($(CPU),pwr8) # IBM POWER8LE / XL
AR=ar
ARFLAGS=rsv
FC=xlf2008_r
FORFLAGS=-WF,-DUSE_IBM -WF,-DUSE_PWR8 -qintsize=8 -qnosave -qsclk=micro -qsmp=omp -qreport=smplist -qlanglvl=extended -qassert=contig
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -qmaxmem=-1 -qarch=auto -qtune=pwr8:smt8 -qhot=level=2:vector -qprefetch=aggressive
DBGFLAGS=-WF,-DNDEBUG -qinfo=mt #:unset
FPUFLAGS=-qfloat=nans:subnormals -qstrict=nans:infinities:subnormals:zerosigns:operationprecision
else # DEBUG
OPTFLAGS=-O0 -qmaxmem=-1 -qarch=auto -qtune=pwr8:smt8
DBGFLAGS=-g -C -qinfo=mt #:unset
FPUFLAGS=-qfloat=nans:subnormals
endif # ?NDEBUG
LIBFLAGS=-I..
else # GNU Fortran
AR=ar
ARFLAGS=rsv
FC=gfortran
FORFLAGS=-DUSE_GNU -DUSE_X64 -fdefault-integer-8 -frecursive -fstack-arrays
ifdef NDEBUG
ifeq ($(ARCH),Darwin)
OPTFLAGS=-O$(NDEBUG) -march=native -Wa,-q -fgcse-sm -fgcse-las -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
else # Linux
OPTFLAGS=-O$(NDEBUG) -march=native -fgcse-sm -fgcse-las -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
endif # ?Darwin
DBGFLAGS=-DNDEBUG -fopt-info-optimized-vec
FPUFLAGS=
else # DEBUG
ifeq ($(ARCH),Darwin)
OPTFLAGS=-O0 -march=native -Wa,-q
else # Linux
OPTFLAGS=-O0 -march=native
endif # ?Darwin
DBGFLAGS=-g -fcheck=all -finit-local-zero -finit-real=snan
FPUFLAGS=-ffpe-trap=invalid,zero,overflow
endif # ?NDEBUG
LIBFLAGS=-I..
endif # ?CPU
FCFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFLAGS)
LIBS=libqxblas.a
LDFLAGS=-L.. -lqxblas

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
qlarnd.F90   \
qlaror.F90   \
qlartg.F90   \
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
OBJS=$(SOURCES:.F90=.o)

.PHONY: all help prepare clean

all: ../$(LIBS)

help:
	@echo "make [WP=4|8|10|16] [CPU=x64|x100|power8|pwr8] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

prepare:
	echo INTEGER, PARAMETER :: WP = $(WP) > ../qx_wp.fi

../$(LIBS): prepare $(OBJS) GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS)

%.o: %.F90 GNUmakefile
	$(FC) $(FCFLAGS) -c $< -o $@

clean:
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) ../$(LIBS)
	-$(RM) ../qx_wp.fi
	-$(RM) *__genmod.mod
	-$(RM) *__genmod.f90
