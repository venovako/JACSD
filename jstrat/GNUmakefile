ifndef SHELL
SHELL=/bin/bash
endif # !SHELL
ifndef ARCH
ARCH=$(shell uname)
endif # !ARCH
ifndef RM
RM=rm -fv
endif # !RM
ifeq ($(CPU),x64) # Xeon
AR=xiar
ARFLAGS=-qnoipo -lib rsv
CC=icc
C11FLAGS=-DUSE_INTEL -DUSE_X64 -std=c11 -fexceptions
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -xHost
DBGFLAGS=-DNDEBUG -qopt-report=5 -traceback -w3 -diag-disable=2547,10397
FPUFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0 -xHost
ifeq ($(ARCH),Darwin)
DBGFLAGS=-g -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check=stack,uninit -traceback -w3 -diag-disable=2547,10397
else # Linux
DBGFLAGS=-g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -check=stack,uninit -traceback -w3 -diag-disable=2547,10397
endif # ?Darwin
FPUFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt #-fp-model strict -fp-stack-check
endif # ?NDEBUG
LIBFLAGS=-pthread
else ifeq ($(CPU),x100) # Knights Corner
AR=xiar
ifdef NDEBUG
ARFLAGS=-lib rsv
else # DEBUG
ARFLAGS=-qnoipo -lib rsv
endif # ?NDEBUG
CC=icc
C11FLAGS=-DUSE_INTEL -DUSE_X100 -std=c11 -mmic -fexceptions
ifdef NDEBUG
OPTFLAGS=-fast #-O$(NDEBUG)
DBGFLAGS=-DNDEBUG -qopt-report=5 -traceback -diag-disable=2547,10397
FPUFLAGS=-fma #-fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0
DBGFLAGS=-g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -check=stack,uninit -w3 -traceback -diag-disable=2547,10397
FPUFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt #-fp-model strict -fp-stack-check
endif # ?NDEBUG
LIBFLAGS=-pthread
else ifeq ($(CPU),x200)
AR=xiar
ARFLAGS=-qnoipo -lib rsv
CC=icc
C11FLAGS=-DUSE_INTEL -DUSE_X200 -std=c11 -fexceptions
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -xMIC-AVX512 #-xHost
DBGFLAGS=-DNDEBUG -qopt-report=5 -traceback -w3 -diag-disable=2547,10397
FPUFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0 -xMIC-AVX512 #-xHost
DBGFLAGS=-g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -check=stack,uninit -traceback -w3 -diag-disable=2547,10397
FPUFLAGS=-fma -fp-model source -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt #-fp-model strict -fp-stack-check
endif # ?NDEBUG
LIBFLAGS=-pthread
else ifeq ($(CPU),power8) # IBM POWER8LE
AR=ar
ARFLAGS=rsv
CC=gcc
C11FLAGS=-DUSE_GNU -DUSE_PWR8 -std=gnu11
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -mcpu=power8 -fgcse-sm -fgcse-las -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
DBGFLAGS=-DNDEBUG -fopt-info-optimized-vec
FPUFLAGS=-ffp-contract=fast
else # DEBUG
OPTFLAGS=-Og -mcpu=power8
DBGFLAGS=-g
FPUFLAGS=-ffp-contract=fast
endif # ?NDEBUG
LIBFLAGS=-pthread
else ifeq ($(CPU),pwr8) # IBM POWER8LE / XL
AR=ar
ARFLAGS=rsv
CC=xlc_r
C11FLAGS=-DUSE_IBM -DUSE_PWR8 -qlanglvl=extc1x
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -qmaxmem=-1 -qaltivec=le -qarch=auto -qtune=pwr8:smt8 -qreport -qhot=level=1:vector -qprefetch=aggressive
DGBFLAGS=-DNDEBUG
FPUFLAGS=-qfloat=nans:spnans:subnormals -qstrict=nans:infinities:subnormals:zerosigns:operationprecision
else # DEBUG
OPTFLAGS=-O0 -qmaxmem=-1 -qaltivec=le -qarch=auto -qtune=pwr8:smt8 -qreport
DBGFLAGS=-g -qcheck
FPUFLAGS=-qfloat=nans:spnans:subnormals -qstrict=nans:infinities:subnormals:zerosigns:operationprecision
endif # ?NDEBUG
LIBFLAGS=-pthread
else # GNU/Clang
AR=ar
ARFLAGS=rsv
ifeq ($(ARCH),Darwin)
CC=clang
C11FLAGS=-DUSE_CLANG -DUSE_X64
else # Linux
CC=gcc
C11FLAGS=-DUSE_GNU -DUSE_X64 -std=gnu11
endif # ?Darwin
ifdef NDEBUG
ifeq ($(ARCH),Darwin)
OPTFLAGS=-O$(NDEBUG) -march=native -integrated-as
else # Linux
OPTFLAGS=-O$(NDEBUG) -march=native -fgcse-sm -fgcse-las -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
endif # ?Darwin
DBGFLAGS=-DNDEBUG -fopt-info-optimized-vec
ifeq ($(ARCH),Darwin)
FPUFLAGS=-ffp-contract=on
else
FPUFLAGS=-ffp-contract=fast
endif # ?Darwin
else # DEBUG
ifeq ($(ARCH),Darwin)
OPTFLAGS=-O0 -march=native -integrated-as
else # Linux
OPTFLAGS=-O0 -march=native
endif # ?Darwin
DBGFLAGS=-g -ftrapv
ifeq ($(ARCH),Darwin)
FPUFLAGS=-ffp-contract=on
else # Linux
FPUFLAGS=-ffp-contract=fast
endif # ?Darwin
endif # ?NDEBUG
LIBFLAGS=
endif # ?CPU
CFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(C11FLAGS) $(FPUFLAGS)

OBJS=jstrat.o
LIBS=libjstrat.a

all: ../$(LIBS)

help:
	@echo "make [CPU=x64|x100|power8|pwr8] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

../$(LIBS): $(OBJS) GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS)

jstrat.o: jstrat.c jstrat.h jstrat_ME.h GNUmakefile
	$(CC) $(CFLAGS) -c jstrat.c

clean:
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) ../$(LIBS)
