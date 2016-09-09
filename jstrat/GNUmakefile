SHELL=/bin/bash
RM=rm -fv
ifndef ARCH
ARCH=none
endif # !ARCH
ifeq ($(CPU),x64) # Xeon
AR=xiar
ARFLAGS=-qnoipo -lib rsv
CC=icc
C11FLAGS=-DUSE_INTEL -std=c11 -fexceptions
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -xHost
DBGFLAGS=-DNDEBUG -fno-omit-frame-pointer -qopt-report=5 -traceback -w3 -diag-disable=2547,10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0 -xHost
ifeq ($(ARCH),MACINT)
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check=stack,uninit -traceback -w3 -diag-disable=2547,10397
else # Linux
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -check=stack,uninit -traceback -w3 -diag-disable=2547,10397
endif # ?MACINT
FPUFLAGS=-fp-model strict -fma -fp-stack-check -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
endif # ?NDEBUG
LIBFLAGS=-pthread
else ifeq ($(CPU),x100) # Knights Corner
AR=xiar
ARFLAGS=-qnoipo -lib rsv
CC=icc
C11FLAGS=-DUSE_INTEL -DUSE_KNC -std=c11 -mmic -fexceptions
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG)
DBGFLAGS=-DNDEBUG -fno-omit-frame-pointer -qopt-report=5 -traceback -diag-disable=2547,10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -check=stack,uninit -w3 -traceback -diag-disable=2547,10397
FPUFLAGS=-fp-model strict -fma -fp-stack-check -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
endif # ?NDEBUG
LIBFLAGS=-pthread
else ifeq ($(CPU),power8) # IBM POWER8LE
AR=ar
ARFLAGS=rsv
CC=gcc
C11FLAGS=-DUSE_IBM -std=gnu11
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
C11FLAGS=-DUSE_IBM -qlanglvl=extc1x
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -qmaxmem=-1 -qaltivec=le -qarch=auto -qtune=pwr8:smt8 -qhot=level=1:vector -qprefetch=aggressive
DGBFLAGS=-DNDEBUG
FPUFLAGS=-qfloat=nans:spnans:subnormals -qstrict=nans:infinities:subnormals:zerosigns:operationprecision
else # DEBUG
OPTFLAGS=-O0 -qmaxmem=-1 -qaltivec=le -qarch=auto -qtune=pwr8:smt8
DBGFLAGS=-g -qcheck
FPUFLAGS=-qfloat=nans:spnans:subnormals -qstrict=nans:infinities:subnormals:zerosigns:operationprecision
endif # ?NDEBUG
LIBFLAGS=-pthread
else # GNU/Clang
AR=ar
ARFLAGS=rsv
ifeq ($(ARCH),MACINT)
CC=clang
C11FLAGS=-DUSE_CLANG
else # Linux
CC=gcc
C11FLAGS=-DUSE_GNU -std=gnu99
endif # ?MACINT
ifdef NDEBUG
ifeq ($(ARCH),MACINT)
OPTFLAGS=-O$(NDEBUG) -march=native -integrated-as
else # Linux
OPTFLAGS=-O$(NDEBUG) -march=native
endif # ?MACINT
DBGFLAGS=-DNDEBUG
ifeq ($(ARCH),MACINT)
FPUFLAGS=-ffp-contract=on
else
FPUFLAGS=-ffp-contract=fast
endif # ?MACINT
else # DEBUG
ifeq ($(ARCH),MACINT)
OPTFLAGS=-O0 -march=native -integrated-as
else # Linux
OPTFLAGS=-O0 -march=native
endif # ?MACINT
DBGFLAGS=-g -ftrapv
ifeq ($(ARCH),MACINT)
FPUFLAGS=-ffp-contract=on
else # Linux
FPUFLAGS=-ffp-contract=fast
endif # ?MACINT
endif # ?NDEBUG
LIBFLAGS=
endif # ?CPU
CFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(C11FLAGS) $(FPUFLAGS)

OBJS=jstrat.o
LIBS=libjstrat.a

all: ../$(LIBS)

help:
	@echo "make [CPU=x64|x100|power8] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

../$(LIBS): $(OBJS) GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS)

jstrat.o: jstrat.c jstrat.h jstrat_ME.h GNUmakefile
	$(CC) $(CFLAGS) -c jstrat.c

clean:
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) ../$(LIBS)
