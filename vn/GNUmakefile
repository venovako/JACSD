ifndef SHELL
SHELL=/bin/bash
endif
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
C11FLAGS=-DUSE_INTEL -DUSE_X64 -DVN_INTEGER_KIND=8 -std=c11 -fexceptions
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -xHost
DBGFLAGS=-DNDEBUG -fno-omit-frame-pointer -qopt-report=5 -traceback -w3 -diag-disable=1572,2547,10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0 -xHost
ifeq ($(ARCH),Darwin)
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check=stack,uninit -traceback -w3 -diag-disable=1572,2547,10397
else # Linux
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -check=stack,uninit -traceback -w3 -diag-disable=1572,2547,10397
endif # ?Darwin
FPUFLAGS=-fp-model strict -fma -fp-stack-check -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
endif # ?NDEBUG
LIBFLAGS=-mkl -pthread
else ifeq ($(CPU),x100) # Knights Corner
AR=xiar
ARFLAGS=-qnoipo -lib rsv
CC=icc
C11FLAGS=-DUSE_INTEL -DUSE_X100 -DVN_INTEGER_KIND=8 -std=c11 -mmic -fexceptions
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG)
DBGFLAGS=-DNDEBUG -fno-omit-frame-pointer -qopt-report=5 -traceback -diag-disable=1572,2547,10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -check=stack,uninit -w3 -traceback -diag-disable=1572,2547,10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt # -fp-model strict -fp-stack-check
endif # ?NDEBUG
LIBFLAGS=-mkl -pthread
else ifeq ($(CPU),x200) # Knights Landing
AR=xiar
ARFLAGS=-qnoipo -lib rsv
CC=icc
C11FLAGS=-DUSE_INTEL -DUSE_X200 -DVN_INTEGER_KIND=8 -std=c11 -fexceptions
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -xMIC-AVX512 #-xHost
DBGFLAGS=-DNDEBUG -fno-omit-frame-pointer -qopt-report=5 -traceback -w3 -diag-disable=1572,2547,10397
FPUFLAGS=-fp-model source -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
else # DEBUG
OPTFLAGS=-O0 -xMIC-AVX512 #-xHost
DBGFLAGS=-fno-omit-frame-pointer -g -debug emit_column -debug extended -debug inline-debug-info -debug parallel -debug pubnames -check=stack,uninit -traceback -w3 -diag-disable=1572,2547,10397
FPUFLAGS=-fp-model strict -fma -fp-stack-check -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt
endif # ?NDEBUG
LIBFLAGS=-mkl -pthread
else ifeq ($(CPU),power8) # IBM POWER8LE
AR=ar
ARFLAGS=rsv
CC=gcc
C11FLAGS=-DUSE_GNU -DUSE_PWR8 -DVN_INTEGER_KIND=8 -std=gnu11
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -mcpu=power8 -fgcse-sm -fgcse-las -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
DGBFLAGS=-DNDEBUG -fopt-info-optimized-vec
FPUFLAGS=-ffp-contract=fast
else # DEBUG
OPTFLAGS=-Og -mcpu=power8
DBGFLAGS=-g
FPUFLAGS=-ffp-contract=fast
endif # ?NDEBUG
LIBFLAGS=-DUSE_OPENBLAS -I$(HOME)/OpenBLAS-gcc/include #-L$(HOME)/OpenBLAS-gcc/lib -lopenblas_omp
else ifeq ($(CPU),pwr8) # IBM POWER8LE / XL
AR=ar
ARFLAGS=rsv
CC=xlc_r
C11FLAGS=-DUSE_IBM -DUSE_PWR8 -DVN_INTEGER_KIND=8 -qlanglvl=extc1x
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -qmaxmem=-1 -qaltivec=le -qarch=auto -qtune=pwr8:smt8 -qreport -qhot=level=1:vector -qprefetch=aggressive
DGBFLAGS=-DNDEBUG
FPUFLAGS=-qfloat=nans:spnans:subnormals -qstrict=nans:infinities:subnormals:zerosigns:operationprecision
else # DEBUG
OPTFLAGS=-O0 -qmaxmem=-1 -qaltivec=le -qarch=auto -qtune=pwr8:smt8 -qreport
DBGFLAGS=-g -qcheck
FPUFLAGS=-qfloat=nans:spnans:subnormals -qstrict=nans:infinities:subnormals:zerosigns:operationprecision
endif # ?NDEBUG
LIBFLAGS=-DUSE_ESSL -L/usr/lib64 -lesslsmp -lessl
#LIBFLAGS=-DUSE_OPENBLAS -I$(HOME)/OpenBLAS-ibm/include #-DUSE_ESSL #-L/usr/lib64 -lesslsmp6464 -lessl6464
else # GNU/Clang
ifeq ($(ARCH),Darwin)
CC=clang
C11FLAGS=-DUSE_CLANG -DUSE_X64 -DVN_INTEGER_KIND=8 -std=gnu11
else # Linux
CC=gcc
C11FLAGS=-DUSE_GNU -DUSE_X64 -DVN_INTEGER_KIND=8 -std=gnu11
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
OPTFLAGS=-Og -march=native
endif # ?Darwin
DBGFLAGS=-g -ftrapv
ifeq ($(ARCH),Darwin)
FPUFLAGS=-ffp-contract=on
else # Linux
FPUFLAGS=-ffp-contract=fast
endif # ?Darwin
endif # ?NDEBUG
LIBFLAGS=-DUSE_OPENBLAS -I$(HOME)/OpenBLAS-gcc/include #-L$(HOME)/OpenBLAS-gcc/lib -lopenblas_omp
endif # ?CPU
CFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(C11FLAGS) $(FPUFLAGS)

SRC=\
vn_align.c  \
vn_alloc.c  \
vn_assert.c \
vn_attrs.c  \
vn_blas.c   \
vn_bmp.c    \
vn_error.c  \
vn_lapack.c \
vn_lib.c    \
vn_mtxvis.c \
vn_simd.c   \
vn_timer.c  \
vn_types.c

OBJS=$(SRC:.c=.o)
HDRS=$(SRC:.c=.h)
LIBS=libvn.a

all: ../$(LIBS)

help:
	@echo "make [CPU=x64|x100|power8|pwr8] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

../$(LIBS): $(OBJS) GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS)

vn_align.o: vn_align.c $(HDRS) GNUmakefile
	$(CC) $(CFLAGS) -c $<

vn_alloc.o: vn_alloc.c $(HDRS) GNUmakefile
	$(CC) $(CFLAGS) -c $<

vn_assert.o: vn_assert.c $(HDRS) GNUmakefile
	$(CC) $(CFLAGS) -c $<

vn_attrs.o: vn_attrs.c $(HDRS) GNUmakefile
	$(CC) $(CFLAGS) -c $<

vn_blas.o: vn_blas.c $(HDRS) GNUmakefile
	$(CC) $(CFLAGS) -c $<

vn_bmp.o: vn_bmp.c $(HDRS) GNUmakefile
	$(CC) $(CFLAGS) -c $<

vn_error.o: vn_error.c $(HDRS) GNUmakefile
	$(CC) $(CFLAGS) -c $<

vn_lapack.o: vn_lapack.c $(HDRS) GNUmakefile
	$(CC) $(CFLAGS) -c $<

vn_lib.o: vn_lib.c $(HDRS) GNUmakefile
	$(CC) $(CFLAGS) -c $<

vn_mtxvis.o: vn_mtxvis.c $(HDRS) GNUmakefile
	$(CC) $(CFLAGS) -c $<

vn_simd.o: vn_simd.c $(HDRS) GNUmakefile
	$(CC) $(CFLAGS) -c $<

vn_timer.o: vn_timer.c $(HDRS) GNUmakefile
	$(CC) $(CFLAGS) -c $<

vn_types.o: vn_types.c $(HDRS) GNUmakefile
	$(CC) $(CFLAGS) -c $<

clean:
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) ../$(LIBS)
