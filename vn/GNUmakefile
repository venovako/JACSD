ifeq ($(CPU),x64) # Xeon / Intel C
include ../x64.mk
MKFS=GNUmakefile ../x64.mk
LIBFLAGS=-DUSE_MKL -mkl -qopenmp # -DMKL_DIRECT_CALL
else ifeq ($(CPU),x100) # Knights Corner / Intel C
include ../x100.mk
MKFS=GNUmakefile ../x100.mk
LIBFLAGS=-DUSE_MKL -mkl -qopenmp # -DMKL_DIRECT_CALL
else ifeq ($(CPU),x200) # Knights Landing / Intel C
include ../x200.mk
MKFS=GNUmakefile ../x200.mk
LIBFLAGS=-DUSE_MKL -mkl -qopenmp # -DMKL_DIRECT_CALL
else ifeq ($(CPU),power8) # IBM POWER8LE / GNU C
include ../power8.mk
MKFS=GNUmakefile ../power8.mk
LIBFLAGS=-DUSE_ATLAS -I$(HOME)/atlas/include -fopenmp
else ifeq ($(CPU),pwr8) # IBM POWER8LE / XL C
include ../pwr8.mk
MKFS=GNUmakefile ../pwr8.mk
LIBFLAGS=-DUSE_ESSL -qsmp=omp
else # GNU/Clang C
include ../gnu.mk
MKFS=GNUmakefile ../gnu.mk
ifeq ($(ARCH),Darwin)
LIBFLAGS=-DUSE_MKL -I. -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include # -DMKL_DIRECT_CALL
else # Linux
LIBFLAGS=-DUSE_MKL -I. -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include -fopenmp # -DMKL_DIRECT_CALL
endif # ?Darwin
endif # ?CPU
CFLAGS=$(OPTCFLAGS) $(DBGCFLAGS) $(LIBFLAGS) $(C11FLAGS) $(FPUCFLAGS)

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

.PHONY: all help clean

all: ../$(LIBS)

help:
	@echo "make [CPU=x64|x100|power8|pwr8] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

../$(LIBS): $(OBJS) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

vn_align.o: vn_align.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_alloc.o: vn_alloc.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_assert.o: vn_assert.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_attrs.o: vn_attrs.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_blas.o: vn_blas.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_bmp.o: vn_bmp.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_error.o: vn_error.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_lapack.o: vn_lapack.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_lib.o: vn_lib.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_mtxvis.o: vn_mtxvis.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_simd.o: vn_simd.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_timer.o: vn_timer.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_types.o: vn_types.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

clean:
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) ../$(LIBS)
	-$(RM) *.dSYM
