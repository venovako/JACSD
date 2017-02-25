ifeq ($(CPU),x64) # Xeon / Intel C
include ../x64.mk
LIBFLAGS=-mkl -pthread
else ifeq ($(CPU),x100) # Knights Corner / Intel C
include ../x100.mk
LIBFLAGS=-mkl -pthread
else ifeq ($(CPU),x200) # Knights Landing / Intel C
include ../x200.mk
LIBFLAGS=-mkl -pthread
else ifeq ($(CPU),power8) # IBM POWER8LE / GNU C
include ../power8.mk
LIBFLAGS=-DUSE_ATLAS -I$(HOME)/atlas/include -pthread
else ifeq ($(CPU),pwr8) # IBM POWER8LE / XL C
include ../pwr8.mk
LIBFLAGS=-DUSE_ESSL -pthread
else # GNU/Clang C
include ../gnu.mk
LIBFLAGS=-DUSE_OPENBLAS -I$(HOME)/OpenBLAS/include
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
MKFS=GNUmakefile ../gnu.mk ../power8.mk ../pwr8.mk ../x100.mk ../x200.mk ../x64.mk
LIBS=libvn.a

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
