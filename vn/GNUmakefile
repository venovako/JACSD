ifeq ($(CPU),x64) # Xeon / Intel C
include ../x64.mk
MKFS=GNUmakefile ../x64.mk
else ifeq ($(CPU),x100) # Knights Corner / Intel C
include ../x100.mk
MKFS=GNUmakefile ../x100.mk
else ifeq ($(CPU),x200) # Knights Landing / Intel C
include ../x200.mk
MKFS=GNUmakefile ../x200.mk
else # GNU C
include ../gnu.mk
MKFS=GNUmakefile ../gnu.mk
endif # ?CPU
LDFLAGS:=-L.. -lvn $(LDFLAGS)

SRC=\
vn_align.c  \
vn_alloc.c  \
vn_assert.c \
vn_attrs.c  \
vn_binio.c  \
vn_blas.c   \
vn_bmp.c    \
vn_error.c  \
vn_lapack.c \
vn_lib.c    \
vn_mtxvis.c \
vn_simd.c   \
vn_stdc11.c \
vn_timer.c  \
vn_types.c

OBJS=$(SRC:.c=.o)
HDRS=$(SRC:.c=.h)
EXES=$(SRC:.c=.exe)

.PHONY: all test help clean

all: ../libvn.a

test: ../libvn.a $(EXES)

help:
	@echo "make [CPU=x64|x100|x200] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

../libvn.a: $(OBJS) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

vn_align.o: vn_align.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_align.exe: vn_align.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_alloc.o: vn_alloc.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_alloc.exe: vn_alloc.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_assert.o: vn_assert.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_assert.exe: vn_assert.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_attrs.o: vn_attrs.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_attrs.exe: vn_attrs.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_binio.o: vn_binio.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_binio.exe: vn_binio.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_blas.o: vn_blas.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_blas.exe: vn_blas.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_bmp.o: vn_bmp.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_bmp.exe: vn_bmp.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_error.o: vn_error.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_error.exe: vn_error.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_lapack.o: vn_lapack.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_lapack.exe: vn_lapack.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_lib.o: vn_lib.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_lib.exe: vn_lib.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_mtxvis.o: vn_mtxvis.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_mtxvis.exe: vn_mtxvis.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_simd.o: vn_simd.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_simd.exe: vn_simd.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_stdc11.o: vn_stdc11.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_stdc11.exe: vn_stdc11.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_timer.o: vn_timer.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_timer.exe: vn_timer.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_types.o: vn_types.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_types.exe: vn_types.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

clean:
	-$(RM) ../libvn.a
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) *.exe
	-$(RM) *.dSYM
