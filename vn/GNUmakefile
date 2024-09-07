ifndef COMPILER
COMPILER=x64x
endif # !COMPILER
include ../$(COMPILER).mk
MKFS=GNUmakefile ../$(COMPILER).mk

SRCC=         \
vn_align.c    \
vn_alloc.c    \
vn_assert.c   \
vn_attrs.c    \
vn_binio.c    \
vn_blas.c     \
vn_bmp.c      \
vn_cmplxvis.c \
vn_error.c    \
vn_lapack.c   \
vn_lib.c      \
vn_lock.c     \
vn_mtxvis.c   \
vn_profile.c  \
vn_simd.c     \
vn_stdc11.c   \
vn_timer.c    \
vn_types.c    \
vn_variant.c

SRCF=              \
vn_alloc_f.F90     \
vn_aux_f.F90       \
vn_binio_f.F90     \
vn_blas_f.F90      \
vn_cmplxvis_f.F90  \
vn_imaginary_f.F90 \
vn_mtxvis_f.F90    \
vn_lib_f.F90       \
vn_sort_f.F90      \
vn_timer_f.F90     \
vn_types_f.F90

SRC=$(SRCC) $(SRCF)
OBJSC=$(SRCC:.c=.o)
OBJSF=$(SRCF:.F90=.o)
OBJS=$(OBJSC) $(OBJSF)
HDRS=$(SRCC:.c=.h)
MODS=$(SRCF:.F90=.mod)
EXES=$(SRCC:.c=.exe)

.PHONY: all test help clean

all: ../libvn$(DEBUG).a

test: ../libvn$(DEBUG).a $(EXES)

help:
	@echo "gmake [COMPILER=x64x|x200|gnu] [NDEBUG=0|1|2|3|4|5] [ABI=ilp64|lp64] [all|clean|help]"

../libvn$(DEBUG).a: $(OBJS) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

vn_align.o: vn_align.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_align.exe: vn_align.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_alloc.o: vn_alloc.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_alloc_f.o vn_alloc_f.mod: vn_alloc_f.F90 $(HDRS) $(MKFS)
	$(FC) $(FFLAGS) -c $<

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

vn_aux_f.o vn_aux_f.mod: vn_aux_f.F90 $(HDRS) $(MKFS)
	$(FC) $(FFLAGS) -c $<

vn_binio.o: vn_binio.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_binio_f.o vn_binio_f.mod: vn_binio_f.F90 $(HDRS) $(MKFS)
	$(FC) $(FFLAGS) -c $<

vn_binio.exe: vn_binio.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_blas.o: vn_blas.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_blas_f.o vn_blas_f.mod: vn_blas_f.F90 $(HDRS) $(MKFS)
	$(FC) $(FFLAGS) -c $<

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

vn_cmplxvis.o: vn_cmplxvis.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_cmplxvis_f.o vn_cmplxvis_f.mod: vn_cmplxvis_f.F90 $(HDRS) $(MKFS)
	$(FC) $(FFLAGS) -c $<

vn_cmplxvis.exe: vn_cmplxvis.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_imaginary_f.o vn_imaginary_f.mod: vn_imaginary_f.F90 $(HDRS) $(MKFS)
	$(FC) $(FFLAGS) -c $<

vn_lapack.o: vn_lapack.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_lapack.exe: vn_lapack.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_lib.o: vn_lib.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_lib_f.o vn_lib_f.mod: vn_lib_f.F90 vn_alloc_f.mod vn_binio_f.mod vn_blas_f.mod vn_cmplxvis_f.mod vn_imaginary_f.mod vn_mtxvis_f.mod vn_sort_f.mod vn_timer_f.mod vn_types_f.mod $(HDRS) $(MKFS)
	$(FC) $(FFLAGS) -c $<

vn_lib.exe: vn_lib.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_lock.o: vn_lock.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_lock.exe: vn_lock.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_mtxvis.o: vn_mtxvis.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_mtxvis_f.o vn_mtxvis_f.mod: vn_mtxvis_f.F90 $(HDRS) $(MKFS)
	$(FC) $(FFLAGS) -c $<

vn_mtxvis.exe: vn_mtxvis.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_profile.o: vn_profile.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_profile.exe: vn_profile.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_simd.o: vn_simd.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_simd.exe: vn_simd.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_sort_f.o vn_sort_f.mod: vn_sort_f.F90 $(HDRS) $(MKFS)
	$(FC) $(FFLAGS) -c $<

vn_stdc11.o: vn_stdc11.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_stdc11.exe: vn_stdc11.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_timer.o: vn_timer.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_timer_f.o vn_timer_f.mod: vn_timer_f.F90 $(HDRS) $(MKFS)
	$(FC) $(FFLAGS) -c $<

vn_timer.exe: vn_timer.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_types.o: vn_types.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_types_f.o vn_types_f.mod: vn_types_f.F90 $(HDRS) $(MKFS)
	$(FC) $(FFLAGS) $(KIND_FLAGS) -c $<

vn_types.exe: vn_types.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

vn_variant.o: vn_variant.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -c $<

vn_variant.exe: vn_variant.c $(HDRS) $(MKFS)
	$(CC) $(CFLAGS) -DVN_TEST $< -o $@ $(LDFLAGS)

clean:
	-$(RM) ../libvn$(DEBUG).a
	-$(RM) *.optrpt
	-$(RM) *.opt.yaml
	-$(RM) $(OBJS)
	-$(RM) $(EXES)
	-$(RM) $(MODS)
	-$(RM) *.dSYM
