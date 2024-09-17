ifndef COMPILER
COMPILER=x64x
endif # !COMPILER
include ../$(COMPILER).mk
MKFS=GNUmakefile ../$(COMPILER).mk
ifneq ($(ABI),ilp64)
$(error ABI=lp64 not supported)
endif # !ilp64

.PHONY: all help svd_test clean

all: ../libl0c$(DEBUG).a xCSGEN.exe xLACSD.exe xL0.exe svd_test

help:
	@echo "gmake [COMPILER=x64x|x200|x64|gnu|nvidia] [NDEBUG=optimization_level] [WP=...] [all|clean|help]"

ifeq ($(COMPILER),x200)
AVX512_DJACV.o: AVX512_DJACV.c AVX512_DJACV.h $(MKFS)
	$(CC) $(CFLAGS) -c AVX512_DJACV.c

../libl0c$(DEBUG).a: AVX512_DJACV.o $(MKFS)
	$(AR) $(ARFLAGS) $@ AVX512_DJACV.o
else # non-KNL
AVX2_FMA_DJACV.o: AVX2_FMA_DJACV.c AVX2_FMA_DJACV.h $(MKFS)
	$(CC) $(CFLAGS) -c AVX2_FMA_DJACV.c

../libl0c$(DEBUG).a: AVX2_FMA_DJACV.o $(MKFS)
	$(AR) $(ARFLAGS) $@ AVX2_FMA_DJACV.o
endif # ?COMPILER

xCSGEN.exe: xCSGEN.o FTNUTILS.o ftnutils.mod ../libqxblas$(WP)$(ABI)$(DEBUG).a $(MKFS)
	$(FC) $(FFLAGS) xCSGEN.o FTNUTILS.o -o$@ -L.. -lqxblas$(WP)$(ABI)$(DEBUG) $(LDFLAGS)

xCSGEN.o: xCSGEN.F90 ftnutils.mod $(MKFS)
	$(FC) $(FFLAGS) -c xCSGEN.F90

xLACSD.exe: xLACSD.o BSCSD.o BLAS.o FTNUTILS.o $(MKFS)
	$(FC) $(FFLAGS) xLACSD.o BSCSD.o BLAS.o FTNUTILS.o -o$@ $(LDFLAGS)

xLACSD.o: xLACSD.F90 bscsd.mod $(MKFS)
	$(FC) $(FFLAGS) -c xLACSD.F90

xL0.exe: xL0.o JACSVD.o BLAS.o FTNUTILS.o ../libl0c$(DEBUG).a ../libjstrat$(DEBUG).a $(MKFS)
	$(FC) $(FFLAGS) xL0.o JACSVD.o BLAS.o FTNUTILS.o -o$@ -L.. -ll0c$(DEBUG) -ljstrat$(DEBUG) $(LDFLAGS)

xL0.o: xL0.F90 jacsvd.mod $(MKFS)
	$(FC) $(FFLAGS) -c xL0.F90

BSCSD.o bscsd.mod: BSCSD.F90 blas.mod ftnutils.mod $(MKFS)
	$(FC) $(FFLAGS) -c BSCSD.F90

JACSVD.o jacsvd.mod: JACSVD.F90 JSTRAT_IFACES.F90 Lx_IFACES.F90 L0_IFACES.F90 Lx_IFACES_IMPL.F90 L0_IFACES_IMPL.F90 blas.mod ftnutils.mod $(MKFS)
	$(FC) $(FFLAGS) -c JACSVD.F90

BLAS.o blas.mod: BLAS.F90 ftnutils.mod $(MKFS)
	$(FC) $(FFLAGS) -c BLAS.F90

FTNUTILS.o ftnutils.mod: FTNUTILS.F90 BIN_IO.F90 BIN_IO_IFACES.F90 CONSTANTS.F90 GET_IOUNIT.F90 GET_NTHR.F90 IFACES_IMPL.F90 INTERFACES.F90 KIND_PARAMS.F90 TIMER.F90 USE_MODULES.F90 VEC_PARAMS.F90 $(MKFS)
	$(FC) $(FFLAGS) -c FTNUTILS.F90

ifeq ($(COMPILER),x200)
svd_test: xLASVD.F90 BLAS.o blas.mod FTNUTILS.o ftnutils.mod $(MKFS)
	for x in C D S Z; do for y in GEJSV GESDD GESVD GESVJ; do for z in FN FW; do $(FC) $(FFLAGS) -D$${z} -D$${x}LASVD -D$${y} xLASVD.F90 BLAS.o FTNUTILS.o -o$${x}$${y}$${z}.exe $(LDFLAGS); done; done; done
else # non-KNL
svd_test: xLASVD.F90 BLAS.o blas.mod FTNUTILS.o ftnutils.mod $(MKFS)
	for x in C D S Z; do for y in GEJSV GESDD GESVD GESVJ; do $(FC) $(FFLAGS) -D$${x}LASVD -D$${y} xLASVD.F90 BLAS.o FTNUTILS.o -o$${x}$${y}.exe $(LDFLAGS); done; done
endif # ?COMPILER

clean:
	-$(RM) ../libl0c$(DEBUG).a
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.a
	-$(RM) *.optrpt
	-$(RM) *.opt.yaml
	-$(RM) *__genmod.f90
	-$(RM) *__genmod.mod
	-$(RM) *.dSYM
