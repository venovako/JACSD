ifeq ($(CPU),x64) # Xeon / Intel Fortran
include x64.mk
MKFS=GNUmakefile x64.mk
else ifeq ($(CPU),x100) # Knights Corner / Intel Fortran
include x100.mk
MKFS=GNUmakefile x100.mk
else ifeq ($(CPU),x200) # Knights Landing / Intel Fortran
include x200.mk
MKFS=GNUmakefile x200.mk
else # GNU Fortran
include gnu.mk
MKFS=GNUmakefile gnu.mk
endif # ?CPU

# -L. -ll0c -ljstrat -lqxblas -lvn
LIBS=libl0c.a libjstrat.a libqxblas.a libvn.a

.PHONY: all help svd_test clean

all: xCSGEN.exe xLACSD.exe svd_test

help:
	@echo "make [WP=4|8|10|16] [CPU=x64|x100|x200] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

xCSGEN.exe: xCSGEN.o FTNUTILS.o libqxblas.a $(MKFS)
	$(FC) $(FCFLAGS) xCSGEN.o FTNUTILS.o -o$@ -L. -lqxblas $(LDFLAGS)

xCSGEN.o: xCSGEN.F90 qx_wp.fi ftnutils.mod $(MKFS)
	$(FC) $(FCFLAGS) -c xCSGEN.F90

xLACSD.exe: xLACSD.o BSCSD.o FTNUTILS.o $(MKFS)
	$(FC) $(FCFLAGS) xLACSD.o BSCSD.o FTNUTILS.o -o$@ $(LDFLAGS)

xLACSD.o: xLACSD.F90 bscsd.mod $(MKFS)
	$(FC) $(FCFLAGS) -c xLACSD.F90

ifeq ($(CPU),x200)
AVX512_DJACV.o: AVX512_DJACV.c AVX512_DJACV.h $(MKFS)
	$(CC) $(CFLAGS) -c AVX512_DJACV.c

libl0c.a: AVX512_DJACV.o $(MKFS)
	$(AR) $(ARFLAGS) $@ AVX512_DJACV.o
else # non-KNL
AVX2_FMA_DJACV.o: AVX2_FMA_DJACV.c AVX2_FMA_DJACV.h $(MKFS)
	$(CC) $(CFLAGS) -c AVX2_FMA_DJACV.c

libl0c.a: AVX2_FMA_DJACV.o $(MKFS)
	$(AR) $(ARFLAGS) $@ AVX2_FMA_DJACV.o
endif # ?CPU

libjstrat.a: $(MKFS)
ifdef NDEBUG
	pushd jstrat && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
else # DEBUG
	pushd jstrat && $(MAKE) CPU=$(CPU) && popd
endif # ?NDEBUG

qx_wp.fi libqxblas.a: $(MKFS)
ifdef NDEBUG
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
else # DEBUG
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) && popd
endif # ?NDEBUG

libvn.a: $(MKFS)
ifdef NDEBUG
	pushd vn && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
else # DEBUG
	pushd vn && $(MAKE) CPU=$(CPU) && popd
endif # ?NDEBUG

BSCSD.o bscsd.mod: BSCSD.F90 ftnutils.mod $(MKFS)
	$(FC) $(FCFLAGS) -c BSCSD.F90

JACSVD.o jacsvd.mod: JACSVD.F90 JSTRAT_IFACES.F90 L0_IFACES.F90 L0_IFACES_IMPL.F90 L1_IFACES.F90 L1_IFACES_IMPL.F90 ftnutils.mod $(MKFS)
	$(FC) $(FCFLAGS) -c JACSVD.F90

FTNUTILS.o ftnutils.mod: FTNUTILS.F90 BIN_IO.F90 BIN_IO_IFACES.F90 BLAS.F90 CONSTANTS.F90 GET_IOUNIT.F90 GET_NTHR.F90 IFACES_IMPL.F90 INTERFACES.F90 KIND_PARAMS.F90 TIMER.F90 USE_MODULES.F90 VEC_PARAMS.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c FTNUTILS.F90

ifeq ($(CPU),x200)
svd_test: xLASVD.F90 FTNUTILS.o ftnutils.mod $(MKFS)
	for x in C D S Z; do for y in GEJSV GESDD GESVD GESVJ; do for z in FN FW; do $(FC) $(FCFLAGS) -D$${z} -D$${x}LASVD -D$${y} xLASVD.F90 FTNUTILS.o -o$${x}$${y}$${z}.exe $(LDFLAGS); done; done; done
else # non-KNL
svd_test: xLASVD.F90 FTNUTILS.o ftnutils.mod $(MKFS)
	for x in C D S Z; do for y in GEJSV GESDD GESVD GESVJ; do $(FC) $(FCFLAGS) -D$${x}LASVD -D$${y} xLASVD.F90 FTNUTILS.o -o$${x}$${y}.exe $(LDFLAGS); done; done
endif # ?CPU

clean:
ifdef NDEBUG
	pushd vn && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) clean && popd
	pushd jstrat && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) NDEBUG=$(NDEBUG) clean && popd
else # DEBUG
	pushd vn && $(MAKE) CPU=$(CPU) clean && popd
	pushd jstrat && $(MAKE) CPU=$(CPU) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) clean && popd
endif # ?NDEBUG
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.a
	-$(RM) *.optrpt
	-$(RM) *__genmod.f90
	-$(RM) *__genmod.mod
	-$(RM) *.dSYM
