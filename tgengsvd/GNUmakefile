ifndef COMPILER
COMPILER=x64x
endif # !COMPILER
include ../$(COMPILER).mk
MKFS=GNUmakefile ../$(COMPILER).mk

.PHONY: all help clean

all: dgengsvd.exe zgengsvd.exe derrgsvd.exe zerrgsvd.exe svalserr.exe dtgsja_test.exe gentxt.exe

help:
	@echo "gmake [COMPILER=x64x|x200|x64|gnu|nvidia] [NDEBUG=optimization_level] [ABI=ilp64|lp64] [WP=...] [all|clean|help]"

dgengsvd.exe: dgengsvd.F90 bio.mod seed.mod lamgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) dgengsvd.F90 bio.o seed.o lamgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(ABI)$(DEBUG) $(LDFLAGS)

zgengsvd.exe: zgengsvd.F90 bio.mod seed.mod lamgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) zgengsvd.F90 bio.o seed.o lamgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(ABI)$(DEBUG) $(LDFLAGS)

derrgsvd.exe: derrgsvd.F90 binio.mod $(MKFS)
	$(FC) $(FFLAGS) derrgsvd.F90 binio.o -o$@ -L../../libpvn/src -lpvn $(LDFLAGS)

zerrgsvd.exe: zerrgsvd.F90 binio.mod $(MKFS)
	$(FC) $(FFLAGS) zerrgsvd.F90 binio.o -o$@ -L../../libpvn/src -lpvn $(LDFLAGS)

svalserr.exe: svalserr.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) svalserr.F90 bio.o -o$@ $(LDFLAGS)

gentxt.exe: gentxt.F90 $(MKFS)
	$(FC) $(FFLAGS) gentxt.F90 -o$@ $(LDFLAGS)

dtgsja_test.exe: dtgsja_test.F90 binio.mod $(MKFS)
	$(FC) $(FFLAGS) dtgsja_test.F90 binio.o -o$@ -L../../libpvn/src -lpvn $(LDFLAGS)

bio.o bio.mod: bio.F90 $(MKFS)
	$(FC) $(FFLAGS) -c bio.F90

seed.o seed.mod: seed.F90 $(MKFS)
	$(FC) $(FFLAGS) -c seed.F90

lamgen.o lamgen.mod: lamgen.F90 seed.mod $(MKFS)
	$(FC) $(FFLAGS) -c lamgen.F90

datgen.o datgen.mod: datgen.F90 seed.mod $(MKFS)
	$(FC) $(FFLAGS) -c datgen.F90

binio.o binio.mod: binio.F90 $(MKFS)
	$(FC) $(FFLAGS) -c binio.F90

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *.opt.yaml
	-$(RM) *__genmod.*
	-$(RM) *.dSYM
