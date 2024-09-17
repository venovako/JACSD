ifndef COMPILER
COMPILER=x64x
endif # !COMPILER
include ../$(COMPILER).mk
MKFS=GNUmakefile ../$(COMPILER).mk

.PHONY: all help clean

all: dgenhsvd.exe zgenhsvd.exe derrhsvd.exe zerrhsvd.exe svalserr.exe gentxt.exe

help:
	@echo "gmake [COMPILER=x64x|x200|x64|gnu] [NDEBUG=optimization_level] [ABI=ilp64|lp64] [WP=...] [all|clean|help]"

dgenhsvd.exe: dgenhsvd.F90 bio.mod seed.mod lamgen.mod indeff.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) dgenhsvd.F90 bio.o seed.o lamgen.o indeff.o datgen.o -o$@ -L.. -lqxblas$(WP)$(ABI)$(DEBUG) $(LDFLAGS)

zgenhsvd.exe: zgenhsvd.F90 bio.mod seed.mod lamgen.mod indeff.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) zgenhsvd.F90 bio.o seed.o lamgen.o indeff.o datgen.o -o$@ -L.. -lqxblas$(WP)$(ABI)$(DEBUG) $(LDFLAGS)

derrhsvd.exe: derrhsvd.F90 binio.mod $(MKFS)
	$(FC) $(FFLAGS) derrhsvd.F90 binio.o -o$@ -L../../libpvn/src -lpvn $(LDFLAGS)

zerrhsvd.exe: zerrhsvd.F90 binio.mod $(MKFS)
	$(FC) $(FFLAGS) zerrhsvd.F90 binio.o -o$@ -L../../libpvn/src -lpvn $(LDFLAGS)

svalserr.exe: svalserr.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) svalserr.F90 bio.o -o$@ $(LDFLAGS)

gentxt.exe: gentxt.F90 $(MKFS)
	$(FC) $(FFLAGS) gentxt.F90 -o$@ $(LDFLAGS)

bio.o bio.mod: bio.F90 $(MKFS)
	$(FC) $(FFLAGS) -c bio.F90

seed.o seed.mod: seed.F90 $(MKFS)
	$(FC) $(FFLAGS) -c seed.F90

lamgen.o lamgen.mod: lamgen.F90 seed.mod $(MKFS)
	$(FC) $(FFLAGS) -c lamgen.F90

indeff.o indeff.mod: indeff.F90 $(MKFS)
	$(FC) $(FFLAGS) -c indeff.F90

datgen.o datgen.mod: datgen.F90 seed.mod indeff.mod $(MKFS)
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
