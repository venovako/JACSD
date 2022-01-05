ifndef COMPILER
COMPILER=gnu
endif # !COMPILER
include ../$(COMPILER).mk
MKFS=GNUmakefile ../$(COMPILER).mk

.PHONY: all help clean

all: gentxt.exe dgensvd.exe zgensvd.exe derrsvd.exe zerrsvd.exe svalserr.exe

help:
	@echo "gmake [COMPILER=gnu|x64|x200] [NDEBUG=0|1|2|3|4|5] [ABI=ilp64|lp64] [all|clean|help]"

gentxt.exe: gentxt.F90 $(MKFS)
	$(FC) $(FFLAGS) gentxt.F90 -o$@ $(LDFLAGS)

dgensvd.exe: dgensvd.F90 bio.mod seed.mod svgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) dgensvd.F90 bio.o seed.o svgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

zgensvd.exe: zgensvd.F90 bio.mod seed.mod svgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) zgensvd.F90 bio.o seed.o svgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

derrsvd.exe: derrsvd.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) derrsvd.F90 bio.o -o$@ $(LDFLAGS)

zerrsvd.exe: zerrsvd.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) zerrsvd.F90 bio.o -o$@ $(LDFLAGS)

svalserr.exe: svalserr.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) svalserr.F90 bio.o -o$@ $(LDFLAGS)

bio.o bio.mod: bio.F90 $(MKFS)
	$(FC) $(FFLAGS) -c bio.F90

seed.o seed.mod: seed.F90 $(MKFS)
	$(FC) $(FFLAGS) -c seed.F90

svgen.o svgen.mod: svgen.F90 seed.mod $(MKFS)
	$(FC) $(FFLAGS) -c svgen.F90

datgen.o datgen.mod: datgen.F90 seed.mod $(MKFS)
	$(FC) $(FFLAGS) -c datgen.F90

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *__genmod.*
	-$(RM) *.dSYM
