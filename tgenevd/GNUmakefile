ifndef COMPILER
COMPILER=gnu
endif # !COMPILER
include ../$(COMPILER).mk
MKFS=GNUmakefile ../$(COMPILER).mk

.PHONY: all help clean

all: dgenevd.exe zgenevd.exe dvalserr.exe gentxt.exe

help:
	@echo "gmake [COMPILER=gnu|x64|x64x|x200] [NDEBUG=0|1|2|3|4|5] [ABI=ilp64|lp64] [all|clean|help]"

dgenevd.exe: dgenevd.F90 bio.mod seed.mod lamgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) dgenevd.F90 bio.o seed.o lamgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

zgenevd.exe: zgenevd.F90 bio.mod seed.mod lamgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) zgenevd.F90 bio.o seed.o lamgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

dvalserr.exe: dvalserr.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) dvalserr.F90 bio.o -o$@ $(LDFLAGS)

gentxt.exe: gentxt.F90 $(MKFS)
	$(FC) $(FFLAGS) gentxt.F90 -o$@ $(LDFLAGS)

bio.o bio.mod: bio.F90 $(MKFS)
	$(FC) $(FFLAGS) -c bio.F90

seed.o seed.mod: seed.F90 $(MKFS)
	$(FC) $(FFLAGS) -c seed.F90

lamgen.o lamgen.mod: lamgen.F90 seed.mod $(MKFS)
	$(FC) $(FFLAGS) -c lamgen.F90

datgen.o datgen.mod: datgen.F90 seed.mod $(MKFS)
	$(FC) $(FFLAGS) -c datgen.F90

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *.opt.yaml
	-$(RM) *__genmod.*
	-$(RM) *.dSYM
