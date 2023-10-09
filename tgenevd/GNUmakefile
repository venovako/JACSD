ifndef COMPILER
COMPILER=gnu
endif # !COMPILER
include ../$(COMPILER).mk
MKFS=GNUmakefile ../$(COMPILER).mk

.PHONY: all help clean

all: sgenevd.exe dgenevd.exe cgenevd.exe zgenevd.exe svalserr.exe dvalserr.exe sgentxt.exe dgentxt.exe

help:
	@echo "gmake [COMPILER=gnu|x64|x64x|x200] [NDEBUG=0|1|2|3|4|5] [ABI=ilp64|lp64] [all|clean|help]"

sgenevd.exe: sgenevd.F90 bio.mod seed.mod lamgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) sgenevd.F90 bio.o seed.o lamgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

dgenevd.exe: dgenevd.F90 bio.mod seed.mod lamgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) dgenevd.F90 bio.o seed.o lamgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

cgenevd.exe: cgenevd.F90 bio.mod seed.mod lamgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) cgenevd.F90 bio.o seed.o lamgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

zgenevd.exe: zgenevd.F90 bio.mod seed.mod lamgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) zgenevd.F90 bio.o seed.o lamgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

svalserr.exe: svalserr.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) svalserr.F90 bio.o -o$@ $(LDFLAGS)

dvalserr.exe: dvalserr.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) dvalserr.F90 bio.o -o$@ $(LDFLAGS)

sgentxt.exe: sgentxt.F90 $(MKFS)
	$(FC) $(FFLAGS) sgentxt.F90 -o$@ $(LDFLAGS)

dgentxt.exe: dgentxt.F90 $(MKFS)
	$(FC) $(FFLAGS) dgentxt.F90 -o$@ $(LDFLAGS)

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
