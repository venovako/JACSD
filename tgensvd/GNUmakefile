ifndef COMPILER
COMPILER=gnu
endif # !COMPILER
include ../$(COMPILER).mk
MKFS=GNUmakefile ../$(COMPILER).mk

.PHONY: all help clean

all: dgentxt.exe sgentxt.exe dgensvd.exe sgensvd.exe zgensvd.exe cgensvd.exe derrsvd.exe serrsvd.exe zerrsvd.exe cerrsvd.exe dsverr.exe ssverr.exe

help:
	@echo "gmake [COMPILER=gnu|x64|x200] [NDEBUG=0|1|2|3|4|5] [ABI=ilp64|lp64] [all|clean|help]"

dgentxt.exe: dgentxt.F90 $(MKFS)
	$(FC) $(FFLAGS) dgentxt.F90 -o$@ $(LDFLAGS)

sgentxt.exe: sgentxt.F90 $(MKFS)
	$(FC) $(FFLAGS) sgentxt.F90 -o$@ $(LDFLAGS)

dgensvd.exe: dgensvd.F90 bio.mod seed.mod svgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) dgensvd.F90 bio.o seed.o svgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

sgensvd.exe: sgensvd.F90 bio.mod seed.mod svgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) sgensvd.F90 bio.o seed.o svgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

zgensvd.exe: zgensvd.F90 bio.mod seed.mod svgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) zgensvd.F90 bio.o seed.o svgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

cgensvd.exe: cgensvd.F90 bio.mod seed.mod svgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) cgensvd.F90 bio.o seed.o svgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

derrsvd.exe: derrsvd.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) derrsvd.F90 bio.o -o$@ $(LDFLAGS)

serrsvd.exe: serrsvd.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) serrsvd.F90 bio.o -o$@ $(LDFLAGS)

zerrsvd.exe: zerrsvd.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) zerrsvd.F90 bio.o -o$@ $(LDFLAGS)

cerrsvd.exe: cerrsvd.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) cerrsvd.F90 bio.o -o$@ $(LDFLAGS)

dsverr.exe: dsverr.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) dsverr.F90 bio.o -o$@ $(LDFLAGS)

ssverr.exe: ssverr.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) ssverr.F90 bio.o -o$@ $(LDFLAGS)

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
