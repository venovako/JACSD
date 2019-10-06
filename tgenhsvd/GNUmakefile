ifndef CPU
CPU=gnu
endif # !CPU
include ../$(CPU).mk
MKFS=GNUmakefile ../$(CPU).mk

.PHONY: all help clean

all: dgenhsvd.exe zgenhsvd.exe

help:
	@echo "gmake [CPU=x64|x200|gnu] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

dgenhsvd.exe: dgenhsvd.F90 bio.mod seed.mod lamgen.mod indeff.mod datgen.mod $(MKFS)
	$(FC) -I.. $(FFLAGS) dgenhsvd.F90 bio.o seed.o lamgen.o indeff.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

zgenhsvd.exe: zgenhsvd.F90 bio.mod seed.mod lamgen.mod indeff.mod datgen.mod $(MKFS)
	$(FC) -I.. $(FFLAGS) zgenhsvd.F90 bio.o seed.o lamgen.o indeff.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

bio.o bio.mod: bio.F90 $(MKFS)
	$(FC) $(FFLAGS) -c bio.F90

seed.o seed.mod: seed.F90 $(MKFS)
	$(FC) $(FFLAGS) -c seed.F90

lamgen.o lamgen.mod: lamgen.F90 seed.mod $(MKFS)
	$(FC) $(FFLAGS) -c lamgen.F90

indeff.o indeff.mod: indeff.F90 $(MKFS)
	$(FC) -I.. $(FFLAGS) -c indeff.F90

datgen.o datgen.mod: datgen.F90 seed.mod indeff.mod $(MKFS)
	$(FC) -I.. $(FFLAGS) -c datgen.F90

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *__genmod.*
	-$(RM) *.dSYM
