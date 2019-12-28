ifndef CPU
CPU=gnu
endif # !CPU
include ../$(CPU).mk
MKFS=GNUmakefile ../$(CPU).mk

.PHONY: all help clean

all: dgenskew.exe zgenskew.exe

help:
	@echo "gmake [CPU=x64|x200|gnu] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

dgenskew.exe: dgenskew.F90 bio.mod seed.mod lamgen.mod datgen.mod $(MKFS)
	$(FC) -I.. $(FFLAGS) dgenskew.F90 bio.o seed.o lamgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

zgenskew.exe: zgenskew.F90 bio.mod seed.mod lamgen.mod datgen.mod $(MKFS)
	$(FC) -I.. $(FFLAGS) zgenskew.F90 bio.o seed.o lamgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

bio.o bio.mod: bio.F90 $(MKFS)
	$(FC) $(FFLAGS) -c bio.F90

seed.o seed.mod: seed.F90 $(MKFS)
	$(FC) $(FFLAGS) -c seed.F90

lamgen.o lamgen.mod: lamgen.F90 seed.mod $(MKFS)
	$(FC) $(FFLAGS) -c lamgen.F90

datgen.o datgen.mod: datgen.F90 seed.mod $(MKFS)
	$(FC) -I.. $(FFLAGS) -c datgen.F90

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *__genmod.*
	-$(RM) *.dSYM
