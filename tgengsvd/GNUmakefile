ifndef CPU
CPU=gnu
endif # !CPU
include ../$(CPU).mk
MKFS=GNUmakefile ../$(CPU).mk

.PHONY: all help clean

all: dgengsvd.exe zgengsvd.exe derrgsvd.exe zerrgsvd.exe

help:
	@echo "gmake [CPU=x64|x200|gnu] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

dgengsvd.exe: dgengsvd.F90 bio.mod seed.mod lamgen.mod datgen.mod $(MKFS)
	$(FC) -I.. $(FFLAGS) dgengsvd.F90 bio.o seed.o lamgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

zgengsvd.exe: zgengsvd.F90 bio.mod seed.mod lamgen.mod datgen.mod $(MKFS)
	$(FC) -I.. $(FFLAGS) zgengsvd.F90 bio.o seed.o lamgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

bio.o bio.mod: bio.F90 $(MKFS)
	$(FC) $(FFLAGS) -c bio.F90

seed.o seed.mod: seed.F90 $(MKFS)
	$(FC) $(FFLAGS) -c seed.F90

lamgen.o lamgen.mod: lamgen.F90 seed.mod $(MKFS)
	$(FC) $(FFLAGS) -c lamgen.F90

datgen.o datgen.mod: datgen.F90 seed.mod $(MKFS)
	$(FC) -I.. $(FFLAGS) -c datgen.F90

binio.o binio.mod: binio.F90 ../vn/vn_binio_f.mod $(MKFS)
	$(FC) $(FFLAGS) -c binio.F90

derrgsvd.exe: derrgsvd.F90 binio.mod $(MKFS)
	$(FC) -I.. $(FFLAGS) derrgsvd.F90 binio.o -o$@ $(LDFLAGS)

zerrgsvd.exe: zerrgsvd.F90 binio.mod $(MKFS)
	$(FC) -I.. $(FFLAGS) zerrgsvd.F90 binio.o -o$@ $(LDFLAGS)

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *__genmod.*
	-$(RM) *.dSYM