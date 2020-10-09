ifndef COMPILER
COMPILER=gnu
endif # !COMPILER
include ../$(COMPILER).mk
MKFS=GNUmakefile ../$(COMPILER).mk

.PHONY: all help clean

all: dgengsvd.exe zgengsvd.exe derrgsvd.exe zerrgsvd.exe svalserr.exe dtgsja_test.exe gentxt.exe

help:
	@echo "gmake [COMPILER=gnu|x64|x200] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

dgengsvd.exe: dgengsvd.F90 bio.mod seed.mod lamgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) dgengsvd.F90 bio.o seed.o lamgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

zgengsvd.exe: zgengsvd.F90 bio.mod seed.mod lamgen.mod datgen.mod $(MKFS)
	$(FC) $(FFLAGS) zgengsvd.F90 bio.o seed.o lamgen.o datgen.o -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

derrgsvd.exe: derrgsvd.F90 binio.mod $(MKFS)
	$(FC) $(FFLAGS) derrgsvd.F90 binio.o -o$@ $(LDFLAGS)

zerrgsvd.exe: zerrgsvd.F90 binio.mod $(MKFS)
	$(FC) $(FFLAGS) zerrgsvd.F90 binio.o -o$@ $(LDFLAGS)

svalserr.exe: svalserr.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) svalserr.F90 bio.o -o$@ $(LDFLAGS)

gentxt.exe: gentxt.F90 $(MKFS)
	$(FC) $(FFLAGS) gentxt.F90 -o$@ $(LDFLAGS)

dtgsja_test.exe: dtgsja_test.F90 binio.mod ../vn/vn_blas_f.mod ../vn/vn_timer_f.mod $(MKFS)
	$(FC) $(FFLAGS) dtgsja_test.F90 binio.o -o$@ $(LDFLAGS)

bio.o bio.mod: bio.F90 $(MKFS)
	$(FC) $(FFLAGS) -c bio.F90

seed.o seed.mod: seed.F90 $(MKFS)
	$(FC) $(FFLAGS) -c seed.F90

lamgen.o lamgen.mod: lamgen.F90 seed.mod $(MKFS)
	$(FC) $(FFLAGS) -c lamgen.F90

datgen.o datgen.mod: datgen.F90 seed.mod $(MKFS)
	$(FC) $(FFLAGS) -c datgen.F90

binio.o binio.mod: binio.F90 ../vn/vn_binio_f.mod $(MKFS)
	$(FC) $(FFLAGS) -c binio.F90

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *__genmod.*
	-$(RM) *.dSYM
