ifndef COMPILER
COMPILER=x64x
endif # !COMPILER
include ../$(COMPILER).mk
MKFS=GNUmakefile ../$(COMPILER).mk

.PHONY: all help clean

all: dortho.exe zortho.exe sortho.exe cortho.exe

help:
	@echo "gmake [COMPILER=x64x|x200|x64|gnu|nvidia] [NDEBUG=optimization_level] [ABI=ilp64|lp64] [all|clean|help]"

dortho.exe: dortho.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) dortho.F90 bio.o -o$@ $(LDFLAGS)

zortho.exe: zortho.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) zortho.F90 bio.o -o$@ $(LDFLAGS)

sortho.exe: sortho.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) sortho.F90 bio.o -o$@ $(LDFLAGS)

cortho.exe: cortho.F90 bio.mod $(MKFS)
	$(FC) $(FFLAGS) cortho.F90 bio.o -o$@ $(LDFLAGS)

bio.o bio.mod: bio.F90 $(MKFS)
	$(FC) $(FFLAGS) -c bio.F90

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *.opt.yaml
	-$(RM) *__genmod.*
	-$(RM) *.dSYM
