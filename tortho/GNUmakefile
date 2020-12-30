ifndef COMPILER
COMPILER=gnu
endif # !COMPILER
include ../$(COMPILER).mk
MKFS=GNUmakefile ../$(COMPILER).mk

.PHONY: all help clean

all: dortho.exe zortho.exe

help:
	@echo "gmake [COMPILER=gnu|x64|x200] [NDEBUG=0|1|2|3|4|5] [ABI=ilp64|lp64] [all|clean|help]"

dortho.exe: dortho.F90 binio.mod $(MKFS)
	$(FC) -I.. $(FFLAGS) dortho.F90 binio.o -o$@ $(LDFLAGS)

zortho.exe: zortho.F90 binio.mod $(MKFS)
	$(FC) -I.. $(FFLAGS) zortho.F90 binio.o -o$@ $(LDFLAGS)

binio.o binio.mod: binio.F90 ../vn/vn_binio_f.mod $(MKFS)
	$(FC) $(FFLAGS) -c binio.F90

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *__genmod.*
	-$(RM) *.dSYM
