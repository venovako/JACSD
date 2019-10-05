ifndef CPU
CPU=gnu
endif # !CPU
include ../$(CPU).mk
MKFS=GNUmakefile ../$(CPU).mk

SRCS=genlam.F90 seedix.F90 seedok.F90 txtlam.F90
OBJS=$(SRCS:.F90=.o)

.PHONY: all help clean

all: dgengsvd.exe zgengsvd.exe derrgsvd.exe zerrgsvd.exe

help:
	@echo "gmake [CPU=x64|x200|gnu] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

dgengsvd.exe: bio.o $(OBJS) dgengsvd.F90 dgendat.F90 $(MKFS)
	$(FC) -I.. $(FFLAGS) dgengsvd.F90 dgendat.F90 bio.o $(OBJS) -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

zgengsvd.exe: bio.o $(OBJS) zgengsvd.F90 zgendat.F90 $(MKFS)
	$(FC) -I.. $(FFLAGS) zgengsvd.F90 zgendat.F90 bio.o $(OBJS) -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

$(OBJS): %.o: %.F90 $(MKFS)
	$(FC) -I.. $(FFLAGS) -c $<

bio.o bio.mod: bio.F90 $(MKFS)
	$(FC) $(FFLAGS) -c bio.F90

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
