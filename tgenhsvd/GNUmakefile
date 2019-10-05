ifndef CPU
CPU=gnu
endif # !CPU
include ../$(CPU).mk
MKFS=GNUmakefile ../$(CPU).mk

SRCS=genlam.F90 seedix.F90 seedok.F90 txtlam.F90
OBJS=$(SRCS:.F90=.o)

.PHONY: all help clean

all: dgenhsvd.exe zgenhsvd.exe

help:
	@echo "gmake [CPU=x64|x200|gnu] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

dgenhsvd.exe: bio.o $(OBJS) dgenhsvd.F90 dgendat.F90 sif.F90 $(MKFS)
	$(FC) -I.. $(FFLAGS) dgenhsvd.F90 dgendat.F90 sif.F90 bio.o $(OBJS) -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

zgenhsvd.exe: bio.o $(OBJS) zgenhsvd.F90 zgendat.F90 hif.F90 $(MKFS)
	$(FC) -I.. $(FFLAGS) zgenhsvd.F90 zgendat.F90 hif.F90 bio.o $(OBJS) -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

$(OBJS): %.o: %.F90 $(MKFS)
	$(FC) -I.. $(FFLAGS) -c $<

bio.o bio.mod: bio.F90 $(MKFS)
	$(FC) $(FFLAGS) -c bio.F90

clean:
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *__genmod.*
	-$(RM) *.dSYM
