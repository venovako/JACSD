ifndef CPU
CPU=gnu
endif # !CPU
include ../$(CPU).mk
MKFS=GNUmakefile ../$(CPU).mk

SRCS=genlam.F90 seedix.F90 seedok.F90 txtlam.F90
OBJS=$(SRCS:.F90=.o)

.PHONY: all help clean

all: dgengsvd.exe zgengsvd.exe

help:
	@echo "gmake [CPU=x64|x200|gnu] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

dgengsvd.exe: $(OBJS) dgengsvd.F90 dgendat.F90 $(MKFS)
	$(FC) -I.. $(FFLAGS) dgengsvd.F90 dgendat.F90 $(OBJS) -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

zgengsvd.exe: $(OBJS) zgengsvd.F90 zgendat.F90 $(MKFS)
	$(FC) -I.. $(FFLAGS) zgengsvd.F90 zgendat.F90 $(OBJS) -o$@ -L.. -lqxblas$(WP)$(PROFILE)$(DEBUG) $(LDFLAGS)

$(OBJS): %.o: %.F90 $(MKFS)
	$(FC) -I.. $(FFLAGS) -c $<

clean:
	-$(RM) dgengsvd.exe
	-$(RM) zgengsvd.exe
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *__genmod.*
	-$(RM) *.dSYM
