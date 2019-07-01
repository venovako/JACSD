ifndef CPU
CPU=gnu
endif # !CPU
include ../$(CPU).mk
MKFS=GNUmakefile ../$(CPU).mk

OBJS=jstrat.o jstrat_f.o
MODS=jstrat_f.mod

.PHONY: all help clean

all: ../libjstrat$(PROFILE).a

help:
	@echo "gmake [CPU=x64|x200|gnu] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

../libjstrat$(PROFILE).a: $(OBJS) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

jstrat.o: jstrat.c jstrat.h jstrat_ME.h $(MKFS)
	$(CC) $(CFLAGS) -c jstrat.c

jstrat_f.o jstrat_f.mod: jstrat_f.F90 jstrat.h $(MKFS)
	$(FC) $(FFLAGS) -c jstrat_f.F90

clean:
	-$(RM) ../libjstrat$(PROFILE).a
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) $(MODS)
	-$(RM) *.dSYM
