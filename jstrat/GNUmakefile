ifndef COMPILER
COMPILER=gnu
endif # !COMPILER
include ../$(COMPILER).mk
MKFS=GNUmakefile ../$(COMPILER).mk

OBJS=jstrat.o jstrat_f.o
MODS=jstrat_f.mod

.PHONY: all help clean

all: ../libjstrat$(PROFILE)$(DEBUG).a

help:
	@echo "gmake [COMPILER=gnu|x64|x200|nvidia] [NDEBUG=0|1|2|3|4|5] [ABI=ilp64|lp64] [all|clean|help]"

../libjstrat$(PROFILE)$(DEBUG).a: $(OBJS) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

jstrat.o: jstrat.c jstrat.h jstrat_ME.h $(MKFS)
	$(CC) $(CFLAGS) -c jstrat.c

jstrat_f.o jstrat_f.mod: jstrat_f.F90 jstrat.h $(MKFS)
	$(FC) $(FFLAGS) -c jstrat_f.F90

clean:
	-$(RM) ../libjstrat$(PROFILE)$(DEBUG).a
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) $(MODS)
	-$(RM) *.dSYM
