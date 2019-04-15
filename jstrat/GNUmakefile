ifeq ($(CPU),x64) # Xeon / Intel Fortran and C
include ../x64.mk
MKFS=GNUmakefile ../x64.mk
else ifeq ($(CPU),x200) # Knights Landing / Intel Fortran and C
include ../x200.mk
MKFS=GNUmakefile ../x200.mk
else ifeq ($(CPU),llvm) # Flang + Clang
include ../llvm.mk
MKFS=GNUmakefile ../llvm.mk
else # GNU Fortran + (GNU C or Clang)
ifndef CPU
CPU=gnu
endif # !CPU
include ../gnu.mk
MKFS=GNUmakefile ../gnu.mk
endif # ?CPU

OBJS=jstrat.o jstrat_f.o
MODS=jstrat_f.mod

.PHONY: all help clean

all: ../libjstrat.a

help:
	@echo "make [CPU=x64|x100|x200|llvm|gnu] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

../libjstrat.a: $(OBJS) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

jstrat.o: jstrat.c jstrat.h jstrat_ME.h $(MKFS)
	$(CC) $(CFLAGS) -c jstrat.c

jstrat_f.o jstrat_f.mod: jstrat_f.F90 jstrat.h $(MKFS)
	$(FC) $(FFLAGS) -c jstrat_f.F90

clean:
	-$(RM) ../libjstrat.a
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) $(MODS)
	-$(RM) *.dSYM
