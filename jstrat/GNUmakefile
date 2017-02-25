ifeq ($(CPU),x64) # Xeon / Intel C
include ../x64.mk
LIBFLAGS=-pthread
else ifeq ($(CPU),x100) # Knights Corner / Intel C
include ../x100.mk
LIBFLAGS=-pthread
else ifeq ($(CPU),x200) # Knights Landing / Intel C
include ../x200.mk
LIBFLAGS=-pthread
else ifeq ($(CPU),power8) # IBM POWER8LE / GNU C
include ../power8.mk
LIBFLAGS=-pthread
else ifeq ($(CPU),pwr8) # IBM POWER8LE / XL C
include ../pwr8.mk
LIBFLAGS=-pthread
else # GNU/Clang
include ../gnu.mk
LIBFLAGS=
endif # ?CPU
CFLAGS=$(OPTCFLAGS) $(DBGCFLAGS) $(LIBFLAGS) $(C11FLAGS) $(FPUCFLAGS)

OBJS=jstrat.o
MKFS=GNUmakefile ../gnu.mk ../power8.mk ../pwr8.mk ../x100.mk ../x200.mk ../x64.mk
LIBS=libjstrat.a

all: ../$(LIBS)

help:
	@echo "make [CPU=x64|x100|power8|pwr8] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

../$(LIBS): $(OBJS) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

jstrat.o: jstrat.c jstrat.h jstrat_ME.h $(MKFS)
	$(CC) $(CFLAGS) -c jstrat.c

clean:
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) ../$(LIBS)
