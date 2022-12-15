SHELL=/bin/bash
ARCH=$(shell uname)
ifndef ABI
ABI=ilp64
endif # !ABI
ifndef WP
WP=16
endif # !WP
ifdef NDEBUG
DEBUG=
else # DEBUG
DEBUG=g
endif # ?NDEBUG
ifndef FP
FP=precise
endif # !FP
RM=rm -rfv
AR=xiar
ARFLAGS=-qnoipo -lib rsv
CC=icx
FC=ifx
CPUFLAGS=-DUSE_INTEL -DUSE_X64 -DQX_WP=$(WP) -fPIC -fexceptions -fno-omit-frame-pointer -qopenmp -rdynamic
ifdef PROFILE
CPUFLAGS += -DVN_PROFILE=$(PROFILE) -fno-inline -finstrument-functions
endif # PROFILE
ifeq ($(ABI),lp64)
CPUFLAGS += -DVN_INTEGER_KIND=4
endif # lp64
ifneq ($(ARCH),Darwin)
CPUFLAGS += -DTSC_FREQ_HZ=$(shell if [ `if [ -r /etc/redhat-release ]; then grep -c 'release 7' /etc/redhat-release; else echo 0; fi` = 1 ]; then echo `dmesg | grep 'TSC clocksource calibration' | cut -d':' -f3 | cut -d' ' -f2 | sed 's/\.//g'`000; else echo 0; fi)ull
endif # Linux
FORFLAGS=$(CPUFLAGS) -standard-semantics -threads
ifneq ($(ABI),lp64)
FORFLAGS += -i8
endif # ilp64
C18FLAGS=$(CPUFLAGS) -std=c18
FPUFLAGS=-fp-model $(FP) -fprotect-parens -no-ftz
ifneq ($(FP),strict)
FPUFLAGS += -fma -fimf-use-svml=true
endif # !strict
FPUFFLAGS=$(FPUFLAGS)
FPUCFLAGS=$(FPUFLAGS)
ifeq ($(FP),strict)
FPUFFLAGS += -assume ieee_fpe_flags
endif # strict
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -xHost -vec-threshold0
OPTFFLAGS=$(OPTFLAGS) -DMKL_DIRECT_CALL
OPTCFLAGS=$(OPTFLAGS)
DBGFLAGS=-DNDEBUG -qopt-report=3 -traceback
DBGFFLAGS=$(DBGFLAGS)
DBGCFLAGS=$(DBGFLAGS)
else # DEBUG
OPTFLAGS=-O0 -xHost
OPTFFLAGS=$(OPTFLAGS)
OPTCFLAGS=$(OPTFLAGS)
DBGFLAGS=-$(DEBUG) -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -traceback
ifneq ($(ARCH),Darwin)
DBGFLAGS += -debug parallel
endif # Linux
DBGFFLAGS=$(DBGFLAGS) -debug-parameters all -check all -warn all
DBGCFLAGS=$(DBGFLAGS)
endif # ?NDEBUG
LIBFLAGS=-DUSE_MKL
ifneq ($(ABI),lp64)
LIBFLAGS += -DMKL_ILP64
endif # ilp64
LIBFLAGS += -I. -I../vn -I${MKLROOT}/include/intel64/$(ABI) -I${MKLROOT}/include
LDFLAGS=-L.. -lvn$(PROFILE)$(DEBUG)
ifeq ($(ARCH),Darwin)
LDFLAGS += -L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -lmkl_intel_$(ABI) -lmkl_intel_thread -lmkl_core
else # Linux
LIBFLAGS += -static-libgcc -D_GNU_SOURCE
LDFLAGS += -L${MKLROOT}/lib/intel64 -Wl,-rpath=${MKLROOT}/lib/intel64 -lmkl_intel_$(ABI) -lmkl_intel_thread -lmkl_core
endif # ?Darwin
LDFLAGS += -lpthread -lm -ldl
FFLAGS=$(OPTFFLAGS) $(DBGFFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFFLAGS)
CFLAGS=$(OPTCFLAGS) $(DBGCFLAGS) $(LIBFLAGS) $(C18FLAGS) $(FPUCFLAGS)
