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
ifdef NDEBUG
FP=precise
else # DEBUG
FP=strict
endif # ?NDEBUG
endif # !FP
RM=rm -rfv
AR=xiar
ARFLAGS=-qnoipo -lib rsv
CC=icc
FC=ifort
ifndef CPU
CPU=Host
# COMMON-AVX512 for KNLs
endif # !CPU
CPUFLAGS=-DUSE_INTEL -DUSE_X64 -DQX_WP=$(WP) -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -qopt-multi-version-aggressive -qopt-zmm-usage=high -vec-threshold0 -qopenmp -x$(CPU)
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
C18FLAGS=$(CPUFLAGS) -std=gnu18
FPUFLAGS=-fp-model $(FP) -fprotect-parens -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt -fno-math-errno
ifeq ($(FP),strict)
FPUFLAGS += -fp-stack-check
endif # ?strict
FPUFFLAGS=$(FPUFLAGS)
FPUCFLAGS=$(FPUFLAGS)
ifeq ($(FP),strict)
FPUFFLAGS += -assume ieee_fpe_flags
endif # strict
DBGFLAGS=-traceback -diag-disable=10397,10441
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG)
OPTFFLAGS=$(OPTFLAGS) -DMKL_DIRECT_CALL
OPTCFLAGS=$(OPTFLAGS)
DBGFLAGS += -DNDEBUG -qopt-report=5
DBGFFLAGS=$(DBGFLAGS) -diag-disable=8293
DBGCFLAGS=$(DBGFLAGS) -diag-disable=161,167
else # DEBUG
OPTFLAGS=-O0
OPTFFLAGS=$(OPTFLAGS)
OPTCFLAGS=$(OPTFLAGS)
DBGFLAGS += -$(DEBUG) -debug emit_column -debug extended -debug inline-debug-info -debug pubnames
ifneq ($(ARCH),Darwin)
DBGFLAGS += -debug parallel
endif # Linux
DBGFFLAGS=$(DBGFLAGS) -debug-parameters all -check all -warn all -diag-disable=7712,8293
DBGCFLAGS=$(DBGFLAGS) -check=stack,uninit -diag-disable=161,167
endif # ?NDEBUG
LIBFLAGS=-DUSE_MKL
ifneq ($(ABI),lp64)
LIBFLAGS += -DMKL_ILP64
endif # ilp64
LIBFLAGS += -I. -I../vn -I${MKLROOT}/include/intel64/$(ABI) -I${MKLROOT}/include
LDFLAGS=-rdynamic
ifeq ($(ARCH),Darwin)
LDFLAGS += -L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -lmkl_intel_$(ABI) -lmkl_intel_thread -lmkl_core
else # Linux
LIBFLAGS += -D_GNU_SOURCE
LDFLAGS += -static-libgcc -L${MKLROOT}/lib/intel64 -Wl,-rpath=${MKLROOT}/lib/intel64 -lmkl_intel_$(ABI) -lmkl_intel_thread -lmkl_core $(shell if [ -L /usr/lib64/libmemkind.so ]; then echo '-lmemkind'; fi)
endif # ?Darwin
LDFLAGS += -lpthread -lm -ldl
FFLAGS=$(OPTFFLAGS) $(DBGFFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFFLAGS)
CFLAGS=$(OPTCFLAGS) $(DBGCFLAGS) $(LIBFLAGS) $(C18FLAGS) $(FPUCFLAGS)
