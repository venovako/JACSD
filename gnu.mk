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
RM=rm -rfv
AR=ar
ARFLAGS=rsv
CPUFLAGS=-DUSE_GNU -DUSE_X64 -DQX_WP=$(WP) -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fvect-cost-model=unlimited -fopenmp
ifeq ($(ABI),lp64)
CPUFLAGS += -DVN_INTEGER_KIND=4
endif # lp64
ifneq ($(ARCH),Darwin)
CPUFLAGS += -DTSC_FREQ_HZ=$(shell if [ `if [ -r /etc/redhat-release ]; then grep -c 'release 7' /etc/redhat-release; else echo 0; fi` = 1 ]; then echo `dmesg | grep 'TSC clocksource calibration' | cut -d':' -f3 | cut -d' ' -f2 | sed 's/\.//g'`000; else echo 0; fi)ull
endif # Linux
FORFLAGS=-cpp $(CPUFLAGS) -ffree-line-length-none -fstack-arrays
ifneq ($(ABI),lp64)
FORFLAGS += -fdefault-integer-8
endif # ilp64
C18FLAGS=$(CPUFLAGS) -std=gnu18
ifeq ($(ARCH),Darwin)
ifndef GNU
GNU=-8
endif # !GNU
endif # Darwin
CC=gcc$(GNU)
FC=gfortran$(GNU)
DBGFLAGS=-Wall -Wextra -Wno-deprecated
FPUFLAGS=-ffp-contract=fast
ifndef MARCH
MARCH=native
endif # !MARCH
ifeq ($(shell uname -m),ppc64le)
OPTFLAGS=-mcpu=$(MARCH) -mpower8-fusion -mtraceback=full
else # x86_64
OPTFLAGS=-march=$(MARCH)
ifeq ($(ARCH),Darwin)
OPTFLAGS += -Wa,-q
endif # Darwin
endif # ?ppc64le
ifdef NDEBUG
OPTFLAGS += -O$(NDEBUG)
DBGFLAGS += -DNDEBUG
OPTFFLAGS=$(OPTFLAGS)
OPTCFLAGS=$(OPTFLAGS)
DBGFFLAGS=$(DBGFLAGS) -Wno-compare-reals -Warray-temporaries -Wno-c-binding-type -Wcharacter-truncation -Wno-function-elimination -Wimplicit-procedure -Wrealloc-lhs-all
DBGCFLAGS=$(DBGFLAGS) -Wno-incompatible-pointer-types
FPUFLAGS += -fno-math-errno
FPUFFLAGS=$(FPUFLAGS)
FPUCFLAGS=$(FPUFLAGS)
else # DEBUG
OPTFLAGS += -O$(DEBUG)
DBGFLAGS += -$(DEBUG)
OPTFFLAGS=$(OPTFLAGS)
OPTCFLAGS=$(OPTFLAGS)
DBGFFLAGS=$(DBGFLAGS) -fcheck=array-temps -finit-local-zero -finit-real=snan -finit-derived -Wno-compare-reals -Warray-temporaries -Wno-c-binding-type -Wcharacter-truncation -Wno-function-elimination -Wimplicit-procedure -Wrealloc-lhs-all #-fcheck=all
DBGCFLAGS=$(DBGFLAGS) -Wno-incompatible-pointer-types #-ftrapv
FPUFFLAGS=$(FPUFLAGS)
FPUCFLAGS=$(FPUFLAGS)
endif # ?NDEBUG
LIBFLAGS=-I. -I../vn
ifeq ($(ARCH),Linux)
LIBFLAGS += -D_GNU_SOURCE
endif # Linux
ifneq ($(ABI),lp64)
LIBFLAGS += -DMKL_ILP64
endif # ilp64
LDFLAGS=-rdynamic -static-libgcc -static-libgfortran -static-libquadmath
ifdef MKLROOT
LIBFLAGS += -DUSE_MKL -I${MKLROOT}/include/intel64/$(ABI) -I${MKLROOT}/include
ifeq ($(ARCH),Darwin)
#-L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -L${MKLROOT}/../../compiler/latest/mac/compiler/lib -Wl,-rpath,${MKLROOT}/../../compiler/latest/mac/compiler/lib -lmkl_intel_$(ABI) -lmkl_intel_thread -lmkl_core -liomp5
LDFLAGS += ${MKLROOT}/lib/libmkl_intel_$(ABI).a ${MKLROOT}/lib/libmkl_intel_thread.a ${MKLROOT}/lib/libmkl_core.a ${MKLROOT}/../../compiler/latest/mac/compiler/lib/libiomp5.a
else # Linux
#-L${MKLROOT}/lib/intel64 -Wl,-rpath=${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_gf_$(ABI) -lmkl_gnu_thread -lmkl_core -lgomp
LDFLAGS += -Wl,--start-group ${MKLROOT}/lib/libmkl_gf_$(ABI).a ${MKLROOT}/lib/libmkl_gnu_thread.a ${MKLROOT}/lib/libmkl_core.a -Wl,--end-group #-lgomp
LDFLAGS += $(shell if [ -L /usr/lib64/libmemkind.so ]; then echo '-lmemkind'; fi)
endif # ?Darwin
else # !MKLROOT
ifndef LAPACK
LAPACK=$(HOME)/lapack-$(ABI)
endif # !LAPACK
LDFLAGS += -L$(LAPACK) -ltmglib -llapack -lrefblas
endif # ?MKLROOT
LDFLAGS += -lpthread -lm -ldl
FFLAGS=$(OPTFFLAGS) $(DBGFFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFFLAGS)
CFLAGS=$(OPTCFLAGS) $(DBGCFLAGS) $(LIBFLAGS) $(C18FLAGS) $(FPUCFLAGS)
