SHELL=/bin/bash
ARCH=$(shell uname)
ifndef ABI
ABI=ilp64
endif # !ABI
ifndef WP
WP=8
endif # !WP
ifdef NDEBUG
DEBUG=
else # DEBUG
DEBUG=g
endif # ?NDEBUG
RM=rm -rfv
AR=ar
ARFLAGS=rsv
ifndef MARCH
MARCH=native
endif # !MARCH
CPUFLAGS=-DUSE_NVIDIA -DUSE_X64 -DQX_WP=$(WP) -m64 -mp -KPIC -Mframe -Meh_frame -Minfo -tp=$(MARCH) -nvmalloc -traceback
ifeq ($(ABI),lp64)
CPUFLAGS += -DVN_INTEGER_KIND=4
endif # lp64
CPUFLAGS += -DTSC_FREQ_HZ=$(shell if [ -r /etc/redhat-release ]; then echo `dmesg | grep 'TSC clocksource calibration' | cut -d':' -f3 | cut -d' ' -f2 | sed 's/\.//g'`000; else echo 0; fi)ull
FORFLAGS=$(CPUFLAGS) -Mdclchk -Mlarge_arrays -Mrecursive -Mstack_arrays
ifeq ($(ABI),ilp64)
FORFLAGS += -i8
endif # ilp64
C18FLAGS=$(CPUFLAGS) -c18
CC=nvc
FC=nvfortran
CXX=nvc++
FPUFLAGS=-Kieee -Mfma -Mnodaz -Mnoflushz -Mnofpapprox -Mnofprelaxed -Mno-recip-div
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG)
OPTFFLAGS=$(OPTFLAGS)
OPTCFLAGS=$(OPTFLAGS)
DBGFLAGS=-DNDEBUG
DBGFFLAGS=$(DBGFLAGS)
DBGCFLAGS=$(DBGFLAGS) --diag_suppress set_but_not_used
FPUFFLAGS=$(FPUFLAGS)
FPUCFLAGS=$(FPUFLAGS)
else # DEBUG
OPTFLAGS=-O0
OPTFFLAGS=$(OPTFLAGS)
OPTCFLAGS=$(OPTFLAGS)
DBGFLAGS=-g -Mbounds -Mchkstk
DBGFFLAGS=$(DBGFLAGS)
DBGCFLAGS=$(DBGFLAGS)
FPUFFLAGS=$(FPUFLAGS)
FPUCFLAGS=$(FPUFLAGS)
endif # ?NDEBUG
LIBFLAGS=-I. -I../vn -D_GNU_SOURCE
ifneq ($(ABI),lp64)
LIBFLAGS += -DMKL_ILP64
endif # ilp64
LDFLAGS=-Wl,-E -static-nvidia
ifdef MKLROOT
LIBFLAGS += -DUSE_MKL -I${MKLROOT}/include/intel64/$(ABI) -I${MKLROOT}/include
# define MKL=pgi_thread for a threaded MKL
ifndef MKL
MKL=sequential
endif # !MKL
#-L${MKLROOT}/lib/intel64 -lmkl_intel_$(ABI) -lmkl_$(MKL) -lmkl_core
LDFLAGS += -Wl,--start-group ${MKLROOT}/lib/libmkl_intel_$(ABI).a ${MKLROOT}/lib/libmkl_$(MKL).a ${MKLROOT}/lib/libmkl_core.a -Wl,--end-group
LDFLAGS += $(shell if [ -L /usr/lib64/libmemkind.so ]; then echo '-lmemkind'; fi)
else # !MKLROOT
ifndef LAPACK
LAPACK=$(HOME)/lapack_$(ABI)
endif # !LAPACK
LDFLAGS += -L$(LAPACK) -ltmglib -llapack -lrefblas
endif # ?MKLROOT
LDFLAGS += -pgf90libs -lpthread -lm -ldl
FFLAGS=$(OPTFFLAGS) $(DBGFFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFFLAGS)
CFLAGS=$(OPTCFLAGS) $(DBGCFLAGS) $(LIBFLAGS) $(C18FLAGS) $(FPUCFLAGS)
