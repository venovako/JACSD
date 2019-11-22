SHELL=/bin/bash
ARCH=$(shell uname)
ifeq ($(ARCH),Darwin)
$(error PGI on macOS is not supported)
endif # Darwin
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
CPUFLAGS=-DUSE_PGI -DUSE_X64 -m64 -mp
FORFLAGS=$(CPUFLAGS) -i8 -Mdclchk -Mlarge_arrays -Mrecursive -Mstack_arrays
C11FLAGS=$(CPUFLAGS) -c11
CC=pgcc
FC=pgfortran
FPUFLAGS=-Kieee -Mfma -Mnodaz -Mnoflushz -Mnofpapprox -Mnofprelaxed
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG)
OPTFFLAGS=$(OPTFLAGS)
OPTCFLAGS=$(OPTFLAGS)
DBGFLAGS=-DNDEBUG -Minfo
DBGFFLAGS=$(DBGFLAGS)
DBGCFLAGS=$(DBGFLAGS)
FPUFFLAGS=$(FPUFLAGS)
FPUCFLAGS=$(FPUFLAGS)
else # DEBUG
OPTFLAGS=-O0
OPTFFLAGS=$(OPTFLAGS)
OPTCFLAGS=$(OPTFLAGS)
DBGFLAGS=-g -Minfo -Mbounds -Mchkstk -traceback
DBGFFLAGS=$(DBGFLAGS)
DBGCFLAGS=$(DBGFLAGS)
FPUFFLAGS=$(FPUFLAGS)
FPUCFLAGS=$(FPUFLAGS)
endif # ?NDEBUG
LIBFLAGS=-DUSE_MKL -DMKL_ILP64 -I. -I../vn -I${MKLROOT}/include/intel64/ilp64 -I${MKLROOT}/include
LDFLAGS=-L.. -lvn$(DEBUG)
ifeq ($(ARCH),Darwin)
ifeq ($(MKL),par)
LDFLAGS=${MKLROOT}/lib/libmkl_intel_ilp64.a ${MKLROOT}/lib/libmkl_intel_ilp64.a ${MKLROOT}/lib/libmkl_pgi_thread.a ${MKLROOT}/lib/libmkl_core.a -pgf90libs -lpthread -lm -ldl
else # seq
LDFLAGS=${MKLROOT}/lib/libmkl_intel_ilp64.a ${MKLROOT}/lib/libmkl_sequential.a ${MKLROOT}/lib/libmkl_core.a -pgf90libs -lpthread -lm -ldl
endif # ?MKL
else # Linux
LIBFLAGS += -D_GNU_SOURCE
ifeq ($(MKL),par)
LDFLAGS += -L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_pgi_thread -lmkl_core -pgf90libs -lpthread -lm -ldl
else # seq
LDFLAGS += -L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_sequential -lmkl_core -pgf90libs -lpthread -lm -ldl
endif # ?MKL
endif # ?Darwin
LDFLAGS += $(shell if [ -L /usr/lib64/libmemkind.so ]; then echo '-lmemkind'; fi)
FFLAGS=$(OPTFFLAGS) $(DBGFFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFFLAGS)
CFLAGS=$(OPTCFLAGS) $(DBGCFLAGS) $(LIBFLAGS) $(C11FLAGS) $(FPUCFLAGS)
