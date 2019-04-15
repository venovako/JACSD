ifndef WP
WP=8
endif # !WP
SHELL=/usr/local/bin/bash
ARCH=$(shell uname)
RM=rm -rfv
AR=ar
ARFLAGS=rsv
CPUFLAGS=-DUSE_LLVM -DUSE_X64
FORFLAGS=-cpp $(CPUFLAGS) -fdefault-integer-8 -fopenmp
C11FLAGS=$(CPUFLAGS) -std=gnu17 #-fopenmp
CC=clang
FC=flang
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -march=native
DBGFLAGS=-DNDEBUG
OPTFFLAGS=$(OPTFLAGS)
OPTCFLAGS=$(OPTFLAGS)
DBGFFLAGS=$(DBGFLAGS) -pedantic -Wall -Wextra
DBGCFLAGS=$(DBGFLAGS)
FPUFLAGS=-ffp-contract=fast
FPUFFLAGS=$(FPUFLAGS)
FPUCFLAGS=$(FPUFLAGS) -fno-math-errno
else # DEBUG
OPTFLAGS=-Og -march=native
OPTFFLAGS=$(OPTFLAGS)
OPTCFLAGS=$(OPTFLAGS)
DBGFLAGS=-g
DBGFFLAGS=$(DBGFLAGS) -pedantic -Wall -Wextra
DBGCFLAGS=$(DBGFLAGS) -ftrapv
FPUFLAGS=-ffp-contract=fast
FPUFFLAGS=$(FPUFLAGS)
FPUCFLAGS=$(FPUFLAGS)
endif # ?NDEBUG
LIBFLAGS=-I. -I/usr/local/include
LDFLAGS=-L/usr/local/lib -llapack -lblas -lgfortran -lompstub -lpthread -lm
FFLAGS=$(OPTFFLAGS) $(DBGFFLAGS) $(LIBFLAGS) $(FORFLAGS) $(FPUFFLAGS)
CFLAGS=$(OPTCFLAGS) $(DBGCFLAGS) $(LIBFLAGS) $(C11FLAGS) $(FPUCFLAGS)
