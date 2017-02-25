ifndef SHELL
SHELL=/bin/bash
endif # !SHELL
ifndef ARCH
ARCH=$(shell uname)
endif # !ARCH
ifndef WP
WP=16
endif # !WP
ifndef RM
RM=rm -fv
endif # !RM
AR=ar
ARFLAGS=rsv
FC=gfortran
CC=gcc
FORFLAGS=-DUSE_GNU -DUSE_PWR8 -fdefault-integer-8 -fopenmp -fstack-arrays
C11FLAGS=-DUSE_GNU -DUSE_PWR8 -DVN_INTEGER_KIND=8 -std=gnu11
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -mcpu=power8 -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
OPTCFLAGS=-O$(NDEBUG) -mcpu=power8 -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller
DBGFLAGS=-DNDEBUG -fopt-info-optimized-vec
DBGCFLAGS=-DNDEBUG -fopt-info-optimized-vec
FPUFLAGS=
FPUCFLAGS=-ffp-contract=fast -fno-math-errno
else # DEBUG
OPTFLAGS=-Og -mcpu=power8
OPTCFLAGS=-Og -mcpu=power8
DBGFLAGS=-g -fcheck=all -finit-local-zero -finit-real=snan
DBGCFLAGS=-g
FPUFLAGS=-ffpe-trap=invalid,zero,overflow
FPUCFLAGS=-ffp-contract=fast
endif # ?NDEBUG
