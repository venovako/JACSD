SHELL=/bin/bash
ARCH=$(shell uname)
RM=rm -rfv
AR=ar
ifndef WP
WP=16
endif # !WP
ARFLAGS=rsv
ifdef USE_MPI
FC=mpfort -WF,-qfpp -WF,-qppsuborigarg
FORFLAGS=-WF,-DUSE_IBM -WF,-DUSE_PWR8 -WF,-DUSE_MPI -qintsize=8 -qnosave -qsclk=micro -qsmp=omp -qlanglvl=extended -qassert=contiguous:refalign -k -qxlf90=signedzero -qxlf2003=nooldnaninf:signdzerointr
else # no MPI
FC=xlf2008_r -WF,-qfpp -WF,-qppsuborigarg
FORFLAGS=-WF,-DUSE_IBM -WF,-DUSE_PWR8 -qintsize=8 -qnosave -qsclk=micro -qsmp=omp -qlanglvl=extended -qassert=contiguous:refalign -qstacktemp=-1 -qtbtable=full -qwarn64
endif # ?USE_MPI
CC=xlc_r
C11FLAGS=-DUSE_IBM -DUSE_PWR8 -DVN_INTEGER_KIND=8 -qsmp=omp -qlanglvl=extc1x
ifdef NDEBUG
OPTFLAGS=-O$(NDEBUG) -qmaxmem=-1 -qarch=pwr8 -qtune=pwr8:smt8 -qcache=auto -qhot=level=2
OPTCFLAGS=-O$(NDEBUG) -qmaxmem=-1 -qarch=pwr8 -qtune=pwr8:smt8 -qcache=auto -qhot=level=2
DBGFLAGS=-WF,-DNDEBUG -qinfo=mt #:unset
DBGCFLAGS=-DNDEBUG
FPUFLAGS=-qfloat=subnormals
FPUCFLAGS=-qfloat=subnormals
else # DEBUG
OPTFLAGS=-O0 -qmaxmem=-1 -qarch=pwr8 -qtune=pwr8:smt8 -qcache=auto
OPTCFLAGS=-O0 -qmaxmem=-1 -qarch=pwr8 -qtune=pwr8:smt8 -qcache=auto
DBGFLAGS=-qinit=f90ptr -qinitalloc -qinitauto -qcheck=all -qdbg=level=9 -qkeepparm -qinfo=mt #:unset
DBGCFLAGS=-g -qcheck
FPUFLAGS=-qfloat=subnormals
FPUCFLAGS=-qfloat=subnormals
endif # ?NDEBUG
