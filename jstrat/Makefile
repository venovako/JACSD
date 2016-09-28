!IFNDEF RM
RM=del /F
!ENDIF # !RM
AR=xilib.exe
ARFLAGS=-qnoipo -lib /NOLOGO /VERBOSE
CC=icl.exe
C99FLAGS=/nologo /DUSE_INTEL /DUSE_X64 /Qstd=c99 /Qlong-double
LIBS=jstrat.lib
!IFDEF NDEBUG
OPTFLAGS=/O$(NDEBUG) /QxHost
DBGFLAGS=/DNDEBUG /Qopt-report:5 /traceback /W5
FPUFLAGS=/fp:source /Qfma /Qftz- /Qcomplex-limited-range- /Qfast-transcendentals- /Qprec-div /Qprec-sqrt
LIBFLAGS=/I. /MD
LDFLAGS=/link /RELEASE /LIBPATH:. $(LIBS)
!ELSE # DEBUG
OPTFLAGS=/Od /QxHost
DBGFLAGS=/debug:full /debug:expr-source-pos /debug:inline-debug-info /check:stack,uninit /warn:all /traceback /W5
FPUFLAGS=/fp:source /Qfma /Qftz- /Qcomplex-limited-range- /Qfast-transcendentals- /Qprec-div /Qprec-sqrt #/fp:strict /Qfp-stack-check
LIBFLAGS=/I. /MDd
LDFLAGS=/link /DEBUG /LIBPATH:. $(LIBS)
!ENDIF # ?NDEBUG
CFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(C99FLAGS) $(FPUFLAGS)

OBJS=jstrat.obj

all: ..\$(LIBS)

help:
	@echo "nmake.exe [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

..\$(LIBS): $(OBJS) Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS)

jstrat.obj: jstrat.c jstrat.h jstrat_ME.h Makefile
	$(CC) $(CFLAGS) /c jstrat.c

clean:
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) ../$(LIBS)
