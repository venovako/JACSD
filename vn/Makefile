RM=del /F
AR=xilib.exe
ARFLAGS=-qnoipo -lib /NOLOGO /VERBOSE
CC=icl.exe
C99FLAGS=/nologo /DUSE_INTEL /Qstd=c99 /Qlong-double
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
FPUFLAGS=/fp:strict /Qfma /Qfp-stack-check /Qftz- /Qcomplex-limited-range- /Qfast-transcendentals- /Qprec-div /Qprec-sqrt
LIBFLAGS=/I. /MDd
LDFLAGS=/link /DEBUG /LIBPATH:. $(LIBS)
!ENDIF # ?NDEBUG
CFLAGS=$(OPTFLAGS) $(DBGFLAGS) $(LIBFLAGS) $(C99FLAGS) $(FPUFLAGS)

SRC=\
vn_align.c  \
vn_alloc.c  \
vn_assert.c \
vn_attrs.c  \
vn_blas.c   \
vn_bmp.c    \
vn_error.c  \
vn_lapack.c \
vn_lib.c    \
vn_mtxvis.c \
vn_simd.c   \
vn_timer.c  \
vn_types.c

OBJS=\
vn_align.obj  \
vn_alloc.obj  \
vn_assert.obj \
vn_attrs.obj  \
vn_blas.obj   \
vn_bmp.obj    \
vn_error.obj  \
vn_lapack.obj \
vn_lib.obj    \
vn_mtxvis.obj \
vn_simd.obj   \
vn_timer.obj  \
vn_types.obj

HDRS=\
vn_align.h  \
vn_alloc.h  \
vn_assert.h \
vn_attrs.h  \
vn_blas.h   \
vn_bmp.h    \
vn_error.h  \
vn_lapack.h \
vn_lib.h    \
vn_mtxvis.h \
vn_simd.h   \
vn_timer.h  \
vn_types.h

LIBS=vn.lib

all: ..\$(LIBS)

help:
	@echo "nmake.exe [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

..\$(LIBS): $(OBJS) Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS)

vn_align.obj: vn_align.c $(HDRS) Makefile
	$(CC) $(CFLAGS) /c vn_align.c

vn_alloc.obj: vn_alloc.c $(HDRS) Makefile
	$(CC) $(CFLAGS) /c vn_alloc.c

vn_assert.obj: vn_assert.c $(HDRS) Makefile
	$(CC) $(CFLAGS) /c vn_assert.c

vn_attrs.obj: vn_attrs.c $(HDRS) Makefile
	$(CC) $(CFLAGS) /c vn_attrs.c

vn_blas.obj: vn_blas.c $(HDRS) Makefile
	$(CC) $(CFLAGS) /c vn_blas.c

vn_bmp.obj: vn_bmp.c $(HDRS) Makefile
	$(CC) $(CFLAGS) /c vn_bmp.c

vn_error.obj: vn_error.c $(HDRS) Makefile
	$(CC) $(CFLAGS) /c vn_error.c

vn_lapack.obj: vn_lapack.c $(HDRS) Makefile
	$(CC) $(CFLAGS) /c vn_lapack.c

vn_lib.obj: vn_lib.c $(HDRS) Makefile
	$(CC) $(CFLAGS) /c vn_lib.c

vn_mtxvis.obj: vn_mtxvis.c $(HDRS) Makefile
	$(CC) $(CFLAGS) /c vn_mtxvis.c

vn_simd.obj: vn_simd.c $(HDRS) Makefile
	$(CC) $(CFLAGS) /c vn_simd.c

vn_timer.obj: vn_timer.c $(HDRS) Makefile
	$(CC) $(CFLAGS) /c vn_timer.c

vn_types.obj: vn_types.c $(HDRS) Makefile
	$(CC) $(CFLAGS) /c vn_types.c

clean:
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) ..\$(LIBS)
