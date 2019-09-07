ifndef CPU
CPU=gnu
endif # !CPU
include ../$(CPU).mk
MKFS=GNUmakefile ../$(CPU).mk

SOURCES=     \
iqamax.F90   \
ixamax.F90   \
qasum.F90    \
qaxpy.F90    \
qcopy.F90    \
qdot.F90     \
qgbmv.F90    \
qgemm.F90    \
qgemv.F90    \
qger.F90     \
qisnan.F90   \
qlacpy.F90   \
qlaev2.F90   \
qlagsy.F90   \
qlaisnan.F90 \
qlamch.F90   \
qlangb.F90   \
qlange.F90   \
qlansb.F90   \
qlansp.F90   \
qlansy.F90   \
qlapy2.F90   \
qlaran.F90   \
qlarfg.F90   \
qlarnd.F90   \
qlarnv.F90   \
qlaror.F90   \
qlartg.F90   \
qlaruv.F90   \
qlaset.F90   \
qlassq.F90   \
qnrm2.F90    \
qrot.F90     \
qrotg.F90    \
qrotm.F90    \
qrotmg.F90   \
qsbmv.F90    \
qscal.F90    \
qspmv.F90    \
qspr.F90     \
qspr2.F90    \
qswap.F90    \
qsymm.F90    \
qsymv.F90    \
qsyr.F90     \
qsyr2.F90    \
qsyr2k.F90   \
qsyrk.F90    \
qtbmv.F90    \
qtbsv.F90    \
qtpmv.F90    \
qtpsv.F90    \
qtrmm.F90    \
qtrmv.F90    \
qtrsm.F90    \
qtrsv.F90    \
qxabs1.F90   \
qxasum.F90   \
qxnrm2.F90   \
xaxpy.F90    \
xcopy.F90    \
xdotc.F90    \
xdotu.F90    \
xgbmv.F90    \
xgemm.F90    \
xgemv.F90    \
xgerc.F90    \
xgeru.F90    \
xhbmv.F90    \
xhemm.F90    \
xhemv.F90    \
xher.F90     \
xher2.F90    \
xher2k.F90   \
xherk.F90    \
xhpmv.F90    \
xhpr.F90     \
xhpr2.F90    \
xlacgv.F90   \
xlacpy.F90   \
xlaev2.F90   \
xlaghe.F90   \
xlarnd.F90   \
xlarnv.F90   \
xlaror.F90   \
xlaset.F90   \
xqrot.F90    \
xqscal.F90   \
xrotg.F90    \
xscal.F90    \
xswap.F90    \
xsymm.F90    \
xsyr2k.F90   \
xsyrk.F90    \
xtbmv.F90    \
xtbsv.F90    \
xtpmv.F90    \
xtpsv.F90    \
xtrmm.F90    \
xtrmv.F90    \
xtrsm.F90    \
xtrsv.F90
OBJS=$(SOURCES:.F90=.o)

.PHONY: all help prepare clean

all: ../libqxblas$(WP)$(PROFILE)$(DEBUG).a

help:
	@echo "gmake [WP=4|8|10|16] [CPU=x64|x200|gnu] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

prepare:
	echo '      INTEGER, PARAMETER :: WP =' $(WP) > ../qx_wp.fi

../libqxblas$(WP)$(PROFILE)$(DEBUG).a: prepare $(OBJS) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

%.o: %.F90 $(MKFS)
	$(FC) -I.. $(FFLAGS) -c $< -o $@

clean:
	-$(RM) ../libqxblas$(WP)$(PROFILE)$(DEBUG).a
	-$(RM) ../qx_wp.fi
	-$(RM) *.optrpt
	-$(RM) $(OBJS)
	-$(RM) *__genmod.mod
	-$(RM) *__genmod.f90
	-$(RM) *.dSYM
