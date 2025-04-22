!IFNDEF MARCH
MARCH=Host
!ENDIF # !MARCH
!IFNDEF NDEBUG
NDEBUG=d
!ENDIF # !NDEBUG
!IFNDEF WP
WP=16
!ENDIF # !WP
!IFNDEF ABI
ABI=ilp64
!ENDIF # !ABI

all: jstrat.lib qxblas$(WP)$(ABI).lib tests

help:
	@echo "nmake.exe [MARCH=Host|...] [NDEBUG=d|1|2|3|...] [ABI=ilp64|lp64] [WP=...] [all|clean|help]"

jstrat.lib: Makefile
	pushd jstrat && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) && popd

qxblas$(WP)$(ABI).lib: Makefile
	pushd qxblas && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) && popd	

tests: qxblas$(WP)$(ABI).lib Makefile
	pushd tortho && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) && popd
	pushd tgenskew && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) && popd
	pushd tgenevd && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) && popd
	pushd tgensvd && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) && popd
	pushd tgenhsvd && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) && popd
	pushd tgengsvd && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) && popd

clean:
	pushd jstrat && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) clean && popd
	pushd qxblas && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) clean && popd
	pushd tortho && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) clean && popd
	pushd tgenskew && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) clean && popd
	pushd tgenevd && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) clean && popd
	pushd tgensvd && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) clean && popd
	pushd tgenhsvd && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) clean && popd
	pushd tgengsvd && $(MAKE) /NOLOGO MARCH=$(MARCH) NDEBUG=$(NDEBUG) ABI=$(ABI) WP=$(WP) clean && popd
