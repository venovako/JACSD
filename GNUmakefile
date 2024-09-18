ifndef COMPILER
COMPILER=x64x
endif # !COMPILER
include $(COMPILER).mk
MKFS=GNUmakefile $(COMPILER).mk

.PHONY: all help clean

all: libjstrat$(DEBUG).a libqxblas$(WP)$(ABI)$(DEBUG).a libvn$(DEBUG).a

help:
	@echo "gmake [COMPILER=x64x|x200|x64|gnu|nvidia] [MARCH=...] [NDEBUG=optimization_level] [ABI=ilp64|lp64] [WP=...] [all|clean|help]"

libjstrat$(DEBUG).a: $(MKFS)
ifdef NDEBUG
	pushd jstrat && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) ABI=$(ABI) && popd
else # DEBUG
	pushd jstrat && $(MAKE) COMPILER=$(COMPILER) ABI=$(ABI) && popd
endif # ?NDEBUG

libqxblas$(WP)$(ABI)$(DEBUG).a: $(MKFS)
ifdef NDEBUG
	pushd qxblas && $(MAKE) WP=$(WP) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) ABI=$(ABI) && popd
else # DEBUG
	pushd qxblas && $(MAKE) WP=$(WP) COMPILER=$(COMPILER) ABI=$(ABI) && popd
endif # ?NDEBUG

libvn$(DEBUG).a: $(MKFS)
ifdef NDEBUG
	pushd vn && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) ABI=$(ABI) && popd
else # DEBUG
	pushd vn && $(MAKE) COMPILER=$(COMPILER) ABI=$(ABI) && popd
endif # ?NDEBUG

clean:
ifdef NDEBUG
	pushd vn && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) ABI=$(ABI) clean && popd
	pushd jstrat && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) ABI=$(ABI) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) ABI=$(ABI) clean && popd
else # DEBUG
	pushd vn && $(MAKE) COMPILER=$(COMPILER) ABI=$(ABI) clean && popd
	pushd jstrat && $(MAKE) COMPILER=$(COMPILER) ABI=$(ABI) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) COMPILER=$(COMPILER) ABI=$(ABI) clean && popd
endif # ?NDEBUG
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *.opt.yaml
	-$(RM) *__genmod.f90
	-$(RM) *__genmod.mod
	-$(RM) *.dSYM
