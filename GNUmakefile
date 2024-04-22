ifndef COMPILER
COMPILER=gnu
endif # !COMPILER
include $(COMPILER).mk
MKFS=GNUmakefile $(COMPILER).mk

.PHONY: all help clean

all: libjstrat$(DEBUG).a libqxblas$(WP)$(ABI)$(DEBUG).a libvn$(DEBUG).a # libl0c$(DEBUG).a

help:
	@echo "gmake [WP=4|8|10|16] [COMPILER=gnu|x64|x64x|x200|nvidia] [NDEBUG=0|1|2|3|4|5] [ABI=ilp64|lp64] [all|clean|help]"

libl0c$(DEBUG).a: libjstrat$(DEBUG).a libqxblas$(WP)$(ABI)$(DEBUG).a libvn$(DEBUG).a $(MKFS)
ifneq ($(ABI),lp64)
ifdef NDEBUG
	pushd src && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) ABI=$(ABI) && popd
else # DEBUG
	pushd src && $(MAKE) COMPILER=$(COMPILER) ABI=$(ABI) && popd
endif # ?NDEBUG
endif # !lp64

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
ifneq ($(ABI),lp64)
	pushd src && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) ABI=$(ABI) clean && popd
endif # !lp64
else # DEBUG
	pushd vn && $(MAKE) COMPILER=$(COMPILER) ABI=$(ABI) clean && popd
	pushd jstrat && $(MAKE) COMPILER=$(COMPILER) ABI=$(ABI) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) COMPILER=$(COMPILER) ABI=$(ABI) clean && popd
ifneq ($(ABI),lp64)
	pushd src && $(MAKE) COMPILER=$(COMPILER) ABI=$(ABI) clean && popd
endif # !lp64
endif # ?NDEBUG
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *.opt.yaml
	-$(RM) *__genmod.f90
	-$(RM) *__genmod.mod
	-$(RM) *.dSYM
