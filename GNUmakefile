ifndef COMPILER
COMPILER=gnu
endif # !COMPILER
include $(COMPILER).mk
MKFS=GNUmakefile $(COMPILER).mk

.PHONY: all help clean

all: libjstrat$(PROFILE)$(DEBUG).a libqxblas$(WP)$(PROFILE)$(DEBUG).a libvn$(PROFILE)$(DEBUG).a # libl0c$(PROFILE)$(DEBUG).a

help:
	@echo "gmake [WP=4|8|10|16] [COMPILER=gnu|x64|x200|nvidia] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

libl0c$(PROFILE)$(DEBUG).a: libjstrat$(PROFILE)$(DEBUG).a libqxblas$(WP)$(PROFILE)$(DEBUG).a libvn$(PROFILE)$(DEBUG).a $(MKFS)
ifneq ($(COMPILER),nvidia)
ifdef NDEBUG
ifdef PROFILE
	pushd src && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd src && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) && popd
endif # ?PROFILE
else # DEBUG
ifdef PROFILE
	pushd src && $(MAKE) COMPILER=$(COMPILER) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd src && $(MAKE) COMPILER=$(COMPILER) && popd
endif # ?PROFILE
endif # ?NDEBUG
endif # !NVIDIA

libjstrat$(PROFILE)$(DEBUG).a: $(MKFS)
ifdef NDEBUG
ifdef PROFILE
	pushd jstrat && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd jstrat && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) && popd
endif # ?PROFILE
else # DEBUG
ifdef PROFILE
	pushd jstrat && $(MAKE) COMPILER=$(COMPILER) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd jstrat && $(MAKE) COMPILER=$(COMPILER) && popd
endif # ?PROFILE
endif # ?NDEBUG

libqxblas$(WP)$(PROFILE)$(DEBUG).a: $(MKFS)
ifdef NDEBUG
ifdef PROFILE
	pushd qxblas && $(MAKE) WP=$(WP) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd qxblas && $(MAKE) WP=$(WP) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) && popd
endif # ?PROFILE
else # DEBUG
ifdef PROFILE
	pushd qxblas && $(MAKE) WP=$(WP) COMPILER=$(COMPILER) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd qxblas && $(MAKE) WP=$(WP) COMPILER=$(COMPILER) && popd
endif # ?PROFILE
endif # ?NDEBUG

libvn$(PROFILE)$(DEBUG).a: $(MKFS)
ifdef NDEBUG
ifdef PROFILE
	pushd vn && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd vn && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) && popd
endif # ?PROFILE
else # DEBUG
ifdef PROFILE
	pushd vn && $(MAKE) COMPILER=$(COMPILER) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd vn && $(MAKE) COMPILER=$(COMPILER) && popd
endif # ?PROFILE
endif # ?NDEBUG

clean:
ifdef NDEBUG
ifdef PROFILE
	pushd vn && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) clean && popd
	pushd jstrat && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) clean && popd
ifneq ($(COMPILER),nvidia)
	pushd src && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) clean && popd
endif # !NVIDIA
else # !PROFILE
	pushd vn && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) clean && popd
	pushd jstrat && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) clean && popd
ifneq ($(COMPILER),nvidia)
	pushd src && $(MAKE) COMPILER=$(COMPILER) NDEBUG=$(NDEBUG) clean && popd
endif # !NVIDIA
endif # ?PROFILE
else # DEBUG
ifdef PROFILE
	pushd vn && $(MAKE) COMPILER=$(COMPILER) PROFILE=$(PROFILE) clean && popd
	pushd jstrat && $(MAKE) COMPILER=$(COMPILER) PROFILE=$(PROFILE) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) COMPILER=$(COMPILER) PROFILE=$(PROFILE) clean && popd
ifneq ($(COMPILER),nvidia)
	pushd src && $(MAKE) COMPILER=$(COMPILER) PROFILE=$(PROFILE) clean && popd
endif # !NVIDIA
else # !PROFILE
	pushd vn && $(MAKE) COMPILER=$(COMPILER) clean && popd
	pushd jstrat && $(MAKE) COMPILER=$(COMPILER) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) COMPILER=$(COMPILER) clean && popd
ifneq ($(COMPILER),nvidia)
	pushd src && $(MAKE) COMPILER=$(COMPILER) clean && popd
endif # !NVIDIA
endif # ?PROFILE
endif # ?NDEBUG
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.optrpt
	-$(RM) *__genmod.f90
	-$(RM) *__genmod.mod
	-$(RM) *.dSYM
