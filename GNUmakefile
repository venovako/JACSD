ifndef CPU
CPU=gnu
endif # !CPU
include $(CPU).mk
MKFS=GNUmakefile $(CPU).mk

.PHONY: all help clean

all: libjstrat$(PROFILE)$(DEBUG).a libqxblas$(WP)$(PROFILE)$(DEBUG).a libvn$(PROFILE)$(DEBUG).a # libl0c$(PROFILE)$(DEBUG).a

help:
	@echo "gmake [WP=4|8|10|16] [CPU=x64|x200|gnu] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

libl0c$(PROFILE)$(DEBUG).a: libjstrat$(PROFILE)$(DEBUG).a libqxblas$(WP)$(PROFILE)$(DEBUG).a libvn$(PROFILE)$(DEBUG).a $(MKFS)
ifneq ($(CPU),nvidia)
ifdef NDEBUG
ifdef PROFILE
	pushd src && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd src && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
endif # ?PROFILE
else # DEBUG
ifdef PROFILE
	pushd src && $(MAKE) CPU=$(CPU) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd src && $(MAKE) CPU=$(CPU) && popd
endif # ?PROFILE
endif # ?NDEBUG
endif # !NVIDIA

libjstrat$(PROFILE)$(DEBUG).a: $(MKFS)
ifdef NDEBUG
ifdef PROFILE
	pushd jstrat && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd jstrat && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
endif # ?PROFILE
else # DEBUG
ifdef PROFILE
	pushd jstrat && $(MAKE) CPU=$(CPU) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd jstrat && $(MAKE) CPU=$(CPU) && popd
endif # ?PROFILE
endif # ?NDEBUG

libqxblas$(WP)$(PROFILE)$(DEBUG).a: $(MKFS)
ifdef NDEBUG
ifdef PROFILE
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
endif # ?PROFILE
else # DEBUG
ifdef PROFILE
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) && popd
endif # ?PROFILE
endif # ?NDEBUG

libvn$(PROFILE)$(DEBUG).a: $(MKFS)
ifdef NDEBUG
ifdef PROFILE
	pushd vn && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd vn && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
endif # ?PROFILE
else # DEBUG
ifdef PROFILE
	pushd vn && $(MAKE) CPU=$(CPU) PROFILE=$(PROFILE) && popd
else # !PROFILE
	pushd vn && $(MAKE) CPU=$(CPU) && popd
endif # ?PROFILE
endif # ?NDEBUG

clean:
ifdef NDEBUG
ifdef PROFILE
	pushd vn && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) clean && popd
	pushd jstrat && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) clean && popd
ifneq ($(CPU),nvidia)
	pushd src && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) PROFILE=$(PROFILE) clean && popd
endif # !NVIDIA
else # !PROFILE
	pushd vn && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) clean && popd
	pushd jstrat && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) NDEBUG=$(NDEBUG) clean && popd
ifneq ($(CPU),nvidia)
	pushd src && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) clean && popd
endif # !NVIDIA
endif # ?PROFILE
else # DEBUG
ifdef PROFILE
	pushd vn && $(MAKE) CPU=$(CPU) PROFILE=$(PROFILE) clean && popd
	pushd jstrat && $(MAKE) CPU=$(CPU) PROFILE=$(PROFILE) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) PROFILE=$(PROFILE) clean && popd
ifneq ($(CPU),nvidia)
	pushd src && $(MAKE) CPU=$(CPU) PROFILE=$(PROFILE) clean && popd
endif # !NVIDIA
else # !PROFILE
	pushd vn && $(MAKE) CPU=$(CPU) clean && popd
	pushd jstrat && $(MAKE) CPU=$(CPU) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) clean && popd
ifneq ($(CPU),nvidia)
	pushd src && $(MAKE) CPU=$(CPU) clean && popd
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
