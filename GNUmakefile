ifeq ($(CPU),x64) # Xeon / Intel Fortran
include x64.mk
MKFS=GNUmakefile x64.mk
else ifeq ($(CPU),x200) # Knights Landing / Intel Fortran
include x200.mk
MKFS=GNUmakefile x200.mk
else ifeq ($(CPU),llvm) # Flang + Clang
include llvm.mk
MKFS=GNUmakefile llvm.mk
else # GNU Fortran + (GNU C or Clang)
ifndef CPU
CPU=gnu
endif # !CPU
include gnu.mk
MKFS=GNUmakefile gnu.mk
endif # ?CPU

.PHONY: all help clean

all: libjstrat.a libqxblas.a libvn.a # libl0c.a

help:
	@echo "make [WP=4|8|10|16] [CPU=x64|x100|x200|llvm|gnu] [NDEBUG=0|1|2|3|4|5] [all|clean|help]"

libl0c.a: libjstrat.a libqxblas.a libvn.a $(MKFS)
ifdef NDEBUG
	pushd src && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
else # DEBUG
	pushd src && $(MAKE) CPU=$(CPU) && popd
endif # ?NDEBUG

libjstrat.a: $(MKFS)
ifdef NDEBUG
	pushd jstrat && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
else # DEBUG
	pushd jstrat && $(MAKE) CPU=$(CPU) && popd
endif # ?NDEBUG

qx_wp.fi libqxblas.a: $(MKFS)
ifdef NDEBUG
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
else # DEBUG
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) && popd
endif # ?NDEBUG

libvn.a: $(MKFS)
ifdef NDEBUG
	pushd vn && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) && popd
else # DEBUG
	pushd vn && $(MAKE) CPU=$(CPU) && popd
endif # ?NDEBUG

clean:
ifdef NDEBUG
	pushd vn && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) clean && popd
	pushd jstrat && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) NDEBUG=$(NDEBUG) clean && popd
	pushd src && $(MAKE) CPU=$(CPU) NDEBUG=$(NDEBUG) clean && popd
else # DEBUG
	pushd vn && $(MAKE) CPU=$(CPU) clean && popd
	pushd jstrat && $(MAKE) CPU=$(CPU) clean && popd
	pushd qxblas && $(MAKE) WP=$(WP) CPU=$(CPU) clean && popd
	pushd src && $(MAKE) CPU=$(CPU) clean && popd
endif # ?NDEBUG
	-$(RM) *.exe
	-$(RM) *.mod
	-$(RM) *.o
	-$(RM) *.a
	-$(RM) *.optrpt
	-$(RM) *__genmod.f90
	-$(RM) *__genmod.mod
	-$(RM) *.dSYM
