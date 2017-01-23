GMP_VER=6.0.0a
GMP_VER2=6.0.0
FFI_VER=3.2.1
GLIBC_VER=2.22
LINUX_VER=4.2.4
GHC_VER=7.10.3

GMP_DL=downloads/gmp-$(GMP_VER).tar.xz
FFI_DL=downloads/libffi-$(FFI_VER).tar.gz
GLIBC_DL=downloads/glibc-$(GLIBC_VER).tar.xz
LINUX_DL=downloads/linux-$(LINUX_VER).tar.xz

DIST_DIR=$(shell pwd)/usr/

GMP_LIB=$(DIST_DIR)/lib/libgmp.a
FFI_LIB=$(DIST_DIR)/lib/libffi.a
GLIBC_LIB=$(DIST_DIR)/lib/librt.a

all: $(GMP_LIB) $(FFI_LIB) $(GLIBC_LIB) linux-kernel init-disk

$(LINUX_DL):
	mkdir -p downloads
	cd downloads && \
	wget https://www.kernel.org/pub/linux/kernel/v4.x/linux-$(LINUX_VER).tar.xz && \
	tar xf linux-$(LINUX_VER).tar.xz

$(GMP_DL):
	mkdir -p downloads
	cd downloads && \
	wget https://gmplib.org/download/gmp/gmp-$(GMP_VER).tar.xz && \
	tar xf gmp-$(GMP_VER).tar.xz

$(FFI_DL):
	mkdir -p downloads
	cd downloads && \
	wget ftp://sourceware.org/pub/libffi/libffi-$(FFI_VER).tar.gz && \
	tar xf libffi-$(FFI_VER).tar.gz

$(GLIBC_DL):
	mkdir -p downloads
	cd downloads && \
	wget http://ftp.gnu.org/gnu/glibc/glibc-$(GLIBC_VER).tar.xz && \
	tar xf glibc-$(GLIBC_VER).tar.xz

linux-kernel: $(LINUX_DL)
	mkdir -p $(DIST_DIR)
	cd downloads/linux-$(LINUX_VER) && \
	make defconfig && \
	make -j 8 && \
	cp arch/x86/boot/bzImage ../../linux-kernel && \
	make modules

$(GMP_LIB): $(GMP_DL)
	mkdir -p $(DIST_DIR)
	cd downloads/gmp-$(GMP_VER2) && \
	./configure --prefix=$(DIST_DIR) && \
	make -j 8 install

$(FFI_LIB): $(FFI_DL)
	mkdir -p $(DIST_DIR)
	cd downloads/libffi-$(FFI_VER) && \
	./configure --prefix=$(DIST_DIR) && \
	make -j 8 install
	cp $(DIST_DIR)/lib/libffi.a $(DIST_DIR)/lib/

$(GLIBC_LIB): $(GLIBC_DL)
	mkdir -p $(DIST_DIR)
	cd downloads/glibc-$(GLIBC_VER) && \
	mkdir -p build && \
	cd build && \
	LD_LIBRARY_PATH= ../configure --prefix=$(DIST_DIR) && \
	make -j 8 && \
	make install

init-disk: src/Test.hs
	stack build
	mkdir -p disk
	# Add other files here if needed
	cp .stack-work/install/x86_64-linux/lts-5.0/$(GHC_VER)/bin/Test disk 
	cp .stack-work/install/x86_64-linux/lts-5.0/$(GHC_VER)/bin/KernelEvent disk 
	(cd disk ; find . | cpio -o -H newc | gzip) > init-disk

copy-modules:
	mkdir -p disk
	#cp -f downloads/linux-${LINUX_VER}/drivers/gpu/drm/drm.ko disk
	cp -f downloads/linux-${LINUX_VER}/drivers/platform/x86/wmi.ko disk
	#cp -f downloads/linux-${LINUX_VER}/drivers/gpu/drm/ttm/ttm.ko disk
	cp -f downloads/linux-${LINUX_VER}/drivers/gpu/drm/nouveau/nouveau.ko disk
	cp -f downloads/linux-${LINUX_VER}/drivers/gpu/drm/bochs/bochs-drm.ko disk

test: init-disk
	qemu-system-x86_64 -kernel linux-kernel -initrd init-disk -append "rdinit=/Test"

test-nographic: init-disk
	#qemu-system-x86_64 -serial stdio -kernel linux-kernel -initrd init-disk -append "rdinit=/Test console=ttyAMA0 console=ttyS0"
	qemu-system-x86_64 -serial stdio -kernel linux-kernel -initrd init-disk -append "rdinit=/Test console=ttyS0 atkbd.softraw=0"

kernelevent: init-disk
	qemu-system-x86_64 -serial stdio -kernel linux-kernel -initrd init-disk -append "rdinit=/KernelEvent console=ttyS0 atkbd.softraw=0"
