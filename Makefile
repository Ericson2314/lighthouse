KERNEL = kernel/house

MKBE2GBF = ./mkbe2gbf

TOP	= $(shell pwd)
GHCSRC  = ghc-6.2-src.tar.bz2
GHCMD5  = cc495e263f4384e1d6b38e851bf6eca0
GHCURL  = http://www.haskell.org/ghc/dist/6.2
GHCTOP	= $(TOP)/ghc-6.8.2
#GRUBDIR	= /usr/lib/grub/grub/i386-pc
#GRUBDIR	= /usr/share/grub/i386-redhat
#GRUBDIR	= /tmp/grub/share/grub/i386-pc
GRUBDIR	= /boot/grub/
MPOINT	= $(TOP)/floppy_dir
FLOPPYSIZE=1440

all:
	@$(MAKE) -C support
	@$(MAKE) $(KERNEL)
	@echo
	@echo "Kernel built successfully."
	@echo
	@echo "Enter 'make floppy' now to build a $(FLOPPYSIZE)KB floppy disk, suitable for loading in emulators such as bochs and qemu."
	@echo
	@echo "Enter 'make cdrom' now to build an ISO9660 image, which can be written on a CD-R to boot on a PC, and can also be used in an emulator.  Note that this requires mkisofs to be installed."
	@echo

boot:
	# TODO: Rewrite for 6.8.2 patchset
	#@if [ ! -f $(GHCSRC) ] ; then \
	  #echo "Downloading $(GHCSRC)..." ;\
	  #wget $(GHCURL)/$(GHCSRC) ;\
        #fi
	#@echo -n "Testing checksum of $(GHCSRC)..."
	#@if echo "$(GHCMD5)  $(GHCSRC)" | md5sum -c > /dev/null ; then \
	  #echo OK ;\
	#else \
	  #echo "failed!" ;\
	  #echo "You will need to fix this problem yourself, sorry." ;\
	  #exit 1 ;\
	#fi
	#@if [ -e $(GHCTOP) ] ; then \
	  #echo "The old directory containing the patched GHC:" ;\
	  #echo "	$(GHCTOP)" ;\
	  #echo "will be deleted;  press Return to continue, or Ctrl-C to abort." ;\
	  #read ;\
	  #rm -rf $(GHCTOP) ;\
	#fi
	#@echo "Unpacking $(GHCSRC)..."
	#@tar --get --bzip2 --file $(GHCSRC)
	#@echo "Done.  Now please do 'make'."

floppy: hOp.flp
	@echo
	@echo "The floppy image is called 'hOp.flp'."

cdrom: hOp.iso
	@echo
	@echo "The cdrom image is called 'hOp.iso'."

#patch-stamp: standalone.diff
patch-stamp:
	@if test ! -d $(GHCTOP); then \
		echo "No 'ghc-6.8.2' directory found." ; \
		echo "Please use 'make boot' to download and unpack GHC 6.8.2" ; \
		echo "(or do it yourself if you have ghc-6.8.2-src.tar.bz2 handy)."; \
		exit 1; \
	fi
	#patch -p0 < standalone.diff
	touch $@

configure-stamp: build.mk patch-stamp
	cp build.mk $(GHCTOP)/mk
	(cd $(GHCTOP) && autoreconf && ./configure --enable-standalone-rts)
	touch $@

ghc-stamp:
	#(cd $(GHCTOP) && $(MAKE) boot && $(MAKE) all)
	touch $@

# Quick fix:
ghc-inplace:
	echo "#!/bin/sh" >ghc-6.8.2/compiler/stage1/ghc-inplace
	echo "exec $(GHCTOP)/compiler/stage1/ghc-6.8.2 -B$(GHCTOP) \"\$$@\"" >>ghc-6.8.2/compiler/stage1/ghc-inplace

$(KERNEL): ghc-stamp .phony
	$(MAKE) -C kernel

G=$(MPOINT)/boot/grub
F=$(MPOINT)/fonts.hf
P=$(MPOINT)/pci.ids.gz

hOp.flp: $G/stage2 $(KERNEL) $G/grub.conf $F $P
	gzip -9 <$(KERNEL) >$(MPOINT)/boot/kernel
	$(MKBE2GBF) $(MPOINT) $@ $(FLOPPYSIZE) $(GRUBDIR)

osker.flp: $G/stage2 kernel/osker $G/grub.conf $F $P
	gzip -9 <kernel/osker >$(MPOINT)/boot/kernel
	$(MKBE2GBF) $(MPOINT) $@ $(FLOPPYSIZE) $(GRUBDIR)

kernel/osker: ghc-stamp .phony
	$(MAKE) -C kernel osker

$G/grub.conf: menu.lst $(MPOINT)
	mkdir -p $G
	-ln -s grub.conf $(MPOINT)/boot/grub/menu.lst
	cp menu.lst $(MPOINT)/boot/grub/grub.conf

$(MPOINT):
	mkdir $(MPOINT)

$G/stage2: stage2
	mkdir -p $G
	cp stage2 $(MPOINT)/boot/grub

U=kernel/Util
$F: createFontFile.hs $U/FixedFont.hs $U/FontEncode.hs
	LANG=C runhugs -h1000000 -Pkernel: createFontFile.hs >$@

$P:
	mkdir -p $(MPOINT)
	cd $(MPOINT) && wget http://pciids.sourceforge.net/pci.ids.gz

stage2:
	wget http://www.cse.ogi.edu/~hallgren/House/stage2

hOp.iso: hOp.flp
	rm -rf iso
	mkdir iso
	ln hOp.flp iso/
	mkisofs -r -b hOp.flp -c boot.catalog -o $@ iso

clean:
	rm -rf $(MPOINT) iso
	$(MAKE) -C kernel clean

distclean:
	$(MAKE) -C support clean
	rm -rf ghc-6.8.2 ghc-6.2-src.tar.bz2 ghc-stamp glafp-utils-stamp \
		configure-stamp patch-stamp

.phony:
	# dummy target to force rebuilding

