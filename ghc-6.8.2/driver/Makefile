# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.76 2005/03/02 09:49:11 simonmar Exp $
#
# (c) The University of Glasgow 2002
#

TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = mangler split ghc ghci

boot all :: package.conf.inplace package.conf

package.conf.inplace :
	echo "[]" > $@

package.conf :
	echo "[]" > $@

override datadir = $(libdir)
INSTALL_DATAS += package.conf ghc-usage.txt ghci-usage.txt

# Since cleaning effectively uninstalls all the packages, we must 
# remove the stamp files that the build system uses to avoid unnecessarily
# re-installing packages.
CLEAN_FILES += package.conf* stamp-pkg-conf*

binary-dist:
	$(INSTALL_DIR)                   $(BIN_DIST_DIR)/driver
	$(INSTALL_DATA) Makefile         $(BIN_DIST_DIR)/driver
	$(INSTALL_DATA) $(INSTALL_DATAS) $(BIN_DIST_DIR)/driver
	set -e; for d in $(SUBDIRS); do $(MAKE) -C $$d binary-dist; done

include $(TOP)/mk/target.mk
