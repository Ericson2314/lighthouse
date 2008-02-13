# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

HPCDIR=$(TOP)/../utils/hpc
HPC   = $(HPCDIR)/hpc-std

build-tix-tar::
	rm -Rf .hpc hpc001.o a.out
	$(TOP)/../compiler/ghc-inplace -fhpc hpc001.hs
	./a.out
	mv a.out.tix hpc_sample.tix
	tar cf tixs.tar hpc_sample.tix .hpc

#
# This is measuring coverage of the hpc tool itself.
#  In the hpc directory 
#   % cd $ROOT/utils/hpc
# It assumes you have make a copy of hpc that
#  is not compiled with -fhpc, and called it hpc-std
#   % make clean ; make boot ; make ; mv hpc hpc-std
# It assumes that the utils/hpc has been compiled with 
#   % make clean ; make boot ; make UseStage1=YES EXTRA_HC_OPTS=-fhpc
#
HPCTOOLS_BORING = HpcParser HpcOverlay HpcLexer HpcDraft HpcCombine
BORING_FLAGS    = $(foreach BORING,$(HPCTOOLS_BORING),--exclude=$(BORING))
COVERAGE_OUT   = tool-coverage.info

HPC_ALL_TIX = hpc-all.tix

hpc-tool-coverage-clean::
	find . -name 'hpc.tix' -print | xargs rm -f

hpc-tool-coverage::
	$(HPC) combine $(shell find . -name 'hpc.tix' -print) --output=$(HPC_ALL_TIX)
	date 								>> $(COVERAGE_OUT)
	$(HPC) report $(HPC_ALL_TIX) --srcdir=$(HPCDIR) --xml-output    >> $(COVERAGE_OUT)
	echo OMIT: $(HPCTOOLS_BORING)					>> $(COVERAGE_OUT)
	$(HPC) report $(HPC_ALL_TIX) --srcdir=$(HPCDIR) --xml-output $(BORING_FLAGS) \
									>> $(COVERAGE_OUT)
	$(HPC) markup $(HPC_ALL_TIX) --srcdir=$(HPCDIR) $(BORING_FLAGS) --destdir=html 
