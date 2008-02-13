TOP=../../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

build-tix-tar::
	rm -Rf .hpc *.o Main
	$(TOP)/../compiler/ghc-inplace -fhpc --make Main.hs
	./Main
	mv Main.tix hpc_sample.tix
	tar cf tixs.tar hpc_sample.tix .hpc

