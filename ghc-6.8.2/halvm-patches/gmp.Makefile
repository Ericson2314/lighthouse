*** ghc-pristine/gmp/Makefile	2007-11-01 19:55:13.000000000 -0700
--- ghc-xen/gmp/Makefile	2007-11-12 12:05:57.000000000 -0800
***************
*** 67,73 ****
  	    export PATH=`pwd`:$$PATH; \
  	    cd gmpbuild && \
  	    CC=$(WhatGccIsCalled) $(SHELL) configure \
! 	          --enable-shared=no --host=$(PLATFORM) --build=$(PLATFORM)
  	touch $@
  
  stamp.gmp.shared:
--- 67,73 ----
  	    export PATH=`pwd`:$$PATH; \
  	    cd gmpbuild && \
  	    CC=$(WhatGccIsCalled) $(SHELL) configure \
! 	          --enable-shared=no 
  	touch $@
  
  stamp.gmp.shared:
