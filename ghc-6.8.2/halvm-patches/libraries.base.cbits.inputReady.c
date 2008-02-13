*** ghc-pristine/libraries/base/cbits/inputReady.c	2007-11-01 19:58:36.000000000 -0700
--- ghc-xen/libraries/base/cbits/inputReady.c	2007-11-05 17:14:28.000000000 -0800
***************
*** 8,13 ****
--- 8,14 ----
  /* #include "PosixSource.h" */
  #include "HsBase.h"
  
+ #ifndef xen_HOST_OS
  /*
   * inputReady(fd) checks to see whether input is available on the file
   * descriptor 'fd'.  Input meaning 'can I safely read at least a
***************
*** 95,97 ****
--- 96,100 ----
      }
  #endif
  }    
+ 
+ #endif
