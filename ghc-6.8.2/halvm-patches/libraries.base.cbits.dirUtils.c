*** ghc-pristine/libraries/base/cbits/dirUtils.c	2007-11-01 19:58:36.000000000 -0700
--- ghc-xen/libraries/base/cbits/dirUtils.c	2007-11-05 17:13:48.000000000 -0800
***************
*** 7,12 ****
--- 7,14 ----
  /* needed only for solaris2_HOST_OS */
  #include "ghcconfig.h"
  
+ #ifndef xen_HOST_OS
+ 
  // The following is required on Solaris to force the POSIX versions of
  // the various _r functions instead of the Solaris versions.
  #ifdef solaris2_HOST_OS
***************
*** 171,173 ****
--- 173,176 ----
  #endif
  }
  
+ #endif
