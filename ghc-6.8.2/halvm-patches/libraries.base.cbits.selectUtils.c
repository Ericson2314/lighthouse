*** ghc-pristine/libraries/base/cbits/selectUtils.c	2007-01-05 10:42:21.000000000 -0800
--- ghc-xen/libraries/base/cbits/selectUtils.c	2007-01-10 15:50:13.000000000 -0800
***************
*** 1,3 ****
--- 1,5 ----
  
  #include "HsBase.h"
+ #ifndef xen_HOST_OS
  void hsFD_ZERO(fd_set *fds) { FD_ZERO(fds); }
+ #endif
