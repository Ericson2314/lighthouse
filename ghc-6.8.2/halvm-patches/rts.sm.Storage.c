*** ghc-pristine/rts/sm/Storage.c	2007-12-10 10:11:32.000000000 -0800
--- ghc-xen/rts/sm/Storage.c	2008-01-02 14:27:14.000000000 -0800
***************
*** 1088,1094 ****
     allegedly floating around in the system.
     -------------------------------------------------------------------------- */
  
! #ifdef DEBUG
  
  nat
  countBlocks(bdescr *bd)
--- 1088,1094 ----
     allegedly floating around in the system.
     -------------------------------------------------------------------------- */
  
! #if defined(DEBUG) || defined(PERFORM_SANITY_CHECKS)
  
  nat
  countBlocks(bdescr *bd)
