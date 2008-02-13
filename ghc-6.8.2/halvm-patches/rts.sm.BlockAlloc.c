*** ghc-pristine/rts/sm/BlockAlloc.c	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/rts/sm/BlockAlloc.c	2007-11-05 17:25:26.000000000 -0800
***************
*** 570,576 ****
     Debugging
     -------------------------------------------------------------------------- */
  
! #ifdef DEBUG
  static void
  check_tail (bdescr *bd)
  {
--- 570,576 ----
     Debugging
     -------------------------------------------------------------------------- */
  
! #if defined(DEBUG) || defined(PERFORM_SANITY_CHECKS)
  static void
  check_tail (bdescr *bd)
  {
