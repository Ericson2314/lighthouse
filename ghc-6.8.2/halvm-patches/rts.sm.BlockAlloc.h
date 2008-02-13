*** ghc-pristine/rts/sm/BlockAlloc.h	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/rts/sm/BlockAlloc.h	2007-11-05 17:26:14.000000000 -0800
***************
*** 11,17 ****
  
  /* Debugging  -------------------------------------------------------------- */
  
! #ifdef DEBUG
  extern void checkFreeListSanity(void);
  nat         countFreeList(void);
  #endif
--- 11,17 ----
  
  /* Debugging  -------------------------------------------------------------- */
  
! #if defined(DEBUG) || defined(PERFORM_SANITY_CHECKS)
  extern void checkFreeListSanity(void);
  nat         countFreeList(void);
  #endif
