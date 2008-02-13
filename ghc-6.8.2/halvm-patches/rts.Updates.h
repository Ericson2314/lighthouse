*** ghc-pristine/rts/Updates.h	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/rts/Updates.h	2007-11-05 18:03:03.000000000 -0800
***************
*** 237,243 ****
  
  #endif /* CMINUSMINUS */
  
! #if !defined(DEBUG) || defined(THREADED_RTS)
  #define DEBUG_FILL_SLOP(p) /* do nothing */
  #else
  #define DEBUG_FILL_SLOP(p) FILL_SLOP(p)
--- 237,243 ----
  
  #endif /* CMINUSMINUS */
  
! #if !defined(DEBUG) || defined(THREADED_RTS) || !defined(PERFORM_SANITY_CHECKS)
  #define DEBUG_FILL_SLOP(p) /* do nothing */
  #else
  #define DEBUG_FILL_SLOP(p) FILL_SLOP(p)
