*** ghc-pristine/rts/Timer.c	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/rts/Timer.c	2007-11-05 18:02:42.000000000 -0800
***************
*** 25,31 ****
  #include "RtsSignals.h"
  
  /* ticks left before next pre-emptive context switch */
! static int ticks_to_ctxt_switch = 0;
  
  #if defined(THREADED_RTS)
  /* idle ticks left before we perform a GC */
--- 25,31 ----
  #include "RtsSignals.h"
  
  /* ticks left before next pre-emptive context switch */
!        int ticks_to_ctxt_switch = 0;
  
  #if defined(THREADED_RTS)
  /* idle ticks left before we perform a GC */
