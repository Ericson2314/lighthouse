*** ghc-pristine/rts/RtsStartup.c	2007-12-10 10:11:32.000000000 -0800
--- ghc-xen/rts/RtsStartup.c	2008-01-02 14:32:40.000000000 -0800
***************
*** 32,38 ****
--- 32,40 ----
  #include "Trace.h"
  #include "RtsTypeable.h"
  #include "Stable.h"
+ #ifndef xen_HOST_OS
  #include "Hpc.h"
+ #endif
  #include "FileLock.h"
  
  #if defined(RTS_GTK_FRONTPANEL)
***************
*** 354,362 ****
      StgRun((StgFunPtr)stg_init, &cap->r);
  
      freeGroup_lock(bd);
! 
      startupHpc();
! 
      // This must be done after module initialisation.
      // ToDo: make this work in the presence of multiple hs_add_root()s.
      initProfiling2();
--- 356,364 ----
      StgRun((StgFunPtr)stg_init, &cap->r);
  
      freeGroup_lock(bd);
! #ifndef xen_HOST_OS
      startupHpc();
! #endif
      // This must be done after module initialisation.
      // ToDo: make this work in the presence of multiple hs_add_root()s.
      initProfiling2();
***************
*** 454,462 ****
  
      /* stop timing the shutdown, we're about to print stats */
      stat_endExit();
!     
      /* shutdown the hpc support (if needed) */
      exitHpc();
  
      // clean up things from the storage manager's point of view.
      // also outputs the stats (+RTS -s) info.
--- 456,466 ----
  
      /* stop timing the shutdown, we're about to print stats */
      stat_endExit();
!    
! #ifndef xen_HOST_OS
      /* shutdown the hpc support (if needed) */
      exitHpc();
+ #endif
  
      // clean up things from the storage manager's point of view.
      // also outputs the stats (+RTS -s) info.
