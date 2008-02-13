*** ghc-pristine/includes/RtsFlags.h	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/includes/RtsFlags.h	2007-11-05 16:49:53.000000000 -0800
***************
*** 45,50 ****
--- 45,52 ----
      int idleGCDelayTime;	/* in milliseconds */
  
      StgWord heapBase;           /* address to ask the OS for memory */
+ 
+     rtsBool nonDebugSanityChecks;
  };
  
  struct DEBUG_FLAGS {  
