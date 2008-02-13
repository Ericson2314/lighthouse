*** ghc-pristine/rts/sm/GC.c	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/rts/sm/GC.c	2007-11-05 17:32:14.000000000 -0800
***************
*** 495,501 ****
--- 495,507 ----
  
      // scavenge static objects 
      if (major_gc && static_objects != END_OF_STATIC_LIST) {
+ #ifdef PERFORM_SANITY_CHECKS
+       if (RtsFlags.GcFlags.nonDebugSanityChecks) {
+         checkStaticObjects(static_objects);
+       }
+ #else
  	IF_DEBUG(sanity, checkStaticObjects(static_objects));
+ #endif
  	scavenge_static();
      }
  
***************
*** 615,621 ****
--- 621,633 ----
        compact();
    }
  
+ #ifdef PERFORM_SANITY_CHECKS
+   if (RtsFlags.GcFlags.nonDebugSanityChecks) {
+     checkGlobalTSOList(rtsFalse);
+   }
+ #else
    IF_DEBUG(sanity, checkGlobalTSOList(rtsFalse));
+ #endif
  
    /* run through all the generations/steps and tidy up 
     */
***************
*** 986,992 ****
--- 998,1010 ----
    updateStablePtrTable(major_gc);
  
    // check sanity after GC 
+ #ifdef PERFORM_SANITY_CHECKS
+   if (RtsFlags.GcFlags.nonDebugSanityChecks) {
+     checkGlobalTSOList(rtsFalse);
+   }
+ #else
    IF_DEBUG(sanity, checkSanity());
+ #endif
  
    // extra GC trace info 
    IF_DEBUG(gc, statDescribeGens());
