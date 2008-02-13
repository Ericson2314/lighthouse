*** ghc-pristine/rts/RtsFlags.c	2007-12-10 10:11:32.000000000 -0800
--- ghc-xen/rts/RtsFlags.c	2008-01-02 14:27:14.000000000 -0800
***************
*** 151,156 ****
--- 151,157 ----
      RtsFlags.GcFlags.frontpanel         = rtsFalse;
  #endif
      RtsFlags.GcFlags.idleGCDelayTime    = 300; /* millisecs */
+     RtsFlags.GcFlags.nonDebugSanityChecks = rtsFalse;
  
  #if osf3_HOST_OS
  /* ToDo: Perhaps by adjusting this value we can make linking without
***************
*** 688,693 ****
--- 689,703 ----
                        printRtsInfo();
                        exit(0);
                    }
+                   else if (strequal("enable-sanity-checking=yes",
+                                &rts_argv[arg][2])) {
+                       RtsFlags.GcFlags.nonDebugSanityChecks = rtsTrue;
+                   }
+                   else if (strequal("enable-sanity-checking=no",
+                                &rts_argv[arg][2])) {
+                       printf("Turning off sanity checking.\n");
+                       RtsFlags.GcFlags.nonDebugSanityChecks = rtsFalse;
+                   }
                    else {
  		      errorBelch("unknown RTS option: %s",rts_argv[arg]);
  		      error = rtsTrue;
