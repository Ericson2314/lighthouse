*** ghc-pristine/rts/Capability.c	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/rts/Capability.c	2007-11-05 17:26:39.000000000 -0800
***************
*** 576,581 ****
--- 576,583 ----
   * Capability.
   * ------------------------------------------------------------------------- */
  
+ #if 0
+ 
  static void
  prodCapabilities(rtsBool all)
  {
***************
*** 623,628 ****
--- 625,632 ----
      prodCapabilities(rtsFalse);
  }
  
+ #endif /* if 0 for dead code check */
+ 
  /* ----------------------------------------------------------------------------
   * shutdownCapability
   *
