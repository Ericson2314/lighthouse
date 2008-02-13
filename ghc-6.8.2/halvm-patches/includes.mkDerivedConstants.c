*** ghc-pristine/includes/mkDerivedConstants.c	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/includes/mkDerivedConstants.c	2007-11-05 16:53:45.000000000 -0800
***************
*** 18,24 ****
   * doesn't affect the offsets of anything else.
   */
  #define PROFILING
! #define THREADED_RTS
  
  #include "Rts.h"
  #include "RtsFlags.h"
--- 18,24 ----
   * doesn't affect the offsets of anything else.
   */
  #define PROFILING
! //#define THREADED_RTS FIXME!
  
  #include "Rts.h"
  #include "RtsFlags.h"
***************
*** 228,234 ****
      def_offset("stgGCFun", FUN_OFFSET(stgGCFun));
  
      field_offset(Capability, r);
!     field_offset(Capability, lock);
      struct_field(Capability, mut_lists);
  
      struct_field(bdescr, start);
--- 228,234 ----
      def_offset("stgGCFun", FUN_OFFSET(stgGCFun));
  
      field_offset(Capability, r);
!     //    field_offset(Capability, lock); FIXME!!
      struct_field(Capability, mut_lists);
  
      struct_field(bdescr, start);
***************
*** 364,369 ****
--- 364,370 ----
      closure_field(StgMVar,tail);
      closure_field(StgMVar,value);
  
+ #ifdef ALLOW_INTERPRETER
      closure_size(StgBCO);
      closure_field(StgBCO, instrs);
      closure_field(StgBCO, literals);
***************
*** 371,376 ****
--- 372,378 ----
      closure_field(StgBCO, arity);
      closure_field(StgBCO, size);
      closure_payload(StgBCO, bitmap);
+ #endif // ALLOW_INTERPRETER
  
      closure_size(StgStableName);
      closure_field(StgStableName,sn);
