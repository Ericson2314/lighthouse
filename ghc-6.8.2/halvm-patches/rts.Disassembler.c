*** ghc-pristine/rts/Disassembler.c	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/rts/Disassembler.c	2007-11-05 17:27:04.000000000 -0800
***************
*** 9,14 ****
--- 9,15 ----
   * ---------------------------------------------------------------------------*/
  
  #ifdef DEBUG
+ #ifdef ALLOW_INTERPRETER
  
  #include "PosixSource.h"
  #include "Rts.h"
***************
*** 286,289 ****
--- 287,291 ----
     ASSERT(pc == nbcs+1);
  }
  
+ #endif // ALLOW_INTERPRETER
  #endif /* DEBUG */
