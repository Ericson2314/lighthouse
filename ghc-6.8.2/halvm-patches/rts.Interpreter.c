*** ghc-pristine/rts/Interpreter.c	2007-12-10 10:11:32.000000000 -0800
--- ghc-xen/rts/Interpreter.c	2008-01-02 14:27:14.000000000 -0800
***************
*** 4,9 ****
--- 4,11 ----
   * Copyright (c) The GHC Team, 1994-2002.
   * ---------------------------------------------------------------------------*/
  
+ #ifdef ALLOW_INTERPRETER // whole file
+ 
  #include "PosixSource.h"
  #include "Rts.h"
  #include "RtsAPI.h"
***************
*** 1410,1412 ****
--- 1412,1416 ----
  
      barf("interpretBCO: fell off end of the interpreter");
  }
+ 
+ #endif // ALLOW_INTERPRETER
