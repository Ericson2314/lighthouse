*** ghc-pristine/rts/RtsSignals.h	2007-03-05 10:34:22.000000000 -0800
--- ghc-xen/rts/RtsSignals.h	2007-03-08 11:37:19.000000000 -0800
***************
*** 9,15 ****
  #ifndef RTS_SIGNALS_H
  #define RTS_SIGNALS_H
  
! #if !defined(PAR) && !defined(mingw32_HOST_OS)
  
  #include "posix/Signals.h"
  
--- 9,15 ----
  #ifndef RTS_SIGNALS_H
  #define RTS_SIGNALS_H
  
! #if !defined(PAR) && !defined(mingw32_HOST_OS) && !defined(xen_HOST_OS)
  
  #include "posix/Signals.h"
  
***************
*** 17,22 ****
--- 17,27 ----
  
  #include "win32/ConsoleHandler.h"
  
+ #elif defined(xen_HOST_OS)
+ 
+ extern int signals_pending();
+ extern void startSignalHandlers(Capability *cap);
+ 
  #else /* PAR */
  
  #define signals_pending() (rtsFalse)
