*** ghc-pristine/rts/Makefile	2007-11-01 19:55:14.000000000 -0700
--- ghc-xen/rts/Makefile	2007-11-12 14:52:41.000000000 -0800
***************
*** 63,70 ****
--- 63,74 ----
  ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
  ALL_DIRS += win32
  else
+ ifeq "$(HOSTPLATFORM)" "i386-unknown-xen"
+ ALL_DIRS += xen xen/libc xen/libc/math/i387 xen/libc/math/c xen/libc/math
+ else
  ALL_DIRS += posix
  endif
+ endif
  
  ifneq "$(findstring dyn, $(way))" ""
  DYNAMIC_RTS=YES
***************
*** 82,87 ****
--- 86,95 ----
  EXCLUDED_SRCS += RtsDllMain.c
  endif
  
+ ifeq "$(HOSTPLATFORM)" "i386-unknown-xen"
+ EXCLUDED_SRCS += Linker.c Hpc.c
+ endif
+ 
  # This file ends up being empty unless we're building for a powerpc
  # or darwin system, and it is reported that Solaris ld chokes on it when
  # building HSrts.o.
***************
*** 143,148 ****
--- 151,160 ----
  SRC_CC_OPTS += $(GhcRtsCcOpts)
  SRC_HC_OPTS += $(GhcRtsHcOpts) -package-name rts
  
+ ifeq "$(HOSTPLATFORM)" "i386-unknown-xen"
+ SRC_CC_OPTS += -Ixen/include -Ixen/include/sys
+ endif
+ 
  ifneq "$(GhcWithSMP)" "YES"
  SRC_CC_OPTS += -DNOSMP
  SRC_HC_OPTS += -optc-DNOSMP
***************
*** 273,279 ****
--- 285,295 ----
  # a superset of the dependencies.  To do this properly, we should generate
  # a different set of dependencies for each way.  Further hack: PROFILING and
  # TICKY_TICKY can't be used together, so we omit TICKY_TICKY for now.
+ ifeq "$(HOSTPLATFORM)" "i386-unknown-xen"
+ SRC_MKDEPENDC_OPTS += -DPROFILING -DDEBUG
+ else
  SRC_MKDEPENDC_OPTS += -DPROFILING -DTHREADED_RTS -DDEBUG
+ endif
  
  # -----------------------------------------------------------------------------
  # The auto-generated apply code
