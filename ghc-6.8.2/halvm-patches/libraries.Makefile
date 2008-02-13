*** ghc-pristine/libraries/Makefile	2007-12-10 10:11:32.000000000 -0800
--- ghc-xen/libraries/Makefile	2008-01-02 14:25:22.000000000 -0800
***************
*** 39,52 ****
  include $(TOP)/mk/boilerplate.mk
  
  SUBDIRS  = base array packedstring containers bytestring
! SUBDIRS += old-locale old-time filepath directory
  ifeq "$(GhcLibsWithUnix)" "YES"
  SUBDIRS += unix
  endif
  ifeq "$(Windows)" "YES"
  SUBDIRS += $(wildcard Win32)
  endif
! SUBDIRS += process pretty hpc template-haskell readline Cabal random haskell98
  
  # Set GhcBootLibs=YES from the command line to work with just the libraries
  # needed to bootstrap GHC.
--- 39,56 ----
  include $(TOP)/mk/boilerplate.mk
  
  SUBDIRS  = base array packedstring containers bytestring
! SUBDIRS += old-locale old-time
  ifeq "$(GhcLibsWithUnix)" "YES"
  SUBDIRS += unix
  endif
  ifeq "$(Windows)" "YES"
  SUBDIRS += $(wildcard Win32)
  endif
! SUBDIRS += pretty hpc template-haskell Cabal random haskell98
! 
! ifneq "$(HOSTPLATFORM)" "i386-unknown-xen"
! SUBDIRS += readline filepath directory process
! endif
  
  # Set GhcBootLibs=YES from the command line to work with just the libraries
  # needed to bootstrap GHC.
***************
*** 57,78 ****
--- 61,88 ----
  SUBDIRS += $(wildcard parsec)
  SUBDIRS += $(wildcard haskell-src)
  SUBDIRS += $(wildcard html)
+ ifneq "$(HOSTPLATFORM)" "i386-unknown-xen"
  SUBDIRS += $(wildcard network)
  SUBDIRS += $(wildcard QuickCheck)
  SUBDIRS += $(wildcard HUnit)
+ endif
  SUBDIRS += $(wildcard mtl)
  SUBDIRS += $(wildcard fgl)
  SUBDIRS += $(wildcard time)
+ ifneq "$(HOSTPLATFORM)" "i386-unknown-xen"
  SUBDIRS += $(wildcard OpenGL)
  SUBDIRS += $(wildcard GLUT)
  SUBDIRS += $(wildcard OpenAL)
  SUBDIRS += $(wildcard ALUT)
+ endif
  SUBDIRS += $(wildcard stm)
  SUBDIRS += $(wildcard xhtml)
+ ifneq "$(HOSTPLATFORM)" "i386-unknown-xen"
  SUBDIRS += $(wildcard cgi)
  ifeq "$(GhcLibsWithObjectIO)" "YES"
  SUBDIRS += $(wildcard ObjectIO)
  endif
+ endif
  SUBDIRS += $(wildcard parallel)
  SUBDIRS += $(wildcard ndp)
  endif
