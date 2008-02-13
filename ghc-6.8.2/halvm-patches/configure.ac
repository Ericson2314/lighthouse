*** ghc-pristine/configure.ac	2007-12-10 10:11:32.000000000 -0800
--- ghc-xen/configure.ac	2008-01-02 14:20:23.000000000 -0800
***************
*** 233,238 ****
--- 233,247 ----
          HostVendor_CPP='unknown'
          HostOS_CPP='linux'
          ;;
+ i[[3456]]86-*-xen)
+ 	HostPlatform=i386-unknown-xen # vaguely true
+ 	TargetPlatform=i386-unknown-xen
+ 	BuildPlatform=i386-unknown-linux
+ 	HostPlatform_CPP='i386_unknown_linux'
+ 	HostArch_CPP='i386'
+ 	HostVendor_CPP='unknown'
+ 	HostOS_CPP='xen'
+ 	;;
  i[[3456]]86-*-kfreebsd*-gnu)
          HostPlatform=i386-unknown-kfreebsdgnu # hack again
          TargetPlatform=i386-unknown-kfreebsdgnu
***************
*** 835,842 ****
  
  if test x"$TargetPlatform" = x"i386-unknown-mingw32"; then
     GhcLibsWithUnix=NO
! else
!    GhcLibsWithUnix=YES
  fi
  AC_SUBST([GhcLibsWithUnix])
  
--- 844,855 ----
  
  if test x"$TargetPlatform" = x"i386-unknown-mingw32"; then
     GhcLibsWithUnix=NO
! else 
!   if test x"$TargetPlatform" = x"i386-unknown-xen"; then
!      GhcLibsWithUnix=NO
!   else
!      GhcLibsWithUnix=YES
!   fi
  fi
  AC_SUBST([GhcLibsWithUnix])
  
***************
*** 987,996 ****
--- 1000,1019 ----
  AC_SYS_LARGEFILE
  
  dnl ** check for specific header (.h) files that we are interested in
+ if test x"$TargetPlatform" = x"i386-unknown-xen"; then
+ AC_CHECK_HEADERS([bfd.h ctype.h dirent.h dlfcn.h errno.h fcntl.h grp.h limits.h locale.h nlist.h pwd.h sys/mman.h sys/resource.h sys/time.h sys/timeb.h sys/timers.h sys/times.h sys/utsname.h sys/wait.h time.h utime.h windows.h winsock.h])
+ else
  AC_CHECK_HEADERS([bfd.h ctype.h dirent.h dlfcn.h errno.h fcntl.h grp.h limits.h locale.h nlist.h pthread.h pwd.h signal.h sys/mman.h sys/resource.h sys/time.h sys/timeb.h sys/timers.h sys/times.h sys/utsname.h sys/wait.h termios.h time.h utime.h windows.h winsock.h])
+ fi
+ 
  
+ if test x"$TargetPlatform" = x"i386-unknown-xen"; then
+ HaveReadlineReadlineH=NO
+ HaveReadlineHistoyH=NO
+ else
  AC_CHECK_HEADER([readline/readline.h], [HaveReadlineReadlineH=YES], [HaveReadlineReadlineH=NO])
  AC_CHECK_HEADER([readline/history.h], [HaveReadlineHistoryH=YES], [HaveReadlineHistoryH=NO])
+ fi
  
  if test $HaveReadlineReadlineH = YES && test $HaveReadlineHistoryH = YES ; then
    GhcLibsWithReadline=YES
***************
*** 1089,1094 ****
--- 1112,1121 ----
  AC_CHECK_FUNCS([getpagesize])
  
  dnl ** check whether this machine has gmp3 installed
+ if test "$HostOS_CPP" = "xen"; then
+ HaveLibGmp=NO
+ LibGmp=not-installed
+ else
  AC_CHECK_LIB(gmp,  __gmpz_fdiv_qr, HaveLibGmp=YES; LibGmp=gmp,
    AC_CHECK_LIB(gmp3, __gmpz_fdiv_qr,  HaveLibGmp=YES; LibGmp=gmp3,
      HaveLibGmp=NO; LibGmp=not-installed))
***************
*** 1114,1119 ****
--- 1141,1147 ----
      ;;
  esac
  AC_SUBST(HaveFrameworkGMP)
+ fi
  
  dnl ** check for mingwex library
  AC_CHECK_LIB(mingwex, closedir, HaveLibMingwEx=YES, HaveLibMingwEx=NO)
***************
*** 1147,1158 ****
--- 1175,1190 ----
  
  dnl ** check whether we need -ldl to get dlopen()
  
+ if test "$HostOS_CPP" = "xen"; then
+ HaveLibDL=NO
+ else
  AC_CHECK_LIB(dl, dlopen,
      [HaveLibDL=YES
       AC_DEFINE([HAVE_LIBDL], [1], [Define to 1 if you need -ldl to get dlopen().])
       LIBS="$LIBS -ldl"],
      [HaveLibDL=NO])
  AC_SUBST(HaveLibDL)
+ fi
  
  dnl --------------------------------------------------
  dnl * Miscellaneous feature tests
***************
*** 1204,1212 ****
--- 1236,1246 ----
      [AC_MSG_RESULT(no)])
  
  dnl ** check for librt
+ if test x"$TargetPlatform" = x"i386-unknown-xen"; then
  AC_CHECK_LIB(rt, clock_gettime)
  AC_CHECK_FUNCS(clock_gettime timer_create timer_settime)
  FP_CHECK_TIMER_CREATE
+ fi
  
  dnl ** check for Apple's "interesting" long double compatibility scheme
  AC_MSG_CHECKING(for printf\$LDBLStub)
