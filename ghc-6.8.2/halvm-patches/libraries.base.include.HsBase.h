*** ghc-pristine/libraries/base/include/HsBase.h	2007-12-10 10:16:54.000000000 -0800
--- ghc-xen/libraries/base/include/HsBase.h	2008-01-02 14:27:13.000000000 -0800
***************
*** 234,240 ****
  #endif
  #endif
  
! #if !defined(_MSC_VER) && !defined(__MINGW32__) && !defined(_WIN32)
  INLINE int
  __hscore_sigemptyset( sigset_t *set )
  { return sigemptyset(set); }
--- 234,240 ----
  #endif
  #endif
  
! #if !defined(_MSC_VER) && !defined(__MINGW32__) && !defined(_WIN32) && !defined(xen_HOST_OS)
  INLINE int
  __hscore_sigemptyset( sigset_t *set )
  { return sigemptyset(set); }
***************
*** 398,403 ****
--- 398,404 ----
    return SEEK_END;
  }
  
+ #if !defined(xen_HOST_OS)
  INLINE int
  __hscore_ftruncate( int fd, off_t where )
  {
***************
*** 675,680 ****
--- 676,683 ----
  extern void hsFD_ZERO(fd_set *fds);
  #endif
  
+ #endif // xen_HOST_OS
+ 
  // gettimeofday()-related
  
  #if !defined(__MINGW32__)
