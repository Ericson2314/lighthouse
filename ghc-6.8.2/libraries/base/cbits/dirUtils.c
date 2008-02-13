/* 
 * (c) The University of Glasgow 2002
 *
 * Directory Runtime Support
 */

/* needed only for solaris2_HOST_OS */
#include "ghcconfig.h"

#ifndef house_HOST_OS

// The following is required on Solaris to force the POSIX versions of
// the various _r functions instead of the Solaris versions.
#ifdef solaris2_HOST_OS
#define _POSIX_PTHREAD_SEMANTICS
#endif

#include "HsBase.h"

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
#include <windows.h>

static
int
toErrno(DWORD rc)
{
    switch (rc) {
    case ERROR_FILE_NOT_FOUND:    return ENOENT;
    case ERROR_PATH_NOT_FOUND:    return ENOENT;
    case ERROR_TOO_MANY_OPEN_FILES: return EMFILE;
    case ERROR_ACCESS_DENIED:     return EACCES;
    case ERROR_INVALID_HANDLE:    return EBADF; /* kinda sorta */
    case ERROR_NOT_ENOUGH_MEMORY: return ENOMEM;
    case ERROR_INVALID_ACCESS:    return EINVAL;
    case ERROR_INVALID_DATA:      return EINVAL;
    case ERROR_OUTOFMEMORY:       return ENOMEM;
    case ERROR_SHARING_VIOLATION: return EACCES;
    case ERROR_LOCK_VIOLATION:    return EACCES;
    case ERROR_ALREADY_EXISTS:    return EEXIST;
    case ERROR_BUSY:              return EBUSY;
    case ERROR_BROKEN_PIPE:       return EPIPE;
    case ERROR_PIPE_CONNECTED:    return EBUSY;
    case ERROR_PIPE_LISTENING:    return EBUSY;
    case ERROR_NOT_CONNECTED:     return EINVAL;

    case ERROR_NOT_OWNER:         return EPERM;
    case ERROR_DIRECTORY:         return ENOTDIR;
    case ERROR_FILE_INVALID:      return EACCES;
    case ERROR_FILE_EXISTS:       return EEXIST;

    default:
	return rc;
    }
}
#endif


/*
 * read an entry from the directory stream; opt for the
 * re-entrant friendly way of doing this, if available.
 */
int
__hscore_readdir( DIR *dirPtr, struct dirent **pDirEnt )
{
#if HAVE_READDIR_R
  struct dirent* p;
  int res;
  static unsigned int nm_max = (unsigned int)-1;
  
  if (pDirEnt == NULL) {
    return -1;
  }
  if (nm_max == (unsigned int)-1) {
#ifdef NAME_MAX
    nm_max = NAME_MAX + 1;
#else
    nm_max = pathconf(".", _PC_NAME_MAX);
    if (nm_max == -1) { nm_max = 255; }
    nm_max++;
#endif
  }
  p = (struct dirent*)malloc(sizeof(struct dirent) + nm_max);
  if (p == NULL) return -1;
  res = readdir_r(dirPtr, p, pDirEnt);
  if (res != 0) {
      *pDirEnt = NULL;
      free(p);
  }
  else if (*pDirEnt == NULL) {
    // end of stream
    free(p);
  }
  return res;
#else

  if (pDirEnt == NULL) {
    return -1;
  }

  *pDirEnt = readdir(dirPtr);
  if (*pDirEnt == NULL) {
    return -1;
  } else {
    return 0;
  }  
#endif
}

/*
 * Function: __hscore_renameFile()
 *
 * Provide Haskell98's semantics for renaming files and directories.
 * It mirrors that of POSIX.1's behaviour for rename() by overwriting
 * the target if it exists (the MS CRT implementation of rename() returns
 * an error
 *
 */
int
__hscore_renameFile( char *src, char *dest)
{
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
    static int forNT = -1;
    
    /* ToDo: propagate error codes back */
    if (MoveFileA(src, dest)) {
	return 0;
    } else {
	;
    }
    
    /* Failed...it could be because the target already existed. */
    if ( !GetFileAttributes(dest) ) {
	/* No, it's not there - just fail. */
	errno = toErrno(GetLastError());
	return (-1);
    }

    if (forNT == -1) {
	OSVERSIONINFO ovi;
	ovi.dwOSVersionInfoSize = sizeof(ovi);
	if ( !GetVersionEx(&ovi) ) {
	    errno = toErrno(GetLastError()); 
	    return (-1);
	}
	forNT = ((ovi.dwPlatformId & VER_PLATFORM_WIN32_NT) != 0);
    }
    
    if (forNT) {
	/* Easy, go for MoveFileEx() */
	if ( MoveFileExA(src, dest, MOVEFILE_REPLACE_EXISTING) ) {
	    return 0;
	} else {
	    errno = toErrno(GetLastError()); 
	    return (-1);
	}
    }

    /* No MoveFileEx() for Win9x, try deleting the target. */
    /* Similarly, if the MoveFile*() ops didn't work out under NT */
    if (DeleteFileA(dest)) {
	if (MoveFileA(src,dest)) {
	    return 0;
	} else {
	    errno = toErrno(GetLastError());
	    return (-1);
	}
    } else {
	errno = toErrno(GetLastError());
	return (-1);
    }
#else
    return rename(src,dest);
#endif
}

#endif
