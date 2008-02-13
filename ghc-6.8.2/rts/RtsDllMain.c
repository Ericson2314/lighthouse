/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1999-2000
 *
 * Entry point for RTS-in-a-DLL
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

/* I'd be mildly surprised if this wasn't defined, but still. */
#if defined(__PIC__) && defined(mingw32_TARGET_OS)
BOOL
WINAPI
DllMain ( HINSTANCE hInstance
        , DWORD reason
	, LPVOID reserved
	)
{
  /*
   * Note: the DllMain() doesn't call startupHaskell() for you,
   *       that is the task of users of the RTS. The reason is
   *       that *you* want to be able to control the arguments
   *       you pass to the RTS.
   */
  switch (reason) {
  case DLL_PROCESS_DETACH: shutdownHaskell();
  }
  return TRUE;
}

#endif /* defined(__PIC__) && defined(mingw32_TARGET_OS) */
