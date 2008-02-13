#ifndef __STGDLL_H__
#define __STGDLL_H__ 1

#if defined(__PIC__) && defined(mingw32_TARGET_OS)
#  define DLL_IMPORT_DATA_REF(x) (_imp__##x)
#  define DLL_IMPORT_DATA_VARNAME(x) *_imp__##x
#  if __GNUC__ && !defined(__declspec)
#    define DLLIMPORT
#  else
#    define DLLIMPORT __declspec(dllimport)
#    define DLLIMPORT_DATA(x) _imp__##x
#  endif
#else
#  define DLL_IMPORT_DATA_REF(x) (&(x))
#  define DLL_IMPORT_DATA_VARNAME(x) x
#  define DLLIMPORT
#endif

/* The view of the ghc/includes/ header files differ ever so
   slightly depending on whether the RTS is being compiled
   or not - so we're forced to distinguish between two.
   [oh, you want details :) : Data symbols defined by the RTS
    have to be accessed through an extra level of indirection
    when compiling generated .hc code compared to when the RTS
    sources are being processed. This is only the case when 
    using Win32 DLLs. ]
*/
#ifdef COMPILING_RTS
#define DLL_IMPORT DLLIMPORT
#define DLL_IMPORT_RTS
#define DLL_IMPORT_DATA_VAR(x) x
#else
#define DLL_IMPORT
#define DLL_IMPORT_RTS DLLIMPORT
# if defined(__PIC__) && defined(mingw32_TARGET_OS)
#  define DLL_IMPORT_DATA_VAR(x) _imp__##x
# else
#  define DLL_IMPORT_DATA_VAR(x) x
# endif
#endif

#ifdef COMPILING_STDLIB
#define DLL_IMPORT_STDLIB
#else
#define DLL_IMPORT_STDLIB DLLIMPORT
#endif

#endif /* __STGDLL_H__ */
