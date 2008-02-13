*** ghc-pristine/libraries/base/System/IO.hs	2007-11-01 19:58:37.000000000 -0700
--- ghc-xen/libraries/base/System/IO.hs	2007-11-14 17:46:18.000000000 -0800
***************
*** 19,24 ****
--- 19,25 ----
      IO,			       -- instance MonadFix
      fixIO,		       -- :: (a -> IO a) -> IO a
  
+ #ifndef xen_HOST_OS
      -- * Files and handles
  
      FilePath,		       -- :: String
***************
*** 161,166 ****
--- 162,169 ----
      openTempFile,
      openBinaryTempFile,
  #endif
+ 
+ #endif
    ) where
  
  #ifndef __NHC__
***************
*** 169,176 ****
--- 172,181 ----
  import Data.Maybe
  import Foreign.C.Error
  import Foreign.C.String
+ #ifndef xen_HOST_OS
  import System.Posix.Internals
  #endif
+ #endif
  
  #ifdef __GLASGOW_HASKELL__
  import GHC.Exception    as ExceptionBase hiding (catch)
***************
*** 182,189 ****
--- 187,196 ----
  #ifdef __GLASGOW_HASKELL__
  import GHC.Base
  import GHC.IOBase	-- Together these four Prelude modules define
+ #ifndef xen_HOST_OS
  import GHC.Handle	-- all the stuff exported by IO for the GHC version
  import GHC.IO
+ #endif
  import GHC.Exception
  import GHC.Num
  import GHC.Read
***************
*** 240,245 ****
--- 247,253 ----
  import NHC.FFI (Ptr)
  #endif
  
+ #ifndef xen_HOST_OS
  -- -----------------------------------------------------------------------------
  -- Standard IO
  
***************
*** 395,401 ****
  
  -- ---------------------------------------------------------------------------
  -- fixIO
! 
  #if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
  fixIO :: (a -> IO a) -> IO a
  fixIO k = do
--- 403,409 ----
  
  -- ---------------------------------------------------------------------------
  -- fixIO
! #endif 
  #if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
  fixIO :: (a -> IO a) -> IO a
  fixIO k = do
***************
*** 409,415 ****
  -- blackholing isn't enough.  In an infinite loop, GHC may run the IO
  -- computation a few times before it notices the loop, which is wrong.
  #endif
! 
  #if defined(__NHC__)
  -- Assume a unix platform, where text and binary I/O are identical.
  openBinaryFile = openFile
--- 417,423 ----
  -- blackholing isn't enough.  In an infinite loop, GHC may run the IO
  -- computation a few times before it notices the loop, which is wrong.
  #endif
! #ifndef xen_HOST_OS
  #if defined(__NHC__)
  -- Assume a unix platform, where text and binary I/O are identical.
  openBinaryFile = openFile
***************
*** 560,562 ****
--- 568,571 ----
      return r
   )
  #endif
+ #endif
