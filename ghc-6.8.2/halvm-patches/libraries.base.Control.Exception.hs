*** ghc-pristine/libraries/base/Control/Exception.hs	2007-11-01 19:58:36.000000000 -0700
--- ghc-xen/libraries/base/Control/Exception.hs	2007-11-05 17:01:54.000000000 -0800
***************
*** 131,138 ****
--- 131,140 ----
  import GHC.Conc		( throwTo, ThreadId )
  import Data.IORef	( IORef, newIORef, readIORef, writeIORef )
  import Foreign.C.String ( CString, withCString )
+ #ifndef xen_HOST_OS
  import System.IO	( stdout, hFlush )
  #endif
+ #endif
  
  #ifdef __HUGS__
  import Hugs.Exception	as ExceptionBase
***************
*** 572,578 ****
--- 574,582 ----
     where
        defaultHandler :: Exception -> IO ()
        defaultHandler ex = do
+ #ifndef xen_HOST_OS
           (hFlush stdout) `catchException` (\ _ -> return ())
+ #endif
           let msg = case ex of
                 Deadlock    -> "no threads to run:  infinite loop or deadlock?"
                 ErrorCall s -> s
