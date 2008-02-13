*** ghc-pristine/libraries/base/GHC/TopHandler.lhs	2007-01-05 10:42:22.000000000 -0800
--- ghc-xen/libraries/base/GHC/TopHandler.lhs	2007-01-10 14:42:29.000000000 -0800
***************
*** 112,121 ****
--- 112,126 ----
  -- try to flush stdout/stderr, but don't worry if we fail
  -- (these handles might have errors, and we don't want to go into
  -- an infinite loop).
+ #ifdef xen_HOST_OS
+ cleanUp :: IO ()
+ cleanUp = return ()
+ #else
  cleanUp :: IO ()
  cleanUp = do
    hFlush stdout `catchException` \_ -> return ()
    hFlush stderr `catchException` \_ -> return ()
+ #endif
  
  cleanUpAndExit :: Int -> IO a
  cleanUpAndExit r = do cleanUp; safeExit r
