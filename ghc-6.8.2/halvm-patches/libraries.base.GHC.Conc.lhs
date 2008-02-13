*** ghc-pristine/libraries/base/GHC/Conc.lhs	2007-12-10 10:16:54.000000000 -0800
--- ghc-xen/libraries/base/GHC/Conc.lhs	2008-01-02 17:46:22.000000000 -0800
***************
*** 41,48 ****
--- 41,50 ----
  	-- * Waiting
  	, threadDelay	  	-- :: Int -> IO ()
  	, registerDelay		-- :: Int -> IO (TVar Bool)
+ #ifndef xen_HOST_OS
  	, threadWaitRead	-- :: Int -> IO ()
  	, threadWaitWrite	-- :: Int -> IO ()
+ #endif
  
  	-- * MVars
  	, MVar		-- abstract
***************
*** 80,86 ****
  	, asyncWriteBA  -- :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int, Int)
  #endif
  
! #ifndef mingw32_HOST_OS
          , signalHandlerLock
  #endif
  
--- 82,88 ----
  	, asyncWriteBA  -- :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int, Int)
  #endif
  
! #if !defined(mingw32_HOST_OS) && !defined(xen_HOST_OS)
          , signalHandlerLock
  #endif
  
***************
*** 94,100 ****
          ) where
  
  import System.Posix.Types
! #ifndef mingw32_HOST_OS
  import System.Posix.Internals
  #endif
  import Foreign
--- 96,102 ----
          ) where
  
  import System.Posix.Types
! #if !defined(mingw32_HOST_OS) && !defined(xen_HOST_OS)
  import System.Posix.Internals
  #endif
  import Foreign
***************
*** 646,651 ****
--- 648,654 ----
  -- -----------------------------------------------------------------------------
  -- Thread IO API
  
+ #ifndef xen_HOST_OS
  -- | Block the current thread until data is available to read on the
  -- given file descriptor (GHC only).
  threadWaitRead :: Fd -> IO ()
***************
*** 669,674 ****
--- 672,678 ----
  	case fromIntegral fd of { I# fd# ->
  	case waitWrite# fd# s of { s -> (# s, () #)
  	}}
+ #endif
  
  -- | Suspends the current thread for a given number of microseconds
  -- (GHC only).
***************
*** 800,805 ****
--- 804,810 ----
  foreign import ccall unsafe "getUSecOfDay" 
    getUSecOfDay :: IO USecs
  
+ #ifndef xen_HOST_OS
  prodding :: IORef Bool
  {-# NOINLINE prodding #-}
  prodding = unsafePerformIO (newIORef False)
***************
*** 808,815 ****
  prodServiceThread = do
    was_set <- atomicModifyIORef prodding (\a -> (True,a))
    if (not (was_set)) then wakeupIOManager else return ()
  
! #ifdef mingw32_HOST_OS
  -- ----------------------------------------------------------------------------
  -- Windows IO manager thread
  
--- 813,829 ----
  prodServiceThread = do
    was_set <- atomicModifyIORef prodding (\a -> (True,a))
    if (not (was_set)) then wakeupIOManager else return ()
+ #endif
  
! #ifdef xen_HOST_OS
! 
! prodServiceThread :: IO ()
! prodServiceThread = return ()
! 
! startIOManagerThread :: IO ()
! startIOManagerThread = return ()
! 
! #elif mingw32_HOST_OS
  -- ----------------------------------------------------------------------------
  -- Windows IO manager thread
  
