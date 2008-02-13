*** ghc-pristine/libraries/base/Control/Concurrent.hs	2007-01-05 10:42:22.000000000 -0800
--- ghc-xen/libraries/base/Control/Concurrent.hs	2007-01-10 14:43:12.000000000 -0800
***************
*** 43,51 ****
--- 43,53 ----
  #ifdef __GLASGOW_HASKELL__
  	-- ** Waiting
  	threadDelay,		-- :: Int -> IO ()
+ #ifndef xen_HOST_OS
  	threadWaitRead,		-- :: Int -> IO ()
  	threadWaitWrite,	-- :: Int -> IO ()
  #endif
+ #endif
  
  	-- * Communication abstractions
  
***************
*** 96,102 ****
  
  #ifdef __GLASGOW_HASKELL__
  import GHC.Conc		( ThreadId(..), myThreadId, killThread, yield,
! 			  threadDelay, threadWaitRead, threadWaitWrite,
  			  forkIO, childHandler )
  import GHC.TopHandler   ( reportStackOverflow, reportError )
  import GHC.IOBase	( IO(..) )
--- 98,107 ----
  
  #ifdef __GLASGOW_HASKELL__
  import GHC.Conc		( ThreadId(..), myThreadId, killThread, yield,
! 			  threadDelay, 
! #ifndef xen_HOST_OS
!                           threadWaitRead, threadWaitWrite,
! #endif
  			  forkIO, childHandler )
  import GHC.TopHandler   ( reportStackOverflow, reportError )
  import GHC.IOBase	( IO(..) )
