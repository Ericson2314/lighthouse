{-# OPTIONS_GHC -fglasgow-exts #-}
module LwConc.Conc
( module LwConc.STM
, module LwConc.Substrate
-- Internals
, startSystem
, timerHandler
, die
-- Control.Concurrent stuff
, ThreadId(..)
, myThreadId
, forkIO
, killThread
, throwTo
, yield
, threadDelay
) where

import Control.Exception(Exception)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import LwConc.Scheduler
import LwConc.Substrate
import LwConc.STM

startSystem :: IO () -> IO ()
startSystem main = forkIO main >> die

timerHandler :: IO ()
timerHandler = yield

-- |Yield, marking the current thread ready & switching to the next one.
yield :: IO ()
yield = do switch $ \currThread -> do schedule currThread
                                      getNextThread
           --checkSignals

-- |Switches to the next thread, but doesn't schedule the current thread again.
-- Since it's not running nor scheduled, it will be garbage collected.
die :: IO ()
die = switch $ \dyingThread -> getNextThread

--newtype ThreadId = ThreadId (Int, IORef Bool)
newtype ThreadId = ThreadId (IORef Bool)

{-
-- Need to figure out race conditions associated with TLS initialization,
-- as well as how getTLS is actually a STM function...
-- alternatively we may be able to make
-- data Thread = Thread ThreadId SCont
-- and switchThread...and set a global before switching...
myThreadIdTLSKey :: TLSKey ThreadId
myThreadIdTLSKey = unsafePerformIO $
  do tidBox <- newIORef False
     newTLSKey (ThreadId tid)

myThreadId :: IO ThreadId
myThreadId = atomically $ getTLS myThreadIdTLSKey
-}

myThreadId :: IO ThreadId
myThreadId = do tidBox <- newIORef False -- this is totally wrong!
                return (ThreadId tidBox)
-- |Fork.
-- We actually wrap the provided computation, and can do initialization code
-- beforehand, or cleanup code afterward.
--
-- Notably, we must call 'die' after performing the computation so that
-- something else can be scheduled.  Otherwise it'll return to the RTS with a
-- "ThreadFinished" status, resulting in the system terminating.
forkIO :: IO () -> IO ThreadId
forkIO computation =
  do tidBox <- newIORef False
     newThread <- newSCont $ do -- setTLS myThreadIdTLSKey tidBox
                                computation
                                die
     atomically $ schedule newThread
     return (ThreadId tidBox)
     
     -- What if we try to kill ourselves?  Want it to be immediate, no?
killThread :: ThreadId -> IO ()
killThread (ThreadId box) = writeIORef box True

--throw (AsyncException ThreadKilled)

throwTo :: ThreadId -> Exception -> IO ()
throwTo tid exn = return ()

threadDelay :: Int -> IO ()
threadDelay n = return ()

