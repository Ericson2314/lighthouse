{-# OPTIONS_GHC -fglasgow-exts #-}
module LwConc.ConcLib
( timerHandler
, blackholeHandler
, startScheduling
, fetchRunnableThread -- only for MVar code
, placeOnReadyQ -- only for MVar code
, forkIO
, yield
, queueLength
) where

--import GHC.Prim(ThreadId#)
import GHC.Conc(ThreadId,myThreadId)
import System.IO.Unsafe (unsafePerformIO)
import LwConc.Substrate
import Data.Sequence as Seq

------ debugging ------
import Foreign.C(CString, withCString)

foreign import ccall unsafe "start.h c_print" c_print :: CString -> IO ()

cPrint :: String -> IO ()
cPrint str = withCString str c_print

------ debugging ------

readyQ :: PVar (Seq SCont)
readyQ = unsafePerformIO $ newPVarIO Seq.empty

timerHandler :: IO ()
timerHandler = return () -- should be yield, but we're going coop for now!

-- Be careful when implementing this: if it tries to evaluate a thunk, and
-- blackholes, it can re-enter.  One could use TLS and busy-waiting to work
-- around this (section 4.2 of LwConc paper).  That said, I think yielding
-- is safe.
blackholeHandler :: IO ()
blackholeHandler = yield

-- |Start the scheduler, scheduling the first thread (presumes one has been
-- created with forkIO already), but _not_ storing the current thread on the
-- run queue.  Presumably the main thread will fork, run this, then do nothing
-- useful (if we have no threads, the system should halt and be done.)
startScheduling :: IO ()
startScheduling =
  do maybeThread <- atomically $ tryFetchRunnableThread
     case maybeThread of
       Nothing -> error "No threads to run! Invalid."
       Just firstThread -> switch $ \mainThread -> return firstThread

-- |Pops a thread off the ready queue and returns it, when there is one.
-- Note that if there is only one thread in the system, and it tries to yield,
-- there will be nothing in the ready queue.
tryFetchRunnableThread :: PTM (Maybe SCont)
tryFetchRunnableThread =
  do q <- readPVar readyQ
     case viewl q of
       EmptyL -> return Nothing
       (x :< xs) -> do writePVar readyQ xs
                       return (Just x)

-- |Pops a thread off the ready queue and returns it.  Throws an exception if
-- there is no runnable thread, as this is probably a bad thing.
fetchRunnableThread :: PTM SCont
fetchRunnableThread =
  do maybeThread <- tryFetchRunnableThread
     case maybeThread of
       Nothing -> error "Fatal scheduler error: No runnable threads!"
       Just t  -> return t

-- |Stores a thread at the end of the ready queue.
placeOnReadyQ :: SCont -> PTM ()
placeOnReadyQ thread =
  do q <- readPVar readyQ
     writePVar readyQ (q |> thread)

-- |Yielding primitive - switches to the next thread in the ready queue, if
-- one exists.  If not, it simply returns and continues running the current
-- thread.
yield :: IO ()
yield = -- It should be okay to do the following non-atomically
  do maybeThread <- atomically $ tryFetchRunnableThread
     case maybeThread of
       Nothing -> return ()
       Just newThread -> switch $ \oldThread -> do placeOnReadyQ oldThread
                                                   return newThread

-- |Fork.
-- In the future we could actually extend the thread to look like
--  1. Init code
--  2. Computation
--  3. Cleanup code
-- or wrap it somehow. This might let us catch, say, kill exceptions.
forkIO :: IO () -> IO ThreadId
forkIO computation =
  do newThread <- newSCont $ do computation
                                cPrint "Some thread just died"
     atomically $ placeOnReadyQ newThread
     myThreadId -- WRONG!
     
queueLength :: IO Int
queueLength = atomically $
  do q <- readPVar readyQ
     return (Seq.length q)
