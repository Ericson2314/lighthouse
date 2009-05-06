module LwConc.Scheduler.Longslice
( getNextThread
, schedule
, timeUp
, dumpQueueLengths
) where

-- This is a round-robin scheduler that gives longer timeslices to higher
-- priority threads in an attempt to reduce scheduling overhead.
--
-- Lighthouse's schedulers are /passive/ - they manage run queues, consider
-- priority, and determine the next thread to run...but don't actively
-- interrupt threads, nor directly switch threads.

import System.IO.Unsafe (unsafePerformIO)
import Data.Sequence as Seq
import LwConc.PTM
import LwConc.Substrate
import LwConc.Threads
import Data.IORef

ticksKey :: TLSKey Int
ticksKey = unsafePerformIO $ newTLSKey 0

-- Specifies the number of extra timeslices allotted to a given priority;
-- all threads get at least one (since timeUp is only called from timerHandler)
extraTicks :: Priority -> Int
extraTicks p = 10 * fromEnum p

timeUp :: IO Bool
timeUp =
  do slicesLeft <- atomically $ getTLS ticksKey
     if slicesLeft < 1
        then do p <- atomically myPriority -- out of time; reset count for next time
                setTLS ticksKey (extraTicks p)
                return True
        else do setTLS ticksKey (slicesLeft-1) -- keep going and decrement count
                return False

-- |The single ready queue used for round robin scheduling.
readyQ :: PVar (Seq Thread)
readyQ = unsafePerformIO $ newPVarIO Seq.empty

-- |Returns the next ready thread, or Nothing.
getNextThread :: PTM (Maybe Thread)
getNextThread =
  do q <- readPVar readyQ
     case viewl q of
       (t :< ts) -> do writePVar readyQ ts
                       return (Just t)
       EmptyL    -> return Nothing

-- |Marks a thread "ready" and schedules it for some future time.
schedule :: Thread -> PTM ()
schedule thread =
  do q <- readPVar readyQ
     writePVar readyQ (q |> thread)

dumpQueueLengths :: (String -> IO ()) -> IO ()
dumpQueueLengths cPrint = do len <- atomically $ do q <- readPVar (readyQ)
                                                    return (Seq.length q)
                             cPrint ("|readyQ| = " ++ show len ++ "\n")

