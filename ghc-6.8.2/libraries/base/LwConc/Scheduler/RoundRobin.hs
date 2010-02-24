module LwConc.Scheduler.RoundRobin
( getNextThread
, schedule
, timeUp
, dumpQueueLengths
) where

-- This is a basic round-robin scheduler which ignores priority.
--
-- Lighthouse's schedulers are /passive/ - they manage run queues, consider
-- priority, and determine the next thread to run...but don't actively
-- interrupt threads, nor directly switch threads.

import System.IO.Unsafe (unsafePerformIO)
import Data.Sequence as Seq
import LwConc.PTM
import LwConc.Threads

timeUp :: IO Bool
timeUp = return True

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

