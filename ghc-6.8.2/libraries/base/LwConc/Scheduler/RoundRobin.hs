module LwConc.Scheduler.RoundRobin
( getNextThread
, schedule
) where

-- This is a basic round-robin scheduler which ignores priority.
--
-- Lighthouse's schedulers are /passive/ - they manage run queues, consider
-- priority, and determine the next thread to run...but don't actively
-- interrupt threads, nor directly switch threads.

import System.IO.Unsafe (unsafePerformIO)
import Data.Sequence as Seq
import LwConc.STM
import LwConc.Substrate

-- |The single ready queue used for round robin scheduling.
readyQ :: TVar (Seq SCont)
readyQ = unsafePerformIO $ newTVarIO Seq.empty

-- |Returns the next ready thread, or Nothing.
getNextThread :: STM (Maybe SCont)
getNextThread =
  do q <- readTVar readyQ
     case viewl q of
       (t :< ts) -> do writeTVar readyQ ts
                       return (Just t)
       EmptyL    -> return Nothing

-- |Marks a thread "ready" and schedules it for some future time.
schedule :: SCont -> STM ()
schedule thread =
  do q <- readTVar readyQ
     writeTVar readyQ (q |> thread)

