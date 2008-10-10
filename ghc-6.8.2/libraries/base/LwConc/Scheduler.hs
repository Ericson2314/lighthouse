{-# OPTIONS_GHC -fglasgow-exts #-}
module LwConc.Scheduler
( anythingReady
, getNextThread
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

-- |Returns whether or not there's another ready thread in the system.
-- Note that the current thread is not included.
anythingReady :: STM Bool
anythingReady =
  do q <- readTVar readyQ
     return (Seq.null q)

-- |Returns the next ready thread, as determined by scheduling policy.
-- If no such thread exists, this will wait until one does.
getNextThread :: STM SCont
getNextThread =
  do q <- readTVar readyQ
     case viewl q of
       (t :< ts) -> do writeTVar readyQ ts
                       return t
       EmptyL    -> -- Nothing to do, so return a new continuation that simply keeps checking...
                    unsafeIOToSTM $ newSCont (switch (\idleThread -> getNextThread))

-- |Marks a thread "ready" and schedules it for some future time.
schedule :: SCont -> STM ()
schedule thread =
  do q <- readTVar readyQ
     writeTVar readyQ (q |> thread)

