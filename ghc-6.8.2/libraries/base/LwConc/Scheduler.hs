{-# OPTIONS_GHC -fglasgow-exts #-}
module LwConc.Scheduler
( anythingReady
, getNextThread
, schedule
, scheduleIn
) where

-- This is a basic round-robin scheduler which ignores priority.
--
-- Lighthouse's schedulers are /passive/ - they manage run queues, consider
-- priority, and determine the next thread to run...but don't actively
-- interrupt threads, nor directly switch threads.

import System.IO.Unsafe (unsafePerformIO)
import Data.Sequence as Seq
import Data.Heap as Heap
import LwConc.STM
import LwConc.Substrate

-- Stuff for sleeping threads (threadDelay backing)
import Foreign.C(CUInt)
foreign import ccall unsafe getourtimeofday :: IO CUInt
foreign import ccall unsafe idlePrint :: IO ()

getJiffies :: Integral i => STM i
getJiffies = do cuint <- unsafeIOToSTM getourtimeofday
                return (fromIntegral cuint)

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
  do wakeThreads
     q <- readTVar readyQ
     case viewl q of
       (t :< ts) -> do writeTVar readyQ ts
                       return t
       EmptyL    -> -- Nothing to do, so return a new continuation that simply keeps checking...
                    unsafeIOToSTM $ idlePrint >> newSCont (switch (\idleThread -> getNextThread))

-- |Marks a thread "ready" and schedules it for some future time.
schedule :: SCont -> STM ()
schedule thread =
  do q <- readTVar readyQ
     writeTVar readyQ (q |> thread)

type SleepQueue = MinPrioHeap Int SCont

-- |A priority queue for sleeping threads, sorted by their wake time in jiffies.
sleepQ :: TVar SleepQueue
sleepQ = unsafePerformIO $ newTVarIO Heap.empty

-- 20 ms in a jiffy (see RtsFlags.c/tickInterval), 1000 us in a ms
usecToJiffies usec = usec `div` 20000

-- |Marks a thread as asleep and schedules it to be woken up and marked ready
-- after (at least) the given number microseconds.
scheduleIn :: Int -> SCont -> STM ()
scheduleIn usec thread = if jfs == 0 then schedule thread else
  do now <- getJiffies -- current timestamp
     q <- readTVar sleepQ
     writeTVar sleepQ (insert (now + jfs, thread) q)
  where jfs = usecToJiffies usec -- duration in jiffies

-- |Finds all threads who've slept long enough and schedules them.
wakeThreads :: STM ()
wakeThreads = 
  do q <- readTVar sleepQ
     now <- getJiffies
     let (elts, q') = Heap.span (\(when, _) -> when <= now) q
     writeTVar sleepQ q'
     mapM_ (schedule . snd) elts
