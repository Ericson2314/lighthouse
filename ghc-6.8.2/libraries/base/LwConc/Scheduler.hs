{-# OPTIONS_GHC -fglasgow-exts #-}
module LwConc.Scheduler
( getNextThread
, schedule
, scheduleIn
, timeUp
, dumpQueueLengths
) where

import qualified LwConc.Scheduler.Nonlinear as SchedPolicy

-- Lighthouse's schedulers are /passive/ - they manage run queues, consider
-- priority, and determine the next thread to run...but don't actively
-- interrupt threads, nor directly switch threads.

import System.IO.Unsafe (unsafePerformIO)
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

-- |Returns the next ready thread, as determined by scheduling policy.
--  If no such thread exists, this will wait until one does.
--
--  This wraps the specific scheduler's implementation, first waking up any
--  sleeping threads whose delay is up, and also taking care of the details
--  of idling (easing the burden on scheduler authors).
getNextThread :: STM SCont
getNextThread =
  do wakeThreads -- wake any blocked threads first.
     whatNext <- SchedPolicy.getNextThread
     case whatNext of
       Just scont -> return scont
       Nothing    -> -- Nothing to do, so return a new continuation that simply keeps checking...
                     unsafeIOToSTM $ idlePrint >> newSCont (switch (\idleThread -> getNextThread))

-- |Marks a thread "ready" and schedules it for some future time.
schedule :: SCont -> STM ()
schedule = SchedPolicy.schedule

-- |Returns true if the current thread's time is up (and something else should run).
timeUp :: IO Bool
timeUp = SchedPolicy.timeUp

dumpQueueLengths :: (String -> IO ()) -> IO ()
dumpQueueLengths cPrint = do len <- atomically $ do q <- readTVar sleepQ
                                                    return (Heap.size q)
                             cPrint ("|sleepQ| = " ++ show len ++ "\n")
                             SchedPolicy.dumpQueueLengths cPrint
