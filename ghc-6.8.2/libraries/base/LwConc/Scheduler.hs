{-# OPTIONS_GHC -fglasgow-exts #-}
module LwConc.Scheduler
( getNextThread
, schedule
, scheduleIn
, timeUp
, idleThreadId
, dumpQueueLengths
) where

import qualified LwConc.Scheduler.Dynamic as SchedPolicy

-- Lighthouse's schedulers are /passive/ - they manage run queues, consider
-- priority, and determine the next thread to run...but don't actively
-- interrupt threads, nor directly switch threads.

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forever)
import Data.Heap as Heap
import qualified Data.Sequence as Seq
import LwConc.PTM
import LwConc.Substrate
import LwConc.Threads

-- Stuff for sleeping threads (threadDelay backing)
import Foreign.C(CUInt)
foreign import ccall unsafe getourtimeofday :: IO CUInt
foreign import ccall unsafe idlePrint :: IO ()

getJiffies :: Integral i => PTM i
getJiffies = do cuint <- unsafeIOToPTM getourtimeofday
                return (fromIntegral cuint)

type SleepQueue = MinPrioHeap Int Thread

-- |A priority queue for sleeping threads, sorted by their wake time in jiffies.
sleepQ :: PVar SleepQueue
sleepQ = unsafePerformIO $ newPVarIO Heap.empty

-- 20 ms in a jiffy (see RtsFlags.c/tickInterval), 1000 us in a ms
usecToJiffies usec = usec `div` 20000

-- |Marks a thread as asleep and schedules it to be woken up and marked ready
-- after (at least) the given number microseconds.
scheduleIn :: Int -> Thread -> PTM ()
scheduleIn usec thread = if jfs == 0 then schedule thread else
  do now <- getJiffies -- current timestamp
     q <- readPVar sleepQ
     writePVar sleepQ (insert (now + jfs, thread) q)
  where jfs = usecToJiffies usec -- duration in jiffies

-- |Finds all threads who've slept long enough and schedules them.
wakeThreads :: PTM ()
wakeThreads = 
  do q <- readPVar sleepQ
     now <- getJiffies
     let (elts, q') = Heap.span (\(when, _) -> when <= now) q
     writePVar sleepQ q'
     mapM_ (schedule . snd) elts

-- Idle Thread
idleThreadId :: ThreadId
idleThreadId = unsafePerformIO $ do tbox <- newPVarIO Seq.empty
                                    prio <- newPVarIO minBound
                                    return (TCB 0 tbox prio)
idleThread :: PVar Thread
idleThread = unsafePerformIO $ do sc <- newSCont $ do setTLS tidTLSKey (Just idleThreadId)
                                                      forever (idlePrint >> switchT (\renewed -> do writePVar idleThread renewed
                                                                                                    getNextThread))
                                  newPVarIO (Thread idleThreadId sc)

{-
idleThread :: PVar Thread
idleThread = unsafePerformIO $ do sc <- newSCont idleloop
                                  newPVarIO (Thread idleThreadId sc)
  where idleLoop = do sc <- newSCont idleLoop
                      switchT (\_ -> writePVar idleThread (Thread idleThreadId sc) >> getNextThread))
 -}

-- |Returns the next ready thread, as determined by scheduling policy.
--  If no such thread exists, this will wait until one does.
--
--  This wraps the specific scheduler's implementation, first waking up any
--  sleeping threads whose delay is up, and also taking care of the details
--  of idling (easing the burden on scheduler authors).
getNextThread :: PTM Thread
getNextThread =
  do wakeThreads -- wake any blocked threads first.
     whatNext <- SchedPolicy.getNextThread
     case whatNext of
       Just t  -> return t
       Nothing -> readPVar idleThread
       --Nothing -> unsafeIOToPTM idlePrint >> readPVar idleThread

-- |Marks a thread "ready" and schedules it for some future time.
schedule :: Thread -> PTM ()
schedule = SchedPolicy.schedule

-- |Returns true if the current thread's time is up (and something else should run).
timeUp :: IO Bool
timeUp = SchedPolicy.timeUp

dumpQueueLengths :: (String -> IO ()) -> IO ()
dumpQueueLengths cPrint = do len <- atomically $ do q <- readPVar sleepQ
                                                    return (Heap.size q)
                             cPrint ("|sleepQ| = " ++ show len ++ "\n")
                             SchedPolicy.dumpQueueLengths cPrint
