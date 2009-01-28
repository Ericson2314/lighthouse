module LwConc.Scheduler.Dynamic
( getNextThread
, schedule
, timeUp
, dumpQueueLengths
) where

-- This is a multi-level queue scheduler, taking priority into account.
--
-- It addresses the following problem:
-- -  If we pull from priority A's queue 15x more often than priority B's queue...
-- -  and there happen to be 15 threads at priority A, and only 1 at priority B...
-- => Then they all effectively run at the same priority.
--
-- This scheduler examines the length of each priority's queue, and pulls that many
-- before moving on to the next priority.
--
-- So while it may follow a simple order for priority, like
--    A AB ABC ABCD ABCDE
-- it's actually much more aggressive, because it'll run 15 threads from A, not just one.
--
-- Of course, the length of the queues may change; this is just a useful approximation.
-- It'll recalculate the numbers when it finishes the sequence of priorities.

import System.IO.Unsafe (unsafePerformIO)
import Data.Sequence as Seq
import GHC.Arr as Array
import LwConc.Priority
import LwConc.STM
import LwConc.Substrate

timeUp :: IO Bool
timeUp = return True

type ReadyQ = TVar (Seq SCont)

-- |An array of ready queues, indexed by priority.
readyQs :: Array Priority ReadyQ
readyQs = listArray (minBound, maxBound) (unsafePerformIO $ sequence [newTVarIO Seq.empty | p <- [minBound .. maxBound :: Priority]])

queueLengths :: TVar (Array Priority Int)
queueLengths = unsafePerformIO $ newTVarIO undefined

priorityBox :: TVar [(Int, Priority)]
priorityBox = unsafePerformIO $ newTVarIO $ []

-- |Returns which priority to pull the next thread from, and updates the countdown for next time.
getNextPriority :: STM Priority
getNextPriority =
  do l <- readTVar priorityBox
     case l of
       [] -> do est <- mapM (\prio -> do q <- readTVar (readyQs ! prio)
                                         return (Seq.length q, prio)) order
                writeTVar priorityBox (filter (\(x,_) -> x /= 0) est) -- skip empty priorities
                return A
       ((1,p):ps) -> do writeTVar priorityBox ps
                        return p
       ((n,p):ps) -> do writeTVar priorityBox ((n-1,p):ps)
                        return p
  where order = [A,B,A,B,C,A,B,C,D,A,B,C,D,E]

-- |Returns the next ready thread, or Nothing.
getNextThread :: STM (Maybe SCont)
getNextThread = do priority <- getNextPriority
                   tryAt priority
  where tryAt priority = do let readyQ = readyQs ! priority
                            q <- readTVar readyQ
                            case viewl q of
                              (t :< ts) -> do writeTVar readyQ ts
                                              return (Just t)
                              EmptyL -> if priority == minBound
                                           then return Nothing
                                           else tryAt (pred priority) -- nothing to run at this priority, try something lower.

-- |Marks a thread "ready" and schedules it for some future time.
schedule :: SCont -> STM ()
schedule thread =
  do priority <- unsafeIOToSTM myPriority
     let readyQ = readyQs ! priority
     q <- readTVar readyQ
     writeTVar readyQ (q |> thread)

dumpQueueLengths :: (String -> IO ()) -> IO ()
dumpQueueLengths cPrint = mapM_ dumpQL [minBound .. maxBound]
  where dumpQL :: Priority -> IO ()
        dumpQL p = do len <- atomically $ do q <- readTVar (readyQs ! p)
                                             return (Seq.length q)
                      cPrint ("|readyQ[" ++ show p ++ "]| = " ++ show len ++ "\n")
