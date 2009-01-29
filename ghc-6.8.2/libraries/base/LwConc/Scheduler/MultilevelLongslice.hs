module LwConc.Scheduler.MultilevelLongslice
( getNextThread
, schedule
, timeUp
, dumpQueueLengths
) where

-- This is a multi-level queue scheduler, taking priority into account.
--
-- Priority scheduling works like this (assuming A > B > C > ...):
--   A AB ABC ABCD ABCDE ...

import System.IO.Unsafe (unsafePerformIO)
import Data.Sequence as Seq
import GHC.Arr as Array
import LwConc.Priority
import LwConc.PTM
import LwConc.Substrate

ticksKey :: TLSKey Priority
ticksKey = unsafePerformIO $ newTLSKey C

timeUp :: IO Bool
timeUp =
  do p <- getTLS ticksKey
     if p == minBound
        then do prio <- myPriority -- out of time; reset count for next time
                setTLS ticksKey prio
                return True
        else setTLS ticksKey (pred p) >> return False -- keep going

type ReadyQ = PVar (Seq SCont)

-- |An array of ready queues, indexed by priority.
readyQs :: Array Priority ReadyQ
readyQs = listArray (minBound, maxBound) (unsafePerformIO $ sequence [newPVarIO Seq.empty | p <- [minBound .. maxBound :: Priority]])

priorityBox :: PVar (Priority, Priority) -- (Priority to run next, lower bound on allowed priority this run)
priorityBox = unsafePerformIO $ newPVarIO (maxBound, maxBound)

-- |Returns which priority to pull the next thread from, and updates the countdown for next time.
getNextPriority :: PTM Priority
getNextPriority =
  do (p, limit) <- readPVar priorityBox
     writePVar priorityBox (if p > limit then (pred p, limit) else (maxBound, cyclicPred limit))
     return p
  where -- |Like `pred`, but cycles back to maxBound instead of throwing an error.
        cyclicPred :: (Bounded a, Enum a, Eq a) => a -> a
        cyclicPred p | p == minBound = maxBound
                     | otherwise     = pred p

-- |Returns the next ready thread, or Nothing.
getNextThread :: PTM (Maybe SCont)
getNextThread = do priority <- getNextPriority
                   tryAt priority
  where tryAt priority = do let readyQ = readyQs ! priority
                            q <- readPVar readyQ
                            case viewl q of
                              (t :< ts) -> do writePVar readyQ ts
                                              return (Just t)
                              EmptyL -> if priority == minBound
                                           then return Nothing
                                           else tryAt (pred priority) -- nothing to run at this priority, try something lower.

-- |Marks a thread "ready" and schedules it for some future time.
schedule :: SCont -> PTM ()
schedule thread =
  do priority <- unsafeIOToPTM myPriority
     let readyQ = readyQs ! priority
     q <- readPVar readyQ
     writePVar readyQ (q |> thread)

dumpQueueLengths :: (String -> IO ()) -> IO ()
dumpQueueLengths cPrint = mapM_ dumpQL [minBound .. maxBound]
  where dumpQL :: Priority -> IO ()
        dumpQL p = do len <- atomically $ do q <- readPVar (readyQs ! p)
                                             return (Seq.length q)
                      cPrint ("|readyQ[" ++ show p ++ "]| = " ++ show len ++ "\n")
