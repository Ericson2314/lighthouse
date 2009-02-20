module LwConc.Scheduler.FixedHigh
( getNextThread
, schedule
, timeUp
, dumpQueueLengths
) where

-- This is a multi-level queue scheduler, taking priority into account.
--
-- It always chooses to run the highest priority thread available.
-- This will likely starve other threads; it's not designed for general use.

import System.IO.Unsafe (unsafePerformIO)
import Data.Sequence as Seq
import GHC.Arr as Array
import LwConc.PTM
import LwConc.Threads

timeUp :: IO Bool
timeUp = return True

type ReadyQ = PVar (Seq Thread)

-- |An array of ready queues, indexed by priority.
readyQs :: Array Priority ReadyQ
readyQs = listArray (minBound, maxBound) (unsafePerformIO $ sequence [newPVarIO Seq.empty | p <- [minBound .. maxBound :: Priority]])

-- |Returns the next ready thread, or Nothing.
getNextThread :: PTM (Maybe Thread)
getNextThread = tryAt maxBound
  where tryAt priority = do let readyQ = readyQs ! priority
                            q <- readPVar readyQ
                            case viewl q of
                              (t :< ts) -> do writePVar readyQ ts
                                              return (Just t)
                              EmptyL -> if priority == minBound
                                           then return Nothing
                                           else tryAt (pred priority) -- nothing to run at this priority, try something lower.

-- |Marks a thread "ready" and schedules it for some future time.
schedule :: Thread -> PTM ()
schedule thread =
  do priority <- myPriority
     let readyQ = readyQs ! priority
     q <- readPVar readyQ
     writePVar readyQ (q |> thread)

dumpQueueLengths :: (String -> IO ()) -> IO ()
dumpQueueLengths cPrint = mapM_ dumpQL [minBound .. maxBound]
  where dumpQL :: Priority -> IO ()
        dumpQL p = do len <- atomically $ do q <- readPVar (readyQs ! p)
                                             return (Seq.length q)
                      cPrint ("|readyQ[" ++ show p ++ "]| = " ++ show len ++ "\n")
