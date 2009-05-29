module LwConc.Scheduler.Chance
( getNextThread
, schedule
, timeUp
, dumpQueueLengths
) where

-- This is a multi-level queue scheduler, taking priority into account.
--
-- To select a priority level, it starts with maxBound, and rolls a die.
-- If d100 < 10, it moves to a lower level, and starts over.

import System.IO.Unsafe (unsafePerformIO)
import Data.Sequence as Seq
import GHC.Arr as Array
import LwConc.PTM
import LwConc.Threads

import GHC.Float(double2Int, int2Double)

-- Introducing the world's worst random number generator (32-bit only)
-- http://en.wikipedia.org/wiki/Linear_congruential_generator
randomX :: PVar Integer
randomX = unsafePerformIO $ newPVarIO 42133786 -- just some seed

random :: PTM Int
random = do x <- readPVar randomX
            let x' = (1103515245 * x + 12345) `mod` 4294967296
            writePVar randomX x'
            return (fromInteger x')

-- |Returns a random number between 0 and 99...probably.
d100 :: PTM Int
d100 = do x <- random
          return $ double2Int ((int2Double x) * 100 / 4294967296)

timeUp :: IO Bool
timeUp = return True

type ReadyQ = PVar (Seq Thread)


-- |An array of ready queues, indexed by priority.
readyQs :: Array Priority ReadyQ
readyQs = listArray (minBound, maxBound) (unsafePerformIO $ sequence [newPVarIO Seq.empty | p <- [minBound .. maxBound :: Priority]])

-- |Returns which priority to pull the next thread from, and updates the countdown for next time.
getNextPriority :: PTM Priority
getNextPriority = helper maxBound
  where helper x | x == minBound = return x
        helper x = do r <- d100
                      if r < 10 -- % chance of trying a lower priority.
                         then helper (pred x)
                         else return x

-- |Returns the next ready thread, or Nothing.
getNextThread :: PTM (Maybe Thread)
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
schedule :: Thread -> PTM ()
schedule thread@(Thread tcb _) =
  do priority <- getPriority tcb
     let readyQ = readyQs ! priority
     q <- readPVar readyQ
     writePVar readyQ (q |> thread)

dumpQueueLengths :: (String -> IO ()) -> IO ()
dumpQueueLengths cPrint = mapM_ dumpQL [minBound .. maxBound]
  where dumpQL :: Priority -> IO ()
        dumpQL p = do len <- atomically $ do q <- readPVar (readyQs ! p)
                                             return (Seq.length q)
                      cPrint ("|readyQ[" ++ show p ++ "]| = " ++ show len ++ "\n")
