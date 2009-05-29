module LwConc.Scheduler.Nonlinear
( getNextThread
, schedule
, timeUp
, dumpQueueLengths
) where

-- This is a multi-level queue scheduler, taking priority into account.
--
-- The multilevel one is:
-- let gen n = concatMap (flip take ps) [1..n]
--  so gen 5 ~> [A,A,B,A,B,C,A,B,C,D,A,B,C,D,E]
--
-- Priority scheduling works like this (assuming A > B > C > ...):
--        A, A AB, A AB ABC, A AB ABC ABCD, A AB ABC ABCDE
--
--        A runs 14x
--        B runs 9x
--        C runs 5x
--        D runs 2x
--        E runs 1x
--
-- This is concatMap gen [1..5]

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

{-
priorityBox :: PVar [Priority]
priorityBox = unsafePerformIO $ newPVarIO []

-- |Returns which priority to pull the next thread from, and updates the countdown for next time.
getNextPriority :: PTM Priority
getNextPriority =
  do ps <- readPVar priorityBox
     case ps of
       []     -> writePVar priorityBox nonlinear >> getNextPriority
       (p:ps) -> writePVar priorityBox ps        >> return p
  where ps lim = [maxBound, pred maxBound .. lim]
        multilevel lim = concatMap ps (ps lim)
        nonlinear = concatMap multilevel (ps minBound)
-}

priorityBox :: PVar [Priority]
priorityBox = unsafePerformIO $ newPVarIO $ (concat . repeat) nonlinear
  where --ps lim = [maxBound, pred maxBound .. lim]
        --multilevel lim = concatMap ps (ps lim)
        --nonlinear = concatMap multilevel (ps minBound)
        nonlinear = [A,A,A,B,A,A,B,A,B,C,A,A,B,A,B,C,A,B,C,D,A,A,B,A,B,C,A,B,C,D,A,B,C,D,E]

-- |Returns which priority to pull the next thread from, and updates the countdown for next time.
getNextPriority :: PTM Priority
getNextPriority =
  do (p:ps) <- readPVar priorityBox
     writePVar priorityBox ps
     return p

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
