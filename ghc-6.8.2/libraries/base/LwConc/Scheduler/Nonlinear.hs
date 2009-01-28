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
import LwConc.Priority
import LwConc.STM
import LwConc.Substrate

timeUp :: IO Bool
timeUp = return True

type ReadyQ = TVar (Seq SCont)

-- |An array of ready queues, indexed by priority.
readyQs :: Array Priority ReadyQ
readyQs = listArray (minBound, maxBound) (unsafePerformIO $ sequence [newTVarIO Seq.empty | p <- [minBound .. maxBound :: Priority]])

{-
priorityBox :: TVar [Priority]
priorityBox = unsafePerformIO $ newTVarIO []

-- |Returns which priority to pull the next thread from, and updates the countdown for next time.
getNextPriority :: STM Priority
getNextPriority =
  do ps <- readTVar priorityBox
     case ps of
       []     -> writeTVar priorityBox nonlinear >> getNextPriority
       (p:ps) -> writeTVar priorityBox ps        >> return p
  where ps lim = [maxBound, pred maxBound .. lim]
        multilevel lim = concatMap ps (ps lim)
        nonlinear = concatMap multilevel (ps minBound)
-}

priorityBox :: TVar [Priority]
priorityBox = unsafePerformIO $ newTVarIO $ (concat . repeat) nonlinear
  where --ps lim = [maxBound, pred maxBound .. lim]
        --multilevel lim = concatMap ps (ps lim)
        --nonlinear = concatMap multilevel (ps minBound)
        nonlinear = [A,A,A,B,A,A,B,A,B,C,A,A,B,A,B,C,A,B,C,D,A,A,B,A,B,C,A,B,C,D,A,B,C,D,E]

-- |Returns which priority to pull the next thread from, and updates the countdown for next time.
getNextPriority :: STM Priority
getNextPriority =
  do (p:ps) <- readTVar priorityBox
     writeTVar priorityBox ps
     return p

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
