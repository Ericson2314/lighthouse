module LwConc.Priority
( Priority(..)
, getPriority
, setPriority
, myPriority
, setMyPriority
) where

import GHC.Arr(Ix)
import LwConc.STM
import LwConc.Substrate

--data Priority = Low | Medium | High
--data Priority = Worthless | Low | Medium | High | Utmost
  --deriving (Show, Eq, Ord, Bounded, Enum, Ix)

-- |Get the given thread's priority.
getPriority :: ThreadId -> STM Priority
getPriority (TCB _ _ pv) = readTVar pv

-- |Set the given thread's priority.
setPriority :: ThreadId -> Priority -> STM ()
setPriority tid p = do m <- unsafeIOToSTM mySafeThreadId
                       case m of
                         Nothing  -> return ()
                         Just (TCB _ _ pv) -> writeTVar pv p

-- |Returns the current thread's priority.
myPriority :: IO Priority
myPriority = do m <- mySafeThreadId
                case m of
                  Nothing  -> return Utmost -- uninitialized thread
                  Just tid -> atomically $ getPriority tid

-- |Sets the current thread's priority.
setMyPriority :: Priority -> IO ()
setMyPriority p = do m <- mySafeThreadId
                     case m of
                       Nothing -> return ()
                       Just tid -> atomically $ setPriority tid p

