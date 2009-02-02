module LwConc.Priority
( Priority(..)
, getPriority
, setPriority
, myPriority
, setMyPriority
) where

import GHC.Arr(Ix)
import LwConc.PTM
import LwConc.Substrate

--data Priority = Low | Medium | High
--data Priority = Worthless | Low | Medium | High | Utmost
  --deriving (Show, Eq, Ord, Bounded, Enum, Ix)

-- |Get the given thread's priority.
getPriority :: ThreadId -> PTM Priority
getPriority (TCB _ _ pv) = readPVar pv

-- |Set the given thread's priority.
setPriority :: ThreadId -> Priority -> PTM ()
setPriority (TCB _ _ pv) p = writePVar pv p

-- |Returns the current thread's priority.
myPriority :: IO Priority
myPriority = do m <- mySafeThreadId
                case m of
                  Nothing  -> return maxBound -- uninitialized thread
                  Just tid -> atomically $ getPriority tid

-- |Sets the current thread's priority.
setMyPriority :: Priority -> IO ()
setMyPriority p = do m <- mySafeThreadId
                     case m of
                       Nothing -> return ()
                       Just tid -> atomically $ setPriority tid p

