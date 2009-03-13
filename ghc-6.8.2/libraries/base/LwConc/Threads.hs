{-# OPTIONS_GHC -fglasgow-exts #-}
module LwConc.Threads
( ThreadId(..)
, tidTLSKey
, mySafeThreadId
, newThreadId
, debugShowTID

, Priority(..)
, getPriority
, setPriority
, myPriority
, setMyPriority

, Thread(..)
, switchT
) where

import GHC.Arr(Ix)
import GHC.Exception(Exception)
import Data.Sequence
import Foreign.C.Types(CUInt)
import LwConc.PTM
import LwConc.Substrate
import System.IO.Unsafe(unsafePerformIO)

-----------------------------------------------------------------------------
-- Priorities

data Priority = E | D | C | B | A
  deriving (Show, Eq, Ord, Bounded, Enum, Ix)

-- |Get the given thread's priority.
getPriority :: ThreadId -> PTM Priority
getPriority (TCB _ _ pv) = readPVar pv

-- |Set the given thread's priority.
setPriority :: ThreadId -> Priority -> PTM ()
setPriority (TCB _ _ pv) p = writePVar pv p

-- |Returns the current thread's priority.
myPriority :: PTM Priority
myPriority = do m <- mySafeThreadId
                case m of
                  Nothing  -> return maxBound -- uninitialized thread
                  Just tid -> getPriority tid

-- |Sets the current thread's priority.
setMyPriority :: Priority -> PTM ()
setMyPriority p = do m <- mySafeThreadId
                     case m of
                       Nothing -> return ()
                       Just tid -> setPriority tid p

-----------------------------------------------------------------------------
-- ThreadIds

-- | Thread IDs are primarily a IORef containing signalling information.
-- However, we also assign each a monotonically increasing integer - mostly
-- for printing, but possibly for other uses in the future.  These start at 1.

data ThreadId = TCB Int (PVar (Seq Exception)) (PVar Priority)

instance Eq ThreadId where
  (TCB _ xbox _) == (TCB _ ybox _) = xbox == ybox

instance Show ThreadId where
  show (TCB id _ _) = "<Thread " ++ show id ++ ">"

-- | We store each thread's ID in their TLS, allowing a simple implementation
-- of myThreadId.  However, threads must initialize their own TLS, and we may
-- get an interrupt before the thread's fully set up.  Hence, we use a Maybe
-- type for the TLS key.

tidTLSKey :: TLSKey (Maybe ThreadId)
tidTLSKey = unsafePerformIO $ newTLSKey Nothing

-- | A version of myThreadId that returns a Maybe type, and hence is safe to
-- call even from interrupt handling contexts.  Mostly useful for debugging.
mySafeThreadId :: PTM (Maybe ThreadId)
mySafeThreadId = getTLS tidTLSKey

-- | Numerical IDs, starting at 1 (0 is reserved for the idle thread).
nextThreadNum :: PVar Int
nextThreadNum = unsafePerformIO $ newPVarIO 1

getNextThreadNum :: IO Int
getNextThreadNum = atomically $ do x <- readPVar nextThreadNum
                                   writePVar nextThreadNum (x+1)
                                   return x

newThreadId :: IO ThreadId
newThreadId = do tnum <- getNextThreadNum
                 tbox <- newPVarIO empty
                 prio <- newPVarIO C
                 return (TCB tnum tbox prio)

-- | Code to support printing the running thread ID in the upper right hand
-- corner of the screen in House text mode.
foreign import ccall unsafe showCornerNumber :: CUInt -> IO ()

debugShowTID = do mtid <- atomically mySafeThreadId
                  showCornerNumber $ case mtid of
                                       Nothing -> 0
                                       Just (TCB tnum _ _) -> fromIntegral tnum


-----------------------------------------------------------------------------
-- Threads = TCB + SCont

data Thread = Thread ThreadId SCont

-- | A wrapper around the switch primitive that operates at Thread level,
-- instead of the SCont level.
switchT :: (Thread -> PTM Thread) -> IO ()
switchT f = do switch $ \currSC -> do m <- mySafeThreadId
                                      case m of
                                        Nothing      -> return currSC -- refuse to switch if uninitialized
                                        Just currTCB -> do (Thread nextTCB nextSC) <- f (Thread currTCB currSC)
                                                           return nextSC
               debugShowTID

