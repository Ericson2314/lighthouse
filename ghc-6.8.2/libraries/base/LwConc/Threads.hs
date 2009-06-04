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

, blockSignalsKey
, checkSignals
, block
, unblock
) where

import GHC.Arr(Ix)
import GHC.Exception(Exception, throw, catchException)
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
               checkSignals

-----------------------------------------------------------------------------
-- Asynchronous Exception stuff

blockSignalsKey :: TLSKey Bool
blockSignalsKey = unsafePerformIO $ newTLSKey False

-- | Check if anyone has sent us an asynchronous exception.
checkSignals :: IO ()
checkSignals = do mx <- atomically $ do mtid <- mySafeThreadId
                                        sigsBlocked <- getTLS blockSignalsKey
                                        case (mtid, sigsBlocked) of
                                          (Nothing, _) -> return Nothing
                                          (_, True)    -> return Nothing
                                          (Just tid@(TCB _ tbox _), _) ->
                                             do exns <- readPVar tbox
                                                case viewl exns of
                                                  (e :< es) -> writePVar tbox es >> return (Just e)
                                                  EmptyL    -> return Nothing
                  -- We need to throw -outside- of the atomically so it doesn't retry.
                  -- Also, don't think you can handle AsyncException ThreadKilled specially!
                  -- It needs to be thrown as an exception so things like withMVar
                  -- can properly clean up before dying - for the sake of others!
                  case mx of
                    Nothing  -> return ()
                    Just exn -> throw exn

-- | Applying 'block' to a computation will
-- execute that computation with asynchronous exceptions
-- /blocked/.  That is, any thread which
-- attempts to raise an exception in the current thread with 'Control.Exception.throwTo' will be
-- blocked until asynchronous exceptions are enabled again.  There\'s
-- no need to worry about re-enabling asynchronous exceptions; that is
-- done automatically on exiting the scope of
-- 'block'.
--
-- Threads created by 'Control.Concurrent.forkIO' inherit the blocked
-- state from the parent; that is, to start a thread in blocked mode,
-- use @block $ forkIO ...@.  This is particularly useful if you need to
-- establish an exception handler in the forked thread before any
-- asynchronous exceptions are received.
block :: IO a -> IO a
block computation = do saved <- atomically $ getTLS blockSignalsKey
                       setTLS blockSignalsKey True
                       -- Catch synchronous (not asynchronous) exns so we unwind properly
                       x <- catchException computation
                                           (\e -> setTLS blockSignalsKey saved >> throw e)
                       setTLS blockSignalsKey saved
                       checkSignals
                       return x

-- | To re-enable asynchronous exceptions inside the scope of
-- 'block', 'unblock' can be
-- used.  It scopes in exactly the same way, so on exit from
-- 'unblock' asynchronous exception delivery will
-- be disabled again.
unblock :: IO a -> IO a
unblock computation = do saved <- atomically $ getTLS blockSignalsKey
                         -- Catch synchronous OR asynchronous exns so we unwind properly
                         x <- catchException (setTLS blockSignalsKey False >> checkSignals >> computation)
                                             (\e -> setTLS blockSignalsKey saved >> throw e)
                         setTLS blockSignalsKey saved
                         return x

