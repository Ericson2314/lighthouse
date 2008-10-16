{-# OPTIONS_GHC -fglasgow-exts #-}
module LwConc.Conc
( module LwConc.STM
, module LwConc.Substrate
-- Internals
, startSystem
, timerHandler
, die
-- Control.Concurrent stuff
, ThreadId(..)
, myThreadId
, forkIO
, killThread
, throwTo
, yield
, threadDelay
) where

import Prelude hiding (catch)
import GHC.Exception(Exception)
import Control.Monad(when)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import LwConc.Scheduler
import LwConc.Substrate
import LwConc.STM
import Foreign.C(CString, withCString)

foreign import ccall unsafe "start.h c_print" c_print :: CString -> IO ()

cPrint :: String -> IO ()
cPrint str = withCString str c_print

startSystem :: IO () -> IO ()
startSystem main = forkIO main >> die

-- | The timer interrupt handler.  Forces the thread to yield, unless it's
-- still starting up (and hasn't initialized its ThreadId.)
timerHandler :: IO ()
timerHandler = do val <- getTLS tidTLSKey
                  case val of
                    Nothing  -> return () -- uninitialized. let it continue
                    Just tid -> yield
                    
-- | Yield, marking the current thread ready & switching to the next one.
yield :: IO ()
yield = do switch $ \currThread -> do schedule currThread
                                      getNextThread
           checkSignals

-- | Switches to the next thread, but doesn't schedule the current thread again.
-- Since it's not running nor scheduled, it will be garbage collected.
die :: IO ()
die = switch $ \dyingThread -> getNextThread

-----------------------------------------------------------------------------
-- ThreadIds

-- | Thread IDs are primarily a IORef containing signalling information.
-- However, we also assign each a monotonically increasing integer - mostly
-- for printing, but possibly for other uses in the future.  These start at 1.

newtype ThreadId = ThreadId (Integer, IORef Bool)
  deriving Eq

instance Show ThreadId where
  show (ThreadId (id, box)) = "<Thread " ++ show id ++ ">"

instance Ord ThreadId where
  compare (ThreadId (x,_)) (ThreadId (y,_)) = compare x y

-- | Numerical IDs, starting at 1. Zero is reserved for special use.
nextThreadNum :: TVar Integer
nextThreadNum = unsafePerformIO $ newTVarIO 1

getNextThreadNum :: IO Integer
getNextThreadNum = atomically $ do x <- readTVar nextThreadNum
                                   writeTVar nextThreadNum (x+1)
                                   return x

-- | We store each thread's ID in their TLS, allowing a simple implementation
-- of myThreadId.  However, threads must initialize their own TLS, and we may
-- get an interrupt before the thread's fully set up.  Hence, we use a Maybe
-- type for the TLS key.

tidTLSKey :: TLSKey (Maybe ThreadId)
tidTLSKey = unsafePerformIO $ newTLSKey Nothing

-- | Returns the current ThreadId.  Only safe to call when threads are fully
-- initialized (i.e. have started running their real computation).
myThreadId :: IO ThreadId
myThreadId = do val <- getTLS tidTLSKey
                case val of
                  Nothing  -> error "myThreadId called on an uninitialized thread"
                  Just tid -> return tid

-- |Fork.
-- We actually wrap the provided computation, and can do initialization code
-- beforehand, or cleanup code afterward.
--
-- Notably, we must call 'die' after performing the computation so that
-- something else can be scheduled.  Otherwise it'll return to the RTS with a
-- "ThreadFinished" status, resulting in the system terminating.
forkIO :: IO () -> IO ThreadId
forkIO computation =
  do tid <- newThreadId
     newThread <- newSCont $ do setTLS tidTLSKey (Just tid)
                                cPrint (show tid ++ " has set its TLS Key.\n")
                                checkSignals -- check for kill before run the first time.
                                computation
                                cPrint (show tid ++ " completed without incident.\n")
                                die
     atomically $ schedule newThread
     return tid
  where newThreadId :: IO ThreadId
        newThreadId = do tnum <- getNextThreadNum
                         tbox <- newIORef False
                         return (ThreadId (tnum, tbox))
     
killThread :: ThreadId -> IO ()
killThread tid@(ThreadId (tnum, tbox)) = do writeIORef tbox True
                                            cPrint ("Asking " ++ show tid ++ " to kill itself...\n")
                                            -- we don't do a suicide check here;
                                            -- call die if you want to kill yourself immediately..

throwTo :: ThreadId -> Exception -> IO ()
throwTo tid exn = return ()


-- | Suspends the current thread for a given number of microseconds (GHC only).
--
-- There is no guarantee that the thread will be rescheduled promptly when the
-- delay has expired, but the thread will never continue to run earlier than specified.
threadDelay :: Int -> IO ()
threadDelay usec = do switch $ \currThread -> do scheduleIn usec currThread
                                                 getNextThread
                      checkSignals


-- | Check if anyone has sent us an asynchronous exception (only kill for now.)
checkSignals :: IO ()
checkSignals = do val <- getTLS tidTLSKey -- Could use myThreadId; we don't call this when uninitialized
                  case val of
                    Nothing -> return ()
                    Just (tid@(ThreadId (tnum, tbox))) -> do flag <- readIORef tbox
                                                             when flag $ cPrint (show tid ++ " is killing itself.\n") >> die
