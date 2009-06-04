{-# OPTIONS_GHC -fglasgow-exts #-}
module LwConc.Conc
( module LwConc.PTM
, module LwConc.Substrate
-- Internals
, startSystem
, timerHandler
, die
-- Control.Concurrent stuff
, myThreadId
, forkIO
, killThread
, throwTo
, yield
, threadDelay

, dumpAllThreads
) where

import GHC.Exception(Exception(..), AsyncException(..), catchException)
import Control.Monad(when, liftM)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Data.Sequence
import LwConc.PTM
import LwConc.Scheduler
import LwConc.Substrate
import LwConc.Threads
import Foreign.C(CString, withCString)
import System.Mem.Weak(Weak(..), mkWeakPtr, deRefWeak)

foreign import ccall unsafe "start.h c_print" c_print :: CString -> IO ()

cPrint :: String -> IO ()
cPrint str = withCString str c_print

-- | Fork the main computation off into its own real, initialized, thread,
-- then switch to it.  We just let the current "thread" die and get collected;
-- it doesn't have any of our necessary wrapper code.
startSystem :: IO () -> IO ()
startSystem main = forkIO main >> die

-- | The timer interrupt handler.  Forces the thread to yield, unless it's
-- still starting up (and hasn't initialized its ThreadId.)
timerHandler :: IO ()
timerHandler = do val <- atomically mySafeThreadId
                  case val of
                    Nothing  -> return () -- uninitialized. let it continue
                    Just tid -> do done <- timeUp
                                   when done yield
                    
-- | Yield, marking the current thread ready & switching to the next one.
yield :: IO ()
yield = switchT $ \currThread -> do schedule currThread
                                    getNextThread

allThreads :: PVar [Weak ThreadId]
allThreads = unsafePerformIO $ newPVarIO []

cleanWeakList :: [Weak a] -> PTM [Weak a]
cleanWeakList [] = return []
cleanWeakList (w:ws) = do m <- unsafeIOToPTM $ deRefWeak w
                          case m of
                            Nothing -> cleanWeakList ws
                            Just _  -> do ws' <- cleanWeakList ws
                                          return (w:ws')

cleanAllThreads :: PTM ()
cleanAllThreads = do ws <- readPVar allThreads
                     ws' <- cleanWeakList ws
                     writePVar allThreads ws'

dumpAllThreads :: (String -> IO ()) -> IO ()
dumpAllThreads putStr
  = do ws <- atomically $ cleanAllThreads >> readPVar allThreads
       strs <- mapM (liftM show . deRefWeak) ws
       putStr (show strs ++ "\n")

-- | Switches to the next thread, but doesn't schedule the current thread again.
-- Since it's not running nor scheduled, it will be garbage collected.
--
-- We don't use switchT here because it doesn't switch if the thread is
-- uninitialized, and we may want to die even if uninitialized.
die :: IO ()
die = switch $ \_ -> do (Thread _ sc) <- getNextThread
                        return sc

-- | Returns the current ThreadId.  Only safe to call when threads are fully
-- initialized (i.e. have started running their real computation).
myThreadId :: IO ThreadId
myThreadId = do val <- atomically $ mySafeThreadId
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
     sigsblocked <- atomically $ getTLS blockSignalsKey
     newThread <- newSCont $ catchException (do -- Inherit exception-blocked status from parent, per the spec.
                                                setTLS blockSignalsKey sigsblocked
                                                setTLS tidTLSKey (Just tid)
                                                cPrint (show tid ++ " is now initialized.\n")
                                                checkSignals -- check for kill before run the first time.
                                                debugShowTID
                                                computation
                                                cPrint (show tid ++ " completed without incident.\n")
                                                die)
                                            (\e -> do case e of
                                                        AsyncException ThreadKilled -> cPrint (show tid ++ " killed.\n")
                                                        _ -> cPrint ("Uncaught exception in " ++ show tid ++ ": " ++ show e ++ "\n")
                                                      die)

     weak <- mkWeakPtr tid Nothing
     atomically $ do schedule (Thread tid newThread)
                     ws <- readPVar allThreads
                     writePVar allThreads (weak:ws)
     return tid
     
killThread :: ThreadId -> IO ()
killThread tid@(TCB _ tbox _) =
  do atomically $ writePVar tbox (singleton (AsyncException ThreadKilled)) -- Override any other signals.
     cPrint ("Asking " ++ show tid ++ " to commit suicide...\n")
     checkSignals -- check if we just signalled ourself

throwTo :: ThreadId -> Exception -> IO ()
throwTo tid@(TCB _ tbox _) exn =
  do cPrint ("Sending " ++ show exn ++ " to " ++ show tid ++ "...\n")
     atomically $ do exns <- readPVar tbox
                     writePVar tbox (exns |> exn)
     checkSignals -- check if we just signalled ourself

-- | Suspends the current thread for a given number of microseconds (GHC only).
--
-- There is no guarantee that the thread will be rescheduled promptly when the
-- delay has expired, but the thread will never continue to run earlier than specified.
threadDelay :: Int -> IO ()
threadDelay usec = switchT $ \currThread -> do scheduleIn usec currThread
                                               getNextThread

