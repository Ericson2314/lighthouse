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
-- Control.Exception stuff
, block
, unblock

, dumpAllThreads
) where

import GHC.Exception(Exception(..), AsyncException(..), catchException, throw)
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
yield = do switchT $ \currThread -> do schedule currThread
                                       getNextThread
           checkSignals

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
threadDelay usec = do switchT $ \currThread -> do scheduleIn usec currThread
                                                  getNextThread
                      checkSignals


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

-- this key, and 'block' and 'unblock' may actually belong in the substrate...
blockSignalsKey :: TLSKey Bool
blockSignalsKey = unsafePerformIO $ newTLSKey False

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

