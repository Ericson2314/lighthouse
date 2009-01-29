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
import Control.Monad(when, unless, liftM)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Data.Sequence
import LwConc.Scheduler
import LwConc.Substrate
import LwConc.PTM
import Foreign.C(CString, withCString)
import System.Mem.Weak(Weak(..), mkWeakPtr, deRefWeak)

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
                    Just tid -> do done <- timeUp
                                   when done yield
                    
-- | Yield, marking the current thread ready & switching to the next one.
yield :: IO ()
yield = do switch $ \currThread -> do schedule currThread
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
die :: IO ()
die = switch $ \dyingThread -> getNextThread

-----------------------------------------------------------------------------
-- ThreadIds

-- | Numerical IDs, starting at 1.
nextThreadNum :: PVar Int
nextThreadNum = unsafePerformIO $ newPVarIO 1

getNextThreadNum :: IO Int
getNextThreadNum = atomically $ do x <- readPVar nextThreadNum
                                   writePVar nextThreadNum (x+1)
                                   return x

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
     sigsblocked <- getTLS blockSignalsKey
     newThread <- newSCont $ do setTLS tidTLSKey (Just tid)
                                cPrint (show tid ++ " has set its TLS Key.\n")
                                -- Inherit exception-blocked status from parent, per the spec.
                                setTLS blockSignalsKey sigsblocked
                                checkSignals -- check for kill before run the first time.
                                computation `catchException` \ex -> do cPrint ("Uncaught exception in " ++ show tid ++ ": " ++ show ex ++ "\n")
                                                                       die
                                -- trappedRunH (from forkH) is what's magically catching my exns!
                                cPrint (show tid ++ " completed without incident.\n")
                                die
     weak <- mkWeakPtr tid Nothing
     atomically $ do schedule newThread
                     ws <- readPVar allThreads
                     writePVar allThreads (weak:ws)
     return tid
  where newThreadId :: IO ThreadId
        newThreadId = do tnum <- getNextThreadNum
                         tbox <- newPVarIO empty
                         prio <- newPVarIO C
                         return (TCB tnum tbox prio)
     
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
threadDelay usec = do switch $ \currThread -> do scheduleIn usec currThread
                                                 getNextThread
                      checkSignals


-- | Check if anyone has sent us an asynchronous exception.
checkSignals :: IO ()
checkSignals = do mtid <- mySafeThreadId
                  case mtid of
                    Nothing -> return ()
                    Just tid@(TCB _ tbox _) ->
                      do sigsblocked <- getTLS blockSignalsKey
                         unless sigsblocked $ do mx <- atomically $ do exns <- readPVar tbox
                                                                       case viewl exns of
                                                                         (e :< es) -> writePVar tbox es >> return (Just e)
                                                                         EmptyL    -> return Nothing
                                                 -- We need to throw -outside- of the atomically.
                                                 case mx of
                                                   Nothing -> return ()
                                                   Just (AsyncException ThreadKilled) -> cPrint (show tid ++ " is killing itself.\n") >> die
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
block computation = do saved <- getTLS blockSignalsKey
                       setTLS blockSignalsKey True
                       -- Catch synchronous (not asynchronous) exns so we unwind properly
                       x <- catchException (setTLS blockSignalsKey True >> computation)
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
unblock computation = do saved <- getTLS blockSignalsKey
                         checkSignals
                         -- Catch synchronous OR asynchronous exns so we unwind properly
                         x <- catchException (setTLS blockSignalsKey True >> computation)
                                             (\e -> setTLS blockSignalsKey saved >> throw e)
                         setTLS blockSignalsKey saved
                         return x

