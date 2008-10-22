-----------------------------------------------------------------------------
-- |
-- Module      :  LwConc.MVar
-- Copyright   :  (c) The University of Glasgow, 1994-2002;
--                (c) Peng Li, 2007;
--                (c) Kenny Graunke, 2008
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  kennyg@cs.pdx.edu
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- MVar synchronizing variables, built atop the lightweight concurrency
-- framework.
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fglasgow-exts #-}
module LwConc.MVar
(
	  -- * @MVar@s
	  MVar		-- abstract
	, newEmptyMVar  -- :: IO (MVar a)
	, newMVar 	-- :: a -> IO (MVar a)
	, takeMVar 	-- :: MVar a -> IO a
	, putMVar  	-- :: MVar a -> a -> IO ()
	--, readMVar 	-- :: MVar a -> IO a
	--, swapMVar 	-- :: MVar a -> a -> IO a
	, tryTakeMVar   -- :: MVar a -> IO (Maybe a)
	, tryPutMVar    -- :: MVar a -> a -> IO Bool
	, isEmptyMVar	-- :: MVar a -> IO Bool
	--, withMVar	-- :: MVar a -> (a -> IO b) -> IO b
	--, modifyMVar_	-- :: MVar a -> (a -> IO a) -> IO ()
	--, modifyMVar 	-- :: MVar a -> (a -> IO (a,b)) -> IO b
	, addMVarFinalizer -- :: MVar a -> IO () -> IO ()

        , cleanBlocked
        , dumpBlocked
) where

import LwConc.Scheduler
import LwConc.STM
import LwConc.Substrate
import Data.IORef
import Data.Sequence as Seq
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak (Weak(..), mkWeak, deRefWeak)
import Foreign.C(CString, withCString)

foreign import ccall unsafe "start.h c_print" c_print :: CString -> IO ()

cPrint :: String -> IO ()
cPrint str = withCString str c_print

newtype MVar a = MVar (TVar (MVState a)) deriving Eq -- Typeable, Data?

data MVState a = Full a (Seq (a, SCont))       -- queue of blocked writers
               | Empty  (Seq (IORef a, SCont)) -- queue of blocked readers

-- |Blocked queue stuff.
blockedQ :: TVar [(ThreadId, Weak SCont)]
blockedQ = unsafePerformIO $ newTVarIO []

markBlocked :: MVar a -> SCont -> STM ()
markBlocked mv scont = do weak <- unsafeIOToSTM $ mkWeak mv scont Nothing
                          Just tid <- unsafeIOToSTM mySafeThreadId
                          q <- readTVar blockedQ
                          writeTVar blockedQ ((tid, weak):q)

markUnblocked :: SCont -> STM ()
markUnblocked scont = do q <- readTVar blockedQ
                         q' <- unsafeIOToSTM $ deleteW scont q
                         writeTVar blockedQ q'
  where deleteW :: SCont -> [(ThreadId, Weak SCont)] -> IO [(ThreadId, Weak SCont)]
        deleteW _ []     = return []
        deleteW x ((t,w):ws) =
          do m <- deRefWeak w
             case m of
               Nothing -> do ws' <- deleteW x ws
                             return ((t,w):ws') -- should probably not cons it back on here - clean as we go
               Just y -> if x == y
                            --then cPrint ("Unblocking " ++ show t) >> return ws
                            then return ws
                            else do ws' <- deleteW x ws
                                    return ((t,w):ws')

cleanBlocked :: IO ()
cleanBlocked = atomically $
  do q <- readTVar blockedQ
     q' <- unsafeIOToSTM $ filterCollected q
     writeTVar blockedQ q'
  where filterCollected :: [(ThreadId, Weak a)] -> IO [(ThreadId, Weak a)]
        filterCollected [] = return []
        filterCollected ((t,w):ws) = do m <- deRefWeak w
                                        case m of
                                          Nothing -> filterCollected ws
                                          Just _  -> do ws' <- filterCollected ws
                                                        return ((t,w):ws')

dumpBlocked :: IO ()
dumpBlocked = atomically $ do q <- readTVar blockedQ
                              unsafeIOToSTM $ do cPrint "\nBlocked thread status"
                                                 cPrint "\n=====================\n"
                                                 mapM_ dumpQ q
                                                 cPrint   "=====================\n"
  where dumpQ (t,w) = do m <- deRefWeak w
                         case m of
                           Nothing -> cPrint (show t ++ " - blocked on dead MVar.\n")
                           Just sc -> cPrint (show t ++ " - alive.\n")

-- |Create an 'MVar' which is initially empty.
newEmptyMVar :: IO (MVar a)
newEmptyMVar = 
  do p <- newTVarIO (Empty empty)
     return $ MVar p

-- |Create an 'MVar' which contains the supplied value.
newMVar :: a -> IO (MVar a)
newMVar x =
  do p <- newTVarIO (Full x empty)
     return $ MVar p

-- |Return the contents of the 'MVar'.  If the 'MVar' is currently
-- empty, 'takeMVar' will wait until it is full.  After a 'takeMVar',
-- the 'MVar' is left empty.
--
-- There are two further important properties of 'takeMVar':
--
--   * 'takeMVar' is single-wakeup.  That is, if there are multiple
--     threads blocked in 'takeMVar', and the 'MVar' becomes full,
--     only one thread will be woken up.  The function guarantees that
--     the woken thread completes its 'takeMVar' operation.
--
--   * When multiple threads are blocked on an 'MVar', they are
--     woken up in FIFO order.  This is useful for providing
--     fairness properties of abstractions built using 'MVar's.
--
takeMVar :: MVar a -> IO a
takeMVar mv@(MVar p) =
  do hole <- newIORef undefined
     switch $ \currThread ->
       do st <- readTVar p
          case st of
            Full x bq -> case viewl bq of
                           EmptyL -> do writeTVar p (Empty empty)
                                        unsafeIOToSTM $ writeIORef hole x
                                        return currThread
                           ((x',t) :< ts) -> do writeTVar p (Full x' ts)
                                                unsafeIOToSTM $ writeIORef hole x -- put value in hole so we can return it at the end
                                                markUnblocked t
                                                schedule t -- wake them up
                                                return currThread -- continue
            Empty bq -> do writeTVar p (Empty (bq |> (hole, currThread))) -- block (queueing hole for answer)
                           markBlocked mv currThread
                           getNextThread                                  -- and run something else
     readIORef hole

-- |Put a value into an 'MVar'.  If the 'MVar' is currently full,
-- 'putMVar' will wait until it becomes empty.
--
-- There are two further important properties of 'putMVar':
--
--   * 'putMVar' is single-wakeup.  That is, if there are multiple
--     threads blocked in 'putMVar', and the 'MVar' becomes empty,
--     only one thread will be woken up.  The function guarantees that
--     the woken thread completes its 'putMVar' operation.
--
--   * When multiple threads are blocked on an 'MVar', they are
--     woken up in FIFO order.  This is useful for providing
--     fairness properties of abstractions built using 'MVar's.
--
{-# INLINE putMVar #-}
putMVar :: MVar a -> a -> IO ()
putMVar mv@(MVar p) x = switch $ \currThread ->
  do st <- readTVar p
     case st of
       Empty bq -> case viewl bq of
                     EmptyL -> do writeTVar p (Full x empty) -- nobody waiting, just store value
                                  return currThread
                     ((hole,t) :< ts) -> do unsafeIOToSTM $ writeIORef hole x -- pass value through hole to blocked reader
                                            writeTVar p (Empty ts)            -- take them off the blocked queue
                                            markUnblocked t
                                            schedule t                        -- put them back on the ready queue
                                            return currThread
       Full y bq -> do writeTVar p (Full y (bq |> (x, currThread))) -- block (queueing value to write)
                       markBlocked mv currThread
                       getNextThread                                -- and run something else

-- |A non-blocking version of 'takeMVar'.  The 'tryTakeMVar' function
-- returns immediately, with 'Nothing' if the 'MVar' was empty, or
-- @'Just' a@ if the 'MVar' was full with contents @a@.  After 'tryTakeMVar',
-- the 'MVar' is left empty.
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar p) = atomically $
  do st <- readTVar p
     case st of
       Full x bq -> do case viewl bq of
                         EmptyL -> writeTVar p (Empty empty)
                         ((x',t) :< ts) -> do writeTVar p (Full x' ts)
                                              markUnblocked t
                                              schedule t
                       return (Just x)
       Empty bq -> return Nothing

-- |A non-blocking version of 'putMVar'.  The 'tryPutMVar' function
-- attempts to put the value @a@ into the 'MVar', returning 'True' if
-- it was successful, or 'False' otherwise.
tryPutMVar  :: MVar a -> a -> IO Bool
tryPutMVar (MVar p) x = atomically $
  do st <- readTVar p
     case st of
       Empty bq -> do case viewl bq of
                        EmptyL -> writeTVar p (Full x empty) -- nobody waiting, just store value
                        ((hole,t) :< ts) -> do unsafeIOToSTM $ writeIORef hole x -- pass value through hole to blocked reader
                                               writeTVar p (Empty ts)            -- take them off the blocked queue
                                               markUnblocked t
                                               schedule t                        -- place them on the ready queue
                      return True
       Full y bq -> return False

-- |Check whether a given 'MVar' is empty.
--
-- Notice that the boolean value returned  is just a snapshot of
-- the state of the MVar. By the time you get to react on its result,
-- the MVar may have been filled (or emptied) - so be extremely
-- careful when using this operation.   Use 'tryTakeMVar' instead if possible.
isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar (MVar p) = atomically $
  do st <- readTVar p
     return $ case st of
                Empty _  -> True
                Full _ _ -> False

addMVarFinalizer :: MVar a -> IO () -> IO ()
addMVarFinalizer = undefined
