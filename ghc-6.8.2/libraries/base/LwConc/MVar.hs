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
	--, modifyMVar_ 	-- :: MVar a -> (a -> IO a) -> IO ()
	--, modifyMVar 	-- :: MVar a -> (a -> IO (a,b)) -> IO b
	, addMVarFinalizer -- :: MVar a -> IO () -> IO ()
) where

import LwConc.Substrate
import LwConc.ConcLib
import Data.IORef
import Data.Sequence as Seq

newtype MVar a = MVar (PVar (MVState a)) deriving Eq -- Typeable, Data?

data MVState a = Full a (Seq (a, SCont))        -- queue of blocked writers
               | Empty  (Seq (IORef a, SCont))  -- queue of blocked readers

-- FIXME: We need an instance of Eq for MVar, or else Chans break...but...
-- I really don't want to do a structural comparison, that could be horrifically slow.
--instance Eq MVar where
  --(MVar m) == (MVar m) = sameMVar# mvar1# mvar2#

-- |Create an 'MVar' which is initially empty.
newEmptyMVar :: IO (MVar a)
newEmptyMVar = 
  do p <- newPVarIO (Empty empty)
     return $ MVar p

-- |Create an 'MVar' which contains the supplied value.
newMVar :: a -> IO (MVar a)
newMVar x =
  do p <- newPVarIO (Full x empty)
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
takeMVar (MVar p) =
  do hole <- newIORef undefined
     switch $ \currThread ->
       do st <- readPVar p
          case st of
            Full x bq -> case viewl bq of
                           EmptyL -> do writePVar p (Empty empty)
                                        unsafeIOToPTM $ writeIORef hole x
                                        return currThread
                           ((x',t) :< ts) -> do writePVar p (Full x' ts)
                                                unsafeIOToPTM $ writeIORef hole x -- put value in hole so we can return it at the end
                                                placeOnReadyQ currThread -- place us on the ready queue
                                                return t                 -- switch to the next blocked thread (could do the other way around)
            Empty bq -> do writePVar p (Empty (bq |> (hole, currThread))) -- block (queueing hole for answer)
                           fetchRunnableThread                            -- and run something else
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
putMVar (MVar p) x = switch $ \currThread ->
  do st <- readPVar p
     case st of
       Empty bq -> case viewl bq of
                     EmptyL -> do writePVar p (Full x empty) -- nobody waiting, just store value
                                  return currThread
                     ((hole,t) :< ts) -> do unsafeIOToPTM $ writeIORef hole x -- pass value through hole to blocked reader
                                            writePVar p (Empty ts)            -- unblock them
                                            placeOnReadyQ currThread          -- place us on the ready queue
                                            return t                          -- switch to them
       Full y bq -> do writePVar p (Full y (bq |> (x, currThread))) -- block (queueing value to write)
                       fetchRunnableThread                          -- and run something else

-- |A non-blocking version of 'takeMVar'.  The 'tryTakeMVar' function
-- returns immediately, with 'Nothing' if the 'MVar' was empty, or
-- @'Just' a@ if the 'MVar' was full with contents @a@.  After 'tryTakeMVar',
-- the 'MVar' is left empty.
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar p) = atomically $
  do st <- readPVar p
     case st of
       Full x bq -> do case viewl bq of
                         EmptyL -> writePVar p (Empty empty)
                         ((x',t) :< ts) -> do writePVar p (Full x' ts)
                                              placeOnReadyQ t
                       return (Just x)
       Empty bq -> return Nothing

-- |A non-blocking version of 'putMVar'.  The 'tryPutMVar' function
-- attempts to put the value @a@ into the 'MVar', returning 'True' if
-- it was successful, or 'False' otherwise.
tryPutMVar  :: MVar a -> a -> IO Bool
tryPutMVar (MVar p) x = atomically $
  do st <- readPVar p
     case st of
       Empty bq -> do case viewl bq of
                        EmptyL -> writePVar p (Full x empty) -- nobody waiting, just store value
                        ((hole,t) :< ts) -> do unsafeIOToPTM $ writeIORef hole x -- pass value through hole to blocked reader
                                               writePVar p (Empty ts)            -- unblock them
                                               placeOnReadyQ t                   -- place them on the ready queue
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
  do st <- readPVar p
     return $ case st of
                Empty _  -> True
                Full _ _ -> False

addMVarFinalizer :: MVar a -> IO () -> IO ()
addMVarFinalizer = undefined
