-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.QSem
-- Copyright   :  (c) The University of Glasgow 2001, Kenny Graunke 2009
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- Simple quantity semaphores.
--
-----------------------------------------------------------------------------

module Control.Concurrent.QSem
	( -- * Simple Quantity Semaphores
	  QSem,		-- abstract
	  newQSem,	-- :: Int  -> IO QSem
	  waitQSem,	-- :: QSem -> IO ()
	  signalQSem	-- :: QSem -> IO ()
	) where

import Control.Monad(unless)
import LwConc.PTM
import LwConc.Scheduler(getNextThread, schedule)
import LwConc.Threads

-- QSems implemented atop PTM.
-- These now wake up all waiting threads and let them compete for the resource.
-- This lets the scheduler decide who gets it next, preventing a large source
-- of priority inversion.  It may suffer some starvation.

-- |A 'QSem' is a simple quantity semaphore, in which the available
-- \"quantity\" is always dealt with in units of one.
newtype QSem = QSem (PVar (Int, [Thread]))

-- |Build a new 'QSem'
newQSem :: Int -> IO QSem
newQSem init = do
   sem <- newPVarIO (init,[])
   return (QSem sem)

-- |Wait for a unit to become available
waitQSem :: QSem -> IO ()
waitQSem q@(QSem pv) =
  do bucket <- newPVarIO False
     switchT $ \currThread -> do (avail,ts) <- readPVar pv
                                 if avail > 0
                                    then do writePVar pv (avail-1,[])
                                            writePVar bucket True
                                            return currThread
                                    else do writePVar pv (avail, currThread:ts)
                                            getNextThread
     gotIt <- atomically $ readPVar bucket
     unless gotIt $ waitQSem q

-- |Signal that a unit of the 'QSem' is available
signalQSem :: QSem -> IO ()
signalQSem (QSem pv) = atomically $ do (avail,blocked) <- readPVar pv
                                       writePVar pv (avail+1,[])
                                       mapM_ schedule blocked -- wake 'em all; duke it out; let the schedule decide

