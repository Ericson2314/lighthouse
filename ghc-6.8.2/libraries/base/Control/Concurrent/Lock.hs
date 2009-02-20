-- Priority-safe locks:
-- 1. It implements priority inheritance (use withLock)
-- 2. When it's unlocked, all waiting threads wake up and compete for the lock.
--    This means if both high priority and low priority things are waiting, the
--    schedule gets to pick which one gets it (likely the high priority one).
--    Contrast this to MVars, which are FIFO, and thus make a static scheduling
--    decision without regarding priority nor consulting the scheduling policy.
module Control.Concurrent.Lock
( Lock
, LockKey
, newLock
, lock
, unlock
, tryLock
, withLock
) where

import Control.Concurrent(myThreadId)
import Control.Monad(when)
import LwConc.PTM
import LwConc.Scheduler(getNextThread, schedule)
import LwConc.Threads

data LockState = Unlocked | Locked ThreadId [Thread]
newtype Lock = Lock (PVar LockState)
newtype LockKey = LockKey (IO ())

newLock :: IO Lock
newLock = do tv <- newPVarIO Unlocked
             return (Lock tv)

lock :: Lock -> IO LockKey
lock l@(Lock pv) = do me <- myThreadId
                      switchT $ \currThread -> do state <- readPVar pv
                                                  case state of
                                                    Unlocked -> do writePVar pv (Locked me [])
                                                                   return currThread -- got it; continue
                                                    Locked owner ts -> do myPrio <- myPriority
                                                                          ownerPrio <- getPriority owner
                                                                          when (myPrio > ownerPrio) $ setPriority owner myPrio
                                                                          writePVar pv (Locked owner (currThread:ts))
                                                                          getNextThread -- run something else
                      return $ LockKey (unsafeUnlock l)
                        
tryLock :: Lock -> IO (Maybe LockKey)
tryLock l@(Lock pv) = do me <- myThreadId
                         atomically $ do state <- readPVar pv
                                         case state of
                                           Unlocked   -> writePVar pv (Locked me []) >> return (Just (LockKey (unsafeUnlock l)))
                                           Locked _ _ -> return Nothing

unlock :: LockKey -> IO ()
unlock (LockKey f) = f

unsafeUnlock :: Lock -> IO ()
unsafeUnlock (Lock pv) = atomically $ do state <- readPVar pv
                                         case state of
                                           Unlocked    -> error "tried to unlock, but it already was unlocked"
                                           Locked _ ts -> do writePVar pv Unlocked
                                                             mapM_ schedule ts -- wake them all up; compete

withLock :: Lock -> IO a -> IO a
withLock l c = do prio <- atomically myPriority
                  k <- lock l
                  v <- c
                  unlock k
                  atomically $ setMyPriority prio -- in case someone boosted our priority while we held the lock...
                                                  -- but this is craptastic if c alters the priority...
                  return v

