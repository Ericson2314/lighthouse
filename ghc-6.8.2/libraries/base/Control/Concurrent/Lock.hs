module Control.Concurrent.Lock
( Lock
, LockKey
, newLock
, lock
, unlock
, tryLock
, withLock
) where

import LwConc.Conc
import LwConc.MVar
import LwConc.Priority

import Control.Monad (when)

data Lock = Lock (TVar (Maybe ThreadId)) (MVar ())
newtype LockKey = LockKey (IO ())

-- Reasoning through:
--    L = Nothing  empty
-- A: ka <- lock L
--    L = (Just A) empty
-- B: kb <- lock L
--    L = (Just A) ()      ...B succeeds in putting () and immediately retries
--    L = (Just A) ()~B    ...B fails and blocks
-- C: kc <- lock L
--    L = (Just A) ()~B,C  ...C fails and blocks
-- A: unlock ka
--    L = Nothing  ()~B,C  ...A finished atomically
--    L = Nothing  ()~C    ...A tryTakeMVar (B awakened)
--    L = (Just B) ()~C    ...B succesfully locks (kb now valid)
-- B: unlock kb
--    L = Nothing  ()~C    ...B finished atomically
--    L = Nothing  ()      ...B tryTakeMVar (C awakened)
--    L = (Just C) ()      ...C succesfully locks (kb now valid)
-- From here we're okay...if we were okay up above.
--
-- Potential races during "A: unlock ka":
--
-- Case 1:
--    L = Nothing  ()~B,C  ...A finished atomically
-- D: kd <- lock L
--    L = (Just D) ()~B,C  ...D steals the lock before B gets woken up...everything OK.
--
-- Case 2:
--    L = Nothing  ()~B,C  ...A finished atomically
--    L = Nothing  ()~C    ...A tryTakeMVar (B awakened)
-- D: kd <- lock L
--    L = (Just D) ()~C    ...D steals the lock before B runs
--    L = (Just D) ()~C,B  ...B runs and blocks again. (interesting ordering property)

newLock :: IO Lock
newLock = do tv <- newTVarIO Nothing
             mv <- newEmptyMVar
             return (Lock tv mv)

lock :: Lock -> IO LockKey
lock l@(Lock tv mv) = do me <- myThreadId
                         atomically $ do owner <- readTVar tv
                                         case owner of
                                           Nothing  -> writeTVar tv (Just me)
                                           Just owner -> do myPrio <- getPriority me
                                                            ownerPrio <- getPriority owner
                                                            when (myPrio > ownerPrio) $ setPriority owner myPrio
                                                            retryButFirst (putMVar mv ()) -- See reasoning above
                                                            -- we could avoid the mvar entirely by using real "retry"
                         return $ LockKey (unsafeUnlock l)
                        
tryLock :: Lock -> IO (Maybe LockKey)
tryLock l@(Lock tv mv) = do me <- myThreadId
                            atomically $ do owner <- readTVar tv
                                            case owner of
                                              Nothing -> writeTVar tv (Just me) >> return (Just (LockKey (unsafeUnlock l)))
                                              Just _  -> return Nothing

unlock :: LockKey -> IO ()
unlock (LockKey f) = f

unsafeUnlock :: Lock -> IO ()
unsafeUnlock (Lock tv mv) = do atomically $ writeTVar tv Nothing
                               tryTakeMVar mv
                               return ()

withLock :: Lock -> IO a -> IO a
withLock l c = do k <- lock l
                  prio <- myPriority
                  v <- c
                  unlock k
                  setMyPriority prio -- in case someone boosted our priority while we held the lock...
                                     -- but this is craptastic if c alters the priority...
                  return v

{-
newtype Lock = Lock (MVar ThreadId)
newtype Key = Key (IO ())

newLock :: IO Lock
newLock = newEmptyMVar >>= Lock

tryLock :: Lock -> IO Bool
tryLock (Lock mv) = do tid <- myThreadId
                       tryPutMVar mv tid

-- hmm.  (random thought)  asynchronous exns...don't interact so well with STM.
-- notably...if doing a transaction...get an async exception...well log looks fine...
-- so retry...ignoring that.  oops.  really should only retry on -synchronous- exns.
-- should try and avoid infinite loops too...?

-- actually this key idea is a bit weird; anybody you pass the key to can unlock it.
lock :: Lock -> IO Key
lock (Lock mv) = do tid <- myThreadId
                    -- really want some kind of priority inheritance here
                    putMVar mv tid
                    return $ Key (unsafeUnlock lock)

unlock :: Key -> IO ()
unlock (Key f) = f

unsafeUnlock :: Lock -> IO ()
unsafeUnlock (Lock mv) = do tid <- myThreadId
                            h <- tryTakeMVar -- really don't want to do this...
                            when h /= Just tid $ -- oh crap, we just stole it from somebody else...shouldn't be possible...
                                                 -- or, it might not be locked at all...also craptastic

withLock :: Lock -> IO a -> IO a
withLock l c = do lock l
                  v <- c -- save/restore priority? though, craptastic if c alters priority...
                  unlock l
                  return v
-}

