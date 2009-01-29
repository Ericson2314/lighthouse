{-# LANGUAGE MagicHash, ExistentialQuantification #-}
{- A simple Haskell-level implementation of PTM.
   Logging approach is similar to what is in current STM,
   as described in the paper
    Harris, Marlow, Peyton Jones, and Herlihy, "Composable Memory Transations", PPoPP'05

  Credits: Andrew Tolmach
           Kenny Graunke (House integration; bugfix)
-}
module LwConc.PTM
( PTM
, PVar
, atomically
, newPVar
, newPVarIO
, readPVar
, writePVar
, unsafeIOToPTM
, retryButFirst
, disableHsIRQs
, restoreHsIRQs
) where

import GHC.Exception(Exception(..), catchException, throw)
import Data.IORef
import Data.List(find)
import GHC.Prim(reallyUnsafePtrEquality#, disableHsIRQs#, restoreHsIRQs#)
import GHC.IOBase(IO(..))
import Unsafe.Coerce

disableHsIRQs :: IO Bool
disableHsIRQs = IO $ \s ->
  case disableHsIRQs# s of
    (# s', 0# #) -> (# s', False #)
    (# s', _  #) -> (# s', True  #)

restoreHsIRQs :: Bool -> IO ()
restoreHsIRQs hsiStatus = IO $ \s -> (# restoreHsIRQs# (if hsiStatus then 1# else 0#) s, () #)

newtype PVar a = PVar (IORef a)
  deriving Eq

data LogEntry = forall a . LogEntry (IORef a) a a  -- underlying-ref, expected-value, new-value

newtype PTM a = PTM (IORef [LogEntry] -> IO a)
unPTM (PTM f) = f

instance Monad PTM where
  return x = PTM (\log -> return x)
  PTM m >>= k = PTM $ \r -> do x <- m r
                               unPTM (k x) r

atomically :: PTM a -> IO a
atomically (PTM m) =
  do r <- newIORef []
     a <- m r `catchException` \ex -> do
            log <- readIORef r
            hsiStatus <- disableHsIRQs
            ok <- validate_log log
            restoreHsIRQs hsiStatus
            if ok
               then throw ex -- propagate exception out of atomically block
               else do -- exception may have been due to fact that we saw an
                       -- inconsistent state, so try again (cf. paper section 6.2)
                       writeIORef r []
                       atomically (PTM m)
     log <- readIORef r
     hsiStatus <- disableHsIRQs
     ok <- validate_log log
     if ok
        then do commit_log log
                restoreHsIRQs hsiStatus
                return a
        else do restoreHsIRQs hsiStatus
                atomically (PTM m)

-- |Abort the current transaction, call the supplied function, then retry.
-- Could be used to implement real STM retry.
retryButFirst :: IO () -> PTM a
retryButFirst f = PTM $ \r -> do log <- readIORef r
                                 retryLoop r log
  where retryLoop :: IORef [LogEntry] -> [LogEntry] -> IO a
        retryLoop r log = do ok <- validate_log log -- don't bother to disable interrupts...we -want- it to become invalid.
                             if ok
                                then do f
                                        retryLoop r log
                                else do ref <- newIORef 1337
                                        writeIORef r [LogEntry ref 0 42]
                                        throw (AssertionFailed "retry called")

{-# INLINE unsafeIOToPTM #-}
unsafeIOToPTM :: IO a -> PTM a
unsafeIOToPTM m = PTM $ \r -> m >>= return

{-# INLINE newPVar #-}
newPVar :: a -> PTM (PVar a)
newPVar a = PTM $ \r ->
  do ref <- newIORef a
     return (PVar ref)

{-# INLINE newPVarIO #-}
newPVarIO :: a -> IO (PVar a)
newPVarIO a = do ref <- newIORef a
                 return (PVar ref)

{-# INLINE writePVar #-}
writePVar :: PVar a -> a -> PTM ()
writePVar (PVar ref) new = PTM $ \r ->
  do log <- readIORef r
     case find_log_entry ref log of
       Nothing ->
         do current <- readIORef ref
            writeIORef r (LogEntry ref current new : log)
       Just (LogEntry _ expected _) ->
         writeIORef r (LogEntry (unsafeCoerce ref) expected (unsafeCoerce new) : (delete_log_entry ref log))

{-# INLINE readPVar #-}
readPVar :: PVar a -> PTM a
readPVar (PVar ref) = PTM $ \r ->
  do log <- readIORef r
     case find_log_entry ref log of
       Nothing ->
         do current <- readIORef ref
            writeIORef r (LogEntry ref current current : log)
            return current
       Just (LogEntry _ expected new) ->
         return (unsafeCoerce new)

{-# INLINE find_log_entry #-}
find_log_entry :: IORef a -> [LogEntry] -> Maybe LogEntry
find_log_entry ref = find (\entry@(LogEntry ref' _ _) -> ref == unsafeCoerce ref')

{-# INLINE delete_log_entry #-}
delete_log_entry :: IORef a -> [LogEntry] -> [LogEntry]
delete_log_entry _   [] = []
delete_log_entry ref (entry@(LogEntry ref' _ _):rest) =
  if ref == unsafeCoerce ref'
     then rest
     else entry:(delete_log_entry ref rest)

{-# INLINE validate_log #-}
validate_log :: [LogEntry] -> IO Bool
validate_log [] = return True
validate_log (LogEntry ref expected _:rest) =
  do current <- readIORef ref
     if current =@= expected
        then validate_log rest
        else return False

{-# INLINE (=@=) #-}
(=@=) :: a -> a -> Bool
v1 =@= v2 =
  case reallyUnsafePtrEquality# v1 v2 of
    0# -> False
    1# -> True

{-# INLINE commit_log #-}
commit_log :: [LogEntry] -> IO ()
commit_log = mapM_ (\ (LogEntry ref _ new) -> writeIORef ref new)

