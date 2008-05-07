{-# LANGUAGE MagicHash, ExistentialQuantification #-}
{- A simple Haskell-level implementation of PTM.
   Logging approach is similar to what is in current STM,
   as described in the paper
    Harris, Marlow, Peyton Jones, and Herlihy, "Composable Memory Transations", PPoPP'05

  Credits: Andrew Tolmach
           Kenny Graunke (House integration; bugfix)
-}
module LwConc.STM
( STM
, TVar
, atomically
, newTVar
, newTVarIO
, readTVar
, writeTVar
) where

import Prelude hiding (catch)
import Control.Exception
import Data.IORef
import Data.List(find)
import GHC.Prim(reallyUnsafePtrEquality#)
import Unsafe.Coerce
import Foreign.C.Types(CInt)

foreign import ccall unsafe allowHaskellInterrupts :: CInt -> IO ()
foreign import ccall unsafe disallowHaskellInterrupts :: IO CInt

newtype TVar a = TVar (IORef a)
  deriving Eq

data LogEntry = forall a . LogEntry (IORef a) a a  -- underlying-ref, expected-value, new-value

newtype STM a = STM (IORef [LogEntry] -> IO a)
unSTM (STM f) = f

instance Monad STM where
  return x = STM (\log -> return x)
  STM m >>= k = STM $ \r -> do x <- m r
                               unSTM (k x) r

atomically :: STM a -> IO a
atomically (STM m) =
  do r <- newIORef []
     a <- m r `catch` \ex -> do
            log <- readIORef r
            hsiStatus <- disallowHaskellInterrupts
            ok <- validate_log log
            allowHaskellInterrupts hsiStatus
            if ok
               then throw ex -- propagate exception out of atomically block
               else do -- exception may have been due to fact that we saw an
                       -- inconsistent state, so try again (cf. paper section 6.2)
                       writeIORef r []
                       atomically (STM m)
     log <- readIORef r
     hsiStatus <- disallowHaskellInterrupts
     ok <- validate_log log
     if ok
        then do commit_log log
                allowHaskellInterrupts hsiStatus
                return a
        else do allowHaskellInterrupts hsiStatus
                atomically (STM m)

{-# INLINE newTVar #-}
newTVar :: a -> STM (TVar a)
newTVar a = STM $ \r ->
  do ref <- newIORef a
     return (TVar ref)

{-# INLINE newTVarIO #-}
newTVarIO :: a -> IO (TVar a)
newTVarIO a = do ref <- newIORef a
                 return (TVar ref)

{-# INLINE writeTVar #-}
writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar ref) new = STM $ \r ->
  do log <- readIORef r
     case find_log_entry ref log of
       Nothing ->
         do current <- readIORef ref
            writeIORef r (LogEntry ref current new : log)
       Just (LogEntry _ expected _) ->
         writeIORef r (LogEntry (unsafeCoerce ref) expected (unsafeCoerce new) : (delete_log_entry ref log))
         --writeIORef r (LogEntry ref (unsafeCoerce expected) new : (delete_log_entry ref log))

{-# INLINE readTVar #-}
readTVar :: TVar a -> STM a
readTVar (TVar ref) = STM $ \r ->
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

