{-# OPTIONS_GHC -ffi #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

module LwConc.Substrate 
( PVar
, PTM
, unsafeIOToPTM
, newPVar
, newPVarIO
, readPVar
, writePVar
, atomically
, SCont
, newSCont
, switch
, TLSKey
, newTLSKey
, getTLS
, setTLS
) where

import GHC.Prim
import GHC.IOBase
import GHC.Exts
import Data.Typeable

import Data.IORef
import Foreign.C.Types(CInt)

foreign import ccall unsafe allowHaskellInterrupts :: CInt -> IO ()
foreign import ccall unsafe disallowHaskellInterrupts :: IO CInt

type PTM a = IO a
type PVar a = IORef a
atomically x = do hsiStatus <- disallowHaskellInterrupts
                  v <- x
                  allowHaskellInterrupts hsiStatus
                  return v
newPVar = newIORef
newPVarIO = newIORef
writePVar = writeIORef
readPVar = readIORef
unsafeIOToPTM m = m

{-
-- KAYDEN: Again, I'm uncertain if this is truly necessary or not.
--         Need to think about it.
-- |@IO@ version of 'newPVar'.  This is useful for creating top-level
-- 'PVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
{-# INLINE newPVarIO  #-}
newPVarIO :: a -> IO (PVar a)
newPVarIO val = IO $ \s1# ->
    case newTVar# val s1# of
         (# s2#, tvar# #) -> (# s2#, PVar tvar# #)

-- |Return the current value stored in a PVar
{-# INLINE readPVar #-}
readPVar :: PVar a -> PTM a
readPVar (PVar tvar#) = PTM $ \s# -> readTVar# tvar# s#

-- |Write the supplied value into a PVar
{-# INLINE writePVar #-}
writePVar :: PVar a -> a -> PTM ()
writePVar (PVar tvar#) val = PTM $ \s1# -> (# writeTVar# tvar# val s1#, () #)
-}



{-
-- * PTM/Primitive Transactional Memory
-- Currently, this is exactly the same as STM, minus blocking stuff (retry)
-- and exceptions.  Underneath, it relies on the STM primops and structures.
-- This could change in the future, but for now, it should work fine.

newtype PTM a = PTM (State# RealWorld -> (# State# RealWorld, a #))

-- TODO: I think we can do this without unPTM...rather than unwrapping the
--       function inside a PTM, applying, then rewrapping...just use...
--       application of sorts...(alternatively, pattern matching would work
--       fine in thenPTM, but would need a case for bindPTM.)
-- (though, STM does the same thing)
unPTM :: PTM a -> (State# RealWorld -> (# State# RealWorld, a #))
unPTM (PTM a) = a

-- INSTANCE_TYPEABLE1(PTM, ptmTc, "PTM")
ptmTc = mkTyCon "PTM"
instance Typeable1 PTM where { typeOf1 _ = mkTyConApp ptmTc [] }

instance  Functor PTM where
   fmap f x = x >>= (return . f)

instance  Monad PTM  where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    m >> k      = thenPTM m k
    return x    = returnPTM x
    m >>= k     = bindPTM m k

{-# INLINE bindPTM  #-}
bindPTM :: PTM a -> (a -> PTM b) -> PTM b
bindPTM (PTM m) k = PTM $ \s ->
  case m s of
    (# new_s, a #) -> unPTM (k a) new_s

{-# INLINE thenPTM  #-}
thenPTM :: PTM a -> PTM b -> PTM b
thenPTM (PTM m) k = PTM $ \s ->
  case m s of
    (# s', a #) -> unPTM k s'

{-# INLINE returnPTM  #-}
returnPTM :: a -> PTM a
returnPTM x = PTM (\s -> (# s, x #))

-- | Unsafely performs IO in the PTM monad.
unsafeIOToPTM :: IO a -> PTM a
unsafeIOToPTM (IO m) = PTM m

-- |Perform a series of PTM actions atomically.
--
-- You cannot use 'atomically' inside an 'unsafePerformIO' or 'unsafeInterleaveIO'. 
-- Any attempt to do so will result in a runtime error.  (Reason: allowing
-- this would effectively allow a transaction inside a transaction, depending
-- on exactly when the thunk is evaluated.)
--
-- However, see 'newPVarIO', which can be called inside 'unsafePerformIO',
-- and which allows top-level PVars to be allocated.

{-# INLINE atomically  #-}
atomically :: PTM a -> IO a
atomically (PTM m) = IO $ \s -> atomically# m s

-- |Exception handling within STM actions.
-- catchPTM :: PTM a -> (Exception -> PTM a) -> PTM a
-- catchPTM (PTM m) k = PTM $ \s -> catchPTM# m (\ex -> unPTM (k ex)) s

-- |Shared memory locations that support atomic memory transactions.
data PVar a = PVar (TVar# RealWorld a)

-- INSTANCE_TYPEABLE1(PVar,pvarTc,"PVar")
pvarTc = mkTyCon "PVar"
instance Typeable1 PVar where { typeOf1 _ = mkTyConApp pvarTc [] }

instance Eq (PVar a) where
        (PVar tvar1#) == (PVar tvar2#) = sameTVar# tvar1# tvar2#

-- |Create a new PVar holding a value supplied
{-# INLINE newPVar  #-}
newPVar :: a -> PTM (PVar a)
newPVar val = PTM $ \s1# ->
    case newTVar# val s1# of
         (# s2#, tvar# #) -> (# s2#, PVar tvar# #)

-- |@IO@ version of 'newPVar'.  This is useful for creating top-level
-- 'PVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
{-# INLINE newPVarIO  #-}
newPVarIO :: a -> IO (PVar a)
newPVarIO val = IO $ \s1# ->
    case newTVar# val s1# of
         (# s2#, tvar# #) -> (# s2#, PVar tvar# #)

-- |Return the current value stored in a PVar
{-# INLINE readPVar #-}
readPVar :: PVar a -> PTM a
readPVar (PVar tvar#) = PTM $ \s# -> readTVar# tvar# s#

-- |Write the supplied value into a PVar
{-# INLINE writePVar #-}
writePVar :: PVar a -> a -> PTM ()
writePVar (PVar tvar#) val = PTM $ \s1# -> (# writeTVar# tvar# val s1#, () #)
-}

-- * TLS/Thread Local State

data TLSKey a = TLSKey Int#

newTLSKey :: a -> IO (TLSKey a)
newTLSKey x = IO $ \s10# ->
  case newTLSKey# x s10# of 
    (#s20#, key #) -> (#s20#, TLSKey key #)

{-# INLINE getTLS #-}
getTLS :: TLSKey a -> PTM a
getTLS (TLSKey key) = IO {- PTM -} $ \s10# -> getTLS# key s10#

setTLS :: TLSKey a -> a -> IO ()
setTLS (TLSKey key) x = IO $ \s10# ->
  case setTLS# key x s10# of
    s20# -> (# s20#, () #)

-- * Stack Continuations
-- Continuations are one-shot and consist of the TSO along with (essentially)
-- a boolean flag to indicate whether the SCont is "used up", i.e. already
-- switched to.
data SContStatus = Used | Usable
data SCont = SCont TSO# SContStatus

{-# INLINE newSCont #-}
newSCont :: IO () -> IO SCont
newSCont x = IO $ \s -> case newSCont# x s of
                          (# s', tso #) -> (# s', SCont tso Usable #)

{-# INLINE getSCont #-}
getSCont :: PTM SCont
getSCont = IO {- PTM -} $ \s10 ->
  case getSCont# s10 of
     (# s20, tso #) -> (# s20, SCont tso Usable #)

{-# INLINE switch #-}
switch :: (SCont -> PTM SCont) -> IO ()
switch scheduler = do s2 <- atomically $ getSCont >>= scheduler
                      switchSCont s2

{-# INLINE switchSCont #-}
switchSCont :: SCont -> IO ()
switchSCont (SCont sc Usable) = IO $ \s -> 
   case (atomicSwitch# sc s) of s1 -> (# s1, () #)
switchSCont (SCont _ Used) = error "Attempted to switch to used-up SCont!"

