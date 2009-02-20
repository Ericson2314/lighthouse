{-# OPTIONS_GHC -ffi #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

module LwConc.Substrate 
( SCont
, newSCont
, switch
, TLSKey
, newTLSKey
, getTLS
, setTLS
) where

import GHC.Exts
import GHC.Prim
import GHC.IOBase
import LwConc.PTM

import Data.IORef
import Data.Sequence as Seq

-----------------------------------------------------------------------------
-- Thread Local State (TLS)

data TLSKey a = TLSKey Int#

newTLSKey :: a -> IO (TLSKey a)
newTLSKey x = IO $ \s10# ->
  case newTLSKey# x s10# of 
    (#s20#, key #) -> (#s20#, TLSKey key #)

getTLS :: TLSKey a -> PTM a
getTLS (TLSKey key) = unsafeIOToPTM $ IO $ \s -> case getTLS# key s of
                                                   (# s', val #) -> (# s', val #)

setTLS :: TLSKey a -> a -> IO ()
setTLS (TLSKey key) x = IO $ \s10# ->
  case setTLS# key x s10# of
    s20# -> (# s20#, () #)

-----------------------------------------------------------------------------
-- Stack Continuations (SCont)
-- Continuations are one-shot and consist of the TSO along with (essentially)
-- a boolean flag to indicate whether the SCont is "used up", i.e. already
-- switched to.
data SContStatus = Used | Usable
data SCont = SCont TSO# (IORef SContStatus)

instance Eq SCont where
  (SCont _ xref) == (SCont _ yref) = xref == yref

{-# INLINE newSCont #-}
newSCont :: IO () -> IO SCont
newSCont x = do ref <- newIORef Usable
                IO $ \s -> case newSCont# x s of
                             (# s', tso #) -> (# s', SCont tso ref #)

{-# INLINE switch #-}
switch :: (SCont -> PTM SCont) -> IO ()
switch scheduler = do s1 <- getSCont
                      s2 <- atomically (scheduler s1)
                      switchTo s2
  where getSCont :: IO SCont
        getSCont = do ref <- newIORef Usable
                      IO $ \s -> case getTSO# s of (# s', tso #) -> (# s', SCont tso ref #)
        switchTo :: SCont -> IO ()
        switchTo (SCont tso ref) =
          do status <- readIORef ref
             case status of
               Used   -> error "Attempted to switch to used SCont!"
               Usable -> do writeIORef ref Used
                            IO $ \s -> case switch# tso s of s' -> (# s', () #)


