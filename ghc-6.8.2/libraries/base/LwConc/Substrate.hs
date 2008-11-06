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

, ThreadId(..)
, tidTLSKey
, mySafeThreadId
) where

import GHC.Prim
import GHC.IOBase
import GHC.Exts
import Data.Typeable
import LwConc.STM

import Data.IORef
import Data.Sequence as Seq
import Foreign.C.Types(CUInt)

foreign import ccall unsafe showCornerNumber :: CUInt -> IO ()

debugShowTID = do mtid <- mySafeThreadId
                  showCornerNumber $ case mtid of
                                       Nothing -> 0
                                       Just (ThreadId tnum tbox) -> fromIntegral tnum

-----------------------------------------------------------------------------
-- Thread Local State (TLS)

data TLSKey a = TLSKey Int#

newTLSKey :: a -> IO (TLSKey a)
newTLSKey x = IO $ \s10# ->
  case newTLSKey# x s10# of 
    (#s20#, key #) -> (#s20#, TLSKey key #)

getTLS :: TLSKey a -> IO a
getTLS (TLSKey key) = IO $ \s -> case getTLS# key s of
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
switch :: (SCont -> STM SCont) -> IO ()
switch scheduler = do debugShowTID
                      s1 <- getSCont
                      s2 <- atomically (scheduler s1)
                      switchTo s2
                      debugShowTID
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

-----------------------------------------------------------------------------
-- ThreadIds

-- | Thread IDs are primarily a IORef containing signalling information.
-- However, we also assign each a monotonically increasing integer - mostly
-- for printing, but possibly for other uses in the future.  These start at 1.

data ThreadId = ThreadId Int (TVar (Seq Exception))

instance Eq ThreadId where
  (ThreadId xnum xbox) == (ThreadId ynum ybox) = xbox == ybox

instance Show ThreadId where
  show (ThreadId id box) = "<Thread " ++ show id ++ ">"

-- | We store each thread's ID in their TLS, allowing a simple implementation
-- of myThreadId.  However, threads must initialize their own TLS, and we may
-- get an interrupt before the thread's fully set up.  Hence, we use a Maybe
-- type for the TLS key.

tidTLSKey :: TLSKey (Maybe ThreadId)
tidTLSKey = unsafePerformIO $ newTLSKey Nothing

-- | A version of myThreadId that returns a Maybe type, and hence is safe to
-- call even from interrupt handling contexts.  Mostly useful for debugging.
mySafeThreadId :: IO (Maybe ThreadId)
mySafeThreadId = getTLS tidTLSKey

