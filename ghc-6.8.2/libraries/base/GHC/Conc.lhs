\begin{code}
{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Conc
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic concurrency stuff.
-- 
-----------------------------------------------------------------------------

-- No: #hide, because bits of this module are exposed by the stm package.
-- However, we don't want this module to be the home location for the
-- bits it exports, we'd rather have Control.Concurrent and the other
-- higher level modules be the home.  Hence:

#include "Typeable.h"

-- #not-home
module GHC.Conc
	( ThreadId(..)

	-- * Forking and suchlike
	, forkOnIO	-- :: Int -> IO a -> IO ThreadId
        , numCapabilities -- :: Int
	, childHandler  -- :: Exception -> IO ()
	, myThreadId 	-- :: IO ThreadId
	, killThread	-- :: ThreadId -> IO ()
	, throwTo       -- :: ThreadId -> Exception -> IO ()
	, par  		-- :: a -> b -> b
	, pseq 		-- :: a -> b -> b
	, labelThread	-- :: ThreadId -> String -> IO ()

	-- * Waiting
	, threadDelay	  	-- :: Int -> IO ()
#ifndef house_HOST_OS
	, registerDelay		-- :: Int -> IO (TVar Bool)
	, threadWaitRead	-- :: Int -> IO ()
	, threadWaitWrite	-- :: Int -> IO ()
#endif

   	-- * TVars
	, STM           -- abstract
	, atomically    -- :: STM a -> IO a
	, retry         -- :: STM a
	, orElse        -- :: STM a -> STM a -> STM a
        , catchSTM      -- :: STM a -> (Exception -> STM a) -> STM a
	, alwaysSucceeds -- :: STM a -> STM ()
	, always        -- :: STM Bool -> STM ()
	, TVar          -- abstract
	, newTVar 	-- :: a -> STM (TVar a)
	, newTVarIO 	-- :: a -> STM (TVar a)
	, readTVar	-- :: TVar a -> STM a
	, writeTVar	-- :: a -> TVar a -> STM ()
	, unsafeIOToSTM	-- :: IO a -> STM a

   	-- * Miscellaneous
#ifdef mingw32_HOST_OS
	, asyncRead     -- :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
	, asyncWrite    -- :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
	, asyncDoProc   -- :: FunPtr (Ptr a -> IO Int) -> Ptr a -> IO Int

	, asyncReadBA   -- :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int, Int)
	, asyncWriteBA  -- :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int, Int)
#endif

#if !defined(mingw32_HOST_OS) && !defined(house_HOST_OS)
        , signalHandlerLock
#endif

	, ensureIOManagerIsRunning

#ifdef mingw32_HOST_OS
        , ConsoleEvent(..)
        , win32ConsoleHandler
        , toWin32ConsoleEvent
#endif
        ) where

import Prelude(undefined)

import System.Posix.Types
#if !defined(mingw32_HOST_OS) && !defined(house_HOST_OS)
import System.Posix.Internals
#endif
import Foreign
import Foreign.C

#ifndef __HADDOCK__
import {-# SOURCE #-} GHC.TopHandler ( reportError, reportStackOverflow )
#endif

import Data.Maybe

import GHC.Base
import GHC.IOBase
import GHC.Num		( Num(..) )
import GHC.Real		( fromIntegral, div )
#ifndef mingw32_HOST_OS
import GHC.Base		( Int(..) )
#endif
#ifdef mingw32_HOST_OS
import GHC.Read         ( Read )
import GHC.Enum         ( Enum )
#endif
import GHC.Exception
import GHC.Pack		( packCString# )
import GHC.Ptr          ( Ptr(..), plusPtr, FunPtr(..) )
import GHC.STRef
import GHC.Show		( Show(..), showString )
import Data.Typeable

infixr 0 `par`, `pseq`
\end{code}

%************************************************************************
%*									*
\subsection{@ThreadId@, @par@, and @fork@}
%*									*
%************************************************************************

\begin{code}
data ThreadId = ThreadId ThreadId# deriving( Typeable )
-- ToDo: data ThreadId = ThreadId (Weak ThreadId#)
-- But since ThreadId# is unlifted, the Weak type must use open
-- type variables.
{- ^
A 'ThreadId' is an abstract type representing a handle to a thread.
'ThreadId' is an instance of 'Eq', 'Ord' and 'Show', where
the 'Ord' instance implements an arbitrary total ordering over
'ThreadId's. The 'Show' instance lets you convert an arbitrary-valued
'ThreadId' to string form; showing a 'ThreadId' value is occasionally
useful when debugging or diagnosing the behaviour of a concurrent
program.

/Note/: in GHC, if you have a 'ThreadId', you essentially have
a pointer to the thread itself.  This means the thread itself can\'t be
garbage collected until you drop the 'ThreadId'.
This misfeature will hopefully be corrected at a later date.

/Note/: Hugs does not provide any operations on other threads;
it defines 'ThreadId' as a synonym for ().
-}

instance Show ThreadId where
   showsPrec d t = 
   	showString "ThreadId " . 
        showsPrec d (getThreadId (id2TSO t))

foreign import ccall unsafe "rts_getThreadId" getThreadId :: ThreadId# -> CInt

id2TSO :: ThreadId -> ThreadId#
id2TSO (ThreadId t) = t

foreign import ccall unsafe "cmp_thread" cmp_thread :: ThreadId# -> ThreadId# -> CInt
-- Returns -1, 0, 1

cmpThread :: ThreadId -> ThreadId -> Ordering
cmpThread t1 t2 = 
   case cmp_thread (id2TSO t1) (id2TSO t2) of
      -1 -> LT
      0  -> EQ
      _  -> GT -- must be 1

instance Eq ThreadId where
   t1 == t2 = 
      case t1 `cmpThread` t2 of
         EQ -> True
         _  -> False

instance Ord ThreadId where
   compare = cmpThread

{- |
Like 'forkIO', but lets you specify on which CPU the thread is
created.  Unlike a `forkIO` thread, a thread created by `forkOnIO`
will stay on the same CPU for its entire lifetime (`forkIO` threads
can migrate between CPUs according to the scheduling policy).
`forkOnIO` is useful for overriding the scheduling policy when you
know in advance how best to distribute the threads.

The `Int` argument specifies the CPU number; it is interpreted modulo
'numCapabilities' (note that it actually specifies a capability number
rather than a CPU number, but to a first approximation the two are
equivalent).
-}
forkOnIO :: Int -> IO () -> IO ThreadId
forkOnIO (I# cpu) action = IO $ \ s -> 
   case (forkOn# cpu action_plus s) of (# s1, id #) -> (# s1, ThreadId id #)
 where
  action_plus = catchException action childHandler

-- | the value passed to the @+RTS -N@ flag.  This is the number of
-- Haskell threads that can run truly simultaneously at any given
-- time, and is typically set to the number of physical CPU cores on
-- the machine.
numCapabilities :: Int
numCapabilities = unsafePerformIO $  do 
                    n <- peek n_capabilities
                    return (fromIntegral n)

foreign import ccall "&n_capabilities" n_capabilities :: Ptr CInt

childHandler :: Exception -> IO ()
childHandler err = catchException (real_handler err) childHandler

real_handler :: Exception -> IO ()
real_handler ex =
  case ex of
	-- ignore thread GC and killThread exceptions:
	BlockedOnDeadMVar            -> return ()
	BlockedIndefinitely          -> return ()
	AsyncException ThreadKilled  -> return ()

	-- report all others:
	AsyncException StackOverflow -> reportStackOverflow
	other       -> reportError other

{- | 'killThread' terminates the given thread (GHC only).
Any work already done by the thread isn\'t
lost: the computation is suspended until required by another thread.
The memory used by the thread will be garbage collected if it isn\'t
referenced from anywhere.  The 'killThread' function is defined in
terms of 'throwTo':

> killThread tid = throwTo tid (AsyncException ThreadKilled)

-}
killThread :: ThreadId -> IO ()
killThread tid = throwTo tid (AsyncException ThreadKilled)

{- | 'throwTo' raises an arbitrary exception in the target thread (GHC only).

'throwTo' does not return until the exception has been raised in the
target thread. 
The calling thread can thus be certain that the target
thread has received the exception.  This is a useful property to know
when dealing with race conditions: eg. if there are two threads that
can kill each other, it is guaranteed that only one of the threads
will get to kill the other.

If the target thread is currently making a foreign call, then the
exception will not be raised (and hence 'throwTo' will not return)
until the call has completed.  This is the case regardless of whether
the call is inside a 'block' or not.

Important note: the behaviour of 'throwTo' differs from that described in
the paper \"Asynchronous exceptions in Haskell\"
(<http://research.microsoft.com/~simonpj/Papers/asynch-exns.htm>).
In the paper, 'throwTo' is non-blocking; but the library implementation adopts
a more synchronous design in which 'throwTo' does not return until the exception
is received by the target thread.  The trade-off is discussed in Section 8 of the paper.
Like any blocking operation, 'throwTo' is therefore interruptible (see Section 4.3 of
the paper).

There is currently no guarantee that the exception delivered by 'throwTo' will be
delivered at the first possible opportunity.  In particular, if a thread may 
unblock and then re-block exceptions (using 'unblock' and 'block') without receiving
a pending 'throwTo'.  This is arguably undesirable behaviour.

 -}
throwTo :: ThreadId -> Exception -> IO ()
throwTo (ThreadId id) ex = IO $ \ s ->
   case (killThread# id ex s) of s1 -> (# s1, () #)

-- | Returns the 'ThreadId' of the calling thread (GHC only).
myThreadId :: IO ThreadId
myThreadId = IO $ \s ->
   case (myThreadId# s) of (# s1, id #) -> (# s1, ThreadId id #)

{- | 'labelThread' stores a string as identifier for this thread if
you built a RTS with debugging support. This identifier will be used in
the debugging output to make distinction of different threads easier
(otherwise you only have the thread state object\'s address in the heap).

Other applications like the graphical Concurrent Haskell Debugger
(<http://www.informatik.uni-kiel.de/~fhu/chd/>) may choose to overload
'labelThread' for their purposes as well.
-}

labelThread :: ThreadId -> String -> IO ()
labelThread (ThreadId t) str = IO $ \ s ->
   let ps  = packCString# str
       adr = byteArrayContents# ps in
     case (labelThread# t adr s) of s1 -> (# s1, () #)

-- 	Nota Bene: 'pseq' used to be 'seq'
--		   but 'seq' is now defined in PrelGHC
--
-- "pseq" is defined a bit weirdly (see below)
--
-- The reason for the strange "lazy" call is that
-- it fools the compiler into thinking that pseq  and par are non-strict in
-- their second argument (even if it inlines pseq at the call site).
-- If it thinks pseq is strict in "y", then it often evaluates
-- "y" before "x", which is totally wrong.  

{-# INLINE pseq  #-}
pseq :: a -> b -> b
pseq  x y = x `seq` lazy y

{-# INLINE par  #-}
par :: a -> b -> b
par  x y = case (par# x) of { _ -> lazy y }
\end{code}


%************************************************************************
%*									*
\subsection[stm]{Transactional heap operations}
%*									*
%************************************************************************

TVars are shared memory locations which support atomic memory
transactions.

\begin{code}
-- |A monad supporting atomic memory transactions.
newtype STM a = STM (State# RealWorld -> (# State# RealWorld, a #))

unSTM :: STM a -> (State# RealWorld -> (# State# RealWorld, a #))
unSTM (STM a) = a

INSTANCE_TYPEABLE1(STM,stmTc,"STM")

instance  Functor STM where
   fmap f x = x >>= (return . f)

instance  Monad STM  where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    m >> k      = thenSTM m k
    return x	= returnSTM x
    m >>= k     = bindSTM m k

bindSTM :: STM a -> (a -> STM b) -> STM b
bindSTM (STM m) k = STM ( \s ->
  case m s of 
    (# new_s, a #) -> unSTM (k a) new_s
  )

thenSTM :: STM a -> STM b -> STM b
thenSTM (STM m) k = STM ( \s ->
  case m s of 
    (# new_s, a #) -> unSTM k new_s
  )

returnSTM :: a -> STM a
returnSTM x = STM (\s -> (# s, x #))

-- | Unsafely performs IO in the STM monad.
unsafeIOToSTM :: IO a -> STM a
unsafeIOToSTM (IO m) = STM m

-- |Perform a series of STM actions atomically.
--
-- You cannot use 'atomically' inside an 'unsafePerformIO' or 'unsafeInterleaveIO'. 
-- Any attempt to do so will result in a runtime error.  (Reason: allowing
-- this would effectively allow a transaction inside a transaction, depending
-- on exactly when the thunk is evaluated.)
--
-- However, see 'newTVarIO', which can be called inside 'unsafePerformIO',
-- and which allows top-level TVars to be allocated.

atomically :: STM a -> IO a
atomically (STM m) = IO (\s -> (atomically# m) s )

-- |Retry execution of the current memory transaction because it has seen
-- values in TVars which mean that it should not continue (e.g. the TVars
-- represent a shared buffer that is now empty).  The implementation may
-- block the thread until one of the TVars that it has read from has been
-- udpated. (GHC only)
retry :: STM a
retry = undefined

-- |Compose two alternative STM actions (GHC only).  If the first action
-- completes without retrying then it forms the result of the orElse.
-- Otherwise, if the first action retries, then the second action is
-- tried in its place.  If both actions retry then the orElse as a
-- whole retries.
orElse :: STM a -> STM a -> STM a
orElse = undefined

-- |Exception handling within STM actions.
catchSTM :: STM a -> (Exception -> STM a) -> STM a
catchSTM (STM m) k = STM $ \s -> catchSTM# m (\ex -> unSTM (k ex)) s

-- | Low-level primitive on which always and alwaysSucceeds are built.
-- checkInv differs form these in that (i) the invariant is not 
-- checked when checkInv is called, only at the end of this and
-- subsequent transcations, (ii) the invariant failure is indicated
-- by raising an exception.
checkInv :: STM a -> STM ()
checkInv (STM m) = STM (\s -> (check# m) s)

-- | alwaysSucceeds adds a new invariant that must be true when passed
-- to alwaysSucceeds, at the end of the current transaction, and at
-- the end of every subsequent transaction.  If it fails at any
-- of those points then the transaction violating it is aborted
-- and the exception raised by the invariant is propagated.
alwaysSucceeds :: STM a -> STM ()
alwaysSucceeds i = do ( do i ; retry ) `orElse` ( return () ) 
                      checkInv i

-- | always is a variant of alwaysSucceeds in which the invariant is
-- expressed as an STM Bool action that must return True.  Returning
-- False or raising an exception are both treated as invariant failures.
always :: STM Bool -> STM ()
always i = alwaysSucceeds ( do v <- i
                               if (v) then return () else ( error "Transacional invariant violation" ) )

-- |Shared memory locations that support atomic memory transactions.
data TVar a = TVar (TVar# RealWorld a)

INSTANCE_TYPEABLE1(TVar,tvarTc,"TVar")

instance Eq (TVar a) where
	(TVar tvar1#) == (TVar tvar2#) = sameTVar# tvar1# tvar2#

-- |Create a new TVar holding a value supplied
newTVar :: a -> STM (TVar a)
newTVar val = STM $ \s1# ->
    case newTVar# val s1# of
	 (# s2#, tvar# #) -> (# s2#, TVar tvar# #)

-- |@IO@ version of 'newTVar'.  This is useful for creating top-level
-- 'TVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTVarIO :: a -> IO (TVar a)
newTVarIO val = IO $ \s1# ->
    case newTVar# val s1# of
	 (# s2#, tvar# #) -> (# s2#, TVar tvar# #)

-- |Return the current value stored in a TVar
readTVar :: TVar a -> STM a
readTVar (TVar tvar#) = STM $ \s# -> readTVar# tvar# s#

-- |Write the supplied value into a TVar
writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar tvar#) val = STM $ \s1# ->
    case writeTVar# tvar# val s1# of
    	 s2# -> (# s2#, () #)
  
\end{code}

%************************************************************************
%*									*
\subsection{Thread waiting}
%*									*
%************************************************************************

\begin{code}
#ifdef mingw32_HOST_OS

-- Note: threadDelay, threadWaitRead and threadWaitWrite aren't really functional
-- on Win32, but left in there because lib code (still) uses them (the manner
-- in which they're used doesn't cause problems on a Win32 platform though.)

asyncRead :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
asyncRead  (I# fd) (I# isSock) (I# len) (Ptr buf) =
  IO $ \s -> case asyncRead# fd isSock len buf s of 
  	       (# s, len#, err# #) -> (# s, (I# len#, I# err#) #)

asyncWrite :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
asyncWrite  (I# fd) (I# isSock) (I# len) (Ptr buf) =
  IO $ \s -> case asyncWrite# fd isSock len buf s of 
  	       (# s, len#, err# #) -> (# s, (I# len#, I# err#) #)

asyncDoProc :: FunPtr (Ptr a -> IO Int) -> Ptr a -> IO Int
asyncDoProc (FunPtr proc) (Ptr param) = 
    -- the 'length' value is ignored; simplifies implementation of
    -- the async*# primops to have them all return the same result.
  IO $ \s -> case asyncDoProc# proc param s  of 
  	       (# s, len#, err# #) -> (# s, I# err# #)

-- to aid the use of these primops by the IO Handle implementation,
-- provide the following convenience funs:

-- this better be a pinned byte array!
asyncReadBA :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int,Int)
asyncReadBA fd isSock len off bufB = 
  asyncRead fd isSock len ((Ptr (byteArrayContents# (unsafeCoerce# bufB))) `plusPtr` off)
  
asyncWriteBA :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int,Int)
asyncWriteBA fd isSock len off bufB = 
  asyncWrite fd isSock len ((Ptr (byteArrayContents# (unsafeCoerce# bufB))) `plusPtr` off)

#endif

-- -----------------------------------------------------------------------------
-- Thread IO API

#ifndef house_HOST_OS
-- | Block the current thread until data is available to read on the
-- given file descriptor (GHC only).
threadWaitRead :: Fd -> IO ()
threadWaitRead fd
#ifndef mingw32_HOST_OS
  | threaded  = waitForReadEvent fd
#endif
  | otherwise = IO $ \s -> 
	case fromIntegral fd of { I# fd# ->
	case waitRead# fd# s of { s -> (# s, () #)
	}}

-- | Block the current thread until data can be written to the
-- given file descriptor (GHC only).
threadWaitWrite :: Fd -> IO ()
threadWaitWrite fd
#ifndef mingw32_HOST_OS
  | threaded  = waitForWriteEvent fd
#endif
  | otherwise = IO $ \s -> 
	case fromIntegral fd of { I# fd# ->
	case waitWrite# fd# s of { s -> (# s, () #)
	}}
#endif

-- | Suspends the current thread for a given number of microseconds
-- (GHC only).
--
-- There is no guarantee that the thread will be rescheduled promptly
-- when the delay has expired, but the thread will never continue to
-- run /earlier/ than specified.
--
#ifdef house_HOST_OS
threadDelay :: Int -> IO ()
threadDelay (I# time#) = IO $ \s -> case delay# time# s of s -> (# s, () #)
#else
threadDelay :: Int -> IO ()
threadDelay time
  | threaded  = waitForDelayEvent time
  | otherwise = IO $ \s -> 
	case fromIntegral time of { I# time# ->
	case delay# time# s of { s -> (# s, () #)
	}}


-- | Set the value of returned TVar to True after a given number of
-- microseconds. The caveats associated with threadDelay also apply.
--
registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs 
  | threaded = waitForDelayEventSTM usecs
  | otherwise = error "registerDelay: requires -threaded"
#endif /* !house_HOST_OS */

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool

-- ----------------------------------------------------------------------------
-- Threaded RTS implementation of threadWaitRead, threadWaitWrite, threadDelay

-- In the threaded RTS, we employ a single IO Manager thread to wait
-- for all outstanding IO requests (threadWaitRead,threadWaitWrite)
-- and delays (threadDelay).  
--
-- We can do this because in the threaded RTS the IO Manager can make
-- a non-blocking call to select(), so we don't have to do select() in
-- the scheduler as we have to in the non-threaded RTS.  We get performance
-- benefits from doing it this way, because we only have to restart the select()
-- when a new request arrives, rather than doing one select() each time
-- around the scheduler loop.  Furthermore, the scheduler can be simplified
-- by not having to check for completed IO requests.

-- Issues, possible problems:
--
--	- we might want bound threads to just do the blocking
--	  operation rather than communicating with the IO manager
--	  thread.  This would prevent simgle-threaded programs which do
--	  IO from requiring multiple OS threads.  However, it would also
--	  prevent bound threads waiting on IO from being killed or sent
--	  exceptions.
--
--	- Apprently exec() doesn't work on Linux in a multithreaded program.
--	  I couldn't repeat this.
--
-- 	- How do we handle signal delivery in the multithreaded RTS?
--
--	- forkProcess will kill the IO manager thread.  Let's just
--	  hope we don't need to do any blocking IO between fork & exec.

{-# NOINLINE pendingEvents #-}
{-# NOINLINE pendingDelays #-}
(pendingEvents,pendingDelays) = unsafePerformIO $ do
  startIOManagerThread
  reqs <- newIORef []
  dels <- newIORef []
  return (reqs, dels)
	-- the first time we schedule an IO request, the service thread
	-- will be created (cool, huh?)

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning 
  | threaded  = seq pendingEvents $ return ()
  | otherwise = return ()

-- XXX: move into GHC.IOBase from Data.IORef?
atomicModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef (IORef (STRef r#)) f = IO $ \s -> atomicModifyMutVar# r# f s

prodServiceThread :: IO ()
prodServiceThread = return ()

startIOManagerThread :: IO ()
startIOManagerThread = return ()

\end{code}
