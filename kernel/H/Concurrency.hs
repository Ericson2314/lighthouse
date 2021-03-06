-- | Concurrency support (inherited from Concurrent Haskell)
module H.Concurrency(module H.Concurrency,{- H,-} Chan,MVar,QSem,ThreadId,L.Lock,L.LockKey) where
import qualified Control.Concurrent as IO
import Control.Concurrent(Chan,MVar,QSem,ThreadId)
import qualified Control.Concurrent.Lock as L
import LwConc.PTM as PTM
import H.Monad(H,liftIO,runH)

------------------------ INTERFACE ---------------------------------------------

-- * Thread control
forkH :: (H a) -> H ThreadId
killH :: ThreadId -> H ()
yield :: H ()
threadDelay :: Int -> H ()
myThreadId :: H ThreadId

-- * PTM
atomically :: PTM a -> H a
newPVarH   :: a -> H (PVar a)

-- * Channels
newChan         :: H (Chan a)
readChan        :: Chan a -> H a
writeChan       :: Chan a -> a -> H ()
isEmptyChan     :: Chan a -> H Bool
getChanContents :: Chan a -> H [a]
writeList2Chan  :: Chan a -> [a] -> H ()

-- * MVars
newMVar      :: a -> H (MVar a)
newEmptyMVar :: H (MVar a)
putMVar      :: MVar a -> a -> H ()
takeMVar     :: MVar a -> H a
readMVar     :: MVar a -> H a
isEmptyMVar  :: MVar a -> H Bool
modifyMVar   :: MVar a -> (a -> H (a, b)) -> H b
modifyMVar_  :: MVar a -> (a -> H a) -> H ()
withMVar     :: MVar a -> (a -> H b) -> H b

-- * Semaphores
newQSem :: Int -> H QSem
signalQSem :: QSem -> H ()
waitQSem :: QSem -> H ()
withQSem :: QSem -> (H a) -> H a

-- * Locks (Mutexes)
newLock :: H L.Lock
lockH :: L.Lock -> H L.LockKey
tryLockH :: L.Lock -> H (Maybe L.LockKey)
unlockH :: L.LockKey -> H ()
withLock :: L.Lock -> H () -> H ()

------------------------ IMPLEMENTATION ----------------------------------------

-- Thread control---------------------------------------------------------------
forkH h = liftIO (IO.forkIO (runH h >> return ()))
killH = liftIO . IO.killThread
yield = liftIO IO.yield
threadDelay = liftIO . IO.threadDelay
myThreadId = liftIO IO.myThreadId

-- PTM -------------------------------------------------------------------------
atomically p = liftIO $ PTM.atomically p
newPVarH v   = liftIO $ newPVarIO v

-- Channels --------------------------------------------------------------------
newChan              = liftIO $ IO.newChan
readChan ch          = liftIO $ IO.readChan ch
writeChan ch x       = liftIO $ IO.writeChan ch x
isEmptyChan ch       = liftIO $ IO.isEmptyChan ch
getChanContents ch   = liftIO $ IO.getChanContents ch
writeList2Chan ch xs = liftIO $ IO.writeList2Chan ch xs

-- MVars -----------------------------------------------------------------------
newMVar        x = liftIO $ IO.newMVar        x
newEmptyMVar     = liftIO $ IO.newEmptyMVar
takeMVar     v   = liftIO $ IO.takeMVar     v
putMVar      v x = liftIO $ IO.putMVar      v x
modifyMVar   v h = liftIO $ IO.modifyMVar   v (runH . h)
modifyMVar_  v h = liftIO $ IO.modifyMVar_  v (runH . h)
withMVar     v h = liftIO $ IO.withMVar     v (runH . h)
readMVar     v   = liftIO $ IO.readMVar     v
isEmptyMVar    x = liftIO $ IO.isEmptyMVar    x

-- Semaphores ------------------------------------------------------------------

newQSem    n   = liftIO $ IO.newQSem n
waitQSem   sem = liftIO $ IO.waitQSem   sem
signalQSem sem = liftIO $ IO.signalQSem sem

withQSem sem action =
    do waitQSem sem
       x <- action
       signalQSem sem
       return x

-- Locks (Mutexes) -------------------------------------------------------------

newLock      = liftIO $ L.newLock
lockH l      = liftIO $ L.lock     l
tryLockH l   = liftIO $ L.tryLock  l
unlockH k    = liftIO $ L.unlock   k
withLock l m = liftIO $ L.withLock l (runH m)

