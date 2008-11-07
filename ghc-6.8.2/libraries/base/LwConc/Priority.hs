module LwConc.Priority
( Priority(..)
, myPriority
, setMyPriority
) where

import GHC.Arr(Ix)
import LwConc.Substrate
import System.IO.Unsafe(unsafePerformIO)

--data Priority = Low | Medium | High
data Priority = Worthless | Low | Medium | High | Utmost
  deriving (Show, Eq, Ord, Bounded, Enum, Ix)

priorityKey :: TLSKey Priority
priorityKey = unsafePerformIO $ newTLSKey Medium

-- |Returns the current thread's priority.
myPriority :: IO Priority
myPriority = getTLS priorityKey

-- |Sets the current thread's priority.
setMyPriority :: Priority -> IO ()
setMyPriority p = setTLS priorityKey p

