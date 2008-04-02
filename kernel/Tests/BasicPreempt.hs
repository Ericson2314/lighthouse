module Scratch(scratchMain) where

import Foreign.C(CString, withCString)

import H.Monad(H, liftIO)
import H.Concurrency
import LwConc.ConcLib(queueLength)
-- for faking allocation
import Data.IORef

foreign import ccall unsafe "start.h c_print" c_print :: CString -> IO ()

cPrint :: String -> H ()
cPrint str = liftIO $ withCString str c_print

-- Call this with some varying parameter - "idle 50000000" will be foiled by
-- GHC optimizations which save the result, causing it to only idle once.
idle n | n <= 0 = return () -- cPrint "................\n"
       | otherwise = idle (n-1)

scratchMain :: H ()
scratchMain =
  do forkH $ preemptTest "a"
     forkH $ preemptTest "b"
     forkH $ preemptTest "c"
     --preemptTest "m"
     cPrint "Scratch exiting...\n"
     return ()

preemptTest s = do i <- liftIO $ queueLength
                   --t <- liftIO $ newIORef [1..50]
                   --u <- liftIO $ readIORef t
                   --cPrint (s ++ "[" ++ show i ++ " threads in queue]\n")
                   idle $ 10000000 + length s
                   cPrint s
                   preemptTest s

