module Scratch(scratchMain) where

import Foreign.C(CString, withCString)

import H.Monad(H, liftIO)
import H.Concurrency

foreign import ccall unsafe "start.h c_print" c_print :: CString -> IO ()

cPrint :: String -> H ()
cPrint str = liftIO $ withCString str c_print

{- MVar test - multiple threads block on putMVar. -}
scratchMain :: H ()
scratchMain =
  do mv <- newEmptyMVar
     forkH $ writer mv "A"
     forkH $ writer mv "B"
     forkH $ writer mv "C"
     forkH $ writer mv "D"
     reader mv

writer mv name =
  do putMVar mv $ "hello from " ++ name ++ "\n"
     writer mv name

reader mv =
  do msg <- takeMVar mv
     cPrint msg
     idle (50000000 + length msg)
     reader mv

idle n | n <= 0 = cPrint "................\n"
       | otherwise = idle (n-1)

