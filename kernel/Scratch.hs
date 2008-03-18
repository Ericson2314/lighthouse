module Scratch(scratchMain) where

import Foreign.C(CString, withCString)

import H.Monad(H, liftIO)
import H.Concurrency

foreign import ccall unsafe "start.h c_print" c_print :: CString -> IO ()

cPrint :: String -> H ()
cPrint str = liftIO $ withCString str c_print

-- Call this with some varying parameter - "idle 50000000" will be foiled by
-- GHC optimizations which save the result, causing it to only idle once.
idle n | n <= 0 = cPrint "................\n"
       | otherwise = idle (n-1)

scratchMain :: H ()
scratchMain =
  do idle 50000000
     cPrint "Scratch exiting...\n"
     return ()



