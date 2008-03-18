module Scratch(scratchMain) where

import Foreign.C(CString, withCString)

import H.Monad(H, liftIO)
import H.Concurrency

foreign import ccall unsafe "start.h c_print" c_print :: CString -> IO ()

cPrint :: String -> H ()
cPrint str = liftIO $ withCString str c_print

{- MVar implementation test: multiple threads block on takeMVar. -}
scratchMain :: H ()
scratchMain =
  do notifier <- newEmptyMVar
     response <- newEmptyMVar
     forkH $ rabbid notifier response "A" 1
     forkH $ rabbid notifier response "B" 1
     forkH $ rabbid notifier response "C" 1
     forkH $ rabbid notifier response "D" 1
     plexus notifier response

plexus notifier response =
  do putMVar notifier "-phone rings-"
     msg <- takeMVar response
     cPrint msg
     plexus notifier response

rabbid notifier response name n =
  do msg <- takeMVar notifier
     idle (50000000 + n) -- the + n is to foil ghc -O
     cPrint $ name ++ ": " ++ msg
     putMVar response (name ++ " says: D" ++ replicate n 'a' ++ "h!\n")
     rabbid notifier response name (((n+1) `mod` 40) + 1)

idle n | n <= 0 = cPrint "................\n"
       | otherwise = idle (n-1)

