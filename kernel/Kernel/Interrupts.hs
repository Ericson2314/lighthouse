module Kernel.Interrupts
    ( registerIRQHandler
     , callIRQHandler      
     , interruptHandler
    ) where

import H.Monad(H,liftIO)
import H.Mutable(HArray,newArray,readArray,writeArray)
import H.Unsafe(unsafePerformH)
import H.Interrupts(IRQ(..),enableIRQ,eoiIRQ)
import H.Concurrency(forkH)
import LwConc.ConcLib(timerHandler)
import Foreign.C(CInt)
import Data.Bits(testBit)

import Foreign.C(CString, withCString)
-- import H.Monad(H, liftIO)
foreign import ccall unsafe "start.h c_print" c_print :: CString -> IO ()

cPrint :: String -> H ()
cPrint str = liftIO $ withCString str c_print

{-# NOINLINE irqTable #-}
irqTable :: HArray IRQ (Maybe (H ()))
irqTable = unsafePerformH (newArray (minBound,maxBound) Nothing)

registerIRQHandler irq handler =
    do cPrint ("Enabling " ++ show irq ++ "\n")
       writeArray irqTable irq (Just wrapper)
       enableIRQ irq
    where
      wrapper = 
         do handler
            eoiIRQ irq

callIRQHandler IRQ0 = liftIO $ timerHandler
callIRQHandler irq = 
      do mHandler <- readArray irqTable irq
         case mHandler of
           Just handler -> forkH handler >> return ()
           Nothing -> return () -- cPrint ("No handler for " ++ show irq ++ "\n")
           -- well, actually we _can't_ fork for timerHandler...


foreign import ccall unsafe getPendingIRQs :: IO CInt

interruptHandler :: H ()
interruptHandler =
  do bits <- liftIO getPendingIRQs
     cPrint ("===Pending IRQs: " ++ show bits)
     let pendingIRQs = [x | x <- [IRQ0 ..], testBit bits (fromEnum x)]
     cPrint (show pendingIRQs ++ "\n")
     sequence_ (map callIRQHandler pendingIRQs)

