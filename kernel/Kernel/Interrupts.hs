module Kernel.Interrupts
    ( registerIRQHandler
     , callIRQHandler      
     , interruptHandler
    ) where

import H.Monad(H,liftIO)
import H.Mutable(HArray,newArray,readArray,writeArray)
import H.Unsafe(unsafePerformH)
import H.Interrupts(IRQ(..),enableIRQ,eoiIRQ)
import H.Concurrency(forkHighPriorityH)
import LwConc.ConcLib(timerHandler)
import Foreign.C(CInt)
import Data.Bits(testBit)
import Control.Monad(when)

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

-- We can't fork for timerHandler, nor is it registered in the table.
callIRQHandler IRQ0 = error "Don't use callIRQHandler on IRQ0!\n"
callIRQHandler irq = 
  do mHandler <- readArray irqTable irq
     case mHandler of
       Just handler -> do forkHighPriorityH handler
                          return ()
       Nothing -> return ()

foreign import ccall unsafe getPendingIRQs :: IO CInt
foreign import ccall unsafe allowHaskellInterrupts :: CInt -> IO ()
foreign import ccall unsafe disallowHaskellInterrupts :: IO CInt

-- if forkH were to yield, this would fail. we may also want to run small
-- handlers NOT in their own thread...if the amount of work to do is comparable
-- to forking...
interruptHandler :: H ()
interruptHandler =
  do hsiStatus <- liftIO disallowHaskellInterrupts
     --when (hsiStatus == 0) (error "interruptHandler called with Haskell-level interrupts off!")
     bits <- liftIO getPendingIRQs
     --cPrint ("===Pending IRQ bits: " ++ show bits)
     let pendingIRQs = [x | x <- [IRQ1 .. IRQ15], testBit bits (fromEnum x)]
     let timerPending = testBit bits 0
     --cPrint (show (if timerPending then IRQ0:pendingIRQs else pendingIRQs) ++ "\n")
     sequence_ (map callIRQHandler pendingIRQs)
     --cPrint "Done pushing IRQ handlers\n"
     liftIO $ allowHaskellInterrupts hsiStatus
     when timerPending (liftIO timerHandler)
