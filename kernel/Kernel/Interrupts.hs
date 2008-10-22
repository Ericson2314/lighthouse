module Kernel.Interrupts
    ( registerIRQHandler
     , callIRQHandler      
     , interruptHandler
    ) where

import H.Monad(H,liftIO)
import H.Mutable(HArray,newArray,readArray,writeArray)
import H.Unsafe(unsafePerformH)
import H.Interrupts(IRQ(..),enableIRQ,eoiIRQ)
import LwConc.Conc(timerHandler)
import Foreign.C(CInt)
import Data.Bits(testBit)
import Control.Monad(when)

import Foreign.C(CString, withCString)
-- import H.Monad(H, liftIO)
foreign import ccall unsafe "start.h c_print" c_print :: CString -> IO ()
foreign import ccall unsafe hsirqPrint :: IO ()

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

-- timerHandler isn't registered in the table.  I suppose it could be.
callIRQHandler IRQ0 = liftIO timerHandler
callIRQHandler irq = liftIO hsirqPrint >>
  do mHandler <- readArray irqTable irq
     case mHandler of
       Just handler -> handler
       Nothing -> return ()

foreign import ccall unsafe "getPendingIRQ" get_pending_irq_number :: IO CInt

getPendingIRQ :: H (Maybe IRQ)
getPendingIRQ = do irqNum <- liftIO get_pending_irq_number
                   return $ if irqNum < 0 || irqNum > 15
                               then Nothing
                               else Just (toEnum (fromIntegral irqNum))

-- Note that this can reenter.
interruptHandler :: H ()
interruptHandler =
  do maybeIRQ <- getPendingIRQ
     case maybeIRQ of
       Just irq  -> do callIRQHandler irq
                       interruptHandler
       Nothing  -> return ()
