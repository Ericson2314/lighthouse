-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.ReceiverEvent
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- OS.ReceiverEvent defines the DeviceEventFunction for the receiver device.
-- This function is placed in a separate module because it is platform dependent.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.ReceiverEvent where



import Graphics.UI.ObjectIO.Device.Events
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.OS.Event
import Graphics.UI.ObjectIO.Id(IdParent(..))
import Data.IORef(readIORef)
import qualified Data.Map as Map (lookup)



{-	receiverEvent filters the appropriate events for the receiver device.
	These are only the message events (as long as receivers do not contain timers).
	receiverEvent assumes that it is not applied to an empty IOSt.
	
	Currently, in this implementation only asynchronous message events are supported.
-}

receiverEvent :: IOSt ps -> SchedulerEvent -> IO (Bool,Maybe DeviceEvent,SchedulerEvent)

receiverEvent ioState schedulerEvent@(ScheduleMsgEvent rId) = do
	iocontext <- readIORef (ioStGetContext ioState)
	case Map.lookup rId (ioContextGetIdTable iocontext) of
		Just idParent | idpIOId idParent == ioStGetIOId ioState && idpDevice idParent == ReceiverDevice ->
        		return (True,Just (ReceiverEvent rId),schedulerEvent)
    		_ -> return (False,Nothing,schedulerEvent)

receiverEvent ioState schedulerEvent = return (False,Nothing,schedulerEvent)
