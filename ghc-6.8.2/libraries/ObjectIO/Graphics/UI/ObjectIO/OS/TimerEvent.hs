-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.TimerEvent
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.TimerEvent(timerEvent) where



import	Graphics.UI.ObjectIO.Device.Events
import  Graphics.UI.ObjectIO.Device.SystemState
import	Graphics.UI.ObjectIO.Timer.Access
import	Graphics.UI.ObjectIO.Timer.Table
import	Graphics.UI.ObjectIO.Timer.Handle(TimerHandles(..))
import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.Process.IOState
import  Graphics.UI.ObjectIO.OS.Event
import  Graphics.UI.ObjectIO.OS.Time
import	Graphics.UI.ObjectIO.OS.ClCrossCall_12
import	Graphics.UI.ObjectIO.Id(IdParent(..))
import  Data.IORef
import  Data.Map as Map (lookup)

timerEventFatalError :: String -> String -> x
timerEventFatalError function error = dumpFatalError function "OS.TimerEvent" error


{-	The timerEvent function determines whether the given SchedulerEvent can be applied
	to a timer of this process. These are the following cases:
	*	ScheduleTimerEvent: the timer event belongs to this process and device
	*	ScheduleMsgEvent:   the message event belongs to this process and device
	timerEvent assumes that it is not applied to an empty IOSt.
-}
timerEvent :: IOSt ps -> SchedulerEvent -> IO (Bool,Maybe DeviceEvent,SchedulerEvent)
timerEvent ioState schedulerEvent = do
	(if not (ioStHasDevice TimerDevice ioState)
	 then timerEventFatalError "timerFunctions.dEvent" "could not retrieve TimerSystemState from IOSt"
	 else timerEvent schedulerEvent)
	where
		timerEvent :: SchedulerEvent -> IO (Bool,Maybe DeviceEvent,SchedulerEvent)
		timerEvent schedulerEvent@(ScheduleOSEvent osEvent@(CrossCallInfo {ccMsg=ccMsg}) _)
			| ccMsg == ccWmIDLETIMER || ccMsg == ccWmTIMER = do
				iocontext <- readIORef (ioStGetContext ioState)
				let tt = ioContextGetTimerTable iocontext
				let t1 = ioContextGetTime iocontext
				t2 <- osGetTime
				let tt1 = shiftTimeInTimerTable (t2-t1) tt
				let (mb_te, tt2) = getActiveTimerInTimerTable tt1
				(case mb_te of
					Just te | tlIOId (teLoc te) == ioStGetIOId ioState -> do
						let iocontext1 = ioContextSetTimerTable tt2 iocontext
						let iocontext2 = ioContextSetTime t2 iocontext1
						writeIORef (ioStGetContext ioState) iocontext2
						return (True, Just (TimerDeviceEvent te),schedulerEvent)
					_ -> return (False,Nothing,schedulerEvent))
			| otherwise = return (False,Nothing,schedulerEvent)

		timerEvent schedulerEvent@(ScheduleMsgEvent rId) = do
			iocontext <- readIORef (ioStGetContext ioState)			
			case Map.lookup rId (ioContextGetIdTable iocontext) of
				Just idParent | idpIOId idParent == ioStGetIOId ioState && idpDevice idParent == TimerDevice ->
					return (True,Just (ReceiverEvent rId),schedulerEvent)
				_ -> return (False,Nothing,schedulerEvent)
