-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.ProcessEvent
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- OS.ProcessEvent defines the DeviceEventFunction for the process device.
-- This function is placed in a separate module because it is platform dependent.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.ProcessEvent (processEvent) where



import Graphics.UI.ObjectIO.StdIOCommon
import Graphics.UI.ObjectIO.CommonDef (dumpFatalError)
import Graphics.UI.ObjectIO.Device.Events
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.OS.ClCrossCall_12
import Graphics.UI.ObjectIO.OS.Event
import Graphics.UI.ObjectIO.OS.Types (osNoWindowPtr, OSWindowPtr)
import Graphics.UI.ObjectIO.OS.Cutil_12(int2addr, free)
import Foreign.C.String(peekCString)


processeventFatalError :: String -> String -> x
processeventFatalError function error
	= dumpFatalError function "ProcessEvent" error


{-	processEvent filters the scheduler events that can be handled by this process device.
	processEvent assumes that it is not applied to an empty IOSt.
-}

processEvent :: IOSt ps -> SchedulerEvent -> IO (Bool,Maybe DeviceEvent,SchedulerEvent)

processEvent ioState schedulerEvent@(ScheduleOSEvent osEvent@(CrossCallInfo {ccMsg=msg}) _)
	| isProcessOSEvent msg = do
		(myEvent,replyToOS,deviceEvent) <- filterOSEvent osEvent True (ioStGetOSDInfo ioState)
		let schedulerEvent1 =
			if   isJust replyToOS
			then ScheduleOSEvent osEvent (fromJust replyToOS)
			else schedulerEvent
		return (myEvent,deviceEvent,schedulerEvent1)
	| otherwise = return (False,Nothing,schedulerEvent)
	where
		isProcessOSEvent :: Int -> Bool
		isProcessOSEvent msg = msg==ccWmPROCESSCLOSE || msg==ccWmDDEEXECUTE || msg==ccWmPROCESSDROPFILES

processEvent ioState schedulerEvent@(ScheduleMsgEvent _) =
    return (False,Nothing,schedulerEvent)



{-	filterOSEvent filters the OSEvents that can be handled by this process device.
		This implementation only handles the ccWmPROCESSCLOSE event (ccWmDDEEXECUTE and ccWmPROCESSDROPFILES skipped).
-}
filterOSEvent :: OSEvent -> Bool -> OSDInfo -> IO (Bool,Maybe [Int],Maybe DeviceEvent)

filterOSEvent (CrossCallInfo {ccMsg=msg,p1=p1,p2=p2}) isActive osdInfo
	| msg==ccWmDDEEXECUTE =
		if not isActive
		then return (False,Nothing,Nothing)
		else do
			let txt = int2addr p1
			fName <- peekCString txt
			free txt
			return (True,Nothing,Just (ProcessRequestOpenFiles [fName]))
		
	| msg==ccWmPROCESSCLOSE
		= if   p1==getOSDInfoFramePtr osdInfo
		  then return (True, Nothing,Just ProcessRequestClose)
		  else return (False,Nothing,Nothing)

	| msg==ccWmPROCESSDROPFILES =
		if p1 /= getOSDInfoFramePtr osdInfo
		then return (False,Nothing,Nothing)
		else do
			let txt = int2addr p2
			allNames <- peekCString txt
			free txt			
			let names = filter (not . null) (lines allNames)
			return (True,Nothing,Just (ProcessRequestOpenFiles names))	
	| otherwise
		= processeventFatalError "filterOSEvent" "unmatched OSEvent"


getOSDInfoFramePtr :: OSDInfo -> OSWindowPtr
getOSDInfoFramePtr osdInfo
	= case getOSDInfoOSInfo osdInfo of
		Just info -> osFrame info
		_         -> osNoWindowPtr
