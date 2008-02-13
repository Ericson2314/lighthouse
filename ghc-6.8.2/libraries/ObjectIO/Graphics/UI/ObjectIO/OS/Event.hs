-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.Event
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- OS.Event contains all type definitions for OS dependent events.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.Event
		( OSEvent, Graphics.UI.ObjectIO.OS.ClCrossCall_12.CrossCallInfo(..)
		, SchedulerEvent(..)
		, osNullEvent
		, osLongSleep, osNoSleep
		, osHandleEvents
		, setReplyInOSEvent
		, osEventIsUrgent
		, createOSActivateWindowEvent, createOSDeactivateWindowEvent
		, createOSActivateControlEvent, createOSDeactivateControlEvent
		, createOSLooseMouseEvent, createOSLooseKeyEvent
		) where 



import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.OS.ClCrossCall_12
import Graphics.UI.ObjectIO.OS.Types
import Foreign.Marshal.Utils(fromBool)

oseventFatalError :: String -> String -> x
oseventFatalError function error
	= dumpFatalError function "OSEvent" error


--	SchedulerEvent and MsgEvent moved from DeviceEvents
data	SchedulerEvent				-- A scheduler event is either:
 =	ScheduleOSEvent  	!OSEvent ![Int]	-- a genuine OS event
 |	ScheduleMsgEvent 	!Id		-- a msg passing event


instance Show SchedulerEvent where
	show (ScheduleOSEvent  e    _)  = "(ScheduleOSEvent " ++ show (ccMsg e) ++ ")"
	show (ScheduleMsgEvent recLoc)  = "ScheduleMsgEvent"


type	OSEvent
	= CrossCallInfo
type	OSSleepTime	-- The max time the process allows multi-tasking
	= Int

osNullEvent :: OSEvent
osNullEvent = rq0Cci 0


osLongSleep :: OSSleepTime
osLongSleep = 2^15-1
osNoSleep :: OSSleepTime
osNoSleep = 0

osHandleEvents :: IO Bool
               -> IO (Maybe SchedulerEvent)
               -> IO Int
               -> (SchedulerEvent -> IO [Int])
               -> IO ()
osHandleEvents isFinalState getOSEvent getSleepTime handleOSEvent = do
	terminate <- isFinalState
	(if terminate then return ()
	 else do
		osEvent <- getOSEvent
		(case osEvent of
			Nothing -> do
				sleep <- getSleepTime
				let getEventCci = rq2Cci ccRqDOMESSAGE (fromBool (sleep/=osLongSleep)) sleep
				issueCleanRequest2 (rccitoevent handleOSEvent) getEventCci
				osHandleEvents isFinalState getOSEvent getSleepTime handleOSEvent
			Just event -> do
				handleOSEvent event
				osHandleEvents isFinalState getOSEvent getSleepTime handleOSEvent))
	where
		rccitoevent :: (SchedulerEvent -> IO [Int]) -> OSEvent -> IO OSEvent
		rccitoevent handleOSEvent osEvent = do
			reply <- handleOSEvent (ScheduleOSEvent osEvent []);
			return (setReplyInOSEvent reply)


setReplyInOSEvent :: [Int] -> CrossCallInfo
setReplyInOSEvent [] = return0Cci
setReplyInOSEvent [e1] = return1Cci e1
setReplyInOSEvent [e1,e2] = return2Cci e1 e2
setReplyInOSEvent [e1,e2,e3] = return3Cci e1 e2 e3
setReplyInOSEvent [e1,e2,e3,e4] = return4Cci e1 e2 e3 e4
setReplyInOSEvent [e1,e2,e3,e4,e5] = return5Cci e1 e2 e3 e4 e5
setReplyInOSEvent [e1,e2,e3,e4,e5,e6] = return6Cci e1 e2 e3 e4 e5 e6
setReplyInOSEvent otherwise             = oseventFatalError "setReplyInOSEvent" "number of reply codes > 6"

osEventIsUrgent :: SchedulerEvent -> Bool
osEventIsUrgent _ = True


{- createOS(Dea/A)ctivateWindowEvent creates the event the platform would generate for a genuine (de)activate event. -}
createOSActivateWindowEvent :: OSWindowPtr -> SchedulerEvent
createOSActivateWindowEvent wPtr = ScheduleOSEvent (rq1Cci ccWmACTIVATE wPtr) []

createOSDeactivateWindowEvent :: OSWindowPtr -> SchedulerEvent
createOSDeactivateWindowEvent wPtr = ScheduleOSEvent (rq1Cci ccWmDEACTIVATE wPtr) []

{- createOS(Dea/A)ctivateControlEvent creates the event the platform would generate for a genuine (de)activate event. -}
createOSActivateControlEvent :: OSWindowPtr -> OSWindowPtr -> SchedulerEvent
createOSActivateControlEvent wPtr cPtr = ScheduleOSEvent (rq2Cci ccWmSETFOCUS wPtr cPtr) []

createOSDeactivateControlEvent :: OSWindowPtr -> OSWindowPtr -> SchedulerEvent
createOSDeactivateControlEvent wPtr cPtr = ScheduleOSEvent (rq2Cci ccWmKILLFOCUS wPtr cPtr) []

{- createOSLoose(Mouse/Key)Event creates the event for reporting loss of mouse/keyboard input (virtual event). -}
createOSLooseMouseEvent :: OSWindowPtr -> OSWindowPtr -> SchedulerEvent
createOSLooseMouseEvent wPtr cPtr = ScheduleOSEvent (rq2Cci ccWmLOSTMOUSE wPtr cPtr) []

createOSLooseKeyEvent :: OSWindowPtr -> OSWindowPtr -> SchedulerEvent
createOSLooseKeyEvent wPtr cPtr = ScheduleOSEvent (rq2Cci ccWmLOSTKEY wPtr cPtr) []
