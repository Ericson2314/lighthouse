-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Timer.Device
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--	
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Timer.Device(timerFunctions) where


import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.Process.IOState
import	Graphics.UI.ObjectIO.Receiver.Handle
import  Graphics.UI.ObjectIO.StdTimerDef(NrOfIntervals)
import	Graphics.UI.ObjectIO.Timer.Access
import	Graphics.UI.ObjectIO.Timer.DefAccess
import	Graphics.UI.ObjectIO.Timer.Handle
import	Graphics.UI.ObjectIO.Timer.Table
import  Graphics.UI.ObjectIO.Id
import	Graphics.UI.ObjectIO.OS.TimerEvent
import  System.IO(fixIO)
import  Data.Map as Map


timerDeviceFatalError :: String -> String -> x
timerDeviceFatalError function error = dumpFatalError function "TimerDevice" error

timerFunctions :: DeviceFunctions ps
timerFunctions =
	DeviceFunctions 
	  {	dDevice	= TimerDevice
	  ,	dShow	= return
	  ,	dHide	= return
	  ,	dEvent	= timerEvent
	  ,	dDoIO	= timerIO
	  ,	dOpen	= timerOpen
	  ,	dClose	= timerClose
	  }

timerOpen :: ps -> GUI ps ps
timerOpen ps = do
	hasTimer <- accIOEnv (ioStHasDevice TimerDevice)
	(if hasTimer then return ps
	 else do
		appIOEnv (ioStSetDevice (TimerSystemState (TimerHandles	{tTimers=[]})))
		appIOEnv (ioStSetDeviceFunctions timerFunctions)
		return ps)
		

timerClose :: ps -> GUI ps ps
timerClose ps = do
	(found,timers) <- accIOEnv (ioStGetDevice TimerDevice)
	(if not found then return ps
	 else do
		tt <- ioStGetTimerTable
		it <- ioStGetIdTable
		pid <- accIOEnv ioStGetIOId
		let tHs	= timerSystemStateGetTimerHandles timers
		let (tt1,it1) = foldr (closeTimerIds pid) (tt,it) (tTimers tHs)
		ioStSetTimerTable tt1
		ioStSetIdTable it1
		appIOEnv (ioStRemoveDevice TimerDevice)
		appIOEnv (ioStRemoveDeviceFunctions TimerDevice)
		return ps)
	where
		closeTimerIds :: SystemId -> TimerStateHandle ps -> (TimerTable,IdTable) -> (TimerTable,IdTable)
		closeTimerIds pid (TimerStateHandle (TimerLSHandle {tHandle=(TimerHandle {tId=tId,tItems=tItems})})) tables =
			let (tt,it)	= unbindTimerElementIds pid tItems tables
			in (removeTimerFromTimerTable teLoc tt,delete tId it)
			where
				teLoc = TimerLoc{tlIOId=pid,tlDevice=TimerDevice,tlParentId=tId,tlTimerId=tId}


timerIO	:: DeviceEvent -> ps -> GUI ps ps
timerIO deviceEvent ps = do
	hasDevice <- accIOEnv (ioStHasDevice TimerDevice)
	(if (not hasDevice)
	 then timerDeviceFatalError "timerFunctions.dDoIO" "could not retrieve TimerSystemState from IOSt"
	 else timerIO deviceEvent ps)
	where
		timerIO	:: DeviceEvent -> ps -> GUI ps ps
		timerIO (TimerDeviceEvent (TimerEvent {teLoc=TimerLoc{tlParentId=tlParentId,tlTimerId=tlTimerId},teNrInterval=teNrInterval})) ps = do
			(_,timer) <- accIOEnv (ioStGetDevice TimerDevice)
			let timers = timerSystemStateGetTimerHandles timer		  
			toGUI (letOneTimerDoIO tlParentId tlTimerId teNrInterval timers ps)
			where
				letOneTimerDoIO :: Id -> Id -> NrOfIntervals -> TimerHandles ps -> ps -> IOSt ps -> IO (ps, IOSt ps)
				letOneTimerDoIO parent timer nrOfIntervals timers ps ioState = f tsH ioState
					where
						(_,tsH,tsHs) = remove (identifyTimerStateHandle parent) (timerDeviceFatalError "timerIO (TimerEvent _)" "timer could not be found") (tTimers timers)

						f (TimerStateHandle tlsH@(TimerLSHandle {tState=ls,tHandle=tH})) ioState = do
							r <- fixIO (\st -> fromGUI (tFun tH nrOfIntervals (ls,ps))
								(ioStSetDevice (TimerSystemState timers{tTimers=tsHs++[TimerStateHandle tlsH{tState=fst (fst st)}]}) ioState))
							let ((_,ps1), ioState) = r
							return (ps1,ioState)

		timerIO (ReceiverEvent rid) ps = do
			(_,timers) <- accIOEnv (ioStGetDevice TimerDevice)
			let tHs = timerSystemStateGetTimerHandles timers
			toGUI (msgTimerStateHandles (\tsHs st ioState -> ioStSetDevice (TimerSystemState tHs{tTimers=tsHs}) ioState) rid (tTimers tHs) ps)
			where			
				msgTimerStateHandles :: ([TimerStateHandle ps] -> ps -> IOSt ps -> IOSt ps) ->
							  Id -> [TimerStateHandle ps] -> ps -> IOSt ps -> IO (ps,IOSt ps)
				msgTimerStateHandles build rid (tsH@(TimerStateHandle (TimerLSHandle {tState=ls,tHandle=tH@(TimerHandle {tItems=itemHs})})):tsHs) ps ioState = do
					r <- msgTimerElementHandles (\itemHs st ioState -> build (TimerStateHandle (TimerLSHandle {tState=fst st,tHandle=tH{tItems=itemHs}}):tsHs) (snd st) ioState) rid itemHs (ls,ps) ioState
					let (done,(ls,ps),ioState) = r				  
					(if done then return (ps, ioState)
					 else msgTimerStateHandles (\tsHs st ioState -> build (tsH:tsHs) st ioState) rid tsHs ps ioState)
					where
						msgTimerElementHandles :: ([TimerElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IOSt ps) ->
									    Id -> [TimerElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IO (Bool,(ls,ps),IOSt ps)
						msgTimerElementHandles build rid (itemH:itemHs) ls_ps ioState = do
							r <- msgTimerElementHandle (\itemH st ioState -> build (itemH:itemHs) st ioState) rid itemH ls_ps ioState
							let (done,ls_ps,ioState) = r
							(if done then return r
							 else msgTimerElementHandles (\itemHs st ioState -> build (itemH:itemHs) st ioState) rid itemHs ls_ps ioState)
							where
								msgTimerElementHandle :: (TimerElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
											   Id -> TimerElementHandle ls ps -> (ls,ps) -> IOSt ps -> IO (Bool,(ls,ps),IOSt ps)
								msgTimerElementHandle build rid (TimerListLSHandle itemHs) ls_ps ioState =
									msgTimerElementHandles (\itemHs st ioState -> build (TimerListLSHandle itemHs) st ioState) rid itemHs ls_ps ioState

								msgTimerElementHandle build rid (TimerExtendLSHandle exLS itemHs) (ls,ps) ioState = do
									r <- msgTimerElementHandles (\itemHs st ioState -> build (TimerExtendLSHandle (fst (fst st)) itemHs) (snd (fst st), snd st) ioState) rid itemHs ((exLS,ls),ps) ioState
									let (done,((_,ls),ps),ioState) = r
									return (done,(ls,ps),ioState)

								msgTimerElementHandle build rid (TimerChangeLSHandle chLS itemHs) (ls,ps) ioState = do
									r <- msgTimerElementHandles (\itemHs st ioState -> build (TimerChangeLSHandle (fst st) itemHs) (ls,snd st) ioState) rid itemHs (chLS,ps) ioState
									let (done,(_,ps),ioState) = r
									return (done,(ls,ps),ioState)

								msgTimerElementHandle build rid itemH@(TimerReceiverHandle rH@(ReceiverHandle {rFun=rFun}) atts) ls_ps ioState
									| receiverIdentified rid rH = do
										r <- fixIO (\st -> fromGUI (rFun ls_ps) (build (TimerReceiverHandle rH atts) (fst st) ioState))
										let (ls_ps, ioState) = r
										return (True, ls_ps, ioState)
									| otherwise = return (False,ls_ps, ioState)

						msgTimerElementHandles _ _ _ ps ioState = return (False,ps,ioState)
				msgTimerStateHandles _ _ _ ps ioState = return (ps,ioState)