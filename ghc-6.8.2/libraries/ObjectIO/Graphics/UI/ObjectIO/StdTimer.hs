module Graphics.UI.ObjectIO.StdTimer
		(
		-- * Opening timers
		  Timers(..),
		-- * Closing timers
		  closeTimer,
		-- * Get the Ids of all timers
		  getTimers,
		-- * Enable\/Disable timers
		  enableTimer, disableTimer, getTimerSelectState,
		-- * Timer interval
		setTimerInterval, getTimerInterval,
		-- * A visible module
		  module Graphics.UI.ObjectIO.StdTimerDef
		) where


--	Clean Object I/O library, version 1.2

import	Graphics.UI.ObjectIO.CommonDef
import  Graphics.UI.ObjectIO.Id
import  Graphics.UI.ObjectIO.Process.IOState
import  Graphics.UI.ObjectIO.Timer.Access
import  Graphics.UI.ObjectIO.Timer.DefAccess
import  Graphics.UI.ObjectIO.Timer.Device
import  Graphics.UI.ObjectIO.Timer.Table
import  Graphics.UI.ObjectIO.Timer.Handle
import	Graphics.UI.ObjectIO.StdId
import	Graphics.UI.ObjectIO.StdTimerDef
import	Graphics.UI.ObjectIO.StdTimerAttribute
import	Graphics.UI.ObjectIO.StdTimerElementClass
import  Control.Monad(unless)
import  System.IO(fixIO)
import  qualified Data.Map as Map

stdTimerFatalError :: String -> String -> x
stdTimerFatalError function error
	= dumpFatalError function "StdTimer" error


--	Open timer:

class Timers tdef where
	openTimer :: ls -> tdef ls ps -> ps -> GUI ps ps
	

instance TimerElements t => Timers (Timer t) where	
	openTimer ls tDef@(Timer period items atts) ps = do
		ps <- dOpen timerFunctions ps
		id <- validateTimerId maybeId
		(ok,timers) <- ioStGetTimerHandles
		unless ok (stdTimerFatalError "openTimer (Timer)" "could not retrieve TimerSystemState from IOSt")
		pid <- accIOEnv ioStGetIOId
		it <- ioStGetIdTable		
		tt <- ioStGetTimerTable
		ts <- timerElementToHandles items
		let itemHs = map timerElementStateToTimerElementHandle ts
		(case bindTimerElementIds pid id itemHs it of
			Just it -> do
				let tLoc = TimerLoc
					{ tlIOId	= pid
					, tlDevice	= TimerDevice
					, tlParentId	= id
					, tlTimerId	= id
					}
				let tt1	= if ableTimer then addTimerToTimerTable tLoc period tt else tt
				let it1	= Map.insert id IdParent{idpIOId=pid,idpDevice=TimerDevice,idpId=id} it
				ioStSetTimerTable tt1				
				ioStSetIdTable it1
				let tH = TimerHandle
					{ tId		= id
					, tSelect	= ableTimer
					, tPeriod	= max 0 period
					, tFun		= f
					, tItems	= itemHs
					}
				(_,ps) <- toGUI (\ioState -> fixIO(\st -> fromGUI (timerInit (ls,ps))
						(ioStSetDevice 
							(TimerSystemState 
								timers{tTimers=(TimerStateHandle (TimerLSHandle {tState=fst (fst st),tHandle=tH}):tTimers timers)})
							ioState)))
				return ps
			Nothing -> do
				appIOEnv (ioStSetDevice (TimerSystemState timers))
				throwGUI ErrorIdsInUse)
		where
			(hasIdAtt,idAtt)	= cselect isTimerId undefined atts
			maybeId			= if hasIdAtt then Just (getTimerIdAtt idAtt) else Nothing
			ableTimer		= enabled (getTimerSelectStateAtt (snd (cselect isTimerSelectState (TimerSelectState Able) atts)))
			f			= getTimerFun (snd (cselect isTimerFunction (TimerFunction (\_ st->return st)) atts))
			timerInit		= getTimerInitFun (snd (cselect isTimerInit (TimerInit return) atts))
			

			validateTimerId :: Maybe Id -> GUI ps Id
			validateTimerId Nothing = openId
			validateTimerId (Just id) = do
				it <- ioStGetIdTable
				(if Map.member id it then throwGUI ErrorIdsInUse
				 else return id)
	
	

eqTimerStateHandleId :: Id -> TimerStateHandle ps -> Bool
eqTimerStateHandleId id (TimerStateHandle (TimerLSHandle {tHandle=tH})) = id == tId tH


--	Close timer:

closeTimer :: Id -> GUI ps ()
closeTimer id = do
	(ok,tHs) <- ioStGetTimerHandles
	(if not ok then return ()
	 else do
		pid <- accIOEnv ioStGetIOId
		tt <- ioStGetTimerTable
		it <- ioStGetIdTable
	  	let (tt1,it1) = closeTimer' id pid tt it (tTimers tHs)
		ioStSetIdTable it1
		ioStSetTimerTable tt1	  
		appIOEnv (ioStSetDevice (TimerSystemState tHs)))
	where
		closeTimer' :: Id -> SystemId -> TimerTable -> IdTable -> [TimerStateHandle ps] -> (TimerTable,IdTable)
		closeTimer' id pid tt it (tsH:tsHs)		
			| eqTimerStateHandleId id tsH = disposeElementIds pid tsH tt it
			| otherwise = closeTimer' id pid tt it tsHs
			where
				disposeElementIds :: SystemId -> TimerStateHandle ps -> TimerTable -> IdTable -> (TimerTable,IdTable)
				disposeElementIds pid (TimerStateHandle (TimerLSHandle {tHandle=tH})) tt it =
					let (tt1,it1) = unbindTimerElementIds pid (tItems tH) (tt,it)
					in (removeTimerFromTimerTable teLoc tt1,Map.delete (tId tH) it1)
					where
						teLoc	= TimerLoc {tlIOId=pid,tlDevice=TimerDevice,tlParentId=(tId tH),tlTimerId=(tId tH)}
		closeTimer' _ _ tt it [] = (tt,it)


--	Get the Ids and TimerTypes of all timers:

getTimers :: GUI ps [Id]
getTimers = do
	(ok,tHs) <- ioStGetTimerHandles
	(if not ok then return []
	 else do
		let ids	= getIds (tTimers tHs)	  
		appIOEnv (ioStSetDevice (TimerSystemState tHs))
		return ids)
	where
		getIds :: [TimerStateHandle ps] -> [Id]
		getIds [] = []
		getIds (TimerStateHandle (TimerLSHandle {tHandle=tH}):tsHs) = (tId tH : getIds tsHs)
	
	

--	Enabling and Disabling of timers:

enableTimer :: Id -> GUI ps ()
enableTimer id = changeTimer id enableTimer'
	where
		enableTimer' :: TimerLoc -> TimerTable -> TimerStateHandle ps -> (TimerTable, TimerStateHandle ps)
		enableTimer' teLoc tt (TimerStateHandle tlsH@(TimerLSHandle {tHandle=tH}))
			| tSelect tH = (tt,TimerStateHandle tlsH)
			| otherwise  =
				let tt1 = addTimerToTimerTable teLoc (tPeriod tH) tt
				in (tt1,TimerStateHandle tlsH{tHandle=tH{tSelect=True}})

disableTimer :: Id -> GUI ps ()
disableTimer id = changeTimer id disableTimer'
	where
		disableTimer' :: TimerLoc -> TimerTable -> TimerStateHandle ps -> (TimerTable, TimerStateHandle ps)
		disableTimer' teLoc tt (TimerStateHandle tlsH@(TimerLSHandle {tHandle=tH}))
			| not (tSelect tH) = (tt,TimerStateHandle tlsH)
			| otherwise =
				let tt1 = removeTimerFromTimerTable teLoc tt
				in (tt1,TimerStateHandle tlsH{tHandle=tH{tSelect=False}})


--	Get the SelectState of timers:

getTimerSelectState :: Id -> GUI ps (Maybe SelectState)
getTimerSelectState id = do
	(ok,tHs) <- ioStGetTimerHandles
	(if not ok then return Nothing
	 else do
		let maybe_select = getTimerSelect id (tTimers tHs)
		appIOEnv (ioStSetDevice (TimerSystemState tHs))
		return maybe_select)
	where
		getTimerSelect :: Id -> [TimerStateHandle ps] -> Maybe SelectState
		getTimerSelect id ((TimerStateHandle tlsH@(TimerLSHandle {tHandle=tH})):tsHs)
			| id == tId tH = Just (if tSelect tH then Able else Unable)
			| otherwise    = getTimerSelect id tsHs
		getTimerSelect _ [] = Nothing


--	Set the TimerInterval of timers:

setTimerInterval :: Id -> TimerInterval -> GUI ps ()
setTimerInterval id interval = do
	changeTimer id (setTimerInterval' interval)
	where
		setTimerInterval' :: TimerInterval -> TimerLoc -> TimerTable -> TimerStateHandle ps -> (TimerTable, TimerStateHandle ps)
		setTimerInterval' period teLoc tt tsH@(TimerStateHandle tlsH@(TimerLSHandle {tHandle=tH}))
			| period' == tPeriod tH = (tt,tsH)
			| not (tSelect tH)	= (tt,tsH')
			| otherwise =
				let (_,tt) = setIntervalInTimerTable teLoc period' tt
				in (tt,tsH')
			where
				period'	= max 0 period
				tsH' = TimerStateHandle tlsH{tHandle=tH{tPeriod=period'}}


-- Get the TimerInterval of timers:

getTimerInterval :: Id -> GUI ps (Maybe TimerInterval)
getTimerInterval id = do
	(ok,tHs) <- ioStGetTimerHandles
	(if not ok then return Nothing
	 else do
		let optInterval = getTimerInterval' id (tTimers tHs)	  
		appIOEnv (ioStSetDevice (TimerSystemState tHs))
		return optInterval)
	where
		getTimerInterval' :: Id -> [TimerStateHandle ps] -> Maybe TimerInterval
		getTimerInterval' id ((TimerStateHandle tlsH@(TimerLSHandle {tHandle=tH})):tsHs)
			| id == tId tH = Just (tPeriod tH)
			| otherwise    = getTimerInterval' id tsHs
		getTimerInterval' _ _  = Nothing


ioStGetTimerHandles :: GUI ps (Bool,TimerHandles ps)
ioStGetTimerHandles = do
	(found,tDevice) <- accIOEnv (ioStGetDevice TimerDevice)
	(if not found then return (False,undefined)
	 else return (True,timerSystemStateGetTimerHandles tDevice))


--	General TimerHandle changing function:

type DeltaTimerStateHandle ps = TimerLoc -> TimerTable -> TimerStateHandle ps -> (TimerTable,TimerStateHandle ps)

changeTimer :: Id -> DeltaTimerStateHandle ps -> GUI ps ()
changeTimer id f = do
	(ok,tHs) <- ioStGetTimerHandles
	(if not ok
	 then return ()
	 else do
		tt <- ioStGetTimerTable
		ioid <- accIOEnv ioStGetIOId
		let (tt1,tHs1) = changeTimerDevice ioid id f tt tHs
		appIOEnv (ioStSetDevice (TimerSystemState tHs1))
		ioStSetTimerTable tt1
		return ())
	where
		changeTimerDevice :: SystemId -> Id -> DeltaTimerStateHandle ps -> TimerTable -> TimerHandles ps -> (TimerTable,TimerHandles ps)
		changeTimerDevice ioid id f tt timers@(TimerHandles {tTimers=tsHs}) =
			let (tt1,tsHs1)	= changeTimerStateHandles ioid id f tt tsHs
			in (tt1,timers{tTimers=tsHs1})
			where
				changeTimerStateHandles :: SystemId -> Id -> DeltaTimerStateHandle ps -> TimerTable -> [TimerStateHandle ps] -> (TimerTable,[TimerStateHandle ps])
				changeTimerStateHandles ioid id f tt (tsH@(TimerStateHandle (TimerLSHandle {tHandle=tH})):tsHs)
					| id == tId tH =
						let
							teLoc		= TimerLoc{tlIOId=ioid,tlDevice=TimerDevice,tlParentId=id,tlTimerId=id}
							(tt1,tsH1)	= f teLoc tt tsH
						in
							 (tt1,tsH1:tsHs)
					| otherwise =
						let
							(tt1,tsHs1)	= changeTimerStateHandles ioid id f tt tsHs
						in 
							(tt1,tsH:tsHs1)
				changeTimerStateHandles _ _ _ tt tsHs = (tt,tsHs)
