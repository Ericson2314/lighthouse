-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Process.Scheduler
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Scheduler contains the process creation, termination, and handling functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Process.Scheduler
		( Context
		, initContext
		, handleEvents
		, handleContextOSEvent
		, addInteractiveProcess
		, closeContext
		, quitProcess
		) where


import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.StdProcessDef
import Graphics.UI.ObjectIO.Timer.Table(getTimeIntervalFromTimerTable)
import Graphics.UI.ObjectIO.OS.Event
import Graphics.UI.ObjectIO.OS.ClCrossCall_12 (osInitToolbox, osCloseToolbox)
import Data.IORef


schedulerFatalError :: String -> String -> x
schedulerFatalError rule message
	= dumpFatalError rule "Process.Scheduler" message


--	Access functions on RuntimeState:

rsIsClosed :: RuntimeState -> Bool
rsIsClosed Closed = True
rsIsClosed _      = False


--	Starting an interactive process.

initContext :: [ProcessAttribute ps] -> ProcessInit ps -> ps -> DocumentInterface -> IO (Maybe Context)
initContext pAtts initIO ps xDI
	= do {
		initOK <- osInitToolbox;							-- initialise toolbox
		if   initOK									-- toolbox correctly initialised
		then do {
			context  <- initialContext Nothing;					-- initialise context
			context1 <- createInitialProcess pAtts initIO ps xDI context;	-- fork the initial interactive process
			return (Just context1)
		     }
		else return Nothing;		
	  }


--	Handling events until termination of all interactive processes.

handleEvents :: Context -> IO ()
handleEvents context
	= osHandleEvents
		terminate
		getosevent
		getsleeptime
		handlecontext
	where
		terminate :: IO Bool
		terminate = do
			iocontext <- readIORef context
			return (ioContextEmptyProcesses iocontext)
		
		getosevent :: IO (Maybe SchedulerEvent)
		getosevent = do
			iocontext <- readIORef context
			let (event, iocontext1) = ioContextTakeEvent iocontext
			writeIORef context iocontext1
			return event
			  
		getsleeptime :: IO Int
		getsleeptime = do
			iocontext <- readIORef context
			let tt = ioContextGetTimerTable iocontext
			let maybe_sleep	= getTimeIntervalFromTimerTable tt
			let sleep = maybe osLongSleep id maybe_sleep
			return sleep
			
		handlecontext = handleContextOSEvent context

--	Closing a final context. 

closeContext :: Context -> IO ()
closeContext context = do
	iocontext <- readIORef context;
	(if ioContextEmptyProcesses iocontext then return ()
	 else schedulerFatalError "closeContext" "not a final Context")
	osCloseToolbox
	return ()


handleContextOSEvent :: Context -> SchedulerEvent -> IO [Int]
handleContextOSEvent context schedulerEvent = do
	iocontext <- readIORef context
	let processes = ioContextGetProcesses iocontext
	schedulerEvent1 <- letProcessesFilterEvent context schedulerEvent processes
	let replyToOS = case schedulerEvent1 of
		(ScheduleOSEvent  _ reply) -> reply
		_ 			   -> []
	return replyToOS
	where
		letProcessesFilterEvent :: Context -> SchedulerEvent -> [PSt] -> IO SchedulerEvent
		letProcessesFilterEvent context schedulerEvent [] = return schedulerEvent
		letProcessesFilterEvent context schedulerEvent (p@(PSt st ioSt) : ps) 
			| ioStClosed ioSt = letProcessesFilterEvent context schedulerEvent ps
			| otherwise = do
				(ok,maybeDeviceEvent,schedulerEvent1) <- processEventFilter ioSt schedulerEvent
				(if   not ok
				 then letProcessesFilterEvent context schedulerEvent1 ps
				 else case maybeDeviceEvent of
				 	Nothing -> return schedulerEvent1
				      	Just (device, deviceEvent) -> do				      		
				      		p <- handleEventForProcess device deviceEvent p
				      		modifyIORef context (ioContextSetProcess p)
				          	return schedulerEvent1)


handleEventForProcess :: Device -> DeviceEvent -> PSt -> IO PSt
handleEventForProcess device schedulerEvent (PSt ps ioState)
	= do {
		(ps1,ioState2) <- fromGUI (initIO ps) ioState1;
		if   ioStClosed ioState2
		then return (PSt ps1 ioState2)
		else let deviceFunctions = ioStGetDeviceFunctions ioState2
			 (found,df)      = cselect (\df -> dDevice df == device) undefined deviceFunctions
		     in  
			 if   not found
			 then schedulerFatalError "ioProcess" "could not find Device to handle DeviceEvent"
			 else do
				(ps2,ioState3) <- fromGUI (catchGUI (dDoIO df schedulerEvent ps1) handleErrorReport) ioState2
				return (PSt ps2 ioState3)
	  }
	where
		initIO   = ioStGetInitIO ioState
		ioState1 = ioStSetInitIO (return) ioState


createInitialProcess :: [ProcessAttribute ps] -> ProcessInit ps -> ps -> DocumentInterface -> Context -> IO Context
createInitialProcess pAtts initIO ps xDI context = do
    iocontext <- readIORef context
    let (ioId,iocontext1) = ioContextNewMaxIONr iocontext
    newIOSt   <- emptyIOSt ioId xDI pAtts initIO context;
    let p = PSt ps newIOSt
    let iocontext2 = ioContextSetProcess p iocontext1
    writeIORef context iocontext2
    p <- handleEventForProcess ProcessDevice ProcessInitialise p	-- Make sure the interactive process gets started
    modifyIORef context (ioContextSetProcess p)
    return context


addInteractiveProcess :: [ProcessAttribute ps'] -> ProcessInit ps' -> ps' -> DocumentInterface -> GUI ps ()
addInteractiveProcess pAtts initIO ps' xDI = do
    ioId      <- ioStNewMaxIONr
    context   <- accIOEnv ioStGetContext
    newIOSt   <- liftIO (emptyIOSt ioId xDI pAtts initIO context)
    let p = PSt ps' newIOSt
    liftIO (modifyIORef context (ioContextSetProcess p))
    p <- liftIO (handleEventForProcess ProcessDevice ProcessInitialise p)
    liftIO (modifyIORef context (ioContextSetProcess p))


{-	processEventFilter passes the current Device dEvent DeviceFunctions to the
	argument SchedulerEvent to determine if this event should be handled by this
	process.
-}
	
processEventFilter :: IOSt ps -> SchedulerEvent -> IO (Bool,Maybe DeviceEventInfo,SchedulerEvent)
processEventFilter ioState schedulerEvent
    | ioStClosed ioState = return (False,Nothing,schedulerEvent)
    | otherwise 	 = filterEventForDevices eventFunctions schedulerEvent ioState
    where        
        eventFunctions             = [(dDevice df,dEvent df) | df <- ioStGetDeviceFunctions ioState]

        filterEventForDevices :: [(Device,EventFunction ps)] -> SchedulerEvent -> IOSt ps
                              -> IO (Bool,Maybe DeviceEventInfo,SchedulerEvent)
        filterEventForDevices ((device,mapDeviceEvent):mapDeviceEvents) schedulerEvent ioState
                = do {
                    (forThisDevice,okDeviceEvent,schedulerEvent1) <- mapDeviceEvent ioState schedulerEvent;
                    if   not forThisDevice
                    then filterEventForDevices mapDeviceEvents schedulerEvent1 ioState
                    else return ( forThisDevice
                                , case okDeviceEvent of
                                       Just deviceEvent -> Just (device,deviceEvent)
                                       nothing          -> Nothing
                                , schedulerEvent1
                                )
                  }
        filterEventForDevices _ schedulerEvent _
            = return (False,Nothing,schedulerEvent)


{-	Quit this interactive process.
	Quitting a process involves the following:
	- Set the RuntimeState to Closed (quitProcess is the only function that does this)
	- Close all devices
	- Remove the process from the Context
-}
quitProcess :: ps -> GUI ps ps
quitProcess ps
	= do {
		rs <- accIOEnv ioStGetRuntimeState;
		if   rsIsClosed rs
		then return ps
		else do {
		             deviceFunctions <- accIOEnv ioStGetDeviceFunctions;
		             ps1 <- seqListM [dClose df | df<-deviceFunctions] ps;
			     osdInfo <- accIOEnv ioStGetOSDInfo;
			     liftIO (osCloseOSDInfo osdInfo);
		             appIOEnv (ioStSetRuntimeState Closed);
		             
		             -- This part is new: remove the process from the context
		             accIOEnv ioStGetIOId >>= ioStRemoveProcess;
		             return ps1
		        }
	     }
	where
		seqListM :: [ps -> GUI ps ps] -> ps -> GUI ps ps
		seqListM (m:ms) ps
			= m ps >>= (\ps -> seqListM ms ps)
		seqListM _ ps
			= return ps

