-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Process.IOState
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- IOState defines the GUI environment types and their access functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Process.IOState
	       ( Context, IOContext, IOSt, GUI, toGUI, appIOEnv, accIOEnv, liftIO, fromGUI
	       , throwGUI, catchGUI, noLS, noLS1
	       , InputTrack(..), InputTrackKind(..)
               , DeviceFunctions(..), dDevice, dEvent, EventFunction, dDoIO, DoIOFunction, dOpen, OpenFunction, dClose, CloseFunction
               , initialContext
               , emptyIOSt
               , ioStGetProcessAttributes, ioStSetProcessAttributes, Graphics.UI.ObjectIO.StdGUI.ProcessAttribute(..)
               , ioStGetOSDInfo, ioStSetOSDInfo 
               , ioStGetInitIO, ioStSetInitIO, Graphics.UI.ObjectIO.StdIOCommon.IdFun
               , ioContextGetInputTrack, ioContextSetInputTrack, ioStGetInputTrack, ioStSetInputTrack
               , ioStClosed, ioStGetRuntimeState, ioStSetRuntimeState, RuntimeState(..)
               , ioStGetIOIsModal, ioStSetIOIsModal 
               , ioStGetIdTable, ioStSetIdTable, Graphics.UI.ObjectIO.Id.IdTable               
               , ioStGetTimerTable, ioStSetTimerTable, Graphics.UI.ObjectIO.Timer.Table.TimerTable
               , ioContextGetTime, ioContextSetTime 
               , ioStTakeEvent, ioStAppendEvents, ioStInsertEvents
               , ioStGetDocumentInterface, ioStGetOSDInfo, ioStSetOSDInfo, module Graphics.UI.ObjectIO.OS.DocumentInterface
               , ioStGetIOId, SystemId(..)
               , ioStGetMaxIONr, ioStSetMaxIONr, ioStNewMaxIONr               
               , ioStGetOSWindowMetrics, Graphics.UI.ObjectIO.OS.System.OSWindowMetrics(..)
               , ioStGetContext
               , DeviceEventInfo
               , PSt(..), ioStEmptyProcesses, ioStGetProcesses, ioStSetProcess, ioStRemoveProcess
               , ioStGetDeviceFunctions, ioStSetDeviceFunctions, ioStRemoveDeviceFunctions
               , ioStHasDevice, ioStGetDevices, ioStGetDevice, ioStRemoveDevice, ioStSetDevice               
               , ioContextTakeEvent, ioContextAppendEvents, ioContextInsertEvents
               , ioContextGetMaxIONr, ioContextSetMaxIONr, ioContextNewMaxIONr
               , ioStGetIdTable, ioStSetIdTable 
               , ioContextGetIdTable, ioContextSetIdTable
               , ioContextGetTimerTable, ioContextSetTimerTable
               , ioContextEmptyProcesses, ioContextGetProcesses, ioContextSetProcess, ioContextRemoveProcess
               , module Graphics.UI.ObjectIO.Device.Types
               , module Graphics.UI.ObjectIO.Device.Events
               , module Graphics.UI.ObjectIO.Device.SystemState
               , liftContextIO
               ) where



import Graphics.UI.ObjectIO.Device.Types
import Graphics.UI.ObjectIO.Device.Events
import Graphics.UI.ObjectIO.Device.SystemState
import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.StdGUI (ProcessAttribute(..))
import Graphics.UI.ObjectIO.StdIOCommon
import Graphics.UI.ObjectIO.SystemId
import Graphics.UI.ObjectIO.Timer.Table(TimerTable, initialTimerTable)
import Graphics.UI.ObjectIO.CommonDef(dumpFatalError)
import Graphics.UI.ObjectIO.OS.DocumentInterface
import Graphics.UI.ObjectIO.OS.Event
import Graphics.UI.ObjectIO.OS.System
import Graphics.UI.ObjectIO.OS.Time(osGetTime, OSTime)
import Graphics.UI.ObjectIO.OS.Types
import Control.Exception
import Data.IORef
import qualified Data.Map as Map
import GHC.Base(unsafeCoerce#)


{-	The GUI monad is an IO monad extended with IOSt.
	Note: GUI can't be defined as newtype because it's abstract.
	      Putting newtype GUI in the hi-boot kills GHC.
-}

data	GUI ps a = GUI !(IOSt ps -> IO (a,IOSt ps))

instance Monad (GUI ps) where
	(>>=) envIOA to_envIOB
		= GUI (bind envIOA to_envIOB)
		where
			bind (GUI fA) to_envIOB ioState
				= fA ioState >>= (\(a,ioState)->case to_envIOB a of
				                            (GUI fB) -> fB ioState)
	
	(>>) envIOA envIOB
		= GUI (bind envIOA envIOB)
		where
			bind (GUI fA) envIOB ioState
				= fA ioState >>= (\(_,ioState)->case envIOB of
				                            (GUI fB) -> fB ioState)
	
	return a = GUI (\ioState -> return (a,ioState))


instance  Functor (GUI ps) where
   fmap f x = x >>= (return . f)


{-  Lift an action (ps -> GUI ps ps) to a GUIFun ls ps. -}
noLS :: (ps -> GUI ps ps) -> (ls,ps) -> GUI ps (ls,ps)  -- Lift action GUI ps to GUIFun ls ps
noLS action (ls,ps) = do { ps1 <- action ps; return (ls,ps1) }

{-  Lift an action (ps -> GUI ps ps) that requires x to a GUIFun ls ps that requires x. -}
noLS1 :: (x -> ps -> GUI ps ps) -> x -> (ls,ps) -> GUI ps (ls,ps)
noLS1 action x (ls,ps) = do { ps1 <- action x ps; return (ls,ps1) }

{- exception handling -}

throwGUI :: ErrorReport	-> a
throwGUI = throwDyn

catchGUI :: GUI ps x -> (ErrorReport -> GUI ps x) -> GUI ps x
catchGUI p1 p2 = toGUI (\ioState -> catchDyn (fromGUI p1 ioState) (\err -> fromGUI (p2 err) ioState))


instance IOMonad (GUI ps) where
	liftIO m = GUI (\ioState->m>>=(\a->return (a,ioState)))

liftContextIO :: (Context -> IO a) -> ps -> GUI ps (a, ps)
liftContextIO action ps = toGUI (\ioSt -> do
	let context = ioStGetContext ioSt
	let id = ioStGetIOId ioSt
	modifyIORef context (ioContextSetProcess (PSt ps ioSt))
	r <- action context
	iocontext <- readIORef context
	let pcs = ioContextGetProcesses iocontext
	let (ps1, ioSt1) = getProcess id pcs
	return ((r,ps1), ioSt1))
	where
	    getProcess :: SystemId -> [PSt] -> (ps, IOSt ps)
	    getProcess id [] = dumpFatalError "liftContextIO" "IOState" "could not retrieve IOSt"
	    getProcess id ((PSt ps ioSt):pcs)
		| ioStGetIOId ioSt == id = unsafeCoerce# (ps, ioSt)
		| otherwise       = getProcess id pcs

{-	The following functions useful for easy composition of IOSt transition functions
	and coercing purposes.
	Since they expose the IOSt type, they are not part of the API.
-}
appIOEnv :: IdFun (IOSt ps) -> GUI ps ()
appIOEnv f = GUI (\env->return ((),f env))

accIOEnv :: (IOSt ps -> x) -> GUI ps x
accIOEnv f = GUI (\env->return (f env,env))

toGUI :: (IOSt ps -> IO (a,IOSt ps)) -> GUI ps a
toGUI fun = GUI fun

fromGUI :: GUI ps a -> IOSt ps -> IO (a,IOSt ps)
fromGUI (GUI fAction) = fAction


{-	IOSt always had all the components to construct a context. These components
	are now being shared via a (IORef IOContext). So we define the IOContext type
	here instead of in Scheduler and have IOSt contain a (IORef IOContext).
-}
data PSt = forall ps . PSt ps (IOSt ps)

type	Context							-- A Context points to an IOContext
	= IORef IOContext
data	IOContext
	= IOContext
		{ ioevents          :: ![SchedulerEvent]	-- The event stream environment
		, ionr              :: !SystemId		-- The max SystemId of all processes		
		, ioidtable         :: !IdTable			-- The table of all bound Ids
		, ioostime	    :: !OSTime			-- The current OSTime
		, iotimertable	    :: !TimerTable		-- The table of all currently active timers
		, ioprocesses       :: ![PSt]			-- The list of all processes
		, ioismodal	    :: !(Maybe SystemId)	-- If a process has some modal windows, then Just id, otherwise Nothing
		, ioinputtrack 	    :: !(Maybe InputTrack)	-- The process is handling mouse/key input flags
		}
type	DeviceEventInfo						-- A DeviceEventInfo event
	=	( Device					-- The Device that accepted the DeviceEvent
		, DeviceEvent					-- The DeviceEvent to be handled
		)
data	IOSt ps
	= IOSt
		{ ioid              :: !SystemId		-- The Id of the process
		, ioinit            :: !(ps -> GUI ps ps)	-- The initialisation functions of the process
		, iodevicefuncs     :: ![DeviceFunctions  ps]	-- The currently active device functions
		, ioatts            :: ![ProcessAttribute ps]	-- The attributes of the process
		, iodevices         :: ![DeviceSystemState ps]	-- The GUI device states of the process
		, ioruntime         :: !RuntimeState		-- The runtime state of the process
		, ioosdinfo         :: !OSDInfo			-- The OS document interface information of the process
		, iooswmetrics      :: !OSWindowMetrics		-- The window metrics
		, iocontext         :: !Context			-- The shared context
		}

data RuntimeState 
	= Running						-- The process is running
	| Blocked !SystemId					-- The process is blocked for the process with given id
	| Closed						-- The process is closed
	deriving (Eq)

data InputTrack
   = InputTrack							-- Input being tracked:
	{ itWindow	:: !OSWindowPtr				-- the parent window
	, itControl	:: !Int					-- zero if parent window, otherwise item nr of control (>0)
	, itKind	:: !InputTrackKind			-- the input kinds being tracked
	}
data InputTrackKind
   = InputTrackKind						-- Input source kinds:
	{ itkMouse	:: !Bool				-- mouse
	, itkKeyboard	:: !Bool				-- keyboard
	}

data	DeviceFunctions ps					-- The major device callback functions:
	= DeviceFunctions
		{ dDevice :: Device				-- The device kind
		, dShow	  :: ShowFunction  ps
		, dHide	  :: HideFunction  ps
		, dEvent  :: EventFunction ps			-- Map an SchedulerEvent to a DeviceEvent
		, dDoIO   :: DoIOFunction  ps			-- Handle a DeviceEvent for this device
		, dOpen   :: OpenFunction  ps			-- Open the initial device
		, dClose  :: CloseFunction ps			-- Close the device and its instances
		}

type	OpenFunction  ps = ps -> GUI ps ps
type	CloseFunction ps = ps -> GUI ps ps
type	ShowFunction  ps = ps -> GUI ps ps
type	HideFunction  ps = ps -> GUI ps ps
type	EventFunction ps = IOSt ps -> SchedulerEvent -> IO (Bool, Maybe DeviceEvent, SchedulerEvent)
type	DoIOFunction  ps = DeviceEvent -> ps -> GUI ps ps



--	Creation of an initial context:

initialContext :: Maybe SystemId -> IO Context
initialContext modalId = do
	osTime <- osGetTime
	newIORef (IOContext
		      { ioevents          = []
		      , ionr              = initSystemId
		      , ioidtable         = Map.empty
		      , ioprocesses 	  = []
		      , ioismodal	  = modalId
		      , ioinputtrack      = Nothing
		      , iotimertable      = initialTimerTable
		      , ioostime	  = osTime
		      }
		  )


--	Creation of an initial, empty IOSt:


emptyIOSt :: SystemId -> DocumentInterface
				-> [ProcessAttribute ps] -> (ps -> GUI ps ps) -> Context -> IO (IOSt ps)

emptyIOSt ioId documentInterface processAtts initIO context
	= do {
		wMetrics <- osDefaultWindowMetrics;
		return IOSt
			  { ioid           = ioId
			  , iodevicefuncs  = []
			  , iodevices	   = []
			  , ioatts	   = processAtts
			  , ioruntime      = Running
			  , ioosdinfo	   = emptyOSDInfo documentInterface
  			  , iooswmetrics   = wMetrics
			  , iocontext      = context
			  , ioinit         = initIO
			  }
	  }

--	Access rules to the IOSt:


--	Access rules to process attributes:

ioStGetProcessAttributes :: IOSt ps -> [ProcessAttribute ps]
ioStGetProcessAttributes ioState = ioatts ioState

ioStSetProcessAttributes :: [ProcessAttribute ps] -> IOSt ps -> IOSt ps
ioStSetProcessAttributes atts ioState = ioState {ioatts=atts}


--	Access rules to the initial actions:

ioStGetInitIO :: IOSt ps -> ps -> GUI ps ps
ioStGetInitIO ioState = ioinit ioState
	
ioStSetInitIO :: (ps -> GUI ps ps) -> IOSt ps -> IOSt ps
ioStSetInitIO initIO ioState = ioState {ioinit=initIO}


--	Access rules to InputTrack:

ioContextGetInputTrack :: IOContext -> Maybe InputTrack
ioContextGetInputTrack ioContext = ioinputtrack ioContext

ioContextSetInputTrack :: Maybe InputTrack -> IOContext -> IOContext
ioContextSetInputTrack inputtrack ioContext = ioContext{ioinputtrack=inputtrack}

ioStGetInputTrack :: GUI ps (Maybe InputTrack)
ioStGetInputTrack = accIOStContext ioContextGetInputTrack

ioStSetInputTrack :: Maybe InputTrack -> GUI ps ()
ioStSetInputTrack inputtrack = appIOStContext (ioContextSetInputTrack inputtrack)


--	Access rules to RuntimeState:

ioStClosed :: IOSt ps -> Bool
ioStClosed ioState = ioruntime ioState == Closed

ioStGetRuntimeState :: IOSt ps -> RuntimeState
ioStGetRuntimeState ioState = ioruntime ioState

ioStSetRuntimeState :: RuntimeState -> IOSt ps -> IOSt ps
ioStSetRuntimeState runtime ioState = ioState {ioruntime=runtime}


--	Access rules to IOIsModal:

ioStGetIOIsModal :: IOContext -> Maybe SystemId
ioStGetIOIsModal iocontext = ioismodal iocontext

ioStSetIOIsModal :: Maybe SystemId -> IOContext -> IOContext
ioStSetIOIsModal optId iocontext = iocontext{ioismodal=optId}


--	Access rules to IdTable:

ioStGetIdTable :: GUI ps IdTable
ioStGetIdTable = accIOStContext ioContextGetIdTable

ioStSetIdTable :: IdTable -> GUI ps ()
ioStSetIdTable idTable = appIOStContext (ioContextSetIdTable idTable)


--	Access rules to TimerTable:

ioStGetTimerTable :: GUI ps TimerTable
ioStGetTimerTable = accIOStContext ioContextGetTimerTable

ioStSetTimerTable :: TimerTable  -> GUI ps ()
ioStSetTimerTable iotimertable = appIOStContext (ioContextSetTimerTable iotimertable)


--	Access rules to the OSEvents environment:

ioStTakeEvent :: GUI ps (Maybe SchedulerEvent)
ioStTakeEvent = updIOStContext ioContextTakeEvent

ioStAppendEvents :: [SchedulerEvent] -> GUI ps ()
ioStAppendEvents es = appIOStContext (ioContextAppendEvents es)

ioStInsertEvents :: [SchedulerEvent] -> GUI ps ()
ioStInsertEvents es = appIOStContext (ioContextInsertEvents es)


--	Access rules to DocumentInterface:

ioStGetDocumentInterface :: IOSt ps -> DocumentInterface
ioStGetDocumentInterface ioState@(IOSt {ioosdinfo=ioosdinfo}) = getOSDInfoDocumentInterface ioosdinfo


--	Access rules to OSDInfo:

ioStGetOSDInfo :: IOSt ps -> OSDInfo
ioStGetOSDInfo ioState@(IOSt {ioosdinfo=ioosdinfo}) = ioosdinfo

ioStSetOSDInfo :: OSDInfo -> IOSt ps -> IOSt ps
ioStSetOSDInfo osdInfo ioState = ioState {ioosdinfo=osdInfo}


--	Access to the SystemId of the IOSt:

ioStGetIOId :: IOSt ps -> SystemId
ioStGetIOId ioState = ioid ioState


--	Access to the max SystemId of the IOSt:

ioStGetMaxIONr :: GUI ps SystemId
ioStGetMaxIONr = accIOStContext ioContextGetMaxIONr

ioStSetMaxIONr :: SystemId -> GUI ps ()
ioStSetMaxIONr maxId = appIOStContext (ioContextSetMaxIONr maxId)

ioStNewMaxIONr :: GUI ps SystemId
ioStNewMaxIONr = updIOStContext ioContextNewMaxIONr


--	Access to the OSWindowMetrics of the IOSt:

ioStGetOSWindowMetrics :: IOSt ps -> OSWindowMetrics
ioStGetOSWindowMetrics ioState = iooswmetrics ioState


--	Access rules to the ProcessEventHandlers:

ioStEmptyProcesses :: GUI ps Bool
ioStEmptyProcesses = accIOStContext ioContextEmptyProcesses

ioStGetProcesses :: GUI ps [PSt]
ioStGetProcesses = accIOStContext ioContextGetProcesses

ioStSetProcess :: PSt -> GUI ps ()
ioStSetProcess p = appIOStContext (ioContextSetProcess p)

ioStRemoveProcess :: SystemId -> GUI ps ()
ioStRemoveProcess ioId = appIOStContext (ioContextRemoveProcess ioId)


--	Access to the Context of the IOSt:

ioStGetContext :: IOSt ps -> Context
ioStGetContext ioState = iocontext ioState

accIOStContext :: (IOContext -> x) -> GUI ps x
accIOStContext fun = GUI (\ioSt -> do
    iocontext <- readIORef (ioStGetContext ioSt)
    return (fun iocontext,ioSt))

appIOStContext :: (IOContext -> IOContext) -> GUI ps ()
appIOStContext fun = GUI (\ioSt -> do
    modifyIORef (ioStGetContext ioSt) fun
    return ((), ioSt))
    
updIOStContext :: (IOContext -> (a,IOContext)) -> GUI ps a
updIOStContext fun = GUI (\ioSt -> do
	iocontext <- readIORef (ioStGetContext ioSt)
	let (r, iocontext1) = fun iocontext
	writeIORef (ioStGetContext ioSt) iocontext1
	return (r,ioSt))


--	Access to the DeviceFunctions:

ioStGetDeviceFunctions :: IOSt ps -> [DeviceFunctions ps]
ioStGetDeviceFunctions ioState = iodevicefuncs ioState

ioStSetDeviceFunctions :: DeviceFunctions ps -> IOSt ps -> IOSt ps
ioStSetDeviceFunctions funcs ioState
	= ioState {iodevicefuncs=setdevicefunctions (priorityDevice (dDevice funcs)) (dDevice funcs) funcs (iodevicefuncs ioState)}
	where
		setdevicefunctions :: Int -> Device -> DeviceFunctions ps -> [DeviceFunctions ps] -> [DeviceFunctions ps]
		setdevicefunctions p device funcs fs@(dfunc:dfuncs)
			| device==dDevice dfunc
				= funcs:dfuncs
			| p>priorityDevice (dDevice dfunc)
				= funcs:fs
			| otherwise
				= dfunc:setdevicefunctions p device funcs dfuncs
		setdevicefunctions _ _ funcs _
			= [funcs]

ioStRemoveDeviceFunctions :: Device -> IOSt ps -> IOSt ps
ioStRemoveDeviceFunctions device ioState
	= ioState {iodevicefuncs=removedevicefunctions device (iodevicefuncs ioState)}
	where
		removedevicefunctions :: Device -> [DeviceFunctions ps] -> [DeviceFunctions ps]
		removedevicefunctions device (dfunc:dfuncs)
			| device==dDevice dfunc
				= dfuncs
			| otherwise
				= dfunc:removedevicefunctions device dfuncs
		removedevicefunctions _ empty
			= empty

--	Access to the DeviceSystemStates:

ioStHasDevice :: Device -> IOSt ps -> Bool
ioStHasDevice d ioState
	= devicesHaveDevice d (iodevices ioState)
	where
		devicesHaveDevice :: Device -> [DeviceSystemState ps] -> Bool
		devicesHaveDevice d (dState:dStates) = toDevice dState==d || devicesHaveDevice d dStates
		devicesHaveDevice _ _                = False

ioStGetDevices :: IOSt ps -> [Device]
ioStGetDevices ioState = map toDevice (iodevices ioState)

ioStGetDevice :: Device -> IOSt ps -> (Bool,DeviceSystemState ps)
ioStGetDevice d ioState = devicesGetDevice d (iodevices ioState)
	where
		devicesGetDevice :: Device -> [DeviceSystemState ps] -> (Bool,DeviceSystemState ps)
		devicesGetDevice d (dState:dStates)
			| toDevice dState==d 	= (True,dState)
			| otherwise 		= devicesGetDevice d dStates				  
		devicesGetDevice _ [] = (False,undefined)

ioStRemoveDevice :: Device -> IOSt ps -> IOSt ps
ioStRemoveDevice d ioState
	= ioState {iodevices=devicesRemoveDevice d (iodevices ioState)}
	where
		devicesRemoveDevice :: Device -> [DeviceSystemState ps] -> [DeviceSystemState ps]
		devicesRemoveDevice d (dState:dStates)
			| toDevice dState==d = dStates
			| otherwise          = dState:devicesRemoveDevice d dStates
		devicesRemoveDevice _ dStates
			= dStates

ioStSetDevice :: DeviceSystemState ps -> IOSt ps -> IOSt ps
ioStSetDevice d ioState
	= let
		device = toDevice d
		ds     = devicesSetDevice (priorityDevice device) device d (iodevices ioState)
	  in	ioState {iodevices=ds}
	where
		devicesSetDevice :: Int -> Device -> DeviceSystemState ps -> [DeviceSystemState ps] -> [DeviceSystemState ps]
		devicesSetDevice p device dState2 ds@(dState1:dStates)
			| device1==device
				= dState2:dStates
			| p>priorityDevice device1
				= dState2:ds
			| otherwise
				= dState1:devicesSetDevice p device dState2 dStates
			where
				device1 = toDevice dState1
		devicesSetDevice _ _ dState _
			= [dState]


--	Access to IOContext:

--	Access rules to the OSEvents environment:

ioContextTakeEvent :: IOContext -> (Maybe SchedulerEvent, IOContext)
ioContextTakeEvent iocontext =
	case ioevents iocontext of
		[] 	-> (Nothing,iocontext)
		(e:es) 	-> (Just e ,iocontext{ioevents=es})

ioContextAppendEvents :: [SchedulerEvent] -> IOContext -> IOContext
ioContextAppendEvents es iocontext = iocontext {ioevents=ioevents iocontext ++ es}

ioContextInsertEvents :: [SchedulerEvent] -> IOContext -> IOContext
ioContextInsertEvents es iocontext = iocontext {ioevents=es ++ ioevents iocontext}


--	Access to the max SystemId of IOContext:

ioContextGetMaxIONr :: IOContext -> SystemId
ioContextGetMaxIONr iocontext = ionr iocontext

ioContextSetMaxIONr :: SystemId -> IOContext -> IOContext
ioContextSetMaxIONr maxId iocontext = iocontext {ionr=maxId}

ioContextNewMaxIONr :: IOContext -> (SystemId,IOContext)
ioContextNewMaxIONr iocontext
	= let (maxId1,newMaxId) = incrSystemId (ionr iocontext)
	  in  (newMaxId,iocontext {ionr=maxId1})


--	Access to the context time:

ioContextGetTime :: IOContext -> OSTime
ioContextGetTime iocontext = ioostime iocontext

ioContextSetTime :: OSTime -> IOContext -> IOContext
ioContextSetTime osTime iocontext = iocontext {ioostime=osTime}

	
--	Access rules to IdTable:

ioContextGetIdTable :: IOContext -> IdTable
ioContextGetIdTable iocontext = ioidtable iocontext

ioContextSetIdTable :: IdTable -> IOContext -> IOContext
ioContextSetIdTable idTable iocontext
	= iocontext {ioidtable=idTable}


--	Access rules to TimerTable:

ioContextGetTimerTable :: IOContext -> TimerTable
ioContextGetTimerTable iocontext = iotimertable iocontext

ioContextSetTimerTable :: TimerTable -> IOContext -> IOContext
ioContextSetTimerTable iotimertable iocontext
	= iocontext {iotimertable=iotimertable}	


--	Access rules to the ProcessEventHandlers:

ioContextEmptyProcesses :: IOContext -> Bool
ioContextEmptyProcesses iocontext = null (ioprocesses iocontext)

ioContextGetProcesses :: IOContext -> [PSt]
ioContextGetProcesses iocontext = ioprocesses iocontext

ioContextSetProcess :: PSt -> IOContext -> IOContext
ioContextSetProcess theP ioContext@(IOContext {ioprocesses=ioprocesses}) =
	ioContext {ioprocesses=setProcess theP ioprocesses}
	where
		setProcess :: PSt -> [PSt] -> [PSt]
		setProcess theP@(PSt _ theIOSt) (p@(PSt _ ioSt):ps)
			| ioid ioSt == ioid theIOSt = if ioStClosed theIOSt then ps else theP : ps
			| otherwise		    = p : setProcess theP ps
		setProcess theP@(PSt _ theIOSt) [] = if ioStClosed theIOSt then [] else [theP]

ioContextRemoveProcess :: SystemId -> IOContext -> IOContext
ioContextRemoveProcess ioId ioContext@(IOContext {ioprocesses=ioprocesses}) =
	ioContext {ioprocesses=removeProcess ioId ioprocesses}
	where
		removeProcess :: SystemId -> [PSt] -> [PSt]
		removeProcess ioId (p@(PSt _ ioSt):ps)
			| ioid ioSt == ioId 	= ps
			| otherwise	    	= p : removeProcess ioId ps
		removeProcess _ []  = []
