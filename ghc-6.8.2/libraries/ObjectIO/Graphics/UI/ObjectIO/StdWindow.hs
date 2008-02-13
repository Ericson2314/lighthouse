-----------------------------------------------------------------------------
-- |
-- Module      :  StdWindow
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdWindow defines functions on windows.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdWindow
		( 
		-- * Opening windows
		  Windows(..), Dialogs(..),
		  
		-- * Closing windows
		  closeWindow, closeActiveWindow,
		  
		-- * Active window
		  setActiveWindow, getActiveWindow,
		  
		-- * Active control
		  setActiveControl, getActiveControl,
		  
		-- * Windows and dialogs stacking
		  stackWindow, getWindowStack, getWindowsStack, getDialogsStack,
		  
		-- * Layout attributes
		
		-- ** Default
		  getDefaultHMargin, getDefaultVMargin, getDefaultItemSpace,
		  
		-- ** Current
		  getWindowHMargin, getWindowVMargin, getWindowItemSpace,
		
		-- * Enable\/Disable windows
		  enableWindow, disableWindow
		, enableWindowMouse, disableWindowMouse, setWindowMouseSelectState
		, enableWindowKeyboard, disableWindowKeyboard, setWindowKeyboardSelectState
		, getWindowSelectState, getWindowMouseSelectState, getWindowKeyboardSelectState,
		
		-- * Mouse and keyboard event filters
		  getWindowMouseStateFilter, getWindowKeyboardStateFilter
		, setWindowMouseStateFilter, setWindowKeyboardStateFilter,
		
		-- * Drawing
		  drawInWindow, updateWindow
		, setWindowLook, getWindowLook,
		
		-- * Positioning & resizing
		  setWindowPos, getWindowPos
		, moveWindowViewFrame, getWindowViewFrame                 
		, setWindowViewSize, getWindowViewSize
		, setWindowOuterSize, getWindowOuterSize
		, setWindowViewDomain, getWindowViewDomain,
		
		-- * Scroll functions
		  setWindowScrollFunction, getWindowScrollFunction,
		
		-- * Window title
		  setWindowTitle, getWindowTitle,
		  
		-- * Window mouse cursor
		  setWindowCursor, getWindowCursor,
		
		-- * \"Ok\" and \"Cancel\" buttons
		  getWindowOk, getWindowCancel,
		  
		-- * Carret pos
		  setWindowCaretPos, getWindowCaretPos,
		  
		-- * Visible module
		  module Graphics.UI.ObjectIO.StdWindowDef
		) where



import Data.List(find)
import qualified Data.Map as Map
import Control.Monad(when)
import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Control.Validate
import Graphics.UI.ObjectIO.Control.Layout(layoutControls)
import Graphics.UI.ObjectIO.Control.Relayout(relayoutControls)
import Graphics.UI.ObjectIO.Control.Internal(enablecontrols,disablecontrols)
import qualified Graphics.UI.ObjectIO.Control.Pos as CP
import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.Process.Scheduler(handleContextOSEvent)
import Graphics.UI.ObjectIO.StdControlClass
import Graphics.UI.ObjectIO.StdWindowDef
import Graphics.UI.ObjectIO.StdWindowAttribute
import Graphics.UI.ObjectIO.StdSystem(maxScrollWindowSize)
import Graphics.UI.ObjectIO.StdId(getParentId)
import Graphics.UI.ObjectIO.Window.Access
import Graphics.UI.ObjectIO.Window.Create
import Graphics.UI.ObjectIO.Window.Device
import Graphics.UI.ObjectIO.Window.Dispose
import Graphics.UI.ObjectIO.Window.Validate
import Graphics.UI.ObjectIO.Window.ClipState(validateWindowClipState, forceValidWindowClipState)
import qualified Graphics.UI.ObjectIO.Window.Draw as WD
import qualified Graphics.UI.ObjectIO.Window.Update as WU
import Graphics.UI.ObjectIO.Window.Draw(drawWindowLook)
import Graphics.UI.ObjectIO.Window.Update(updateWindowBackgrounds)
import Graphics.UI.ObjectIO.KeyFocus(getCurrentFocusItem)
import Graphics.UI.ObjectIO.OS.Picture(Draw(..))
import Graphics.UI.ObjectIO.OS.Window
import Graphics.UI.ObjectIO.OS.Event
import Graphics.UI.ObjectIO.OS.System(osStripOuterSize)
import Graphics.UI.ObjectIO.OS.ToolBar(OSToolbar(..))



stdWindowFatalError :: String -> String -> x
stdWindowFatalError function error
	= dumpFatalError function "StdWindow" error

class Windows wdef where
	openWindow :: ls -> wdef ls ps -> ps -> GUI ps ps

	
--	Functions applied to non-existent windows or unknown ids have no effect.
class Dialogs ddef where
	openDialog :: ls -> ddef ls ps -> ps -> GUI ps ps
	openModalDialog	:: ls -> ddef ls ps -> ps -> GUI ps (ps, (Maybe ls))

 
instance Controls c => Windows (Window c) where	
	openWindow ls (Window title controls atts) ps
		= do {
			ps1 <- dOpen windowFunctions ps;
			isZero <- accIOEnv checkZeroWindowBound;
			if isZero then throwGUI ErrorViolateDI
			else do {
			   maybe_okId <- validateWindowId maybe_id;
			   case maybe_okId of
			   	Nothing -> throwGUI ErrorIdsInUse
			   	Just okId -> do
				  	cs   <- controlToHandles controls
				  	it   <- ioStGetIdTable
				  	ioId <- accIOEnv ioStGetIOId
				  	let itemHs = map controlStateToWElementHandle cs
				  	(case controlIdsAreConsistent ioId okId itemHs it of
				  		Nothing -> throwGUI ErrorIdsInUse
				  		Just it -> do
							ioStSetIdTable (Map.insert okId (IdParent {idpIOId=ioId,idpDevice=WindowDevice,idpId=okId}) it)
							ps2 <- openwindow okId (WindowLSHandle {wlsState=ls,wlsHandle=initWindowHandle title Modeless IsWindow NoWindowInfo itemHs atts}) ps1
							appIOEnv decreaseWindowBound
							return ps2)
				  }
			  }
		where
			maybe_id = getWindowIdAttribute atts


instance Controls c => Dialogs (Dialog c) where
	openDialog ls (Dialog title controls atts) ps
		= do {
			ps1 <- dOpen windowFunctions ps;
			maybe_okId <- validateWindowId maybe_id;
			case maybe_okId of
				Nothing   -> throwGUI ErrorIdsInUse
				Just okId -> do
					cs   <- controlToHandles controls
					it   <- ioStGetIdTable					
					ioId <- accIOEnv ioStGetIOId
					let itemHs          = map controlStateToWElementHandle cs				    
				    	(case controlIdsAreConsistent ioId okId itemHs it of
				    		Nothing -> throwGUI ErrorIdsInUse
				    		Just it -> do
							ioStSetIdTable (Map.insert okId (IdParent {idpIOId=ioId,idpDevice=WindowDevice,idpId=okId}) it)
							openwindow okId (WindowLSHandle {wlsState=ls,wlsHandle=initWindowHandle title Modeless IsDialog NoWindowInfo itemHs atts}) ps1)
		}
		where
			maybe_id = getWindowIdAttribute atts


	openModalDialog ls (Dialog title controls atts) ps
		= do {
			ps1 <- dOpen windowFunctions ps;
			maybe_okId <- validateWindowId maybe_id;
			case maybe_okId of
				Nothing   -> throwGUI ErrorIdsInUse
				Just okId -> do
					cs   <- controlToHandles controls
					it   <- ioStGetIdTable
					ioId <- accIOEnv ioStGetIOId
					let itemHs          = map controlStateToWElementHandle cs
				    	(case controlIdsAreConsistent ioId okId itemHs it of
				    		Nothing -> throwGUI ErrorIdsInUse
				    		Just it -> do
							ioStSetIdTable (Map.insert okId (IdParent {idpIOId=ioId,idpDevice=WindowDevice,idpId=okId}) it)
							openModalWindow okId (WindowLSHandle {wlsState=ls,wlsHandle=initWindowHandle title Modal IsDialog NoWindowInfo itemHs atts}) ps1)
		}
		where
			maybe_id = getWindowIdAttribute atts


getWindowIdAttribute :: [WindowAttribute ls ps] -> Maybe Id
getWindowIdAttribute atts
	| hasIdAtt  = Just (getWindowIdAtt idAtt)
	| otherwise = Nothing
	where
		(hasIdAtt,idAtt) = cselect isWindowId undefined atts


-- | closeWindow closes the indicated window.
closeWindow :: Id -> ps -> GUI ps ps
closeWindow id ps
	= disposeWindow (toWID id) ps

-- | closeActiveWindow closes the current active window.
closeActiveWindow :: ps -> GUI ps ps
closeActiveWindow ps
	= do {
		maybeId <- getActiveWindow;
		if   isNothing maybeId
		then return ps
		else closeWindow (fromJust maybeId) ps
	  }

-- | Call this function to activate and restore the window so that it is visible and available to the user.
setActiveWindow :: Id -> GUI ps ()
setActiveWindow winId = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let windows	= windowSystemStateGetWindowHandles wDevice
	      in
	     	if not (hasWindowHandlesWindow wid windows) then return ()	-- Indicated window does not exist
		else case getWindowHandlesActiveWindow windows of
			Just wids | wId wids /= winId ->
				let (modal,modeless) = span isModalWindow (whsWindows windows)
				in if any (identifyWindowStateHandle wid) modal
				   then return ()
				   else do
					   osdInfo <- accIOEnv ioStGetOSDInfo
					   let isSDI = getOSDInfoDocumentInterface osdInfo==SDI
					   let (framePtr,clientPtr) = case getOSDInfoOSInfo osdInfo of
						  Just info -> (osFrame info, osClient info)
						  _         -> (osNoWindowPtr,osNoWindowPtr)
					   (if null modal	-- There are no modal windows, so put activated window in front
					    then let
						   (_,wsH,others) = remove (identifyWindowStateHandle wid) undefined modeless
						   shown = getWindowStateHandleShow wsH
						   wids = getWindowStateHandleWIDS wsH
						   activatePtr = if isSDI && wPtr wids==clientPtr then framePtr else wPtr wids	-- Do not activate SDI client, but SDI frame
						 in do
						   appIOEnv (ioStSetDevice (WindowSystemState windows{whsWindows=wsH:others}))
						   liftIO (if shown then return [] else osShowWindow activatePtr True)
						   context <- accIOEnv ioStGetContext
						   delayinfo <- liftIO (osActivateWindow osdInfo activatePtr (handleOSEvent context))
						   bufferDelayedEvents delayinfo
					    else let		-- There are modal windows, so put activated window behind last modal
						   (befModals,lastModal) = initLast modal
						   modalWIDS = getWindowStateHandleWIDS lastModal
						   (_,wsH,others) = remove (identifyWindowStateHandle wid) undefined modeless
						   shown = getWindowStateHandleShow wsH
						   modelessWIDS	= getWindowStateHandleWIDS wsH
						   activatePtr = if isSDI && wPtr modelessWIDS==clientPtr then framePtr else wPtr modelessWIDS	-- Do not activate SDI client, but SDI frame
						 in do
						   appIOEnv (ioStSetDevice (WindowSystemState windows{whsWindows=befModals++(lastModal:wsH:others)}))
						   liftIO (if shown then return [] else osShowWindow activatePtr True)
						   context <- accIOEnv ioStGetContext
						   delayinfo <- liftIO (osStackWindow activatePtr (wPtr modalWIDS) (handleOSEvent context))
						   bufferDelayedEvents delayinfo)
			_ -> return ())
	where
		wid = toWID winId


-- | Call this function to obtain an Id of the active window.
getActiveWindow :: GUI ps (Maybe Id)
getActiveWindow
	= do {
		(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice);
		if   not found
		then return Nothing
		else let
			windows    = windowSystemStateGetWindowHandles wDevice
			activeWIDS = getWindowHandlesActiveWindow windows
		in do {
			appIOEnv (ioStSetDevice (WindowSystemState windows));
			return (fmap wId activeWIDS)
		   }
	  }
	  
-- | Claims the input focus. The input focus directs all subsequent keyboard input to this window. 
-- Any window that previously had the input focus loses it. setActiveControl makes the indicated
-- control active only if its parent window is already active.
setActiveControl :: Id -> GUI ps ()
setActiveControl controlId = do
	mb_parentId <- getParentId controlId
	mb_activeId <- getActiveWindow
	(case (mb_parentId,mb_activeId) of
		(Just parentId,Just activeId) | parentId==activeId -> do
			(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
			(if not found then return ()
			 else let 
				windows	= windowSystemStateGetWindowHandles wDevice
				(found,wsH,windows1) = getWindowHandlesWindow (toWID activeId) windows
			      in
				(if not found
				 then stdWindowFatalError "setActiveControl" "parent window could not be located"
				 else do
					(delayinfo,wsH)	<- liftIO (setactivecontrol controlId wsH)
					let windows2 = setWindowHandlesWindow wsH windows1
					appIOEnv (ioStSetDevice (WindowSystemState windows2))
					bufferDelayedEvents delayinfo))
		_	-> return ())
	where
		setactivecontrol :: Id -> WindowStateHandle ps -> IO ([DelayActivationInfo],WindowStateHandle ps)
		setactivecontrol controlId (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) =
			let (found,itemNr,itemPtr,itemHs)	= getWElementHandlesItemNrPtr controlId (whItems wH)
			in if not found
			   then stdWindowFatalError "setActiveControl" "indicated control could not be located"
			   else do
			   		delayinfo <- osActivateControl (wPtr wids) itemPtr
					return (delayinfo,(WindowStateHandle wids (Just wlsH{wlsHandle=wH{whItems=itemHs}})))

			where
				getWElementHandlesItemNrPtr :: Id -> [WElementHandle ls ps] -> (Bool,Int,OSWindowPtr,[WElementHandle ls ps])
				getWElementHandlesItemNrPtr id (itemH:itemHs) =
					let (found,itemNr,itemPtr,itemH1) = getWElementHandleItemNrPtr id itemH
					in if found then (found,itemNr,itemPtr,itemH1:itemHs)
					   else let (found,itemNr,itemPtr,itemHs1) = getWElementHandlesItemNrPtr id itemHs
						in (found,itemNr,itemPtr,itemH1:itemHs1)
					where
						getWElementHandleItemNrPtr :: Id -> WElementHandle ls ps -> (Bool,Int,OSWindowPtr,WElementHandle ls ps)
						getWElementHandleItemNrPtr id itemH@(WItemHandle {wItemNr=wItemNr,wItems=itemHs,wItemId=itemId,wItemPtr=wItemPtr})
							| itemId /= Just id =
								let (found,itemNr,itemPtr,itemHs1) = getWElementHandlesItemNrPtr id itemHs
								in (found,itemNr,itemPtr,itemH{wItems=itemHs1})
							| otherwise = (True,wItemNr,wItemPtr,itemH)
						getWElementHandleItemNrPtr itemNr (WListLSHandle itemHs) =
							let (found,itemNr,itemPtr,itemHs1) = getWElementHandlesItemNrPtr id itemHs
							in (found,itemNr,itemPtr,WListLSHandle itemHs1)
						getWElementHandleItemNrPtr itemNr (WExtendLSHandle exLS itemHs) =
							let (found,itemNr,itemPtr,itemHs1) = getWElementHandlesItemNrPtr id itemHs
							in (found,itemNr,itemPtr,WExtendLSHandle exLS itemHs)
						getWElementHandleItemNrPtr itemNr (WChangeLSHandle chLS itemHs) =
							let (found,itemNr,itemPtr,itemHs1) = getWElementHandlesItemNrPtr id itemHs
							in (found,itemNr,itemPtr,WChangeLSHandle chLS itemHs)
				getWElementHandlesItemNrPtr _ _ = (False,0,osNoWindowPtr,[])
		setactivecontrol _ _ = stdWindowFatalError "setActiveControl" "unexpected window placeholder argument"



-- | Retrieves an Id of the control that currently has the input focus.
getActiveControl :: GUI ps (Maybe Id)
getActiveControl = do
	mb_activeId <- getActiveWindow
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(case mb_activeId of
		Just activeId | found ->
			let
				windows	= windowSystemStateGetWindowHandles wDevice
				(hasWindow,wsH,windows1) = getWindowHandlesWindow (toWID activeId) windows
				keyfocus = getWindowStateHandleKeyFocus wsH
			in
				if not hasWindow then stdWindowFatalError "getActiveControl" "active window could not be located"
				else case getCurrentFocusItem keyfocus of
					Nothing     -> return Nothing
					Just itemNr -> return (getControlIdFromItemNr itemNr wsH)
		Nothing -> return Nothing)
	where
		getControlIdFromItemNr :: Int -> WindowStateHandle ps -> Maybe Id
		getControlIdFromItemNr itemNr (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) =
			getWElementHandlesIdFromItemNr itemNr (whItems wH)			
			where
				getWElementHandlesIdFromItemNr :: Int -> [WElementHandle ls ps] -> Maybe Id
				getWElementHandlesIdFromItemNr itemNr (itemH:itemHs)
					| isJust foundId = foundId
					| otherwise	 = getWElementHandlesIdFromItemNr itemNr itemHs
					where
						foundId = getWElementHandleIdFromItemNr itemNr itemH

						getWElementHandleIdFromItemNr :: Int -> WElementHandle ls ps -> Maybe Id
						getWElementHandleIdFromItemNr itemNr (WItemHandle {wItemNr=wItemNr,wItems=itemHs,wItemId=itemId})
							| itemNr/=wItemNr = getWElementHandlesIdFromItemNr itemNr itemHs
							| otherwise 	  = itemId
						getWElementHandleIdFromItemNr itemNr (WListLSHandle itemHs) =
							getWElementHandlesIdFromItemNr itemNr itemHs
						getWElementHandleIdFromItemNr itemNr (WExtendLSHandle exLS itemHs) =
							getWElementHandlesIdFromItemNr itemNr itemHs
						getWElementHandleIdFromItemNr itemNr (WChangeLSHandle chLS itemHs) =
							getWElementHandlesIdFromItemNr itemNr itemHs
				getWElementHandlesIdFromItemNr _ _ = Nothing
		getControlIdFromItemNr _ _ = stdWindowFatalError "getActiveControl" "unexpected window placeholder argument"


-- | stackWindow changes the stacking order of the current windows.
stackWindow :: Id -> Id -> GUI ps ()
stackWindow windowId behindId
	| windowId==behindId = return ()				-- Don't stack a window behind itself
	| otherwise = do
		(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
		(if not found then return ()
		 else let windows	= windowSystemStateGetWindowHandles wDevice
		      in if not (hasWindowHandlesWindow behindWID windows) then return ()		-- Behind window does not exist
			 else let (hasWindow,wsH,windows1) = getWindowHandlesWindow windowWID windows
			      in if not hasWindow || getWindowStateHandleWindowMode wsH==Modal 		-- Stack window does not exist or it's modal, skip
			      then return ()					
			      else do
			     		let (_,_,windows2) = removeWindowHandlesWindow windowWID windows1	-- remove placeholder window
					let wids = getWindowStateHandleWIDS wsH
					let (behindWIDS,windows3) = addBehindWindowHandlesWindow behindWID wsH windows2
					appIOEnv (ioStSetDevice (WindowSystemState windows3))
					context <- accIOEnv ioStGetContext
					delayinfo <- liftIO (osStackWindow (wPtr wids) (wPtr behindWIDS) (handleOSEvent context))
					bufferDelayedEvents delayinfo)
	where
		windowWID	= toWID windowId
		behindWID	= toWID behindId


--	handleOSEvent turns handleContextOSEvent into the form required by osActivateWindow and osStackWindow.
--	(Used by stackWindow, setActiveWindow.)
handleOSEvent :: Context -> OSEvent -> IO ()
handleOSEvent context osEvent = do
	handleContextOSEvent context (ScheduleOSEvent osEvent [])
	return ()


-- | returns list of window and dialog ids in stacking order.
getWindowStack :: GUI ps [(Id,WindowKind)]
getWindowStack = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return []
	 else let windows = windowSystemStateGetWindowHandles wDevice
	      in return (map getWindowIdType (whsWindows windows)))
	where
		getWindowIdType :: WindowStateHandle ps -> (Id,WindowKind)
		getWindowIdType wsH = (wId wids,kind)
			where
				wids = getWindowStateHandleWIDS wsH
				kind = getWindowStateHandleWindowKind wsH

-- | returns list of window ids in stacking order.
getWindowsStack :: GUI ps [Id]
getWindowsStack = do
	id_types <- getWindowStack
	return (filterMap (\(id,kind)->(kind==IsWindow,id)) id_types)

-- | returns list of dialog ids in stacking order.
getDialogsStack :: GUI ps [Id]
getDialogsStack = do
	id_types <- getWindowStack
	return (filterMap (\(id,kind)->(kind==IsDialog,id)) id_types)


--	Return layout attributes and default values.

getDefaultHMargin :: Bool -> GUI ps Int
getDefaultHMargin isWindow
	| isWindow  = return 0
	| otherwise = do
		wMetrics <- accIOEnv ioStGetOSWindowMetrics
		return (osmHorMargin wMetrics)
		
getDefaultVMargin :: Bool -> GUI ps Int
getDefaultVMargin isWindow
	| isWindow  = return 0
	| otherwise = do
		wMetrics <- accIOEnv ioStGetOSWindowMetrics
		return (osmVerMargin wMetrics)
		

getDefaultItemSpace :: GUI ps (Int,Int)
getDefaultItemSpace = do
	wMetrics <- accIOEnv ioStGetOSWindowMetrics
	return (osmHorItemSpace wMetrics,osmVerItemSpace wMetrics)


getWindowHMargin :: Id	-> GUI ps (Maybe (Int,Int))
getWindowHMargin id = do
	(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found then return Nothing
		 else do
			wMetrics <- accIOEnv ioStGetOSWindowMetrics
			let marginAtt = gethmargin wMetrics wsH
			return (Just marginAtt))
	where
		gethmargin :: OSWindowMetrics -> WindowStateHandle ps -> (Int,Int)
		gethmargin wMetrics (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) =
			getWindowHMargins (whKind wH) wMetrics (whAtts wH)
		gethmargin _ _ = stdWindowFatalError "getWindowHMargin" "unexpected window placeholder argument"
		

getWindowVMargin :: Id	-> GUI ps (Maybe (Int,Int))
getWindowVMargin id = do
	(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found then return Nothing
		 else do
			wMetrics <- accIOEnv ioStGetOSWindowMetrics
			let marginAtt = getvmargin wMetrics wsH
			return (Just marginAtt))
	where
		getvmargin :: OSWindowMetrics -> WindowStateHandle ps -> (Int,Int)
		getvmargin wMetrics (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) =
			getWindowVMargins (whKind wH) wMetrics (whAtts wH)
		getvmargin _ _ = stdWindowFatalError "getWindowVMargin" "unexpected window placeholder argument"


getWindowItemSpace :: Id -> GUI ps (Maybe (Int,Int))
getWindowItemSpace id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found then return Nothing
		 else do
			wMetrics <- accIOEnv ioStGetOSWindowMetrics
			let marginAtt = getitemspaces (osmHorItemSpace wMetrics,osmVerItemSpace wMetrics) wsH
			return (Just marginAtt))
	where
		getitemspaces :: (Int,Int) -> WindowStateHandle ps -> (Int,Int)
		getitemspaces (defHSpace,defVSpace) (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) =
			getWindowItemSpaceAtt (snd (cselect isWindowItemSpace (WindowItemSpace defHSpace defVSpace) (whAtts wH)))
		getitemspaces _ _ = stdWindowFatalError "getWindowItemSpace" "unexpected window placeholder argument"


--	Setting the SelectState of windows.

enableWindow :: Id -> GUI ps ()
enableWindow id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found || getWindowStateHandleWindowKind wsH/=IsWindow || getWindowStateHandleSelect wsH
	         then return ()
	         else do
			osdInfo <- accIOEnv ioStGetOSDInfo
			wMetrics <- accIOEnv ioStGetOSWindowMetrics
		  	let wsH1 = setWindowStateHandleSelect True wsH
			wsH2 <- liftIO (enableControls osdInfo wMetrics wsH1)
			appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH2 windows1))))
	where
		enableControls :: OSDInfo -> OSWindowMetrics -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		enableControls osdInfo wMetrics (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
			let framePtr = case getOSDInfoOSInfo osdInfo of
				Just info -> osFrame info
		  		Nothing   -> osNoWindowPtr
			wH <- enablecontrols [] True wMetrics (wPtr wids) wH
			let scrollInfo = case whWindowInfo wH of
				info@(WindowInfo {}) -> (isJust (windowHScroll info),isJust (windowVScroll info))
				NoWindowInfo	     -> (False,False)
			osEnableWindow (if getOSDInfoDocumentInterface osdInfo==SDI then framePtr else wPtr wids) scrollInfo False
			osInvalidateWindow (wPtr wids)
			return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))

disableWindow :: Id -> GUI ps ()
disableWindow id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found || getWindowStateHandleWindowKind wsH/=IsWindow || not (getWindowStateHandleSelect wsH)
		 then return ()
		 else do
			osdInfo <- accIOEnv ioStGetOSDInfo
			wMetrics <- accIOEnv ioStGetOSWindowMetrics
		  	let wsH1 = setWindowStateHandleSelect False wsH
			wsH2 <- liftIO (disableControls osdInfo wMetrics wsH1)
			appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH2 windows1))))
	where
		disableControls :: OSDInfo -> OSWindowMetrics -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		disableControls osdInfo wMetrics (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
			let framePtr = case getOSDInfoOSInfo osdInfo of
				Just info -> osFrame info
		  		Nothing   -> osNoWindowPtr
			wH <- disablecontrols [] True wMetrics (wPtr wids) wH
			let scrollInfo = case whWindowInfo wH of
				info@(WindowInfo {}) -> (isJust (windowHScroll info),isJust (windowVScroll info))
				NoWindowInfo 	     -> (False,False)
			osDisableWindow (if getOSDInfoDocumentInterface osdInfo==SDI then framePtr else wPtr wids) scrollInfo False
			osInvalidateWindow (wPtr wids)
			return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))

enableWindowMouse  :: Id -> GUI ps ()
enableWindowMouse  id = setWindowMouseSelectState Able   id

disableWindowMouse :: Id -> GUI ps ()
disableWindowMouse id = setWindowMouseSelectState Unable id

setWindowMouseSelectState :: SelectState -> Id -> GUI ps ()
setWindowMouseSelectState selectState id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in
		if not found
		then appIOEnv (ioStSetDevice (WindowSystemState windows))
		else if getWindowStateHandleWindowKind wsH /= IsWindow
		     then appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
		     else
			let wsH1 = setMouseSelectState selectState wsH
			in appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH1 windows1))))
	where
		setMouseSelectState :: SelectState -> WindowStateHandle ps -> WindowStateHandle ps
		setMouseSelectState selectState (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) =
			WindowStateHandle wids (Just wlsH{wlsHandle=wH{whAtts=setMouseSelectStateAtt selectState (whAtts wH)}})
			where
				setMouseSelectStateAtt :: SelectState -> [WindowAttribute ls ps] -> [WindowAttribute ls ps]
				setMouseSelectStateAtt selectState atts
					| not found = atts
					| otherwise =
						let (filter,_,fun) = getWindowMouseAtt mouseAtt
						in (WindowMouse filter selectState fun:atts1)
					where
						(found,mouseAtt,atts1) = remove isWindowMouse undefined atts
		setMouseSelectState _ _ = stdWindowFatalError "setWindowMouseSelectState" "unexpected window placeholder argument"


enableWindowKeyboard  :: Id -> GUI ps ()
enableWindowKeyboard  id = setWindowKeyboardSelectState Able   id

disableWindowKeyboard :: Id -> GUI ps ()
disableWindowKeyboard id = setWindowKeyboardSelectState Unable id

setWindowKeyboardSelectState :: SelectState -> Id -> GUI ps ()
setWindowKeyboardSelectState selectState id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found || getWindowStateHandleWindowKind wsH /= IsWindow then return ()
	         else do
			let wsH1 = setKeyboardSelectState selectState wsH
			appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH1 windows1))))
	where
		setKeyboardSelectState :: SelectState -> WindowStateHandle ps -> WindowStateHandle ps
		setKeyboardSelectState selectState (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) =
			WindowStateHandle wids (Just wlsH{wlsHandle=wH{whAtts=setKeyboardSelectStateAtt selectState (whAtts wH)}})
			where
				setKeyboardSelectStateAtt :: SelectState -> [WindowAttribute ls ps] -> [WindowAttribute ls ps]
				setKeyboardSelectStateAtt selectState atts					
					| not found = atts
					| otherwise =
						let (filter,_,fun) = getWindowKeyboardAtt keyAtt
						in (WindowKeyboard filter selectState fun:atts1)
					where
						(found,keyAtt,atts1) = remove isWindowKeyboard undefined atts
		setKeyboardSelectState _ _ =
			stdWindowFatalError "setWindowKeyboardSelectState" "unexpected window placeholder argument"


getWindowSelectState :: Id -> GUI ps (Maybe SelectState)
getWindowSelectState id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let
		windows = windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found || getWindowStateHandleWindowKind wsH/=IsWindow then return Nothing
	 	 else return (Just (if getWindowStateHandleSelect wsH then Able else Unable)))

getWindowMouseSelectState :: Id -> GUI ps (Maybe SelectState)
getWindowMouseSelectState id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found || getWindowStateHandleWindowKind wsH/=IsWindow then return Nothing
		 else return (fmap snd (getWindowMouseAttInfo wsH)))

getWindowMouseAttInfo :: WindowStateHandle ps -> Maybe (MouseStateFilter,SelectState)
getWindowMouseAttInfo (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH})))	
	| not hasMouseAtt = Nothing
	| otherwise	  = Just (filter, selectState)
	where
		(hasMouseAtt,mouseAtt) = cselect isWindowMouse undefined (whAtts wH)
		(filter,selectState,_) = getWindowMouseAtt mouseAtt
getWindowMouseAttInfo _ = stdWindowFatalError "getWindowMouseAttInfo" "unexpected window placeholder argument"


getWindowKeyboardSelectState :: Id -> GUI ps (Maybe SelectState)
getWindowKeyboardSelectState id = do
	(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice);
	(if not found then return Nothing
	 else let 
	     	windows = windowSystemStateGetWindowHandles wDevice
	     	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found || getWindowStateHandleWindowKind wsH /= IsWindow then return Nothing
	         else return (fmap snd (getWindowKeyboardAttInfo wsH)))

getWindowKeyboardAttInfo :: WindowStateHandle ps -> Maybe (KeyboardStateFilter,SelectState)
getWindowKeyboardAttInfo (WindowStateHandle wids (Just (WindowLSHandle {wlsHandle=wH@(WindowHandle {whAtts=whAtts})}))) =
	case find isWindowKeyboard whAtts of
	  Just keyAtt -> let (filter,selectState,_)= getWindowKeyboardAtt keyAtt
	  		 in Just (filter,selectState)
	  Nothing -> Nothing
getWindowKeyboardAttInfo _
	= stdWindowFatalError "getWindowKeyboardAttInfo" "unexpected window placeholder argument"

-- | returns the current mouse event filter
getWindowMouseStateFilter :: Id -> GUI ps (Maybe MouseStateFilter)
getWindowMouseStateFilter id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let
		windows = windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found || getWindowStateHandleWindowKind wsH/=IsWindow then return Nothing
	         else return (fmap fst (getWindowMouseAttInfo wsH)))
		

-- | returns the current keyboard filter
getWindowKeyboardStateFilter :: Id -> GUI ps (Maybe KeyboardStateFilter)
getWindowKeyboardStateFilter id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found || getWindowStateHandleWindowKind wsH/=IsWindow then return Nothing
	         else return (fmap fst (getWindowKeyboardAttInfo wsH)))


-- | Receiving mouse event can be additionally disabled with state filter
setWindowMouseStateFilter :: Id -> MouseStateFilter -> GUI ps ()
setWindowMouseStateFilter id filter = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found || getWindowStateHandleWindowKind wsH/=IsWindow then return ()
	         else let wsH1 = setMouseFilter filter wsH
		      in appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH1 windows1))))
	where
		setMouseFilter :: MouseStateFilter -> WindowStateHandle ps -> WindowStateHandle ps
		setMouseFilter filter (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH@(WindowHandle {whAtts=atts})}))) =
			(WindowStateHandle wids (Just wlsH{wlsHandle=wH{whAtts=setMouseStateFilterAtt filter atts}}))
			where
				setMouseStateFilterAtt :: MouseStateFilter -> [WindowAttribute ls ps] -> [WindowAttribute ls ps]
				setMouseStateFilterAtt filter atts				
					| not found = atts
					| otherwise = (WindowMouse filter select fun:atts)
					where
						(found,mouseAtt,atts) = remove isWindowMouse undefined atts
						(_,select,fun)	      = getWindowMouseAtt mouseAtt
		setMouseFilter _ _
			= stdWindowFatalError "setWindowMouseStateFilter" "unexpected window placeholder argument"


-- | Receiving keyboard event can be additionally disabled with state filter
setWindowKeyboardStateFilter :: Id -> KeyboardStateFilter -> GUI ps ()
setWindowKeyboardStateFilter id filter = do
	(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let
	 	windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found || getWindowStateHandleWindowKind wsH/=IsWindow then return ()
	      	 else let wsH1 = setKeyboardFilter filter wsH
	      	      in appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH1 windows1))))
	where
		setKeyboardFilter :: KeyboardStateFilter -> WindowStateHandle ps -> WindowStateHandle ps
		setKeyboardFilter filter (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH@(WindowHandle {whAtts=whAtts})}))) =
			(WindowStateHandle wids (Just wlsH{wlsHandle=wH{whAtts=setKeyboardStateFilterAtt filter whAtts}}))
			where
				setKeyboardStateFilterAtt :: KeyboardStateFilter -> [WindowAttribute ls ps] -> [WindowAttribute ls ps]
				setKeyboardStateFilterAtt filter atts					
					| not found = atts
					| otherwise = (WindowKeyboard filter select fun:atts)
					where
						(found,keyAtt,atts) = remove isWindowKeyboard undefined atts
						(_,select,fun)	    = getWindowKeyboardAtt keyAtt
		setKeyboardFilter _ _ = stdWindowFatalError "setWindowKeyboardStateFilter" "unexpected window placeholder argument"


drawInWindow :: Id -> Draw x -> GUI ps (Maybe x)
drawInWindow id drawf = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found then do
	     		appIOEnv (ioStSetDevice (WindowSystemState windows))
	     		return Nothing
	      	 else if getWindowStateHandleWindowKind wsH /= IsWindow
	     	      then do
	     	     	appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
	     	        return Nothing
	              else do
	             	wMetrics <- accIOEnv ioStGetOSWindowMetrics
			(x,wsH)	<- liftIO (drawInWindow' wMetrics drawf wsH)
			appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
			return (Just x))
	where
		drawInWindow' :: OSWindowMetrics -> Draw x -> WindowStateHandle ps -> IO (x,WindowStateHandle ps)
		drawInWindow' wMetrics drawf (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
			wH <- validateWindowClipState wMetrics False (wPtr wids) wH
			(x,wH) <- WD.drawInWindow wMetrics (wPtr wids) drawf wH
			return (x, WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
		drawInWindow' _ _ _ = stdWindowFatalError "drawInWindow" "unexpected window placeholder argument"


updateWindow :: Id -> Maybe ViewFrame -> GUI ps ()
updateWindow id maybeViewFrame = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found || getWindowStateHandleWindowKind wsH/=IsWindow then return ()	
		 else do
			wMetrics <- accIOEnv ioStGetOSWindowMetrics
			wsH <- liftIO (updateWindowBackground wMetrics maybeViewFrame wsH)
			appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1))))
	where
		updateWindowBackground :: OSWindowMetrics -> Maybe ViewFrame -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		updateWindowBackground wMetrics maybeViewFrame wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH})))
			| isEmptyRect updArea = return wsH
			| otherwise = do
				wH <- WU.updateWindow wMetrics updInfo wH
				return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
			where
				winSize				= whSize wH
				info				= whWindowInfo wH
				(origin,domainRect,hasScrolls)	= (windowOrigin info,windowDomain info,(isJust (windowHScroll info),isJust (windowVScroll info)))
				visScrolls			= osScrollbarsAreVisible wMetrics domainRect (toTuple winSize) hasScrolls
				contentRect			= getWindowContentRect wMetrics visScrolls (sizeToRect winSize)
				updArea	= case maybeViewFrame of
					Nothing		-> contentRect
					Just rect	-> intersectRects (rectangleToRect (subVector (toVector origin) rect)) contentRect
				updInfo	= UpdateInfo
					{ updWIDS	= wids
					, updWindowArea	= updArea
					, updControls	= []
					, updGContext	= Nothing
					}
		updateWindowBackground _ _ _ = stdWindowFatalError "updateWindow" "unexpected window placeholder argument"


setWindowLook :: Id -> Bool -> (Bool,Look) -> GUI ps ()
setWindowLook wId redraw (sysLook,lookFun) = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else do
		let windows	= windowSystemStateGetWindowHandles wDevice
	  	let (found,wsH,windows1) = getWindowHandlesWindow (toWID wId) windows
		(if not found
		 then appIOEnv (ioStSetDevice (WindowSystemState windows))
		 else if getWindowStateHandleWindowKind wsH /= IsWindow
		     then appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
		     else do
				wMetrics <- accIOEnv ioStGetOSWindowMetrics
				wsH <- liftIO (setwindowlook wMetrics redraw (sysLook,lookFun) wsH)
				appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))))
	where
		setwindowlook :: OSWindowMetrics -> Bool -> (Bool,Look) -> (WindowStateHandle ps) -> IO (WindowStateHandle ps)
		setwindowlook wMetrics redraw (sysLook,lookFun) wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) =
			let 
				lookInfo1 = lookInfo{lookFun=lookFun,lookSysUpdate=sysLook}		
				wH1 = wH{whWindowInfo=windowInfo{windowLook=lookInfo1}}
			in if not redraw
			   then return (WindowStateHandle wids (Just wlsH{wlsHandle=wH1}))
			   else do
					wH2 <- validateWindowClipState wMetrics False (wPtr wids) wH1
					wH3 <- drawWindowLook wMetrics (wPtr wids) (return ()) updState wH2
					return (WindowStateHandle wids (Just wlsH{wlsHandle=wH3}))
			where
				windowInfo				= whWindowInfo wH
				lookInfo				= windowLook windowInfo
				domainRect				= windowDomain windowInfo
				origin					= windowOrigin windowInfo
				hasScrolls				= (isJust (windowHScroll windowInfo),isJust (windowVScroll windowInfo))
				visScrolls				= osScrollbarsAreVisible wMetrics domainRect (toTuple (whSize wH)) hasScrolls
				contentRect				= getWindowContentRect wMetrics visScrolls (sizeToRect (whSize wH))
				wFrame					= posSizeToRectangle origin (rectSize contentRect)
				updState				= rectangleToUpdateState wFrame
		setwindowlook _ _ _ _ = stdWindowFatalError "setWindowLook" "unexpected window placeholder argument"

getWindowLook :: Id -> GUI ps (Maybe (Bool,Look))
getWindowLook id = do
	(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
	(if not found
	 then return Nothing
	 else let 
	 		windows = windowSystemStateGetWindowHandles wDevice
			(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in 
	      	if not found
		then do
			appIOEnv (ioStSetDevice (WindowSystemState windows))
			return Nothing
		else if getWindowStateHandleWindowKind wsH/=IsWindow
		     then do
		     	appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
			return Nothing
		     else
			let
				windowInfo = getWindowStateHandleWindowInfo wsH	
		  		LookInfo{lookFun=lookFun,lookSysUpdate=lookSysUpdate} = windowLook windowInfo
			in do
				appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
				return (Just (lookSysUpdate,lookFun)))


--	Operations that are concerned with the position of windows/dialogues.

-- | The setWindowPos function changes the position the specified window.
-- The position is relative to the upper-left corner of the screen. 
setWindowPos :: Id -> ItemPos -> GUI ps ()
setWindowPos id pos = do
	osdInfo <- accIOEnv ioStGetOSDInfo
	wMetrics <- accIOEnv ioStGetOSWindowMetrics
	(found1,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
	let windows	= windowSystemStateGetWindowHandles wDevice
	let (found2,wsH,_) = getWindowHandlesWindow (toWID id) windows
	(if not found1 || not found2 || getWindowStateHandleWindowMode wsH == Modal then return ()
	 else let (okId,pos1) = validateRelativeId id pos windows
	      in if not okId then return ()
	         else let
	        	wids  = getWindowStateHandleWIDS wsH
		  	wSize = getWindowStateHandleSize wsH
		  	wKind = getWindowStateHandleWindowKind wsH
		     	isSDI = getOSDInfoDocumentInterface osdInfo==SDI
		  	(framePtr,clientPtr) = case getOSDInfoOSInfo osdInfo of
		  			Just info -> (osFrame info, osClient info)
		  			_         -> (osNoWindowPtr,osNoWindowPtr)
		      in do
			   pos <- liftIO (exactWindowPos wMetrics wSize (Just pos) wKind Modeless windows)
			   liftIO (osSetWindowPos (if isSDI && wPtr wids==clientPtr then framePtr else wPtr wids) (toTuple pos) True True))
	where
		-- validateRelativeId checks the validity of the ItemPos. 
		-- It assumes that the WindowHandles argument is not empty (containing atleast the target window).
		validateRelativeId :: Id -> ItemPos -> WindowHandles ps -> (Bool,ItemPos)
		validateRelativeId id itemPos@(itemLoc,itemOffset) windows
			| isRelative 	 = (hasWindowHandlesWindow (toWID relativeId) windows,itemPos)
			| isRelativePrev =
				let
					wsHs		 = whsWindows windows
					widsstack 	 = map getWindowStateHandleWIDS wsHs
					(found,itemLoc1) =
						case findPrevId (toWID id) widsstack of
							Nothing -> (False, itemLoc)
							Just prevId -> case itemLoc of
								LeftOfPrev  -> (True, LeftOf  prevId)
								RightToPrev -> (True, RightTo prevId)
								AbovePrev   -> (True, Above   prevId)
								BelowPrev   -> (True, Below   prevId)
								
					findPrevId :: WID -> [WIDS] -> Maybe Id
					findPrevId wid [_] = Nothing
					findPrevId wid (wid1:(wid2:wids))
						| identifyWIDS wid wid2	= Just (wId wid1)
						| otherwise		= findPrevId wid (wid2:wids)
				in
					(found,(itemLoc1,itemOffset))
			| otherwise = (True,itemPos)
			where
				(isRelative,relativeId)	= case itemLoc of
					LeftOf  id  -> (True,id)
					RightTo id  -> (True,id)
					Above   id  -> (True,id)
					Below   id  -> (True,id)
					_           -> (False,undefined)
				isRelativePrev		= case itemLoc of
					LeftOfPrev  -> True
					RightToPrev -> True
					AbovePrev   -> True
					BelowPrev   -> True
					_           -> False


-- | The getWindowPos function retrieves the position of specified window. 
getWindowPos :: Id -> GUI ps (Maybe Vector2)
getWindowPos id = do
	(found1,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
	let windows = windowSystemStateGetWindowHandles wDevice
	let (found2,wsH,_) = getWindowHandlesWindow (toWID id) windows
	(if not found1 || not found2 then return Nothing	
	 else do
		osdInfo <- accIOEnv ioStGetOSDInfo
		let di = getOSDInfoDocumentInterface osdInfo
		let isSDI = di==SDI
		let isMDI = di==MDI
		let (framePtr,clientPtr,getParentPos) = case getOSDInfoOSInfo osdInfo of
		  	Just info -> (osFrame info,osClient info,if isMDI then osGetWindowPos (osClient info) else return (0,0))
			Nothing   -> (osNoWindowPtr, osNoWindowPtr,return (0,0))
		let wids = getWindowStateHandleWIDS wsH		
		(wx,wy) <- liftIO (osGetWindowPos (if isSDI && wPtr wids==clientPtr then framePtr else wPtr wids))
		(fx,fy) <- liftIO getParentPos
		return (Just (Vector2{vx=wx-fx,vy=wy-fy})))

--	Operations that are concerned with the ViewFrame of a window.

-- | The function moves view frame through the specified vector.
moveWindowViewFrame :: Id -> Vector2 -> GUI ps ()
moveWindowViewFrame id v = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in 
	        if not found then appIOEnv (ioStSetDevice (WindowSystemState windows))
	        else if getWindowStateHandleWindowKind wsH /= IsWindow
	             then appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
	             else do
			wMetrics <- accIOEnv ioStGetOSWindowMetrics
			wsH <- liftIO (moveWindowViewFrame' wMetrics v wsH)
			appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1))))
	where
		moveWindowViewFrame' :: OSWindowMetrics -> Vector2 -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		moveWindowViewFrame' wMetrics v wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
			wH <- CP.moveWindowViewFrame wMetrics v wids wH
			return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
		moveWindowViewFrame' _ _ _ = stdWindowFatalError "moveWindowViewFrame" "unexpected window placeholder argument"

-- | 'ViewFrame' is the current visible 'Rectangle' of the window. When there are horizontal
-- and vertical scroll bars, then the changing of the scroller thumb changes the ViewFrame.
-- getWindowViewFrame returns the current ViewFrame
getWindowViewFrame :: Id -> GUI ps ViewFrame
getWindowViewFrame id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return zero
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in
		if not found
		then do
			appIOEnv (ioStSetDevice (WindowSystemState windows))
			return zero
		else do
			wMetrics <- accIOEnv ioStGetOSWindowMetrics
			let viewFrame = getwindowviewframe wMetrics wsH
			appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
			return viewFrame)


-- getwindowviewframe is also used by getWindowOuterSize.
getwindowviewframe :: OSWindowMetrics -> (WindowStateHandle ps) -> ViewFrame
getwindowviewframe wMetrics (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH})))
	| whKind wH ==IsWindow = rectToRectangle contentRect
	| otherwise = sizeToRectangle (whSize wH)
	where	
		info = whWindowInfo wH
		(origin,domainRect,hasHScroll,hasVScroll) = (windowOrigin info,windowDomain info,isJust (windowHScroll info),isJust (windowVScroll info))
		visScrolls  = osScrollbarsAreVisible wMetrics domainRect (toTuple (whSize wH)) (hasHScroll,hasVScroll)
		contentRect = getWindowContentRect wMetrics visScrolls (posSizeToRect origin (whSize wH))
getwindowviewframe _ _ = stdWindowFatalError "getWindowViewFrame" "unexpected window placeholder argument"


-- | ViewSize is the current inner size of the window
-- (It doesn\'t include the title bar and the scrollers\' area).
setWindowViewSize :: Id -> Size -> ps -> GUI ps ps
setWindowViewSize wid reqSize ps = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ps
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID wid) windows
	      in
		if not found || getWindowStateHandleWindowKind wsH /= IsWindow then return ps
		else do
			wMetrics <- accIOEnv ioStGetOSWindowMetrics
	  		let (diffSize,viewSize,frameSize) = validateSize wMetrics reqSize wsH
			(if not diffSize then return ps
			 else do
				osdInfo <- accIOEnv ioStGetOSDInfo
		  		let (framePtr,clientPtr,tbHeight) = case getOSDInfoOSInfo osdInfo of
		  			Just info -> (osFrame info,osClient info,maybe 0 toolbarHeight (osToolbar info))
		  			_         -> (osNoWindowPtr,osNoWindowPtr,0)
				let wids = getWindowStateHandleWIDS wsH
				liftIO (osSetWindowViewFrameSize (wPtr wids) (toTuple viewSize))
				liftIO (if getOSDInfoDocumentInterface osdInfo==SDI && wPtr wids==clientPtr
				        then osSetWindowViewFrameSize framePtr (toTuple viewSize{h=h viewSize+tbHeight})
				        else return ())
				let activeWIDS	= getWindowHandlesActiveWindow windows
				(wsH,ps) <- windowStateSizeAction wMetrics (isJust activeWIDS && wId (fromJust activeWIDS)==wid) (WindowSizeActionInfo {wsWIDS=wids,wsSize=frameSize,wsUpdateAll=False}) wsH ps
				appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
				return ps))
	where
		validateSize :: OSWindowMetrics -> Size -> WindowStateHandle ps -> (Bool,Size,Size)
		validateSize wMetrics reqSize (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) =
			let
				(visHScroll,visVScroll) = osScrollbarsAreVisible wMetrics domainRect (toTuple curSize) (hasHScroll,hasVScroll)
				newW = if visVScroll then w okSize+osmVSliderWidth  wMetrics else w okSize	-- Correct newW in case of visible vertical   scrollbar
				newH = if visHScroll then h okSize+osmHSliderHeight wMetrics else h okSize	-- Correct newH in case of visible horizontal scrollbar
			in
				(okSize/=curSize,okSize,Size{w=newW,h=newH})
			where
				(minWidth,minHeight)	= osMinWindowSize
				minSize			= Size{w=minWidth,h=minHeight}
				maxSize			= maxScrollWindowSize
				okSize			= Size{w=setBetween (w reqSize) (w minSize) (w maxSize),h=setBetween (h reqSize) (h minSize) (h maxSize)}
				curSize			= whSize wH
				windowInfo		= whWindowInfo wH
				domainRect		= windowDomain windowInfo
				hasHScroll		= isJust (windowHScroll windowInfo)
				hasVScroll		= isJust (windowVScroll windowInfo)
		validateSize _ _ _ = stdWindowFatalError "setWindowViewSize" "unexpected window placeholder argument"


-- | ViewSize is the current inner size of the window
-- (It doesn\'t include the title bar and the scrollers\' area).
getWindowViewSize :: Id -> GUI ps Size
getWindowViewSize id = do
	viewFrame <- getWindowViewFrame id
	return (rectangleSize viewFrame)


-- | The setWindowOuterSize function changes the dimensions of the specified window.
setWindowOuterSize :: Id -> Size -> ps -> GUI ps ps
setWindowOuterSize id (Size{w=w,h=h}) ps = do
	osdInfo <- accIOEnv ioStGetOSDInfo
	let isMDI = getOSDInfoDocumentInterface osdInfo == MDI
	(dw,dh)	<- liftIO (osStripOuterSize isMDI True)
	setWindowViewSize id (Size{w=w-dw,h=h-dh}) ps

-- | The getWindowOuterSize function retrieves the dimensions of the bounding rectangle
-- of the specified window.
getWindowOuterSize :: Id -> GUI ps Size
getWindowOuterSize id = do
	(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return zero
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found then return zero
	  	 else do
			wMetrics <- accIOEnv ioStGetOSWindowMetrics
			let viewFrame = getwindowviewframe wMetrics wsH
			let viewFrameSize = rectangleSize viewFrame
			let wKind = getWindowStateHandleWindowKind wsH
			(if wKind==IsDialog
			 then return (exactWindowSize undefined undefined viewFrameSize undefined undefined wKind)
			 else
				let
					info = getWindowStateHandleWindowInfo wsH
					(hasHScroll,hasVScroll)	= (isJust (windowHScroll info),isJust (windowVScroll info))
					domain	= rectToRectangle (windowDomain info)
				in do
					osdInfo <- accIOEnv ioStGetOSDInfo
					let isMDI = getOSDInfoDocumentInterface osdInfo == MDI					
					let outerSize	= exactWindowSize wMetrics domain viewFrameSize hasHScroll hasVScroll wKind
					(dw,dh)	<- liftIO (osStripOuterSize isMDI True)
					let windows1	= setWindowHandlesWindow wsH windows					
					appIOEnv (ioStSetDevice (WindowSystemState windows1))
					return (Size{w=w outerSize+dw,h=h outerSize+dh})))


setWindowViewDomain :: Id -> ViewDomain -> GUI ps ()
setWindowViewDomain wId newDomain = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found
	 then return ()
	 else let
	 	windows = windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID wId) windows
	      in
	      	if not found
		then appIOEnv (ioStSetDevice (WindowSystemState windows))
		else if getWindowStateHandleWindowKind wsH /= IsWindow
		     then appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
		     else do
			wMetrics <- accIOEnv ioStGetOSWindowMetrics
			wsH <- liftIO (setwindowviewdomain wMetrics newDomain wsH)
			appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1))))
	where
		setwindowviewdomain :: OSWindowMetrics -> ViewDomain -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		setwindowviewdomain wMetrics newDomain (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) =
			let
				newDomain1 = validateViewDomain newDomain
				(Rect {rleft=dl,rtop=dt,rright=dr,rbottom=db}) = rectangleToRect newDomain1
				newDomainSize = rectangleSize newDomain
				newDomainRect = rectangleToRect newDomain
				newOrigin = Point2
					{ x = if w1 >= w newDomainSize then rleft newDomainRect else setBetween (x oldOrigin) (rleft newDomainRect) ((rright  newDomainRect)-w1)
					, y = if h1 >= h newDomainSize then rtop  newDomainRect else setBetween (y oldOrigin) (rtop  newDomainRect) ((rbottom newDomainRect)-h1)
					}
				newVisScrolls = osScrollbarsAreVisible wMetrics newDomainRect wSize' hasScrolls
				newContentRect = getWindowContentRect wMetrics newVisScrolls (sizeToRect wSize)
				Rect{rright=w',rbottom=h'} = newContentRect
				osHState = toOSscrollbarRange (rleft newDomainRect,x newOrigin,rright  newDomainRect) w'
				osVState = toOSscrollbarRange (rtop  newDomainRect,y newOrigin,rbottom newDomainRect) h'
				windowInfo1 		= windowInfo{windowDomain=newDomainRect,windowOrigin=newOrigin}
				newViewFrameRect 	= posSizeToRect newOrigin (Size {w=w',h=h'})
				newViewFrame		= rectToRectangle newViewFrameRect
				oldViewFrame		= rectToRectangle oldViewFrameRect
				oldDomainViewMax	= getdomainviewmax oldDomainRect oldViewFrameRect
				newDomainViewMax	= getdomainviewmax newDomainRect newViewFrameRect
				updArea			= if sysLook && oldOrigin==newOrigin && oldDomainViewMax==newDomainViewMax
							  then []
							  else [newViewFrame]
				updState		= UpdateState{oldFrame=oldViewFrame,newFrame=newViewFrame,updArea=updArea}
				(hasCaret,catt)  = cselect isWindowCaret undefined atts
				(Point2 cx cy,Size cw ch) = getWindowCaretAtt catt
			  in do
				setwindowslider hasHScroll wMetrics (wPtr wids) True  osHState wSize'
				setwindowslider hasVScroll wMetrics (wPtr wids) False osVState wSize'
				when hasCaret (osSetCaretPos (wPtr wids) (setBetween cx dl (dr-cw)) (setBetween cy dt (db-ch)))
				(if null oldItems		-- window has no controls
				 then let 
					wH1 = wH{whWindowInfo=windowInfo1}
				      in
					if null updArea				-- nothing has to updated
					then return (WindowStateHandle wids (Just wlsH{wlsHandle=wH1}))
					else do
						wH2 <- drawWindowLook wMetrics (wPtr wids) (return ()) updState wH1
						return (WindowStateHandle wids (Just wlsH{wlsHandle=wH2}))
				 else let 			-- window has controls
					hMargins = getWindowHMargins   IsWindow wMetrics atts
					vMargins = getWindowVMargins   IsWindow wMetrics atts
					spaces = getWindowItemSpaces IsWindow wMetrics atts
					reqSize	= Size{w=w'-fst hMargins-snd hMargins,h=h'-fst vMargins-snd vMargins}
				      in do
					(_,newItems) <- layoutControls wMetrics hMargins vMargins spaces reqSize minSize [(newDomain,newOrigin)] oldItems
					let wH1 = wH{whWindowInfo=windowInfo,whItems=newItems}
					wH2 <- forceValidWindowClipState wMetrics True (wPtr wids) wH1
					updRgn <- relayoutControls wMetrics (whSelect wH) (whShow wH) newContentRect newContentRect zero zero (wPtr wids) (whDefaultId wH) oldItems (whItems wH)
					wH3 <- drawWindowLook wMetrics (wPtr wids) (return ()) updState wH2
					wH4 <- updateWindowBackgrounds wMetrics updRgn wids wH3
					return (WindowStateHandle wids (Just wlsH{wlsHandle=wH4})))
			where
				atts			= whAtts wH
				wSize			= whSize wH
				wSize'			= toTuple wSize
				(w1,h1)			= wSize'
				windowInfo		= whWindowInfo wH
				oldDomainRect		= windowDomain windowInfo
				oldOrigin		= windowOrigin windowInfo
				sysLook			= lookSysUpdate (windowLook windowInfo)
				oldItems		= whItems wH
				hasScrolls		= (isJust (windowHScroll windowInfo),isJust (windowVScroll windowInfo))
				(hasHScroll,hasVScroll)	= hasScrolls
				oldVisScrolls		= osScrollbarsAreVisible wMetrics oldDomainRect wSize' hasScrolls
				oldContentRect		= getWindowContentRect wMetrics oldVisScrolls (sizeToRect wSize)
				oldViewFrameRect	= posSizeToRect oldOrigin (rectSize oldContentRect)
				(defMinW,defMinH)	= osMinWindowSize
				minSize			= Size{w=defMinW,h=defMinH}

				setwindowslider :: Bool -> OSWindowMetrics -> OSWindowPtr -> Bool -> (Int,Int,Int,Int) -> (Int,Int) -> IO ()
				setwindowslider hasScroll wMetrics wPtr isHorizontal state maxcoords
					| hasScroll	= osSetWindowSlider wMetrics wPtr isHorizontal state maxcoords
					| otherwise	= return ()

				getdomainviewmax :: Rect -> Rect -> Point2
				getdomainviewmax domainRect viewframeRect =
					Point2 {x=min (rright domainRect) (rright viewframeRect),y=min (rbottom domainRect) (rbottom viewframeRect)}
		setwindowviewdomain _ _ _
			= stdWindowFatalError "setWindowViewDomain" "unexpected window placeholder argument"

getWindowViewDomain :: Id -> GUI ps (Maybe ViewDomain)
getWindowViewDomain id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let 
	 	windows = windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in
		if not found then do
			appIOEnv (ioStSetDevice (WindowSystemState windows))
			return Nothing
		else if getWindowStateHandleWindowKind wsH /= IsWindow
		     then do
		     		appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
		     		return Nothing
		     else let 
		     		wInfo = getWindowStateHandleWindowInfo wsH	
		  		domain	= rectToRectangle (windowDomain wInfo)
		  	  in do
		  	  	appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
		  	  	return (Just domain))
		  	  	
--	Set and get ScrollFunctions:

setWindowScrollFunction :: Id -> Direction -> ScrollFunction -> GUI ps ()
setWindowScrollFunction wId direction scrollFun = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID wId) windows
	      in if not found || getWindowStateHandleWindowKind wsH /= IsWindow then return ()
		 else let wsH1 = setwindowscrollfunction direction scrollFun wsH
		      in appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH1 windows1))))
	where
		setwindowscrollfunction :: Direction -> ScrollFunction -> WindowStateHandle ps -> WindowStateHandle ps
		setwindowscrollfunction direction scrollFun wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH})))
			| direction==Horizontal && isJust hScroll =
				let info = windowInfo{windowHScroll=fmap (setScrollFun scrollFun) hScroll}
				in WindowStateHandle wids (Just wlsH{wlsHandle=wH{whWindowInfo=info}})
			| direction==Vertical && isJust vScroll =
				let info = windowInfo{windowVScroll=fmap (setScrollFun scrollFun) vScroll}
				in WindowStateHandle wids (Just wlsH{wlsHandle=wH{whWindowInfo=info}})
			| otherwise = wsH
			where
				windowInfo	= whWindowInfo wH
				hScroll		= windowHScroll windowInfo
				vScroll		= windowVScroll windowInfo

				setScrollFun :: ScrollFunction -> ScrollInfo -> ScrollInfo
				setScrollFun f scrollInfo = scrollInfo{scrollFunction=f}
		setwindowscrollfunction _ _ _ =
			stdWindowFatalError "setWindowScrollFunction" "unexpected window placeholder argument"

getWindowScrollFunction :: Id -> Direction -> GUI ps (Maybe ScrollFunction)
getWindowScrollFunction wId direction = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID wId) windows
	      in
		if not found || getWindowStateHandleWindowKind wsH /= IsWindow then return Nothing
		else return (getwindowscrollfunction direction wsH))
	where
		getwindowscrollfunction :: Direction -> WindowStateHandle ps -> Maybe ScrollFunction
		getwindowscrollfunction direction wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH})))
			| direction==Horizontal && isJust hScroll = fmap scrollFunction hScroll
			| direction==Vertical   && isJust vScroll = fmap scrollFunction vScroll
			| otherwise = Nothing
			where
				windowInfo	= whWindowInfo wH
				hScroll		= windowHScroll windowInfo
				vScroll		= windowVScroll windowInfo
		getwindowscrollfunction _ _ =
			stdWindowFatalError "getWindowScrollFunction" "unexpected window placeholder argument"

--	Operations that are concerned with remaining attributes of windows.

-- | The setWindowTitle function changes the text of the specified window\'s title bar.
setWindowTitle :: Id -> Title -> GUI ps ()
setWindowTitle id title = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found then return ()
		 else do
			osdInfo <- accIOEnv (ioStGetOSDInfo)
		  	let isSDI = getOSDInfoDocumentInterface osdInfo==SDI
		  	let (framePtr,clientPtr) = case (getOSDInfoOSInfo osdInfo) of
				Just info -> (osFrame info, osClient info)
				_         -> (osNoWindowPtr,osNoWindowPtr)
		  	let wids = getWindowStateHandleWIDS wsH
		  	let wsH1 = setWindowStateHandleWindowTitle title wsH
			liftIO (osSetWindowTitle (if isSDI && (wPtr wids) == clientPtr then framePtr else wPtr wids) title)
			appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH1 windows1))))

setWindowCursor :: Id -> CursorShape -> GUI ps ()
setWindowCursor id shape = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found
		 then return ()
		 else do
			wsH1 <- liftIO (setwindowcursor shape wsH)
			appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH1 windows1))))
	where
		setwindowcursor :: CursorShape -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		setwindowcursor shape (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
			osSetWindowCursor (wPtr wids) (toCursorCode shape)
			let cursorAtt = WindowCursor shape
			let (replaced,atts) = creplace isWindowCursor cursorAtt (whAtts wH)
			let atts1 = if replaced then atts else (cursorAtt:atts)
			return (WindowStateHandle wids (Just wlsH{wlsHandle=wH{whAtts=atts1}}))
		setwindowcursor _ _ = stdWindowFatalError "setWindowCursor" "unexpected window placeholder argument"


-- | The setWindowCaretPos function moves the caret to the specified coordinates. 
setWindowCaretPos :: Id -> Point2 -> GUI ps ()
setWindowCaretPos id pos = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found
		 then return ()
		 else do
		 	wMetrics <- accIOEnv ioStGetOSWindowMetrics
			wsH <- liftIO (setwindowcaret wMetrics pos wsH)
			appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1))))
	where
		setwindowcaret :: OSWindowMetrics -> Point2 -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		setwindowcaret wMetrics (Point2 cx cy) wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH})))
			| whKind wH == IsWindow && hasCaret = do
				osSetCaretPos (wPtr wids) (cx'-ox) (cy'-oy)
				let viewFrame = getwindowviewframe wMetrics wsH
				let scrollVector =
					if pointInRectangle oldPos viewFrame
					then Vector2 (cx-ocx) (cy-ocy)
					else Vector2 ((max 0 (ocx-ww `div` 2))-ox) ((max 0 (ocy-wh `div` 2))-oy)
				wH <- (if not (pointInRectangle newPos viewFrame)
				       then (CP.moveWindowViewFrame wMetrics scrollVector wids)
				       else return) wH{whAtts=WindowCaret newPos size:atts}
				return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
			| otherwise = return (wsH)
			where
				(hasCaret,att,atts) = remove isWindowCaret undefined (whAtts wH)
				WindowCaret oldPos@(Point2 ocx ocy) size@(Size cw ch) = att
				WindowInfo { windowDomain = Rect {rleft=dl,rtop=dt,rright=dr,rbottom=db}
					   , windowOrigin = Point2 ox oy
					   } = whWindowInfo wH
				cx' = setBetween cx dl (dr-cw)
				cy' = setBetween cy dt (db-ch)
				newPos = Point2 cx' cy'
				Size ww wh = whSize wH
		setwindowcaret _ _ _ = stdWindowFatalError "setWindowCursor" "unexpected window placeholder argument"

-- | The getWindowTitle function returns the text of the specified window\'s title bar.
getWindowTitle :: Id -> GUI ps (Maybe Title)
getWindowTitle id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let
	 	windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found
	         then return Nothing
		 else return (Just (getWindowStateHandleWindowTitle wsH)))


getWindowOk :: Id -> GUI ps (Maybe Id)
getWindowOk id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found
	         then return Nothing
	     	 else return (getWindowStateHandleDefaultId wsH))
			
getWindowCancel :: Id -> GUI ps (Maybe Id)
getWindowCancel id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found
	      	 then return Nothing
	     	 else return (getWindowStateHandleCancelId wsH))

getWindowCursor :: Id -> GUI ps (Maybe CursorShape)
getWindowCursor id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found
	         then return Nothing
	      	 else return (getcursor wsH))
	where
		getcursor :: WindowStateHandle ps -> Maybe CursorShape
		getcursor wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) =
			fmap getWindowCursorAtt (find isWindowCursor (whAtts wH))
		getcursor _ = stdWindowFatalError "getWindowCursor" "unexpected window placeholder argument"

-- | If the window has 'WindowCaret' attribute then the getWindowCaretPos function returns the caret\'s position,
-- otherwise it returns Nothing
getWindowCaretPos :: Id -> GUI ps (Maybe Point2)
getWindowCaretPos id = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return Nothing
	 else let
		windows	= windowSystemStateGetWindowHandles wDevice
	  	(found,wsH,windows1) = getWindowHandlesWindow (toWID id) windows
	      in if not found
	         then return Nothing
	      	 else return (getcaret wsH))
	where
		getcaret :: WindowStateHandle ps -> Maybe Point2
		getcaret wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) =
			fmap (fst . getWindowCaretAtt) (find isWindowCaret (whAtts wH))
		getcaret _ = stdWindowFatalError "getWindowCursor" "unexpected window placeholder argument"