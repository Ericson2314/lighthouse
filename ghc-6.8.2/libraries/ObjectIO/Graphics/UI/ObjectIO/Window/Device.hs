-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Window.Device
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Window.Device defines the window device event handlers
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Window.Device
		( windowFunctions
		, windowStateSizeAction -- used from StdWindow.setWindowViewSize
		) where



import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Control.Create
import Graphics.UI.ObjectIO.Control.Draw(drawCompoundLook)
import Graphics.UI.ObjectIO.Control.Layout(layoutControls)
import Graphics.UI.ObjectIO.Control.Relayout(relayoutControls)
import Graphics.UI.ObjectIO.Control.Resize(resizeControls)
import Graphics.UI.ObjectIO.Device.Events
import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.StdControlAttribute
import Graphics.UI.ObjectIO.StdWindowAttribute
import Graphics.UI.ObjectIO.Window.Access
import Graphics.UI.ObjectIO.Window.ClipState (forceValidWindowClipState)
import Graphics.UI.ObjectIO.Window.Create (bufferDelayedEvents)
import Graphics.UI.ObjectIO.Window.Dispose
import Graphics.UI.ObjectIO.Window.Draw (drawWindowLook')
import Graphics.UI.ObjectIO.Window.Handle
import Graphics.UI.ObjectIO.Window.Update (updateWindow, updateWindowBackgrounds)
import Graphics.UI.ObjectIO.KeyFocus (setNewFocusItem, setNoFocusItem)
import Graphics.UI.ObjectIO.Receiver.Handle
import Graphics.UI.ObjectIO.OS.WindowEvent
import Graphics.UI.ObjectIO.OS.Window
import Graphics.UI.ObjectIO.OS.Event (createOSLooseMouseEvent, createOSLooseKeyEvent, createOSDeactivateControlEvent)
import Graphics.UI.ObjectIO.OS.Rgn (osGetRgnBox)
import Graphics.UI.ObjectIO.OS.Picture (Draw(..), pictScroll)
import System.IO(fixIO)
import Control.Monad(when)
import qualified Data.Map as Map

windowdeviceFatalError :: String -> String -> x
windowdeviceFatalError function error
	= dumpFatalError function "WindowDevice" error

checkErrorIO :: Bool -> String -> String -> GUI ps ()
checkErrorIO flag function error
	| flag 	    = return ()
	| otherwise = dumpFatalError function "WindowDevice" error

windowFunctions :: DeviceFunctions ps
windowFunctions
	= DeviceFunctions
		{ dDevice = WindowDevice
		, dShow	  = return
		, dHide	  = return
		, dEvent  = windowEvent
		, dDoIO   = windowIO
		, dOpen   = windowOpen
		, dClose  = windowClose
		}

{-	windowOpen initialises the window device for this interactive process.
-}
windowOpen :: ps -> GUI ps ps
windowOpen ps
	= do {
		hasWindow <- accIOEnv (ioStHasDevice WindowDevice);
		if   hasWindow
		then return ps
		else do {
			xDI <- accIOEnv ioStGetDocumentInterface;
			let bound   = case xDI of
			                NDI -> Finite 0
			                SDI -> Finite 1
			                MDI -> Infinite
			    windows = WindowHandles {whsWindows=[],whsNrWindowBound=bound, whsModal=False, whsFinalModalLS = []}
			in  appIOEnv ( ioStSetDevice (WindowSystemState windows) . ioStSetDeviceFunctions windowFunctions ) >> return ps
		     }
	  }


{-	windowClose closes all windows associated with this interactive process.
	System resources are released.
	Note that the window device is not removed from the IOSt because there still might be
	a modal dialog which final state has to be retrieved.
-}
windowClose :: ps -> GUI ps ps
windowClose ps
	= do {	(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice);
		if   not found
		then return ps
		else
		do
			osdinfo <- accIOEnv ioStGetOSDInfo
			let windows = windowSystemStateGetWindowHandles wDevice
			oldInputTrack <- ioStGetInputTrack
			(disposeInfo,(newInputTrack,ps1)) <- stateMapM (disposeWindowStateHandle' osdinfo) (whsWindows windows) (oldInputTrack, ps)
			ioStSetInputTrack newInputTrack
			let (freeIdss, _, finalLS) = unzip3 disposeInfo
			idtable <- ioStGetIdTable
			ioStSetIdTable (foldr Map.delete idtable (concat freeIdss))
			let windows1 = windows {whsWindows=[], whsFinalModalLS=(whsFinalModalLS windows)++(concat finalLS)}
			appIOEnv (ioStSetDevice (WindowSystemState windows1))
			return ps1
	  }
	  where
		disposeWindowStateHandle' :: OSDInfo -> WindowStateHandle ps -> (Maybe InputTrack, ps) -> GUI ps (([Id],[DelayActivationInfo],[FinalModalLS]),(Maybe InputTrack, ps))
		disposeWindowStateHandle' osdinfo wsH (inputTrack, ps) = do
			(a,b,c,inputTrack,ps) <- disposeWindowStateHandle osdinfo inputTrack wsH ps
			return ((a,b,c),(inputTrack,ps))

{-	windowIO handles the DeviceEvents that have been filtered by windowEvent.
	The only events that are handled in this implementation are:
		ControlKeyboardAction
		ControlSelection
		WindowActivation
		WindowDeactivation
		WindowInitialise
		WindowRequestClose
-}
windowIO :: DeviceEvent -> ps -> GUI ps ps
windowIO deviceEvent ps
	= do {
		hasDevice <- accIOEnv (ioStHasDevice WindowDevice);
		if   not hasDevice
		then windowdeviceFatalError "dDoIO windowFunctions" "could not retrieve WindowSystemState from IOSt"
		else windowIO' deviceEvent ps
	  }
	where
		windowIO' :: DeviceEvent -> ps -> GUI ps ps
		windowIO' (ReceiverEvent rId) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows = windowSystemStateGetWindowHandles wDevice
			it <- ioStGetIdTable
			let Just idParent = Map.lookup rId it
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (idpId idParent)) windows
			checkErrorIO found  "windowIO (ReceiverEvent _) _" "window could not be found"
			toGUI (windowStateMsgIO rId wsH windows1 ps)
		
		windowIO' (CompoundScrollAction info) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)			
			let windows = windowSystemStateGetWindowHandles wDevice			
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId (csaWIDS info))) windows
			checkErrorIO found "windowIO (CompoundScrollAction _) _" "window could not be found"
			wMetrics <- accIOEnv ioStGetOSWindowMetrics
			wsH <- liftIO (windowStateCompoundScrollActionIO wMetrics info wsH)
			let windows2 = setWindowHandlesWindow wsH windows1
			appIOEnv (ioStSetDevice (WindowSystemState windows2))
			return ps
			
		windowIO' (ControlGetKeyFocus info) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows		 = windowSystemStateGetWindowHandles wDevice
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId (ckfWIDS info))) windows
			checkErrorIO found "windowIO (ControlGetKeyFocus _) _" "window could not be found"
			toGUI (windowStateControlKeyFocusActionIO True info wsH windows1 ps)

		windowIO' (ControlKeyboardAction info) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows              = windowSystemStateGetWindowHandles wDevice
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId (ckWIDS info))) windows
			checkErrorIO found "windowIO (ControlKeyboardAction _) _" "window could not be found"
			toGUI (windowStateControlKeyboardActionIO info wsH windows1 ps)

		windowIO' (ControlLooseKeyFocus info) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows 	= windowSystemStateGetWindowHandles wDevice
			let wids	= ckfWIDS info
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId wids)) windows
			checkErrorIO found "windowIO (ControlLooseKeyFocus _) _" "window could not be found"
			oldInputTrack <- ioStGetInputTrack
			let (newInputTrack,lostMouse,lostKey) =
				case oldInputTrack of
					Just it@(InputTrack {itWindow=itWindow,itControl=itControl,itKind=itKind}) ->
						if (itWindow==(wPtr wids) && itControl==(ckfItemNr info))
						then ( Nothing
						     , if (itkMouse    itKind) then [createOSLooseMouseEvent itWindow itControl] else []
						     , if (itkKeyboard itKind) then [createOSLooseKeyEvent   itWindow itControl] else []
						     )
						else (oldInputTrack,[],[])
					Nothing	-> (Nothing,[],[])
			let lostInputEvents = lostMouse ++ lostKey
			ioStSetInputTrack newInputTrack
			(if null lostInputEvents		-- no input was being tracked: simply evaluate control deactivate function
			 then toGUI (windowStateControlKeyFocusActionIO False info wsH windows1 ps)				
			 else do				-- handle control deactivate function AFTER lost input events
				ioStInsertEvents (lostInputEvents ++ [createOSDeactivateControlEvent (wPtr wids) (ckfItemPtr info)])
				return ps)

		windowIO' (ControlMouseAction info) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
		  	let windows = windowSystemStateGetWindowHandles wDevice
		  	let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId (cmWIDS info))) windows
		  	checkErrorIO found  "windowIO (ControlMouseAction _) _" "window could not be found"
		  	toGUI (windowStateControlMouseActionIO info wsH windows1 ps)		  	

		windowIO' (ControlSelection info) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows = windowSystemStateGetWindowHandles wDevice
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId (csWIDS info))) windows
			checkErrorIO found "windowIO (ControlSelection _) _" "window could not be found"
			toGUI (windowStateControlSelectionIO info wsH windows1 ps)

		windowIO' (ControlSliderAction info) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows = windowSystemStateGetWindowHandles wDevice
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId (cslWIDS info))) windows
			checkErrorIO found "windowIO (ControlSliderAction _) _" "window could not be found"
			toGUI (windowStateControlSliderActionIO info wsH windows1 ps)

		windowIO' (WindowActivation wids) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows              = windowSystemStateGetWindowHandles wDevice
			let wid                  = toWID (wId wids)
			let (found,wsH,windows1) = getWindowHandlesWindow wid windows
			checkErrorIO found "windowIO (WindowActivation _)" "window could not be found"
			let (_,_,windows2)  = removeWindowHandlesWindow wid windows1	-- Remove the placeholder from windows1
			toGUI (windowStateActivationIO wsH windows2 ps)

		windowIO' (WindowCreateControls wids) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows              = windowSystemStateGetWindowHandles wDevice
			let (found,wsH,windows1) = removeWindowHandlesWindow (toWID (0::OSWindowPtr)) windows
			checkErrorIO found "windowIO (WindowCreateControls _)" "window could not be found"
			wMetrics        <- accIOEnv ioStGetOSWindowMetrics
			let wsH1         = setWindowStateHandleWIDS wids wsH
			cPtr     <- liftIO (createDialogControls wMetrics wsH1)
			when (cPtr /= osNoWindowPtr)
				(liftIO (osActivateControl (wPtr wids) cPtr) >>= bufferDelayedEvents)
			let windows2     = addWindowHandlesActiveWindow wsH1 windows1
			appIOEnv (ioStSetDevice (WindowSystemState windows2))
			return ps
			where
				createDialogControls :: OSWindowMetrics -> WindowStateHandle ps -> IO OSWindowPtr
				createDialogControls wMetrics wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
					itemHs <- createControls wMetrics (whDefaultId wH) (whCancelId wH) True (wPtr wids) (whItems wH)
					return (getInitActiveControl (wH {whItems=itemHs}))
				createDialogControls _ _
					= windowdeviceFatalError "windowIO (WindowCreateControls _)" "placeholder not expected"

		windowIO' (WindowDeactivation wids) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows              = windowSystemStateGetWindowHandles wDevice
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId wids)) windows
			checkErrorIO found "windowIO (WindowDeactivation _)" "window could not be found"
			toGUI (windowStateDeactivationIO wsH windows1 ps)

		windowIO' (WindowInitialise wids) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows              = windowSystemStateGetWindowHandles wDevice
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId wids)) windows
			checkErrorIO found "windowIO (WindowInitialise _)" "window could not be found"
			toGUI (windowStateInitialiseIO wsH windows1 ps)

		windowIO' (WindowKeyboardAction action) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows = windowSystemStateGetWindowHandles wDevice
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId (wkWIDS action))) windows
			checkErrorIO found "windowIO (WindowKeyboardAction _)" "window could not be found"
			toGUI (windowStateWindowKeyboardActionIO action wsH windows1 ps)

		windowIO' (WindowMouseAction action) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows = windowSystemStateGetWindowHandles wDevice
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId (wmWIDS action))) windows
			checkErrorIO found "windowIO (WindowMouseAction _)" "window could not be found"
			toGUI (windowStateWindowMouseActionIO action wsH windows1 ps)

		windowIO' (WindowCANCEL wids) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows = windowSystemStateGetWindowHandles wDevice
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId wids)) windows
			checkErrorIO found "windowIO (WindowCANCEL _)" "window could not be found"
			toGUI (windowStateCANCELIO wsH windows1 ps)
			
		windowIO' (WindowOK wids) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows = windowSystemStateGetWindowHandles wDevice
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId wids)) windows
			checkErrorIO found "windowIO (WindowOK _)" "window could not be found"
			toGUI (windowStateOKIO wsH windows1 ps)

		windowIO' (WindowRequestClose wids) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows              = windowSystemStateGetWindowHandles wDevice
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId wids)) windows
			checkErrorIO found "windowIO (WindowRequestClose _)" "window could not be found"
			toGUI (windowStateRequestCloseIO wsH windows1 ps)

		windowIO' (WindowScrollAction info) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows = windowSystemStateGetWindowHandles wDevice
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId (wsaWIDS info))) windows
			checkErrorIO found "windowIO (WindowScrollAction _)" "window could not be found"
			wMetrics <- accIOEnv (ioStGetOSWindowMetrics)
			wsH1 <- liftIO (windowStateScrollActionIO wMetrics info wsH)
			let windows2 = setWindowHandlesWindow wsH1 windows1
			appIOEnv (ioStSetDevice (WindowSystemState windows2))				
			return ps

		windowIO' (WindowSizeAction info) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows = windowSystemStateGetWindowHandles wDevice
			let winId = wId (wsWIDS info)
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID winId) windows			
			checkErrorIO found "windowIO (WindowSizeAction _)" "window could not be found"
			let activeWIDS = getWindowHandlesActiveWindow windows1
			wMetrics <- accIOEnv (ioStGetOSWindowMetrics)
			(wsH,ps) <- windowStateSizeAction wMetrics (isJust activeWIDS && (wId (fromJust activeWIDS)) == winId) info wsH ps
			let windows2 = setWindowHandlesWindow wsH windows1
			appIOEnv (ioStSetDevice (WindowSystemState windows2))
			return ps
		
		windowIO' (WindowUpdate info) ps = do
			(_,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
			let windows 		 = windowSystemStateGetWindowHandles wDevice
			let (found,wsH,windows1) = getWindowHandlesWindow (toWID (wId (updWIDS info))) windows
			checkErrorIO found "windowIO (WindowUpdate _)" "window could not be found"
			wMetrics        <- accIOEnv ioStGetOSWindowMetrics
			wsH <- liftIO (windowStateUpdateIO wMetrics info wsH)
			let windows2 = setWindowHandlesWindow wsH windows1
			appIOEnv (ioStSetDevice (WindowSystemState windows2))
			return ps
			where
				windowStateUpdateIO :: OSWindowMetrics -> UpdateInfo -> WindowStateHandle ps -> IO (WindowStateHandle ps)
				windowStateUpdateIO wMetrics info wsH@(WindowStateHandle wids (Just wlsHandle@(WindowLSHandle {wlsHandle=wH}))) = do
					wH <- updateWindow wMetrics info wH
					return (WindowStateHandle wids (Just wlsHandle{wlsHandle=wH}))
				windowStateUpdateIO _ _ _
					= windowdeviceFatalError "windowIO (WindowUpdate _) _" "unexpected placeholder argument"



{-	windowStateMsgIO handles all message events. -}

windowStateMsgIO :: Id -> WindowStateHandle ps -> WindowHandles ps -> ps -> IOSt ps -> IO (ps, IOSt ps)
windowStateMsgIO rId wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsState=ls,wlsHandle=wH}))) windows ps ioState = do
	r <- windowControlMsgIO  (\wH st ioState -> ioStSetDevice
					(WindowSystemState
						(setWindowHandlesWindow
							(WindowStateHandle wids (Just (WindowLSHandle {wlsState=fst st,wlsHandle=wH})))
							 windows))
						 ioState) rId wH (ls,ps) ioState
	let ((_,ps1), ioState) = r
	return (ps1,ioState)
	where
	--	windowControlASyncIO handles the first asynchronous message in the message queue of the indicated receiver control.
		windowControlMsgIO :: (WindowHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) -> 
					Id -> WindowHandle ls ps -> (ls,ps) -> IOSt ps -> IO ((ls,ps), IOSt ps)
		windowControlMsgIO build rId wH@(WindowHandle {whItems=itemHs}) ls_ps ioState = do
			r <- elementsControlMsgIO (\itemHs st ioState -> build (wH {whItems=itemHs}) st ioState) rId itemHs ls_ps ioState
			let (_,ls_ps1,ioState) = r
			return (ls_ps1,ioState)
			where
				elementsControlMsgIO :: ([WElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IOSt ps) -> 
							  Id -> [WElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IO (Bool,(ls,ps),IOSt ps)
				elementsControlMsgIO build rId (itemH:itemHs) ls_ps ioState = do
					r <- elementControlMsgIO (\itemH st ioState -> build (itemH:itemHs) st ioState) rId itemH ls_ps ioState
					let (done,ls_ps,ioState) = r
					(if done then return r
					 else elementsControlMsgIO (\itemHs st ioState -> build (itemH:itemHs) st ioState) rId itemHs ls_ps ioState)
					where
						elementControlMsgIO :: (WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) -> 
									 Id -> WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IO (Bool,(ls,ps),IOSt ps)
						elementControlMsgIO build rId itemH@(WItemHandle {}) ls_ps ioState
							| not (identifyMaybeId rId (wItemId itemH)) =
								if not (isRecursiveControl itemKind)
								then return (False,ls_ps,build itemH ls_ps ioState)
								else elementsControlMsgIO (\itemHs st ioState -> build (itemH{wItems = itemHs}) st ioState) rId (wItems itemH) ls_ps ioState
							| itemKind /= IsReceiverControl =
								return (True,ls_ps,ioState)
							| otherwise = itemControlMsgIO build rId itemH ls_ps ioState
								where
									itemKind = wItemKind itemH

									itemControlMsgIO :: (WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) -> 
									 			Id -> WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IO (Bool,(ls,ps),IOSt ps)
									itemControlMsgIO build rId itemH@(WItemHandle {wItemInfo=WReceiverInfo (ReceiverHandle {rFun=f})}) ls_ps ioState = do
										r <- fixIO (\st -> fromGUI (f ls_ps) (build itemH (fst st) ioState))
										let (ls_ps1, ioState1) = r
										return (True, ls_ps1, ioState1)

						elementControlMsgIO build rId (WListLSHandle itemHs) ls_ps ioState = do
							elementsControlMsgIO (\itemHs st ioState -> build (WListLSHandle itemHs) st ioState) rId itemHs ls_ps ioState

						elementControlMsgIO build rId (WExtendLSHandle exLS itemHs) (ls,ps) ioState = do
							r <- elementsControlMsgIO (\itemHs st ioState -> build (WExtendLSHandle (fst (fst st)) itemHs) (snd (fst st),snd st) ioState) rId itemHs ((exLS,ls),ps) ioState
							let (done,((exLS,ls),ps),ioState) = r
							return (done,(ls,ps),ioState)

						elementControlMsgIO build rId (WChangeLSHandle chLS itemHs) (ls,ps) ioState = do
							r <- elementsControlMsgIO (\itemHs st ioState -> build (WChangeLSHandle (fst st) itemHs) (ls,snd st) ioState) rId itemHs (chLS,ps) ioState
							let (done,(chLS,ps),ioState) = r
							return (done,(ls,ps),ioState)

				elementsControlMsgIO _ _ _ ls_ps ioState = return (False,ls_ps,ioState)


windowStateMsgIO  _ _ _ _ _ = windowdeviceFatalError "windowStateMsgIO" "unexpected window placeholder"

{-	windowStateCompoundScrollActionIO handles the mouse actions of CompoundControl scrollbars.  -}

windowStateCompoundScrollActionIO :: OSWindowMetrics -> CompoundScrollActionInfo -> WindowStateHandle ps -> IO (WindowStateHandle ps)
windowStateCompoundScrollActionIO wMetrics info@(CompoundScrollActionInfo {csaWIDS=WIDS{wPtr=wPtr}})
			wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = 
    let 
    	WindowHandle{whKind=whKind,whItems=itemHs,whSize=whSize,whAtts=whAtts,whSelect=whSelect} = wH
	windowInfo		= whWindowInfo wH
	(origin,domainRect,hasHScroll,hasVScroll) = 
		if (whKind==IsWindow)
		then (windowOrigin windowInfo,windowDomain windowInfo,isJust (windowHScroll windowInfo),isJust (windowVScroll windowInfo))
		else (zero,sizeToRect whSize,False,False)
	domain			= rectToRectangle domainRect
	visScrolls		= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)
	wFrame			= getWindowContentRect wMetrics visScrolls (sizeToRect whSize)
	contentSize		= rectSize wFrame
	(defMinW,  defMinH)	= osMinWindowSize
	minSize			= Size{w=defMinW,h=defMinH}
	hMargins		= getWindowHMargins   whKind wMetrics whAtts
	vMargins		= getWindowVMargins   whKind wMetrics whAtts
	spaces			= getWindowItemSpaces whKind wMetrics whAtts
    in do    	    
	    (done,originChanged,itemHs1) <- calcNewCompoundOrigin wMetrics info itemHs	    
	    (if not done then windowdeviceFatalError "windowStateCompoundScrollActionIO" "could not locate CompoundControl"
	     else
		(if not originChanged then return (WindowStateHandle wids (Just wlsH{wlsHandle=wH{whItems=itemHs1}}))
		 else do
		    (_,newItems) <- layoutControls wMetrics hMargins vMargins spaces contentSize minSize [(domain,origin)] itemHs1		    
		    wH <- forceValidWindowClipState wMetrics True wPtr (wH{whItems=newItems})
		    updRgn <- relayoutControls wMetrics whSelect (whShow wH) wFrame wFrame zero zero wPtr (whDefaultId wH) itemHs (whItems wH)
		    wH <- updateWindowBackgrounds wMetrics updRgn (csaWIDS info) wH
		    wH <- drawcompoundlook wMetrics whSelect wFrame (csaItemNr info) wPtr wH	-- PA: this might be redundant now because of updateWindowBackgrounds
		    return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))))
    where	
	drawcompoundlook :: OSWindowMetrics -> Bool -> Rect -> Int -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
	drawcompoundlook wMetrics ableContext clipRect itemNr wPtr wH = do
	    (_,itemHs) <- drawcompoundlook' wMetrics ableContext clipRect itemNr wPtr (whItems wH)
	    return (wH{whItems=itemHs})
	    where
		drawcompoundlook' :: OSWindowMetrics -> Bool -> Rect -> Int -> OSWindowPtr -> [WElementHandle ls ps] -> IO (Bool,[WElementHandle ls ps])
		drawcompoundlook' wMetrics ableContext clipRect itemNr wPtr (itemH:itemHs) = do
		    (done,itemH) <- drawWElementLook wMetrics ableContext clipRect itemNr wPtr itemH
		    (if done then return (done,itemH:itemHs)
		     else do
		        (done,itemHs) <- drawcompoundlook' wMetrics ableContext clipRect itemNr wPtr itemHs
		        return (done,itemH:itemHs))
		    where
			drawWElementLook :: OSWindowMetrics -> Bool -> Rect -> Int -> OSWindowPtr -> WElementHandle ls ps -> IO (Bool,WElementHandle ls ps)
			drawWElementLook wMetrics ableContext clipRect itemNr wPtr itemH@(WItemHandle {wItemKind=wItemKind,wItemSelect=wItemSelect})
				| csaItemNr info /= wItemNr itemH =
					if not (isRecursiveControl wItemKind) then
						return (False,itemH)
					else
					   if wItemKind==IsLayoutControl then do
						(done,itemHs) <- drawcompoundlook' wMetrics isAble (intersectRects clipRect itemRect) itemNr wPtr (wItems itemH)
						return (done,itemH{wItems=itemHs})
					   else do
					       (done,itemHs) <- drawcompoundlook' wMetrics isAble clipRect1 itemNr wPtr (wItems itemH)
					       return (done,itemH{wItems=itemHs})
				| wItemKind /= IsCompoundControl
					= windowdeviceFatalError "drawWElementLook (windowStateCompoundScrollActionIO)" "argument control is not a CompoundControl"
				| otherwise = do
					itemH <- drawCompoundLook wMetrics isAble wPtr clipRect1 itemH
					osValidateWindowRect (wItemPtr itemH) clipRect1
					return (True,itemH)
				where
					isAble		= ableContext && wItemSelect
					itemPos		= wItemPos itemH
					itemSize	= wItemSize itemH
					itemInfo	= getWItemCompoundInfo (wItemInfo itemH)
					domainRect	= compoundDomain itemInfo
					hasScrolls	= (isJust (compoundHScroll itemInfo),isJust (compoundVScroll itemInfo))
					visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
					itemRect	= posSizeToRect itemPos itemSize
					contentRect	= getCompoundContentRect wMetrics visScrolls itemRect 
					clipRect1	= intersectRects clipRect contentRect
		
			drawWElementLook wMetrics ableContext clipRect itemNr wPtr (WListLSHandle itemHs) = do
				(done,itemHs) <- drawcompoundlook' wMetrics ableContext clipRect itemNr wPtr itemHs
				return (done,WListLSHandle itemHs)
			
			drawWElementLook wMetrics ableContext clipRect itemNr wPtr (WExtendLSHandle exLS itemHs) = do
				(done,itemHs) <- drawcompoundlook' wMetrics ableContext clipRect itemNr wPtr itemHs
				return (done,WExtendLSHandle exLS itemHs)
			
			drawWElementLook wMetrics ableContext clipRect itemNr wPtr (WChangeLSHandle chLS itemHs) = do
				(done,itemHs) <- drawcompoundlook' wMetrics ableContext clipRect itemNr wPtr itemHs
				return (done,WChangeLSHandle chLS itemHs)
		
		drawcompoundlook' _ _ _ _ _ itemHs = return (False,itemHs)
	
	calcNewCompoundOrigin :: OSWindowMetrics -> CompoundScrollActionInfo -> [WElementHandle ls ps] -> IO (Bool,Bool,[WElementHandle ls ps])
	calcNewCompoundOrigin wMetrics info (itemH:itemHs) = do
	    (done,changed,itemH) <- calcNewWElementOrigin wMetrics info itemH
	    (if done then return (done,changed,(itemH:itemHs))
 	     else do
			(done,changed,itemHs) <- calcNewCompoundOrigin wMetrics info itemHs
			return (done,changed,itemH:itemHs))
	    where
		calcNewWElementOrigin :: OSWindowMetrics -> CompoundScrollActionInfo -> WElementHandle ls ps -> IO (Bool,Bool,WElementHandle ls ps)
		calcNewWElementOrigin wMetrics info itemH@(WItemHandle {wItemKind=wItemKind,wItemAtts=wItemAtts})
			| csaItemNr info /= wItemNr itemH =
				if not (isRecursiveControl wItemKind) then
					return (False,False,itemH)
				else do
					(done,changed,itemHs) <- calcNewCompoundOrigin wMetrics info (wItems itemH)
					return (done,changed,itemH{wItems=itemHs})
			| wItemKind /= IsCompoundControl		-- This alternative should never occur
				= windowdeviceFatalError "windowStateCompoundScrollActionIO" "CompoundScrollAction does not correspond with CompoundControl"
			| newThumb == oldThumb =
				return (True,False,itemH)
			| otherwise = do
				osSetCompoundSliderThumb wMetrics itemPtr isHorizontal newOSThumb (toTuple compoundSize) True
				let itemH1 = itemH{ wItemInfo=WCompoundInfo compoundInfo{compoundOrigin=newOrigin}
						  , wItemAtts=replaceOrAppend isControlViewSize (ControlViewSize rsize) wItemAtts
						  }
				return (True,True,itemH1)
			where
				itemPtr	= csaItemPtr info
				compoundSize = wItemSize itemH
				compoundInfo = getWItemCompoundInfo (wItemInfo itemH)
				(domainRect,origin,hScroll,vScroll)	= (compoundDomain compoundInfo,compoundOrigin compoundInfo,compoundHScroll compoundInfo,compoundVScroll compoundInfo)
				visScrolls = osScrollbarsAreVisible wMetrics domainRect (toTuple compoundSize) (isJust hScroll,isJust vScroll)
				rsize@(Size {w=w,h=h}) = rectSize (getCompoundContentRect wMetrics visScrolls (sizeToRect compoundSize))
				isHorizontal = csaDirection info==Horizontal
				scrollInfo = fromJust (if isHorizontal then hScroll else vScroll)
				scrollFun = scrollFunction scrollInfo
				viewFrame = posSizeToRectangle origin rsize
				(min',oldThumb,max',viewSize) = 
					if isHorizontal
					then (rleft domainRect,x origin,rright  domainRect,w)
					else (rtop  domainRect,y origin,rbottom domainRect,h)
				sliderState = SliderState {sliderMin=min',sliderThumb=oldThumb,sliderMax=max min' (max'-viewSize)}
				newThumb' = scrollFun viewFrame sliderState (csaSliderMove info)
				newThumb  = setBetween newThumb' min' (max min' (max'-viewSize))
				(_,newOSThumb,_,_) = toOSscrollbarRange (min',newThumb,max') viewSize
				newOrigin = if isHorizontal then origin{x=newThumb} else origin{y=newThumb}
		
		calcNewWElementOrigin wMetrics info (WListLSHandle itemHs) = do
			(done,changed,itemHs) <- calcNewCompoundOrigin wMetrics info itemHs
			return (done,changed,WListLSHandle itemHs)
		
		calcNewWElementOrigin wMetrics info (WExtendLSHandle extLS itemHs) = do
			(done,changed,itemHs) <- calcNewCompoundOrigin wMetrics info itemHs
			return (done,changed,WExtendLSHandle extLS itemHs)
		
		calcNewWElementOrigin wMetrics info (WChangeLSHandle chLS itemHs) = do
			(done,changed,itemHs) <- calcNewCompoundOrigin wMetrics info itemHs
			return (done,changed,WChangeLSHandle chLS itemHs)
	
	calcNewCompoundOrigin _ _ _ = return (False,False,[])
	
windowStateCompoundScrollActionIO _ _ _
	= windowdeviceFatalError "windowStateCompoundScrollActionIO" "unexpected window placeholder"

{-	windowStateControlKeyFocusActionIO handles the keyboard focus actions of (Compound/Custom/Edit/PopUp)Controls.
	The Bool argument indicates whether the control has obtained key focus (True) or lost key focus (False).
-}

windowStateControlKeyFocusActionIO :: Bool -> ControlKeyFocusInfo -> WindowStateHandle ps -> WindowHandles ps -> ps -> IOSt ps -> IO (ps, IOSt ps)

windowStateControlKeyFocusActionIO activated info wsH@(WindowStateHandle wids (Just (WindowLSHandle {wlsState=ls,wlsHandle=wH}))) windows ps ioState = do
	r <- windowControlKeyFocusActionIO (\wH st ioState -> ioStSetDevice
							  (WindowSystemState
							      (setWindowHandlesWindow
								  (WindowStateHandle wids (Just (WindowLSHandle {wlsState=fst st,wlsHandle=wH})))
								  windows))
							  ioState) activated info wH (ls,ps) ioState
	let ((ls1, ps1), ioState) = r
	return (ps1, ioState)
	where
		windowControlKeyFocusActionIO :: (WindowHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
						Bool -> ControlKeyFocusInfo -> WindowHandle ls ps -> (ls, ps) -> IOSt ps -> IO ((ls, ps), IOSt ps)
		windowControlKeyFocusActionIO build activated info wH@(WindowHandle {whItems=whItems,whKeyFocus=whKeyFocus}) ls_ps ioState = do
			r <- elementsControlKeyFocusActionIO (\itemHs st ioState -> build wH{whItems=itemHs, whKeyFocus=keyfocus} st ioState) activated info whItems ls_ps ioState
			let (_, ls_ps1, ioState) = r
			return (ls_ps1, ioState)
			where
				keyfocus =
				   if activated then setNewFocusItem (ckfItemNr info) whKeyFocus
				   else setNoFocusItem whKeyFocus

				elementsControlKeyFocusActionIO :: ([WElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IOSt ps) ->
								   Bool -> ControlKeyFocusInfo -> [WElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IO (Bool, (ls,ps), IOSt ps)
				elementsControlKeyFocusActionIO build activated info (itemH:itemHs) ls_ps ioState = do
					r <- elementControlKeyFocusActionIO (\itemH st ioState -> build (itemH:itemHs) st ioState) activated info itemH ls_ps ioState
					let (done, ls_ps1, ioState) = r
					(if done then return r
					 else elementsControlKeyFocusActionIO (\itemHs st ioState -> build (itemH:itemHs) st ioState) activated info itemHs ls_ps1 ioState)
					where
						elementControlKeyFocusActionIO :: (WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
										  Bool -> ControlKeyFocusInfo -> WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IO (Bool, (ls,ps), IOSt ps)
						elementControlKeyFocusActionIO build activated info itemH@(WItemHandle {wItemAtts=wItemAtts, wItems=itemHs, wItemKind=wItemKind}) ls_ps ioState
							| (ckfItemNr info) /= (wItemNr itemH) =
								if not (isRecursiveControl wItemKind) then return (False, ls_ps, ioState)
								else elementsControlKeyFocusActionIO (\itemHs st ioState -> build itemH{wItems=itemHs} st ioState) activated info itemHs ls_ps ioState
									
							| otherwise = do
								r <- fixIO (\st -> fromGUI (f ls_ps) (build itemH (fst st) ioState))
								let (ls_ps1, ioState) = r
								return (True, ls_ps1, ioState)
							where
								f = getAtt (snd (cselect reqAtt undefined wItemAtts))
								(reqAtt,getAtt)	=
									if activated
									then (isControlActivate,  getControlActivateFun  )
									else (isControlDeactivate,getControlDeactivateFun)

						elementControlKeyFocusActionIO build activated info (WListLSHandle itemHs) ls_ps ioState = do
							elementsControlKeyFocusActionIO (\itemHs st ioState -> build (WListLSHandle itemHs) st ioState) activated info itemHs ls_ps ioState

						elementControlKeyFocusActionIO build activated info (WExtendLSHandle extLS itemHs) (ls,ps) ioState = do
							r <- elementsControlKeyFocusActionIO (\itemHs st ioState -> build (WExtendLSHandle (fst (fst st)) itemHs) ((snd (fst st)), snd st) ioState) activated info itemHs ((extLS,ls), ps) ioState
							let (done,((_,ls1),ps1),ioState)  = r
							return (done,(ls1,ps1),ioState)

						elementControlKeyFocusActionIO build activated info (WChangeLSHandle chLS itemHs) (ls,ps) ioState = do
							r <- elementsControlKeyFocusActionIO (\itemHs st ioState -> build (WChangeLSHandle (fst st) itemHs) (ls,snd st) ioState) activated info itemHs (chLS,ps) ioState
							let (done,(_,ps1),ioState) = r
							return (done,(ls,ps1),ioState)

				elementsControlKeyFocusActionIO _ _ _ [] ls_ps ioState = return (False, ls_ps, ioState)
windowStateControlKeyFocusActionIO _ _ _ _ _ _
	= windowdeviceFatalError "windowStateControlKeyFocusActionIO" "unexpected window placeholder"


{-	windowStateControlKeyboardActionIO handles the keyboard actions of (Edit/Custom/PopUp)Controls
	and CompoundControls (not yet).
	In this implementation only EditControls are taken into account.
-}
windowStateControlKeyboardActionIO :: ControlKeyboardActionInfo -> WindowStateHandle ps -> WindowHandles ps -> ps -> IOSt ps -> IO (ps,IOSt ps)
windowStateControlKeyboardActionIO info wsH@(WindowStateHandle wids (Just (WindowLSHandle {wlsState=ls,wlsHandle=wH}))) windows ps ioState
	= do {
		r <- windowControlKeyboardActionIO (\wH st ioState -> ioStSetDevice
		                                                          (WindowSystemState
		                                                              (setWindowHandlesWindow
		                                                                  (WindowStateHandle wids (Just (WindowLSHandle {wlsState=fst st,wlsHandle=wH})))
		                                                                  windows))
		                                                          ioState)
		                                   wH (ls,ps) ioState;
		let ((_,ps1),ioState1) = r
		in  return (ps1,ioState1)
	  }
	where
		windowControlKeyboardActionIO :: (WindowHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps)
		                               -> WindowHandle ls ps -> (ls,ps) -> IOSt ps -> IO ((ls,ps),IOSt ps)
		windowControlKeyboardActionIO build wH@(WindowHandle {whItems=whItems}) ls_ps ioState
			= do {
				r <- elementsControlKeyboardActionIO (\itemHs st ioState -> build (wH {whItems=itemHs}) st ioState) whItems ls_ps ioState;
				let (_,ls_ps1,ioState1) = r
				in  return (ls_ps1,ioState1)
			  }
			where
				elementsControlKeyboardActionIO :: ([WElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IOSt ps)
				                                 -> [WElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IO (Bool,(ls,ps),IOSt ps)
				elementsControlKeyboardActionIO build (itemH:itemHs) ls_ps ioState
					= do {
						r <- elementControlKeyboardActionIO (\itemH st ioState -> build (itemH:itemHs) st ioState) itemH ls_ps ioState;
						let (done,ls_ps1,ioState1) = r
						in  if   done
						    then return (done,ls_ps1,ioState1)
						    else elementsControlKeyboardActionIO (\itemHs st ioState -> build (itemH:itemHs) st ioState) itemHs ls_ps1 ioState1
					  }
					where
						elementControlKeyboardActionIO :: (WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps)
						                                -> WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IO (Bool,(ls,ps),IOSt ps)

						elementControlKeyboardActionIO build (WListLSHandle itemHs) ls_ps ioState
							= elementsControlKeyboardActionIO (\itemHs st ioState -> build (WListLSHandle itemHs) st ioState) itemHs ls_ps ioState

						elementControlKeyboardActionIO build (WExtendLSHandle addLS itemHs) (ls,ps) ioState
							= do {
								r <- elementsControlKeyboardActionIO (\itemHs st ioState -> build (WExtendLSHandle (fst (fst st)) itemHs) (snd (fst st),snd st) ioState)
								                                     itemHs ((addLS,ls),ps) ioState;
								let (done,((_,ls1),ps1),ioState1) = r
								in  return (done,(ls1,ps1),ioState1)
							  }

						elementControlKeyboardActionIO build (WChangeLSHandle newLS itemHs) (ls,ps) ioState
							= do {
								r <- elementsControlKeyboardActionIO (\itemHs st ioState -> build (WChangeLSHandle (fst st) itemHs) (ls,snd st) ioState)
								                                     itemHs (newLS,ps) ioState;
								let (done,(_,ps1),ioState1) = r
								in  return (done,(ls,ps1),ioState1)
							  }

						elementControlKeyboardActionIO build itemH ls_ps ioState
							| ckItemNr info /= wItemNr itemH =
								if not (isRecursiveControl (wItemKind itemH))
								then return (False,ls_ps,build itemH ls_ps ioState)
								else elementsControlKeyboardActionIO (\itemHs st ioState -> build (itemH{wItems = itemHs}) st ioState) (wItems itemH) ls_ps ioState
							| otherwise = do								
								r <- itemControlKeyboardActionIO build itemH ls_ps ioState
								let (ls_ps1,ioState1) = r
								return (True,ls_ps1,ioState1)								
							where
								itemControlKeyboardActionIO :: (WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps)
								                             -> WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IO ((ls,ps),IOSt ps)
								itemControlKeyboardActionIO build itemH ls_ps ioState
									= fixIO (\st -> fromGUI (f (ckKeyboardState info) ls_ps) (build itemH (fst st) ioState))
									where
										(_,_,f) = getControlKeyboardAtt (snd (cselect isControlKeyboard undefined (wItemAtts itemH)))

				elementsControlKeyboardActionIO build nil ls_ps ioState
					= return (False,ls_ps,build nil ls_ps ioState)

windowStateControlKeyboardActionIO _ _ _ _ _
	= windowdeviceFatalError "windowStateControlKeyboardActionIO" "unexpected window placeholder"


{-	windowStateControlMouseActionIO handles the mouse actions of CustomControls and CompoundControls (not yet). -}

windowStateControlMouseActionIO :: ControlMouseActionInfo -> WindowStateHandle ps -> WindowHandles ps -> ps -> IOSt ps -> IO (ps, IOSt ps)
windowStateControlMouseActionIO info wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsState=ls,wlsHandle=wH@(WindowHandle {whItems=itemHs})}))) windows ps ioState = do
	r <- elementsControlMouseActionIO (\itemHs st ioState -> ioStSetDevice
						  (WindowSystemState
						      (setWindowHandlesWindow
							  (WindowStateHandle wids (Just (WindowLSHandle {wlsState=fst st,wlsHandle=wH{whItems=itemHs}})))
							  windows))
						  ioState) info itemHs (ls,ps) ioState
	let (_,(ls,ps),ioState) = r
	return (ps,ioState)
	where
		elementsControlMouseActionIO :: ([WElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IOSt ps) ->
						ControlMouseActionInfo -> [WElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IO (Bool,(ls,ps),IOSt ps)
		elementsControlMouseActionIO build info (itemH:itemHs) ls_ps ioState = do
		    r <- elementControlMouseActionIO (\itemH st ioState -> build (itemH:itemHs) st ioState) info itemH ls_ps ioState
		    let (done,ls_ps,ioState) = r
		    (if done then return r
 		     else elementsControlMouseActionIO (\itemHs st ioState -> build (itemH:itemHs) st ioState) info itemHs ls_ps ioState)
		    where
			elementControlMouseActionIO :: (WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
							ControlMouseActionInfo -> WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IO (Bool,(ls,ps),IOSt ps)
			elementControlMouseActionIO build info itemH@(WItemHandle {wItemAtts=wItemAtts}) ls_ps ioState
				| cmItemNr info /= wItemNr itemH =
					if not (isRecursiveControl (wItemKind itemH)) then
						return (False,ls_ps,ioState)
					else elementsControlMouseActionIO (\itemHs st ioState -> build itemH{wItems=itemHs} st ioState) info (wItems itemH) ls_ps ioState
						
				| otherwise = do
					r <- fixIO (\st -> fromGUI (f (cmMouseState info) ls_ps) (build itemH (fst st) ioState))
					let (ls_ps, ioState) = r
					return (True,ls_ps,ioState)
					where
						(_,_,f)	= getControlMouseAtt (snd (cselect isControlMouse undefined wItemAtts))
			
			
			elementControlMouseActionIO build info (WListLSHandle itemHs) ls_ps ioState = do
				elementsControlMouseActionIO (\itemHs st ioState -> build (WListLSHandle itemHs) st ioState) info itemHs ls_ps ioState
			
			elementControlMouseActionIO build info (WExtendLSHandle extLS itemHs) (ls,ps) ioState = do
				r <- elementsControlMouseActionIO (\itemHs st ioState -> build (WExtendLSHandle (fst (fst st)) itemHs) (snd (fst st), snd st) ioState) info itemHs ((extLS,ls),ps) ioState
				let (done,((_,ls),ps),ioState) = r
				return (done,(ls,ps),ioState)
			
			elementControlMouseActionIO build info (WChangeLSHandle chLS itemHs) (ls,ps) ioState = do
				r <- elementsControlMouseActionIO (\itemHs st ioState -> build (WChangeLSHandle (fst st) itemHs) (ls, snd st) ioState) info itemHs (chLS,ps) ioState
				let (done,(_,ps),ioState) = r
				return (done,(ls,ps),ioState)
		
		elementsControlMouseActionIO _ _ _ ls_ps ioState = return (False,ls_ps,ioState)
windowStateControlMouseActionIO _ _ _ _ _
	= windowdeviceFatalError "windowStateControlMouseActionIO" "unexpected window placeholder"


{-	windowStateControlSelectionIO handles the selection of the control. -}

windowStateControlSelectionIO :: ControlSelectInfo -> WindowStateHandle ps -> WindowHandles ps -> ps -> IOSt ps -> IO (ps, IOSt ps)
windowStateControlSelectionIO info (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsState=ls,wlsHandle=wH}))) windows ps ioState = do
     r <- windowControlSelectionIO (\wH st ioState -> ioStSetDevice
						  (WindowSystemState
						      (setWindowHandlesWindow
							  (WindowStateHandle wids (Just (WindowLSHandle {wlsState=fst st,wlsHandle=wH})))
							  windows))
						  ioState) info wH (ls,ps) ioState
     let ((_,ps1),ioState) = r
     return (ps1,ioState)
     where
	  windowControlSelectionIO :: (WindowHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
	  			      ControlSelectInfo -> WindowHandle ls ps -> (ls,ps) -> IOSt ps -> IO ((ls, ps), IOSt ps)
	  windowControlSelectionIO build info wH@(WindowHandle {whItems=whItems}) ls_ps ioState = do
		  r <- elementsControlSelectionIO (\itemHs st ioState -> build wH{whItems=itemHs} st ioState) info whItems ls_ps ioState
		  let (_,ls_ps,ioState) = r
		  return (ls_ps,ioState)
		  where
			elementsControlSelectionIO :: ([WElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IOSt ps) ->
						      ControlSelectInfo -> [WElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IO (Bool, (ls, ps), IOSt ps)
			elementsControlSelectionIO build info (itemH:itemHs) ls_ps ioState = do
				r <- elementControlSelectionIO (\itemH st ioState -> build (itemH:itemHs) st ioState) info itemH ls_ps ioState
				let (done, ls_ps1, ioState) = r
				(if done then return r
				 else elementsControlSelectionIO (\itemHs st ioState -> build (itemH:itemHs) st ioState) info itemHs ls_ps1 ioState)
				where
					elementControlSelectionIO :: (WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
								     ControlSelectInfo -> WElementHandle ls ps -> (ls, ps) -> IOSt ps -> IO (Bool, (ls, ps), IOSt ps)
					elementControlSelectionIO build info itemH@(WItemHandle{wItemKind=wItemKind,wItemNr=wItemNr,wItems=wItems}) ls_ps ioState
						| (csItemNr info) /= wItemNr =
							if isRecursiveControl wItemKind then do
								elementsControlSelectionIO (\itemHs st ioState -> build itemH{wItems=itemHs} st ioState) info wItems ls_ps ioState								
							else return (False, ls_ps, ioState)
						| otherwise = do
							r <- itemControlSelectionIO build info itemH ls_ps ioState
							let (ls_ps1, ioState) = r
							return (True, ls_ps1, ioState)
						where
							itemControlSelectionIO :: (WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
										  ControlSelectInfo -> WElementHandle ls ps -> (ls, ps) -> IOSt ps -> IO ((ls, ps), IOSt ps)

							itemControlSelectionIO build info itemH@(WItemHandle {wItemKind=IsRadioControl,wItemInfo=wItemInfo}) ls_ps ioState =
								fixIO (\st -> fromGUI (f ls_ps) (build itemH{wItemInfo=WRadioInfo (radioInfo{radioIndex=index})} (fst st) ioState))
								where
									(_, _, f)	= radioItem radio
									itemPtr	  	= csItemPtr info
									radioInfo 	= getWItemRadioInfo wItemInfo
									error		= windowdeviceFatalError "windowIO _ (ControlSelection _) _" "RadioControlItem could not be found"
									(index,radio)	= selectedAtIndex (\(RadioItemInfo{radioItemPtr=radioItemPtr})->radioItemPtr==itemPtr) error (radioItems radioInfo)									

							itemControlSelectionIO build info itemH@(WItemHandle {wItemKind=IsCheckControl,wItemInfo=wItemInfo}) ls_ps ioState =
								fixIO (\st -> fromGUI (f ls_ps) (build itemH{wItemInfo=WCheckInfo (checkInfo{checkItems=checks})} (fst st) ioState))
								where
									itemPtr		= csItemPtr info
									checkInfo 	= getWItemCheckInfo wItemInfo
									error		= windowdeviceFatalError "windowIO _ (ControlSelection _) _" "CheckControlItem could not be found"
									(_,f,checks)	= access (isCheckItem itemPtr) error (checkItems checkInfo)

									isCheckItem :: OSWindowPtr -> CheckItemInfo ls ps -> ((Bool,GUIFun ls ps),CheckItemInfo ls ps)
									isCheckItem itemPtr check@(CheckItemInfo {checkItemPtr=checkItemPtr,checkItem=(title,width,mark,f)})
										| itemPtr==checkItemPtr
											= ((True,f), check{checkItem=(title,width,toggle mark,f)})
										| otherwise
											= ((False,return),check)

							itemControlSelectionIO build info itemH@(WItemHandle {wItemKind=IsPopUpControl,wItemInfo=wItemInfo}) ls_ps ioState =
								fixIO (\st -> fromGUI (f ls_ps) (build itemH{wItemInfo= WPopUpInfo (popUpInfo{popUpInfoIndex=index})} (fst st) ioState))
								where
									popUpInfo	= getWItemPopUpInfo wItemInfo
									index		= csMoreData info
									f		= snd ((popUpInfoItems popUpInfo) !! (index-1))
									
							itemControlSelectionIO build info itemH@(WItemHandle {wItemKind=IsListBoxControl,wItemInfo=itemInfo}) ls_ps ioState =
								fixIO (\st -> fromGUI (f ls_ps) (build itemH{wItemInfo=WListBoxInfo (lboxInfo{listBoxInfoItems=newItems})} (fst st) ioState))
								where
									lboxInfo	= getWItemListBoxInfo itemInfo
									oldItems	= listBoxInfoItems lboxInfo
									(newItems,f) 	= updateItems (csMoreData info) oldItems
									
									updateItems n [] = ([], return)
									updateItems n ((title,mark,f):items)
										| n == 1 =
											let (items',_) = updateItems (n-1) items
											in ((title,if listBoxInfoMultiSel lboxInfo then toggle mark else Mark,f):items',f)
										| otherwise =
											let (items',f') = updateItems (n-1) items
											in ((title,if listBoxInfoMultiSel lboxInfo then mark else NoMark,f):items',f')

							itemControlSelectionIO build info itemH@(WItemHandle {wItemKind=wItemKind,wItemAtts=atts}) ls_ps ioState
								| (wItemKind==IsButtonControl || wItemKind==IsCustomButtonControl) && hasAtt =
									fixIO (\st -> fromGUI (f ls_ps) (build itemH (fst st) ioState))
								| otherwise = return (ls_ps, ioState)
								where
									(hasAtt,fAtt) = cselect (\att->isControlFunction att || isControlModsFunction att) undefined atts
									f             = case fAtt of
											   ControlFunction     fun -> fun
											   ControlModsFunction fun -> fun (csModifiers info)
											   wrongAttribute          -> windowdeviceFatalError "windowStateControlSelectionIO" "argument is not a function attribute"
					elementControlSelectionIO build info (WListLSHandle itemHs) ls_ps ioState = do
						elementsControlSelectionIO (\itemHs st ioState -> build (WListLSHandle itemHs) st ioState) info itemHs ls_ps ioState

					elementControlSelectionIO build info (WExtendLSHandle extLS itemHs) (ls,ps) ioState = do
						r <- elementsControlSelectionIO (\itemHs st ioState -> build (WExtendLSHandle (fst (fst st)) itemHs) (snd (fst st), snd st) ioState) info itemHs ((extLS,ls),ps) ioState
						let (done,((_,ls1),ps1), ioState) = r
						return (done, (ls1,ps1), ioState)

					elementControlSelectionIO build info (WChangeLSHandle chLS itemHs) (ls,ps) ioState = do
						r <- elementsControlSelectionIO (\itemHs st ioState -> build (WChangeLSHandle (fst st) itemHs) (ls,snd st) ioState) info itemHs (chLS,ps) ioState
						let (done,(_,ps1),ioState) = r
						return (done,(ls,ps1),ioState)

			elementsControlSelectionIO _ _ _ ls_ps ioState = return (False, ls_ps, ioState)
windowStateControlSelectionIO _ _ _ _ _
	= windowdeviceFatalError "windowStateControlSelectionIO" "unexpected window placeholder"




{- windowStateControlSliderActionIO handles the slider of windows/dialogs. -}

windowStateControlSliderActionIO :: ControlSliderInfo -> WindowStateHandle ps -> WindowHandles ps -> ps -> IOSt ps -> IO (ps, IOSt ps)
windowStateControlSliderActionIO info (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsState=ls,wlsHandle=wH}))) windows ps ioState = do
    r <- windowControlSliderAction (\wH st ioState -> ioStSetDevice
						  (WindowSystemState
						      (setWindowHandlesWindow
							  (WindowStateHandle wids (Just (WindowLSHandle {wlsState=fst st,wlsHandle=wH})))
							  windows))
						  ioState) info wH (ls,ps) ioState
    let ((_,ps1),ioState) = r
    return (ps1,ioState)
    where
	  windowControlSliderAction :: (WindowHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
	  				ControlSliderInfo -> (WindowHandle ls ps) -> (ls,ps) -> IOSt ps -> IO ((ls,ps), IOSt ps)
	  windowControlSliderAction build info wH@(WindowHandle {whItems=itemHs}) ls_ps ioState = do
	  	r <- elementsControlSliderActionIO (\itemHs st ioState -> build wH{whItems=itemHs} st ioState) info itemHs ls_ps ioState
	  	let (_,ls_ps1, ioState) = r
		return (ls_ps1, ioState)
	        where
		  elementsControlSliderActionIO :: ([WElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IOSt ps) ->
		  				   ControlSliderInfo -> [WElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IO (Bool,(ls,ps),IOSt ps)
		  elementsControlSliderActionIO build info (itemH:itemHs) ls_ps ioState = do
			r <- elementControlSliderActionIO (\itemH st ioState -> build (itemH:itemHs) st ioState) info itemH ls_ps ioState
			let (done,ls_ps,ioState) = r
			(if done then return r
			 else elementsControlSliderActionIO (\itemHs st ioState -> build (itemH:itemHs) st ioState) info itemHs ls_ps ioState)
			where
			    elementControlSliderActionIO :: (WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
			    				    ControlSliderInfo -> (WElementHandle ls ps) -> (ls,ps) -> IOSt ps -> IO (Bool, (ls,ps), IOSt ps)
			    elementControlSliderActionIO build info itemH@(WItemHandle {wItemNr=wItemNr, wItemKind=wItemKind, wItems=itemHs}) ls_ps ioState
				  | (cslItemNr info) /= wItemNr =
					    if isRecursiveControl wItemKind then
						  elementsControlSliderActionIO (\itemHs st ioState -> build itemH{wItems=itemHs} st ioState) info itemHs ls_ps ioState
					    else return (False,ls_ps,ioState)
				  | otherwise = do
					    r <- itemControlSliderActionIO build info itemH ls_ps ioState
					    let (ls_ps,ioState) = r
					    return (True,ls_ps,ioState)
				  where
				    itemControlSliderActionIO :: (WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
				    				 ControlSliderInfo -> (WElementHandle ls ps) -> (ls,ps) -> IOSt ps -> IO ((ls,ps), IOSt ps)
				    itemControlSliderActionIO build info itemH@(WItemHandle {wItemKind=IsSliderControl, wItemInfo=wItemInfo}) ls_ps ioState = do
					fixIO (\st -> fromGUI (f ls_ps) (build itemH (fst st) ioState))
					where
						f = sliderInfoAction (getWItemSliderInfo wItemInfo) (cslSliderMove info)
				    itemControlSliderActionIO _ _ itemH ls_ps ioState = return (ls_ps, ioState)

			    elementControlSliderActionIO build info (WListLSHandle itemHs) ls_ps ioState = do
				  elementsControlSliderActionIO (\itemHs st ioState -> build (WListLSHandle itemHs) st ioState) info itemHs ls_ps ioState

			    elementControlSliderActionIO build info (WExtendLSHandle extLS itemHs) (ls,ps) ioState = do
				  r <- elementsControlSliderActionIO (\itemHs st ioState -> build (WExtendLSHandle (fst (fst st)) itemHs) (snd (fst st), snd st) ioState) info itemHs ((extLS,ls),ps) ioState
				  let (done,((_,ls1),ps1),ioState) = r
				  return (done,(ls1,ps1),ioState)

			    elementControlSliderActionIO build info (WChangeLSHandle chLS itemHs) (ls,ps) ioState = do
				  r <- elementsControlSliderActionIO (\itemHs st ioState -> build (WChangeLSHandle (fst st) itemHs) (ls,snd st) ioState) info itemHs (chLS,ps) ioState
				  let (done,(_,ps1),ioState) = r
				  return (done,(ls,ps1),ioState)

		  elementsControlSliderActionIO _ _ _ ls_ps ioState = return (False,ls_ps,ioState)
windowStateControlSliderActionIO _ _ _ _ _
	= windowdeviceFatalError "windowStateControlSliderActionIO" "unexpected window placeholder"


{-	windowStateActivationIO handles the activation of the window/dialog. -}

windowStateActivationIO :: WindowStateHandle ps -> WindowHandles ps -> ps -> IOSt ps -> IO (ps,IOSt ps)
windowStateActivationIO (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsState=ls,wlsHandle=wH}))) windows ps ioState = do
	r <- fixIO (\st -> fromGUI (f (ls,ps))
				   (ioStSetDevice
				       (WindowSystemState
					   (addWindowHandlesActiveWindow
					       (WindowStateHandle (wids {wActive=True})
								  (Just (wlsH {wlsState=fst (fst st)})))
					       windows))
				       ioState))	
	let ((_,ps1),ioState1) = r
	liftIO (when hasCaret (osCreateCaret (wPtr wids) cw ch >> osSetCaretPos (wPtr wids) (cx-ox) (cy-oy)))
	return (ps1,ioState1)
	where
		(hasAtt,att) = cselect isWindowActivate undefined (whAtts wH)
		f            = if hasAtt then getWindowActivateFun att else return
		(hasCaret,catt)           = cselect isWindowCaret undefined (whAtts wH)
		(Point2 cx cy,Size cw ch) = getWindowCaretAtt catt
		(Point2 ox oy)		  = windowOrigin (whWindowInfo wH)
windowStateActivationIO _ _ _ _
	= windowdeviceFatalError "windowStateActivationIO" "unexpected window placeholder"


{-	windowStateDeactivationIO handles the deactivation of the window/dialog.
-}
windowStateDeactivationIO :: WindowStateHandle ps -> WindowHandles ps -> ps -> IOSt ps -> IO (ps,IOSt ps)
windowStateDeactivationIO (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsState=ls,wlsHandle=wH}))) windows ps ioState = do
	r <- fixIO (\st -> fromGUI (f (ls,ps))
				   (ioStSetDevice
				       (WindowSystemState
					  (setWindowHandlesWindow
					       (WindowStateHandle (wids {wActive=False})
								  (Just (wlsH {wlsState=fst (fst st)})))
					       windows))
				       ioState))
	let ((_,ps1),ioState1) = r
	liftIO (when hasCaret (osDestroyCaret (wPtr wids)))
	return (ps1,ioState1)
	where
		(hasAtt,att) = cselect isWindowDeactivate undefined (whAtts wH)
		f            = if hasAtt then getWindowDeactivateFun att else return
		(hasCaret,catt)           = cselect isWindowCaret undefined (whAtts wH)
		(Point2 cx cy,Size cw ch) = getWindowCaretAtt catt
windowStateDeactivationIO _ _ _ _
	= windowdeviceFatalError "windowStateDeactivationIO" "unexpected window placeholder"


{-	windowStateInitialiseIO handles the initialisation of the window/dialog.  -}

windowStateInitialiseIO :: WindowStateHandle ps -> WindowHandles ps -> ps -> IOSt ps -> IO (ps,IOSt ps)
windowStateInitialiseIO (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsState=ls,wlsHandle=wH}))) windows ps ioState
	= do {
		r <- fixIO (\st -> fromGUI (f (ls,ps))
		                           (ioStSetDevice
		                               (WindowSystemState
		                                   (setWindowHandlesWindow
		                                       (WindowStateHandle wids (Just (wlsH {wlsState=fst (fst st),wlsHandle=wH1})))
		                                       windows))
		                               ioState));
		let ((_,ps1),ioState1) = r
		in  return (ps1,ioState1)
	  }
	where
		(hasAtt,initAtt,atts) = remove isWindowInit undefined (whAtts wH)
		f                     = if hasAtt then getWindowInitFun initAtt else return
		wH1                   = wH {whAtts=atts}
windowStateInitialiseIO _ _ _ _
	= windowdeviceFatalError "windowStateInitialiseIO" "unexpected window placeholder"


{-	windowStateWindowKeyboardActionIO handles the keyboard for the window. -}

windowStateWindowKeyboardActionIO :: WindowKeyboardActionInfo -> WindowStateHandle ps -> WindowHandles ps -> ps -> IOSt ps -> IO (ps, IOSt ps)
windowStateWindowKeyboardActionIO info (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsState=ls,wlsHandle=wH}))) windows ps ioState	
	= do {
		r <- fixIO (\st -> fromGUI (f (wkKeyboardState info) (ls,ps))
					   (ioStSetDevice
					       (WindowSystemState
						   (setWindowHandlesWindow
						       (WindowStateHandle wids (Just (wlsH {wlsState=fst (fst st)})))
						       windows))
					       ioState));
		let ((_,ps1),ioState1) = r;
		in return (ps1,ioState1)
	}
	where
		(_,_,f) = getWindowKeyboardAtt (snd (cselect isWindowKeyboard undefined (whAtts wH)))
windowStateWindowKeyboardActionIO _ _ _ _ _
	= windowdeviceFatalError "windowStateWindowKeyboardActionIO" "unexpected window placeholder"


{- windowStateWindowMouseActionIO handles the mouse for the window. -}

windowStateWindowMouseActionIO :: WindowMouseActionInfo -> WindowStateHandle ps -> WindowHandles ps -> ps -> IOSt ps -> IO (ps, IOSt ps)
windowStateWindowMouseActionIO info (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsState=ls,wlsHandle=wH}))) windows ps ioState
	= do {
		r <- fixIO (\st -> fromGUI (f (wmMouseState info) (ls,ps))
					   (ioStSetDevice
		                               (WindowSystemState
		                                   (setWindowHandlesWindow
		                                       (WindowStateHandle wids (Just (wlsH {wlsState=fst (fst st)})))
		                                       windows))
		                               ioState));
		let ((_,ps1),ioState1) = r;
		in return (ps1,ioState1)
	}
	where
		(_,_,f) = getWindowMouseAtt (snd (cselect isWindowMouse undefined (whAtts wH)))
windowStateWindowMouseActionIO _ _ _ _ _
	= windowdeviceFatalError "windowStateWindowMouseActionIO" "unexpected window placeholder"


{-	windowButtonActionIO id wH st
		evaluates the button function of the (Custom)ButtonControl associated with id.
	This function is used by windowStateCANCELIO and windowStateOKIO.
-}

windowButtonActionIO :: (WindowHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
			Id -> WindowHandle ls ps -> (ls,ps) -> IOSt ps -> IO ((ls,ps), IOSt ps)
windowButtonActionIO build buttonId wH@(WindowHandle {whItems=itemHs}) ls_ps ioState = do
    r <- elementsButtonActionIO (\itemHs st ioState -> build wH{whItems=itemHs} st ioState) buttonId itemHs ls_ps ioState
    let (_,ls_ps1,ioState) = r
    return (ls_ps1,ioState)
    where	
	elementsButtonActionIO :: ([WElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IOSt ps) -> 
				  Id -> [WElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IO (Bool,(ls,ps),IOSt ps)
	elementsButtonActionIO build id (itemH:itemHs) ls_ps ioState = do
	    r <- elementButtonActionIO (\itemH st ioState -> build (itemH:itemHs) st ioState) id itemH ls_ps ioState
	    let (done,ls_ps1,ioState) = r
   	    (if done then return r
 	     else elementsButtonActionIO (\itemHs st ioState -> build (itemH:itemHs) st ioState) id itemHs ls_ps1 ioState)
	    where
		elementButtonActionIO :: (WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
					 Id -> WElementHandle ls ps -> (ls,ps) -> IOSt ps -> IO (Bool,(ls,ps),IOSt ps)
		elementButtonActionIO build id itemH@(WItemHandle {wItemId=itemId,wItemKind=kind,wItemAtts=atts,wItems=itemHs}) ls_ps ioState
			| isNothing itemId || fromJust itemId /= id =
				if not (isRecursiveControl (wItemKind itemH)) then
					return (False,ls_ps,ioState)
				else elementsButtonActionIO (\itemHs st ioState -> build itemH{wItems=itemHs} st ioState) id itemHs ls_ps ioState
			| kind /= IsButtonControl && kind /= IsCustomButtonControl =
				windowdeviceFatalError "windowButtonActionIO" "Id argument does not refer to (Custom)ButtonControl"
			| hasFunAtt = do
				r <- fixIO (\st -> fromGUI (getControlFun fAtt ls_ps) (build itemH (fst st) ioState))
				let (ls_ps1, ioState) = r
				return (True,ls_ps1,ioState)
			| otherwise =
				return (True,ls_ps,ioState)
			where
				(hasFunAtt,fAtt) = cselect isControlFunction undefined atts

		elementButtonActionIO build id (WListLSHandle itemHs) ls_ps ioState = do
			elementsButtonActionIO (\itemHs st ioState -> build (WListLSHandle itemHs) st ioState) id itemHs ls_ps ioState

		elementButtonActionIO build id (WExtendLSHandle exLS itemHs) (ls,ps) ioState = do
			r <- elementsButtonActionIO (\itemHs st ioState -> build (WExtendLSHandle (fst (fst st)) itemHs) (snd (fst st), snd st) ioState) id itemHs ((exLS,ls),ps) ioState
			let (done,((_,ls1),ps1),ioState) = r
			return (done,(ls1,ps1),ioState)

		elementButtonActionIO build info (WChangeLSHandle chLS itemHs) (ls,ps) ioState = do
			r <- elementsButtonActionIO (\itemHs st ioState -> build (WChangeLSHandle (fst st) itemHs) (ls,snd st) ioState) info itemHs (chLS,ps) ioState
			let (done,(_,ps1),ioState) = r
			return (done,(ls,ps1),ioState)
	
	elementsButtonActionIO _ _ _ ls_ps ioState = return (False,ls_ps,ioState)

{-	windowStateCANCELIO handles the evaluation of the Cancel button. -}

windowStateCANCELIO :: WindowStateHandle ps -> WindowHandles ps -> ps -> IOSt ps -> IO (ps, IOSt ps)
windowStateCANCELIO (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsState=ls,wlsHandle=wH}))) windows ps ioState =
	case whCancelId wH of
		Just id -> do
			r <- windowButtonActionIO (\wH st ioState -> ioStSetDevice
						  (WindowSystemState
						      (setWindowHandlesWindow
							  (WindowStateHandle wids (Just (WindowLSHandle {wlsState=fst st,wlsHandle=wH})))
							  windows))
						  ioState) id wH (ls,ps) ioState
			let ((_, ps1), ioState) = r
			return (ps1, ioState)
		Nothing -> windowdeviceFatalError "windowStateCANCELIO" "no Cancel button administrated"	
windowStateCANCELIO _ _ _ _
	= windowdeviceFatalError "windowStateCANCELIO" "unexpected window placeholder"
	
{-	windowStateOKIO handles the evaluation of the Ok button. -}

windowStateOKIO :: WindowStateHandle ps -> WindowHandles ps -> ps -> IOSt ps -> IO (ps, IOSt ps)
windowStateOKIO (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsState=ls,wlsHandle=wH}))) windows ps ioState =
	case whDefaultId wH of
		Just id -> do
			r <- windowButtonActionIO (\wH st ioState -> ioStSetDevice
						  (WindowSystemState
						      (setWindowHandlesWindow
							  (WindowStateHandle wids (Just (WindowLSHandle {wlsState=fst st,wlsHandle=wH})))
							  windows))
						  ioState) id wH (ls,ps) ioState
			let ((_, ps1), ioState) = r
			return (ps1, ioState)
		Nothing -> windowdeviceFatalError "windowStateOKIO" "no Ok button administrated"
windowStateOKIO _ _ _ _
	= windowdeviceFatalError "windowStateOKIO" "unexpected window placeholder"


{-	windowStateRequestCloseIO handles the request to close the window/dialog.  -}

windowStateRequestCloseIO :: WindowStateHandle ps -> WindowHandles ps -> ps -> IOSt ps -> IO (ps,IOSt ps)
windowStateRequestCloseIO (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsState=ls,wlsHandle=wH}))) windows ps ioState
	= do {
		r <- fixIO (\st -> fromGUI (f (ls,ps))
		                           (ioStSetDevice
		                               (WindowSystemState
		                                   (setWindowHandlesWindow
		                                       (WindowStateHandle wids (Just (wlsH {wlsState=fst (fst st)})))
		                                       windows))
		                               ioState));
		let ((_,ps1),ioState1) = r
		in  return (ps1,ioState1)
	  }
	where
		(hasAtt,closeAtt) = cselect isWindowClose undefined (whAtts wH)
		f                 = if hasAtt then getWindowCloseFun closeAtt else return
windowStateRequestCloseIO _ _ _ _
	= windowdeviceFatalError "windowStateRequestCloseIO" "unexpected window placeholder"


{-	windowStateScrollActionIO handles the mouse action of window scrollbars. -}

windowStateScrollActionIO :: OSWindowMetrics -> WindowScrollActionInfo -> WindowStateHandle ps -> IO (WindowStateHandle ps)
windowStateScrollActionIO wMetrics info (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
    wH <- windowScrollActionIO wMetrics info wH
    return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
    where
	windowScrollActionIO :: OSWindowMetrics -> WindowScrollActionInfo -> WindowHandle ls ps -> IO (WindowHandle ls ps)
	windowScrollActionIO wMetrics info@(WindowScrollActionInfo {wsaWIDS=WIDS{wPtr=wPtr}}) wH@(WindowHandle {whItems=oldItems,whSize=whSize,whAtts=whAtts,whSelect=whSelect,whShow=whShow})
	    | newThumb==oldThumb = return wH
	    | otherwise = do
		let (_,newOSThumb,_,_) = toOSscrollbarRange (min',newThumb,max') viewSize
		let newOrigin	       = if isHorizontal then oldOrigin{x=newThumb} else oldOrigin{y=newThumb}
		osSetWindowSliderThumb wMetrics wPtr isHorizontal newOSThumb (toTuple whSize) True
		liftIO (when hasCaret (osSetCaretPos wPtr (x caretPos - x newOrigin) (y caretPos - y newOrigin)))
		(_,newItems) <- layoutControls wMetrics hMargins vMargins spaces contentSize minSize [(domain,newOrigin)] oldItems
		let wH1 = wH{whWindowInfo = windowInfo{windowOrigin=newOrigin}, whItems = newItems}
		wH2 <- forceValidWindowClipState wMetrics True wPtr wH1
		(isRect,areaRect) <- case whWindowInfo wH2 of
			WindowInfo {windowClip=ClipState{clipRgn=clipRgn}} -> osGetRgnBox clipRgn			
		updRgn <- relayoutControls wMetrics whSelect whShow contentRect contentRect zero zero wPtr (whDefaultId wH) oldItems (whItems wH2)
		wH3 <- updateWindowBackgrounds wMetrics updRgn (wsaWIDS info) wH2
		let newFrame = posSizeToRectangle newOrigin contentSize
		let toMuch = if isHorizontal then abs((x newOrigin)-(x oldOrigin))>=w' else abs ((y newOrigin)-(y oldOrigin))>=h'
		let (updArea,updAction)	= if (not (lookSysUpdate lookInfo) || toMuch || not isRect)
					  then (newFrame,return [])
					  else calcScrollUpdateArea oldOrigin newOrigin areaRect
		let updState = UpdateState{oldFrame=oldFrame,newFrame=newFrame,updArea=[updArea]}
		drawWindowLook' wMetrics wPtr updAction updState wH3
	    where
		windowInfo				= whWindowInfo wH
		(oldOrigin,domainRect,hasHScroll,hasVScroll,lookInfo)
							= (windowOrigin windowInfo,windowDomain windowInfo,isJust (windowHScroll windowInfo),isJust (windowVScroll windowInfo),windowLook windowInfo)
		isHorizontal				= wsaDirection info==Horizontal
		domain					= rectToRectangle domainRect
		visScrolls				= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)
		contentRect				= getWindowContentRect wMetrics visScrolls (sizeToRect whSize)
		contentSize				= rectSize contentRect
		Size{w=w',h=h'}				= contentSize
		oldFrame				= posSizeToRectangle oldOrigin contentSize
		(min',oldThumb,max',viewSize)		= if isHorizontal
							  then (x (corner1 domain),x oldOrigin,x (corner2 domain),w')
							  else (y (corner1 domain),y oldOrigin,y (corner2 domain),h')
		sliderState				= SliderState{sliderMin=min',sliderThumb=oldThumb,sliderMax=max'-viewSize}
		scrollInfo				= fromJust ((if isHorizontal then windowHScroll else  windowVScroll) windowInfo)
		scrollFun				= scrollFunction scrollInfo
		newThumb'				= scrollFun oldFrame sliderState (wsaSliderMove info)
		newThumb				= setBetween newThumb' min' (max'-viewSize)
		(defMinW,  defMinH)			= osMinWindowSize
		minSize					= Size{w=defMinW,h=defMinH}
		hMargins				= getWindowHMargins   IsWindow wMetrics whAtts
		vMargins				= getWindowVMargins   IsWindow wMetrics whAtts
		spaces					= getWindowItemSpaces IsWindow wMetrics whAtts
		(hasCaret,catt)           		= cselect isWindowCaret undefined whAtts
		(caretPos,_) 				= getWindowCaretAtt catt
		
	{-	calcScrollUpdateArea p1 p2 area calculates the new Rectangle that has to be updated. 
		Assumptions: p1 is the origin before scrolling,
		             p2 is the origin after  scrolling,
		             area is the visible area of the window view frame,
		             scrolling occurs either horizontally or vertically.
	-}
		calcScrollUpdateArea :: Point2 -> Point2 -> Rect -> (Rectangle,Draw [Rect])
		calcScrollUpdateArea oldOrigin newOrigin areaRect =
			(updArea,scroll newOriginAreaRect{rright=rright+1,rbottom=rbottom+1} restArea v)
			where
				newOriginAreaRect		= addVector (toVector newOrigin) areaRect
				Rect{rleft=rleft,rtop=rtop,rright=rright,rbottom=rbottom} = newOriginAreaRect
				newOriginAreaRectangle		= rectToRectangle newOriginAreaRect
				v				= toVector (oldOrigin-newOrigin)
				Vector2{vx=vx,vy=vy}		= v
				(updArea,restArea)		= 
					if (vx<0) then (newOriginAreaRectangle{corner1=Point2{x=rright+vx,y=rtop}},      newOriginAreaRect{rright =rright +vx}) else
					if (vx>0) then (newOriginAreaRectangle{corner2=Point2{x=rleft +vx,y=rbottom}},   newOriginAreaRect{rleft  =rleft  +vx}) else
					if (vy<0) then (newOriginAreaRectangle{corner1=Point2{x=rleft,    y=rbottom+vy}},newOriginAreaRect{rbottom=rbottom+vy}) else
					if (vy>0) then (newOriginAreaRectangle{corner2=Point2{x=rright,   y=rtop+vy}},   newOriginAreaRect{rtop   =rtop   +vy}) else
						       (windowdeviceFatalError "calcScrollUpdateArea (scrolling window)" "assumption violation")
			
				scroll :: Rect -> Rect -> Vector2 -> Draw [Rect]
				scroll scrollRect restRect v = do
					updRect <- pictScroll scrollRect v
					return (if updRect == zero then [] else [restRect])
				 

windowStateScrollActionIO _ _ _
	= windowdeviceFatalError "windowStateScrollActionIO" "unexpected window placeholder"


{-	windowStateSizeAction handles resizing a window and its controls. -}

windowStateSizeAction :: OSWindowMetrics -> Bool -> WindowSizeActionInfo -> WindowStateHandle ps -> ps -> GUI ps (WindowStateHandle ps, ps)
windowStateSizeAction wMetrics isActive info@(WindowSizeActionInfo {wsWIDS=WIDS{wPtr=wPtr},wsSize=wsSize,wsUpdateAll=wsUpdateAll}) (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsState=ls,wlsHandle=wH}))) ps =
    let 
  	visOldScrolls 	= osScrollbarsAreVisible wMetrics domainRect oldSize' (hasHScroll,hasVScroll)
	oldContent	= getWindowContentRect wMetrics visOldScrolls (sizeToRect oldSize)
	oldContentSize	= rectSize oldContent
	visNewScrolls	= osScrollbarsAreVisible wMetrics domainRect newSize' (hasHScroll,hasVScroll)
	newContent	= getWindowContentRect wMetrics visNewScrolls (sizeToRect wsSize)
	newContentSize	= rectSize newContent
	newOrig		= newOrigin oldOrig domainRect newContentSize
	newWindowInfo	= windowInfo{windowOrigin=newOrig}
	resizedAtt	= WindowViewSize newContentSize
	(replaced,atts)	= creplace isWindowViewSize resizedAtt (whAtts wH)
	resizedAtts	= if replaced then atts else (resizedAtt:atts)
	wH1		= wH{whSize=wsSize,whWindowInfo=newWindowInfo,whAtts=resizedAtts}
    in do
    	(ls,ps) <- getWindowResizeAtt resizeAtt oldSize wsSize (ls,ps)
	liftIO (setWindowScrollThumbValues hasHScroll wMetrics wPtr True  ((w newContentSize)+1) (x oldOrig) (x newOrig) newSize')
	liftIO (setWindowScrollThumbValues hasVScroll wMetrics wPtr False ((h newContentSize)+1) (y oldOrig) (y newOrig) newSize')
	wH <- liftIO (resizeControls wMetrics isActive wsUpdateAll (wsWIDS info) oldOrig oldContentSize newContentSize wH1)
	return (WindowStateHandle wids (Just wlsH{wlsState=ls,wlsHandle=wH}),ps)
    where
	oldSize				= whSize wH
	oldSize'			= toTuple oldSize
	newSize'			= toTuple wsSize
	windowInfo			= whWindowInfo wH
	(oldOrig,domainRect,hasHScroll,hasVScroll) = (windowOrigin windowInfo,windowDomain windowInfo,isJust (windowHScroll windowInfo),isJust (windowVScroll windowInfo))
	
	(_,resizeAtt) = cselect isWindowResize (WindowResize (\_ _ -> return)) (whAtts wH)	
	
	newOrigin :: Point2 -> Rect -> Size -> Point2
	newOrigin (Point2 {x=x,y=y}) (Rect {rleft=rleft,rtop=rtop,rright=rright,rbottom=rbottom}) (Size{w=w,h=h})
		= Point2 {x=x',y=y'}
		where
			x' = if x+w > rright  then max (rright-w) rleft else x
			y' = if y+h > rbottom then max (rbottom-h) rtop else y
	
	setWindowScrollThumbValues :: Bool -> OSWindowMetrics -> OSWindowPtr -> Bool -> Int -> Int -> Int -> (Int,Int) -> IO ()
	setWindowScrollThumbValues hasScroll wMetrics wPtr isHorizontal size old new maxcoords
		| not hasScroll	= return ()
		| otherwise = do
			osSetWindowSliderThumbSize wMetrics wPtr isHorizontal size maxcoords (old==new)
			(if old==new then return ()
			 else osSetWindowSliderThumb wMetrics wPtr isHorizontal new maxcoords True)
windowStateSizeAction _ _ _ _ _
	= windowdeviceFatalError "windowIO _ (WindowSizeAction _) _" "unexpected placeholder argument"

--	Auxiliary function (move to CommonDef??):
selectedAtIndex :: Cond x -> x -> [x] -> (Index, x)		-- if index==0 then not found; item was found at index
selectedAtIndex cond dummy xs
	= (if found then i else 0,x)
	where
		(found,i,x) = selected cond dummy xs 1

		selected :: Cond x -> x -> [x] -> Int -> (Bool,Int,x)
		selected cond dummy (x:xs) i
			| cond x    = (True,i,x)
			| otherwise = selected cond dummy xs (i+1)
		selected _ dummy _ i
			= (False,i,dummy)
