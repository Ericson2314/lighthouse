-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.WindowEvent
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- OS.WindowEvent defines the DeviceEventFunction for the window device.
-- This function is placed in a separate module because it is platform dependent.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.WindowEvent (windowEvent, module Graphics.UI.ObjectIO.Process.IOState) where


import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Control.Create
import Graphics.UI.ObjectIO.Device.Events
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.StdControlAttribute
import Graphics.UI.ObjectIO.StdWindowAttribute(isWindowKeyboard, getWindowKeyboardAtt, isWindowMouse, getWindowMouseAtt)
import Graphics.UI.ObjectIO.Window.Access
import Graphics.UI.ObjectIO.OS.ClCCall_12
import Graphics.UI.ObjectIO.OS.ClCrossCall_12
import Graphics.UI.ObjectIO.OS.Event
import Graphics.UI.ObjectIO.OS.WindowCrossCall_12
import Graphics.UI.ObjectIO.OS.Window(fromOSscrollbarRange, osScrollbarsAreVisible)
import Graphics.UI.ObjectIO.OS.Cutil_12 (int2addr)
import Graphics.UI.ObjectIO.Id(IdParent(..))
import Data.Bits		-- Defines .&. for bitand;
import Data.Int			-- Defines Int32 instance for Bits;
import Data.IORef
import qualified Data.Map as Map (lookup)
import Data.Maybe(fromJust)



windoweventFatalError :: String -> String -> x
windoweventFatalError function error
	= dumpFatalError function "OS.WindowEvent" error


{-	windowEvent filters the scheduler events that can be handled by this window device.
	For the time being no timer controls are added, so these events are ignored.
	windowEvent assumes that it is not applied to an empty IOSt.
-}
windowEvent :: IOSt ps -> SchedulerEvent -> IO (Bool,Maybe DeviceEvent,SchedulerEvent)
windowEvent ioState schedulerEvent
	| not (ioStHasDevice WindowDevice ioState)		-- This condition should never occur: WindowDevice must have been 'installed'
		= windoweventFatalError "dEvent windowFunctions" "could not retrieve WindowSystemState from IOSt"
	| otherwise
		= windowEvent2 ioState schedulerEvent
	where
		windowEvent2 :: IOSt ps -> SchedulerEvent -> IO (Bool,Maybe DeviceEvent,SchedulerEvent)

		windowEvent2 ioState schedulerEvent@(ScheduleOSEvent osEvent _)
			| isNothing maybe_filterFun
				= return (False,Nothing,schedulerEvent)
			| otherwise
				= do {
					(myEvent,replyToOS,deviceEvent)
					     <- fromJust maybe_filterFun wMetrics osEvent windows;
--					appIOEnv (ioStSetDevice (WindowSystemState windows));
					let schedulerEvent1 = if   isJust replyToOS
					                      then ScheduleOSEvent osEvent (fromJust replyToOS)
					                      else schedulerEvent
					in  return (myEvent,deviceEvent,schedulerEvent1)
				  }
			where
				(_,wDevice) 	= ioStGetDevice WindowDevice ioState
				wMetrics    	= ioStGetOSWindowMetrics ioState
				context   	= ioStGetContext ioState
				windows     	= windowSystemStateGetWindowHandles wDevice
				maybe_filterFun
					= lookup (ccMsg osEvent)
					         [
						   (ccWmACTIVATE,        filterACTIVATE)
						 , (ccWmBUTTONCLICKED,   filterBUTTONCLICKED)
						 , (ccWmCLOSE,           filterCLOSE)
						 , (ccWmITEMSELECT,      filterITEMSELECT)
						 , (ccWmDEACTIVATE,      filterDEACTIVATE)
					         , (ccWmDRAWCONTROL,     filterDRAWCONTROL)
						 , (ccWmIDLEDIALOG,      filterIDLEDIALOG)
						 , (ccWmINITDIALOG,      filterINITDIALOG)
						 , (ccWmKEYBOARD,        filterKEYBOARD context)
					      	 , (ccWmKILLFOCUS,       filterKILLFOCUS)
					         , (ccWmLOSTKEY,         filterLOSTKEY)
					         , (ccWmLOSTMOUSE,       filterLOSTMOUSE)
					         , (ccWmMOUSE,           filterMOUSE context)
					         , (ccWmPAINT,           filterPAINT)
					         , (ccWmSCROLLBARACTION, filterSCROLLBARACTION)
						 , (ccWmSETFOCUS,        filterSETFOCUS)
					         , (ccWmSIZE,            filterSIZE)
					         , (ccWmSPECIALBUTTON,   filterSPECIALBUTTON)
					         ]

		windowEvent2 ioState schedulerEvent@(ScheduleMsgEvent rId) = do
			iocontext <- readIORef (ioStGetContext ioState)
			case Map.lookup rId (ioContextGetIdTable iocontext) of
				Just idParent | idpIOId idParent == ioStGetIOId ioState && idpDevice idParent == WindowDevice ->
					let
						(_,wDevice)	= ioStGetDevice WindowDevice ioState
						windows		= windowSystemStateGetWindowHandles wDevice
						found 		= hasWindowHandlesWindow (toWID (idpId idParent)) windows
						deviceEvent	= if found then Just (ReceiverEvent rId) else Nothing
					in
						return (found,deviceEvent,schedulerEvent)
				_ ->
					return (False,Nothing,schedulerEvent)
			

{-	Because Haskell has no support for macros the function filterOSEvent that was defined by
	pattern matching on the ccWmXXX messages has now been split into distinct functions named
	filterXXX.
	No check is performed on the correct ccMsg field of the OSEvent argument.
	In this implementation not all events are handled. These have been placed in comments.
	They have however already been given the proper name and type.
-}

filterACTIVATE :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterACTIVATE _ (CrossCallInfo {p1=wptr}) windows
	| not found
		= return (False,Nothing,Nothing)
	| active                             -- The window is already active, skip
		= return (True,Nothing,Nothing)
	| otherwise
		= let windows2    = setWindowHandlesWindow wsH windows1
		      activeModal = getWindowHandlesActiveModalDialog windows2
		  in
		      return (True,Nothing,if isJust activeModal then Just (WindowInitialise wids) else Just (WindowActivation wids))
	where
		(found, wsH,windows1) = getWindowHandlesWindow (toWID wptr) windows
		active = getWindowStateHandleActive wsH
		wids   = getWindowStateHandleWIDS   wsH

filterBUTTONCLICKED :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterBUTTONCLICKED _ (CrossCallInfo {p1=wptr,p2=cptr,p3=mods,p4=toolbarIndex}) windows
	| not found
		= return (False,Nothing,Nothing)
	| not (getWindowStateHandleSelect wsH)
		= return (True,Nothing,Nothing)
	| otherwise
		= let wids 		 = getWindowStateHandleWIDS wsH
		      controlSelectInfo  =
		      		case getControlsItemNr cptr wsH of
		      			Nothing     -> Nothing
		      			Just itemNr -> Just (ControlSelection
							(ControlSelectInfo
							    { csWIDS      = wids
							    , csItemNr    = itemNr
							    , csItemPtr   = cptr
							    , csMoreData  = 0
							    , csModifiers = toModifiers mods
							    }
						    	)   )
		  in  return (True,Nothing,controlSelectInfo)
	where
		(found,wsH,_)	= getWindowHandlesWindow (toWID wptr) windows


		getControlsItemNr :: OSWindowPtr -> WindowStateHandle ps -> Maybe Int
		getControlsItemNr cPtr (WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=(WindowHandle {whItems=whItems})})))
			= getControlsItemNr cPtr whItems
			where
				getControlsItemNr :: OSWindowPtr -> [WElementHandle ls ps] -> Maybe Int
				getControlsItemNr cPtr (itemH:itemHs) =
					case getControlItemNr cPtr itemH of
						Nothing -> getControlsItemNr cPtr itemHs
						Just n  -> Just n
					where
						getControlItemNr :: OSWindowPtr -> (WElementHandle ls ps) -> Maybe Int
						getControlItemNr cPtr itemH@(WItemHandle {wItemPtr=wItemPtr})
							| cPtr==wItemPtr		= Just itemNr
							| itemKind==IsRadioControl	=
								if itemSelect && any (\RadioItemInfo{radioItemPtr=radioItemPtr}->radioItemPtr==cPtr) (radioItems (getWItemRadioInfo info))
								then Just itemNr
								else Nothing
							| itemKind==IsCheckControl	=
								if itemSelect && any (\CheckItemInfo{checkItemPtr=checkItemPtr}->checkItemPtr==cPtr) (checkItems (getWItemCheckInfo info))
								then Just itemNr
								else Nothing
							| itemSelect && (wItemShow itemH) =
								getControlsItemNr cPtr (wItems itemH)
							| otherwise			= Nothing
							where
								info		= wItemInfo itemH
								itemKind	= wItemKind itemH
								itemSelect	= wItemSelect itemH
								itemNr		= wItemNr itemH

						getControlItemNr cPtr (WListLSHandle itemHs)
							= getControlsItemNr cPtr itemHs

						getControlItemNr cPtr (WExtendLSHandle ls1 itemHs)
							= getControlsItemNr cPtr itemHs

						getControlItemNr cPtr (WChangeLSHandle ls1 itemHs)
							= getControlsItemNr cPtr itemHs

				getControlsItemNr _ _
					= Nothing

		getControlsItemNr _ _ = windoweventFatalError "filterBUTTONCLICKED" "window placeholder not expected"



filterCLOSE :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterCLOSE _ (CrossCallInfo {p1=wPtr}) windows
	| not found
		= return (False,Nothing,Nothing)
	| otherwise
		= let wids = getWindowStateHandleWIDS wsH
		  in  return (True,Nothing,Just (WindowRequestClose wids))
	where
		(found,wsH,_)  = getWindowHandlesWindow (toWID wPtr) windows

filterITEMSELECT :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterITEMSELECT _ (CrossCallInfo {ccMsg=ccWm,p1=wPtr,p2=cPtr,p3=index}) windows
	| not found
		= return (False,Nothing,Nothing)
	| not (getWindowStateHandleSelect wsH)
		= return (True,Nothing,Nothing)
	| otherwise =
		let
			wids			= getWindowStateHandleWIDS wsH
		  	itemNr			= getControlItemNr cPtr wsH
		  	controlSelectInfo	= if (itemNr==0) then Nothing	-- itemNrs are always > 0
						  else 	(Just (ControlSelection (ControlSelectInfo
						  			{csWIDS		= wids
									,csItemNr	= itemNr
									,csItemPtr	= cPtr
									,csMoreData	= index+1
									,csModifiers	= noModifiers
									}))
							)
		in return (True,Nothing,controlSelectInfo)
	where
		(found,wsH,_) 	= getWindowHandlesWindow (toWID wPtr) windows

		getControlItemNr :: OSWindowPtr -> WindowStateHandle ps -> Int
		getControlItemNr cPtr wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=wH@(WindowHandle {whItems=whItems})}))) =
			case getControlsItemNr cPtr whItems of
				Just itemNr -> itemNr
				Nothing	    -> 0
			where
				getControlsItemNr :: OSWindowPtr -> [WElementHandle ls ps] -> Maybe Int
				getControlsItemNr cPtr (itemH:itemHs) =
					case getControlItemNr cPtr itemH of
						Nothing 	-> getControlsItemNr cPtr itemHs
						x		-> x
					where
						getControlItemNr :: OSWindowPtr -> WElementHandle ls ps -> Maybe Int
						getControlItemNr cPtr itemH@(WItemHandle {wItemPtr=wItemPtr,wItemNr=wItemNr,wItemKind=wItemKind,wItemSelect=wItemSelect,wItemShow=wItemShow,wItems=wItems})
							| cPtr==wItemPtr
								= Just (if (wItemKind==IsPopUpControl || wItemKind==IsListBoxControl) && wItemSelect && wItemShow then wItemNr else 0)
							| wItemShow
								= getControlsItemNr cPtr wItems
							| otherwise
								= Nothing

						getControlItemNr cPtr (WListLSHandle itemHs)
							= getControlsItemNr cPtr itemHs

						getControlItemNr cPtr (WExtendLSHandle ls1 itemHs)
							= getControlsItemNr cPtr itemHs

						getControlItemNr cPtr (WChangeLSHandle ls1 itemHs)
							= getControlsItemNr cPtr itemHs

				getControlsItemNr _ []
					= Nothing

		getControlItemNr _ _
			= windoweventFatalError "getControlItemNr" "window placeholder not expected"

filterDEACTIVATE :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterDEACTIVATE _ (CrossCallInfo {p1=wptr}) windows
	| not found
		= return (False,Nothing,Nothing)
	| not active                         -- The window is already inactive, skip
	        = return (True,Nothing,Nothing)
	| otherwise
		= let windows2    = setWindowHandlesWindow wsH windows1
		      activeModal = getWindowHandlesActiveModalDialog windows2
		  in  return (True,Nothing,if isJust activeModal then Nothing else Just (WindowDeactivation wids))
	where
		(found,wsH,windows1)  = getWindowHandlesWindow (toWID wptr) windows
		active	= getWindowStateHandleActive wsH
		wids	= getWindowStateHandleWIDS wsH

filterDRAWCONTROL :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterDRAWCONTROL _ (CrossCallInfo {p1=wPtr,p2=cPtr,p3=gc}) windows
    | not found
	    = return (False,Nothing,Nothing)
    | otherwise	=
	    let controls = getUpdateControls cPtr wsH
		updateInfo = if null controls then Nothing
			     else (Just (WindowUpdate (UpdateInfo {updWIDS=wids,updWindowArea=zero,updControls=controls,updGContext=Just (int2addr gc)})))
	    in return (True,Nothing,updateInfo)
    where
	(found,wsH,windows1)	= getWindowHandlesWindow (toWID wPtr) windows
	wids = getWindowStateHandleWIDS wsH

	getUpdateControls :: OSWindowPtr -> WindowStateHandle ps -> [ControlUpdateInfo]
	getUpdateControls cPtr wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=wH@(WindowHandle {whItems=whItems,whSize=whSize})}))) =
	    fromJust (getUpdateControls cPtr (sizeToRect whSize) whItems)
	    where
		getUpdateControls :: OSWindowPtr -> Rect -> [WElementHandle ls ps] -> Maybe [ControlUpdateInfo]
		getUpdateControls cPtr clipRect (itemH:itemHs) =
			case getUpdateControl cPtr clipRect itemH of
			    Just controls -> Just controls
			    Nothing	  -> getUpdateControls cPtr clipRect itemHs
			where
			    getUpdateControl :: OSWindowPtr -> Rect -> WElementHandle ls ps -> Maybe [ControlUpdateInfo]
			    getUpdateControl cPtr clipRect itemH@(WItemHandle {wItemPtr=wItemPtr,wItemNr=wItemNr,wItemShow=wItemShow,wItemPos=wItemPos,wItemSize=wItemSize,wItems=wItems})
				    | cPtr==wItemPtr
					    = Just [ControlUpdateInfo {cuItemNr=wItemNr,cuItemPtr=wItemPtr,cuArea=clipRect1}]
				    | wItemShow = getUpdateControls cPtr clipRect1 wItems
				    | otherwise = Nothing
				    where
					    clipRect1 = intersectRects clipRect (posSizeToRect wItemPos wItemSize)

			    getUpdateControl cPtr clipRect (WListLSHandle itemHs) =
				    getUpdateControls cPtr clipRect itemHs

			    getUpdateControl cPtr clipRect (WExtendLSHandle exLS itemHs) =
				    getUpdateControls cPtr clipRect itemHs

			    getUpdateControl cPtr clipRect (WChangeLSHandle chLS itemHs) =
				    getUpdateControls cPtr clipRect itemHs

		getUpdateControls _ _ [] = Nothing

	getUpdateControls _ _
		= windoweventFatalError "getUpdateControls" "placeholder not expected"

{-	ccWmIDLEDIALOG is sent after a modal dialogue and its controls have been created.
		At that moment the initialisation action can be evaluated. This is done by the
		WindowInitialise device event.
-}
filterIDLEDIALOG :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterIDLEDIALOG _ (CrossCallInfo {p1=wptr}) windows
	| isNothing maybeWIDS
		= return (False,Nothing,Nothing)
	| wptr/=wPtr wids
		= return (False,Nothing,Nothing)
	| otherwise
		= return (True,Nothing,Just (WindowInitialise (fromJust maybeWIDS)))
	where
		maybeWIDS = getWindowHandlesActiveModalDialog windows
		wids      = fromJust maybeWIDS


{-	ccWmINITDIALOG is generated for modal dialogs. It contains the ptr to the modal dialog.
	It is assumed that the current active window is the modal dialogue, with a zero ptr.
	filterINITDIALOG updates the ptr field and returns the WindowCreateControls DeviceEvent.
	This causes the WindowDevice dDoIO function to create all the controls of the
	dialog.
	Note that CcWmINITDIALOG now replies to the OS only the desired position, and size.
	The focus control set to 0 as it will be set by windowCreateControls.
-}
filterINITDIALOG :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterINITDIALOG wMetrics (CrossCallInfo {p1=wptr}) windows
	| isNothing maybeWIDS
		= return (False,Nothing,Nothing)
	| wPtr wids/=0
		= return (False,Nothing,Nothing)
	| otherwise
		= return (True,Just (getDialogMetrics wsH),Just (WindowCreateControls (wids {wPtr=wptr})))
	where
		maybeWIDS = getWindowHandlesActiveWindow windows
		wids      = fromJust maybeWIDS
		(_,wsH,_) = getWindowHandlesWindow (toWID (0::OSWindowPtr)) windows

		getDialogMetrics :: WindowStateHandle ps -> [Int]
		getDialogMetrics (WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=wH})))
			= [-1,-1,w size,h size]
			where
				size  = whSize wH
		getDialogMetrics _
			= windoweventFatalError "getDialogMetrics" "placeholder not expected"

{-	ccWmKEYBOARD events are generated for windows/dialogues and keyboard sensitive
	controls.
	In this implementation windows/dialogues are ignored (p1==p2 in the OSEvent argument).
	The system also does not keep track of what GUI element has the keyboard input focus.
-}
filterKEYBOARD :: Context -> OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterKEYBOARD context _ (CrossCallInfo {p1=wptr,p2=cptr,p3=keycode,p4=state,p5=mods}) windows
	= let (found,wsH,_) = getWindowHandlesWindow (toWID wptr) windows
	  in
	  if   not found
	  then return (False,Nothing,Nothing)
	  else if   wptr==cptr			-- The keyboard action takes place in the window
	       then do
	          iocontext <- readIORef context
	          let inputTrack = ioContextGetInputTrack iocontext
		  let (deviceEvent,newInputTrack) = getWindowKeyboardEvent wsH inputTrack
		  let iocontext1 = ioContextSetInputTrack newInputTrack iocontext
		  writeIORef context iocontext1
		  return (True,Nothing,deviceEvent)
	       else return (True,Nothing,getControlKeyboardEvent wsH)
	where
		getControlKeyboardEvent :: WindowStateHandle ps -> Maybe DeviceEvent
		getControlKeyboardEvent (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH})))
			= accessWElementHandles (whItems wH)
			where				
				accessWElementHandles :: [WElementHandle ls ps] -> Maybe DeviceEvent
				accessWElementHandles (itemH:itemHs)
					= let maybe = accessWElementHandle itemH
					  in  if   isJust maybe
					      then maybe
					      else accessWElementHandles itemHs
					where
						accessWElementHandle :: WElementHandle ls ps -> Maybe DeviceEvent
						accessWElementHandle (WListLSHandle itemHs)
							= accessWElementHandles itemHs
						accessWElementHandle (WExtendLSHandle addLS itemHs)
							= accessWElementHandles itemHs
						accessWElementHandle (WChangeLSHandle newLS itemHs)
							= accessWElementHandles itemHs
						accessWElementHandle itemH@(WItemHandle {wItemNr=wItemNr,wItemAtts=wItemAtts})
							| cptr==wItemPtr itemH
								= if filter keystate
									then Just (ControlKeyboardAction
			                                                              (ControlKeyboardActionInfo
			                                                                  { ckWIDS          = wids
			                                                                  , ckItemNr        = wItemNr
			                                                                  , ckItemPtr       = cptr
			                                                                  , ckKeyboardState = keystate
			                                                                  }
			                                                          )   )
									else Nothing
							| otherwise = accessWElementHandles (wItems itemH)
							where
								(filter,_,_)  = getControlKeyboardAtt (snd (cselect isControlKeyboard (ControlKeyboard (const False) Unable undefined) wItemAtts))
				accessWElementHandles [] = Nothing
		getControlKeyboardEvent _ = windowaccessFatalError "getControlKeyboardEvent" "window placeholder not expected"
		

		getWindowKeyboardEvent :: WindowStateHandle ps -> Maybe InputTrack -> (Maybe DeviceEvent,Maybe InputTrack)
		getWindowKeyboardEvent wsH@(WindowStateHandle wids (Just (WindowLSHandle {wlsHandle=(WindowHandle {whKind=whKind,whWindowInfo=whWindowInfo,whAtts=whAtts})}))) inputTrack
		    | whKind==IsDialog = (Nothing,inputTrack)
		    | trackingKeyboard wptr 0 inputTrack =			-- Window is already handle Key(Repeat/Up)
			    if isDownKey then (Nothing,inputTrack)		-- Ignore all key down events
			    else (mb_event,if pressState==KeyUp then untrackKeyboard inputTrack else inputTrack)	-- Clear keyboard tracking
		    | isDownKey = (mb_event,trackKeyboard wptr 0 inputTrack)	-- Key down sets input track
		    | otherwise = (Nothing,inputTrack)
		    where
		    	mb_event =
		    		if filter keystate && selectState==Able 
		    			then Just (WindowKeyboardAction (WindowKeyboardActionInfo {wkWIDS=wids,wkKeyboardState=keystate})) 
		    			else Nothing
			pressState		= getKeyboardStateKeyState keystate
			isDownKey		= pressState==KeyDown False
			(filter,selectState,_)	= getWindowKeyboardAtt (snd (cselect isWindowKeyboard (WindowKeyboard (const False) Unable undefined) whAtts))
		getWindowKeyboardEvent _ _ = windoweventFatalError "getWindowKeyboardEvent" "placeholder not expected"

		keystate :: KeyboardState
		keystate
			| isJust maybeSpecial = SpecialKey (fromJust maybeSpecial) ks modifiers
			| otherwise           = CharKey (toEnum keycode) ks	-- toChar --> toEnum
			where
				modifiers       = toModifiers mods
				ks              = fromJust $ lookup state
							[(keyDOWN,  KeyDown False)
							,(keyREPEAT,KeyDown True)
							,(keyUP,    KeyUp)
							]
				maybeSpecial    = lookup keycode
							[(winBackSpKey,backSpaceKey)
							,(winBeginKey, beginKey)
							,(winDelKey,   deleteKey)
							,(winDownKey,  downKey)
							,(winEndKey,   endKey)
							,(winEscapeKey,escapeKey)
							,(winHelpKey,  helpKey)
							,(winLeftKey,  leftKey)
							,(winPgDownKey,pgDownKey)
							,(winPgUpKey,  pgUpKey)
							,(winReturnKey,enterKey)
							,(winRightKey, rightKey)
							,(winUpKey,    upKey)
							,(winF1Key,    f1Key)
							,(winF2Key,    f2Key)
							,(winF3Key,    f3Key)
							,(winF4Key,    f4Key)
							,(winF5Key,    f5Key)
							,(winF6Key,    f6Key)
							,(winF7Key,    f7Key)
							,(winF8Key,    f8Key)
							,(winF9Key,    f9Key)
							,(winF10Key,   f10Key)
							,(winF11Key,   f11Key)
							,(winF12Key,   f12Key)
							]

filterKILLFOCUS :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterKILLFOCUS _ (CrossCallInfo {p1=wPtr,p2=cPtr}) windows
	= let (found,wsH,windows1)	= getWindowHandlesWindow (toWID wPtr) windows
	  in
	    if not found
	    then return (False,Nothing,Nothing)
	    else let wids = getWindowStateHandleWIDS wsH
	         in case getControlKeyFocusItemNr False cPtr wsH of
	            	Nothing     -> return (True,Nothing,Nothing)
	            	Just itemNr -> return (True,Nothing,Just (ControlLooseKeyFocus (ControlKeyFocusInfo {ckfWIDS=wids,ckfItemNr=itemNr,ckfItemPtr=cPtr})))


filterLOSTKEY :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterLOSTKEY _ (CrossCallInfo {p1=wPtr,p2=cPtr}) windows =
	let (found,wsH,windows1) = getWindowHandlesWindow (toWID wPtr) windows
	in
	  if not found || not (getWindowStateHandleSelect wsH)
	  then return (found,Nothing,Nothing)
	  else
	    let wids = getWindowStateHandleWIDS wsH
	    in
	      if wPtr==cPtr then	-- The window lost the keyboard input
		let info = WindowKeyboardActionInfo
			{ wkWIDS=wids
			, wkKeyboardState=KeyLost
			}
		    deviceEvent = if okWindowKeyLost wsH then Just (WindowKeyboardAction info) else Nothing
		in return (True,Nothing,deviceEvent)
	      else
		let (ok,itemNr)	= okControlItemNrsKeyLost cPtr wsH
		    info = ControlKeyboardActionInfo
		        { ckWIDS	  = wids
			, ckItemNr	  = itemNr
			, ckItemPtr	  = cPtr
			, ckKeyboardState = KeyLost
			}
		    deviceEvent	= if (ok && itemNr>0) then (Just (ControlKeyboardAction info)) else Nothing
		in return (True,Nothing,deviceEvent)
	where
		okWindowKeyLost :: WindowStateHandle ps -> Bool
		okWindowKeyLost (WindowStateHandle wids' (Just (WindowLSHandle {wlsHandle=(WindowHandle {whKind=whKind,whAtts=whAtts})})))
		    | whKind==IsDialog = False
		    | otherwise =
			 if filter KeyLost && selectState==Able
			 then True
			 else False
		    where
		       (filter,selectState,_) = getWindowKeyboardAtt (snd (cselect isWindowKeyboard (WindowKeyboard (const False) Unable undefined) whAtts))
		okWindowKeyLost _
			= windoweventFatalError "okWindowKeyLost" "placeholder not expected"

		okControlItemNrsKeyLost :: OSWindowPtr -> WindowStateHandle ps -> (Bool,Int)
		okControlItemNrsKeyLost itemPtr (WindowStateHandle wids' (Just (WindowLSHandle {wlsHandle=(WindowHandle {whItems=whItems})}))) =
		     fromJust (okControlsItemNrKeyLost True itemPtr whItems)
		     where
			okControlsItemNrKeyLost :: Bool -> OSWindowPtr -> [WElementHandle ls ps] -> Maybe (Bool,Int)
			okControlsItemNrKeyLost contextAble itemPtr (itemH:itemHs) =
			    case okControlItemNrKeyLost contextAble itemPtr itemH of
			      Just x -> Just x
			      Nothing -> okControlsItemNrKeyLost contextAble itemPtr itemHs
			    where
				okControlItemNrKeyLost :: Bool -> OSWindowPtr -> WElementHandle ls ps -> Maybe (Bool,Int)
				okControlItemNrKeyLost contextAble itemPtr itemH@(WItemHandle {wItemPtr=wItemPtr,wItemNr=wItemNr,wItemSelect=wItemSelect,wItemShow=wItemShow,wItemAtts=wItemAtts,wItems=wItems})
					| itemPtr /= wItemPtr =
						if wItemShow then okControlsItemNrKeyLost contextAble1 itemPtr wItems
						else Nothing
					| otherwise = Just (okKeyAtt,wItemNr)
					where
					  contextAble1= contextAble && wItemSelect
					  (filter,selectState,_) = getControlKeyboardAtt (snd (cselect isControlKeyboard (ControlKeyboard (const False) Unable undefined) wItemAtts))
					  okKeyAtt	= contextAble1 && enabled selectState && filter KeyLost

				okControlItemNrKeyLost contextAble itemPtr (WListLSHandle itemHs) =
					okControlsItemNrKeyLost contextAble itemPtr itemHs

				okControlItemNrKeyLost contextAble itemPtr (WExtendLSHandle _ itemHs) =
					okControlsItemNrKeyLost contextAble itemPtr itemHs

				okControlItemNrKeyLost contextAble itemPtr (WChangeLSHandle _ itemHs) =
					okControlsItemNrKeyLost contextAble itemPtr itemHs

			okControlsItemNrKeyLost _ _ [] = Nothing

		okControlItemNrsKeyLost _ _
			= windoweventFatalError "okControlItemNrsKeyLost" "placeholder not expected"


filterLOSTMOUSE :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterLOSTMOUSE _ (CrossCallInfo {p1=wPtr,p2=cPtr}) windows
	| not found = return (False,Nothing,Nothing)
	| not able  = return (True, Nothing,Nothing)
	| wPtr==cPtr =	-- The window lost the mouse input		
	    let deviceEvent =
			if okWindowMouseLost wsH 
			then (Just (WindowMouseAction (WindowMouseActionInfo {wmWIDS=wids,wmMouseState=MouseLost})))
			else Nothing
	    in return (True,Nothing,deviceEvent)
	| otherwise =		-- One of the window controls lost the mouse input
		let 
			(ok,itemNr) = okControlItemNrsMouseLost cPtr wsH
			info = ControlMouseActionInfo
				{ cmWIDS	= wids
				, cmItemNr	= itemNr
				, cmItemPtr	= cPtr
				, cmMouseState	= MouseLost
				}
		     	deviceEvent = if ok && itemNr>0 then (Just (ControlMouseAction info)) else Nothing
		in return (True,Nothing,deviceEvent)
	where
		(found,wsH,windows1) = getWindowHandlesWindow (toWID wPtr) windows
		able		     = getWindowStateHandleSelect wsH
		wids		     = getWindowStateHandleWIDS wsH
	
		okWindowMouseLost :: WindowStateHandle ps -> Bool
		okWindowMouseLost (WindowStateHandle wids' (Just (WindowLSHandle {wlsHandle=(WindowHandle {whKind=whKind,whAtts=whAtts})})))
			| whKind==IsDialog = False
			| otherwise = okMouseAtt
			where
				(filter,selectState,_)	= getWindowMouseAtt (snd (cselect isWindowMouse (WindowMouse (const False) Unable undefined) whAtts))
				okMouseAtt		= filter MouseLost && selectState==Able
		okWindowMouseLost _ =
		    windoweventFatalError "okWindowMouseLost" "placeholder not expected"

		okControlItemNrsMouseLost :: OSWindowPtr -> WindowStateHandle ps -> (Bool,Int)
		okControlItemNrsMouseLost itemPtr (WindowStateHandle wids' (Just (WindowLSHandle {wlsHandle=(WindowHandle {whItems=whItems})}))) =
		    let (_,ok,itemNr) = okControlsItemNrMouseLost True itemPtr whItems
		    in (ok,itemNr)
		    where
			okControlsItemNrMouseLost :: Bool -> OSWindowPtr -> [WElementHandle ls ps] -> (Bool,Bool,Int)
			okControlsItemNrMouseLost contextAble itemPtr (itemH:itemHs) =
			    let (found,ok,itemNr) = okControlItemNrMouseLost contextAble itemPtr itemH
  			    in if found then (found,ok,itemNr)
			       else okControlsItemNrMouseLost contextAble itemPtr itemHs
			    where
				okControlItemNrMouseLost :: Bool -> OSWindowPtr -> WElementHandle ls ps -> (Bool,Bool,Int)
				okControlItemNrMouseLost contextAble itemPtr itemH@(WItemHandle {wItemPtr=wItemPtr,wItemNr=wItemNr,wItemSelect=wItemSelect,wItemShow=wItemShow,wItemAtts=wItemAtts,wItems=wItems})
					| itemPtr /= wItemPtr =
						if wItemShow
						then okControlsItemNrMouseLost contextAble1 itemPtr wItems							
						else (False,False,0)
					| otherwise = (True,okMouseAtt,wItemNr)
					where
						contextAble1= contextAble && wItemSelect
						(filter,selectState,_) = getControlMouseAtt (snd (cselect isControlMouse (ControlMouse (const False) Unable undefined) wItemAtts))
						okMouseAtt	= contextAble1 && enabled selectState && filter MouseLost

				okControlItemNrMouseLost contextAble itemPtr (WListLSHandle itemHs) =
					okControlsItemNrMouseLost contextAble itemPtr itemHs					

				okControlItemNrMouseLost contextAble itemPtr (WExtendLSHandle exLS itemHs) =
					okControlsItemNrMouseLost contextAble itemPtr itemHs

				okControlItemNrMouseLost contextAble itemPtr (WChangeLSHandle chLS itemHs) =
					okControlsItemNrMouseLost contextAble itemPtr itemHs

			okControlsItemNrMouseLost _ _ []
				= (False,False,0)

		okControlItemNrsMouseLost _ _
			= windoweventFatalError "okControlItemNrsMouseLost" "placeholder not expected"


filterMOUSE :: Context -> OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterMOUSE context _ (CrossCallInfo {p1=wPtr,p2=cPtr,p3=action,p4=x,p5=y,p6=mods}) windows
	| not found = return (False,Nothing,Nothing)
	| not able  = return (True, Nothing,Nothing)
	| wPtr==cPtr =	-- The mouse action takes place in the window
	    do
	      	iocontext <- readIORef context
		let inputTrack = ioContextGetInputTrack iocontext
		let (ok,mouse,newInputTrack) = okWindowMouseState action (Point2{x=x,y=y}) wsH inputTrack
		let deviceEvent = if ok then (Just (WindowMouseAction (WindowMouseActionInfo {wmWIDS=wids,wmMouseState=mouse}))) else Nothing
		let iocontext1 = ioContextSetInputTrack newInputTrack iocontext
		writeIORef context iocontext1
	        return (True,Nothing,deviceEvent)

	| otherwise = do	 			-- The mouse action takes place in a control
		iocontext <- readIORef context
		let inputTrack = ioContextGetInputTrack iocontext
		let (ok,itemNr,mouse,newInputTrack) = okControlItemsNrMouseState wPtr cPtr action (Point2{x=x,y=y}) wsH inputTrack
		let iocontext1 = ioContextSetInputTrack newInputTrack iocontext
		writeIORef context iocontext1
		let info = ControlMouseActionInfo
				{ cmWIDS	= wids
				, cmItemNr	= itemNr
				, cmItemPtr	= cPtr
				, cmMouseState	= mouse
				}
		let deviceEvent	= if ok then Just (ControlMouseAction info) else Nothing		  
		return (True,Nothing,deviceEvent)
	where
		okWindowMouseState :: Int -> Point2 -> WindowStateHandle ps -> Maybe InputTrack
							   -> (Bool,MouseState,Maybe InputTrack)
		okWindowMouseState action eventPos (WindowStateHandle wids' (Just (WindowLSHandle {wlsHandle=(WindowHandle {whKind=whKind,whWindowInfo=windowInfo,whAtts=whAtts})}))) inputTrack
		    | whKind==IsDialog =
			  (False,undefined,inputTrack)
		    | trackingMouse wPtr 0 inputTrack =					-- Window is already handling Mouse(Drag/Up)
			  if isDownButton || buttonstate==ButtonStillUp 		-- Ignore all mouse down and mouse move events
			  then (False,undefined,inputTrack)
			  else if buttonstate==ButtonUp					-- Clear mouse tracking
			      then (okMouseAtt,mousestate,untrackMouse inputTrack)
			      else (okMouseAtt,mousestate,inputTrack)
		     | isDownButton =							-- Mouse down event sets input track
			  (okMouseAtt,mousestate,trackMouse wPtr 0 inputTrack)
		     | buttonstate `elem` [ButtonStillDown,ButtonUp]	=		-- Ignore all mouse drag and up events when not tracking
			  (False,undefined,inputTrack)
		     | otherwise =
			  (okMouseAtt,mousestate,inputTrack)
		     where
			origin			= windowOrigin windowInfo
			mousestate		= mouseState action (eventPos+origin)
			buttonstate		= getMouseStateButtonState mousestate
			isDownButton		= buttonstate `elem` [ButtonDown,ButtonDoubleDown,ButtonTripleDown]
			(filter,selectState,_)	= getWindowMouseAtt (snd (cselect isWindowMouse (WindowMouse (const False) Unable undefined) whAtts))
			okMouseAtt		= filter mousestate && selectState==Able
		okWindowMouseState _ _ _ _ =
		    windoweventFatalError "okWindowMouseState" "placeholder not expected"
			
			
		okControlItemsNrMouseState :: OSWindowPtr -> OSWindowPtr -> Int -> Point2 -> WindowStateHandle ps -> Maybe InputTrack
												   -> (Bool,Int,MouseState,Maybe InputTrack)
		okControlItemsNrMouseState wPtr itemPtr action eventPos (WindowStateHandle wids' (Just (WindowLSHandle {wlsHandle=(WindowHandle {whItems=whItems})}))) inputTrack =
	  	    let (_,ok,itemNr,itemPos,newInputTrack) = okControlsItemNrMouseState True wPtr itemPtr action eventPos whItems inputTrack
	  	    in (ok,itemNr,itemPos,newInputTrack)
   		    where
			okControlsItemNrMouseState :: Bool -> OSWindowPtr -> OSWindowPtr -> Int -> Point2 -> [WElementHandle ls ps] -> Maybe InputTrack
													   -> (Bool,Bool,Int,MouseState,Maybe InputTrack)
			okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos (itemH:itemHs) inputTrack =
			    let res@(found,ok,itemNr,itemPos,newInputTrack) = okControlItemNrMouseState contextAble wPtr itemPtr action eventPos itemH inputTrack
			    in if found then res
			       else okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos itemHs newInputTrack
			    where
				okControlItemNrMouseState :: Bool -> OSWindowPtr -> OSWindowPtr -> Int -> Point2 -> WElementHandle ls ps -> Maybe InputTrack
														   -> (Bool,Bool,Int,MouseState,Maybe InputTrack)
				okControlItemNrMouseState contextAble wPtr itemPtr action eventPos
										  itemH@(WItemHandle {wItemPtr=wItemPtr
										  		     ,wItemSelect=wItemSelect
										  		     ,wItemKind=wItemKind
										  		     ,wItemNr=wItemNr
										  		     ,wItemShow=wItemShow
										  		     ,wItemAtts=wItemAtts
										  		     ,wItems=wItems
										  		     ,wItemInfo=wItemInfo})
										  inputTrack
					| itemPtr /= wItemPtr =
						if wItemShow then okControlsItemNrMouseState contextAble1 wPtr itemPtr action eventPos wItems inputTrack
						else (False,False,0,undefined,inputTrack)
					| trackingMouse wPtr itemPtr inputTrack	=			-- Control is already handling Mouse(Drag/Up)
						if isDownButton || buttonstate==ButtonStillUp then	-- Ignore all mouse down and mouse move events
							(True,False,0,undefined,inputTrack)
						else 
						   if buttonstate==ButtonUp then			-- Clear mouse tracking
							(True,okMouseAtt,wItemNr,mousestate,untrackMouse inputTrack)
						   else
							(True,okMouseAtt,wItemNr,mousestate,inputTrack)
					| isDownButton =										-- Mouse down event sets input track 
						(True,okMouseAtt,wItemNr,mousestate,trackMouse wPtr itemPtr inputTrack)
					| buttonstate `elem` [ButtonStillDown,ButtonUp]	= -- Ignore all mouse drag and up events when not tracking
						(True,False,0,undefined,inputTrack)
					| otherwise =
						(True,okMouseAtt,wItemNr,mousestate,inputTrack)
					where
						contextAble1 = contextAble && wItemSelect
						(filter,selectState,_) = getControlMouseAtt (snd (cselect isControlMouse (ControlMouse (const False) Unable undefined) wItemAtts))
						okMouseAtt	= contextAble1 && enabled selectState && filter mousestate
						mousestate	= mouseState action (origin+eventPos)
						buttonstate	= getMouseStateButtonState mousestate
						isDownButton    = buttonstate `elem` [ButtonDown,ButtonDoubleDown,ButtonTripleDown]
						origin		= case wItemKind of
										IsCustomButtonControl	-> zero
										IsCustomControl		-> zero
										IsCompoundControl	-> compoundOrigin (getWItemCompoundInfo wItemInfo)
										_			-> windoweventFatalError "okControlItemsNrMouseState" "mouse event generated for unexpected control"

				okControlItemNrMouseState contextAble wPtr itemPtr action eventPos (WListLSHandle itemHs) inputTrack =
					okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos itemHs inputTrack					

				okControlItemNrMouseState contextAble wPtr itemPtr action eventPos (WExtendLSHandle exLS itemHs) inputTrack =
					okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos itemHs inputTrack					

				okControlItemNrMouseState contextAble wPtr itemPtr action eventPos (WChangeLSHandle chLS itemHs) inputTrack =
					okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos itemHs inputTrack					

			okControlsItemNrMouseState _ _ _ _ _ [] inputTrack = (False,False,0,undefined,inputTrack)

		okControlItemsNrMouseState _ _ _ _ _ _
			= windoweventFatalError "okControlItemsNrMouseState" "placeholder not expected"
			
		(found,wsH,windows1)	= getWindowHandlesWindow (toWID wPtr) windows
		able			= getWindowStateHandleSelect wsH
		wids			= getWindowStateHandleWIDS wsH

		modifiers	= toModifiers mods
		nrDown		=
			if action == buttonDOWN       then 1 else
			if action == buttonDOUBLEDOWN then 2 else
			 				   3
		mouseState action pos =
			if action == buttonSTILLUP   then MouseMove pos modifiers else 
			if action == buttonUP        then MouseUp   pos modifiers else 
			if action == buttonSTILLDOWN then MouseDrag pos modifiers else 
						          MouseDown pos modifiers nrDown
						     

{-	The ccWmPAINT message is generated to update the indicated rectangle of the argument window. -}
filterPAINT :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterPAINT _ (CrossCallInfo {p1=wptr,p2=left,p3=top,p4=right,p5=bottom,p6=gc}) windows
	| not found
		= return (False,Nothing,Nothing)
	| otherwise
		= let wids 	  = getWindowStateHandleWIDS wsH
		      updRect     = fromTuple4 (left,top,right,bottom)
		      updateInfo  = UpdateInfo
		                       { updWIDS       = wids
		                       , updWindowArea = updRect
		                       , updControls   = []
		                       , updGContext   = if gc==0 then Nothing else Just (int2addr gc)
		                       }
		  in  return (True,Nothing,Just (WindowUpdate updateInfo))
	where
		(found,wsH,_)     = getWindowHandlesWindow (toWID wptr) windows


filterSCROLLBARACTION :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterSCROLLBARACTION _ (CrossCallInfo {p1=wptr,p2=cPtr,p3=iBar,p4=action,p5=osThumb}) windows
  | not found = return (False,Nothing,Nothing)
  | not (getWindowStateHandleSelect wsH) = return (True,Nothing,Nothing)
  | otherwise =
	let wids = getWindowStateHandleWIDS wsH
	    sliderEvent = getSlidersEvent wids iBar osThumb cPtr wsH
	in return (True,Nothing,Just sliderEvent)
  where
     (found,wsH,windows1) = getWindowHandlesWindow (toWID wptr) windows

     getSlidersEvent :: WIDS -> Int -> Int -> OSWindowPtr -> WindowStateHandle ps -> DeviceEvent
     getSlidersEvent wids iBar osThumb itemPtr (WindowStateHandle wids' (Just (WindowLSHandle {wlsHandle=wH@(WindowHandle {whWindowInfo=windowInfo,whItems=itemHs,whSize=Size w h})})))
	 | wPtr wids == itemPtr =
	 	let isHorizontal		= iBar==sb_HORZ
		    info = WindowScrollActionInfo
		    	{ wsaWIDS	= wids
			, wsaSliderMove	= move min max view osThumb
			, wsaDirection	= if isHorizontal then Horizontal else Vertical
			}
		    domainRect	= windowDomain windowInfo
		    (min,max,view) = if isHorizontal then (rleft domainRect,rright domainRect, w)
				     else (rtop domainRect, rbottom domainRect, h)
		in WindowScrollAction info
	| otherwise =
		case getSlidersEvent wids iBar osThumb itemPtr itemHs of
		  Just sliderEvent -> sliderEvent
		  Nothing	   -> windoweventFatalError "getSlidersEvent" "SliderControl could not be located"
		where
		  getSlidersEvent :: WIDS -> Int -> Int -> OSWindowPtr -> [WElementHandle ls ps] -> Maybe DeviceEvent
		  getSlidersEvent wids iBar osThumb itemPtr (itemH:itemHs) =
		      case getSliderEvent wids iBar osThumb itemPtr itemH of
		         Just sliderEvent -> Just sliderEvent
		         Nothing 	  -> getSlidersEvent wids iBar osThumb itemPtr itemHs
		      where
			  getSliderEvent :: WIDS -> Int -> Int -> OSWindowPtr -> WElementHandle ls ps -> Maybe DeviceEvent
			  getSliderEvent wids iBar osThumb itemPtr itemH@(WItemHandle {wItemPtr=wItemPtr,wItemNr=wItemNr,wItemKind=wItemKind,wItemShow=wItemShow,wItems=itemHs,wItemInfo=wItemInfo,wItemSize=Size w h})
			      | itemPtr /= wItemPtr =
				      if wItemShow then getSlidersEvent wids iBar osThumb itemPtr itemHs
				      else Nothing
			      | wItemKind==IsCompoundControl =
				      let isHorizontal	= iBar==sb_HORZ
					  info = CompoundScrollActionInfo
					  	{ csaWIDS	= wids
						, csaItemNr	= wItemNr
						, csaItemPtr	= itemPtr
						, csaSliderMove	= move min max view osThumb
						, csaDirection	= if isHorizontal then Horizontal else Vertical
						}
					  compoundInfo	= getWItemCompoundInfo wItemInfo
					  domainRect		= compoundDomain compoundInfo
					  (min,max,view)	= if isHorizontal then (rleft domainRect,rright domainRect, w)
								  else (rtop domainRect, rbottom domainRect,h)
				      in Just (CompoundScrollAction info)
			      | otherwise =
				      let info = ControlSliderInfo
				      		{ cslWIDS	= wids
						, cslItemNr	= wItemNr
						, cslItemPtr	= itemPtr
						, cslSliderMove	= move (sliderMin sliderState) (sliderMax sliderState) 0 osThumb
						}
					  sliderInfo	= getWItemSliderInfo wItemInfo
					  sliderState	= sliderInfoState sliderInfo
				      in Just (ControlSliderAction info)

			  getSliderEvent wids iBar osThumb itemPtr (WListLSHandle itemHs) =
			      getSlidersEvent wids iBar osThumb itemPtr itemHs

			  getSliderEvent wids iBar osThumb itemPtr (WExtendLSHandle exLS itemHs) =
			      getSlidersEvent wids iBar osThumb itemPtr itemHs

			  getSliderEvent wids iBar osThumb itemPtr (WChangeLSHandle chLS itemHs) =
			      getSlidersEvent wids iBar osThumb itemPtr itemHs

		  getSlidersEvent _ _ _ _ []
			= Nothing

     getSlidersEvent _ _ _ _ _
		= windoweventFatalError "getSlidersEvent" "placeholder not expected"

     move :: Int -> Int -> Int -> Int -> SliderMove
     move min max view osThumb = fromJust maybe_move
       where
	  maybe_move = lookup action
	     [ (sb_LINEUP,	SliderDecSmall)
	     , (sb_LINEDOWN,	SliderIncSmall)
	     , (sb_PAGEUP,	SliderDecLarge)
	     , (sb_PAGEDOWN,	SliderIncLarge)
	     , (sb_THUMBPOSITION,	SliderThumb (fromOSscrollbarRange (min,max) osThumb))
	     , (sb_THUMBTRACK,	SliderThumb (fromOSscrollbarRange (min,max) osThumb))
	     , (sb_TOP,		SliderThumb min)
	     , (sb_BOTTOM,	SliderThumb (max-view))
	     , (sb_ENDSCROLL,	SliderThumb (fromOSscrollbarRange (min,max) osThumb))
	     ]



filterSETFOCUS :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterSETFOCUS _ (CrossCallInfo {p1=wPtr,p2=cPtr}) windows
	= let (found,wsH,windows1) = getWindowHandlesWindow (toWID wPtr) windows
	  in if not found
	     then return (False,Nothing,Nothing)
	     else let wids = getWindowStateHandleWIDS wsH
		  in case getControlKeyFocusItemNr True cPtr wsH of
		  	Nothing     -> return (True,Nothing,Nothing)
		  	Just itemNr -> return (True,Nothing,Just (ControlGetKeyFocus (ControlKeyFocusInfo {ckfWIDS=wids,ckfItemNr=itemNr,ckfItemPtr=cPtr})))


filterSIZE :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterSIZE wMetrics (CrossCallInfo {p1=wptr,p2=w,p3=h,p4=usersizing}) windows
	| not found
		= return (False,Nothing,Nothing)
	| getWindowStateHandleWindowKind wsH ==IsDialog		-- This alternative should never occur
		= windoweventFatalError "filterSIZE" "WindowSizeAction event generated for Dialog"
	| otherwise
		= let wids = getWindowStateHandleWIDS wsH
		      info = getWindowStateHandleSize wids w h (usersizing/=0) wsH
		  in  return (True,Nothing,Just (WindowSizeAction info))
	where
		(found,wsH,windows1) = getWindowHandlesWindow (toWID wptr) windows

		getWindowStateHandleSize :: WIDS -> Int -> Int -> Bool -> WindowStateHandle ps -> WindowSizeActionInfo
		getWindowStateHandleSize wids newW newH usersizing wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=wH})))
			= WindowSizeActionInfo
			      { wsWIDS=wids
			      , wsSize=Size {w=newW',h=newH'}
			      , wsUpdateAll=not usersizing
			      }			  
			where
				windowInfo = whWindowInfo wH
				domainRect = windowDomain windowInfo
				hasScrolls = (isJust (windowHScroll windowInfo),isJust (windowVScroll windowInfo))
				(visHScroll,visVScroll)
				           = osScrollbarsAreVisible wMetrics domainRect (toTuple (whSize wH)) hasScrolls
				newW'      = if visVScroll then (newW+osmVSliderWidth  wMetrics) else newW	-- Correct newW in case of visible vertical   scrollbar
				newH'      = if visHScroll then (newH+osmHSliderHeight wMetrics) else newH	-- Correct newH in case of visible horizontal scrollbar
		getWindowStateHandleSize _ _ _ _ _
			= windoweventFatalError "getWindowStateHandleSize" "placeholder not expected"


filterSPECIALBUTTON :: OSWindowMetrics -> OSEvent -> WindowHandles ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)
filterSPECIALBUTTON _ (CrossCallInfo {p1=wPtr,p2=okOrCancel}) windows =
	let (found,wsH,windows1) = getWindowHandlesWindow (toWID wPtr) windows
	in if not found then return (False,Nothing,Nothing)
	   else let wids = getWindowStateHandleWIDS wsH
		    okId	= getWindowStateHandleDefaultId wsH
		    cancelId	= getWindowStateHandleCancelId  wsH
		    okOrCancelEvent =
			if okOrCancel == isOKBUTTON     then (if (isJust okId)     then (Just (WindowOK wids))     else Nothing) else
		  	if okOrCancel == isCANCELBUTTON then (if (isJust cancelId) then (Just (WindowCANCEL wids)) else Nothing) else
		  	           windoweventFatalError "filterOSEvent (ccWmSPECIALBUTTON)" "incorrect argument"
		in return (True,Nothing,okOrCancelEvent)


toModifiers :: Int -> Modifiers
toModifiers i
	= Modifiers
		{ shiftDown   = shifton
		, optionDown  = alton
		, commandDown = ctrlon
		, controlDown = ctrlon
		, altDown     = alton
		}
	where
		shifton = (i1 .&. (fromIntegral shiftBIT)) /= 0
		alton   = (i1 .&. (fromIntegral altBIT))   /= 0
		ctrlon  = (i1 .&. (fromIntegral ctrlBIT))  /= 0
		i1      = fromIntegral i :: Int32


getControlKeyFocusItemNr :: Bool -> OSWindowPtr -> WindowStateHandle ps -> Maybe Int
getControlKeyFocusItemNr activated cPtr wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=wH})))
	= getControlsKeyFocusItemNr' activated cPtr (whItems wH)
	where
		getControlsKeyFocusItemNr' :: Bool -> OSWindowPtr -> [WElementHandle ls ps] -> Maybe Int
		getControlsKeyFocusItemNr' activated cPtr [] = Nothing
		getControlsKeyFocusItemNr' activated cPtr (itemH:itemHs) =
			case getControlKeyFocusItemNr' activated cPtr itemH of
				Nothing 	-> getControlsKeyFocusItemNr' activated cPtr itemHs
				Just itemNr 	-> Just itemNr
			where
				getControlKeyFocusItemNr' :: Bool -> OSWindowPtr -> WElementHandle ls ps -> Maybe Int
				getControlKeyFocusItemNr' activated cPtr itemH@(WItemHandle {wItemPtr=wItemPtr,wItemNr=wItemNr,wItemKind=wItemKind,wItemSelect=wItemSelect,wItemAtts=wItemAtts,wItems=wItems})
					| cPtr==wItemPtr =
						if (wItemKind `elem` [IsCompoundControl,IsCustomControl,IsEditControl,IsPopUpControl]) &&
						   wItemSelect && (any reqAttribute wItemAtts)
						then Just wItemNr
						else Nothing
					| isRecursiveControl wItemKind = getControlsKeyFocusItemNr' activated cPtr wItems
					| otherwise = Nothing
					where
						reqAttribute =
							if activated then isControlActivate
							else isControlDeactivate

				getControlKeyFocusItemNr' activated cPtr (WListLSHandle itemHs)
					= getControlsKeyFocusItemNr' activated cPtr itemHs

				getControlKeyFocusItemNr' activated cPtr (WExtendLSHandle _ itemHs)
					= getControlsKeyFocusItemNr' activated cPtr itemHs

				getControlKeyFocusItemNr' activated cPtr (WChangeLSHandle _ itemHs)
					= getControlsKeyFocusItemNr' activated cPtr itemHs

getControlKeyFocusItemNr _ _ _
	= windoweventFatalError "getControlKeyFocusItemNr" "window placeholder not expected"


{-	The following operations on InputTrack are ignored. -}
--	Access operations on InputTrack:

trackingMouse :: OSWindowPtr -> OSWindowPtr -> Maybe InputTrack -> Bool
trackingMouse wPtr cPtr (Just (InputTrack {itWindow=itWindow,itControl=itControl,itKind=InputTrackKind{itkMouse=itkMouse}}))
	= wPtr==itWindow && cPtr==itControl && itkMouse
trackingMouse _ _ _ = False

trackingKeyboard :: OSWindowPtr -> OSWindowPtr -> Maybe InputTrack -> Bool
trackingKeyboard wPtr cPtr (Just (InputTrack {itWindow=itWindow,itControl=itControl,itKind=InputTrackKind{itkKeyboard=itkKeyboard}}))
	= wPtr==itWindow && cPtr==itControl && itkKeyboard
trackingKeyboard _ _ Nothing = False

trackMouse :: OSWindowPtr -> OSWindowPtr -> Maybe InputTrack -> Maybe InputTrack
trackMouse wPtr cPtr (Just it@(InputTrack {itWindow=itWindow,itControl=itControl,itKind=itk}))
	| wPtr /= itWindow || cPtr /= itControl =
		windoweventFatalError "trackMouse" "incorrect window/control parameters"
	| otherwise = Just (it{itKind=itk{itkMouse=True}})
trackMouse wPtr cPtr nothing
	= Just (InputTrack {itWindow=wPtr,itControl=cPtr,itKind=InputTrackKind{itkMouse=True,itkKeyboard=False}})

untrackMouse :: Maybe InputTrack -> Maybe InputTrack
untrackMouse (Just it@(InputTrack {itKind=itk}))
	| itkKeyboard itk = Just (it{itKind=itk{itkMouse=False}})
	| otherwise       = Nothing
untrackMouse Nothing      = Nothing

untrackKeyboard :: Maybe InputTrack -> Maybe InputTrack
untrackKeyboard (Just it@(InputTrack {itKind=itk}))
	| itkMouse itk  = Just (it{itKind=itk{itkKeyboard=False}})
	| otherwise     = Nothing
untrackKeyboard Nothing = Nothing

trackKeyboard :: OSWindowPtr -> OSWindowPtr -> Maybe InputTrack -> Maybe InputTrack
trackKeyboard wPtr cPtr (Just it@(InputTrack {itWindow=itWindow,itControl=itControl,itKind=itk}))
	| wPtr /= itWindow || cPtr /= itControl =
		windoweventFatalError "trackKeyboard" "incorrect window/control parameters"
	| otherwise
		= Just (it{itKind=itk{itkKeyboard=True}})
trackKeyboard wPtr cPtr Nothing =
	Just (InputTrack {itWindow=wPtr,itControl=cPtr,itKind=InputTrackKind{itkMouse=False,itkKeyboard=True}})

