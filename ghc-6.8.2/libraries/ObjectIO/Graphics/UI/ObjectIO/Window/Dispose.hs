-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Window.Dispose
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Window.Dispose disposes all system resources associated with the
-- indicated window if it exists.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Window.Dispose
		( disposeWindow
		, disposeWindowStateHandle
		, disposeWElementHandle
		) where


import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Control.Validate
import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.Process.Scheduler
import Graphics.UI.ObjectIO.Window.Access
import Graphics.UI.ObjectIO.Window.Handle
import Graphics.UI.ObjectIO.Window.Create(bufferDelayedEvents)
import Graphics.UI.ObjectIO.Window.ClipState(disposeClipState)
import Graphics.UI.ObjectIO.OS.Window
import qualified Data.Map as Map


windowdisposeFatalError :: String -> String -> x
windowdisposeFatalError function error
	= dumpFatalError function "WindowDispose" error


{-	disposeWindow disposes all system resources associated with the indicated window if it exists.
	Inactive modal dialogues are not removed.
	If the window belongs to an SDI process, then only the SDI client is removed, not the SDI frame.
	It removes the indicated window from the window device administration.
	Because the window may contain controls that are 'logically' disposed, but not 'physically'
	disposeWindow also applies the init function contained in the IOSt.
-}
disposeWindow :: WID -> ps -> GUI ps ps
disposeWindow wid ps
	= do {
		(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice);
		if   not found
		then return ps
		else let
			windows               = windowSystemStateGetWindowHandles wDevice
			(found,wsH,windows1)  = getWindowHandlesWindow wid windows
		in
		if   not found                -- The window could not be found
		then appIOEnv (ioStSetDevice (WindowSystemState windows1)) >> return ps
		else
		if   getWindowStateHandleClosing wsH -- The window is already in the act of being closed
		then appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1))) >> return ps
		else let                      -- Any modeless window can be disposed
			wKind = getWindowStateHandleWindowKind wsH
			wids  = getWindowStateHandleWIDS wsH
		in do {
		        xDI <- accIOEnv ioStGetDocumentInterface;
			-- Of a SDI process, the SDI client should be closed, not the SDI frame (which is closed by closeProcess)
		        if   xDI==SDI && wKind==IsWindow
		        then dispose wids wsH (incWindowBound windows1) ps
		        else dispose wids wsH windows1 ps
		   }
	  }
	where
		incWindowBound :: WindowHandles ps -> WindowHandles ps
		incWindowBound wHs@(WindowHandles {whsNrWindowBound=whsNrWindowBound})
			= wHs {whsNrWindowBound=incBound whsNrWindowBound}

		dispose :: WIDS -> WindowStateHandle ps -> WindowHandles ps -> ps -> GUI ps ps
		dispose wids@(WIDS {wId=wId}) wsH windows ps
			= do {
				disposeFun    <-  accIOEnv ioStGetInitIO;
				ps1  <- disposeFun ps;
				osdinfo       <-  accIOEnv ioStGetOSDInfo;
				oldInputTrack <-  ioStGetInputTrack;
				(ids,delayinfo,finalLS,inputTrack,ps2) <- disposeWindowStateHandle osdinfo oldInputTrack wsH ps1;
				ioStSetInputTrack inputTrack;
				idtable      <- ioStGetIdTable;
				ioStSetIdTable (foldr Map.delete idtable ids);
				(let windows2 = windows1{whsFinalModalLS=finalLS++(whsFinalModalLS windows1)}
				 in appIOEnv (ioStSetDevice (WindowSystemState windows2)));
				bufferDelayedEvents delayinfo;
				return ps2
			  }
			where
				(_,_,windows1)    = removeWindowHandlesWindow (toWID wId) windows	-- Remove window placeholder


{-	disposeWindowStateHandle disposes all system resources associated with the given WindowStateHandle.
	The return [Id] are the Ids of the other controls.
	When timers are part of windows, also timer ids should be returned.
-}

disposeWindowStateHandle :: OSDInfo -> Maybe InputTrack -> WindowStateHandle ps -> ps -> GUI ps ([Id], [DelayActivationInfo], [FinalModalLS], Maybe InputTrack, ps)
disposeWindowStateHandle osdinfo oldInputTrack (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsState=ls, wlsHandle=wH@(WindowHandle {whKind=whKind, whMode=whMode})}))) ps
	= do		
		(ids,fs) <- liftIO (disposeWElementHandles (wPtr wids) (whItems wH))
		liftIO fs
		(delayInfo,ps) <- liftContextIO (\context -> osDestroyWindow osdinfo (whMode==Modal) (whKind==IsWindow) (wPtr wids) (handleContextOSEvent context)) ps
		let finalModalLS  = if whKind==IsDialog && whMode==Modal then [FinalModalLS wids ls] else []
		let newInputTrack =
			case oldInputTrack of
			  Just (InputTrack{itWindow=itWindow}) ->
			  		if itWindow==(wPtr wids) then Nothing else oldInputTrack
	  		  Nothing -> Nothing
		return ((wId wids):ids, delayInfo, finalModalLS, newInputTrack, ps)

disposeWindowStateHandle _ _ _ _
	= windowdisposeFatalError "disposeWindowStateHandle" "window expected instead of placeholder"
	
	
{-	disposeWElementHandle(s) (recursively) hides all system resources associated with the given
	WElementHandle(s). The argument OSWindowPtr must be the parent window.
	The (IO ()) action must be used to actually dispose the controls.
	It returns all freed receiver and control ids.
	When timers are part of windows, also timer ids should be returned.
-}
disposeWElementHandles :: OSWindowPtr -> [WElementHandle ls ps] -> IO ([Id],IO ())
disposeWElementHandles wptr (itemH:itemHs)
	= do {
		(ids, fs)  <- disposeWElementHandle  wptr itemH;
		(idss,fss) <- disposeWElementHandles wptr itemHs;
		return (ids++idss,fs>>fss)
	  }
disposeWElementHandles _ []
	= return ([],return ())

{-	disposeWElementHandle (recursively) hides all system resources associated with the given WItemHandle.
	The OSWindowPtr argument must identify the parent window.
	The (IO ()) function must be used to actually dispose the controls.
	It returns all freed ids.
	When timers are part of windows, also timer ids should be returned.
-}
disposeWElementHandle :: OSWindowPtr -> WElementHandle ls ps -> IO ([Id],IO ())
disposeWElementHandle wptr (WExtendLSHandle _ itemHs)
	= disposeWElementHandles wptr itemHs
disposeWElementHandle wptr (WChangeLSHandle _ itemHs)
	= disposeWElementHandles wptr itemHs
disposeWElementHandle wptr (WListLSHandle itemHs)
	= disposeWElementHandles wptr itemHs

disposeWElementHandle wPtr itemH@(WItemHandle {wItemKind=IsCheckControl, wItemInfo=wItemInfo, wItemId=wItemId}) =
	let checkInfo = getWItemCheckInfo wItemInfo
	    items = checkItems checkInfo

	    show CheckItemInfo{checkItemPtr=checkItemPtr, checkItemPos=checkItemPos, checkItemSize=checkItemSize} _ =
	    	osSetCheckControlShow wPtr checkItemPtr (posSizeToRect checkItemPos checkItemSize) False

	    destroy CheckItemInfo{checkItemPtr=checkItemPtr} _ =
	    	osDestroyCheckControl checkItemPtr
	in do
		foldrM show () items
		return (maybeToList wItemId,foldrM destroy () items)

disposeWElementHandle wPtr itemH@(WItemHandle {wItemKind=IsCompoundControl}) = do
	(idss,fs) <- disposeWElementHandles wPtr (wItems itemH)
	let ids	= maybeToList (wItemId itemH) ++ idss
	let info = getWItemCompoundInfo (wItemInfo itemH)
	let itemPtr = wItemPtr itemH
	osSetCompoundShow wPtr itemPtr (posSizeToRect (wItemPos itemH) (wItemSize itemH)) False
	return (ids,fs >> osDestroyCompoundControl itemPtr >> disposeClipState (compoundClip (compoundLookInfo info)))

disposeWElementHandle wPtr itemH@(WItemHandle {wItemKind=IsLayoutControl, wItems=wItems}) =
	disposeWElementHandles wPtr wItems

disposeWElementHandle wPtr itemH@(WItemHandle {wItemKind=IsRadioControl, wItemInfo=wItemInfo, wItemId=wItemId}) =
	let radioInfo = getWItemRadioInfo wItemInfo
	    items = radioItems radioInfo

	    show RadioItemInfo{radioItemPtr=radioItemPtr,radioItemPos=radioItemPos,radioItemSize=radioItemSize} _ =
		osSetRadioControlShow wPtr radioItemPtr (posSizeToRect radioItemPos radioItemSize) False

	    destroy RadioItemInfo{radioItemPtr=radioItemPtr} _ =
	     	osDestroyRadioControl radioItemPtr
	in do
		foldrM show () items
		return (maybeToList wItemId,foldrM destroy () items)

disposeWElementHandle wPtr itemH@(WItemHandle {wItemKind=IsReceiverControl, wItemId=wItemId}) =
	return (maybeToList wItemId,return ())

disposeWElementHandle wptr (WItemHandle {wItemKind=wItemKind,wItemId=wItemId,wItemPtr=wItemPtr,wItemPos=wItemPos,wItemSize=wItemSize})
	= hide wptr wItemPtr (posSizeToRect wItemPos wItemSize) False >> return (maybeToList wItemId,dispose wItemPtr)
	where
		(hide,dispose)	= case wItemKind of
					IsPopUpControl	-> (osSetPopUpControlShow,  osDestroyPopUpControl)
					IsListBoxControl-> (osSetListBoxControlShow,osDestroyListBoxControl)
					IsSliderControl	-> (osSetSliderControlShow, osDestroySliderControl)
					IsTextControl   -> (osSetTextControlShow,   osDestroyTextControl)
					IsEditControl   -> (osSetEditControlShow,   osDestroyEditControl)
					IsButtonControl -> (osSetButtonControlShow, osDestroyButtonControl)
					IsCustomButtonControl	-> (osSetCustomButtonControlShow, osDestroyCustomButtonControl)
					IsCustomControl	-> (osSetCustomControlShow, osDestroyCustomControl)
					_               -> windowdisposeFatalError "disposeWItemHandle" ("unmatched ControlKind: "++show wItemKind)
