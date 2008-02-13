-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.Window
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- OS.Window contains OS operations to manage windows and controls.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.Window
		( osControlTitleSpecialChars
                , osMinWindowSize, osMinCompoundSize
                , osGetCompoundContentRect, osGetCompoundHScrollRect, osGetCompoundVScrollRect
                , osGetWindowContentRect, osGetWindowHScrollRect, osGetWindowVScrollRect
                , osGetButtonControlSize, osGetButtonControlHeight
                , osGetTextControlSize,   osGetTextControlHeight
                , osGetEditControlSize,   osGetEditControlHeight
                , osGetPopUpControlSize,  osGetPopUpControlHeight
                , osGetListBoxControlSize, osGetListBoxControlHeight 
		, osGetRadioControlItemSize, osGetRadioControlItemHeight
		, osGetCheckControlItemSize, osGetCheckControlItemHeight
		, osGetSliderControlSize
                , osGetButtonControlMinWidth, osGetTextControlMinWidth, osGetEditControlMinWidth
                , osGetPopUpControlMinWidth, osGetListBoxControlMinWidth, osGetRadioControlItemMinWidth
                , osGetCheckControlItemMinWidth, osGetSliderControlMinWidth
                , osCreateDialog, osCreateWindow, osCreateModalDialog
                , OKorCANCEL(..)
                , osCreateRadioControl, osCreateCheckControl, osCreateEmptyPopUpControl, osAddPopUpControlItem
                , osCreateEmptyListBoxControl, osAddListBoxControlItem
                , osCreateSliderControl, osCreateTextControl, osCreateEditControl, osCreateButtonControl
                , osCreateCustomButtonControl, osCreateCustomControl, osCreateCompoundControl
                , Graphics.UI.ObjectIO.OS.Types.DelayActivationInfo(..)
                , osDestroyWindow
                , osDestroyRadioControl, osDestroyCheckControl, osDestroyPopUpControl, osDestroyListBoxControl
                , osDestroySliderControl, osDestroyTextControl, osDestroyEditControl, osDestroyButtonControl
                , osDestroyCustomButtonControl, osDestroyCustomControl, osDestroyCompoundControl
                , osUpdateRadioControl, osUpdateCheckControl, osUpdatePopUpControl, osUpdateSliderControl
                , osUpdateTextControl, osUpdateEditControl, osUpdateButtonControl, osUpdateCompoundControl
                , osUpdateListBoxControl
                , osClipRadioControl, osClipCheckControl, osClipPopUpControl, osClipSliderControl
                , osClipTextControl, osClipEditControl, osClipButtonControl, osClipListBoxControl
                , osClipCustomButtonControl, osClipCustomControl, osClipCompoundControl
                , osGrabControlPictContext, osGrabWindowPictContext, osReleaseControlPictContext, osReleaseWindowPictContext
                , toOSscrollbarRange, fromOSscrollbarRange, osScrollbarIsVisible, osScrollbarsAreVisible
                , osSetWindowSliderThumb, osSetWindowSliderThumbSize, osSetWindowSlider
                , osInvalidateWindow, osInvalidateWindowRect, osValidateWindowRect, osValidateWindowRgn
                , osDisableWindow, osEnableWindow
                , osActivateWindow, osActivateControl
                , osStackWindow
                , osHideWindow, osShowWindow
                , osSetWindowCursor
                , osGetWindowPos, osGetWindowViewFrameSize, osGetWindowSize
                , osSetWindowPos, osSetWindowViewFrameSize, osSetWindowSize
                , osSetWindowTitle
                , osCreateCaret, osSetCaretPos, osDestroyCaret
                , osInvalidateCompound, osInvalidateCompoundRect, osSetCompoundSliderThumb
                , osSetCompoundSliderThumbSize, osSetCompoundSlider, osSetCompoundSelect
                , osSetCompoundShow, osSetCompoundPos, osSetCompoundSize, osCompoundMovesControls
		, osSetSliderThumb, osSetSliderControlSelect, osSetSliderControlShow
		, osSetSliderControlPos, osSetSliderControlSize
                , osCheckRadioControl, osSetRadioControlSelect, osSetRadioControlShow
                , osSetRadioControlPos, osSetRadioControlSize
                , osCheckCheckControl, osSetCheckControlSelect, osSetCheckControlShow
		, osSetCheckControlShow, osSetCheckControlPos, osSetCheckControlSize
		, osSetPopUpControlSize, osSetPopUpControlPos, osSetPopUpControlShow
		, osSetPopUpControlSelect, osSelectPopUpControlItem
		, osSelectListBoxControlItem, osMarkListBoxControlItem, osSetListBoxControlSelect
		, osSetListBoxControlShow, osSetListBoxControlPos, osSetListBoxControlSize
                , osSetEditControlText, osGetEditControlText, osSetEditControlCursor, osSetEditControlSelect
                , osSetEditControlShow, osSetEditControlPos, osSetEditControlSize
                , osSetTextControlText, osSetTextControlSelect, osSetTextControlShow, osSetTextControlPos, osSetTextControlSize
                , osSetButtonControlText, osSetButtonControlSelect, osSetButtonControlShow, osSetButtonControlPos, osSetButtonControlSize
                , osSetCustomButtonControlSelect, osSetCustomButtonControlShow, osSetCustomButtonControlPos, osSetCustomButtonControlSize
                , osSetCustomControlSelect, osSetCustomControlShow, osSetCustomControlPos, osSetCustomControlSize
                , Graphics.UI.ObjectIO.OS.System.OSWindowMetrics(..), ScrollbarInfo(..)
                ) where



import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Window.Handle
import Graphics.UI.ObjectIO.OS.ClCCall_12
import Graphics.UI.ObjectIO.OS.ClCrossCall_12
import Graphics.UI.ObjectIO.OS.Cutil_12(addr2int, int2addr, newCString, free, nullPtr)
import Graphics.UI.ObjectIO.OS.DocumentInterface
import Graphics.UI.ObjectIO.OS.Event
import Graphics.UI.ObjectIO.OS.Font
import Graphics.UI.ObjectIO.OS.Picture
import Graphics.UI.ObjectIO.OS.Rgn
import Graphics.UI.ObjectIO.OS.System
import Graphics.UI.ObjectIO.OS.Types
import Graphics.UI.ObjectIO.OS.WindowCCall_12
import Graphics.UI.ObjectIO.OS.WindowCrossCall_12
import Foreign.Marshal.Utils(fromBool)
import Data.Word
import Data.Bits

oswindowFatalError :: String -> String -> x
oswindowFatalError function error
	= dumpFatalError function "OSWindow" error


{-	System dependent constants:
-}
osControlTitleSpecialChars :: [Char]
osControlTitleSpecialChars = []		-- Special prefix characters that should be removed


{-	System dependent metrics:
-}

osMinWindowSize :: (Int,Int)
osMinWindowSize = winMinimumWinSize

osMinCompoundSize :: (Int,Int)
osMinCompoundSize = (0,0)		-- PA: (0,0)<--WinMinimumWinSize (Check if this safe)


{-	Window frame dimensions: (PA: were defined as constants in windowvalidate. Moved here.)
-}

osWindowFrameWidth     :: Int
osWindowFrameWidth     = 0

osWindowTitleBarHeight :: Int
osWindowTitleBarHeight = 0


--	Calculating the view frame of window/compound with visibility of scrollbars.

osGetCompoundContentRect :: OSWindowMetrics -> (Bool,Bool) -> Rect -> Rect
osGetCompoundContentRect (OSWindowMetrics {osmHSliderHeight=hSliderHeight,osmVSliderWidth=vSliderWidth}) (visHScroll,visVScroll) itemRect@(Rect{rright=rright,rbottom=rbottom})
	| visHScroll && visVScroll	= itemRect{rright=r,rbottom=b}
	| visHScroll			= itemRect{rbottom=b}
	| visVScroll			= itemRect{rright=r}
	| otherwise			= itemRect
	where
		r	= rright -vSliderWidth
		b	= rbottom-hSliderHeight

osGetCompoundHScrollRect :: OSWindowMetrics -> (Bool,Bool) -> Rect -> Rect
osGetCompoundHScrollRect (OSWindowMetrics {osmHSliderHeight=hSliderHeight,osmVSliderWidth=vSliderWidth}) (visHScroll,visVScroll) itemRect@(Rect{rright=rright,rbottom=rbottom})
	| not visHScroll	= zero
	| otherwise		= itemRect{rtop=b,rright=if visVScroll then r else rright}
	where
		r	= rright -vSliderWidth
		b	= rbottom-hSliderHeight

osGetCompoundVScrollRect :: OSWindowMetrics -> (Bool,Bool) -> Rect -> Rect
osGetCompoundVScrollRect (OSWindowMetrics {osmHSliderHeight=hSliderHeight,osmVSliderWidth=vSliderWidth}) (visHScroll,visVScroll) itemRect@(Rect{rright=rright,rbottom=rbottom})
	| not visVScroll	= zero
	| otherwise		= itemRect{rleft=r,rbottom=if visHScroll then b else rbottom}
	where
		r	= rright -vSliderWidth
		b	= rbottom-hSliderHeight


osGetWindowContentRect :: OSWindowMetrics -> (Bool,Bool) -> Rect -> Rect
osGetWindowContentRect (OSWindowMetrics {osmHSliderHeight=hSliderHeight,osmVSliderWidth=vSliderWidth}) (visHScroll,visVScroll) itemRect@(Rect{rright=rright,rbottom=rbottom})
	| visHScroll && visVScroll	= itemRect{rright=r,rbottom=b}
	| visHScroll			= itemRect{rbottom=b}
	| visVScroll			= itemRect{rright=r}
	| otherwise			= itemRect
	where
		r	= rright -vSliderWidth
		b	= rbottom-hSliderHeight

osGetWindowHScrollRect :: OSWindowMetrics -> (Bool,Bool) -> Rect -> Rect
osGetWindowHScrollRect (OSWindowMetrics {osmHSliderHeight=hSliderHeight,osmVSliderWidth=vSliderWidth}) (visHScroll,visVScroll) itemRect@(Rect{rleft=rleft,rright=rright,rbottom=rbottom})
	| not visHScroll	= zero
	| otherwise		= Rect{rleft=rleft-1,rtop=b,rright=if visVScroll then r+1 else rright+1,rbottom=rbottom+1}
	where
		r	= rright -vSliderWidth  + 1
		b	= rbottom-hSliderHeight + 1

osGetWindowVScrollRect :: OSWindowMetrics -> (Bool,Bool) -> Rect -> Rect
osGetWindowVScrollRect (OSWindowMetrics {osmHSliderHeight=hSliderHeight,osmVSliderWidth=vSliderWidth}) (visHScroll,visVScroll) itemRect@(Rect{rtop=rtop,rright=rright,rbottom=rbottom})
	| not visVScroll	= zero
	| otherwise		= Rect{rleft=r,rtop=rtop-1,rright=rright+1,rbottom=if visHScroll then b+1 else rbottom+1}
	where
		r	= rright -vSliderWidth  + 1
		b	= rbottom-hSliderHeight + 1


{-	Determine the size of controls.
-}
osGetButtonControlSize :: OSWindowMetrics -> String -> IO (Int,Int)
osGetButtonControlSize wMetrics text
	= do {
		[width] <- osGetFontStringWidths Nothing [text] (osmFont wMetrics);
		return (2*osmHeight wMetrics+width,osGetButtonControlHeight wMetrics)
	  }

osGetButtonControlHeight :: OSWindowMetrics -> Int
osGetButtonControlHeight wMetrics = 2*osmHeight wMetrics

osGetTextControlSize :: OSWindowMetrics -> String -> IO (Int,Int)
osGetTextControlSize wMetrics text
	= do {
		widths <- osGetFontStringWidths Nothing [text] (osmFont wMetrics);
		return (head widths+round ((fromIntegral (osmHeight wMetrics))/4.0),osGetTextControlHeight wMetrics)
	  }

osGetTextControlHeight :: OSWindowMetrics -> Int
osGetTextControlHeight (OSWindowMetrics {osmHeight=osmHeight}) = osmHeight+round ((fromIntegral osmHeight)/2)

osGetEditControlSize :: OSWindowMetrics -> Int -> Int -> IO (Int,Int)
osGetEditControlSize wMetrics width nrlines
	= return (width,osGetEditControlHeight wMetrics nrlines)

osGetEditControlHeight :: OSWindowMetrics -> Int -> Int
osGetEditControlHeight (OSWindowMetrics {osmHeight=osmHeight}) nrlines = round ((fromIntegral osmHeight)/2.0)+osmHeight*nrlines

osGetPopUpControlSize :: OSWindowMetrics -> [String] -> IO (Int,Int)
osGetPopUpControlSize wMetrics@(OSWindowMetrics{osmFont=osmFont,osmHeight=osmHeight}) items = do
	widths <- osGetFontStringWidths Nothing items osmFont
	return ((maximum widths)+2*osmHeight+(round ((fromIntegral osmHeight)/2)), osGetPopUpControlHeight wMetrics)

osGetPopUpControlHeight :: OSWindowMetrics -> Int
osGetPopUpControlHeight (OSWindowMetrics{osmHeight=osmHeight}) = osmHeight+(round ((fromIntegral osmHeight)/2))+2

osGetListBoxControlSize :: OSWindowMetrics -> NrLines -> [String] -> IO (Int,Int)
osGetListBoxControlSize wMetrics@(OSWindowMetrics{osmFont=osmFont,osmHeight=osmHeight}) nrLines items = do
	widths <- osGetFontStringWidths Nothing items osmFont
	return ((maximum widths)+2*osmHeight+(round ((fromIntegral osmHeight)/2)), osGetListBoxControlHeight wMetrics nrLines)
	
osGetListBoxControlHeight :: OSWindowMetrics -> NrLines -> Int
osGetListBoxControlHeight (OSWindowMetrics{osmHeight=osmHeight}) nrLines = (osmHeight+(round ((fromIntegral osmHeight)/2))+2)*nrLines

osGetRadioControlItemSize :: OSWindowMetrics -> String -> IO (Int,Int)
osGetRadioControlItemSize wMetrics@(OSWindowMetrics{osmFont=osmFont,osmHeight=osmHeight}) text = do
	widths <- osGetFontStringWidths Nothing [text] osmFont
	return (head widths+2*osmHeight+(round ((fromIntegral osmHeight)/2)),osGetRadioControlItemHeight wMetrics)

osGetRadioControlItemHeight :: OSWindowMetrics -> Int
osGetRadioControlItemHeight (OSWindowMetrics{osmHeight=osmHeight}) = osmHeight+round ((fromIntegral osmHeight)/2)

osGetCheckControlItemSize :: OSWindowMetrics -> String -> IO (Int,Int)
osGetCheckControlItemSize wMetrics@(OSWindowMetrics{osmFont=osmFont,osmHeight=osmHeight}) text = do
	widths <- osGetFontStringWidths Nothing [text] osmFont
	return (head widths+2*osmHeight+(round ((fromIntegral osmHeight)/2)),osGetCheckControlItemHeight wMetrics)

osGetCheckControlItemHeight :: OSWindowMetrics -> Int
osGetCheckControlItemHeight (OSWindowMetrics{osmHeight=osmHeight}) = osmHeight+round ((fromIntegral osmHeight)/2)

osGetSliderControlSize :: OSWindowMetrics -> Bool -> Int -> (Int,Int)
osGetSliderControlSize wMetrics isHorizontal length
	| isHorizontal	= (length,osmHSliderHeight wMetrics)
	| otherwise	= (osmVSliderWidth wMetrics,length)


{-	Determine the minimum width of controls. -}
osGetButtonControlMinWidth :: OSWindowMetrics -> Int
osGetButtonControlMinWidth (OSWindowMetrics {osmHeight=osmHeight}) = 2*osmHeight

osGetTextControlMinWidth :: OSWindowMetrics -> Int
osGetTextControlMinWidth (OSWindowMetrics {osmHeight=osmHeight}) = round ((fromIntegral osmHeight)/4.0)

osGetEditControlMinWidth :: OSWindowMetrics -> Int
osGetEditControlMinWidth _ = 0

osGetPopUpControlMinWidth :: OSWindowMetrics -> Int
osGetPopUpControlMinWidth (OSWindowMetrics {osmHeight=osmHeight}) = 2*osmHeight+(round ((fromIntegral osmHeight)/2))

osGetListBoxControlMinWidth :: OSWindowMetrics -> Int
osGetListBoxControlMinWidth (OSWindowMetrics {osmHeight=osmHeight}) = 2*osmHeight+(round ((fromIntegral osmHeight)/2))

osGetRadioControlItemMinWidth :: OSWindowMetrics -> Int
osGetRadioControlItemMinWidth (OSWindowMetrics {osmHeight=osmHeight}) = 2*osmHeight+(round ((fromIntegral osmHeight)/2))

osGetCheckControlItemMinWidth :: OSWindowMetrics -> Int
osGetCheckControlItemMinWidth (OSWindowMetrics {osmHeight=osmHeight}) = 2*osmHeight+(round ((fromIntegral osmHeight)/2))

osGetSliderControlMinWidth :: OSWindowMetrics -> Int
osGetSliderControlMinWidth _ = 0


{-	Window creation functions. -}
osCreateDialog :: Bool -> Bool -> String -> (Int,Int) -> (Int,Int) -> OSWindowPtr
               -> (s -> OSWindowPtr)
               -> (OSWindowPtr -> s -> IO s)
               -> (OSWindowPtr -> OSWindowPtr -> OSPictContext -> s -> IO s)
               -> OSDInfo -> s
               -> IO ([DelayActivationInfo],OSWindowPtr,s)
osCreateDialog isModal isClosable title pos size behindptr get_focus create_controls update_controls osdinfo control_info
	= do {
		textptr      <- newCString title;
		let createcci = rq4Cci ccRqCREATEDIALOG (addr2int textptr) parentptr behindptr (fromBool isModal)
		in
		do {
			(returncci,(control_info1,delay_info))
			     <- issueCleanRequest (osCreateDialogCallback get_focus create_controls update_controls)
			                          createcci
			                          (control_info,[]);
			free textptr;
			let msg  = ccMsg returncci
			    wptr = if      msg==ccRETURN1 then p1 returncci
			           else if msg==ccWASQUIT then osNoWindowPtr
			           else                        oswindowCreateError 1 "osCreateDialog"
			in  return (reverse delay_info,wptr,control_info1)
		}
	  }
	where
		parentptr = case getOSDInfoOSInfo osdinfo of
		                Nothing   -> 0
		                Just info -> osFrame info

		osCreateDialogCallback :: (s -> OSWindowPtr)
		                       -> (OSWindowPtr -> s -> IO s)
		                       -> (OSWindowPtr -> OSWindowPtr -> OSPictContext -> s -> IO s)
		                       -> CrossCallInfo
		                       -> (s,[DelayActivationInfo])
		                       -> IO (CrossCallInfo,(s,[DelayActivationInfo]))
		osCreateDialogCallback get_focus create_controls update_controls cci s@(control_info,delay_info)
			| msg==ccWmPAINT
				= winFakePaint (p1 cci) >> return (return0Cci, s)
			| msg==ccWmACTIVATE
				= return (return0Cci, (control_info,(DelayActivatedWindow (p1 cci)):delay_info))
			| msg==ccWmDEACTIVATE
				= return (return0Cci, (control_info,(DelayDeactivatedWindow (p1 cci)):delay_info))
			| msg==ccWmINITDIALOG
				= do {
					control_info1 <- create_controls (p1 cci) control_info;
					let defhandle = get_focus control_info1
					    (x,y)     = pos
					    (w,h)     = size
					    r5cci     = return5Cci x y w h defhandle
					in  return (r5cci, (control_info1,delay_info))
				  }
			| msg==ccWmDRAWCONTROL
				= do {
					control_info1 <- update_controls (p1 cci) (p2 cci) (int2addr (p3 cci)) control_info;
					return (return0Cci, (control_info1,delay_info))
				  }
			| msg==ccWmKEYBOARD || msg==ccWmSETFOCUS || msg==ccWmKILLFOCUS
				= return (return0Cci, s)
			| otherwise
				= oswindowFatalError "osCreateDialogCallback" ("unknown message type ("++show msg++")")
			where
				msg = ccMsg cci



osCreateWindow :: OSWindowMetrics -> Bool -> ScrollbarInfo -> ScrollbarInfo ->
				  (Int,Int) -> (Int,Int) -> Bool -> String -> (Int,Int) -> (Int,Int) ->
				  (s->OSWindowPtr) ->
				  (OSWindowPtr -> s -> IO s) ->
				  (OSWindowPtr -> OSWindowPtr -> OSPictContext -> s -> IO s) ->
				  OSDInfo -> OSWindowPtr -> s ->
			   	  IO ([DelayActivationInfo],OSWindowPtr,OSWindowPtr,OSWindowPtr,OSDInfo,s)
osCreateWindow	wMetrics isResizable hInfo@(ScrollbarInfo {cbiHasScroll=hasHScroll}) vInfo@(ScrollbarInfo {cbiHasScroll=hasVScroll}) minSize maxSize
				isClosable title pos size
				get_focus
				create_controls
				update_controls
				osdInfo behindPtr control_info
	| di==MDI =
		do
		   textPtr <- newCString title
		   let styleFlags = ws_SYSMENU
		  		  + ws_OVERLAPPED
		  		  + (if hasHScroll then ws_HSCROLL else 0)
		  		  + (if hasVScroll then ws_VSCROLL else 0)
		  		  + (if isResizable then ws_THICKFRAME else 0)
		       createcci = rq6Cci ccRqCREATEMDIDOCWINDOW (addr2int textPtr) (osClient osinfo) behindPtr pos' size' styleFlags
		   (returncci,(control_info,delay_info)) <-
						issueCleanRequest (osCreateWindowCallback isResizable minSize maxSize create_controls update_controls)
											createcci
											(control_info,[])
		   free textPtr
		   let msg  = ccMsg returncci
		       wPtr = if      msg==ccRETURN1 then p1 returncci
		   	      else if msg==ccWASQUIT then osNoWindowPtr
			      else                        oswindowCreateError 1 "osCreateWindow (MDI)"
		   setScrollRangeAndPos hasHScroll False wMetrics sb_HORZ (cbiState hInfo) (0,0) wPtr
		   setScrollRangeAndPos hasVScroll False wMetrics sb_VERT (cbiState vInfo) (0,0) wPtr
		   return (reverse delay_info,wPtr,osNoWindowPtr,osNoWindowPtr,osdInfo,control_info)

	| di==SDI =
		do
		   let styleFlags = (if hasHScroll then ws_HSCROLL else 0) + (if hasVScroll then ws_VSCROLL else 0)
		       createcci  = rq6Cci ccRqCREATESDIDOCWINDOW 0 (osFrame osinfo) pos' (fst size) (snd size) styleFlags
		   (returncci,(control_info,delay_info)) <-
						issueCleanRequest (osCreateWindowCallback isResizable minSize maxSize create_controls update_controls)
											createcci
											(control_info,[])
		   let msg  = ccMsg returncci
		       clientPtr = if      msg==ccRETURN1 then p1 returncci
		   	       	   else if msg==ccWASQUIT then osNoWindowPtr
			      	   else                        oswindowCreateError 1 "osCreateWindow (SDI)"
		       osdInfo1 = setOSDInfoOSInfo osinfo{osClient=clientPtr} osdInfo
		   setScrollRangeAndPos hasHScroll True wMetrics sb_HORZ (cbiState hInfo) (0,0) clientPtr
		   setScrollRangeAndPos hasVScroll True wMetrics sb_VERT (cbiState vInfo) (0,0) clientPtr
		   osSetWindowTitle (osFrame osinfo) title
		   return (reverse delay_info,clientPtr,osNoWindowPtr,osNoWindowPtr,osdInfo1,control_info)

	| otherwise
		= oswindowFatalError "osCreateWindow" "unexpected OSDInfo (OSNoInfo) argument"
	where
		pos'	= pack pos	-- packed into one 32-bit integer
		size'	= pack size
		di	= getOSDInfoDocumentInterface osdInfo
		osinfo	= fromJust (getOSDInfoOSInfo  osdInfo)

		pack (x,y) = fromIntegral ((shiftL (fromIntegral x) 16 .|. ((fromIntegral y) .&. 0xFFFF)) :: Word32)


osCreateWindowCallback :: Bool -> (Int,Int) -> (Int,Int) ->
				(OSWindowPtr -> s -> IO s) ->
				(OSWindowPtr -> OSWindowPtr -> OSPictContext -> s -> IO s) ->
				CrossCallInfo -> (s,[DelayActivationInfo]) ->
				IO (CrossCallInfo,(s,[DelayActivationInfo]))
osCreateWindowCallback _ _ _ createControls updateControls (CrossCallInfo {ccMsg=msg,p1=hwnd,p2=p2,p3=p3}) s@(control_info,delay_info)
	| msg == ccWmPAINT = do
	      hdc <- winBeginPaint hwnd
	      winEndPaint hwnd hdc
	      return (return0Cci, s)
	| msg == ccWmACTIVATE =
	      return (return0Cci, (control_info,DelayActivatedWindow hwnd:delay_info))
	| msg == ccWmDEACTIVATE =
	      return (return0Cci, (control_info,DelayDeactivatedWindow hwnd:delay_info))
	| msg == ccWmCREATE = do
	      control_info <- createControls hwnd control_info
	      return (return0Cci, (control_info,delay_info))
	| msg == ccWmNEWHTHUMB =
	      return (return0Cci, s)
	| msg == ccWmNEWVTHUMB =
	      return (return0Cci, s)
	| msg == ccWmSIZE =
	      return (return0Cci, s)
	| msg == ccWmDRAWCONTROL = do
	      control_info1 <- updateControls hwnd p2 (int2addr p3) control_info
	      return (return0Cci, (control_info1,delay_info))
	| msg == ccWmKILLFOCUS =
	      return (return0Cci, s)
	| msg == ccWmKEYBOARD =
	      return (return0Cci, s)
	| otherwise =
	      oswindowFatalError "osCreateWindowCallback" ("unknown message type (" ++ show msg ++")")


{-	PA: new function that creates modal dialog and handles events until termination. 
		The Bool result is True iff no error occurred. 
-}
osCreateModalDialog :: Bool -> String -> OSDInfo -> Maybe OSWindowPtr -> (OSEvent -> IO [Int]) -> IO Bool
osCreateModalDialog isClosable title osdinfo currentActiveModal handleOSEvents = do
	textPtr <- newCString title
	let createcci = rq2Cci ccRqCREATEMODALDIALOG (addr2int textPtr) parentPtr
	returncci <- issueCleanRequest2 (osCreateModalDialogCallback handleOSEvents) createcci
	free textPtr
	return (if ccMsg returncci == ccRETURN1 then p1 returncci==0 else
		if ccMsg returncci == ccWASQUIT then True	      else
						     oswindowCreateError 1 "osCreateModalDialog")
	where
		parentPtr =
			case currentActiveModal of
				Nothing -> (case (getOSDInfoOSInfo osdinfo) of
						Just info -> osFrame info
						Nothing   -> 0
					)
				Just _  -> 0
	
		osCreateModalDialogCallback :: (OSEvent -> IO [Int]) -> CrossCallInfo -> IO CrossCallInfo
		osCreateModalDialogCallback handleOSEvents osEvent = do
			replyToOS <- handleOSEvents osEvent
			return (setReplyInOSEvent replyToOS)


-- Window caret

osCreateCaret  = winCreateCaret
osSetCaretPos  = winSetCaretPos
osDestroyCaret = winDestroyCaret

--	Control creation functions.

oswindowCreateError :: Int -> String -> x
oswindowCreateError arity function
	= oswindowFatalError function ("Expected ccRETURN"++show arity++" value")

osIgnoreCallback :: CrossCallInfo -> IO CrossCallInfo
osIgnoreCallback ccinfo
	| ccMsg ccinfo==ccWmPAINT
		= winFakePaint (p1 ccinfo) >> return return0Cci
	| otherwise
		= return return0Cci

osIgnoreCallback' :: CrossCallInfo -> [DelayActivationInfo] -> IO (CrossCallInfo,[DelayActivationInfo])
osIgnoreCallback' ccinfo delayinfo
	| msg==ccWmPAINT
		= winFakePaint (p1 ccinfo) >> return (return0Cci,delayinfo)
	| msg==ccWmACTIVATE
		= return (return0Cci,(DelayActivatedWindow (p1 ccinfo)):delayinfo)
	| msg==ccWmDEACTIVATE
		= return (return0Cci,(DelayDeactivatedWindow (p1 ccinfo)):delayinfo)
	| otherwise
		= return (return0Cci,delayinfo)
	where
		msg = ccMsg ccinfo


{-	OKorCANCEL type is used to tell Windows that a (Custom)ButtonControl is
	the OK, CANCEL, or normal button.
-}
data	OKorCANCEL
	= OK | CANCEL | NORMAL
	deriving (Show)

ok_toInt OK     = isOKBUTTON
ok_toInt CANCEL = isCANCELBUTTON
ok_toInt NORMAL = isNORMALBUTTON

osCreateSliderControl :: OSWindowPtr -> (Int,Int) -> Bool -> Bool -> Bool -> (Int,Int) -> (Int,Int) -> (Int,Int,Int,Int) -> IO OSWindowPtr
osCreateSliderControl parentWindow parentPos show able horizontal (x',y') (w,h) (min,thumb,max,thumbSize)
	= do {
		returncci <- issueCleanRequest2 osIgnoreCallback createcci;
		let msg = ccMsg returncci
	  	    sliderPtr = if      msg == ccRETURN1 then p1 returncci
	  		       	else if msg == ccWASQUIT then osNoWindowPtr
			    	else 		     	 oswindowCreateError 1 "osCreateSliderControl"
		in
		do {
			winSetScrollRange sliderPtr sb_CTL min max False;
			winSetScrollPos   sliderPtr sb_CTL thumb (x+w) (y+h) (if horizontal then h else w);
			winEnableControl  sliderPtr able;
			winShowControl	  sliderPtr show;
			return sliderPtr
		}
	}
	where
		(x,y)	  	= (x'-fst parentPos,y'-snd parentPos)
		nisHorizontal 	= if horizontal then 1 else 0
	    	createcci	= rq6Cci ccRqCREATESCROLLBAR parentWindow x y w h nisHorizontal

osCreateTextControl :: OSWindowPtr -> (Int,Int) -> String -> Bool -> (Int,Int) -> (Int,Int) -> IO OSWindowPtr
osCreateTextControl parentWindow parentPos text show (x,y) (w,h)
	= do {
		returncci <- issueCleanRequest2 osIgnoreCallback createcci;
		let msg     = ccMsg returncci
		    textPtr = if      msg==ccRETURN1  then p1 returncci
		              else if msg==ccWASQUIT then osNoWindowPtr
		              else                        oswindowCreateError 1 "osCreateTextControl"
		in
		do {
			winSetWindowTitle textPtr text;
			winShowControl    textPtr show;
			return textPtr
		}
	  }
	where
		createcci = rq5Cci ccRqCREATESTATICTXT parentWindow (x-fst parentPos) (y-snd parentPos) w h

osCreateEditControl :: OSWindowPtr -> (Int,Int) -> String -> Bool -> Bool -> Bool -> (Int,Int) -> (Int,Int) -> IO OSWindowPtr
osCreateEditControl parentWindow parentPos text visible able isKeySensitive (x,y) (w,h)
	= do {
		wMetrics           <- osDefaultWindowMetrics;
		let nrLines         = round ((fromIntegral (h-osmHeight wMetrics))/(fromIntegral (osmHeight wMetrics)))
		    isMultiLine     = nrLines>1
		    editflags       = (if isMultiLine then editISMULTILINE else 0) + (if isKeySensitive then editISKEYSENSITIVE else 0)
		    createcci       = rq6Cci ccRqCREATEEDITTXT parentWindow (x-fst parentPos) (y-snd parentPos) w h editflags
		in
		do {
			returncci  <- issueCleanRequest2 osIgnoreCallback createcci;
			let msg     = ccMsg returncci
			    editPtr = if      msg==ccRETURN1 then p1 returncci
			              else if msg==ccWASQUIT then osNoWindowPtr
			              else                        oswindowCreateError 1 "osCreateEditControl"
			in
			do {
				winSetWindowTitle editPtr text;
				winEnableControl  editPtr able;
				winShowControl    editPtr visible;
				return editPtr
			}
		}
	  }


osCreateButtonControl :: OSWindowPtr -> (Int,Int) -> String -> Bool -> Bool -> (Int,Int) -> (Int,Int) -> OKorCANCEL -> IO OSWindowPtr
osCreateButtonControl parentWindow parentPos title show able (x,y) (w,h) okOrCancel
	= do {
		returncci    <- issueCleanRequest2 osIgnoreCallback createcci;
		let msg       = ccMsg returncci
		    buttonPtr = if      msg==ccRETURN1 then p1 returncci
		                else if msg==ccWASQUIT then osNoWindowPtr
		                else                   oswindowCreateError 1 "osCreateButtonControl"
		in
		do {
			winSetWindowTitle buttonPtr title;
			winEnableControl  buttonPtr able;
			winShowControl    buttonPtr show;
			return buttonPtr
		}
	  }
	where
		createcci = rq6Cci ccRqCREATEBUTTON parentWindow (x-fst parentPos) (y-snd parentPos) w h (ok_toInt okOrCancel)


osCreateCustomButtonControl :: OSWindowPtr -> (Int,Int) -> Bool -> Bool -> (Int,Int) -> (Int,Int) -> OKorCANCEL -> IO OSWindowPtr
osCreateCustomButtonControl parentWindow parentPos show able (x',y') (w,h) okOrCancel = do
	let (x,y) = (x'-fst parentPos,y'-snd parentPos)
	let createcci = rq6Cci ccRqCREATEICONBUT parentWindow x y w h (ok_toInt okOrCancel)
	returncci <- issueCleanRequest2 osIgnoreCallback createcci
	let buttonPtr =
		if      ccMsg returncci == ccRETURN1 then p1 returncci
		else if ccMsg returncci == ccWASQUIT then osNoWindowPtr
		else                                 oswindowCreateError 1 "OScreateCustomButtonControl"
	winEnableControl buttonPtr able
	winShowControl	buttonPtr show
	return buttonPtr

osCreateCustomControl :: OSWindowPtr -> (Int,Int) -> Bool -> Bool -> (Int,Int) -> (Int,Int) -> IO OSWindowPtr
osCreateCustomControl parentWindow parentPos show able (x',y') (w,h) = do
	let (x,y) = (x'-fst parentPos,y'-snd parentPos)
	let createcci = rq5Cci ccRqCREATECUSTOM parentWindow x y w h
	returncci <- issueCleanRequest2 osIgnoreCallback createcci
	let customPtr =
		if      ccMsg returncci == ccRETURN1 then p1 returncci
		else if ccMsg returncci == ccWASQUIT then osNoWindowPtr
		else    			     oswindowCreateError 1 "OScreateCustomControl"
	winEnableControl customPtr able
	winShowControl	customPtr show
	return customPtr

osCreateCompoundControl ::  OSWindowMetrics -> OSWindowPtr -> (Int,Int) -> Bool -> Bool -> Bool -> (Int,Int) -> (Int,Int)
							-> ScrollbarInfo ->  ScrollbarInfo -> IO (OSWindowPtr,OSWindowPtr,OSWindowPtr)
osCreateCompoundControl wMetrics parentWindow parentPos show able isTransparent (x,y) (w,h)
						hInfo@(ScrollbarInfo {cbiHasScroll=hasHScroll})
						vInfo@(ScrollbarInfo {cbiHasScroll=hasVScroll}) = do
	let (x1,y1) = (x-fst parentPos,y-snd parentPos)
	    scrollFlags	= (if hasHScroll then ws_HSCROLL else 0) .|. (if hasVScroll then ws_VSCROLL else 0)
	    createcci = rq6Cci ccRqCREATECOMPOUND parentWindow ((x1 `shiftL` 16)+((y1 `shiftL` 16) `shiftR` 16)) w h scrollFlags (fromBool isTransparent)
	returncci <- issueCleanRequest2 osIgnoreCallback createcci
	let msg = ccMsg returncci
	    compoundPtr	= if      msg == ccRETURN1 then p1 returncci
			  else if msg == ccWASQUIT then osNoWindowPtr
			  else 			   oswindowCreateError 1 "osCreateCompoundControl"
	setScrollRangeAndPos hasHScroll False wMetrics sb_HORZ (cbiState hInfo) (0,0) compoundPtr
	setScrollRangeAndPos hasVScroll False wMetrics sb_VERT (cbiState vInfo) (0,0) compoundPtr
	winSetSelectStateWindow	compoundPtr (hasHScroll,hasVScroll) able False
	winShowControl compoundPtr show
	return (compoundPtr,osNoWindowPtr,osNoWindowPtr)


osCreateRadioControl :: OSWindowPtr -> (Int,Int) -> String -> Bool -> Bool -> (Int,Int) -> (Int,Int) -> Bool -> Bool -> IO OSWindowPtr
osCreateRadioControl parentWindow parentPos title show able (x,y) (w,h) selected isfirst
	= do {
		returncci <- issueCleanRequest2 osIgnoreCallback createcci;
		let msg = ccMsg returncci
		    radioPtr = if      msg==ccRETURN1 then p1 returncci
		     	       else if msg==ccWASQUIT then osNoWindowPtr
		     	       else                   oswindowCreateError 1 "osCreateRadioControl"
		in
		do {
			winSetWindowTitle radioPtr title;
			winCheckControl   radioPtr selected;
			winEnableControl  radioPtr able;
			winShowControl	radioPtr show;
			return radioPtr
		}
	}
	where
		nfirst = if isfirst then 1 else 0
		createcci = rq6Cci ccRqCREATERADIOBUT parentWindow (x-fst parentPos) (y-snd parentPos) w h nfirst


osCreateCheckControl :: OSWindowPtr -> (Int,Int) -> String -> Bool -> Bool -> (Int,Int) -> (Int,Int) -> Bool -> Bool -> IO OSWindowPtr
osCreateCheckControl parentWindow parentPos title show able (x,y) (w,h) selected isfirst
	= do {
		returncci    <- issueCleanRequest2 osIgnoreCallback createcci;
		let msg       = ccMsg returncci
		    checkPtr = if      msg==ccRETURN1 then p1 returncci
		               else if msg==ccWASQUIT then osNoWindowPtr
		               else                   oswindowCreateError 1 "osCreateCheckControl"
		in
		do {
			winSetWindowTitle checkPtr title;
			winCheckControl   checkPtr  selected;
			winEnableControl  checkPtr able;
			winShowControl    checkPtr show;
			return checkPtr
		}
	  }
	where
		nfirst = if isfirst then 1 else 0
		createcci = rq6Cci ccRqCREATECHECKBOX parentWindow (x-fst parentPos) (y-snd parentPos) w h nfirst


maxComboboxWidth	= 65535		-- System maximum for width  of combo box
maxComboboxHeight	= 65535		-- System maximum for height of combo box
maxComboElementsVisible	= 15		-- If there are <=MaxComboElementsVisible then show all elements
maxComboElementsScroll	= 12		-- otherwise, show MaxComboElementsScroll elements

osCreateEmptyPopUpControl :: OSWindowPtr -> (Int,Int) -> Bool -> Bool -> (Int,Int) -> (Int,Int) -> Int -> Bool -> IO (OSWindowPtr,OSWindowPtr)
osCreateEmptyPopUpControl parentWindow parentPos show able (x,y) (w1,h1) nrItems isEditable
	= do {
		screenRect <- osScreenRect;
		wMetrics <- osDefaultWindowMetrics;
	  	let screenSize	= rectSize screenRect
		    height	= osmHeight wMetrics
		    okNrItems	= if (nrItems<=maxComboElementsVisible) then nrItems else maxComboElementsScroll
		    overall_h	= min (h screenSize) (min maxComboboxHeight (h1 + (okNrItems+1)*(height+2)))
		    overall_w	= min (w screenSize) (min maxComboboxWidth w1)
		    nisEditable = if isEditable then 1 else 0
		    createcci	= rq6Cci ccRqCREATEPOPUP parentWindow (x-fst parentPos) (y-snd parentPos) overall_w overall_h nisEditable
	  	in
	 	do {
	 		returncci <- issueCleanRequest2 osIgnoreCallback createcci;
	  		let msg = ccMsg returncci
	  		    (popUpPtr,editPtr) = if      msg == ccRETURN2 then (p1 returncci, p2 returncci)
						 else if msg == ccWASQUIT then (osNoWindowPtr,osNoWindowPtr)
						 else			  oswindowCreateError 2 "OScreateEmptyPopUpControl"
			in
			do {
				winEnableControl popUpPtr able;
				winShowControl   popUpPtr show;
				return (popUpPtr,editPtr)
			}
		}
	}

osAddPopUpControlItem :: OSWindowPtr -> String -> Bool -> IO Int
osAddPopUpControlItem parentPopUp title selected
	= do {
		textPtr <- newCString title;
	  	let addcci = rq3Cci ccRqADDTOPOPUP parentPopUp (addr2int textPtr) (fromBool selected)
	  	in
	  	do {
	  		returncci <- issueCleanRequest2 osIgnoreCallback addcci;
			free textPtr;
			(let msg = ccMsg returncci
			     index = if      msg == ccRETURN1 then p1 returncci
				     else if msg == ccWASQUIT then 0
				     else		      oswindowCreateError 1 "osCreatePopUpControlItem"
			 in return index)
		}
	}


osCreateEmptyListBoxControl :: OSWindowPtr -> (Int,Int) -> Bool -> Bool -> (Int,Int) -> (Int,Int) -> Bool -> IO OSWindowPtr
osCreateEmptyListBoxControl parentWindow parentPos show able (x,y) (w,h) multi
	= do {
		wMetrics <- osDefaultWindowMetrics;
	  	let createcci	= rq6Cci ccRqCREATELISTBOX parentWindow (x-fst parentPos) (y-snd parentPos) w h (fromBool multi)
	  	in
	 	do {
	 		returncci <- issueCleanRequest2 osIgnoreCallback createcci;
	  		let msg = ccMsg returncci
	  		    listBoxPtr = if      msg == ccRETURN1 then p1 returncci
				         else if msg == ccWASQUIT then osNoWindowPtr
				         else oswindowCreateError 2 "osCreateEmptyListBoxControl"
			in
			do {
				winEnableControl listBoxPtr able;
				winShowControl   listBoxPtr show;
				return listBoxPtr
			}
		}
	}

osAddListBoxControlItem :: OSWindowPtr -> String -> Bool -> IO Int
osAddListBoxControlItem parentListBox title selected
	= do {
		textPtr <- newCString title;
	  	let addcci = rq3Cci ccRqADDTOLISTBOX parentListBox (addr2int textPtr) (fromBool selected)
	  	in
	  	do {
	  		returncci <- issueCleanRequest2 osIgnoreCallback addcci;
			free textPtr;
			(let msg = ccMsg returncci
			     index = if      msg == ccRETURN1 then p1 returncci
				     else if msg == ccWASQUIT then 0
				     else		      oswindowCreateError 1 "OScreatePopUpControlItem"
			 in return index)
		}
	}


data ScrollbarInfo
   = ScrollbarInfo
   	{ cbiHasScroll	:: !Bool		-- The scrollbar exists
	, cbiPos	:: (Int,Int)		-- Its position within the parent
	, cbiSize	:: (Int,Int)		-- Its size within the parent
	, cbiState	:: (Int,Int,Int,Int)	-- Its (min,thumb,max,thumbsize) settings
	}


setScrollRangeAndPos :: Bool -> Bool -> OSWindowMetrics -> Int -> (Int,Int,Int,Int) -> (Int,Int) -> OSWindowPtr -> IO ()
setScrollRangeAndPos hasScroll redraw wMetrics iBar state maxcoords wPtr =
	if hasScroll then do
	    winSetScrollRange     wPtr iBar min max   False
	    winSetScrollPos       wPtr iBar thumb     0 0 0
	    (if redraw then winSetScrollThumbSize wPtr iBar thumbsize maxx maxy extent
	     else winSetScrollThumbSize wPtr iBar thumbsize 0 0 0)
	else return ()
	where
	    (min,thumb,max,thumbsize) = state
	    (maxx,maxy)		      = maxcoords
	    extent		      = (if iBar==sb_HORZ then osmHSliderHeight else osmVSliderWidth) wMetrics



{-	Window destruction operations.
	osDestroyWindow checks the process document interface and applies the appropriate destruction operation.
-}
osDestroyWindow :: OSDInfo -> Bool -> Bool -> OSWindowPtr -> (SchedulerEvent -> IO [Int]) -> IO [DelayActivationInfo]
osDestroyWindow osdInfo isModal isWindow wPtr handleOSEvent
	| di==MDI
		= let osinfo     = fromJust (getOSDInfoOSInfo  osdInfo)
		      destroycci = if   isWindow   then rq3Cci ccRqDESTROYMDIDOCWINDOW (osFrame osinfo) (osClient osinfo) wPtr
		                   else if isModal then rq1Cci ccRqDESTROYMODALDIALOG wPtr
		                   else                 rq1Cci ccRqDESTROYWINDOW wPtr
		  in do {
		  	(_,delayInfo) <- issueCleanRequest (osDelayCallback handleOSEvent) destroycci [];
		  	return (reverse delayInfo)
		     }
	| di==SDI
		= let destroycci = if isModal then rq1Cci ccRqDESTROYMODALDIALOG wPtr
		                              else rq1Cci ccRqDESTROYWINDOW wPtr
		  in do {
		  	(_,delayInfo) <- issueCleanRequest (osDelayCallback handleOSEvent) destroycci [];
		  	return (reverse delayInfo)
		     }
	-- It's a NDI process
	| isWindow		{- This condition should never occur (NDI processes have only dialogues). -}
		= oswindowFatalError "osDestroyWindow" "trying to destroy window of NDI process"
	| otherwise
		= let destroycci = if isModal then rq1Cci ccRqDESTROYMODALDIALOG wPtr
		                              else rq1Cci ccRqDESTROYWINDOW wPtr
		  in do {
		  	(_,delayInfo) <- issueCleanRequest (osDelayCallback handleOSEvent) destroycci [];
		  	return (reverse delayInfo)
		     }
	where
		di     = getOSDInfoDocumentInterface osdInfo

osDelayCallback :: (SchedulerEvent -> IO [Int]) -> CrossCallInfo -> [DelayActivationInfo] -> IO (CrossCallInfo,[DelayActivationInfo])
osDelayCallback handleOSEvent osEvent delayinfo
	| toBeHandled = do
		replyToOS <- handleOSEvent (ScheduleOSEvent osEvent [])
		return (setReplyInOSEvent replyToOS,delayinfo)
	| msg==ccWmACTIVATE =
		return (return0Cci,(DelayActivatedWindow (p1 osEvent)):delayinfo)
	| msg==ccWmDEACTIVATE =
		return (return0Cci,(DelayDeactivatedWindow (p1 osEvent)):delayinfo)
	| toBeSkipped =
		return (return0Cci,delayinfo)
	| otherwise =
		oswindowFatalError "osDelayCallback" ("unexpected delay message "++show msg)
	where
		msg         = ccMsg osEvent
		toBeHandled = msg `elem` [ccWmPAINT,ccWmDRAWCONTROL,ccWmKEYBOARD,ccWmKILLFOCUS,ccWmMOUSE,ccWmSETFOCUS]
		toBeSkipped = msg `elem` [ccWmCLOSE,ccWmIDLETIMER,ccWmSIZE]


{-	Control destruction operations.
-}
destroycontrol :: OSWindowPtr -> IO ()
destroycontrol wPtr
	= issueCleanRequest2 osDestroyControlCallback (rq1Cci ccRqDESTROYWINDOW wPtr) >> return ()
	where
		osDestroyControlCallback :: CrossCallInfo -> IO CrossCallInfo
		osDestroyControlCallback info@(CrossCallInfo {ccMsg=ccMsg})
			| ccMsg==ccWmPAINT
				= winFakePaint (p1 info) >> return return0Cci
			| expected
				= return return0Cci
			| otherwise
				= oswindowFatalError "osDestroyControlCallback" ("unexpected message "++show ccMsg)
			where
				expected = ccMsg `elem`   [ccWmACTIVATE,ccWmBUTTONCLICKED,ccWmITEMSELECT,ccWmCOMMAND,ccWmDEACTIVATE
				                          ,ccWmDRAWCONTROL,ccWmIDLETIMER,ccWmKEYBOARD,ccWmKILLFOCUS,ccWmSETFOCUS
				                          ]
osDestroyRadioControl :: OSWindowPtr -> IO ()
osDestroyRadioControl wPtr = destroycontrol wPtr

osDestroyCheckControl :: OSWindowPtr -> IO ()
osDestroyCheckControl wPtr = destroycontrol wPtr

osDestroyPopUpControl :: OSWindowPtr -> IO ()
osDestroyPopUpControl wPtr = destroycontrol wPtr

osDestroyListBoxControl :: OSWindowPtr -> IO ()
osDestroyListBoxControl wPtr = destroycontrol wPtr

osDestroySliderControl :: OSWindowPtr -> IO ()
osDestroySliderControl wPtr = destroycontrol wPtr

osDestroyTextControl :: OSWindowPtr -> IO ()
osDestroyTextControl wPtr = destroycontrol wPtr

osDestroyEditControl :: OSWindowPtr -> IO ()
osDestroyEditControl wPtr = destroycontrol wPtr

osDestroyButtonControl :: OSWindowPtr -> IO ()
osDestroyButtonControl wPtr = destroycontrol wPtr

osDestroyCustomButtonControl :: OSWindowPtr -> IO ()
osDestroyCustomButtonControl wPtr = destroycontrol wPtr

osDestroyCustomControl :: OSWindowPtr -> IO ()
osDestroyCustomControl wPtr = destroycontrol wPtr

osDestroyCompoundControl :: OSWindowPtr -> IO ()
osDestroyCompoundControl wPtr = destroycontrol wPtr

--	Control update operations.

osUpdateRadioControl :: Rect -> OSWindowPtr -> OSWindowPtr -> IO ()
osUpdateRadioControl area parentWindow theControl = updateControl theControl area

osUpdateCheckControl :: Rect -> OSWindowPtr -> OSWindowPtr -> IO ()
osUpdateCheckControl area parentWindow theControl = updateControl theControl area

osUpdatePopUpControl :: Rect -> OSWindowPtr -> OSWindowPtr -> IO ()
osUpdatePopUpControl area parentWindow theControl = updateControl theControl area

osUpdateListBoxControl :: Rect -> OSWindowPtr -> OSWindowPtr -> IO ()
osUpdateListBoxControl area parentWindow theControl = updateControl theControl area

osUpdateSliderControl :: Rect -> OSWindowPtr -> OSWindowPtr -> IO ()
osUpdateSliderControl area parentWindow theControl = updateControl theControl area

osUpdateTextControl :: Rect -> OSWindowPtr -> OSWindowPtr -> IO ()
osUpdateTextControl area parentWindow theControl = updateControl theControl area

osUpdateEditControl :: Rect -> OSWindowPtr -> OSWindowPtr -> IO ()
osUpdateEditControl area parentWindow theControl = updateControl theControl area

osUpdateButtonControl :: Rect -> OSWindowPtr -> OSWindowPtr -> IO ()
osUpdateButtonControl area parentWindow theControl = updateControl theControl area

osUpdateCompoundControl :: Rect -> OSWindowPtr -> OSWindowPtr -> IO ()
osUpdateCompoundControl area parentWindow theControl = updateControl theControl area

updateControl :: OSWindowPtr -> Rect -> IO ()
updateControl theControl rect = winUpdateWindowRect theControl (toTuple4 rect)


{-	Control clipping operations.
-}
osClipRectRgn :: (Int,Int) -> Rect -> (Int,Int) -> (Int,Int) -> IO OSRgnHandle
osClipRectRgn parent_pos@(parent_x,parent_y) rect (x,y) (w,h)
	= osNewRectRgn (intersectRects area item)
	where
		area = subVector (fromTuple parent_pos) rect
		x'   = x-parent_x
		y'   = y-parent_y
		item = Rect {rleft=x',rtop=y',rright=x'+w,rbottom=y'+h}

osClipRadioControl :: OSWindowPtr -> (Int,Int) -> Rect -> (Int,Int) -> (Int,Int) -> IO OSRgnHandle
osClipRadioControl _ parentPos area itemPos itemSize = osClipRectRgn parentPos area itemPos itemSize

osClipCheckControl :: OSWindowPtr -> (Int,Int) -> Rect -> (Int,Int) -> (Int,Int) -> IO OSRgnHandle
osClipCheckControl _ parentPos area itemPos itemSize = osClipRectRgn parentPos area itemPos itemSize

osClipPopUpControl :: OSWindowPtr -> (Int,Int) -> Rect -> (Int,Int) -> (Int,Int) -> IO OSRgnHandle
osClipPopUpControl _ parentPos area itemPos itemSize = osClipRectRgn parentPos area itemPos itemSize

osClipListBoxControl :: OSWindowPtr -> (Int,Int) -> Rect -> (Int,Int) -> (Int,Int) -> IO OSRgnHandle
osClipListBoxControl _ parentPos area itemPos itemSize = osClipRectRgn parentPos area itemPos itemSize

osClipSliderControl :: OSWindowPtr -> (Int,Int) -> Rect -> (Int,Int) -> (Int,Int) -> IO OSRgnHandle
osClipSliderControl _ parentPos area itemPos itemSize = osClipRectRgn parentPos area itemPos itemSize

osClipTextControl :: OSWindowPtr -> (Int,Int) -> Rect -> (Int,Int) -> (Int,Int) -> IO OSRgnHandle
osClipTextControl _ parentPos area itemPos itemSize = osClipRectRgn parentPos area itemPos itemSize

osClipEditControl :: OSWindowPtr -> (Int,Int) -> Rect -> (Int,Int) -> (Int,Int) -> IO OSRgnHandle
osClipEditControl _ parentPos area itemPos itemSize = osClipRectRgn parentPos area itemPos itemSize

osClipButtonControl :: OSWindowPtr -> (Int,Int) -> Rect -> (Int,Int) -> (Int,Int) -> IO OSRgnHandle
osClipButtonControl _ parentPos area itemPos itemSize = osClipRectRgn parentPos area itemPos itemSize

osClipCustomButtonControl :: OSWindowPtr -> (Int,Int) -> Rect -> (Int,Int) -> (Int,Int) -> IO OSRgnHandle
osClipCustomButtonControl _ parentPos area itemPos itemSize = osClipRectRgn parentPos area itemPos itemSize

osClipCustomControl :: OSWindowPtr -> (Int,Int) -> Rect -> (Int,Int) -> (Int,Int) -> IO OSRgnHandle
osClipCustomControl _ parentPos area itemPos itemSize = osClipRectRgn parentPos area itemPos itemSize

osClipCompoundControl :: OSWindowPtr -> (Int,Int) -> Rect -> (Int,Int) -> (Int,Int) -> IO OSRgnHandle
osClipCompoundControl _ parentPos area itemPos itemSize = osClipRectRgn parentPos area itemPos itemSize


{-	Window graphics context access operations.
-}
osGrabWindowPictContext :: OSWindowPtr -> IO OSPictContext
osGrabWindowPictContext wPtr = winGetDC wPtr

osGrabControlPictContext :: OSWindowPtr -> OSWindowPtr -> IO OSPictContext
osGrabControlPictContext wPtr cPtr = winGetDC cPtr

osReleaseWindowPictContext :: OSWindowPtr -> OSPictContext -> IO ()
osReleaseWindowPictContext wPtr hdc = winReleaseDC wPtr hdc

osReleaseControlPictContext :: OSWindowPtr -> OSPictContext -> IO ()
osReleaseControlPictContext cPtr hdc = winReleaseDC cPtr hdc


{-	Window access operations.
-}
toOSscrollbarRange :: (Int,Int,Int) -> Int -> (Int,Int,Int,Int)
toOSscrollbarRange (domainMin,viewMin,domainMax) viewSize
	= (osRangeMin,osThumb,osRangeMax,osThumbSize+1)
	where
		(osRangeMin,osRangeMax) = toOSRange (domainMin,domainMax)
		range                   =  domainMax- domainMin
		osRange                 = osRangeMax-osRangeMin
		osThumb                 = inRange osRangeMin osRange (viewMin-domainMin) range
		osThumbSize             = if viewSize>=range then osRange else round (((fromIntegral viewSize)/(fromIntegral range))*(fromIntegral osRange))

fromOSscrollbarRange :: (Int,Int) -> Int -> Int
fromOSscrollbarRange (domainMin,domainMax) osThumb
	= inRange domainMin range (osThumb-osRangeMin) osRange
	where
		(osRangeMin,osRangeMax) = toOSRange (domainMin,domainMax)
		range                   =  domainMax- domainMin
		osRange                 = osRangeMax-osRangeMin

osScrollbarIsVisible :: (Int,Int) -> Int -> Bool
osScrollbarIsVisible (domainMin,domainMax) viewSize
	= viewSize<domainMax-domainMin

osScrollbarsAreVisible :: OSWindowMetrics -> Rect -> (Int,Int) -> (Bool,Bool) -> (Bool,Bool)
osScrollbarsAreVisible (OSWindowMetrics {osmHSliderHeight=osmHSliderHeight,osmVSliderWidth=osmVSliderWidth})
                       (Rect {rleft=xMin,rtop=yMin,rright=xMax,rbottom=yMax})
                       (width,height)
                       (hasHScroll,hasVScroll)
	= visScrollbars (False,False) (hasHScroll && (osScrollbarIsVisible hRange width),hasVScroll && (osScrollbarIsVisible vRange height))
	where
		hRange = (xMin,xMax)
		vRange = (yMin,yMax)

		visScrollbars :: (Bool,Bool) -> (Bool,Bool) -> (Bool,Bool)
		visScrollbars (showH1,showV1) (showH2,showV2)
			| showH1==showH2 && showV1==showV2
				= (showH1,showV1)
			| otherwise
				= visScrollbars (showH2,showV2) (showH,showV)
			where
				showH = if showV2 then hasHScroll && osScrollbarIsVisible hRange (width -osmVSliderWidth ) else showH2
				showV = if showH2 then hasVScroll && osScrollbarIsVisible vRange (height-osmHSliderHeight) else showV2

toOSRange :: (Int,Int) -> (Int,Int)
toOSRange (min,max)
	= (osSliderMin,if range<=osSliderRange then osSliderMin+range else osSliderMax)
	where
		range = max-min

inRange :: Int -> Int -> Int -> Int -> Int
inRange destMin destRange sourceValue sourceRange
	= destMin + round (((fromIntegral sourceValue) / (fromIntegral sourceRange)) * (fromIntegral destRange))

osSliderMin   = 0		-- 0
osSliderMax   = 32767
osSliderRange = 32767		-- osSliderMax-osSliderMin


osSetWindowSliderThumb :: OSWindowMetrics -> OSWindowPtr -> Bool -> Int -> (Int,Int) -> Bool -> IO ()
osSetWindowSliderThumb wMetrics theWindow isHorizontal thumb (maxx,maxy) redraw =
    winSetScrollPos theWindow (if isHorizontal then sb_HORZ else sb_VERT) thumb maxx maxy extent
    where
       extent = (if isHorizontal then osmHSliderHeight else osmVSliderWidth) wMetrics

osSetWindowSliderThumbSize :: OSWindowMetrics -> OSWindowPtr -> Bool -> Int -> (Int,Int) -> Bool -> IO ()
osSetWindowSliderThumbSize wMetrics theWindow isHorizontal size (maxx,maxy) redraw =
    winSetScrollThumbSize theWindow (if isHorizontal then sb_HORZ else sb_VERT) size maxx maxy extent
    where
	extent	= (if isHorizontal then osmHSliderHeight else osmVSliderWidth) wMetrics

osSetWindowSlider :: OSWindowMetrics -> OSWindowPtr -> Bool -> (Int,Int,Int,Int) -> (Int,Int) -> IO ()
osSetWindowSlider wMetrics theWindow isHorizontal state maxcoords =
	setScrollRangeAndPos True True wMetrics (if isHorizontal then sb_HORZ else sb_VERT) state maxcoords theWindow
	
{-	Window access operations. -}

osInvalidateWindow :: OSWindowPtr -> IO ()
osInvalidateWindow theWindow
	= winInvalidateWindow theWindow

osInvalidateWindowRect :: OSWindowPtr -> Rect -> IO ()
osInvalidateWindowRect theWindow rect
	= winInvalidateRect theWindow (toTuple4 rect)

osValidateWindowRect :: OSWindowPtr -> Rect -> IO ()
osValidateWindowRect theWindow rect
	= winValidateRect theWindow (toTuple4 rect)

osValidateWindowRgn :: OSWindowPtr -> OSRgnHandle -> IO ()
osValidateWindowRgn theWindow rgn
	= winValidateRgn theWindow rgn

osDisableWindow :: OSWindowPtr -> (Bool,Bool) -> Bool -> IO ()
osDisableWindow theWindow scrollInfo modalContext
	= winSetSelectStateWindow theWindow scrollInfo False modalContext

osEnableWindow :: OSWindowPtr -> (Bool,Bool) -> Bool -> IO ()
osEnableWindow theWindow scrollInfo modalContext
	= winSetSelectStateWindow theWindow scrollInfo True modalContext

osActivateWindow :: OSDInfo -> OSWindowPtr -> (OSEvent -> IO ()) -> IO [DelayActivationInfo]
osActivateWindow osdInfo thisWindow handleOSEvent
	= do {
		(_,delayinfo) <- issueCleanRequest (osCallback handleOSEvent)
		                                            (rq3Cci ccRqACTIVATEWINDOW (fromBool isMDI) clientPtr thisWindow)
		                                            [];
		return (reverse delayinfo)
	  }
	where
		isMDI     = getOSDInfoDocumentInterface osdInfo==MDI
		clientPtr = case getOSDInfoOSInfo osdInfo of
		                Just osinfo -> osClient osinfo
		                nothing     -> oswindowFatalError "osActivateWindow" "illegal DocumentInterface context"

	{-	osCallback delays activate and deactivate events.
		All other events are passed to the callback function.
	-}	osCallback :: (OSEvent -> IO ()) -> CrossCallInfo -> [DelayActivationInfo] -> IO (CrossCallInfo,[DelayActivationInfo])
		osCallback handleOSEvent osEvent@(CrossCallInfo {ccMsg=ccMsg,p1=p1,p2=p2}) delayinfo
			| isJust maybeEvent
				= return (return0Cci,(fromJust maybeEvent):delayinfo)
			| otherwise
				= do { handleOSEvent osEvent;
				       return (return0Cci,delayinfo)
				  }
			where
				maybeEvent   = lookup ccMsg [(ccWmACTIVATE , DelayActivatedWindow    p1)
				                            ,(ccWmDEACTIVATE,DelayDeactivatedWindow  p1)
				                            ,(ccWmKILLFOCUS, DelayDeactivatedControl p1 p2)
				                            ,(ccWmSETFOCUS,  DelayActivatedControl   p1 p2)
				                            ]


osActivateControl :: OSWindowPtr -> OSWindowPtr -> IO [DelayActivationInfo]
osActivateControl parentWindow controlPtr
	= do {
		(_,delayinfo) <- issueCleanRequest osIgnoreCallback' (rq1Cci ccRqACTIVATECONTROL controlPtr) [];
		return (reverse delayinfo)
	  }
	where
		osIgnoreCallback' :: CrossCallInfo -> [DelayActivationInfo] -> IO (CrossCallInfo,[DelayActivationInfo])
		osIgnoreCallback' cci@(CrossCallInfo {ccMsg=msg,p1=p1, p2=p2}) delayinfo
			| msg==ccWmPAINT
				= winFakePaint p1 >> return (return0Cci,delayinfo)
			| msg==ccWmKILLFOCUS
				= return (return0Cci,(DelayDeactivatedControl p1 p2):delayinfo)
			| msg==ccWmSETFOCUS
				= return (return0Cci,(DelayActivatedControl p1 p2):delayinfo)
			| otherwise
				= return (return0Cci,delayinfo)

osStackWindow :: OSWindowPtr -> OSWindowPtr -> (OSEvent -> IO ()) -> IO [DelayActivationInfo]
osStackWindow thisWindow behindWindow handleOSEvent
	= do {
		(_,delayinfo) <- issueCleanRequest (osCallback handleOSEvent)
		                                            (rq2Cci ccRqRESTACKWINDOW thisWindow behindWindow)
		                                            [];
		return (reverse delayinfo)
	  }
	where
	{-	osCallback delays activate and deactivate events.
		All other events are passed to the callback function.
		PA: is now identical to osActivateWindow!!
	-}	osCallback :: (OSEvent -> IO ()) -> CrossCallInfo -> [DelayActivationInfo] -> IO (CrossCallInfo,[DelayActivationInfo])
		osCallback handleOSEvent osEvent@(CrossCallInfo {ccMsg=msg,p1=p1}) delayinfo
			| msg==ccWmACTIVATE
				= return (return0Cci,(DelayActivatedWindow p1):delayinfo)
			| msg==ccWmDEACTIVATE
				= return (return0Cci,(DelayDeactivatedWindow p1):delayinfo)
			| otherwise
				= handleOSEvent osEvent >> return (return0Cci,delayinfo)

osHideWindow :: OSWindowPtr -> Bool -> IO [DelayActivationInfo]
osHideWindow wPtr activate
	= do {
		(_,delayinfo) <- issueCleanRequest osIgnoreCallback' (rq3Cci ccRqSHOWWINDOW wPtr (fromBool False) (fromBool activate)) [];
		return (reverse delayinfo)
	  }

osShowWindow :: OSWindowPtr -> Bool -> IO [DelayActivationInfo]
osShowWindow wPtr activate
	= do {
		(_,delayinfo) <- issueCleanRequest osIgnoreCallback' (rq3Cci ccRqSHOWWINDOW wPtr (fromBool True) (fromBool activate)) [];
		return (reverse delayinfo)
	  }

osSetWindowCursor :: OSWindowPtr -> Int -> IO ()
osSetWindowCursor wPtr cursorCode
	= winSetWindowCursor wPtr cursorCode

osGetWindowPos :: OSWindowPtr -> IO (Int,Int)
osGetWindowPos wPtr
	= winGetWindowPos wPtr

osGetWindowViewFrameSize :: OSWindowPtr -> IO (Int,Int)
osGetWindowViewFrameSize wPtr
	= winGetClientSize wPtr

osGetWindowSize :: OSWindowPtr -> IO (Int,Int)
osGetWindowSize wPtr
	= winGetWindowSize wPtr

osSetWindowPos :: OSWindowPtr -> (Int,Int) -> Bool -> Bool -> IO ()
osSetWindowPos wPtr pos update inclScrollbars
	= winSetWindowPos wPtr pos update inclScrollbars

osSetWindowViewFrameSize :: OSWindowPtr -> (Int,Int) -> IO ()
osSetWindowViewFrameSize wPtr size
	= winSetClientSize wPtr size

osSetWindowSize :: OSWindowPtr -> (Int,Int) -> Bool -> IO ()
osSetWindowSize wPtr size update
	= winSetWindowSize wPtr size update

osSetWindowTitle :: OSWindowPtr -> String -> IO ()
osSetWindowTitle wPtr title
	= winSetWindowTitle wPtr title


{-	Control access operations. -}

--	On compound controls:

osInvalidateCompound :: OSWindowPtr -> IO ()
osInvalidateCompound compoundPtr
	= winInvalidateWindow compoundPtr

osInvalidateCompoundRect :: OSWindowPtr -> Rect -> IO ()
osInvalidateCompoundRect compoundPtr rect
	= winInvalidateRect compoundPtr (toTuple4 rect)

osSetCompoundSliderThumb :: OSWindowMetrics -> OSWindowPtr -> Bool -> Int -> (Int,Int) -> Bool -> IO ()
osSetCompoundSliderThumb wMetrics compoundPtr isHorizontal thumb (maxx,maxy) redraw
	= winSetScrollPos compoundPtr (if isHorizontal then sb_HORZ else sb_VERT) thumb maxx' maxy' extent
	where
	   (maxx',maxy',extent) = if redraw then (maxx,maxy,(if isHorizontal then osmHSliderHeight else osmVSliderWidth) wMetrics) else (0,0,0)

osSetCompoundSliderThumbSize :: OSWindowMetrics -> OSWindowPtr -> Bool -> Int -> (Int,Int) -> Bool -> IO ()
osSetCompoundSliderThumbSize wMetrics compoundPtr isHorizontal size (maxx,maxy) redraw
	= winSetScrollThumbSize compoundPtr (if isHorizontal then sb_HORZ else sb_VERT) size maxx' maxy' extent
	where
	   (maxx',maxy',extent)	= if redraw then (maxx,maxy,(if isHorizontal then osmHSliderHeight else osmVSliderWidth) wMetrics) else (0,0,0)

osSetCompoundSlider :: OSWindowMetrics -> OSWindowPtr -> Bool -> (Int,Int,Int,Int) -> (Int,Int) -> IO ()
osSetCompoundSlider wMetrics compoundPtr isHorizontal state maxcoords
	= setScrollRangeAndPos True True wMetrics (if isHorizontal then sb_HORZ else sb_VERT) state maxcoords compoundPtr

osSetCompoundSelect :: OSWindowPtr -> OSWindowPtr -> Rect -> (Bool,Bool) -> Bool -> IO ()
osSetCompoundSelect _ compoundPtr _ scrollInfo select
	= winSetSelectStateWindow compoundPtr scrollInfo select False

osSetCompoundShow :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetCompoundShow _ compoundPtr _ show
	= winShowControl compoundPtr show

osSetCompoundPos :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetCompoundPos _ (parent_x,parent_y) compoundPtr (x,y) _ update
	= winSetWindowPos compoundPtr (x-parent_x,y-parent_y) update True

osSetCompoundSize :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetCompoundSize _ _ compoundPtr _ size update
	= winSetWindowSize compoundPtr size update

osCompoundMovesControls = True

--	On slider controls:

osSetSliderThumb :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> (Int,Int,Int) -> IO ()
osSetSliderThumb _ cPtr _ redraw (min,thumb,max)
	= winSetScrollPos cPtr sb_CTL thumb 0 0 0

osSetSliderControlSelect :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetSliderControlSelect _ cPtr _ select
	= winEnableControl cPtr select

osSetSliderControlShow :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetSliderControlShow _ cPtr _ show
	= winShowControl cPtr show

osSetSliderControlPos :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetSliderControlPos _ (parent_x,parent_y) sliderPtr (x,y) _ update
	= winSetWindowPos sliderPtr (x-parent_x,y-parent_y) update False

osSetSliderControlSize :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetSliderControlSize _ _ sliderPtr _ size update
	= winSetWindowSize sliderPtr size update

--	On radio controls:

osCheckRadioControl :: OSWindowPtr -> OSWindowPtr -> Bool -> IO ()
osCheckRadioControl _ cPtr b
	= winCheckControl cPtr b

osSetRadioControlSelect :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetRadioControlSelect _ cPtr _ select
	= winEnableControl cPtr select

osSetRadioControlShow :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetRadioControlShow _ cPtr _ show
	= winShowControl cPtr show

osSetRadioControlPos :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetRadioControlPos _ (parent_x,parent_y) radioPtr (x,y) _ update
	= winSetWindowPos radioPtr (x-parent_x,y-parent_y) update False

osSetRadioControlSize :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetRadioControlSize _ _ radioPtr _ size update
	= winSetWindowSize radioPtr size update


--	On check controls:

osCheckCheckControl :: OSWindowPtr -> OSWindowPtr -> Bool -> IO ()
osCheckCheckControl _ cPtr check = winCheckControl cPtr check

osSetCheckControlSelect :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetCheckControlSelect _ cPtr _ select
	= winEnableControl cPtr select

osSetCheckControlShow :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetCheckControlShow _ cPtr _ show
	= winShowControl cPtr show

osSetCheckControlPos :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetCheckControlPos _ (parent_x,parent_y) checkPtr (x,y) _ update
	= winSetWindowPos checkPtr (x-parent_x,y-parent_y) update False

osSetCheckControlSize :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetCheckControlSize _ _ checkPtr _ size update
	= winSetWindowSize checkPtr size update


--	On pop up controls:

osSelectPopUpControlItem :: OSWindowPtr -> OSWindowPtr -> Int -> IO ()
osSelectPopUpControlItem _ pPtr new
	= winSelectPopupItem pPtr (new-1)

osSetPopUpControlSelect :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetPopUpControlSelect _ pPtr _ select
	= winEnableControl pPtr select

osSetPopUpControlShow :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetPopUpControlShow _ pPtr _ show
	= winShowControl pPtr show

osSetPopUpControlPos :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetPopUpControlPos _ (parent_x,parent_y) popupPtr (x,y) _ update
	= winSetWindowPos popupPtr (x-parent_x,y-parent_y) update False

osSetPopUpControlSize :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetPopUpControlSize _ _ popupPtr _ size update
	= winSetWindowSize popupPtr size update
	
--	On list box controls:

osSelectListBoxControlItem :: OSWindowPtr -> OSWindowPtr -> Int -> IO ()
osSelectListBoxControlItem _ pPtr new
	= winSelectListBoxItem pPtr (new-1)
	
osMarkListBoxControlItem :: OSWindowPtr -> OSWindowPtr -> Int -> Bool -> IO ()
osMarkListBoxControlItem _ pPtr new mark
	= winMarkListBoxItem pPtr (new-1) mark

osSetListBoxControlSelect :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetListBoxControlSelect _ pPtr _ select
	= winEnableControl pPtr select

osSetListBoxControlShow :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetListBoxControlShow _ pPtr _ show
	= winShowControl pPtr show

osSetListBoxControlPos :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetListBoxControlPos _ (parent_x,parent_y) popupPtr (x,y) _ update
	= winSetWindowPos popupPtr (x-parent_x,y-parent_y) update False

osSetListBoxControlSize :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetListBoxControlSize _ _ popupPtr _ size update
	= winSetWindowSize popupPtr size update

--	On edit controls:

osSetEditControlText :: OSWindowPtr -> OSWindowPtr -> Rect -> Rect -> Bool -> String -> IO ()
osSetEditControlText _ ePtr _ _ _ text
	= winSetWindowTitle ePtr text

osGetEditControlText :: OSWindowPtr -> OSWindowPtr -> IO String
osGetEditControlText _ ePtr
	= winGetWindowText ePtr

osSetEditControlCursor :: OSWindowPtr -> OSWindowPtr -> Rect -> Rect -> Int -> IO ()
osSetEditControlCursor _ ePtr _ _ pos
	= winSetEditSelection ePtr pos (pos+1)

osSetEditControlSelect :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetEditControlSelect _ ePtr _ select
	= winEnableControl ePtr select

osSetEditControlShow :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetEditControlShow _ ePtr _ show
	= winShowControl ePtr show

osSetEditControlPos :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetEditControlPos _ (parent_x,parent_y) editPtr (x,y) _ update
	= winSetWindowPos editPtr (x-parent_x,y-parent_y) update False

osSetEditControlSize :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetEditControlSize _ _ editPtr _ size update
	= winSetWindowSize editPtr size update


--	On text controls:

osSetTextControlText :: OSWindowPtr -> OSWindowPtr -> Rect -> Rect -> Bool -> String -> IO ()
osSetTextControlText _ tPtr _ _ _ text
	= winSetWindowTitle tPtr text

osSetTextControlSelect :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetTextControlSelect _ tPtr _ select
	= winEnableControl tPtr select

osSetTextControlShow :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetTextControlShow _ tPtr _ show
	= winShowControl tPtr show

osSetTextControlPos :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetTextControlPos _ (parent_x,parent_y) textPtr (x,y) _ update
	= winSetWindowPos textPtr (x-parent_x,y-parent_y) update False

osSetTextControlSize :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetTextControlSize _ _ textPtr _ size update
	= winSetWindowSize textPtr size update


--	On button controls:

osSetButtonControlText :: OSWindowPtr -> OSWindowPtr -> Rect -> String -> IO ()
osSetButtonControlText _ bPtr _ text
	= winSetWindowTitle bPtr text

osSetButtonControlSelect :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetButtonControlSelect _ bPtr _ select
	= winEnableControl bPtr select

osSetButtonControlShow :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetButtonControlShow _ bPtr _ show
	= winShowControl bPtr show

osSetButtonControlPos :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetButtonControlPos _ (parent_x,parent_y) buttonPtr (x,y) _ update
	= winSetWindowPos buttonPtr (x-parent_x,y-parent_y) update False

osSetButtonControlSize :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetButtonControlSize _ _ buttonPtr _ size update
	= winSetWindowSize buttonPtr size update

--	On custom button controls:

osSetCustomButtonControlSelect :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetCustomButtonControlSelect _ cPtr _ select
	= winEnableControl cPtr select

osSetCustomButtonControlShow :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetCustomButtonControlShow _ cPtr _ show
	= winShowControl cPtr show

osSetCustomButtonControlPos :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetCustomButtonControlPos _ (parent_x,parent_y) cPtr (x,y) _ update
	= winSetWindowPos cPtr (x-parent_x,y-parent_y) update False

osSetCustomButtonControlSize :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetCustomButtonControlSize _ _ cPtr _ size update
	= winSetWindowSize cPtr size update

--	On custom controls:

osSetCustomControlSelect :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetCustomControlSelect _ cPtr _ select
	= winEnableControl cPtr select

osSetCustomControlShow :: OSWindowPtr -> OSWindowPtr -> Rect -> Bool -> IO ()
osSetCustomControlShow _ cPtr _ show
	= winShowControl cPtr show

osSetCustomControlPos :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetCustomControlPos _ (parent_x,parent_y) customPtr (x,y) _ update
	= winSetWindowPos customPtr (x-parent_x,y-parent_y) update False

osSetCustomControlSize :: OSWindowPtr -> (Int,Int) -> OSWindowPtr -> (Int,Int) -> (Int,Int) -> Bool -> IO ()
osSetCustomControlSize _ _ customPtr _ size update
	= winSetWindowSize customPtr size update
