-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Window.Access
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Window.Access defines access operations to Window(State)Handle(s).
--	
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Window.Access
		( module Graphics.UI.ObjectIO.Window.Access
		, module Graphics.UI.ObjectIO.Window.Handle
		) where



import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.OS.System
import Graphics.UI.ObjectIO.Window.Handle
import Graphics.UI.ObjectIO.KeyFocus
import Graphics.UI.ObjectIO.StdControlAttribute(isControlKeyboard)
import Graphics.UI.ObjectIO.StdWindowAttribute(isWindowInitActive, getWindowInitActiveAtt)
import Graphics.UI.ObjectIO.OS.WindowCrossCall_12
import Data.List(find)
import GHC.Base(unsafeCoerce#)


windowaccessFatalError :: String -> String -> x
windowaccessFatalError function error
	= dumpFatalError function "WindowAccess" error


--	Dummy values for window handles.

dummyWindowHandles :: WindowHandles ps
dummyWindowHandles
	= WindowHandles
		{ whsWindows       = undefined
		, whsNrWindowBound = Infinite
		, whsFinalModalLS  = []
		, whsModal	   = False
		}

dummyWindowStateHandle :: WindowStateHandle ps
dummyWindowStateHandle
	= WindowStateHandle undefined undefined

dummyWindowLSHandle :: WindowLSHandle ls ps
dummyWindowLSHandle
	= WindowLSHandle
		{ wlsState    = undefined
		, wlsHandle   = undefined
		}

dummyWindowHandle :: WindowHandle ls ps
dummyWindowHandle
	= WindowHandle
		{ whMode      = undefined
		, whKind      = undefined
		, whTitle     = undefined
		, whItemNrs   = undefined
		, whKeyFocus  = undefined
		, whWindowInfo= undefined
		, whItems     = undefined
		, whShow      = undefined
		, whAtts      = undefined
		, whSelect    = undefined
		, whDefaultId = undefined
		, whCancelId  = undefined
		, whSize      = undefined
		, whClosing   = undefined
		}

initWindowHandle :: Title -> WindowMode -> WindowKind -> WindowInfo -> [WElementHandle ls ps] -> [WindowAttribute ls ps] -> WindowHandle ls ps
initWindowHandle title mode kind info itemHs atts
	= WindowHandle
		{ whMode      = mode
		, whKind      = kind
		, whTitle     = title
		, whItemNrs   = [1..]
		, whKeyFocus  = KeyFocus {kfItem=Nothing,kfItems=[]}
		, whWindowInfo= info
		, whItems     = itemHs
		, whShow      = True
		, whAtts      = atts
		, whSelect    = True
		, whDefaultId = Nothing
		, whCancelId  = Nothing
		, whSize      = zero
		, whClosing   = False
		}
		

--	Access to the additional WItemInfo field of a WItemHandle (partial functions!).

getWItemRadioInfo :: WItemInfo ls ps -> RadioInfo ls ps
getWItemRadioInfo (WRadioInfo info) = info

getWItemCheckInfo :: WItemInfo ls ps -> CheckInfo ls ps
getWItemCheckInfo (WCheckInfo info) = info

getWItemPopUpInfo :: WItemInfo ls ps -> PopUpInfo ls ps
getWItemPopUpInfo (WPopUpInfo info) = info

getWItemListBoxInfo :: WItemInfo ls ps -> ListBoxInfo ls ps
getWItemListBoxInfo (WListBoxInfo info) = info

getWItemSliderInfo :: WItemInfo ls ps -> SliderInfo ls ps
getWItemSliderInfo (WSliderInfo info) = info

getWItemTextInfo :: WItemInfo ls ps -> TextInfo
getWItemTextInfo (WTextInfo info) = info

getWItemEditInfo :: WItemInfo ls ps -> EditInfo
getWItemEditInfo (WEditInfo info) = info

getWItemButtonInfo :: WItemInfo ls ps -> ButtonInfo
getWItemButtonInfo (WButtonInfo info) = info

getWItemCustomButtonInfo :: WItemInfo ls ps -> CustomButtonInfo
getWItemCustomButtonInfo (WCustomButtonInfo info) = info

getWItemCustomInfo :: WItemInfo ls ps -> CustomInfo
getWItemCustomInfo (WCustomInfo info) = info

getWItemCompoundInfo :: WItemInfo ls ps -> CompoundInfo
getWItemCompoundInfo (WCompoundInfo info) = info



--	For internal identification of windows/dialogs Id and OSWindowPtr (Integer) can be used.

data	WID				-- Identify a window/dialog either
	= ById  !Id			-- by its Id, or
	| ByPtr !OSWindowPtr		-- by its OSWindowPtr

class ToWID x where
	toWID :: x -> WID

instance ToWID Id where
	toWID id = ById id
instance ToWID Int where
	toWID wPtr = ByPtr wPtr
instance ToWID WIDS where
	toWID wids = ByPtr (wPtr wids)

widbyId :: WID -> Bool
widbyId (ById _) = True
widbyId _        = False

widbyPtr :: WID -> Bool
widbyPtr (ByPtr _) = True
widbyPtr _         = False

widgetId :: WID -> Id
widgetId (ById id) = id

widgetPtr :: WID -> OSWindowPtr
widgetPtr (ByPtr ptr) = ptr

identifyWIDS :: WID -> WIDS -> Bool
identifyWIDS (ById  id)  (WIDS {wId=wId})   = id==wId
identifyWIDS (ByPtr ptr) (WIDS {wPtr=wPtr}) = ptr==wPtr

identifyMaybeId :: Id -> Maybe Id -> Bool
identifyMaybeId id (Just id') = id==id'
identifyMaybeId _ _ = False


--	Transforming CursorShape to OS cursor codes:
toCursorCode :: CursorShape -> Int
toCursorCode StandardCursor	= cursARROW
toCursorCode BusyCursor		= cursBUSY
toCursorCode IBeamCursor	= cursIBEAM
toCursorCode CrossCursor	= cursCROSS
toCursorCode FatCrossCursor	= cursFATCROSS
toCursorCode ArrowCursor	= cursARROW
toCursorCode HiddenCursor	= cursHIDDEN


--	Calculating the view frame of window/compound with visibility of scrollbars.

getCompoundContentRect :: OSWindowMetrics -> (Bool,Bool) -> Rect -> Rect
getCompoundContentRect wMetrics (visHScroll,visVScroll) itemRect
	| visHScroll && visVScroll = itemRect {rright=r',rbottom=b'}
	| visHScroll               = itemRect {          rbottom=b'}
	| visVScroll               = itemRect {rright=r'           }
	| otherwise                = itemRect
	where
		r'                 = rright  itemRect - osmVSliderWidth  wMetrics
		b'                 = rbottom itemRect - osmHSliderHeight wMetrics

getCompoundHScrollRect :: OSWindowMetrics -> (Bool,Bool) -> Rect -> Rect
getCompoundHScrollRect wMetrics (visHScroll,visVScroll) itemRect
	| not visHScroll = zero
	| visVScroll     = itemRect {rtop=b',rright=r'}
	| otherwise      = itemRect {rtop=b'}
	where
		r'       = rright  itemRect - osmVSliderWidth  wMetrics
		b'       = rbottom itemRect - osmHSliderHeight wMetrics

getCompoundVScrollRect :: OSWindowMetrics -> (Bool,Bool) -> Rect -> Rect
getCompoundVScrollRect wMetrics (visHScroll,visVScroll) itemRect
	| not visVScroll = zero
	| visHScroll     = itemRect {rleft=r',rbottom=b'}
	| otherwise      = itemRect {rleft=r'}
	where
		r'       = rright  itemRect - osmVSliderWidth  wMetrics
		b'       = rbottom itemRect - osmHSliderHeight wMetrics


getWindowContentRect :: OSWindowMetrics -> (Bool,Bool) -> Rect -> Rect
getWindowContentRect wMetrics (visHScroll,visVScroll) itemRect
	| visHScroll && visVScroll = itemRect {rright=r',rbottom=b'}
	| visHScroll               = itemRect {          rbottom=b'}
	| visVScroll               = itemRect {rright=r'           }
	| otherwise                = itemRect
	where
		r'                 = rright  itemRect - osmVSliderWidth  wMetrics
		b'                 = rbottom itemRect - osmHSliderHeight wMetrics

getWindowHScrollRect :: OSWindowMetrics -> (Bool,Bool) -> Rect -> Rect
getWindowHScrollRect wMetrics (visHScroll,visVScroll) (Rect {rleft=rleft,rtop=rtop,rright=rright,rbottom=rbottom})
	| not visHScroll = zero
	| otherwise      = Rect {rleft=rleft-1, rtop=b', rright=if visVScroll then r'+1 else rright+1, rbottom=rbottom+1}
	where
		r'       = rright  - osmVSliderWidth  wMetrics + 1
		b'       = rbottom - osmHSliderHeight wMetrics + 1

getWindowVScrollRect :: OSWindowMetrics -> (Bool,Bool) -> Rect -> Rect
getWindowVScrollRect wMetrics (visHScroll,visVScroll) (Rect {rleft=rleft,rtop=rtop,rright=rright,rbottom=rbottom})
	| not visVScroll = zero
	| otherwise      = Rect {rleft=r', rtop=rtop-1, rright=rright+1, rbottom=if visHScroll then b'+1 else rbottom+1}
	where
		r'       = rright  - osmVSliderWidth  wMetrics + 1
		b'       = rbottom - osmHSliderHeight wMetrics + 1


--	Access operations on WindowStateHandles:

isModalWindow :: WindowStateHandle ps -> Bool
isModalWindow wsH = getWindowStateHandleWindowMode wsH == Modal
		
getWindowStateHandleWIDS :: WindowStateHandle ps -> WIDS
getWindowStateHandleWIDS wsH@(WindowStateHandle wshIds _) = wshIds

getWindowStateHandleWindowMode :: WindowStateHandle ps -> WindowMode
getWindowStateHandleWindowMode wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=WindowHandle {whMode=whMode}}))) = whMode
	
getWindowStateHandleWindowKind :: WindowStateHandle ps -> WindowKind
getWindowStateHandleWindowKind wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=WindowHandle {whKind=whKind}}))) = whKind

getWindowStateHandleWindowTitle :: WindowStateHandle ps -> Title
getWindowStateHandleWindowTitle wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=WindowHandle {whTitle=whTitle}}))) = whTitle

getWindowStateHandleItemNrs :: WindowStateHandle ps -> [Int]
getWindowStateHandleItemNrs wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=WindowHandle {whItemNrs=whItemNrs}}))) = whItemNrs

getWindowStateHandleKeyFocus :: WindowStateHandle ps -> KeyFocus
getWindowStateHandleKeyFocus wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=WindowHandle {whKeyFocus=whKeyFocus}}))) = whKeyFocus

getWindowStateHandleWindowInfo :: WindowStateHandle ps -> WindowInfo
getWindowStateHandleWindowInfo wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=WindowHandle {whWindowInfo=whWindowInfo}}))) = whWindowInfo

getWindowStateHandleShow :: WindowStateHandle ps -> Bool
getWindowStateHandleShow wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=WindowHandle {whShow=whShow}}))) = whShow

getWindowStateHandleActive :: WindowStateHandle ps -> Bool
getWindowStateHandleActive wsH@(WindowStateHandle (WIDS {wActive=wActive}) _) = wActive

getWindowStateHandleDefaultId :: WindowStateHandle ps -> Maybe Id
getWindowStateHandleDefaultId wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=WindowHandle{whDefaultId=whDefaultId}})))
	= whDefaultId

getWindowStateHandleCancelId :: WindowStateHandle ps -> Maybe Id
getWindowStateHandleCancelId wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=WindowHandle{whCancelId=whCancelId}})))
	= whCancelId

getWindowStateHandleSelect :: WindowStateHandle ps -> Bool
getWindowStateHandleSelect wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=WindowHandle {whSelect=whSelect}}))) = whSelect

getWindowStateHandleSize :: WindowStateHandle ps -> Size
getWindowStateHandleSize wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=WindowHandle {whSize=whSize}}))) = whSize

getWindowStateHandleClosing :: WindowStateHandle ps -> Bool
getWindowStateHandleClosing wsH@(WindowStateHandle _ (Just (WindowLSHandle {wlsHandle=WindowHandle {whClosing=whClosing}}))) = whClosing

isWindowStateHandlePlaceHolder :: WindowStateHandle ps -> Bool
isWindowStateHandlePlaceHolder wsH@(WindowStateHandle _ Nothing) = True
isWindowStateHandlePlaceHolder wsH = False

identifyWindowStateHandle :: WID -> WindowStateHandle ps -> Bool
identifyWindowStateHandle wid wsH = identifyWIDS wid (getWindowStateHandleWIDS wsH)

setWindowStateHandleWIDS :: WIDS -> WindowStateHandle ps -> WindowStateHandle ps
setWindowStateHandleWIDS wids (WindowStateHandle _ wlsH) = WindowStateHandle wids wlsH

setWindowStateHandleWindowTitle :: Title -> WindowStateHandle ps -> WindowStateHandle ps
setWindowStateHandleWindowTitle title (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH})))
	= WindowStateHandle wids (Just (wlsH {wlsHandle=wH {whTitle=title}}))

setWindowStateHandleItemNrs :: [Int] -> WindowStateHandle ps -> WindowStateHandle ps
setWindowStateHandleItemNrs itemNrs (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH})))
	= WindowStateHandle wids (Just (wlsH {wlsHandle=wH {whItemNrs=itemNrs}}))

setWindowStateHandleActive :: Bool -> WindowStateHandle ps -> WindowStateHandle ps
setWindowStateHandleActive active (WindowStateHandle wids wlsH)
	= WindowStateHandle (wids {wActive=active}) wlsH

setWindowStateHandleSelect :: Bool -> WindowStateHandle ps -> WindowStateHandle ps
setWindowStateHandleSelect select (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH})))
	= WindowStateHandle wids (Just (wlsH {wlsHandle=wH {whSelect=select}}))
	
setWindowStateHandleSize :: Size -> WindowStateHandle ps -> WindowStateHandle ps
setWindowStateHandleSize size (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH})))
	= WindowStateHandle wids (Just (wlsH {wlsHandle=wH {whSize=size}}))

setWindowStateHandleClosing :: Bool -> WindowStateHandle ps -> WindowStateHandle ps
setWindowStateHandleClosing closing (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH})))
	= WindowStateHandle wids (Just (wlsH {wlsHandle=wH {whClosing=closing}}))


{-	Access operations on the margins and item space attributes of the window attributes.
	getWindow((H/V)Margin/ItemSpace)s type metrics atts
		retrieves the indicated attribute if present from the attribute list. If the attribute
		could not be found, the appropriate default value is returned. 
-}
getWindowHMargins :: WindowKind -> OSWindowMetrics -> [WindowAttribute ls ps] -> (Int,Int)
getWindowHMargins wKind wMetrics atts
	= (defaultLeft,defaultRight)
--	= getWindowHMarginAtt (snd (cselect isWindowHMargin (WindowHMargin defaultLeft defaultRight) atts))
	where
		(defaultLeft,defaultRight) = case wKind of
						IsDialog -> (osmHorMargin wMetrics,osmHorMargin wMetrics)
						other    -> (0,0)

getWindowVMargins :: WindowKind -> OSWindowMetrics -> [WindowAttribute ls ps] -> (Int,Int)
getWindowVMargins wKind wMetrics atts
	= (defaultTop,defaultBottom)
--	= getWindowVMarginAtt (snd (cselect isWindowVMargin (WindowVMargin defaultTop defaultBottom) atts))
	where
		(defaultTop,defaultBottom) = case wKind of
						IsDialog -> (osmVerMargin wMetrics,osmVerMargin wMetrics)
						other    -> (0,0)

getWindowItemSpaces :: WindowKind -> OSWindowMetrics -> [WindowAttribute ls ps] -> (Int,Int)
getWindowItemSpaces wKind wMetrics atts
	= (defaultHor,defaultVer)
--	= getWindowItemSpaceAtt (snd (cselect isWindowItemSpace (WindowItemSpace defaultHor defaultVer) atts))
	where
		(defaultHor,defaultVer) = case wKind of
						IsDialog -> (osmHorItemSpace wMetrics,osmVerItemSpace wMetrics)
						other    -> (0,0)


--	Search, get, and set WindowStateHandles.

getWindowHandlesActiveWindow :: WindowHandles ps -> Maybe WIDS
getWindowHandlesActiveWindow wHs@(WindowHandles {whsWindows=wsHs})
	= fmap getWindowStateHandleWIDS (find (wActive . getWindowStateHandleWIDS) wsHs)
		

--	getWindowHandlesActiveModalDialog assumes that all modal dialogues are at the front of the list
getWindowHandlesActiveModalDialog :: WindowHandles ps -> Maybe WIDS
getWindowHandlesActiveModalDialog wHs@(WindowHandles {whsWindows=[]})
	= Nothing
getWindowHandlesActiveModalDialog wHs@(WindowHandles {whsWindows=(wsH:wsHs)})	
	| mode /= Modal = Nothing
	| otherwise    	= Just wids
	where
		mode = getWindowStateHandleWindowMode wsH
		wids = getWindowStateHandleWIDS wsH

hasWindowHandlesWindow :: WID -> WindowHandles ps -> Bool
hasWindowHandlesWindow wid wHs@(WindowHandles {whsWindows=whsWindows})
	= haswindow wid whsWindows
	where		
		haswindow :: WID -> [WindowStateHandle ps] -> Bool
		haswindow wid (wsH:wsHs) = identifyWIDS wid wIds || haswindow wid wsHs
			where wIds = getWindowStateHandleWIDS wsH
		haswindow _ _ = False

getWindowHandlesWindow :: WID -> WindowHandles ps -> (Bool,WindowStateHandle ps,WindowHandles ps)
getWindowHandlesWindow wid wHs@(WindowHandles {whsWindows=whsWindows})
	= (ok,wsH,wHs {whsWindows=whsWindows1})
	where
		(ok,wsH,whsWindows1) = getwindow wid whsWindows
		
		getwindow :: WID -> [WindowStateHandle ps] -> (Bool,WindowStateHandle ps,[WindowStateHandle ps])
		getwindow wid (wsH:wsHs)
			| identifyWIDS wid wIds
			      = (True,wsH,(WindowStateHandle wIds Nothing):wsHs)
			| otherwise
			      = let (found,wsH1,wsHs1) = getwindow wid wsHs
			        in  (found,wsH1,wsH:wsHs1)
			where
				wIds = getWindowStateHandleWIDS wsH
		getwindow _ _
			= (False,dummyWindowStateHandle,[])

removeWindowHandlesWindow :: WID -> WindowHandles ps -> (Bool,WindowStateHandle ps,WindowHandles ps)
removeWindowHandlesWindow wid wHs@(WindowHandles {whsWindows=whsWindows})
	= (ok,wsH,wHs {whsWindows=whsWindows1})
	where
		(ok,wsH,whsWindows1) = remove (identifyWindowStateHandle wid) dummyWindowStateHandle whsWindows
		
		identifyWindowStateHandle :: WID -> WindowStateHandle ps -> Bool
		identifyWindowStateHandle wid wsH = identifyWIDS wid wIds
			where wIds = getWindowStateHandleWIDS wsH

setWindowHandlesWindow :: WindowStateHandle ps -> WindowHandles ps -> WindowHandles ps
setWindowHandlesWindow wsH wHs@(WindowHandles {whsWindows=whsWindows})
	| isWindowStateHandlePlaceHolder wsH
		= windowaccessFatalError "setWindowHandlesWindow" "WindowStateHandle argument should not be a place holder"
	| otherwise
		= wHs{whsWindows=setwindow (getWindowStateHandleWIDS wsH) wsH whsWindows}
	where		
		setwindow :: WIDS -> WindowStateHandle ps -> [WindowStateHandle ps] -> [WindowStateHandle ps]
		setwindow wids' wsH' (wsH:wsHs)
			| wids/=wids'
				= wsH:setwindow wids' wsH' wsHs
			| isWindowStateHandlePlaceHolder wsH
				= wsH':wsHs
			| otherwise
				= windowaccessFatalError "setWindowHandlesWindow" "place holder expected instead of WindowStateHandle"
			where
				wids = getWindowStateHandleWIDS wsH
		setwindow _ _ _
			= windowaccessFatalError "setWindowHandlesWindow" "place holder not found"

addBehindWindowHandlesWindow :: WID -> WindowStateHandle ps -> WindowHandles ps -> (WIDS,WindowHandles ps)
addBehindWindowHandlesWindow behindWID wsH wHs@(WindowHandles {whsWindows=whsWindows})
	| isWindowStateHandlePlaceHolder wsH
		= windowaccessFatalError "addBehindWindowHandlesWindow" "WindowStateHandle argument should not be a place holder"
	| otherwise
		= let (behindWIDS,whsWindows1) = stackBehind behindWID wsH whsWindows
	          in  (behindWIDS,wHs {whsWindows=whsWindows1})
	where		
		stackBehind :: WID -> WindowStateHandle ps -> [WindowStateHandle ps] -> (WIDS,[WindowStateHandle ps])
		stackBehind behindWID wsH (wsH':wsHs)
			| not (identifyWIDS behindWID wids')
				= let (behindWIDS,wsHs1) = stackBehind behindWID wsH wsHs
			          in  (behindWIDS,wsH':wsHs1)
			| otherwise
				= (wids',wsH':wsH:wsHs)
			where
				wids' = getWindowStateHandleWIDS wsH'
		stackBehind _ _ _
			= windowaccessFatalError "addBehindWindowHandlesWindow" "behind window could not be found"

addWindowHandlesWindow :: Index -> WindowStateHandle ps -> WindowHandles ps -> WindowHandles ps
addWindowHandlesWindow index wsH wHs@(WindowHandles {whsWindows=whsWindows})
	= wHs {whsWindows=insert (max 0 index) wsH whsWindows}
	where
		insert :: Index -> x -> [x] -> [x]
		insert 0 x ys
			= x:ys
		insert i x (y:ys)
			= y:insert (i-1) x ys
		insert _ x _
			= [x]

addWindowHandlesActiveWindow :: WindowStateHandle ps -> WindowHandles ps -> WindowHandles ps
addWindowHandlesActiveWindow wsH wHs@(WindowHandles {whsWindows=whsWindows})
	= wHs {whsWindows=wsH:whsWindows}


{-	Checking WindowBounds:
-}
checkZeroWindowHandlesBound :: WindowHandles ps -> Bool
checkZeroWindowHandlesBound (WindowHandles {whsNrWindowBound=whsNrWindowBound})
	= zeroBound whsNrWindowBound

decreaseWindowHandlesBound :: WindowHandles ps -> WindowHandles ps
decreaseWindowHandlesBound wHs@(WindowHandles {whsNrWindowBound=whsNrWindowBound})
	= wHs {whsNrWindowBound=decBound whsNrWindowBound}


{-	getInitActiveControl retrieves the OSWindowPtr of the control that has the initial input focus.
	It is assumed that the control identified by the WindowInitActive attribute exists.
-}
getInitActiveControl :: WindowHandle ls ps -> OSWindowPtr
getInitActiveControl wH@(WindowHandle {whItems=itemHs,whAtts=whAtts}) = accessWElementHandles itemHs
	where
		initActiveId = fmap getWindowInitActiveAtt (find isWindowInitActive whAtts)

		condition itemH = 
			case initActiveId of
				Just _	-> wItemId   itemH==initActiveId
				Nothing -> wItemKind itemH==IsEditControl
		
		accessWElementHandles :: [WElementHandle ls ps] -> OSWindowPtr
		accessWElementHandles (itemH:itemHs)
			= let wPtr = accessWElementHandle itemH
			  in  if wPtr /= osNoWindowPtr then wPtr else accessWElementHandles itemHs
			where
				accessWElementHandle :: WElementHandle ls ps -> OSWindowPtr
				accessWElementHandle (WListLSHandle itemHs)
					= accessWElementHandles itemHs
				accessWElementHandle (WExtendLSHandle addLS itemHs)
					= accessWElementHandles itemHs
				accessWElementHandle (WChangeLSHandle newLS itemHs)
					= accessWElementHandles itemHs
				accessWElementHandle itemH
					| condition itemH = wItemPtr itemH
					| otherwise       = accessWElementHandles (wItems itemH)
		accessWElementHandles [] = osNoWindowPtr


{-	Determine the list of window items that can obtain the keyboard input focus. -}

getWElementKeyFocusIds :: Bool -> [WElementHandle ls ps] -> [FocusItem]
getWElementKeyFocusIds shownContext (itemH:itemHs) =
	(getWElementKeyFocusIds' shownContext itemH)   ++
	(getWElementKeyFocusIds  shownContext itemHs)
	where
		getWElementKeyFocusIds' :: Bool -> WElementHandle ls ps -> [FocusItem]
		getWElementKeyFocusIds' shownContext itemH@(WItemHandle {wItemNr=wItemNr,wItemKind=wItemKind,wItemShow=wItemShow,wItemAtts=wItemAtts,wItems=wItems})
			| wItemKind==IsEditControl   	= focus
			| keySensitive && hasKeyAtt  	= focus
			| otherwise 			= getWElementKeyFocusIds (shownContext && wItemShow) wItems			
			where
				focus		= [FocusItem {focusNr=wItemNr,focusShow=shownContext}]
				hasKeyAtt	= any isControlKeyboard wItemAtts
				keySensitive	= wItemKind==IsCustomControl

		getWElementKeyFocusIds' shownContext (WListLSHandle itemHs) =
			getWElementKeyFocusIds shownContext itemHs

		getWElementKeyFocusIds' shownContext (WExtendLSHandle ls1 itemHs) =
			getWElementKeyFocusIds shownContext itemHs			

		getWElementKeyFocusIds' shownContext (WChangeLSHandle ls1 itemHs) =
			getWElementKeyFocusIds shownContext itemHs			


getWElementKeyFocusIds _ _ = []


{-	Generate internal numbers for all WElementHandles which wItemNr==0. -}

genWElementItemNrs :: [Int] -> [WElementHandle ls ps] -> ([Int],[WElementHandle ls ps])
genWElementItemNrs nrs (itemH:itemHs)
	= (nrs2,itemH1:itemHs1)
	where
		(nrs1,itemH1)  = genWElementNrs     nrs  itemH
		(nrs2,itemHs1) = genWElementItemNrs nrs1 itemHs
		
		genWElementNrs :: [Int] -> WElementHandle ls ps -> ([Int],WElementHandle ls ps)
		genWElementNrs nrs wItemH@(WItemHandle {wItemNr=wItemNr,wItems=wItems})
			= let (nrs1,itemHs)	= genWElementItemNrs nrs wItems
			  in if wItemNr/=0 then (nrs1,wItemH{wItems=itemHs})
			     else (tail nrs1,wItemH{wItemNr=head nrs1,wItems=itemHs})
		
		genWElementNrs nrs (WListLSHandle itemHs)
			= let (nrs1,itemHs1)  = genWElementItemNrs nrs itemHs
			  in  (nrs1,WListLSHandle itemHs1)
		
		genWElementNrs nrs (WExtendLSHandle addLS itemHs)
			= let (nrs1,itemHs1) = genWElementItemNrs nrs itemHs
			  in  (nrs1,WExtendLSHandle addLS itemHs1)
		
		genWElementNrs nrs (WChangeLSHandle newLS itemHs)
			= let (nrs1,itemHs1) = genWElementItemNrs nrs itemHs
			  in  (nrs1,WChangeLSHandle newLS itemHs1)

genWElementItemNrs nrs _
	= (nrs,[])

getFinalModalLS :: WID -> FinalModalLS -> Maybe ls
getFinalModalLS wid (FinalModalLS wids ls)
	| identifyWIDS wid wids = Just (unsafeCoerce# ls)
	| otherwise		= Nothing
