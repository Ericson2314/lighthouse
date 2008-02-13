-----------------------------------------------------------------------------
-- |
-- Module      :  StdControl
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdControl specifies all control operations.
-- Object I\/O library supports various kinds of built-in controls which
-- can be placed inside windows and dialogs. In order to define his\/her
-- own type controls, the user should give an instance of Controls class
-- (see "Graphics.UI.ObjectIO.StdWindow")
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdControl
		( 
		-- * Function declarations
		
		-- ** Common
		  getParentWindowId, controlSize
		, openControls, closeControls, closeAllControls,
		
		-- ** Show\/Hide controls.
		  showControl, showControls
		, hideControl, hideControls
		, getControlShowStates, getControlShowState,
		
		-- ** Enabling\/Disabling of controls.
		  enableControl,  enableControls
		, disableControl, disableControls
		, getControlSelectStates, getControlSelectState, 
		
		-- ** Control layout
		  getControlLayouts, getControlLayout,
		
		-- ** Get\/Change the text of (Text\/Edit\/Button)Control.
		  setControlText, setControlTexts
		, getControlText, getControlTexts, 
		
		-- ** Drawing in CustomControl, CustomButtonControl and CompoundControl
		  drawInControl, updateControl
		, setControlLooks, setControlLook
		, getControlLooks, getControlLook,
		
		-- ** Positioning and resizing of controls
		  setControlPos, getControlViewSize, getControlViewSizes
		, getControlOuterSizes, getControlOuterSize
		, getControlMinimumSizes, getControlMinimumSize
		, getControlResizes, getControlResize,

		-- ** Selection controls (The functions are applicable to 
		-- PopUpControl, ListBoxControl, CheckControl and RadioControl)
		  getControlItems, getControlItem
		, selectControlItem
		, getControlSelections, getControlSelection
		, markControlItems, unmarkControlItems, setControlsMarkState
		, getControlMarks, getControlMark,
		
		-- ** Compound control specific functions.
		  getControlViewFrames, getControlViewFrame, moveControlViewFrame
		, getControlViewDomains, getControlViewDomain, setControlViewDomain
		, getControlScrollFunction, getControlScrollFunctions, setControlScrollFunction
		, openCompoundControls,
		
		-- ** Edit control specific functions.
		  setEditControlCursor
		, getControlNrLines, getControlNrLine,
		
		-- ** PopUp control specific functions.
		  openPopUpControlItems, closePopUpControlItems,
		  
		-- ** ListBox control specific functions.
		  openListBoxControlItems, closeListBoxControlItems,
		
		-- ** Slider control specific functions.
		  setSliderState, setSliderStates
		, setSliderThumb, setSliderThumbs
		, getSliderDirection, getSliderDirections
		, getSliderState, getSliderStates, 
		
		-- * A visible modules
		  module Graphics.UI.ObjectIO.StdControlDef
		, module Graphics.UI.ObjectIO.StdControlClass
		) where



import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Control.Access
import Graphics.UI.ObjectIO.Control.Internal
import Graphics.UI.ObjectIO.Control.Layout
import Graphics.UI.ObjectIO.Control.Validate(disjointControlIds, controlIdsAreConsistent, getWElementControlIds)
import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.StdControlDef
import Graphics.UI.ObjectIO.StdControlClass
import Graphics.UI.ObjectIO.Window.Access
import Graphics.UI.ObjectIO.Window.Update(updateWindow)
import Graphics.UI.ObjectIO.Window.ClipState (forceValidWindowClipState)
import Graphics.UI.ObjectIO.Window.Controls
import Graphics.UI.ObjectIO.OS.System
import Graphics.UI.ObjectIO.OS.Window(osScrollbarsAreVisible)
import Graphics.UI.ObjectIO.OS.Picture(Draw)
import Control.Monad(unless)
import qualified Data.Map as Map


stdControlFatalError :: String -> String -> x
stdControlFatalError function error
	= dumpFatalError function "StdControl" error

--	The function isOkControlId can be used to filter out the proper IdParent records.
isOkControlId :: SystemId -> (x,Maybe IdParent) -> (Bool,(x,Id))
isOkControlId ioId (x,Just (IdParent {idpIOId=idpIOId,idpDevice=idpDevice,idpId=idpId})) =
	(ioId==idpIOId && idpDevice==WindowDevice,(x,idpId))
isOkControlId _ _ =
	(False,undefined)

-- | getParentWindowId returns id of parent window of control with specified Id.
getParentWindowId :: Id -> GUI ps (Maybe Id)
getParentWindowId id = do
	idtable <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	let maybeParent = Map.lookup id idtable
	let (valid,(_,parentId)) = isOkControlId ioId ((),maybeParent)
	return (if valid then Just parentId else Nothing)

getParentWindowIds :: (a -> Id) -> [a] -> GUI ps [(a, Id)]
getParentWindowIds getId xs = do
	idtable <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	let parentIds  = map (flip Map.lookup idtable . getId) xs
	return (filterMap (isOkControlId ioId) (zip xs parentIds))


mapWindow :: Id -> (forall ls . OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)) -> x -> GUI ps x
mapWindow windowId f x = do
	(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice);
	(if   not found
	 then stdControlFatalError "mapWindow" "WindowSystemState could not be retrieved from IOSt"
	 else 
		let
			windows              = windowSystemStateGetWindowHandles wDevice
			(found,wsH,windows1) = getWindowHandlesWindow (toWID windowId) windows
		in if not found then throwGUI ErrorUnknownObject
		   else do
				wMetrics <- accIOEnv ioStGetOSWindowMetrics
				(wsH1, x1) <- liftIO (ff f wMetrics wsH x)
				let windows2 = setWindowHandlesWindow wsH1 windows1
				appIOEnv (ioStSetDevice (WindowSystemState windows2))
				return x1)
	where
		ff :: (forall ls . OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)) -> OSWindowMetrics -> WindowStateHandle ps -> x -> IO (WindowStateHandle ps, x)
	  	ff f wMetrics (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) x = do
	  		(wH1,x1) <- f wMetrics wids wH x
	  		return (WindowStateHandle wids (Just wlsH{wlsHandle=wH1}),x1)


-- | giving horizontal and vertical margins and item spaces, controlSize function  
-- calculates the size of the given control.
controlSize :: (Controls cdef) => cdef ls ps -> Bool -> Maybe (Int,Int) -> Maybe (Int,Int) -> Maybe (Int,Int) -> GUI ps Size
controlSize cdef isWindow hMargins vMargins itemSpaces
	= do {
		cs       <- controlToHandles cdef;
		wMetrics <- accIOEnv ioStGetOSWindowMetrics;
		let
			itemHs      = map controlStateToWElementHandle cs
			hMargins'   = case hMargins of
					Just (left,right) -> (max 0 left,max 0 right)
					_                 -> if isWindow then (0,0) else (osmHorMargin wMetrics,osmHorMargin wMetrics)
			vMargins'   = case vMargins of
					Just (top,bottom) -> (max 0 top,max 0 bottom)
					_                 -> if isWindow then (0,0) else (osmVerMargin wMetrics,osmVerMargin wMetrics)
			itemSpaces' = case itemSpaces of
					Just (hor,vert)   -> (max 0 hor,max 0 vert)
					_                 -> (osmHorItemSpace wMetrics,osmVerItemSpace wMetrics)
			domain      = viewDomainRange {corner1=zero}
		in liftIO (calcControlsSize wMetrics hMargins' vMargins' itemSpaces' zero zero [(domain,zero)] itemHs)
	  }


-- | Controls can be dynamically added to an existing window with openControls function.
openControls :: Controls cdef => Id -> ls -> cdef ls ps -> GUI ps ()
openControls wId ls newControls = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then throwGUI ErrorUnknownObject
	 else let
		windows = windowSystemStateGetWindowHandles wDevice
		(found,wsH,windows1) = getWindowHandlesWindow (toWID wId) windows
	      in if not found then throwGUI ErrorUnknownObject
	         else do
			cs <- controlToHandles newControls
			let newItemHs = map controlStateToWElementHandle cs
	  		let currentIds = getWindowStateHandleIds wsH
	  		(if not (disjointControlIds currentIds newItemHs) then throwGUI ErrorIdsInUse
			 else do				
				it <- ioStGetIdTable
				ioId <- accIOEnv ioStGetIOId
				(case controlIdsAreConsistent ioId wId newItemHs it of
					Nothing -> throwGUI ErrorIdsInUse
					Just it -> do
						ioStSetIdTable it
						wMetrics <- accIOEnv ioStGetOSWindowMetrics
						wsH <- liftIO (opencontrols wMetrics ls newItemHs wsH)
						appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
						return ())))


---	getWindowStateHandleIds returns all Ids of the controls in this window.
-- 	This function is used by open(Compound)Controls.
getWindowStateHandleIds :: WindowStateHandle ps -> [Id]
getWindowStateHandleIds (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) =
	getWElementControlIds (whItems wH)
getWindowStateHandleIds _ = stdControlFatalError "getWindowStateHandleIds" "unexpected window placeholder argument"

-- | openCompoundControls adds controls to the indicated CompoundControl
openCompoundControls :: Controls cdef => Id -> ls -> cdef ls ps -> GUI ps ()
openCompoundControls cId ls newControls = do
	maybeId <- getParentWindowId cId
	(case maybeId of
		Nothing  -> throwGUI ErrorUnknownObject
		Just wId -> do
			(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
			unless found (throwGUI ErrorUnknownObject)
			(let
				windows = windowSystemStateGetWindowHandles wDevice
				(found,wsH,windows1) = getWindowHandlesWindow (toWID wId) windows
			 in
				if not found then throwGUI ErrorUnknownObject
				else do
					cs <- controlToHandles newControls
					let newItemHs = map controlStateToWElementHandle cs
					let currentIds = getWindowStateHandleIds wsH
					(if not (disjointControlIds currentIds newItemHs) then throwGUI ErrorIdsInUse
					 else do
						it <- ioStGetIdTable
						ioId <- accIOEnv ioStGetIOId
						(case controlIdsAreConsistent ioId wId newItemHs it of
							Nothing -> throwGUI ErrorIdsInUse
							Just it -> do
								ioStSetIdTable it
								osdInfo <- accIOEnv ioStGetOSDInfo
								wMetrics <- accIOEnv ioStGetOSWindowMetrics
								(ok,wsH) <- liftIO (opencompoundcontrols osdInfo wMetrics cId ls newItemHs wsH)
								appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
								(if ok then return () else throwGUI ErrorUnknownObject)))))


-- | The openPopUpControlItems function adds new items to the PopUpControl.
openPopUpControlItems :: Id -> Index -> [PopUpControlItem ps ps] -> GUI ps ()
openPopUpControlItems popUpId index [] = return ()
openPopUpControlItems popUpId index items = do
	maybeId	<- getParentWindowId popUpId
	(case maybeId of
		Nothing  -> return ()		
		Just wId -> do
			(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
			(if not found then return ()
			 else
				let
					wHs	= windowSystemStateGetWindowHandles wDevice
					(found,wsH,wHs1) = getWindowHandlesWindow (toWID wId) wHs
				in
					if not found
					then return ()
					else do		
							wsH <- liftIO (openpopupcontrolitems popUpId index items wsH)
							appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs1)))))
	where
		openpopupcontrolitems :: Id -> Index -> [PopUpControlItem ps ps] -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		openpopupcontrolitems popUpId index items (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
			wH <- openpopupitems popUpId index items (wPtr wids) wH
			return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
		openpopupcontrolitems _ _ _ _ =
			stdControlFatalError "openPopUpControlItems" "unexpected window placeholder argument"

-- | The openListBoxControlItems function adds new items to the ListBoxControl.
openListBoxControlItems :: Id -> Index -> [ListBoxControlItem ps ps] -> GUI ps ()
openListBoxControlItems lboxId index [] = return ()
openListBoxControlItems lboxId index items = do
	maybeId	<- getParentWindowId lboxId
	(case maybeId of
		Nothing  -> return ()		
		Just wId -> do
			(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
			(if not found then return ()
			 else
				let
					wHs	= windowSystemStateGetWindowHandles wDevice
					(found,wsH,wHs1) = getWindowHandlesWindow (toWID wId) wHs
				in
					if not found
					then return ()
					else do		
							wsH <- liftIO (openlistboxcontrolitems lboxId index items wsH)
							appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs1)))))
	where
		openlistboxcontrolitems :: Id -> Index -> [ListBoxControlItem ps ps] -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		openlistboxcontrolitems lboxId index items (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
			wH <- openlistboxitems lboxId index items (wPtr wids) wH
			return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
		openlistboxcontrolitems _ _ _ _ =
			stdControlFatalError "openListBoxControlItems" "unexpected window placeholder argument"

-- | The closeControls function closes the specified controls in the indicated window.
closeControls :: Id -> [Id] -> Bool -> GUI ps ()
closeControls wId ids relayout = do
	(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let
		windows = windowSystemStateGetWindowHandles wDevice
		(found,wsH,windows1) = getWindowHandlesWindow (toWID wId) windows
	      in
	     	if not found then return ()
		else do
			wMetrics <- accIOEnv ioStGetOSWindowMetrics
			(freeIds,disposeFun,wsH) <- liftIO (closecontrols wMetrics ids relayout wsH)
			it <- ioStGetIdTable			
			ioStSetIdTable (foldr Map.delete it (freeIds))
			f <- accIOEnv ioStGetInitIO
			appIOEnv (ioStSetInitIO (\ps -> f ps >>= \ps -> liftIO disposeFun >> return ps))
			appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1))))


-- The closeAllControls function closes all controls in the indicated window.
closeAllControls :: Id -> GUI ps ()
closeAllControls wId = do
	(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else
		let
			wHs	= windowSystemStateGetWindowHandles wDevice
			(found,wsH,wHs1) = getWindowHandlesWindow (toWID wId) wHs
		in
			if not found then return ()
			else do
				(freeIds,disposeFun,wsH) <- liftIO (closeallcontrols wsH)				
				it <- ioStGetIdTable
				ioStSetIdTable (foldr Map.delete it freeIds)
				f <- accIOEnv ioStGetInitIO
				appIOEnv (ioStSetInitIO (\ps -> f ps >>= \ps -> liftIO disposeFun >> return ps))
				appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs1))))


-- | The closePopUpControlItems function deletes a string with specified index from the list box of the popup control.
closePopUpControlItems :: Id -> [Index] -> GUI ps ()
closePopUpControlItems popUpId [] = return ()
closePopUpControlItems popUpId indexs = do
	maybeId <- getParentWindowId popUpId
	(case maybeId of
		Nothing  -> return ()
		Just wId -> do
			(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
			(if not found then return ()
			 else
				let
					wHs	= windowSystemStateGetWindowHandles wDevice
					(found,wsH,wHs1) = getWindowHandlesWindow (toWID wId) wHs
				in
					if not found
					then return ()
					else do
						wsH <- liftIO (closepopupcontrolitems popUpId indexs wsH)
						appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs1)))))
	where
		closepopupcontrolitems :: Id -> [Index] -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		closepopupcontrolitems popUpId indexs wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
			wH <- closepopupitems popUpId indexs (wPtr wids) wH
			return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
		closepopupcontrolitems _ _ _ = do
			stdControlFatalError "closePopUpControlItems" "unexpected window placeholder argument"
			
-- | The closeListBoxControlItems function deletes a string with specified index from the control.
closeListBoxControlItems :: Id -> [Index] -> GUI ps ()
closeListBoxControlItems lboxId [] = return ()
closeListBoxControlItems lboxId indexs = do
	maybeId <- getParentWindowId lboxId
	(case maybeId of
		Nothing  -> return ()
		Just wId -> do
			(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
			(if not found then return ()
			 else
				let
					wHs	= windowSystemStateGetWindowHandles wDevice
					(found,wsH,wHs1) = getWindowHandlesWindow (toWID wId) wHs
				in
					if not found
					then return ()
					else do
						wsH <- liftIO (closelistboxcontrolitems lboxId indexs wsH)
						appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs1)))))
	where
		closelistboxcontrolitems :: Id -> [Index] -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		closelistboxcontrolitems lboxId indexs wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
			wH <- closelistboxitems lboxId indexs (wPtr wids) wH
			return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
		closelistboxcontrolitems _ _ _ = do
			stdControlFatalError "closeListBoxControlItems" "unexpected window placeholder argument"


-- | The setControlPos function changes the position of the specified control. 
-- It is relative to the upper-left corner of the parent window\'s client area.
setControlPos :: Id -> [(Id,ItemPos)] -> GUI ps Bool
setControlPos wId newPoss = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return False
	 else
		let
			wHs		 = windowSystemStateGetWindowHandles wDevice
			(found,wsH,wHs1) = getWindowHandlesWindow (toWID wId) wHs
		in if not found then return False
		   else do
				wMetrics <- accIOEnv ioStGetOSWindowMetrics
				(ok,wsH) <- liftIO (setcontrolpositions wMetrics newPoss wsH)
				let wHs2 = setWindowHandlesWindow wsH wHs1
				appIOEnv (ioStSetDevice (WindowSystemState wHs2))
				return ok)


--	Show/Hide controls.

-- | displays a given set of controls
showControls :: [Id] -> GUI ps ()
showControls = setControlsShowState True

-- | displays a control in its most recent size and position.
showControl :: Id -> GUI ps ()
showControl id = showControls [id]

-- | hides a given set of controls
hideControls :: [Id] -> GUI ps ()
hideControls = setControlsShowState False

-- | hides the control and activates another control
hideControl :: Id -> GUI ps ()
hideControl id = hideControls [id]

setControlsShowState :: Bool -> [Id] -> GUI ps ()
setControlsShowState show ids = do
	cIds_wIds <- getParentWindowIds id ids
	sequence_ [mapWindow wId (setControlsShowState cIds) () | (cIds,wId)<-gather cIds_wIds]
	where
		setControlsShowState :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		setControlsShowState ids wMetrics wids wH s = do
			wH <- setcontrolsshowstate ids show wMetrics wids wH
			wH <- forceValidWindowClipState wMetrics True (wPtr wids) wH			
			return (wH, s)

--	Enabling/Disabling of controls.

-- | enables set of controls
enableControls :: [Id] -> GUI ps ()
enableControls ids = do
	cIds_wIds <- getParentWindowIds id ids
	sequence_ [mapWindow wId (enableControls cIds) () | (cIds,wId)<-gather cIds_wIds]
	where
		enableControls :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		enableControls ids wMetrics wids wH s = do
			wH <- enablecontrols ids False wMetrics (wPtr wids) wH
			return (wH, s)

-- | The enableControl function enables mouse and keyboard input to the specified control.
-- When input is enabled, the control receives all input.
enableControl :: Id -> GUI ps ()
enableControl id = enableControls [id]

-- | disables set of controls
disableControls :: [Id] -> GUI ps ()
disableControls ids = do
	cIds_wIds <- getParentWindowIds id ids
	sequence_ [mapWindow wId (disableControls cIds) () | (cIds,wId)<-gather cIds_wIds]
	where
		disableControls :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		disableControls ids wMetrics wids wH s = do
			wH <- disablecontrols ids False wMetrics (wPtr wids) wH
			return (wH, s)

-- | The disableControl function disables mouse and keyboard input to the specified control.
-- When input is disabled, the control does not receive input such as mouse clicks and key presses.
disableControl :: Id -> GUI ps ()
disableControl id = disableControls [id]


--	Marking/Unmarking of selection controls.

-- | The items with specified indices becomes set on.
markControlItems :: Id -> [Index] -> GUI ps ()
markControlItems cId indexs = setControlsMarkState Mark cId indexs

-- | The items with specified indices becomes set off.
unmarkControlItems :: Id -> [Index] -> GUI ps ()
unmarkControlItems cId indexs = setControlsMarkState NoMark cId indexs

-- | Sets or resets the mark state of a selection control.
setControlsMarkState :: MarkState -> Id -> [Index] -> GUI ps ()
setControlsMarkState mark cId [] = return ()
setControlsMarkState mark cId indexs = do	
	maybeId <- getParentWindowId cId
	(case maybeId of
		Nothing  -> return ()
		Just wId -> mapWindow wId (setControlsMarkState mark cId indexs) ())
	where
		setControlsMarkState :: MarkState -> Id -> [Index] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		setControlsMarkState mark id indexs wMetrics wids wH s = do
			wH <- setcontrolsmarkstate id mark indexs wMetrics (wPtr wids) wH
			return (wH,s)


-- | The selectControlItem function selects a string in the selection control.
-- Any previous selection in the control is removed.
selectControlItem :: Id -> Index -> GUI ps ()
selectControlItem cId index = do
	maybeId <- getParentWindowId cId
	(case maybeId of
		Nothing  -> return ()
		Just wId -> mapWindow wId (selectControlItem cId index) ())
	where
		selectControlItem :: Id -> Index -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		selectControlItem id index wMetrics wids wH s = do
			wH <- selectcontrolitem id index wMetrics (wPtr wids) wH
			return (wH,s)

-- | moves the 'ViewFrame' of the CompoundControl in the direction of the given vector.
moveControlViewFrame :: Id -> Vector2 -> GUI ps ()
moveControlViewFrame cId v = do
	maybeId <- getParentWindowId cId
	(case maybeId of
		Nothing  -> return ()
		Just wId -> mapWindow wId (moveControlViewFrame cId v) ())
	where
		moveControlViewFrame :: Id -> Vector2 -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		moveControlViewFrame id v wMetrics wids wH s = do
			wH <- movecontrolviewframe id v wMetrics wids wH		
			return (wH, s)

--	Set a new view domain of a CompoundControl.

setControlViewDomain :: Id -> ViewDomain -> GUI ps ()
setControlViewDomain cId newDomain = do
	maybeId <- getParentWindowId cId
	(case maybeId of
		Nothing  -> return ()
		Just wId -> mapWindow wId (setControlViewDomain cId newDomain) ())
	where
		setControlViewDomain :: Id -> ViewDomain -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		setControlViewDomain id newDomain wMetrics wids wH s = do
			wH <- setcontrolviewdomain id newDomain wMetrics wids wH
			return (wH, s)

-- | Sets a new scroll function of the CompoundControl.
setControlScrollFunction :: Id -> Direction -> ScrollFunction -> GUI ps ()
setControlScrollFunction cId direction scrollFun = do
	maybeId <- getParentWindowId cId
	(case maybeId of
		Nothing  -> return ()
		Just wId -> mapWindow wId (setControlScrollFunction cId direction scrollFun) ())
	where
		setControlScrollFunction :: Id -> Direction -> ScrollFunction -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		setControlScrollFunction id direction scrollFun wMetrics wids wH s = do
			wH <- setcontrolscrollfun id direction scrollFun wH
			return (wH, s)


--	Change the text of (Text/Edit/Button)Control.

-- | Works like 'setControlText' but for set of controls
setControlTexts :: [(Id,String)] -> GUI ps ()
setControlTexts cid_texts = do
	cid_texts_wIds <- getParentWindowIds fst cid_texts
	sequence_ [mapWindow wId (setControlTexts' cid_texts) () | (cid_texts,wId)<-gather cid_texts_wIds]
	where
		setControlTexts' :: [(Id,String)] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> s -> IO (WindowHandle ls ps, s)
		setControlTexts' texts wMetrics wids wH s = do
			wH1 <- setcontroltexts texts wMetrics (wPtr wids) wH
			return (wH1, s)

-- | If the specified Id is an Id of Text, Edit of Button control, the text (caption) of the control is changed.
setControlText :: Id -> String -> GUI ps ()
setControlText id text = setControlTexts [(id,text)]


-- | Set the cursor position of an EditControl.
setEditControlCursor :: Id -> Int -> GUI ps ()
setEditControlCursor cId pos = do
	maybeParent <- getParentWindowId cId
	(case maybeParent of
		Nothing  -> return ()
		Just wId -> mapWindow wId (setEditControlCursor cId pos) ())
	where
		setEditControlCursor :: Id -> Int -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> s -> IO (WindowHandle ls ps, s)
		setEditControlCursor id pos wMetrics wids wH s = do
			wH <- seteditcontrolcursor id pos wMetrics (wPtr wids) wH
			return (wH, s)


-- | change the 'Look' of the corresponding set of controls but redraw only if the first boolean parameter is 'True'
setControlLooks :: [(Id,Bool,(Bool,Look))] -> GUI ps ()
setControlLooks cid_looks = do		
	cid_looks_wIds <- getParentWindowIds (\(x,y,z) -> x) cid_looks
	sequence_ [mapWindow wId (setControlLooks cid_looks) () |	(cid_looks,wId)<-gather cid_looks_wIds]
	where
		setControlLooks :: [(Id,Bool,(Bool,Look))] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> s -> IO (WindowHandle ls ps, s)
		setControlLooks looks wMetrics wids wH s = do
			wH <- setcontrolslook looks wMetrics (wPtr wids) wH
			return (wH, s)

-- | change the 'Look' of the corresponding control but redraw only if the first boolean parameter is 'True'
setControlLook :: Id -> Bool -> (Bool,Look) -> GUI ps ()
setControlLook id redraw newlook = setControlLooks [(id,redraw,newlook)]

-- | changes the 'SliderState' and redraws the settings of the SliderControls.
setSliderStates :: [(Id,IdFun SliderState)] -> GUI ps ()
setSliderStates cid_fs = do		
	cid_fs_wIds <- getParentWindowIds fst cid_fs
	sequence_ [mapWindow wId (setSliderStates cid_fs) () | (cid_fs,wId)<-gather cid_fs_wIds]
	where
		setSliderStates :: [(Id,IdFun SliderState)] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> s -> IO (WindowHandle ls ps, s)
		setSliderStates id_fs wMetrics wids wH s = do
			wH <- setsliderstates id_fs wMetrics (wPtr wids) wH
			return (wH, s)

-- | changes the 'SliderState' and redraws the settings of the SliderControls.
setSliderState :: Id -> IdFun SliderState -> GUI ps ()
setSliderState id fun = setSliderStates [(id,fun)]


-- | changes the thumb value of the 'SliderState' for each SliderControl from the given set.
setSliderThumbs :: [(Id,Int)] -> GUI ps ()
setSliderThumbs cid_thumbs =
	setSliderStates (map (\(cid,thumb)->(cid,\state->state{sliderThumb=thumb})) cid_thumbs)

-- | changes the thumb value of the 'SliderState' of SliderControl.
setSliderThumb :: Id -> Int -> GUI ps ()
setSliderThumb id thumb = setSliderThumbs [(id,thumb)]


--	Draw in a {Custom(Button)|Compound}Control.

-- | direct draw in control
drawInControl :: Id -> Draw x -> GUI ps (Maybe x)
drawInControl cId drawfun = do
	maybeParent <- getParentWindowId cId
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(case maybeParent of		
		Just wId | found ->
			let
				windows 	= windowSystemStateGetWindowHandles wDevice
				(_,wsH,windows1)= getWindowHandlesWindow (toWID wId) windows
				wids 		= getWindowStateHandleWIDS wsH
			in do
				wMetrics <- accIOEnv ioStGetOSWindowMetrics
				(maybe_result,wsH) <- liftIO (drawInControl cId drawfun wMetrics (wPtr wids) wsH)
				let windows2 = setWindowHandlesWindow wsH windows1
				appIOEnv (ioStSetDevice (WindowSystemState windows2))
				return maybe_result
		_  -> return Nothing)
	where
		drawInControl :: Id -> Draw x -> OSWindowMetrics -> OSWindowPtr -> WindowStateHandle ps -> IO (Maybe x,WindowStateHandle ps)
		drawInControl controlId drawfun wMetrics wPtr (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
			(maybe_result,wH) <- drawincontrol controlId drawfun wMetrics wPtr wH
			return (maybe_result,WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
		drawInControl _ _ _ _ _ = stdControlFatalError "drawInControl" "unexpected window placeholder argument"

--	Update a selection of a (Compound/Custom(Button))Control:

-- | The updateControl function updates the view frame of the specified control.
updateControl :: Id -> Maybe ViewFrame -> GUI ps ()
updateControl cId maybeViewFrame = do
	maybeParent <- getParentWindowId cId
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(case maybeParent of
		Just wId | found ->
			let
				windows 	= windowSystemStateGetWindowHandles wDevice
				(_,wsH,windows1)= getWindowHandlesWindow (toWID wId) windows
				wKind 		= getWindowStateHandleWindowKind wsH
			in
				if wKind /= IsWindow
				then appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
				else do
					wMetrics <- accIOEnv ioStGetOSWindowMetrics
					wsH <- liftIO (updateControlBackground wMetrics wKind cId maybeViewFrame wsH)
					appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
		Nothing -> return ())
	where
		updateControlBackground :: OSWindowMetrics -> WindowKind -> Id -> Maybe ViewFrame -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		updateControlBackground wMetrics wKind cId maybeViewFrame wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH@(WindowHandle {whSize=whSize,whItems=itemHs})}))) = do
			let (Just updInfo) = getWElementHandlesUpdateInfo wMetrics cId contentRect itemHs
			wH <- updateWindow wMetrics updInfo wH
			return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
			where
				info = whWindowInfo wH
				(domainRect,hasScrolls)	= case wKind of
					IsWindow -> (windowDomain info,(isJust (windowHScroll info),isJust (windowVScroll info)))
					_        -> (sizeToRect whSize,(False,False))
				visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
				contentRect	= getWindowContentRect wMetrics visScrolls (sizeToRect whSize)

				getWElementHandlesUpdateInfo :: OSWindowMetrics -> Id -> Rect -> [WElementHandle ls ps] -> Maybe UpdateInfo
				getWElementHandlesUpdateInfo wMetrics cId clipRect [] = Nothing
				getWElementHandlesUpdateInfo wMetrics cId clipRect (itemH:itemHs) =
					let res = getWElementHandleUpdateInfo wMetrics cId clipRect itemH
					in  if isJust res then res
					    else getWElementHandlesUpdateInfo wMetrics cId clipRect itemHs
					where
						getWElementHandleUpdateInfo :: OSWindowMetrics -> Id -> Rect -> WElementHandle ls ps -> Maybe UpdateInfo
						getWElementHandleUpdateInfo wMetrics cId clipRect itemH@(WItemHandle {wItemKind=itemKind,wItemPos=itemPos,wItemSize=itemSize})
							| wItemId itemH /= Just cId =
								if not (isRecursiveControl itemKind) then Nothing
								else getWElementHandlesUpdateInfo wMetrics cId visRect (wItems itemH)
							| itemKind `elem` [IsCompoundControl,IsCustomControl,IsCustomButtonControl] = Just updInfo
							| otherwise = Nothing
							where
								itemRect	= posSizeToRect itemPos itemSize
								itemInfo	= wItemInfo itemH
								compoundInfo	= getWItemCompoundInfo itemInfo
								origin		= if itemKind==IsCompoundControl
										  then compoundOrigin compoundInfo
										  else zero
								domain		= compoundDomain compoundInfo
								hasScrolls	= (isJust (compoundHScroll compoundInfo),isJust (compoundVScroll compoundInfo))
								visScrolls	= osScrollbarsAreVisible wMetrics domain (toTuple itemSize) hasScrolls
								contentRect	= if itemKind==IsCompoundControl
										  then getCompoundContentRect wMetrics visScrolls itemRect
										  else itemRect
								visRect		= intersectRects contentRect clipRect
								updArea		= case maybeViewFrame of
									Nothing	  -> visRect
									Just rect -> intersectRects (rectangleToRect (addVector (toVector itemPos)
																(subVector (toVector origin) rect)
														     )
												    ) visRect
								updInfo	= UpdateInfo
									{ updWIDS 	= wids
									, updWindowArea	= zero
									, updControls	= [ControlUpdateInfo
												{ cuItemNr	= wItemNr itemH
												, cuItemPtr	= wItemPtr itemH
												, cuArea		= updArea
												}]
									, updGContext	= Nothing
									}
						getWElementHandleUpdateInfo wMetrics cId clipRect (WListLSHandle itemHs) =
							getWElementHandlesUpdateInfo wMetrics cId clipRect itemHs

						getWElementHandleUpdateInfo wMetrics cId clipRect (WExtendLSHandle exLS itemHs) =
							getWElementHandlesUpdateInfo wMetrics cId clipRect itemHs

						getWElementHandleUpdateInfo wMetrics cId clipRect (WChangeLSHandle chLS itemHs) =
							getWElementHandlesUpdateInfo wMetrics cId clipRect itemHs

		updateControlBackground _ _ _ _ _ = stdControlFatalError "updateControl" "unexpected window placeholder argument"


getControlLayouts :: [Id] -> GUI ps [(Bool,(Maybe ItemPos,Vector2))]
getControlLayouts ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolslayouts ids)) [(id,False,(Nothing,zero)) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getControlLayout :: Id -> GUI ps (Bool,(Maybe ItemPos,Vector2))
getControlLayout id = fmap head (getControlLayouts [id])

-- | returns a list of control dimensions from list of controls ids
getControlViewSizes :: [Id] -> GUI ps [(Bool,Size)]
getControlViewSizes ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsviewsizes ids)) [(id,False,zero) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

-- | The getControlViewSize function retrieves the dimensions of a control\'s client area
getControlViewSize :: Id -> GUI ps (Bool,Size)
getControlViewSize id = fmap head (getControlViewSizes [id])

-- | returns a list of control outer dimensions from list of controls ids
getControlOuterSizes :: [Id] -> GUI ps [(Bool,Size)]
getControlOuterSizes ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsoutersizes ids)) [(id,False,zero) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)	

-- | The getControlOuterSize function retrieves the dimensions of the bounding rectangle of the specified control. 
getControlOuterSize :: Id -> GUI ps (Bool,Size)
getControlOuterSize id = fmap head (getControlOuterSizes [id])

-- | returns a list of select state from list of controls ids
getControlSelectStates :: [Id] -> GUI ps [(Bool,SelectState)]
getControlSelectStates ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsselects ids)) [(id,False,Able) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

-- | returns current select state of given control
getControlSelectState :: Id -> GUI ps (Bool,SelectState)
getControlSelectState id = fmap head (getControlSelectStates [id])

-- | returns a list of currents show states from list of control ids
getControlShowStates :: [Id] -> GUI ps [(Bool,Bool)]
getControlShowStates ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsshowstates ids)) [(id,False,False) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

-- | returns current show state of the control
getControlShowState :: Id -> GUI ps (Bool,Bool)
getControlShowState id = fmap head (getControlShowStates [id])

--	Access operations on Windows:

-- | Works like getControlText but for set of controls.
getControlTexts :: [Id] -> GUI ps [(Bool,Maybe String)]
getControlTexts ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolstexts ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

-- | If the specified Id is an Id of Text, Edit or Button control,
-- the text (caption) of the control is returned. 
getControlText :: Id -> GUI ps (Bool,Maybe String)
getControlText id = fmap head (getControlTexts [id])

-- | The getControlNrLines function returns the number of lines for each EditControl from specified list of ids.
getControlNrLines :: [Id] -> GUI ps [(Bool,Maybe NrLines)]
getControlNrLines ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsnrlines ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

-- | The getControlNrLine function returns the number of lines that can be seen
-- (on the screen at the moment) for a given control.
getControlNrLine :: Id -> GUI ps (Bool,Maybe NrLines)
getControlNrLine id = fmap head (getControlNrLines [id])

-- | returns current controls 'Look'
getControlLooks :: [Id] -> GUI ps [(Bool,Maybe (Bool,Look))]
getControlLooks ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolslooks ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

-- | returns current controls 'Look'
getControlLook :: Id -> GUI ps (Bool,Maybe (Bool,Look))
getControlLook id = fmap head (getControlLooks [id])

-- | returns the minimal valid size when resizing
getControlMinimumSizes :: [Id] -> GUI ps [(Bool,Maybe Size)]
getControlMinimumSizes ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsminsizes ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

-- | returns the minimal valid size when resizing
getControlMinimumSize :: Id -> GUI ps (Bool,Maybe Size)
getControlMinimumSize id = fmap head (getControlMinimumSizes [id])

-- | Works like 'getControlResizes' but for set of controls
getControlResizes :: [Id] -> GUI ps [(Bool,Maybe ControlResizeFunction)]
getControlResizes ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsresizes ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

-- | returns the control resizing function. When the parent window of a given control is resized,
-- then the control can be resized if it has a resize function.
getControlResize :: Id -> GUI ps (Bool,Maybe ControlResizeFunction)
getControlResize id = fmap head (getControlResizes [id])

-- | Call this member function to determine which item is selected, for each selection control in the list.
getControlSelections :: [Id] -> GUI ps [(Bool,Maybe Index)]
getControlSelections ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsselections ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)
	
-- | Call this member function to determine which item in the selection control is selected.
getControlSelection :: Id -> GUI ps (Bool,Maybe Index)
getControlSelection id = fmap head (getControlSelections [id])

-- | The getControlItems function returns the list of control items.
getControlItems :: [Id] -> GUI ps [(Bool,Maybe [String])]
getControlItems ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolitems ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)
	
-- | returns the list of items for a given control
getControlItem :: Id -> GUI ps (Bool,Maybe [String])
getControlItem id = fmap head (getControlItems [id])

-- | Works like 'getControlMark' but for set of selection controls
getControlMarks :: [Id] -> GUI ps [(Bool,Maybe [Index])]
getControlMarks ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsmarks ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

-- | returns a the list of indices which indicates currently selected items
getControlMark :: Id -> GUI ps (Bool,Maybe [Index])
getControlMark id = fmap head (getControlMarks [id])

-- | gets the sliders directions.
getSliderDirections :: [Id] -> GUI ps [(Bool,Maybe Direction)]
getSliderDirections ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getslidersdirections ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

-- | gets the slider direction i. e. 'Horizontal' or 'Vertical'.
getSliderDirection :: Id -> GUI ps (Bool,Maybe Direction)
getSliderDirection id = fmap head (getSliderDirections [id])

-- | gets a current 'SliderState's
getSliderStates :: [Id] -> GUI ps [(Bool,Maybe SliderState)]
getSliderStates ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getslidersstates ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

-- | gets a current 'SliderState'
getSliderState :: Id -> GUI ps (Bool,Maybe SliderState)
getSliderState id = fmap head (getSliderStates [id])

-- | returns a list of 'ViewFrame's from list of control ids
getControlViewFrames :: [Id] -> GUI ps [(Bool,Maybe ViewFrame)]
getControlViewFrames ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsframes ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

-- | If the specified Id is an Id of Compound control, the current 'ViewFrame' of the control is returned.
getControlViewFrame :: Id -> GUI ps (Bool,Maybe ViewFrame)
getControlViewFrame id = fmap head (getControlViewFrames [id])

getControlViewDomains :: [Id] -> GUI ps [(Bool,Maybe ViewDomain)]
getControlViewDomains ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsdomains ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getControlViewDomain :: Id -> GUI ps (Bool,Maybe ViewDomain)
getControlViewDomain id = fmap head (getControlViewDomains [id])

-- | Works like 'setControlText' but for set of controls
getControlScrollFunctions :: [Id] -> GUI ps [(Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))]
getControlScrollFunctions ids  = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getscrollfunctions ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

-- | getControlScrollFunction(s) yields the 'ScrollFunction's of the indicated CompoundControl.
-- If the given control is not a CompoundControl, then Nothing is returned.
getControlScrollFunction :: Id -> GUI ps (Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))
getControlScrollFunction id = fmap head (getControlScrollFunctions [id])
