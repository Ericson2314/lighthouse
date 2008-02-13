-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Window.Update
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Window.Update contains the window update functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Window.Update
		( updateWindow, updateWindowBackgrounds
		, updateRectControls, updateWindowBackground
		, updateControls
		) where



import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.Device.Events
import	Graphics.UI.ObjectIO.Window.Handle
import	Graphics.UI.ObjectIO.Window.Access(getWItemCompoundInfo, getWItemCustomButtonInfo, getWItemCustomInfo,
				  	   getCompoundContentRect, getWindowContentRect, getCompoundHScrollRect, getCompoundVScrollRect)
import	Graphics.UI.ObjectIO.Window.ClipState(validateWindowClipState)
import  Graphics.UI.ObjectIO.StdPicture(toRegion, accClipPicture)
import  Graphics.UI.ObjectIO.StdControlAttribute(isControlDoubleBuffered)
import  Graphics.UI.ObjectIO.StdWindowAttribute(isWindowDoubleBuffered)
import	Graphics.UI.ObjectIO.OS.Picture
import	Graphics.UI.ObjectIO.OS.Rgn
import	Graphics.UI.ObjectIO.OS.Window
import  Control.Monad(when)
import  Foreign.Ptr(nullPtr)

windowupdateFatalError :: String -> String -> x
windowupdateFatalError rule error = dumpFatalError rule "windowupdate" error


--	updateWindow updates the window, using the UpdateInfo argument. 

updateWindow :: OSWindowMetrics -> UpdateInfo -> WindowHandle ls ps -> IO (WindowHandle ls ps)
updateWindow wMetrics info@(UpdateInfo {updWIDS=WIDS{wPtr=wPtr},updGContext=updGContext}) wH = do
	wH1 <- validateWindowClipState wMetrics False wPtr wH
	osPict <- getUpdateContext wPtr updGContext
	wH2 <- updateWindowBackground wMetrics False Nothing info wH1 osPict
	wH3 <- updateControls wMetrics info wH2 osPict
	setUpdateContext wPtr updGContext osPict
	return wH3
	where
	    getUpdateContext :: OSWindowPtr -> Maybe OSPictContext -> IO OSPictContext
	    getUpdateContext _ (Just osPict) = return osPict
	    getUpdateContext wPtr _ = osGrabWindowPictContext wPtr
	
	    setUpdateContext :: OSWindowPtr -> Maybe OSPictContext -> OSPictContext -> IO ()
	    setUpdateContext wPtr updContext osPict
		| isJust updContext = return ()
		| otherwise	    = osReleaseWindowPictContext wPtr osPict


{-	updateWindowBackgrounds redraws the (window/compound) backgrounds that are inside the OSRgnHandle argument.
	After redrawing the OSRgnHandle argument is disposed!
-}
updateWindowBackgrounds :: OSWindowMetrics -> OSRgnHandle -> WIDS -> WindowHandle ls ps -> IO (WindowHandle ls ps)
updateWindowBackgrounds wMetrics backgrRgn wids@(WIDS {wPtr=wPtr}) wH@(WindowHandle {whItems=whItems,whSelect=whSelect,whSize=whSize,whWindowInfo=whWindowInfo}) = do
    (_,backgrRect) <- osGetRgnBox backgrRgn
    (if isEmptyRect backgrRect then osDisposeRgn backgrRgn >> return wH
     else do
	let updInfo = UpdateInfo
	      { updWIDS       = wids
	      , updWindowArea	= backgrRect
	      , updControls	= []
	      , updGContext	= Nothing
	      }
	osPict <- osGrabWindowPictContext wPtr	    
	wH1 <- updateWindowBackground wMetrics True (Just backgrRgn) updInfo wH osPict
	backgrRgn1 <- updateControlBackgrounds wMetrics backgrRgn wH1 osPict
	osReleaseWindowPictContext wPtr osPict
	osDisposeRgn backgrRgn1
	return wH1)
    where
	pen = case whWindowInfo of
		WindowInfo{windowLook=look} -> lookPen look
		NoWindowInfo 		    -> defaultPen
	
	updateControlBackgrounds :: OSWindowMetrics -> OSRgnHandle -> WindowHandle ls ps -> OSPictContext -> IO OSRgnHandle
	updateControlBackgrounds wMetrics backgrRgn wH@(WindowHandle {whItems=whItems,whSelect=whSelect,whSize=whSize}) osPict = do
	    updateBackgrounds wMetrics (sizeToRect whSize) whSelect backgrRgn whItems osPict
	    where
		updateBackgrounds :: OSWindowMetrics -> Rect -> Bool -> OSRgnHandle -> [WElementHandle ls ps] -> OSPictContext -> IO OSRgnHandle
		updateBackgrounds wMetrics wFrame ableContext backgrRgn itemHs osPict = do
		    empty <- osIsEmptyRgn backgrRgn
		    (if empty || null itemHs then return backgrRgn
		     else do
			backgrRgn <- updateBackground  wMetrics wFrame ableContext backgrRgn (head itemHs) osPict
			backgrRgn <- updateBackgrounds wMetrics wFrame ableContext backgrRgn (tail itemHs) osPict
			return backgrRgn)
		    where
			updateBackground :: OSWindowMetrics -> Rect -> Bool -> OSRgnHandle -> WElementHandle ls ps -> OSPictContext -> IO OSRgnHandle
			updateBackground wMetrics wFrame ableContext backgrRgn itemH@(WItemHandle {wItemShow=wItemShow,wItemVirtual=wItemVirtual,wItemKind=wItemKind,wItemPos=wItemPos,wItemSize=wItemSize}) osPict
			    | not wItemShow || wItemVirtual || not (isRecursiveControl wItemKind) =	-- PA: isRecursive includes also LayoutControls
				    return backgrRgn
			    | otherwise = do
				    (_, backgrRect) <- osGetRgnBox backgrRgn
				    let compoundRect = posSizeToRect wItemPos wItemSize
				    (if disjointRects backgrRect compoundRect then
				       return backgrRgn
				     else updateCompoundBackground wMetrics wFrame compoundRect ableContext backgrRgn itemH osPict)
			    where
				updateCompoundBackground :: OSWindowMetrics -> Rect -> Rect -> Bool -> OSRgnHandle -> WElementHandle ls ps -> OSPictContext -> IO OSRgnHandle
				updateCompoundBackground wMetrics wFrame compoundRect ableContext backgrRgn itemH@(WItemHandle {wItemKind=IsLayoutControl,wItems=wItems}) osPict = do
				    backgrRgn <- updateBackgrounds wMetrics cFrame cAble backgrRgn wItems osPict
				    rectRgn	<- osNewRectRgn cFrame
				    diffRgn <- osDiffRgn backgrRgn rectRgn
				    mapM_ osDisposeRgn [rectRgn,backgrRgn]
				    return diffRgn
				    where
					cFrame = intersectRects wFrame compoundRect
					cAble  = ableContext && wItemSelect itemH
				updateCompoundBackground wMetrics wFrame compoundRect ableContext backgrRgn itemH@(WItemHandle {wItemKind=IsCompoundControl,wItems=wItems,wItemPos=itemPos,wItemSize=itemSize,wItemAtts=attrs}) osPict = do
				    updRgn <- osSectRgn backgrRgn (clipRgn clipInfo)
				    empty <- osIsEmptyRgn updRgn
				    (if empty then do
					backgrRgn <- updateBackgrounds wMetrics cFrame cAble backgrRgn wItems osPict
					rectRgn <- osNewRectRgn cFrame
					diffRgn <- osDiffRgn backgrRgn rectRgn
					mapM_ osDisposeRgn [rectRgn,backgrRgn,updRgn]
					return diffRgn
				     else do				     
				     	contentRgn <- osNewRectRgn contentRect
				     	clipRgn <- osSectRgn contentRgn updRgn
				     	osDisposeRgn contentRgn
					(x,_,_,_) <- doDraw (origin-itemPos) lookPen True clipRgn osPict (any isControlDoubleBuffered attrs) (lookFun (if cAble then Able else Unable) updState)
					osDisposeRgn clipRgn
					backgrRgn <- updateBackgrounds wMetrics cFrame cAble backgrRgn wItems osPict
					rectRgn <- osNewRectRgn cFrame
					diffRgn <- osDiffRgn backgrRgn rectRgn
					mapM_ osDisposeRgn [rectRgn,backgrRgn,updRgn]
					return diffRgn)
				    where				
					info		= getWItemCompoundInfo (wItemInfo itemH)
					compLookInfo	= compoundLookInfo info
					LookInfo{lookFun=lookFun,lookPen=lookPen} = compoundLook compLookInfo
					clipInfo	= compoundClip compLookInfo
					origin		= compoundOrigin info
					domainRect	= compoundDomain info
					hasScrolls	= (isJust (compoundHScroll info),isJust (compoundVScroll info))
					visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
					contentRect	= getCompoundContentRect wMetrics visScrolls compoundRect
					cFrame		= intersectRects wFrame contentRect
					cAble		= ableContext && wItemSelect itemH
					updFrame	= rectToRectangle cFrame
					updState	= UpdateState{oldFrame=updFrame,newFrame=updFrame,updArea=[updFrame]}

			        updateCompoundBackground _ _ _ _ _ WItemHandle {wItemKind=wItemKind} _ =
				    windowupdateFatalError "updateCompoundBackground (updateWindowBackgrounds)" ("unexpected control kind: " ++ show wItemKind)
			
			updateBackground wMetrics wFrame ableContext backgrRgn (WListLSHandle itemHs) osPict = do
				updateBackgrounds wMetrics wFrame ableContext backgrRgn itemHs osPict				
			
			updateBackground wMetrics wFrame ableContext backgrRgn (WExtendLSHandle exLS itemHs) osPict = do
				updateBackgrounds wMetrics wFrame ableContext backgrRgn itemHs osPict				
			
			updateBackground wMetrics wFrame ableContext backgrRgn (WChangeLSHandle chLS itemHs) osPict = do
				updateBackgrounds wMetrics wFrame ableContext backgrRgn itemHs osPict				


{-	updateRectControls updates the controls that fit in the Rect argument of the indicated window or compound control. 
	The Rect is in window/compound coordinates. 
-}
updateRectControls :: OSWindowMetrics -> Rect -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
updateRectControls wMetrics area wPtr wH@(WindowHandle {whItems=whItems,whSelect=whSelect})
    | isEmptyRect area = return wH
    | otherwise = do
	itemHs <- updateControlsInRect wMetrics wPtr whSelect area whItems
	return (wH{whItems=itemHs})
    where
	updateControlsInRect :: OSWindowMetrics -> OSWindowPtr -> Bool -> Rect -> [WElementHandle ls ps] -> IO [WElementHandle ls ps]
	updateControlsInRect wMetrics parentPtr ableContext area itemHs
	    | isEmptyRect area || null itemHs = return itemHs
	    | otherwise = do
		itemH  <- updateControlInRect  wMetrics parentPtr ableContext area (head itemHs)
		itemHs <- updateControlsInRect wMetrics parentPtr ableContext area (tail itemHs)
		return (itemH:itemHs)
	    where
		updateControlInRect :: OSWindowMetrics -> OSWindowPtr -> Bool -> Rect -> WElementHandle ls ps -> IO (WElementHandle ls ps)
		updateControlInRect wMetrics parentPtr ableContext area itemH@(WItemHandle {wItemKind=wItemKind,wItemPtr=wItemPtr,wItemPos=wItemPos,wItemSize=wItemSize,wItemShow=wItemShow,wItemVirtual=wItemVirtual})
		    | not wItemShow || wItemVirtual || isEmptyRect intersectRect || wItemKind == IsReceiverControl =
			    return itemH
		    | isCustomControl = updateCustomControl wMetrics parentPtr ableContext intersectRect itemH
		    | Just updateOSControl <- mb_updateOSControl = do
		            updateOSControl (subVector (toVector wItemPos) intersectRect) parentPtr wItemPtr
		            return itemH
		    | isRecursiveControl wItemKind	= do -- This includes LayoutControl and excludes CompoundControl which is already guarded as iscustomcontrol
			    let ableContext1 = ableContext && wItemSelect itemH
			    itemHs <- updateControlsInRect wMetrics parentPtr ableContext1 area (wItems itemH) -- PA: shouldn't area be clipped (also below at updatecustomcontrol?)
			    return (itemH{wItems=itemHs})
		    | otherwise	-- This alternative should never be reached
			    = windowupdateFatalError "updateControlInRect" "unexpected ControlKind"
		    where
			controlRect		= posSizeToRect wItemPos wItemSize
			intersectRect		= intersectRects area controlRect
			isCustomControl		= case wItemKind of
				IsCustomButtonControl	-> True
				IsCustomControl		-> True
				IsCompoundControl	-> True
				_			-> False			
			mb_updateOSControl = case wItemKind of
				IsRadioControl		-> Just osUpdateRadioControl
				IsCheckControl		-> Just osUpdateCheckControl
				IsPopUpControl		-> Just osUpdatePopUpControl
				IsListBoxControl	-> Just osUpdateListBoxControl
				IsSliderControl		-> Just osUpdateSliderControl
				IsTextControl		-> Just osUpdateTextControl
				IsEditControl		-> Just osUpdateEditControl
				IsButtonControl		-> Just osUpdateButtonControl
				_			-> Nothing
			
	--		updatecustomcontrol updates a ((Custom)Button/Compound)Control.
			updateCustomControl :: OSWindowMetrics -> OSWindowPtr -> Bool -> Rect -> WElementHandle ls ps -> IO (WElementHandle ls ps)
			updateCustomControl _ parentPtr contextAble area itemH@(WItemHandle {wItemKind=IsCustomButtonControl, wItemSize=wItemSize, wItemPos=wItemPos, wItemPtr=itemPtr, wItemAtts=attrs}) = do
			    osPict <- osGrabControlPictContext parentPtr itemPtr
			    (_,_,pen,_) <- doDraw zero (lookPen lookInfo) True osNoRgn osPict (any isControlDoubleBuffered attrs) (accClipPicture (toRegion updArea) (lookFun lookInfo selectState updState))
			    let info1 = info{cButtonInfoLook=lookInfo{lookPen=pen}}
			    osReleaseControlPictContext itemPtr osPict	
			    osValidateWindowRect itemPtr areaLocal
			    return (itemH{wItemInfo=WCustomButtonInfo info1})
			    where
				selectState	= if (contextAble && wItemSelect itemH) then Able else Unable
				info		= getWItemCustomButtonInfo (wItemInfo itemH)
				lookInfo	= cButtonInfoLook info
				areaLocal	= subVector (toVector wItemPos) area
				cFrame		= sizeToRectangle wItemSize
				updArea		= rectToRectangle areaLocal
				updState	= UpdateState{oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
			
			updateCustomControl _ parentPtr contextAble area itemH@(WItemHandle {wItemKind=IsCustomControl,wItemSize=wItemSize,wItemPtr=itemPtr,wItemPos=itemPos,wItemAtts=attrs}) = do
			    osPict <- osGrabControlPictContext parentPtr itemPtr			    
			    (_,_,pen,_) <- doDraw zero (lookPen lookInfo) True osNoRgn osPict (any isControlDoubleBuffered attrs) (accClipPicture (toRegion updArea) (lookFun lookInfo selectState updState))
			    let info1 = info{customInfoLook=lookInfo{lookPen=pen}}
			    osReleaseControlPictContext itemPtr osPict
			    osValidateWindowRect itemPtr areaLocal
			    return (itemH{wItemInfo=WCustomInfo info1})
			    where
				selectState	= if (contextAble && wItemSelect itemH) then Able else Unable
				info		= getWItemCustomInfo (wItemInfo itemH)
				lookInfo	= customInfoLook info
				areaLocal	= subVector (toVector itemPos) area
				cFrame		= sizeToRectangle wItemSize
				updArea		= rectToRectangle areaLocal
				updState	= UpdateState{oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
			
			updateCustomControl wMetrics parentPtr contextAble area itemH@(WItemHandle {wItemKind=IsCompoundControl,wItems=wItems,wItemSize=itemSize,wItemPtr=itemPtr,wItemPos=itemPos,wItemAtts=attrs}) = do
			    osPict <- osGrabControlPictContext parentPtr itemPtr			    
			    (_,_,pen,_) <- doDraw origin (lookPen lookInfo) True osNoRgn osPict (any isControlDoubleBuffered attrs) (accClipPicture (toRegion updArea) (lookFun lookInfo selectState updState))
			    let info1 = info{compoundLookInfo=compLookInfo{compoundLook=lookInfo{lookPen=pen}}}
			    osReleaseControlPictContext itemPtr osPict
			    updateScrollAreas
			    itemHs <- updateControlsInRect wMetrics parentPtr compoundAble area wItems
			    osValidateWindowRect itemPtr areaLocal
			    return (itemH{wItemInfo=WCompoundInfo info1,wItems=itemHs})
			    where				
				compoundAble	= contextAble && wItemSelect itemH
				selectState	= if compoundAble then Able else Unable
				info		= getWItemCompoundInfo (wItemInfo itemH)
				compLookInfo	= compoundLookInfo info
				lookInfo	= compoundLook compLookInfo
				(origin,domainRect,hasScrolls) = (compoundOrigin info,compoundDomain info,(isJust (compoundHScroll info),isJust (compoundVScroll info)))
				visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
				contentRect	= getCompoundContentRect wMetrics visScrolls (posSizeToRect origin itemSize)
				areaLocal	= subVector (toVector itemPos) area
				cFrame		= rectToRectangle contentRect
				updArea		= rectToRectangle (intersectRects contentRect (subVector (toVector (itemPos-origin)) area))
				updState	= UpdateState {oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
				
				updateScrollAreas = if emptyH && emptyV then return () else
						    (if emptyH	then (osUpdateCompoundControl updVRect parentPtr itemPtr) else
						     (if emptyV	then (osUpdateCompoundControl updHRect parentPtr itemPtr) else
						      ((osUpdateCompoundControl (updVRect{rbottom=rbottom updHRect}) parentPtr itemPtr) >>
						       (osUpdateCompoundControl updHRect parentPtr itemPtr))))
				itemRect	= posSizeToRect itemPos itemSize
				hRect		= getCompoundHScrollRect wMetrics hasScrolls itemRect
				vRect		= getCompoundVScrollRect wMetrics hasScrolls itemRect
				updHRect	= intersectRects hRect area
				updVRect	= intersectRects vRect area
				emptyH		= isEmptyRect updHRect
				emptyV		= isEmptyRect updVRect
			
			updateCustomControl _ _ _ _ (WItemHandle {wItemKind=wItemKind}) =
				windowupdateFatalError "updateCustomControl" ("unexpected ControlKind: " ++ show wItemKind)
		
		updateControlInRect wMetrics parentPtr ableContext area (WListLSHandle itemHs) = do
		    itemHs <- updateControlsInRect wMetrics parentPtr ableContext area itemHs
		    return (WListLSHandle itemHs)
		
		updateControlInRect wMetrics parentPtr ableContext area (WExtendLSHandle exLS itemHs) = do
		    itemHs <- updateControlsInRect wMetrics parentPtr ableContext area itemHs
		    return (WExtendLSHandle exLS itemHs)
		
		updateControlInRect wMetrics parentPtr ableContext area (WChangeLSHandle chLS itemHs) = do
		    itemHs <- updateControlsInRect wMetrics parentPtr ableContext area itemHs
		    return (WChangeLSHandle chLS itemHs)
			

{-	updateBackground updates the background of the window.
-}
updateWindowBackground :: OSWindowMetrics -> Bool -> Maybe OSRgnHandle -> UpdateInfo -> WindowHandle ls ps -> OSPictContext -> IO (WindowHandle ls ps)
updateWindowBackground wMetrics pictureInitialised alsoClipRgn info@(UpdateInfo {updWIDS=WIDS{wPtr=wPtr}}) wH@(WindowHandle {whKind=whKind,whWindowInfo=windowInfo,whSize=whSize,whAtts=attrs}) osPict
    | isEmptyRect updRect || whKind /= IsWindow =		-- Nothing to update at all
	    return wH
    | isEmptyRect updAreaRect =					-- Nothing to update inside viewframe
	    return wH
    | otherwise = do	    
	    let drawf = do
		maybe (return ()) pictAndClipRgn alsoClipRgn
		setPictOrigin origin
		accClipPicture (toRegion updArea) (lookFun lookInfo selectState updState)
	    (_,_,pen,_) <- doDraw zero initPen True (clipRgn clipState) osPict (any isWindowDoubleBuffered attrs) drawf
	    return (wH{whWindowInfo=windowInfo{windowLook=lookInfo{lookPen=pen}}})
    where
	updRect				= updWindowArea info	
	(origin,domainRect,hasScrolls)	= (windowOrigin windowInfo,windowDomain windowInfo,(isJust (windowHScroll windowInfo),isJust (windowVScroll windowInfo)))
	visScrolls			= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
	wFrameRect			= posSizeToRect origin (rectSize (getWindowContentRect wMetrics visScrolls (sizeToRect whSize)))
	wFrame				= rectToRectangle wFrameRect
	updAreaRect			= intersectRects wFrameRect (addVector (toVector origin) updRect)
	updArea				= rectToRectangle updAreaRect
	updState			= UpdateState{oldFrame=wFrame,newFrame=wFrame,updArea=[updArea]}
	lookInfo			= windowLook windowInfo
	clipState			= windowClip windowInfo
	selectState			= if whSelect wH then Able else Unable
	initPen				= lookPen lookInfo


-- 	updatecontrols updates the controls that are registered for update in UpdateInfo.

updateControls :: OSWindowMetrics -> UpdateInfo -> WindowHandle ls ps -> OSPictContext -> IO (WindowHandle ls ps)
updateControls wMetrics info@(UpdateInfo {updWIDS=updWIDS,updControls=updControls}) wH@(WindowHandle {whSelect=whSelect,whItems=whItems}) osPict = do
    (_,itemHs) <- updateControls wMetrics updWIDS whSelect updControls whItems osPict
    return (wH{whItems=itemHs})
    where
	updateControls :: OSWindowMetrics -> WIDS -> Bool -> [ControlUpdateInfo] -> [WElementHandle ls ps] -> OSPictContext -> IO ([ControlUpdateInfo],[WElementHandle ls ps])
	updateControls wMetrics wids contextAble updControls itemHs osPict
	    | null updControls || null itemHs = return (updControls,itemHs)
	    | otherwise = do			
		  (updControls1,itemH1) <- updateControl  wMetrics wids contextAble updControls  (head itemHs) osPict
		  (updControls2,itemHs1) <- updateControls wMetrics wids contextAble updControls1 (tail itemHs) osPict
		  return (updControls2,itemH1:itemHs1)
	    where
		updateControl :: OSWindowMetrics -> WIDS -> Bool -> [ControlUpdateInfo] -> WElementHandle ls ps -> OSPictContext -> IO ([ControlUpdateInfo], WElementHandle ls ps)
		updateControl wMetrics wids contextAble updControls itemH@(WItemHandle {wItemShow=wItemShow}) osPict = do
		    updateControl' wMetrics wids contextAble updControls itemH osPict		    
		    where
			updateControl' :: OSWindowMetrics -> WIDS -> Bool -> [ControlUpdateInfo] -> WElementHandle ls ps -> OSPictContext -> IO ([ControlUpdateInfo], WElementHandle ls ps)
			updateControl' wMetrics wids contextAble updControls itemH@(WItemHandle {wItemNr=wItemNr}) osPict = do
			    let (found,updInfo,updControls1) = remove (\ControlUpdateInfo{cuItemNr=cuItemNr}->cuItemNr==wItemNr) undefined updControls
			    (updControls2,itemHs) <- updateControls wMetrics wids (contextAble && wItemSelect itemH) updControls1 (wItems itemH) osPict
			    let itemH1 = itemH{wItems=itemHs}
			    (if not found then return (updControls2,itemH1)
			     else do
				 itemH <- updateControl'' wMetrics wids contextAble (cuArea updInfo) itemH1 osPict
				 return (updControls2,itemH))
			    where
				updateControl'' :: OSWindowMetrics -> WIDS -> Bool -> Rect -> WElementHandle ls ps -> OSPictContext -> IO (WElementHandle ls ps)
				updateControl'' wMetrics wids contextAble area itemH@(WItemHandle {wItemKind=IsCustomButtonControl,wItemAtts=attrs}) osPict = do
				    (_,_,pen,_) <- doDraw zero (lookPen lookInfo) True osNoRgn osPict (any isControlDoubleBuffered attrs) (accClipPicture (toRegion cFrame) (lookFun lookInfo selectState updState))
				    let info1 = info{cButtonInfoLook=lookInfo{lookPen=pen}}
				    return (itemH{wItemInfo=WCustomButtonInfo info1})
				    where
					selectState	= if (contextAble && wItemSelect itemH) then Able else Unable
					info		= getWItemCustomButtonInfo (wItemInfo itemH)
					lookInfo	= cButtonInfoLook info
					cFrame		= sizeToRectangle (wItemSize itemH)
					updArea		= rectToRectangle (subVector (toVector (wItemPos itemH)) area)
					updState	= UpdateState{oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
				
				updateControl'' wMetrics wids contextAble area itemH@(WItemHandle {wItemKind=IsCustomControl,wItemAtts=attrs}) osPict = do
				    (_,_,pen,_) <- doDraw zero (lookPen lookInfo) True osNoRgn osPict (any isControlDoubleBuffered attrs) (accClipPicture (toRegion cFrame) (lookFun lookInfo selectState updState))
				    let info1 = info{customInfoLook=lookInfo{lookPen=pen}}
				    return (itemH{wItemInfo=WCustomInfo info1})
				    where
					selectState	= if (contextAble && wItemSelect itemH) then Able else Unable
					info		= getWItemCustomInfo (wItemInfo itemH)
					lookInfo	= customInfoLook info
					cFrame		= sizeToRectangle (wItemSize itemH)
					updArea		= rectToRectangle (subVector (toVector (wItemPos itemH)) area)
					updState	= UpdateState{oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
				
				updateControl'' wMetrics wids contextAble area itemH@(WItemHandle {wItemKind=IsCompoundControl,wItemAtts=attrs}) osPict = do
				    (_,_,pen,_) <- doDraw origin (lookPen lookInfo) True osNoRgn osPict (any isControlDoubleBuffered attrs) (accClipPicture (toRegion cFrame) (lookFun lookInfo selectState updState))
				    let info1 = info{compoundLookInfo=compLookInfo{compoundLook=lookInfo{lookPen=pen}}}
				    return (itemH{wItemInfo=WCompoundInfo info1})
				    where
					selectState	= if contextAble && wItemSelect itemH then Able else Unable
					itemSize	= wItemSize itemH
					itemPos		= wItemPos itemH
					info		= getWItemCompoundInfo (wItemInfo itemH)
					domainRect	= compoundDomain info
					hasScrolls	= (isJust (compoundHScroll info),isJust (compoundVScroll info))
					visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
					compLookInfo	= compoundLookInfo info
					lookInfo	= compoundLook compLookInfo
					origin		= compoundOrigin info
					cFrame		= posSizeToRectangle origin (rectSize (getCompoundContentRect wMetrics visScrolls (sizeToRect itemSize)))
					updArea		= rectToRectangle (intersectRects (rectangleToRect cFrame) (addVector (toVector (origin-itemPos)) area))
					updState	= UpdateState{oldFrame=cFrame,newFrame=cFrame,updArea=[updArea]}
				
				updateControl'' _ wids contextAble area itemH@(WItemHandle {wItemKind=IsRadioControl}) osPict = do
					osUpdateRadioControl area (wPtr wids) (wItemPtr itemH)
					return itemH
				
				updateControl'' _ wids contextAble area itemH@(WItemHandle {wItemKind=IsCheckControl}) osPict = do
					osUpdateCheckControl area (wPtr wids) (wItemPtr itemH)
					return itemH
				
				updateControl'' _ wids contextAble area itemH@(WItemHandle {wItemKind=IsPopUpControl}) osPict = do
					osUpdatePopUpControl area (wPtr wids) (wItemPtr itemH)
					return itemH
				
				updateControl'' _ wids contextAble area itemH@(WItemHandle {wItemKind=IsSliderControl}) osPict = do
					osUpdateSliderControl area (wPtr wids) (wItemPtr itemH)
					return itemH
				
				updateControl'' _ wids contextAble area itemH@(WItemHandle {wItemKind=IsTextControl}) osPict = do
					osUpdateTextControl area (wPtr wids) (wItemPtr itemH)
					return itemH
				
				updateControl'' _ wids contextAble area itemH@(WItemHandle {wItemKind=IsEditControl}) osPict = do
					osUpdateEditControl area (wPtr wids) (wItemPtr itemH)
					return itemH
				
				updateControl'' _ wids contextAble area itemH@(WItemHandle {wItemKind=IsButtonControl}) osPict = do
					osUpdateButtonControl area (wPtr wids) (wItemPtr itemH)
					return itemH
				
				updateControl'' _ _ _ _ itemH@(WItemHandle {wItemKind=IsReceiverControl}) osPict =
					return itemH
				
				updateControl'' _ _ _ _ itemH@(WItemHandle {wItemKind=IsLayoutControl}) _ =
					windowupdateFatalError "updatecontrols (updateControl``)" "LayoutControl should never be updated"
		
		updateControl wMetrics wids contextAble updControls (WListLSHandle itemHs) osPict = do
		    (updControls1,itemHs1) <- updateControls wMetrics wids contextAble updControls itemHs osPict
		    return (updControls1,WListLSHandle itemHs1)
		
		updateControl wMetrics wids contextAble updControls (WExtendLSHandle exLS itemHs) osPict = do
		    (updControls1,itemHs1) <- updateControls wMetrics wids contextAble updControls itemHs osPict
		    return (updControls1,WExtendLSHandle exLS itemHs1)
		
		updateControl wMetrics wids contextAble updControls (WChangeLSHandle chLS itemHs) osPict = do
		    (updControls1,itemHs1) <- updateControls wMetrics wids contextAble updControls itemHs osPict
		    return (updControls1,WChangeLSHandle chLS itemHs1)
