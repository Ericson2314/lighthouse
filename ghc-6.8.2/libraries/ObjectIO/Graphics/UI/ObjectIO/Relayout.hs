-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Relayout
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Relayout where


import	Graphics.UI.ObjectIO.CommonDef
import  Graphics.UI.ObjectIO.Window.Handle
import  Graphics.UI.ObjectIO.Window.Access(getCompoundContentRect, getCompoundHScrollRect, getCompoundVScrollRect)
import  Graphics.UI.ObjectIO.StdPicture(Look, toRegion, accClipPicture)
import	Graphics.UI.ObjectIO.OS.Rgn
import  Graphics.UI.ObjectIO.OS.Window
import  Graphics.UI.ObjectIO.OS.Picture(Draw(..), defaultPen, 
		  getPictOrigin, setPictOrigin, getPictPen, setPictPen,
		  pictGetClipRgn, pictSetClipRgn, pictAndClipRgn, doDraw)
import  Graphics.UI.ObjectIO.OS.Cutil_12(addr2int, nullPtr)
import  Control.Monad(when, unless)


data RelayoutItem
   = RelayoutItem
   	{ rliItemKind	:: !ControlKind		-- The control kind
	, rliItemPtr	:: !OSWindowPtr		-- The ptr to the item
	, rliItemPos	:: !Point2		-- The exact position of the item
	, rliItemSize	:: !Size		-- The exact size of the item
	, rliItemSelect	:: !Bool		-- The item is Able (True) or Unable (False)
	, rliItemShow	:: !Bool		-- The item is visible (True) or invisible (False)
	, rliItemInfo	:: CompoundInfo		-- If the control kind is IsCompoundControl: its CompoundInfo; otherwise undefined
	, rliItemLook	:: LookInfo		-- If the control kind is IsCustom(Button)Control: its LookInfo; otherwise undefined
	, rliItems	:: ![RelayoutItem]	-- If the control kind is Is(Compound/Layout)Control: its elements; otherwise: []
	}

relayoutFatalError :: String -> String -> x
relayoutFatalError function error
	= dumpFatalError function "relayout" error


{-	relayoutItems resizes and moves changed items.
		The two Rect   arguments are the window frames in which the elements reside.
		The two Point2 arguments are the positions of the parent window/compound.
		The OSWindowPtr is the parent window/dialog.
		The first  RelayoutItem list contains the elements at their original location and size.
		The second RelayoutItem list contains the elements at their new location and size.
	Assumptions: 
		* The two lists contain elements that are identical except for size and position
		* (Radio/Check)Controls are flattened and have rliItemKind Is(Radio/Check)Control
		* The ClipStates of CompoundControls are valid.
	This version uses the HDC of the parent window in order to suppress calls to initpicture.
		Regions are used to clip sibling controls.
		In addition, two regions (validRegion,invalidRegion) are maintained that administrate whether part of the window requires
		update after relayout. This is done as follows:
			* initially validRegion and invalidRegion are empty.
			* for each relayoutitem: if its oldFrame<>newFrame then it adds newFrame to validRegion, and oldFrame to invalidRegion
			* the area to be updated equals validRegion - invalidRegion (so if its empty, then no update is required)
	relayoutItems returns the update region. 
-}


relayoutItems :: OSWindowMetrics -> Rect -> Rect -> Point2 -> Point2 -> OSWindowPtr -> [RelayoutItem] -> [RelayoutItem] -> IO OSRgnHandle
relayoutItems wMetrics oldFrame newFrame oldParentPos newParentPos wPtr oldHs newHs = do
    clipRgn <- osNewRectRgn newFrame
    validRgn <- osNewRectRgn zero
    invalidRgn <- osNewRectRgn zero
    osPict <- osGrabWindowPictContext wPtr
    ((clipRgn,validRgn,invalidRgn), _, _, _) <-
    		doDraw zero defaultPen True osNoRgn osPict False (accClipPicture (toRegion (rectToRectangle newFrame))
			(relayoutItems' wPtr wMetrics newArea (oldFrame,oldParentPos,oldHs)
				(newFrame,newParentPos,newHs)
				(clipRgn,validRgn,invalidRgn)))
    osReleaseWindowPictContext wPtr osPict
    updRgn <- osDiffRgn	invalidRgn validRgn
    mapM_ osDisposeRgn [clipRgn,validRgn,invalidRgn]
    return updRgn
    where
	newArea				= subtractRects newFrame oldFrame
	
	relayoutItems' :: OSWindowPtr -> OSWindowMetrics -> [Rect] -> (Rect,Point2,[RelayoutItem]) -> (Rect,Point2,[RelayoutItem]) 
			-> (OSRgnHandle,OSRgnHandle,OSRgnHandle) -> Draw (OSRgnHandle,OSRgnHandle,OSRgnHandle)
	relayoutItems' wPtr wMetrics newArea (oldFrame,oldParentPos,(oldH:oldHs)) (newFrame,newParentPos,(newH:newHs)) rgnHs = do
	    rgnHs1 <- relayoutItem wPtr wMetrics newArea (oldFrame,oldParentPos,oldH)  (newFrame,newParentPos,newH)  rgnHs
	    rgnHs2 <- relayoutItems' wPtr wMetrics newArea (oldFrame,oldParentPos,oldHs) (newFrame,newParentPos,newHs) rgnHs1
	    return rgnHs2
	    where
		relayoutItem :: OSWindowPtr -> OSWindowMetrics -> [Rect] -> (Rect,Point2,RelayoutItem) -> (Rect,Point2,RelayoutItem) ->
						(OSRgnHandle,OSRgnHandle,OSRgnHandle) -> Draw (OSRgnHandle,OSRgnHandle,OSRgnHandle)
		relayoutItem wPtr wMetrics newArea old@(_,_,RelayoutItem{rliItemKind=k1}) new@(_,_,RelayoutItem{rliItemKind=k2}) rgnHs
			| k1 /= k2		= relayoutFatalError "relayoutItem" "mismatching RelayoutItems"		
			| otherwise		= relayout wPtr wMetrics newArea k1 old new rgnHs
			where
			    {-	relayout assumes that the two RelayoutItem arguments 
				    have the same ControlKind (fourth argument) and differ only in size or position or both.
			    -}
			    relayout :: OSWindowPtr -> OSWindowMetrics -> [Rect] -> ControlKind -> (Rect,Point2,RelayoutItem) -> (Rect,Point2,RelayoutItem) ->
						    (OSRgnHandle,OSRgnHandle,OSRgnHandle) -> Draw (OSRgnHandle,OSRgnHandle,OSRgnHandle)

			    relayout wPtr wMetrics newArea IsCompoundControl (oldFrame,oldParentPos,old) (newFrame,newParentPos,new)
					    (clipRgn,validRgn,invalidRgn) = do
				liftIO (sizeF >> moveF)
				updF
				liftIO updScrollbars		-- update scrollbars AFTER moving/sizing/updating
				(clipRgn,validRgn,invalidRgn) <- relayoutItems' wPtr wMetrics newArea1 (oldFrame1,oldPos,rliItems old)
											(newFrame1,newPos,rliItems new) (clipRgn,validRgn,invalidRgn)
				(if rliItemShow new then do
				   (validRgn1,invalidRgn1) <- liftIO (checkUpdateRegions oldFrame1 newFrame1 (validRgn,invalidRgn))
				   clipRgn1 <- liftIO (subtractRectFromRgn (intersectRects newFrame newCompoundRect) clipRgn)
				   return (clipRgn1,validRgn1,invalidRgn1)
				 else
				   return (clipRgn,validRgn,invalidRgn))				
				 where
				    sameSize = oldSize==newSize
				    samePos	 = osCompoundMovesControls && oldPos-oldParentPos==newPos-newParentPos || oldPos==newPos
				    sizeF	 = unless sameSize
					    (osSetCompoundSize wPtr newParentPos' itemPtr newPos' newSize' True)
				    moveF	 = unless (samePos && all isEmptyRect (map (intersectRects newFrame1) newArea))
					    (osSetCompoundPos  wPtr newParentPos' itemPtr newPos' newSize' True)
				    updScrollbars = unless (sameSize && samePos && all isEmptyRect (concat (map (\area->[intersectRects hRect' area,intersectRects vRect' area]) newArea)))
					    ( (setCompoundScroll (fst hasScrolls) wMetrics itemPtr True  newHThumbSize (x oldOrigin) (x newOrigin) hRect) >>
					      (setCompoundScroll (snd hasScrolls) wMetrics itemPtr False newVThumbSize (y oldOrigin) (y newOrigin) vRect)
					    )
				    updF = unless (sameSize && oldPos==newPos && oldFrame1==newFrame1 || isEmptyRect newFrame1 || not (rliItemShow new))
					    (updateCustomControl wPtr clipRgn newFrame1 new)
				    newParentPos'	= toTuple newParentPos
				    itemPtr		= rliItemPtr new
				    newSize		= rliItemSize new
				    newSize'		= toTuple newSize
				    oldSize		= rliItemSize old
				    newPos		= rliItemPos new
				    newPos'		= toTuple newPos
				    oldPos		= rliItemPos old
				    newInfo		= rliItemInfo new
				    oldInfo		= rliItemInfo old 
				    newOrigin		= compoundOrigin newInfo
				    oldOrigin		= compoundOrigin oldInfo
				    newDomainRect	= compoundDomain newInfo
				    oldDomainRect	= compoundDomain oldInfo
				    newCompoundRect	= posSizeToRect newPos newSize
				    oldCompoundRect	= posSizeToRect oldPos oldSize
				    newFrame1		= intersectRects newFrame newContentRect
				    oldFrame1		= intersectRects oldFrame oldContentRect
				    newArea1		= subtractRects newFrame1 oldFrame1
				    hasScrolls		= (isJust (compoundHScroll newInfo),isJust (compoundVScroll newInfo))
				    newVisScrolls	= osScrollbarsAreVisible wMetrics newDomainRect newSize' hasScrolls
				    oldVisScrolls	= osScrollbarsAreVisible wMetrics oldDomainRect (toTuple oldSize) hasScrolls
				    newHThumbSize	= ((w newSize)-(if snd newVisScrolls then osmVSliderWidth  wMetrics else 0)+1)
				    newVThumbSize	= ((h newSize)-(if fst newVisScrolls then osmHSliderHeight wMetrics else 0)+1)
				    oldContentRect	= getCompoundContentRect wMetrics oldVisScrolls oldCompoundRect
				    newContentRect	= getCompoundContentRect wMetrics newVisScrolls newCompoundRect
				    hRect		= getCompoundHScrollRect wMetrics newVisScrolls (sizeToRect newSize)
				    hRect'		= addVector (toVector newPos) hRect
				    vRect		= getCompoundVScrollRect wMetrics newVisScrolls (sizeToRect newSize)
				    vRect'		= addVector (toVector newPos) vRect

				    setCompoundScroll :: Bool -> OSWindowMetrics -> OSWindowPtr -> Bool -> Int -> Int -> Int -> Rect -> IO ()
				    setCompoundScroll hasScroll wMetrics compoundPtr isHorizontal size old new (Rect {rright=right,rbottom=bottom})
					    | not hasScroll	= return ()
					    | otherwise = do
						    osSetCompoundSliderThumbSize wMetrics compoundPtr isHorizontal size (right,bottom) (old==new)
						    when (old /= new) (osSetCompoundSliderThumb wMetrics compoundPtr isHorizontal new (right,bottom) True)

			    relayout wPtr wMetrics newArea IsLayoutControl (oldFrame,oldParentPos,old) (newFrame,newParentPos,new) rgnHs =
				    relayoutItems' wPtr wMetrics newArea (oldFrame1,oldParentPos,rliItems old) (newFrame1,newParentPos,rliItems new) rgnHs				    
				    where
				      newSize	= rliItemSize new
				      oldSize	= rliItemSize old
				      newPos	= rliItemPos  new
				      oldPos	= rliItemPos  old
				      newLayoutRect	= posSizeToRect newPos newSize
				      oldLayoutRect	= posSizeToRect oldPos oldSize
				      newFrame1	= intersectRects newFrame newLayoutRect
				      oldFrame1	= intersectRects oldFrame oldLayoutRect

			    relayout wPtr wMetrics newArea controlKind (oldFrame,oldParentPos,old) (newFrame,newParentPos,new) (clipRgn,validRgn,invalidRgn) = do
				    liftIO (sizeF >> moveF)
				    updF
				    (if rliItemShow new then do
					    (validRgn,invalidRgn) <- liftIO (checkUpdateRegions oldFrame1 newFrame1 (validRgn,invalidRgn))
					    clipRgn <- liftIO (subtractRectFromRgn newFrame1 clipRgn)
					    return (clipRgn,validRgn,invalidRgn)
				     else return (clipRgn,validRgn,invalidRgn))				    
				    where
					sameSize	= oldSize==newSize
					samePos		= osCompoundMovesControls && oldPos-oldParentPos==newPos-newParentPos || oldPos==newPos
					sizeF		= unless sameSize
					    			(setSize wPtr newParentPos' itemPtr newPos' newSize' (not redraw))
					moveF		= unless (samePos && all isEmptyRect (map (intersectRects newFrame1) newArea))
					    			(setPos  wPtr newParentPos' itemPtr newPos' (toTuple oldSize) (not redraw))
					updF		= unless (not redraw || sameSize && oldPos==newPos && newFrame1==oldFrame1 || isEmptyRect newFrame1 || not (rliItemShow new))
					    			(updateCustomControl wPtr clipRgn newFrame1 new)
					newParentPos'	= toTuple newParentPos
					itemPtr		= rliItemPtr new
					newPos		= rliItemPos new
					newPos'		= toTuple newPos
					oldPos		= rliItemPos  old
					newSize		= rliItemSize new
					newSize'	= toTuple newSize
					oldSize		= rliItemSize old				    
					oldFrame1	= intersectRects oldFrame (posSizeToRect oldPos oldSize)
					newFrame1	= intersectRects newFrame (posSizeToRect newPos newSize)
					(setPos,setSize,redraw) = case controlKind of
						    IsRadioControl		-> (osSetRadioControlPos,		\_ _ _ _ _ _ -> return (),	False)
						    IsCheckControl		-> (osSetCheckControlPos,		\_ _ _ _ _ _ -> return (),	False)
						    IsPopUpControl		-> (osSetPopUpControlPos,		osSetPopUpControlSize,		False)
						    IsListBoxControl		-> (osSetListBoxControlPos,		osSetListBoxControlSize,	False)
						    IsSliderControl		-> (osSetSliderControlPos,		osSetSliderControlSize,		False)
						    IsTextControl		-> (osSetTextControlPos,		osSetTextControlSize,		False)
						    IsEditControl		-> (osSetEditControlPos,		osSetEditControlSize,		False)
						    IsButtonControl		-> (osSetButtonControlPos,		osSetButtonControlSize,		False)
						    IsCustomButtonControl	-> (osSetCustomButtonControlPos,	osSetCustomButtonControlSize,	True)
						    IsCustomControl		-> (osSetCustomControlPos,		osSetCustomControlSize,		True)
						    IsReceiverControl 		-> (\_ _ _ _ _ _ -> return (),		\_ _ _ _ _ _ -> return (),	False)
						    _			-> relayoutFatalError "relayout" "unexpected ControlKind alternative"
			
	relayoutItems' _ _ _ (_,_,[]) (_,_,[]) rgnHs = return rgnHs
	relayoutItems' _ _ _ _ _ _
		= relayoutFatalError "relayoutItems'" "mismatching RelayoutItems"
	
	checkUpdateRegions :: Rect -> Rect -> (OSRgnHandle,OSRgnHandle) -> IO (OSRgnHandle,OSRgnHandle)
	checkUpdateRegions oldFrame newFrame rgnHs@(validRgn,invalidRgn)
		| oldFrame==newFrame = return rgnHs
		| otherwise = do
		    newFrameRgn <- osNewRectRgn newFrame
		    oldFrameRgn <- osNewRectRgn oldFrame
		    okNewRgn <- osDiffRgn  newFrameRgn invalidRgn			-- PA+++
		    newValidRgn <- osUnionRgn okNewRgn validRgn				-- PA: okNewRgn <-- newFrameRgn
		    newInvalidRgn <- osUnionRgn oldFrameRgn invalidRgn
		    mapM_ osDisposeRgn [validRgn,invalidRgn,newFrameRgn,oldFrameRgn,okNewRgn]	-- PA: okNewRgn added
		    return (newValidRgn,newInvalidRgn)
	
	subtractRectFromRgn :: Rect -> OSRgnHandle -> IO OSRgnHandle
	subtractRectFromRgn rect rgn
		| isEmptyRect rect = return rgn
		| otherwise = do
		    rectRgn <- osNewRectRgn rect
		    diffRgn <- osDiffRgn rgn rectRgn
		    osDisposeRgn rectRgn
		    osDisposeRgn rgn
		    return diffRgn
	
	--	updateCustomControl assumes that the item is visible.
	updateCustomControl :: OSWindowPtr -> OSRgnHandle -> Rect -> RelayoutItem -> Draw ()
	updateCustomControl parentPtr clipRgn contentRect itemH@(RelayoutItem{rliItemKind=IsCustomButtonControl}) = do		
		curOrigin <- getPictOrigin
		curPen <- getPictPen
		setPictOrigin (zero-itemPos)
		setPictPen lookPen
		clipOSPicture clipRgn contentRect (lookFun selectState updState)
		setPictPen curPen
		setPictOrigin curOrigin
		return ()
		where
		  selectState		= if rliItemSelect itemH then Able else Unable
		  itemPos		= rliItemPos itemH		  
		  cFrame		= sizeToRectangle (rliItemSize itemH)
		  updState		= UpdateState{oldFrame=cFrame,newFrame=cFrame,updArea=[cFrame]}
		  LookInfo{lookFun=lookFun,lookPen=lookPen} = rliItemLook itemH
	
	updateCustomControl parentPtr clipRgn contentRect itemH@(RelayoutItem{rliItemKind=IsCustomControl}) = do
		curOrigin <- getPictOrigin
		curPen <- getPictPen
		setPictOrigin (zero-itemPos)
		setPictPen lookPen
		clipOSPicture clipRgn contentRect (lookFun selectState updState)
		setPictPen curPen
		setPictOrigin curOrigin
		return ()
		where
		  selectState		= if rliItemSelect itemH then Able else Unable
		  itemPos		= rliItemPos itemH		  
		  cFrame		= sizeToRectangle (rliItemSize itemH)
		  updState		= UpdateState{oldFrame=cFrame,newFrame=cFrame,updArea=[cFrame]}
		  LookInfo{lookFun=lookFun,lookPen=lookPen} = rliItemLook itemH
	
	updateCustomControl parentPtr clipRgn' contentRect itemH@(RelayoutItem{rliItemKind=IsCompoundControl}) = do
		curOrigin <- getPictOrigin
		curPen <- getPictPen
		setPictOrigin (origin-itemPos)
		setPictPen lookPen
		clip <- liftIO (osSectRgn clipRgn' (clipRgn clipInfo)) -- PA+++
		clipOSPicture clip clipRect (lookFun selectState updState)
		liftIO (osDisposeRgn clip)				-- PA+++
		setPictPen curPen
		setPictOrigin curOrigin
		return ()
		where
		  selectState		= if rliItemSelect itemH then Able else Unable
		  itemSize		= rliItemSize itemH
		  itemPos		= rliItemPos itemH
		  info			= rliItemInfo itemH		  
		  visScrolls		= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
		  cFrameRect		= getCompoundContentRect wMetrics visScrolls (posSizeToRect origin itemSize)
		  cFrame		= rectToRectangle cFrameRect
		  compLookInfo		= compoundLookInfo info		  
		  clipInfo		= compoundClip compLookInfo
		  updState		= UpdateState{oldFrame=cFrame,newFrame=cFrame,updArea=[cFrame]}
		  cRect			= addVector (toVector (itemPos-origin)) cFrameRect
		  clipRect		= intersectRects contentRect cRect
		  LookInfo{lookFun=lookFun,lookPen=lookPen} = compoundLook compLookInfo
		  (origin,domainRect,hasScrolls) = (compoundOrigin info,compoundDomain info,(isJust (compoundHScroll info),isJust (compoundVScroll info)))

	clipOSPicture :: OSRgnHandle -> Rect -> Draw () -> Draw ()
	clipOSPicture newClipRgn rect drawf = do
		rectRgn <- liftIO (osNewRectRgn rect)
		curClipRgn <- pictGetClipRgn
		(if curClipRgn==nullPtr then pictSetClipRgn else pictAndClipRgn) rectRgn
		when (newClipRgn /= nullPtr) (pictAndClipRgn newClipRgn)
		drawf
		pictSetClipRgn curClipRgn
		liftIO (when (curClipRgn /= nullPtr) (osDisposeRgn curClipRgn) >> osDisposeRgn rectRgn)
