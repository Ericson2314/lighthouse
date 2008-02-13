-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Window.ClipState
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Window.ClipState
		( validateWindowClipState, validateCompoundClipState
	        , forceValidWindowClipState, forceValidCompoundClipState		      
		, invalidateWindowClipState, invalidateCompoundClipState
		, disposeClipState
		) where


import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Window.Access(getWItemRadioInfo,  getWItemCheckInfo,  getWItemCompoundInfo,
		    			  getCompoundContentRect, getCompoundHScrollRect, getCompoundVScrollRect, getWindowContentRect)
import Graphics.UI.ObjectIO.Window.Handle
import Graphics.UI.ObjectIO.OS.Rgn
import Graphics.UI.ObjectIO.OS.Window
import Graphics.UI.ObjectIO.OS.Cutil_12(addr2int)
import Foreign.Ptr(nullPtr)


{-	createClipState wMetrics allClipStates validate wPtr clipRect defId isVisible items
		calculates the ClipState that corresponds with items.
		If the Boolean argument is True, also the invalid ClipStates are recalculated of CompoundControls
			that are inside the window frame. 
-}
createClipState :: OSWindowMetrics -> Bool -> Bool -> OSWindowPtr -> Rect -> Maybe Id -> Bool -> [WElementHandle ls ps] -> IO (ClipState,[WElementHandle ls ps])
createClipState wMetrics allClipStates validate wPtr clipRect defId isVisible itemHs = do	
	clipRgn <- osNewRectRgn clipRect
	(itemHs,clipRgn) <- createWElementsClipState wMetrics allClipStates validate wPtr clipRect defId isVisible itemHs clipRgn
	return (ClipState{clipRgn=clipRgn,clipOk=True},itemHs)
	where
	    createWElementsClipState :: OSWindowMetrics -> Bool -> Bool -> OSWindowPtr -> Rect -> Maybe Id -> Bool -> [WElementHandle ls ps] -> OSRgnHandle -> IO ([WElementHandle ls ps],OSRgnHandle)
	    createWElementsClipState wMetrics allClipStates validate wPtr clipRect defId isVisible (itemH:itemHs) clipRgn = do
		(itemH, clipRgn1) <- createWElementClipState  wMetrics allClipStates validate wPtr clipRect defId isVisible itemH  clipRgn
		(itemHs,clipRgn2) <- createWElementsClipState wMetrics allClipStates validate wPtr clipRect defId isVisible itemHs clipRgn1
		return (itemH:itemHs,clipRgn2)
		where
		  createWElementClipState :: OSWindowMetrics -> Bool -> Bool -> OSWindowPtr -> Rect -> Maybe Id -> Bool -> WElementHandle ls ps -> OSRgnHandle -> IO (WElementHandle ls ps, OSRgnHandle)
		  createWElementClipState wMetrics allClipStates validate wPtr clipRect defId isVisible itemH@(WItemHandle {wItemShow=wItemShow,wItemKind=wItemKind}) clipRgn
		      | not itemVisible || disjointRects clipRect itemClipRect =
			      if not allClipStates || not (isRecursiveControl wItemKind) then	-- PA:<>IsCompoundControl
				      return (itemH, clipRgn)
			      else if wItemKind==IsLayoutControl then do				-- PA: this alternative added				      
				      (itemHs,clipRgn1) <- createWElementsClipState wMetrics allClipStates validate wPtr itemClipRect defId False (wItems itemH) clipRgn
				      return (itemH{wItems=itemHs},clipRgn1)
			      else if validate then do
				      itemH1 <- validateCompoundClipState wMetrics allClipStates wPtr defId itemVisible itemH
				      return (itemH1,clipRgn)
			      else do			      	      
				      itemH1 <- forceValidCompoundClipState wMetrics allClipStates wPtr defId itemVisible itemH
				      return (itemH1,clipRgn)
		      | otherwise = do
			      (itemH,clipRgn1) <- createWItemClipState wMetrics allClipStates validate wPtr clipRect defId itemH clipRgn
			      return (itemH,clipRgn1)
		      where
			  itemVisible  = isVisible && wItemShow
			  itemClipRect = posSizeToRect (wItemPos itemH) (wItemSize itemH)

			  createWItemClipState :: OSWindowMetrics -> Bool -> Bool -> OSWindowPtr -> Rect -> Maybe Id -> WElementHandle ls ps -> OSRgnHandle -> IO (WElementHandle ls ps, OSRgnHandle)
			  createWItemClipState _ _ _ wPtr clipRect _ itemH@(WItemHandle {wItemKind=IsRadioControl,wItemInfo=wItemInfo}) clipRgn = do
			      clipRgn1 <- foldrM (createRadioClipState wPtr clipRect) clipRgn (radioItems (getWItemRadioInfo wItemInfo))
			      return (itemH,clipRgn1)
			      where
				  createRadioClipState :: OSWindowPtr -> Rect -> RadioItemInfo ls ps -> OSRgnHandle -> IO OSRgnHandle
				  createRadioClipState wPtr clipRect (RadioItemInfo{radioItemPos=radioItemPos,radioItemSize=radioItemSize}) clipRgn = do
				      radioRgn <- osClipRadioControl wPtr (0,0) clipRect (toTuple radioItemPos) (toTuple radioItemSize)
				      diffRgn <- osDiffRgn clipRgn radioRgn
				      osDisposeRgn clipRgn				      
				      osDisposeRgn radioRgn
				      return diffRgn

			  createWItemClipState _ _ _ wPtr clipRect defId itemH@(WItemHandle {wItemKind=IsCheckControl,wItemInfo=wItemInfo}) clipRgn = do
			      clipRgn <- foldrM (createCheckClipState wPtr clipRect) clipRgn (checkItems (getWItemCheckInfo wItemInfo))
			      return (itemH,clipRgn)
			      where
				  createCheckClipState :: OSWindowPtr -> Rect -> CheckItemInfo ls ps -> OSRgnHandle -> IO OSRgnHandle
				  createCheckClipState wPtr clipRect (CheckItemInfo{checkItemPos=checkItemPos,checkItemSize=checkItemSize}) clipRgn = do
				      checkRgn <- osClipCheckControl wPtr (0,0) clipRect (toTuple checkItemPos) (toTuple checkItemSize)
				      diffRgn <- osDiffRgn clipRgn checkRgn
				      osDisposeRgn clipRgn
				      osDisposeRgn checkRgn
				      return diffRgn

			  createWItemClipState wMetrics allClipStates validate wPtr clipRect defId itemH@(WItemHandle {wItemKind=IsCompoundControl,wItems=wItems}) clipRgn = do
			      rectRgn <- osClipCompoundControl wPtr (0,0) clipRect (toTuple itemPos) (toTuple itemSize)
			      diffRgn <- osDiffRgn clipRgn rectRgn
			      osDisposeRgn clipRgn
			      osDisposeRgn rectRgn
			      (if allClipStates then
			         if validate then do
			           itemH1 <- validateCompoundClipState wMetrics allClipStates wPtr defId True itemH
				   return (itemH1,diffRgn)
				 else do
				   itemH1 <- forceValidCompoundClipState wMetrics allClipStates wPtr defId True itemH				   
				   return (itemH1,diffRgn)
			       else
				 return (itemH,diffRgn))
			      where
				 itemPos	= wItemPos itemH
				 itemSize	= wItemSize itemH

			  createWItemClipState wMetrics allClipStates validate wPtr clipRect defId itemH@(WItemHandle {wItemKind=IsLayoutControl,wItems=wItems}) clipRgn = do
			      (itemHs,clipRgn) <- createWElementsClipState wMetrics allClipStates validate wPtr clipRect1 defId True wItems clipRgn
			      return (itemH{wItems=itemHs},clipRgn)
			      where
				  clipRect1 = intersectRects (posSizeToRect (wItemPos itemH) (wItemSize itemH)) clipRect

			  createWItemClipState _ _ _ wPtr clipRect defId itemH@(WItemHandle {wItemKind=wItemKind,wItemPos=wItemPos,wItemSize=wItemSize}) clipRgn
				  | Just clipItem <- mb_item = do
					  itemRgn <- clipItem wPtr (0,0) clipRect (toTuple wItemPos) (toTuple wItemSize)
					  diffRgn <- osDiffRgn clipRgn itemRgn					  
					  osDisposeRgn clipRgn
					  osDisposeRgn itemRgn
					  return (itemH,diffRgn)
			  		  where
					    mb_item = case wItemKind of
						IsPopUpControl		-> Just osClipPopUpControl
						IsListBoxControl	-> Just osClipListBoxControl
						IsSliderControl		-> Just osClipSliderControl
						IsTextControl		-> Just osClipTextControl
						IsEditControl		-> Just osClipEditControl
						IsButtonControl		-> Just osClipButtonControl
						IsCustomButtonControl	-> Just osClipCustomButtonControl
						IsCustomControl		-> Just osClipCustomControl
						_			-> Nothing

			  createWItemClipState _ _ _ _ _ _ itemH clipRgn = return (itemH,clipRgn)

		  createWElementClipState wMetrics allClipStates validate wPtr clipRect defId isVisible (WListLSHandle itemHs) clipRgn = do
			  (itemHs1,clipRgn1) <- createWElementsClipState wMetrics allClipStates validate wPtr clipRect defId isVisible itemHs clipRgn
			  return (WListLSHandle itemHs1,clipRgn1)

		  createWElementClipState wMetrics allClipStates validate wPtr clipRect defId isVisible (WExtendLSHandle exLS itemHs) clipRgn = do
			  (itemHs1,clipRgn1) <- createWElementsClipState wMetrics allClipStates validate wPtr clipRect defId isVisible itemHs clipRgn
			  return (WExtendLSHandle exLS itemHs1,clipRgn1)

		  createWElementClipState wMetrics allClipStates validate wPtr clipRect defId isVisible (WChangeLSHandle chLS itemHs) clipRgn = do
			  (itemHs1,clipRgn1) <- createWElementsClipState wMetrics allClipStates validate wPtr clipRect defId isVisible itemHs clipRgn
			  return (WChangeLSHandle chLS itemHs1,clipRgn1)
	
	    createWElementsClipState _ _ _ _ _ _ _ itemHs clipRgn =
		return (itemHs,clipRgn)


disposeClipState :: ClipState -> IO ()
disposeClipState (ClipState{clipRgn=clipRgn})
	| clipRgn==nullPtr = return ()
	| otherwise        = osDisposeRgn clipRgn


{-	validateAllClipStates(`) validate wPtr defaultId isVisible items
		checks for all items if the ClipState of CompoundControls is valid.
		If validate holds, then the ClipState is checked, otherwise the ClipState is disposed and recalculated(!!).
-}
validateAllClipStates :: OSWindowMetrics -> Bool -> OSWindowPtr -> Maybe Id -> Bool -> [WElementHandle ls ps] -> IO [WElementHandle ls ps]
validateAllClipStates wMetrics validate wPtr defaultId isVisible itemHs =
    mapM (validateClipState wMetrics validate wPtr defaultId isVisible) itemHs
    where
	validateClipState :: OSWindowMetrics -> Bool -> OSWindowPtr -> Maybe Id -> Bool -> WElementHandle ls ps -> IO (WElementHandle ls ps)
	validateClipState wMetrics validate wPtr defaultId isVisible itemH@(WItemHandle {wItemKind=wItemKind})
	    | wItemKind /= IsCompoundControl =
		    if isRecursiveControl wItemKind then do		-- PA: added for LayoutControls
			itemHs <- mapM (validateClipState wMetrics validate wPtr defaultId itemVisible) (wItems itemH)
			return (itemH{wItems=itemHs})
		    else
			return itemH
	    | validate = validateCompoundClipState wMetrics True wPtr defaultId itemVisible itemH
	    | otherwise = forceValidCompoundClipState wMetrics True wPtr defaultId itemVisible itemH
	    where
		itemVisible			= isVisible && wItemShow itemH
	
	validateClipState wMetrics validate wPtr defaultId isVisible (WListLSHandle itemHs) = do
		itemHs <- mapM (validateClipState wMetrics validate wPtr defaultId isVisible) itemHs
		return (WListLSHandle itemHs)

	validateClipState wMetrics validate wPtr defaultId isVisible (WExtendLSHandle exLS itemHs) = do
		itemHs <- mapM (validateClipState wMetrics validate wPtr defaultId isVisible) itemHs
		return (WExtendLSHandle exLS itemHs)

	validateClipState wMetrics validate wPtr defaultId isVisible (WChangeLSHandle chLS itemHs) = do
		itemHs <- mapM (validateClipState wMetrics validate wPtr defaultId isVisible) itemHs
		return (WChangeLSHandle chLS itemHs)


validateWindowClipState :: OSWindowMetrics -> Bool -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
validateWindowClipState wMetrics allClipStates wPtr wH@(WindowHandle {whKind=whKind,whWindowInfo=windowInfo,whItems=whItems,whSize=whSize,whDefaultId=whDefaultId,whShow=whShow})
	| whKind==IsDialog =
		if not allClipStates then return wH
		else do
		  itemHs <- validateAllClipStates wMetrics True wPtr whDefaultId whShow whItems
		  return (wH{whItems=itemHs})
	| clipOk clipState =
		if not allClipStates then return wH
		else do
		  itemHs <- validateAllClipStates wMetrics True wPtr whDefaultId whShow whItems
		  return (wH{whItems=itemHs})
	| otherwise = do		
		disposeClipState clipState
		(newClipState,itemHs) <- createClipState wMetrics allClipStates True wPtr contentRect whDefaultId whShow whItems		
		return (wH{whItems=itemHs,whWindowInfo=windowInfo{windowClip=newClipState}})
	where
	  clipState	= windowClip windowInfo
	  domainRect	= windowDomain windowInfo
	  hasScrolls	= (isJust (windowHScroll windowInfo), isJust (windowVScroll windowInfo))
	  visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
	  contentRect	= getWindowContentRect wMetrics visScrolls (sizeToRect whSize)

forceValidWindowClipState :: OSWindowMetrics -> Bool -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
forceValidWindowClipState wMetrics allClipStates wPtr wH@(WindowHandle {whKind=whKind,whWindowInfo=windowInfo,whItems=whItems,whSize=whSize,whDefaultId=whDefaultId,whShow=whShow})
	| whKind == IsDialog =
		if not allClipStates then return wH
		else do
		  itemHs <- validateAllClipStates wMetrics False wPtr whDefaultId whShow whItems
		  return (wH{whItems=itemHs})
	| otherwise = do		
		disposeClipState (windowClip windowInfo)
		(clipState,itemHs) <- createClipState wMetrics allClipStates False wPtr contentRect whDefaultId whShow whItems		
		return (wH{whItems=itemHs,whWindowInfo=windowInfo{windowClip=clipState}})
	where
	  domainRect	= windowDomain windowInfo
	  hasScrolls	= (isJust (windowHScroll windowInfo), isJust (windowVScroll windowInfo))
	  visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
	  contentRect	= getWindowContentRect wMetrics visScrolls (sizeToRect whSize)

invalidateWindowClipState :: WindowHandle ls ps -> WindowHandle ls ps
invalidateWindowClipState wH@(WindowHandle {whKind=whKind,whWindowInfo=windowInfo})
	| whKind==IsWindow =
		let clipState  = windowClip windowInfo
		in wH{whWindowInfo=windowInfo {windowClip=clipState{clipOk=False}}}
	| otherwise = wH

validateCompoundClipState :: OSWindowMetrics -> Bool -> OSWindowPtr -> Maybe Id -> Bool -> WElementHandle ls ps -> IO (WElementHandle ls ps)
validateCompoundClipState wMetrics allClipStates wPtr defId isVisible itemH@(WItemHandle {wItemShow=wItemShow,wItemPos=wItemPos,wItemSize=wItemSize,wItemInfo=wItemInfo,wItems=wItems})
	| clipOk clipState =
		if not allClipStates then return itemH
		else do
		   itemHs <- validateAllClipStates wMetrics True wPtr defId itemVisible wItems
		   return (itemH{wItems=itemHs})
	| otherwise = do		
		disposeClipState clipState
		(newClipState,itemHs) <- createClipState wMetrics allClipStates True wPtr contentRect defId itemVisible wItems		
		let compoundInfo1 = compoundInfo{compoundLookInfo=compoundLook{compoundClip=newClipState}}
		return (itemH{wItemInfo=WCompoundInfo compoundInfo1,wItems=itemHs})
	where
	  itemVisible		= isVisible && wItemShow
	  compoundInfo		= getWItemCompoundInfo wItemInfo
	  compoundLook		= compoundLookInfo compoundInfo
	  clipState		= compoundClip compoundLook
	  domainRect		= compoundDomain compoundInfo
	  hasScrolls		= (isJust (compoundHScroll compoundInfo), isJust (compoundVScroll compoundInfo))
	  visScrolls		= osScrollbarsAreVisible wMetrics domainRect (toTuple wItemSize) hasScrolls
	  contentRect		= getCompoundContentRect wMetrics visScrolls (posSizeToRect wItemPos wItemSize)

forceValidCompoundClipState :: OSWindowMetrics -> Bool -> OSWindowPtr -> Maybe Id -> Bool -> WElementHandle ls ps -> IO (WElementHandle ls ps)
forceValidCompoundClipState wMetrics allClipStates wPtr defId isVisible itemH@(WItemHandle {wItemShow=wItemShow,wItemPos=wItemPos,wItemSize=wItemSize,wItemInfo=wItemInfo,wItems=wItems}) = do	
	disposeClipState (compoundClip compoundLook)
	(clipState,itemHs) <- createClipState wMetrics allClipStates False wPtr contentRect defId itemVisible wItems	
	let compoundInfo1 = compoundInfo{compoundLookInfo=compoundLook{compoundClip=clipState}}
	return (itemH{wItemInfo=WCompoundInfo compoundInfo1,wItems=itemHs})
	where
	  itemVisible		= isVisible && wItemShow
	  compoundInfo		= getWItemCompoundInfo wItemInfo
	  compoundLook		= compoundLookInfo compoundInfo	  
	  domainRect		= compoundDomain compoundInfo
	  hasScrolls		= (isJust (compoundHScroll compoundInfo), isJust (compoundVScroll compoundInfo))
	  visScrolls		= osScrollbarsAreVisible wMetrics domainRect (toTuple wItemSize) hasScrolls
	  contentRect		= getCompoundContentRect wMetrics visScrolls (posSizeToRect wItemPos wItemSize)

invalidateCompoundClipState :: WElementHandle ls ps -> WElementHandle ls ps
invalidateCompoundClipState itemH@(WItemHandle {wItemInfo=wItemInfo}) =
	let compoundInfo = getWItemCompoundInfo wItemInfo
	    compoundLook = compoundLookInfo compoundInfo
	    clipState	 = compoundClip compoundLook
	in (itemH{wItemInfo=WCompoundInfo compoundInfo{compoundLookInfo=compoundLook{compoundClip=clipState{clipOk=False}}}})