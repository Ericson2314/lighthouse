-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Window.Controls
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Window.Controls
		( opencontrols,  opencompoundcontrols
		, closecontrols, closeallcontrols
		, setcontrolpositions
		) where



import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.Control.Create
import	Graphics.UI.ObjectIO.Control.Layout(layoutControls)
import	Graphics.UI.ObjectIO.Control.Relayout(relayoutControls)
import	Graphics.UI.ObjectIO.StdControlAttribute(isControlPos)
import	Graphics.UI.ObjectIO.Window.ClipState
import	Graphics.UI.ObjectIO.Window.Access(identifyMaybeId, genWElementItemNrs, getWindowContentRect,
					   getWindowHMargins, getWindowVMargins, getWindowItemSpaces)
import	Graphics.UI.ObjectIO.Window.Dispose(disposeWElementHandle)
import	Graphics.UI.ObjectIO.Window.Draw(drawWindowLook)
import	Graphics.UI.ObjectIO.Window.Update(updateWindowBackgrounds)
import	Graphics.UI.ObjectIO.Window.ClipState(invalidateWindowClipState)
import	Graphics.UI.ObjectIO.Window.Handle
import	Graphics.UI.ObjectIO.OS.DocumentInterface
import	Graphics.UI.ObjectIO.OS.Window


windowControlsFatalError :: String -> String -> x
windowControlsFatalError function error = dumpFatalError function "WindowControls" error


--	Auxiliary functions:

{-	opencontrols adds the given controls to the window. 
	It is assumed that the new controls do not conflict with the current controls.
-}
opencontrols :: OSWindowMetrics -> ls -> [WElementHandle ls ps] -> WindowStateHandle ps -> IO (WindowStateHandle ps)
opencontrols wMetrics ls newItems wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH@(WindowHandle {whItems=curItems,whSize=winSize})}))) =
	let
		(itemNrs1,newItems1) 	= genWElementItemNrs itemNrs newItems
		newItems2 		= [WChangeLSHandle ls newItems1]
		allItems		= curItems++newItems2
		visScrolls		= osScrollbarsAreVisible wMetrics domainRect (toTuple winSize) (hasHScroll,hasVScroll)
		Rect{rright=curw,rbottom=curh} = getWindowContentRect wMetrics visScrolls (sizeToRect winSize)
		hMargins		= getWindowHMargins   winKind wMetrics atts
		vMargins		= getWindowVMargins   winKind wMetrics atts
		spaces			= getWindowItemSpaces winKind wMetrics atts
		reqSize			= Size{w=curw-fst hMargins-snd hMargins,h=curh-fst vMargins-snd vMargins}
	in do
		(_,allItems) <- layoutControls wMetrics hMargins vMargins spaces reqSize zero [(domain,origin)] allItems
		let (updCurItems,updNewItems)	= split (length curItems) allItems
		newItems <- createControls wMetrics defaultId cancelId select winPtr newItems
		let updAllItems = updCurItems++updNewItems
		let wH1 = wH{whItemNrs=itemNrs1,whItems=updAllItems}
		let wH2 = invalidateWindowClipState wH1
		return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
	where
		winPtr		= wPtr wids
		atts		= whAtts wH
		defaultId	= whDefaultId wH
		cancelId	= whCancelId wH
		select		= whSelect wH
		itemNrs		= whItemNrs wH
		winKind		= whKind wH
		info		= whWindowInfo wH
		(origin,hasHScroll,hasVScroll,domainRect) = case info of
			info@(WindowInfo {}) -> (windowOrigin info,isJust (windowHScroll info),isJust (windowVScroll info),windowDomain info)
			NoWindowInfo         -> (zero,             False,                      False,                      sizeToRect winSize)
		domain		= rectToRectangle domainRect
opencontrols _ _ _ _ = windowControlsFatalError "opencontrols" "unexpected window placeholder argument"


{-	opencompoundcontrols adds the given controls to the compound control of the given window. 
	It is assumed that the new controls do not conflict with the current controls.
-}
opencompoundcontrols :: OSDInfo -> OSWindowMetrics -> Id -> ls -> [WElementHandle ls ps] -> WindowStateHandle ps -> IO (Bool,WindowStateHandle ps)
opencompoundcontrols osdInfo wMetrics compoundId ls newItems wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH@(WindowHandle {whItems=itemHs})}))) =
	let (found,nrSkip,_,_,itemNrs1,oldItemHs) = addControlsToCompound compoundId ls newItems itemNrs itemHs
	in if not found then return (False,WindowStateHandle wids (Just wlsH{wlsHandle=wH{whItems=oldItemHs}}))
	   else let
	   		curSize@(Size {w=curw,h=curh}) = Size{w=w winSize-(if visVScroll then osmVSliderWidth wMetrics else 0),h=h winSize-(if visHScroll then osmHSliderHeight wMetrics else 0)}
		  	wFrame	= sizeToRect curSize
		  	hMargins = getWindowHMargins winKind wMetrics atts
		  	vMargins = getWindowVMargins winKind wMetrics atts
		  	spaces	= getWindowItemSpaces winKind wMetrics atts
		  	reqSize	= Size{w=curw-fst hMargins-snd hMargins,h=curh-fst vMargins-snd vMargins}
		in do
			(derSize,newItemHs) <- layoutControls wMetrics hMargins vMargins spaces reqSize zero [(domain,origin)] oldItemHs
			newItemHs <- createCompoundControls wMetrics compoundId nrSkip defaultId cancelId select winPtr newItemHs
		  	let wH1 = wH{whItemNrs=itemNrs,whItems=newItemHs}
			wH2 <- forceValidWindowClipState wMetrics True winPtr wH1
			updRgn <- relayoutControls wMetrics select show wFrame wFrame zero zero winPtr defaultId oldItemHs (whItems wH2)
			wH3 <- updateWindowBackgrounds wMetrics updRgn wids wH2
			return (True,WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
	where
		winPtr		= wPtr wids
		atts		= whAtts wH
		defaultId	= whDefaultId wH
		cancelId	= whCancelId wH
		select		= whSelect wH
		show		= whShow wH
		itemNrs		= whItemNrs wH
		winKind		= whKind wH
		winSize		= whSize wH
		domain		= rectToRectangle domainRect
		(origin,domainRect,hasHScroll,hasVScroll) = case whWindowInfo wH of
			info@(WindowInfo {}) -> (windowOrigin info,windowDomain info,isJust (windowHScroll info),isJust (windowVScroll info))
			NoWindowInfo         -> (zero,             sizeToRect winSize,False,False)
		(visHScroll,visVScroll)	 = osScrollbarsAreVisible wMetrics domainRect (toTuple winSize) (hasHScroll,hasVScroll)

		addControlsToCompound :: Id -> ls1 -> [WElementHandle ls1 ps] -> [Int] -> [WElementHandle ls ps] -> (Bool,Int,ls1,[WElementHandle ls1 ps],[Int],[WElementHandle ls ps])
		addControlsToCompound compoundId ls newItems itemNrs [] = (False,0,ls,newItems,itemNrs,[])
		addControlsToCompound compoundId ls newItems itemNrs (itemH:itemHs) =
			let (found,nrSkip,ls1,newItems1,itemNrs1,itemH1) = addControlsToCompound' compoundId ls newItems itemNrs itemH
			in if found then (found,nrSkip,ls1,newItems1,itemNrs1,itemH1:itemHs)
			   else let (found,nrSkip,ls2,newItems2,itemNrs2,itemHs1) = addControlsToCompound compoundId ls1 newItems1 itemNrs1 itemHs
				in (found,nrSkip,ls2,newItems2,itemNrs2,itemH1:itemHs1)
			where
				addControlsToCompound' :: Id -> ls1 -> [WElementHandle ls1 ps] -> [Int] -> WElementHandle ls ps -> (Bool,Int,ls1,[WElementHandle ls1 ps],[Int], WElementHandle ls ps)
				addControlsToCompound' compoundId ls newItems itemNrs itemH@(WItemHandle {wItemKind=itemKind,wItemId=itemId,wItems=itemHs})
					| not (isRecursiveControl itemKind) =
						(False,0,ls,newItems,itemNrs,itemH)
					| itemKind==IsLayoutControl =
						let (found,nrSkip,ls1,newItems1,itemNrs1,itemHs1) = addControlsToCompound compoundId ls newItems itemNrs itemHs
						in (found,nrSkip,ls1,newItems1,itemNrs1,itemH{wItems=itemHs1})
					| not (identifyMaybeId compoundId itemId) =
						let
							(found,nrSkip,ls1,newItems1,itemNrs1,itemHs1) = addControlsToCompound compoundId ls newItems itemNrs itemHs
							itemH1 = itemH{wItems=itemHs1}
							itemH2 = if found then invalidateCompoundClipState itemH1 else itemH1
						in
							(found,nrSkip,ls1,newItems1,itemNrs1,itemH2)
					| otherwise =
						let
							nrSkip 			= length itemHs
							(itemNrs1,newItems1) 	= genWElementItemNrs itemNrs newItems
							newItems2		= [WChangeLSHandle ls newItems1]
							itemH1			= itemH{wItems=itemHs++newItems2}
							itemH2			= invalidateCompoundClipState itemH1
						in
							(True,nrSkip,undefined,[],itemNrs,itemH2)

				addControlsToCompound' compoundId ls newItems itemNrs (WListLSHandle itemHs) =
					let (found,nrSkip,ls1,newItems1,itemNrs1,itemHs1) = addControlsToCompound compoundId ls newItems itemNrs itemHs
					in (found,nrSkip,ls1,newItems1,itemNrs1,WListLSHandle itemHs1)

				addControlsToCompound' compoundId ls newItems itemNrs (WExtendLSHandle exLS itemHs) =
					let (found,nrSkip,ls1,newItems1,itemNrs1,itemHs1) = addControlsToCompound compoundId ls newItems itemNrs itemHs
					in (found,nrSkip,ls1,newItems1,itemNrs1,WExtendLSHandle exLS itemHs1)

				addControlsToCompound' compoundId ls newItems itemNrs (WChangeLSHandle chLS itemHs) =
					let (found,nrSkip,ls1,newItems1,itemNrs1,itemHs1) = addControlsToCompound compoundId ls newItems itemNrs itemHs
					in (found,nrSkip,ls1,newItems1,itemNrs1,WChangeLSHandle chLS itemHs1)
opencompoundcontrols _ _ _ _ _ _
	= windowControlsFatalError "opencompoundcontrols" "unexpected window placeholder argument"


{-	closecontrols closes the indicated controls and returns their R(2)Ids (first result [Id]) and
	Ids (second result [Id]) if appropriate.
	When closecontrols returns, the indicated controls will have been hidden. To actually dispose of them,
	the return (IdFun *OSToolbox) function should be applied.
-}
closecontrols :: OSWindowMetrics -> [Id] -> Bool -> WindowStateHandle ps -> IO ([Id],IO (),WindowStateHandle ps)
closecontrols wMetrics closeIds relayout (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH@(WindowHandle {whItems=curItems})}))) = do
	(freeIds,disposeFun,_,itemNrs,oldItemHs) <- closeWElementHandles winPtr closeIds itemNrs curItems
	(if not relayout
	 then let wH1 = invalidateWindowClipState wH{whItemNrs=itemNrs,whItems=oldItemHs}
	      in return (freeIds,disposeFun,WindowStateHandle wids (Just wlsH{wlsHandle=wH1}))
	 else let
		curw = w winSize-(if visVScroll then osmVSliderWidth  wMetrics else 0)
		curh = h winSize-(if visHScroll then osmHSliderHeight wMetrics else 0)
		wFrame = sizeToRect (Size{w=curw,h=curh})
		hMargins = getWindowHMargins   winKind wMetrics atts
		vMargins = getWindowVMargins   winKind wMetrics atts
		spaces	 = getWindowItemSpaces winKind wMetrics atts
		reqSize	 = Size{w=curw-fst hMargins-snd hMargins,h=curh-fst vMargins-snd vMargins}
	     in do
		(_,newItemHs) <- layoutControls wMetrics hMargins vMargins spaces reqSize zero [(domain,origin)] oldItemHs
		let wH1 = wH{whItemNrs=itemNrs, whItems=newItemHs}
		wH2 <- forceValidWindowClipState wMetrics True winPtr wH1
		updRgn <- relayoutControls wMetrics select show wFrame wFrame zero zero winPtr defaultId oldItemHs (whItems wH2)
		wH3 <- updateWindowBackgrounds wMetrics updRgn wids wH2
		return (freeIds,disposeFun,WindowStateHandle wids (Just wlsH{wlsHandle=wH})))
	where
		winPtr		= wPtr wids
		itemNrs		= whItemNrs wH
		atts		= whAtts wH
		winKind		= whKind wH
		winSize		= whSize wH
		select		= whSelect wH
		show		= whShow wH
		defaultId	= whDefaultId wH
		domain		= rectToRectangle domainRect
		(origin,domainRect,hasHScroll,hasVScroll) = case whWindowInfo wH of
			info@(WindowInfo {}) -> (windowOrigin info,windowDomain info,isJust (windowHScroll info),isJust (windowVScroll info))
			NoWindowInfo         -> (zero,             sizeToRect winSize,False,False)
		(visHScroll,visVScroll)	= osScrollbarsAreVisible wMetrics domainRect (toTuple winSize) (hasHScroll,hasVScroll)

		closeWElementHandles :: OSWindowPtr -> [Id] -> [Int] -> [WElementHandle ls ps] -> IO ([Id],IO (),[Id],[Int],[WElementHandle ls ps])
		closeWElementHandles parentPtr ids itemNrs [] = return ([],return (),ids,itemNrs,[])
		closeWElementHandles parentPtr ids itemNrs (itemH:itemHs)
			| null ids = return ([],return (),ids,itemNrs,itemHs)
			| otherwise = do
				(close,freeIds1,f1,ids,itemNrs,itemH ) <- closeWElementHandle  parentPtr ids itemNrs itemH
				(      freeIds2,f2,ids,itemNrs,itemHs) <- closeWElementHandles parentPtr ids itemNrs itemHs			  	
			  	let freeIds  = freeIds1 ++freeIds2
				return (freeIds,f1 >> f2,ids,itemNrs, if close then itemHs else itemH:itemHs)
			where
				closeWElementHandle :: OSWindowPtr -> [Id] -> [Int] -> WElementHandle ls ps -> IO (Bool,[Id],IO (),[Id],[Int],WElementHandle ls ps)
				closeWElementHandle parentPtr ids itemNrs itemH@(WItemHandle {wItemKind=itemKind,wItems=itemHs}) =
					let (close,ids) = case wItemId itemH of
						Just id	-> removeCheck id ids
						_	-> (False,ids)
					in if isRecursiveControl itemKind
					   then do
						(freeIds,f1,ids,itemNrs,itemHs) <- closeWElementHandles parentPtr ids itemNrs itemHs
						let itemH1 = itemH{wItems=itemHs}
						(if not close
						 then let itemH2 = if itemKind==IsCompoundControl then invalidateCompoundClipState itemH1 else itemH1
						      in return (close,freeIds,f1,ids,itemNrs,itemH)
						 else do
							(freeIds1,f2) <- disposeWElementHandle parentPtr itemH
							osInvalidateWindowRect parentPtr (posSizeToRect (wItemPos itemH) (wItemSize itemH))
							return (close,freeIds1++freeIds,f1 >> f2,ids,itemNrs,itemH))
					   else
						if not close
						then return (close,[],return (),ids,itemNrs,itemH)
						else do
							(freeIds,f) <- disposeWElementHandle parentPtr itemH
							osInvalidateWindowRect parentPtr (posSizeToRect (wItemPos itemH) (wItemSize itemH))
							return (close,freeIds,f,ids,wItemNr itemH:itemNrs,itemH)

				closeWElementHandle parentPtr ids itemNrs (WListLSHandle itemHs) = do
					(freeIds,f,ids,itemNrs,itemHs)	<- closeWElementHandles parentPtr ids itemNrs itemHs
					return (null itemHs,freeIds,f,ids,itemNrs,WListLSHandle itemHs)

				closeWElementHandle parentPtr ids itemNrs (WExtendLSHandle exLS itemHs) = do
					(freeIds,f,ids,itemNrs,itemHs)	<- closeWElementHandles parentPtr ids itemNrs itemHs
					return (null itemHs,freeIds,f,ids,itemNrs,WExtendLSHandle exLS itemHs)

				closeWElementHandle parentPtr ids itemNrs (WChangeLSHandle chLS itemHs) = do
					(freeIds,f,ids,itemNrs,itemHs)	<- closeWElementHandles parentPtr ids itemNrs itemHs
					return (null itemHs,freeIds,f,ids,itemNrs,WChangeLSHandle chLS itemHs)
closecontrols _ _ _ _ = windowControlsFatalError "closecontrols" "unexpected window placeholder argument"


{-	closeallcontrols closes all controls and returns their R(2)Ids (first result [Id]) and Ids (second result [Id]).
	When closeallcontrols returns, the indicated controls will have been hidden. To actually dispose of them,
	the return (IdFun *OSToolbox) function should be applied.
-}
closeallcontrols :: WindowStateHandle ps -> IO ([Id],IO (),WindowStateHandle ps)
closeallcontrols (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH@(WindowHandle {whItems=curItems,whItemNrs=itemNrs})}))) = do
	(freeIds,disposeFun,itemNrs) <- closeWElementHandles (wPtr wids) curItems itemNrs
	let wH1 = invalidateWindowClipState wH{whItemNrs=itemNrs,whItems=[]}
	return (freeIds,disposeFun,WindowStateHandle wids (Just wlsH{wlsHandle=wH1}))
	where
		closeWElementHandles :: OSWindowPtr -> [WElementHandle ls ps] -> [Int] -> IO ([Id],IO (),[Int])
		closeWElementHandles parentPtr [] itemNrs = return ([],return (),itemNrs)
		closeWElementHandles parentPtr (itemH:itemHs) itemNrs = do
			(freeIds1,f1,itemNrs) <- closeWElementHandle  parentPtr itemH  itemNrs
			(freeIds2,f2,itemNrs) <- closeWElementHandles parentPtr itemHs itemNrs
			return (freeIds1++freeIds2,f1 >> f2,itemNrs)
			where
				closeWElementHandle :: OSWindowPtr -> WElementHandle ls ps -> [Int] -> IO ([Id],IO (),[Int])
				closeWElementHandle parentPtr itemH@(WItemHandle {wItemKind=itemKind,wItems=itemHs}) itemNrs
					| isRecursiveControl itemKind = do
						(freeIds1,f1,itemNrs) <- closeWElementHandles parentPtr itemHs itemNrs
						(freeIds2,f2) <- disposeWElementHandle parentPtr itemH{wItems=[]}
						osInvalidateWindowRect parentPtr (posSizeToRect (wItemPos itemH) (wItemSize itemH))
						return (freeIds2++freeIds1,f1 >> f2,itemNrs)
					| otherwise = do
						(freeIds,f) <- disposeWElementHandle parentPtr itemH
						osInvalidateWindowRect parentPtr (posSizeToRect (wItemPos itemH) (wItemSize itemH))
						return (freeIds,f,wItemNr itemH:itemNrs)

				closeWElementHandle parentPtr (WListLSHandle itemHs) itemNrs =
					closeWElementHandles parentPtr itemHs itemNrs

				closeWElementHandle parentPtr (WExtendLSHandle exLS itemHs) itemNrs =
					closeWElementHandles parentPtr itemHs itemNrs

				closeWElementHandle parentPtr (WChangeLSHandle chLS itemHs) itemNrs =
					closeWElementHandles parentPtr itemHs itemNrs
closeallcontrols _ = windowControlsFatalError "closeallcontrols" "unexpected window placeholder argument"


{-	setcontrolpositions changes the position of the indicated controls.
	It is assumed that the argument WindowStateHandle is either a Window or a Dialog. 
-}
setcontrolpositions :: OSWindowMetrics -> [(Id,ItemPos)] -> WindowStateHandle ps -> IO (Bool,WindowStateHandle ps)
setcontrolpositions wMetrics newPoss wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH@(WindowHandle {whItems=oldItems})})))
	| not (validateNewItemPoss newPoss oldItems) = return (False,wsH)
	| otherwise =
		let
			curw = w winSize-(if visVScroll then osmVSliderWidth  wMetrics else 0)
			curh = h winSize-(if visHScroll then osmHSliderHeight wMetrics else 0)
		  	wFrame = sizeToRect (Size{w=curw,h=curh})
		  	hMargins = getWindowHMargins winKind wMetrics atts
		  	vMargins = getWindowVMargins winKind wMetrics atts
		  	spaces = getWindowItemSpaces winKind wMetrics atts
		  	reqSize	= Size{w=curw-fst hMargins-snd hMargins,h=curh-fst vMargins-snd vMargins}
		  	(_,newItems) = setNewItemPoss newPoss oldItems
		  in do
		  	(_,newItems) <- layoutControls wMetrics hMargins vMargins spaces reqSize zero [(domain,origin)] newItems
		  	let wH1 = wH{whItems=newItems}
			wH2 <- forceValidWindowClipState wMetrics True winPtr wH1
		  	let viewFrame = posSizeToRectangle origin (Size{w=curw,h=curh})
		  	let updState = rectangleToUpdateState viewFrame
		  	let drawbackground = if winKind==IsDialog then return else drawWindowLook wMetrics winPtr (return ()) updState
			wH3 <- drawbackground wH2
			updRgn <- relayoutControls wMetrics select show wFrame wFrame zero zero winPtr defaultId oldItems (whItems wH3)
			wH4 <- updateWindowBackgrounds wMetrics updRgn wids wH3
			osValidateWindowRect winPtr (sizeToRect winSize)
		  	return (True,WindowStateHandle wids (Just wlsH{wlsHandle=wH4}))
	where
		winPtr		= wPtr wids
		atts		= whAtts wH
		winKind		= whKind wH
		winSize		= whSize wH
		select		= whSelect wH
		show		= whShow wH
		defaultId	= whDefaultId wH
		domain		= rectToRectangle domainRect
		(origin,domainRect,hasHScroll,hasVScroll) = case whWindowInfo wH of
			info@(WindowInfo {}) -> (windowOrigin info,windowDomain info,isJust (windowHScroll info),isJust (windowVScroll info))
			NoWindowInfo         -> (zero,             sizeToRect winSize,False,False)
		(visHScroll,visVScroll)	= osScrollbarsAreVisible wMetrics domainRect (toTuple winSize) (hasHScroll,hasVScroll)

		validateNewItemPoss :: [(Id,ItemPos)] -> [WElementHandle ls ps] -> Bool
		validateNewItemPoss idPoss itemHs = null (controlsExist (getids idPoss) itemHs)
			where
				getids :: [(Id,ItemPos)] -> [Id]
				getids ((controlId,(itemLoc,_)):idPoss) = case itemLoc of
					LeftOf	id	-> id:ids
					RightTo	id	-> id:ids
					Above	id	-> id:ids
					Below	id	-> id:ids
					_		-> ids
					where
						ids	= controlId:getids idPoss
				getids _ = []

				controlsExist :: [Id] -> [WElementHandle ls ps] -> [Id]
				controlsExist ids [] = ids
				controlsExist ids (itemH:itemHs)
					| null ids  = []
					| otherwise = controlsExist (controlsExist' ids itemH) itemHs
					where
						controlsExist' :: [Id] -> WElementHandle ls ps -> [Id]
						controlsExist' ids (WItemHandle {wItemId=wItemId,wItems=itemHs}) =
							controlsExist (if isJust wItemId then filter ((==) (fromJust wItemId)) ids else ids) itemHs
						controlsExist' ids (WListLSHandle itemHs) = controlsExist ids itemHs
						controlsExist' ids (WExtendLSHandle exLS itemHs) = controlsExist ids itemHs
						controlsExist' ids (WChangeLSHandle chLS itemHs) = controlsExist ids itemHs

		setNewItemPoss :: [(Id,ItemPos)] -> [WElementHandle ls ps] -> ([(Id,ItemPos)],[WElementHandle ls ps])
		setNewItemPoss idPoss [] = (idPoss,[])
		setNewItemPoss idPoss (itemH:itemHs)
			| null idPoss = (idPoss,itemH:itemHs)
			| otherwise =
				let
				  	(idPoss1,itemH1)  = setNewItemPos' idPoss  itemH
				  	(idPoss2,itemHs1) = setNewItemPoss idPoss1 itemHs
				in
					(idPoss2,itemH1:itemHs1)
			where
				setNewItemPos' :: [(Id,ItemPos)] -> WElementHandle ls ps -> ([(Id,ItemPos)],WElementHandle ls ps)
				setNewItemPos' idPoss itemH@(WItemHandle {wItemKind=itemKind,wItemAtts=atts,wItems=itemHs}) =
					case wItemId itemH of
						Nothing ->
							if isRecursiveControl itemKind
							then let (idPoss1,itemHs1) = setNewItemPoss idPoss itemHs
							     in (idPoss1,itemH{wItems=itemHs1})
							else (idPoss,itemH)
						Just itemId ->
							let
								(found,idPos1,idPoss1) = remove ((==) itemId . fst) undefined idPoss
					  			itemH1 = itemH{wItemAtts=if found then snd (creplace isControlPos (ControlPos (snd idPos1)) atts) else atts}
					  		in
								if isRecursiveControl itemKind
								then let
									(idPoss2,itemHs1) = setNewItemPoss idPoss1 itemHs
						  			itemH2 = itemH1{wItems=itemHs1}
						  			itemH3 = if found && itemKind==IsCompoundControl then invalidateCompoundClipState itemH2 else itemH2
								     in (idPoss2,itemH3)
								else (idPoss1,itemH1)

				setNewItemPos' idPoss (WListLSHandle itemHs) =
					let (idPoss1,itemHs1) = setNewItemPoss idPoss itemHs
					in (idPoss1,WListLSHandle itemHs1)

				setNewItemPos' idPoss (WExtendLSHandle exLS itemHs) =
					let (idPoss1,itemHs1) = setNewItemPoss idPoss itemHs
					in (idPoss1,WExtendLSHandle exLS itemHs1)

				setNewItemPos' idPoss (WChangeLSHandle chLS itemHs) =
					let (idPoss1,itemHs1) = setNewItemPoss idPoss itemHs
					in (idPoss1,WChangeLSHandle chLS itemHs1)

setcontrolpositions _ _ _ = windowControlsFatalError "setcontrolpositions" "unexpected window placeholder argument"
