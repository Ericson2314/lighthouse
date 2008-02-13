-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Layout
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Control.Layout contains the control layout calculation functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Control.Layout ( layoutControls, calcControlsSize ) where


import Prelude hiding (Either(..))	-- Either = Left | Right must be hidden
import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.Layout
import Graphics.UI.ObjectIO.StdControlAttribute
import Graphics.UI.ObjectIO.Window.Access
import Graphics.UI.ObjectIO.OS.Window
import Data.Unique


controlLayoutFatalError :: String -> String -> x
controlLayoutFatalError rule message
	= dumpFatalError rule "controlLayout" message


--	Calculate the precise position (in pixels) of each Control.

layoutControls :: OSWindowMetrics -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Size -> Size -> [(ViewDomain,Point2)] -> [WElementHandle ls ps]
                                                                                                        -> IO (Size, [WElementHandle ls ps])
layoutControls wMetrics hMargins vMargins spaces reqSize minSize orientations itemHs = do
	u <- newUnique
	(layouts,_,_,itemHs2) <- getLayoutItems wMetrics hMargins vMargins spaces orientations [] (toId u) itemHs1;
	let (size,roots)         = layoutItems hMargins vMargins spaces reqSize minSize orientations layouts
	let (_,itemHs3)          = setLayoutItems roots itemHs2
	return (size,itemHs3)
	where
		(_,_,itemHs1) = validateFirstWElementsPos False itemHs

calcControlsSize :: OSWindowMetrics -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Size -> Size -> [(ViewDomain,Point2)] -> [WElementHandle ls ps]
                 -> IO Size
calcControlsSize wMetrics hMargins vMargins spaces reqSize minSize orientations itemHs = do
	u <- newUnique
	(layouts,_,_,_) <- getLayoutItems wMetrics hMargins vMargins spaces orientations [] (toId u) itemHs1;
	return (fst (layoutItems hMargins vMargins spaces reqSize minSize orientations layouts))
	where
		(_,_,itemHs1) = validateFirstWElementsPos False itemHs


{-	validateFirstWElementsPos verifies that the first non line layout, not virtual, WElementHandle either:
	-	already has a layout attribute, or
	-	obtains the (Left,zero) layout attribute if not preceded by a fix or corner WItemHandle.
-}
validateFirstWElementsPos :: Bool -> [WElementHandle ls ps] -> (Bool,Bool,[WElementHandle ls ps])
validateFirstWElementsPos fix_corner_item_found []
	= (False,fix_corner_item_found,[])
validateFirstWElementsPos fix_corner_item_found (itemH:itemHs)
	= let (done,fix_corner_item_found1,itemH1) = validateFirstWElementPos fix_corner_item_found itemH
	  in
	  if   done
	  then (done,fix_corner_item_found1,itemH1:itemHs)
	  else let (done1,fix_corner_item_found2,itemHs1) = validateFirstWElementsPos fix_corner_item_found1 itemHs
	       in  (done1,fix_corner_item_found2,itemH1:itemHs1)
	where
		validateFirstWElementPos :: Bool -> WElementHandle ls ps -> (Bool,Bool,WElementHandle ls ps)
		validateFirstWElementPos fix_corner_item_found itemH@(WItemHandle {wItemVirtual=wItemVirtual,wItemAtts=wItemAtts})
			| wItemVirtual
				= (False,fix_corner_item_found,itemH)
			| not hasPos
				= if   fix_corner_item_found
				  then (True,fix_corner_item_found,itemH)
				  else (True,fix_corner_item_found,itemH {wItemAtts=posAtt:wItemAtts})
			| otherwise
				= let  fix_corner_item_found1 = case pos of
									Fix         -> True
									LeftTop     -> True
									RightTop    -> True
									LeftBottom  -> True
									RightBottom -> True
									_           -> fix_corner_item_found
				       is_line_item           = case pos of
									Left        -> True
									Right       -> True
									Center      -> True
									_           -> False
				  in  (is_line_item,fix_corner_item_found1,itemH)
			where
				(hasPos,posAtt) = cselect isControlPos (ControlPos (Left, {-NoOffset-}zero)) wItemAtts
				pos             = fst (getControlPosAtt posAtt)

		validateFirstWElementPos fix_corner_item_found (WListLSHandle itemHs)
			= let (done,fix_corner_item_found1,itemHs1) = validateFirstWElementsPos fix_corner_item_found itemHs
			  in  (done,fix_corner_item_found1,WListLSHandle itemHs1)

		validateFirstWElementPos fix_corner_item_found (WExtendLSHandle addLS itemHs)
			= let (done,fix_corner_item_found1,itemHs1) = validateFirstWElementsPos fix_corner_item_found itemHs
			  in  (done,fix_corner_item_found1,WExtendLSHandle  addLS itemHs1)

		validateFirstWElementPos fix_corner_item_found (WChangeLSHandle newLS itemHs)
			= let (done,fix_corner_item_found1,itemHs1) = validateFirstWElementsPos fix_corner_item_found itemHs
			  in  (done,fix_corner_item_found1,WChangeLSHandle newLS itemHs1)


{-	Transform the list of WElementHandles to LayoutItem elements and add private information to
	the list of WElementHandles.
	Only the definition fields of the WElementHandle are inspected (except for the recursive
	WElementHandles (WList/WExtend/WChange(LSHandle), and IsCompoundControls)
	which also inspects the recursive elements).
	The recursive LayoutItems of WList/WExtend/WChange(LSHandle)s are flattened.
	In case a control has no Id or an invalid Id (the Id already occurs earlier), then the control
		is provided with a correct ControlId attribute in the attribute list.
		The new ControlId (or the legal ControlId) attribute is placed in front of the attribute
		list in the resulting WElementHandle list. This front position is assumed by
		setLayoutItems!
	In case a control has no ControlPos attribute then it becomes (RightTo previous,NoOffset).
-}
getLayoutItems :: OSWindowMetrics -> (Int,Int) -> (Int,Int) -> (Int,Int) -> [(ViewDomain,Origin)]
                                  -> [Id] -> Id -> [WElementHandle ls ps]
                 -> IO ([LayoutItem],[Id],   Id,   [WElementHandle ls ps])
getLayoutItems wMetrics hMargins vMargins spaces orientations prevIds prevId (itemH:itemHs)
	= do {
		(itPoss1,prevIds1,prevId1,itemH1)  <- getLayoutItem  wMetrics hMargins vMargins spaces orientations prevIds  prevId  itemH;
		(itPoss2,prevIds2,prevId2,itemHs1) <- getLayoutItems wMetrics hMargins vMargins spaces orientations prevIds1 prevId1 itemHs;
		return (itPoss1++itPoss2,prevIds2,prevId2,itemH1:itemHs1)
	  }
	where
		getLayoutItem :: OSWindowMetrics -> (Int,Int) -> (Int,Int) -> (Int,Int) -> [(ViewDomain,Origin)]
		                       -> [Id] -> Id -> WElementHandle ls ps
		      -> IO ([LayoutItem],[Id],   Id,   WElementHandle ls ps)


		getLayoutItem wMetrics hMargins vMargins spaces orientations prevIds prevId (WExtendLSHandle addLS itemHs)
			= do {
				(itPoss,prevIds1,prevId1,itemHs1) <- getLayoutItems wMetrics hMargins vMargins spaces orientations prevIds prevId itemHs;
				return (itPoss,prevIds1,prevId1,WExtendLSHandle addLS itemHs1)
			  }

		getLayoutItem wMetrics hMargins vMargins spaces orientations prevIds prevId (WChangeLSHandle newLS itemHs)
			= do {
				(itPoss,prevIds1,prevId1,itemHs1) <- getLayoutItems wMetrics hMargins vMargins spaces orientations prevIds prevId itemHs;
				return (itPoss,prevIds1,prevId1,WChangeLSHandle newLS itemHs1)
			  }

		getLayoutItem wMetrics hMargins vMargins spaces orientations prevIds prevId (WListLSHandle itemHs)
			= do {
				(itPoss,prevIds1,prevId1,itemHs1) <- getLayoutItems wMetrics hMargins vMargins spaces orientations prevIds prevId itemHs;
				return (itPoss,prevIds1,prevId1,WListLSHandle itemHs1)
			  }

		getLayoutItem wMetrics hMargins vMargins spaces orientations prevIds prevId itemH@(WItemHandle {wItemVirtual=wItemVirtual})
			| wItemVirtual = return ([],prevIds,prevId,itemH)
			| otherwise
			= do {
				(itPos,prevIds1,prevId1,itemH1) <- getLayoutWItem wMetrics hMargins vMargins spaces orientations prevIds prevId itemH;
				return ([itPos],prevIds1,prevId1,itemH1)
			  }
			where
				getLayoutWItem :: OSWindowMetrics -> (Int,Int) -> (Int,Int) -> (Int,Int) -> [(ViewDomain,Origin)]
				                          -> [Id] -> Id -> WElementHandle ls ps
				           -> IO (LayoutItem,[Id],   Id,   WElementHandle ls ps)

				getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId itemH@(WItemHandle {wItemKind=IsButtonControl,wItemAtts=atts}) = do
					(id,prevIds1) <- getLayoutItemId (wItemId itemH) prevIds
					let itPos = newLayoutItem id pos (wItemSize itemH)
					return (itPos,prevIds1,id,itemH {wItemAtts=(ControlId id):atts})
					where
						pos           = getLayoutItemPos prevId atts

				getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId itemH@(WItemHandle {wItemKind=IsCustomButtonControl,wItemAtts=atts}) = do
					(id,prevIds1) <- getLayoutItemId (wItemId itemH) prevIds
					let itPos = newLayoutItem id pos (checkCustomSize (wItemSize itemH))
					return (itPos,prevIds1,id,itemH {wItemAtts=(ControlId id):atts})
					where
						pos           = getLayoutItemPos prevId atts						

				getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId itemH@(WItemHandle {wItemKind=IsTextControl,wItemAtts=atts}) = do
					(id,prevIds1) <- getLayoutItemId (wItemId itemH) prevIds
					let itPos = newLayoutItem id pos (wItemSize itemH)
					return (itPos,prevIds1,id,itemH {wItemAtts=(ControlId id):atts})
					where
						pos           = getLayoutItemPos prevId atts

				getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId itemH@(WItemHandle {wItemKind=IsEditControl,wItemAtts=atts}) = do
					(id,prevIds1) <- getLayoutItemId (wItemId itemH) prevIds
					let itPos = newLayoutItem id pos (wItemSize itemH)
					return (itPos,prevIds1,id,itemH {wItemAtts=(ControlId id):atts})
					where						
						pos           = getLayoutItemPos prevId atts

				getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId itemH@(WItemHandle{wItemKind=IsPopUpControl,wItemAtts=atts}) = do
					(id,prevIds1) <- getLayoutItemId (wItemId itemH) prevIds
					let itPos = newLayoutItem id pos (wItemSize itemH)
					return (itPos,prevIds1,id,itemH {wItemAtts=(ControlId id):atts})
					where						
						pos           = getLayoutItemPos prevId atts
						
				getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId itemH@(WItemHandle{wItemKind=IsListBoxControl,wItemAtts=atts}) = do
					(id,prevIds1) <- getLayoutItemId (wItemId itemH) prevIds
					let itPos = newLayoutItem id pos (wItemSize itemH)
					return (itPos,prevIds1,id,itemH {wItemAtts=(ControlId id):atts})
					where						
						pos           = getLayoutItemPos prevId atts

				getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId itemH@(WItemHandle {wItemKind=IsRadioControl,wItemAtts=atts,wItemInfo=wItemInfo})
					| null items
						= controlLayoutFatalError "RadioControl definition" "Empty list of RadioItem-s"
					| otherwise
						= let
							colitemsizes     = toColumns layout items
							colwidths        = map (map (\RadioItemInfo{radioItemSize=Size{w=w}}->w)) colitemsizes
							colmaxwidths     = map maximum colwidths
							width            = sum colmaxwidths
							height           = itemHeight*(length (head colwidths))							
							collaynoutitems  = position_items (\pos item->item{radioItemPos=pos}) itemHeight 0 colmaxwidths colitemsizes
							laynoutitems     = fromColumns layout collaynoutitems
							info'            = info{radioItems=laynoutitems}
						  in do
						  	(id,prevIds1) <- getLayoutItemId (wItemId itemH) prevIds
						  	let itPos = newLayoutItem id pos Size{w=width,h=height}
						  	return (itPos,prevIds1,id,itemH {wItemAtts=(ControlId id):atts,wItemInfo=WRadioInfo info'})
					where
						pos                 = getLayoutItemPos prevId atts						
						itemHeight          = osGetRadioControlItemHeight wMetrics
						info                = getWItemRadioInfo wItemInfo
						items               = radioItems  info
						layout              = radioLayout info

				getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId itemH@(WItemHandle {wItemKind=IsCheckControl,wItemAtts=atts,wItemInfo=wItemInfo})
					| null items
						= controlLayoutFatalError "CheckControl definition" "Empty list of CheckItem-s"
					| otherwise
						= let
							colitemsizes    = toColumns layout items
							colwidths       = map (map (w . checkItemSize)) colitemsizes
							colmaxwidths    = map maximum colwidths
							width           = sum colmaxwidths
							height          = itemHeight*(length (head colwidths))
							collaynoutitems = position_items (\pos item->item{checkItemPos=pos}) itemHeight 0 colmaxwidths colitemsizes
							laynoutitems    = fromColumns layout collaynoutitems
							info'           = info{checkItems=laynoutitems}
						  in do
						  	(id,prevIds1) <- getLayoutItemId (wItemId itemH) prevIds
						  	let itPos = newLayoutItem id pos Size{w=width,h=height}
						  	return (itPos,prevIds1,id,itemH {wItemAtts=(ControlId id):atts,wItemInfo=WCheckInfo info'})
					where
						pos           = getLayoutItemPos prevId atts						
						itemHeight    = osGetCheckControlItemHeight wMetrics
						info          = getWItemCheckInfo wItemInfo
						items         = checkItems info
						layout        = checkLayout info

				getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId itemH@(WItemHandle {wItemKind=IsCustomControl,wItemAtts=atts}) = do
					(id,prevIds1) <- getLayoutItemId (wItemId itemH) prevIds
					let itPos = newLayoutItem id pos (checkCustomSize (wItemSize itemH))
					return (itPos,prevIds1,id,itemH {wItemAtts=(ControlId id):atts})
					where
						pos           = getLayoutItemPos prevId atts

				getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId itemH@(WItemHandle {wItemKind=IsSliderControl,wItemAtts=atts}) = do
					(id,prevIds1) <- getLayoutItemId (wItemId itemH) prevIds
					let itPos = newLayoutItem id pos (wItemSize itemH)
					return (itPos,prevIds1,id,itemH{wItemAtts=(ControlId id):atts})
					where
						pos           = getLayoutItemPos prevId atts

				getLayoutWItem wMetrics hMargins vMargins spaces _ prevIds prevId itemH@(WItemHandle {wItemKind=IsReceiverControl,wItemAtts=atts}) = do
					(id,prevIds1) <- getLayoutItemId (wItemId itemH) prevIds
					let itPos = newLayoutItem id pos (wItemSize itemH)
					return (itPos,prevIds1,id,itemH {wItemAtts=(ControlId id):atts})
					where						
						pos           = getLayoutItemPos prevId atts

				getLayoutWItem wMetrics hMargins vMargins spaces orientations prevIds prevId
				               itemH@(WItemHandle {wItemKind=IsCompoundControl,wItemAtts=atts,wItems=wItems}) = do
					(size,info1,items,atts1) <- calcCompoundSize wMetrics hMargins vMargins spaces orientations info wItems atts
					(id,prevIds1) <- getLayoutItemId (wItemId itemH) prevIds
					let itPos = newLayoutItem id pos size
					return (itPos,prevIds1,id,itemH {wItemAtts=(ControlId id:atts1),wItems=items,wItemInfo=WCompoundInfo info1})
					where
						info          = getWItemCompoundInfo (wItemInfo itemH)						
						pos           = getLayoutItemPos prevId atts

						calcCompoundSize :: OSWindowMetrics -> (Int,Int) -> (Int,Int) -> (Int,Int) -> [(ViewDomain,Origin)]
						                          -> CompoundInfo -> [WElementHandle ls ps] -> [ControlAttribute ls ps]
						                 -> IO (Size,CompoundInfo,   [WElementHandle ls ps],   [ControlAttribute ls ps])
						calcCompoundSize wMetrics hMargins@(lMargin,rMargin) vMargins@(tMargin,bMargin) spaces orientations info itemHs atts
							= do {
								(derSize,itemHs1) <- layoutControls wMetrics newHMargins newVMargins newItemSpaces reqSize minSize newOrientations itemHs;
								let okDerivedSize  = validateDerivedCompoundSize wMetrics domainRect (hasHScroll,hasVScroll) derSize reqSize
								    info1          = layoutScrollbars wMetrics okDerivedSize info
								in  return (okDerivedSize,info1,itemHs1,if hadSize then atts2 else (ControlViewSize derSize):atts2)
							  }
							where
								origin          = compoundOrigin info
								domainRect      = compoundDomain info
								hasHScroll      = isJust (compoundHScroll info)
								hasVScroll      = isJust (compoundVScroll info)
								(minSize,atts1) = validateMinSize atts
								(hadSize,reqSize,atts2)
								                = validateCompoundSize wMetrics (rectToRectangle domainRect) (hasHScroll,hasVScroll) atts1
								(_,hMarginAtt)  = cselect isControlHMargin (ControlHMargin lMargin rMargin) atts2
								newHMargins     = validateControlMargin (getControlHMarginAtt hMarginAtt)
								(_,vMarginAtt)  = cselect isControlVMargin (ControlVMargin tMargin bMargin) atts2
								newVMargins     = validateControlMargin (getControlVMarginAtt vMarginAtt)
								(_,spaceAtt)    = cselect isControlItemSpace (ControlItemSpace (fst spaces) (snd spaces)) atts2
								newItemSpaces   = validateControlItemSpace (getControlItemSpaceAtt spaceAtt)
								domain          = rectToRectangle domainRect
								newOrientations = (domain,origin):orientations

								validateMinSize :: [ControlAttribute ls ps] -> (Size,[ControlAttribute ls ps])
								validateMinSize atts
									= (okMinSize,if hadMinSize then (ControlMinimumSize okMinSize):atts1 else atts1)
									where
										(defMinW,defMinH)         = osMinCompoundSize
										(hadMinSize,minAtt,atts1) = remove isControlMinimumSize (ControlMinimumSize (Size {w=defMinW,h=defMinH})) atts
										minSize                   = getControlMinimumSizeAtt minAtt
										okMinSize                 = Size {w=max defMinW (w minSize),h=max defMinH (h minSize)}

							{-	validateCompoundSize wMetrics viewDomain (hasHScroll,hasVScroll) atts
									validates the Control(View/Outer)Size attribute. The Boolean result is True iff the attribute list contains
									the Control(View/Outer)Size attribute.
									The Booleans hasHScroll hasVScroll should be True iff the compound has the ControlHScroll, ControlVScroll
									attribute set respectively.
									In addition, the ControlOuterSize attribute is mapped to ControlViewSize attribute.
							-}
								validateCompoundSize :: OSWindowMetrics -> ViewDomain -> (Bool,Bool) -> [ControlAttribute ls ps]
								                                                          -> (Bool,Size,[ControlAttribute ls ps])
								validateCompoundSize wMetrics domain hasScrolls atts
									| not hasSize
										= (False,zero,atts)
									| isControlViewSize sizeAtt =
										let
											size  = getControlViewSizeAtt sizeAtt
											size1 = Size {w=max (w size) (fst minSize),h=max (h size) (snd minSize)}
										in (True,size1,snd (creplace isControlViewSize (ControlViewSize size1) atts))
									| otherwise =
										let
											(w,h)       = toTuple (getControlOuterSizeAtt sizeAtt)
											visScrolls  = osScrollbarsAreVisible wMetrics (rectangleToRect domain) (w,h) hasScrolls
											viewSize    = rectSize (getCompoundContentRect wMetrics visScrolls (sizeToRect (Size {w=w,h=h})))
											(_,_,atts1) = remove isControlOuterSize undefined atts
											(_,_,atts2) = remove isControlViewSize  undefined atts1
										in (True,viewSize,(ControlViewSize viewSize):atts2)
									where
										(hasSize,sizeAtt) = cselect (\att->isControlViewSize att || isControlOuterSize att) undefined atts
										minSize           = osMinCompoundSize

								validateDerivedCompoundSize :: OSWindowMetrics -> Rect -> (Bool,Bool) -> Size -> Size -> Size
								validateDerivedCompoundSize wMetrics domain hasScrolls derSize reqSize
									| reqSize==zero = validateScrollbarSize wMetrics domain hasScrolls derSize
									| otherwise     = validateScrollbarSize wMetrics domain hasScrolls reqSize
									where
										validateScrollbarSize :: OSWindowMetrics -> Rect -> (Bool,Bool) -> Size -> Size
										validateScrollbarSize wMetrics domainRect (hasHScroll,hasVScroll) size@(Size {w=w,h=h})
											| domainSize==zero         = size
											| visHScroll && visVScroll = Size {w=w',h=h'}
											| visHScroll               = size {h=h'}
											| visVScroll               = size {w=w'}
											| otherwise                = size
											where
												domainSize = rectSize domainRect
												visHScroll = hasHScroll && osScrollbarIsVisible (rleft domainRect,rright domainRect)  w
												visVScroll = hasVScroll && osScrollbarIsVisible (rtop domainRect, rbottom domainRect) h
												(w',h')    = (w+osmVSliderWidth wMetrics,h+osmHSliderHeight wMetrics)

				getLayoutWItem wMetrics hMargins vMargins spaces orientations prevIds prevId
				               itemH@(WItemHandle {wItemKind=IsLayoutControl,wItemAtts=atts,wItems=wItems}) = do
					(size,items,atts1) <- calcLayoutSize wMetrics hMargins vMargins spaces orientations wItems atts					
					(id,prevIds1) <- getLayoutItemId (wItemId itemH) prevIds
					let itPos = newLayoutItem id pos size
					return (itPos,prevIds1,id,itemH {wItemAtts=(ControlId id):atts1,wItems=items})
					where
						pos           = getLayoutItemPos prevId atts

						calcLayoutSize :: OSWindowMetrics -> (Int,Int) -> (Int,Int) -> (Int,Int) -> [(ViewDomain,Origin)]
						                                  -> [WElementHandle ls ps] -> [ControlAttribute ls ps]
						                         -> IO (Size,[WElementHandle ls ps],   [ControlAttribute ls ps])
						calcLayoutSize wMetrics hMargins@(lMargin,rMargin) vMargins@(tMargin,bMargin) spaces orientations itemHs atts
							= do {
								(derSize,itemHs1) <- layoutControls wMetrics newHMargins newVMargins newItemSpaces reqSize minSize orientations itemHs;
								let okDerivedSize  = validateDerivedLayoutSize wMetrics derSize reqSize
								in  return (okDerivedSize,itemHs1,if hadSize then atts2 else (ControlViewSize okDerivedSize):atts2)
							  }
							where
								(minSize,atts1)         = validateMinSize atts
								(hadSize,reqSize,atts2) = validateLayoutSize wMetrics atts1
								(_,hMarginAtt)          = cselect isControlHMargin (ControlHMargin lMargin rMargin) atts2
								newHMargins             = validateControlMargin (getControlHMarginAtt hMarginAtt)
								(_,vMarginAtt)          = cselect isControlVMargin (ControlVMargin tMargin bMargin) atts2
								newVMargins             = validateControlMargin (getControlVMarginAtt vMarginAtt)
								(_,spaceAtt)            = cselect isControlItemSpace (ControlItemSpace (fst spaces) (snd spaces)) atts2
								newItemSpaces           = validateControlItemSpace (getControlItemSpaceAtt spaceAtt)

								validateMinSize :: [ControlAttribute ls ps] -> (Size,[ControlAttribute ls ps])
								validateMinSize atts
									= (okMinSize,if hadMinSize then (ControlMinimumSize okMinSize):atts1 else atts1)
									where
										(hadMinSize,minAtt,atts1) = remove isControlMinimumSize (ControlMinimumSize zero) atts
										minSize                   = getControlMinimumSizeAtt minAtt
										okMinSize                 = Size{w=max 0 (w minSize),h=max 0 (h minSize)}

							{-	validateLayoutSize wMetrics atts
									validates the Control(View/Outer)Size attribute. The Boolean result is True iff the attribute list contains
									the Control(View/Outer)Size attribute.
									In addition, the ControlOuterSize attribute is mapped to ControlViewSize attribute (identical value).
							-}
								validateLayoutSize :: OSWindowMetrics -> [ControlAttribute ls ps]
								                           -> (Bool,Size,[ControlAttribute ls ps])
								validateLayoutSize wMetrics atts
									| not hasSize
										= (False,zero,atts)
									| otherwise =
										let size1       = Size{w=max (w size) 0,h=max (h size) 0}
										    (_,_,atts1) = remove isControlOuterSize undefined atts
										    (_,_,atts2) = remove isControlViewSize  undefined atts1
										in (True,size1,(ControlViewSize size1):atts2)
									where
										(hasSize,sizeAtt) = cselect (\att->isControlViewSize att || isControlOuterSize att) undefined atts
										size              = if   isControlViewSize sizeAtt
										                    then getControlViewSizeAtt  sizeAtt
										                    else getControlOuterSizeAtt sizeAtt

				getLayoutWItem _ _ _ _ _ _ _ _
					= controlLayoutFatalError "getLayoutWItem" "unmatched control implementation alternative"

				getLayoutItemPos :: Id -> [ControlAttribute ls ps] -> ItemPos
				getLayoutItemPos prevId atts
					= (itemLoc1,offset)
					where
						(itemLoc,offset) = getControlPosAtt (snd (cselect isControlPos (ControlPos (RightTo prevId,{-NoOffset-}zero)) atts))
						itemLoc1         = if   isRelativeToPrev itemLoc
						                   then setRelativeTo prevId itemLoc
						                   else itemLoc

getLayoutItems _ _ _ _ _ prevIds prevId []
	= return ([],prevIds,prevId,[])


{-	Functions shared above.
-}
validateControlMargin :: (Int,Int) -> (Int,Int)
validateControlMargin (a,b) = (max 0 a,max 0 b)

validateControlItemSpace :: (Int,Int) -> (Int,Int)
validateControlItemSpace (hspace,vspace) = (max 0 hspace,max 0 vspace)

validateDerivedLayoutSize :: OSWindowMetrics -> Size -> Size -> Size
validateDerivedLayoutSize wMetrics derSize reqSize
	| reqSize==zero = derSize
	| otherwise     = reqSize

--	Only used for CompoundControls
layoutScrollbars :: OSWindowMetrics -> Size -> CompoundInfo -> CompoundInfo
layoutScrollbars wMetrics size info@(CompoundInfo {compoundHScroll=compoundHScroll,compoundVScroll=compoundVScroll})
	= info { compoundHScroll=fmap (layoutScrollbar hRect) compoundHScroll
	       , compoundVScroll=fmap (layoutScrollbar vRect) compoundVScroll
	       }
	where
		hasScrolls = (isJust compoundHScroll,isJust compoundVScroll)	-- PA: this should actually become: (visHScroll,visVScroll)!!
		rect       = sizeToRect size
		hRect      = getCompoundHScrollRect wMetrics hasScrolls rect
		vRect      = getCompoundVScrollRect wMetrics hasScrolls rect

		layoutScrollbar :: Rect -> ScrollInfo -> ScrollInfo
		layoutScrollbar r scrollInfo
			= scrollInfo {scrollItemPos=Point2{x=rleft r,y=rtop r},scrollItemSize=rectSize r}


position_items :: (Point2 -> x -> x) -> Int -> Int -> [Int] -> [[x]] -> [[x]]
position_items setPosition itemHeight left (maxwidth:maxwidths) (col:cols)
	= let
		col1  = position_items' setPosition itemHeight (Point2 {x=left,y=0}) col
		cols1 = position_items  setPosition itemHeight (left+maxwidth) maxwidths cols
	  in col1:cols1
	where
		position_items' :: (Point2 -> x -> x) -> Int -> Point2 -> [x] -> [x]
		position_items' setPosition itemHeight pos (item:items)
			= (setPosition pos item) : (position_items' setPosition itemHeight (pos {y=y pos+itemHeight}) items)
		position_items' _ _ _ _
			= []
position_items _ _ _ _ _
	= []

toColumns :: RowsOrColumns -> [x] -> [[x]]
toColumns (Columns n) items
	= repeat_splitting perColumn items
	where
		nrItems   = length items
		n'        = max 1 n
		perColumn = if (nrItems `rem` n') == 0 then nrItems `div` n' else (nrItems `div` n') + 1

		repeat_splitting :: Int -> [x] -> [[x]]
		repeat_splitting n items
			| null after  = [before]
			| otherwise   = before : repeat_splitting n after
			where
				(before,after) = split n items
toColumns (Rows n) items
	= repeat_spreading nrColumns items cols
	where
		nrItems   = length items
		n'        = setBetween n 1 nrItems
		nrColumns = if (nrItems `rem` n') == 0 then nrItems `div` n' else (nrItems `div` n') + 1
		cols      = take nrColumns (repeat [])

		repeat_spreading :: Int -> [x] -> [[x]] -> [[x]]
		repeat_spreading n items cols
			| null after = spread before cols
			| otherwise  = spread before (repeat_spreading n after cols)
			where
				(before,after) = split n items

				spread :: [a] -> [[a]] -> [[a]]
				spread (x:xs) (ys:zs) = (x:ys):spread xs zs
				spread [] zs          = zs

fromColumns :: RowsOrColumns -> [[x]] -> [x]
fromColumns (Columns _) items
	= concat items
fromColumns (Rows _) items
	= repeat_collecting items
	where
		repeat_collecting :: [[x]] -> [x]
		repeat_collecting items
			| null after = before
			| otherwise  = before ++ repeat_collecting after
			where
				(before,after) = collect items

				collect :: [[x]] -> ([x],[[x]])
				collect ((x:xs):ys)
					= let (zs,ys1) = collect ys
					  in  (x:zs,xs:ys1)
				collect _
					= ([],[])

checkCustomSize :: Size -> Size
checkCustomSize size = Size {w=max 0 (w size),h=max 0 (h size)}

getLayoutItemId :: Maybe Id -> [Id] -> IO (Id,[Id])
getLayoutItemId maybeId prevIds = 
	case maybeId of
		Nothing -> do
			u <- newUnique			
			return (toId u,prevIds)
		Just id | id `elem` prevIds -> do			
			u <- newUnique
			return (toId u,prevIds)			
		Just id -> return (id, id:prevIds)

newLayoutItem :: Id -> ItemPos -> Size -> LayoutItem
newLayoutItem id pos size
	= LayoutItem {liId=id,liItemPos=pos,liItemSize=size}

isRelativeToPrev :: ItemLoc -> Bool
isRelativeToPrev itemLoc = itemLoc==LeftOfPrev || itemLoc==RightToPrev || itemLoc==AbovePrev || itemLoc==BelowPrev

setRelativeTo :: Id -> ItemLoc -> ItemLoc
setRelativeTo id LeftOfPrev  = LeftOf  id
setRelativeTo id RightToPrev = RightTo id
setRelativeTo id AbovePrev   = Above   id
setRelativeTo id BelowPrev   = Below   id


{-	After calculating the layout of the elements, the calculated control positions and sizes must be added
	to the original WElementHandle list.
	In case of recursive WElementHandles (WList/WExtend/WChange(LSHandle), and (Compound/Layout)Controls)
	the recursively calculated positions must also be added.
	In case of (Radio/Check)Controls the already calculated (radio/check) control positions that are
	oriented at base zero need to be shifted to the calculated base position of the (Radio/Check)Control.

	Note that setLayoutItems relies on the fact that the hd element of the attribute
	list contains the ControlId attribute, used for computing the layout (as provided by
	getLayoutItems)! It will remove this element from the attribute list.
-}
setLayoutItems :: [Root] -> [WElementHandle ls ps] -> ([Root],[WElementHandle ls ps])
setLayoutItems roots (itemH:itemHs)
	= let
		(roots1,itemH1 ) = setLayoutItem  roots  itemH
		(roots2,itemHs1) = setLayoutItems roots1 itemHs
	  in (roots2,itemH1:itemHs1)
	where
		setLayoutItem :: [Root] -> WElementHandle ls ps -> ([Root],WElementHandle ls ps)

		setLayoutItem roots (WExtendLSHandle addLS itemHs)
			= let (roots1,itemHs1) = setLayoutItems roots itemHs
			  in  (roots1,WExtendLSHandle addLS itemHs1)

		setLayoutItem roots (WChangeLSHandle newLS itemHs)
			= let (roots1,itemHs1) = setLayoutItems roots itemHs
			  in  (roots1,WChangeLSHandle newLS itemHs1)

		setLayoutItem roots (WListLSHandle itemHs)
			= let (roots1,itemHs1) = setLayoutItems roots itemHs
			  in  (roots1,WListLSHandle itemHs1)

		setLayoutItem roots itemH@(WItemHandle {wItemVirtual=wItemVirtual})
			| wItemVirtual = (roots,itemH)
			| otherwise
			= setLayoutWItem roots itemH
			where
				setLayoutWItem :: [Root] -> WElementHandle ls ps -> ([Root],WElementHandle ls ps)
				setLayoutWItem roots itemH@(WItemHandle {wItemAtts=(ControlId id:atts)})
					= ( roots1
					  , itemH { wItemAtts       = atts
					          , wItemInfo       = info
					          , wItems          = itemHs
					          , wItemPos        = Point2 {x=vx corner,y=vy corner}
					          , wItemSize       = size
					          , wItemLayoutInfo = layoutInfo
					          }
					  )
					where
						(layoutInfo,corner,size,roots1) = getLayoutItem id roots
						itemHs                          = map (shiftCompounds layoutInfo corner) (wItems itemH)
						info                            = shiftWItemInfo corner (wItemInfo itemH)

						shiftCompounds :: LayoutInfo -> Vector2 -> WElementHandle ls ps -> WElementHandle ls ps
						shiftCompounds layoutInfo offset itemH@(WItemHandle{wItemPos=wItemPos,wItemInfo=wItemInfo,wItemLayoutInfo=wItemLayoutInfo,wItems=wItems})
							= itemH { wItemPos        = movePoint offset wItemPos
							        , wItems          = itemHs
							        , wItemInfo       = info
							        , wItemLayoutInfo = layoutInfo
							        }
							where
								layoutInfo1 = if layoutInfo==LayoutFix then layoutInfo else wItemLayoutInfo
								itemHs      = map (shiftCompounds layoutInfo1 offset) wItems
								info        = shiftWItemInfo offset wItemInfo

						shiftCompounds layoutInfo offset (WListLSHandle itemHs)
							= WListLSHandle (map (shiftCompounds layoutInfo offset) itemHs)

						shiftCompounds layoutInfo offset (WExtendLSHandle addLS itemHs)
							= WExtendLSHandle addLS (map (shiftCompounds layoutInfo offset) itemHs)

						shiftCompounds layoutInfo offset (WChangeLSHandle newLS itemHs)
							= WChangeLSHandle newLS (map (shiftCompounds layoutInfo offset) itemHs)

						shiftWItemInfo :: Vector2 -> WItemInfo ls ps -> WItemInfo ls ps
						shiftWItemInfo offset (WCheckInfo info)
							= WCheckInfo (info{checkItems=map shiftCheckItem (checkItems info)})
							where
								shiftCheckItem :: CheckItemInfo ls ps -> CheckItemInfo ls ps
								shiftCheckItem item
									= item {checkItemPos=movePoint offset (checkItemPos item)}
						shiftWItemInfo offset (WRadioInfo info)
							= WRadioInfo (info{radioItems=map shiftRadioItem (radioItems info)})
							where
								shiftRadioItem :: RadioItemInfo ls ps -> RadioItemInfo ls ps
								shiftRadioItem item
									= item {radioItemPos=movePoint offset (radioItemPos item)}
						shiftWItemInfo offset (WCompoundInfo info)
							= WCompoundInfo (info{compoundHScroll=fmap shiftScrollbar (compoundHScroll info)
							       , compoundVScroll=fmap shiftScrollbar (compoundVScroll info)
							       })
							where
								shiftScrollbar :: ScrollInfo -> ScrollInfo
								shiftScrollbar info
									= info {scrollItemPos=movePoint offset (scrollItemPos info)}
						shiftWItemInfo _ info
							= info

				setLayoutWItem _ _
					= controlLayoutFatalError "setLayoutWItem" "WElementHandle has no ControlId"

setLayoutItems roots itemHs
	= (roots,itemHs)
