-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Create
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Control.Create contains all control creation functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Control.Create ( createControls, createCompoundControls ) where



import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.StdControlAttribute
import Graphics.UI.ObjectIO.Control.Layout
import Graphics.UI.ObjectIO.Control.Validate
import Graphics.UI.ObjectIO.Window.Access
import Graphics.UI.ObjectIO.OS.Window
import Graphics.UI.ObjectIO.OS.ToolTip(osAddControlToolTip)



{-	createControls generates the proper system resources for all given WElementHandles of the window.
-}
createControls :: OSWindowMetrics -> Maybe Id -> Maybe Id -> Bool -> OSWindowPtr -> [WElementHandle ls ps]
                                                                              -> IO [WElementHandle ls ps]
createControls wMetrics okId cancelId ableContext wPtr itemHs
	= sequenceMap (createWElementHandle wMetrics okId cancelId True ableContext zero wPtr) itemHs


{-	createCompoundControls generates the proper system resources for those controls that are part of the 
	CompoundControl with the given Id, skipping its first nr of controls given by the Int argument.
	The WElementHandles must be the complete list of controls of the window.
-}
createCompoundControls :: OSWindowMetrics -> Id -> Int -> Maybe Id -> Maybe Id -> Bool -> OSWindowPtr -> [WElementHandle ls ps] -> IO [WElementHandle ls ps]
createCompoundControls wMetrics compoundId nrSkip okId cancelId ableContext wPtr itemHs =
	mapM (createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId True ableContext zero wPtr) itemHs
	where
		createCompoundWElementHandle :: OSWindowMetrics -> Id -> Int -> Maybe Id -> Maybe Id -> Bool -> Bool -> Point2 -> OSWindowPtr -> WElementHandle ls ps -> IO (WElementHandle ls ps)
		createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext ableContext parentPos wPtr itemH@(WItemHandle {wItemKind=itemKind,wItemId=itemId,wItems=itemHs})
			| itemKind /= IsCompoundControl =
				if isRecursiveControl itemKind
				then do
					itemHs <- mapM (createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext1 ableContext parentPos wPtr) itemHs
					return itemH{wItems=itemHs}
				else return itemH
			| not (identifyMaybeId compoundId itemId) = do
				itemHs <- mapM (createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext1 ableContext1 itemPos itemPtr) itemHs
				return itemH{wItems=itemHs}
			| otherwise = do
				let (oldItems,newItems)	= split nrSkip itemHs
				newItems <- mapM (createWElementHandle wMetrics okId cancelId showContext1 ableContext1 itemPos itemPtr) newItems
				osInvalidateCompound itemPtr
				return itemH{wItems=oldItems++newItems}
			where
				showContext1	= showContext && wItemShow   itemH
				ableContext1	= ableContext && wItemSelect itemH
				itemPos		= wItemPos itemH
				itemPtr		= wItemPtr itemH

		createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext ableContext parentPos wPtr (WListLSHandle itemHs) = do
			itemHs <- mapM (createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext ableContext parentPos wPtr) itemHs
			return (WListLSHandle itemHs)

		createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext ableContext parentPos wPtr (WExtendLSHandle exLS itemHs) = do
			itemHs <- mapM (createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext ableContext parentPos wPtr) itemHs
			return (WExtendLSHandle exLS itemHs)

		createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext ableContext parentPos wPtr (WChangeLSHandle chLS itemHs) = do
			itemHs <- mapM (createCompoundWElementHandle wMetrics compoundId nrSkip okId cancelId showContext ableContext parentPos wPtr) itemHs
			return (WChangeLSHandle chLS itemHs)
		

{-	toOKorCANCEL okId cancelId controlId
		checks if the optional Id of a control (controlId) is the OK control (OK), the CANCEL control (CANCEL), or a normal button (NORMAL).
-}
toOKorCANCEL :: Maybe Id -> Maybe Id -> Maybe Id -> OKorCANCEL
toOKorCANCEL okId cancelId maybeControlId
	= case maybeControlId of
		Just id -> if      isJust okId     && fromJust okId    ==id then OK
		           else if isJust cancelId && fromJust cancelId==id then CANCEL
		                                                            else NORMAL
		nothing -> NORMAL


{-	createWElementHandle generates the proper system resources.
-}
createWElementHandle :: OSWindowMetrics -> Maybe Id -> Maybe Id -> Bool -> Bool -> Point2 -> OSWindowPtr -> WElementHandle ls ps
                                                                                                     -> IO (WElementHandle ls ps)

createWElementHandle wMetrics okId cancelId showContext ableContext parentPos wPtr (WListLSHandle itemHs)
	= do {	itemHs1 <- sequenceMap (createWElementHandle wMetrics okId cancelId showContext ableContext parentPos wPtr) itemHs;
	  	return (WListLSHandle itemHs1)
	  }

createWElementHandle wMetrics okId cancelId showContext ableContext parentPos wPtr (WExtendLSHandle addLS itemHs)
	= do {	itemHs1 <- sequenceMap (createWElementHandle wMetrics okId cancelId showContext ableContext parentPos wPtr) itemHs;
		return (WExtendLSHandle addLS itemHs1)
	  }

createWElementHandle wMetrics okId cancelId showContext ableContext parentPos wPtr (WChangeLSHandle newLS itemHs)
	= do {	itemHs1 <- sequenceMap (createWElementHandle wMetrics okId cancelId showContext ableContext parentPos wPtr) itemHs;
	  	return (WChangeLSHandle newLS itemHs1)
	  }

createWElementHandle wMetrics okId cancelId showContext ableContext parentPos wPtr itemH@(WItemHandle {wItemKind=wItemKind})
	| wItemKind==IsRadioControl
		= let
			radioInfo = getWItemRadioInfo (wItemInfo itemH)

			createRadioItem :: Bool -> Bool -> (Int,Int) -> OSWindowPtr -> Index -> RadioItemInfo ls ps -> Index
											 -> IO (RadioItemInfo ls ps,   Index)
			createRadioItem show able parentPos wPtr index item@(RadioItemInfo {radioItem=(title,_,_),radioItemPos=pos,radioItemSize=size}) itemNr
				= do {
					radioPtr <- osCreateRadioControl wPtr parentPos title show able (toTuple pos) (toTuple size) (index==itemNr) (itemNr==1);
					let itemH1 = item {radioItemPtr=radioPtr}
					in  if   hasTip
					    then osAddControlToolTip wPtr radioPtr tip >> return (itemH1,itemNr+1)
					    else return (itemH1,itemNr+1)
		  		}
		  in
			do {
				(items,_) <- stateMapM (createRadioItem show able (toTuple parentPos) wPtr (radioIndex radioInfo)) (radioItems radioInfo) 1;
				return itemH {wItemInfo=WRadioInfo (radioInfo{radioItems=items})}
		  	}

	| wItemKind==IsCheckControl
 		= let
 			checkInfo = getWItemCheckInfo (wItemInfo itemH)

			createCheckItem :: Bool -> Bool -> (Int,Int) -> OSWindowPtr -> CheckItemInfo ls ps -> Index
					                                                        -> IO (CheckItemInfo ls ps,   Index)
			createCheckItem show able parentPos wPtr item@(CheckItemInfo {checkItem=(title,_,mark,_),checkItemPos=pos,checkItemSize=size}) itemNr
				= do {
					checkPtr <- osCreateCheckControl wPtr parentPos title show able (toTuple pos) (toTuple size) (marked mark) (itemNr==1);
					let itemH1 = item {checkItemPtr=checkPtr}
					in  if   hasTip
					    then osAddControlToolTip wPtr checkPtr tip >> return (itemH1,itemNr+1)
					    else return (itemH1,itemNr+1)
		  		}
		  in
 			do {
				(items,n) <- stateMapM (createCheckItem show able (toTuple parentPos) wPtr) (checkItems checkInfo) 1;
				return itemH {wItemInfo=WCheckInfo (checkInfo{checkItems=items})}
		  	}


	| wItemKind==IsPopUpControl
		= let
			info            = getWItemPopUpInfo (wItemInfo itemH)
			items           = popUpInfoItems info
			isEditable      = any isControlKeyboard atts

			appendPopUp :: OSWindowPtr -> Index -> PopUpControlItem ps (ls,ps) -> Int -> IO Int
			appendPopUp popUpPtr index (title,_) itemNr
				= osAddPopUpControlItem popUpPtr title (index==itemNr) >> return (itemNr+1)
		  in
			do {
				(popUpPtr,editPtr) <- osCreateEmptyPopUpControl wPtr (toTuple parentPos) show able pos' size' (length items) isEditable;
				foldrM (appendPopUp popUpPtr (popUpInfoIndex info)) 1 items;
				let info1  = if isEditable then info {popUpInfoEdit=Just (PopUpEditInfo{popUpEditText="",popUpEditPtr=editPtr})} else info
			    	    itemH1 = itemH {wItemPtr=popUpPtr, wItemInfo=WPopUpInfo info1}
				in  if   hasTip
				then osAddControlToolTip wPtr popUpPtr tip >> return itemH1
			    	else return itemH1
			}
	| wItemKind==IsListBoxControl
		= let
			info            = getWItemListBoxInfo (wItemInfo itemH)
			items           = listBoxInfoItems info

			appendListBox :: OSWindowPtr -> ListBoxControlItem ps (ls,ps) -> IO ()
			appendListBox listBoxPtr (title,isMarked,_)
				= osAddListBoxControlItem listBoxPtr title (isMarked==Mark) >> return ()
		  in
			do {
				listBoxPtr <- osCreateEmptyListBoxControl wPtr (toTuple parentPos) show able pos' size' (listBoxInfoMultiSel info);
				mapM_ (appendListBox listBoxPtr) items;
				let itemH1 = itemH {wItemPtr=listBoxPtr, wItemInfo=WListBoxInfo info}
				in  if   hasTip
				then osAddControlToolTip wPtr listBoxPtr tip >> return itemH1
				else return itemH1
		}

	| wItemKind==IsSliderControl
		= let
			info                              = getWItemSliderInfo (wItemInfo itemH)
			direction                         = sliderInfoDir info
			sliderState                       = sliderInfoState info
			min                               = sliderMin sliderState
			max                               = sliderMax sliderState
			(osMin,osThumb,osMax,osThumbSize) = toOSscrollbarRange (min,sliderThumb sliderState,max) 0
		  in
		  	do {
				sliderPtr <- osCreateSliderControl wPtr (toTuple parentPos) show able (direction==Horizontal) pos' size'
								   (osMin,osThumb,osMax,osThumbSize);
				let itemH1 = itemH {wItemPtr=sliderPtr}
				in  if   hasTip
				    then osAddControlToolTip wPtr sliderPtr tip >> return itemH1
				    else return itemH1
		  	}

	| wItemKind==IsTextControl
		= let	title = textInfoText $ getWItemTextInfo $ wItemInfo itemH
		  in	do {
				textPtr <- osCreateTextControl wPtr (toTuple parentPos) title show pos' size';
				let itemH1 = itemH {wItemPtr=textPtr}
				in  if   hasTip
				    then osAddControlToolTip wPtr textPtr tip >> return itemH1
				    else return itemH1
			}

	| wItemKind==IsEditControl
		= let	keySensitive = any isControlKeyboard atts
			text         = editInfoText $ getWItemEditInfo $ wItemInfo itemH
		  in	do {
				editPtr <- osCreateEditControl wPtr (toTuple parentPos) text show able keySensitive pos' size';
				let itemH1 = itemH {wItemPtr=editPtr}
				in  if   hasTip
				    then osAddControlToolTip wPtr editPtr tip >> return itemH1
				    else return itemH1
			}

	| wItemKind==IsButtonControl
		= let	itemId     = wItemId   itemH
			okOrCancel = toOKorCANCEL okId cancelId itemId
			title      = buttonInfoText $ getWItemButtonInfo $ wItemInfo itemH
		  in	do {
				buttonPtr <- osCreateButtonControl wPtr (toTuple parentPos) title show able pos' size' okOrCancel;
				let itemH1 = itemH {wItemPtr=buttonPtr}
				in  if   hasTip
				    then osAddControlToolTip wPtr buttonPtr tip >> return itemH1
				    else return itemH1
			}

	| wItemKind==IsCustomButtonControl
		= let 	itemId     = wItemId itemH
		      	okOrCancel = toOKorCANCEL okId cancelId itemId
		  in	do {
				buttonPtr <- osCreateCustomButtonControl wPtr (toTuple parentPos) show able pos' size' okOrCancel;
				let itemH1 = itemH {wItemPtr=buttonPtr}
				in  if   hasTip
				    then osAddControlToolTip wPtr buttonPtr tip >> return itemH1
				    else return itemH1
		  	}

	| wItemKind==IsCustomControl
		= do {
			customPtr <- osCreateCustomControl wPtr (toTuple parentPos) show able pos' size';
			let itemH1 = itemH {wItemPtr=customPtr}
			in  if   hasTip
			    then osAddControlToolTip wPtr customPtr tip >> return itemH1
			    else return itemH1
		  }

	| wItemKind==IsCompoundControl =
		let
		      info                    = getWItemCompoundInfo (wItemInfo itemH)
		      domainRect              = compoundDomain info
		      origin                  = compoundOrigin info
		      (hasHScroll,hasVScroll) = (isJust (compoundHScroll info),isJust (compoundVScroll info))
		      visScrolls              = osScrollbarsAreVisible wMetrics domainRect size' (hasHScroll,hasVScroll)
		      (Size {w=w',h=h'})      = rectSize (getCompoundContentRect wMetrics visScrolls (sizeToRect size))

		      hScroll :: ScrollbarInfo
		      hScroll
			      | hasHScroll    = ScrollbarInfo {cbiHasScroll=True, cbiPos=toTuple (scrollItemPos hInfo),cbiSize=toTuple hSize,cbiState=hState}
			      | otherwise     = ScrollbarInfo {cbiHasScroll=False,cbiPos=undefined,cbiSize=undefined,cbiState=undefined}
			      where
				      hInfo   = fromJust (compoundHScroll info)
				      hSize   = scrollItemSize hInfo
				      hState  = toOSscrollbarRange (rleft domainRect,x origin,rright domainRect) w'

		      vScroll :: ScrollbarInfo
		      vScroll
			      | hasVScroll    = ScrollbarInfo {cbiHasScroll=True, cbiPos=toTuple (scrollItemPos vInfo),cbiSize=toTuple vSize,cbiState=vState}
			      | otherwise     = ScrollbarInfo {cbiHasScroll=False,cbiPos=undefined,cbiSize=undefined,cbiState=undefined}
			      where
				      vInfo   = fromJust (compoundVScroll info)
				      vSize   = scrollItemSize vInfo
				      vState  = toOSscrollbarRange (rtop domainRect,y origin,rbottom domainRect) h'

		      setScrollbarPtr :: OSWindowPtr -> ScrollInfo -> ScrollInfo
		      setScrollbarPtr scrollPtr info
				= info {scrollItemPtr=scrollPtr}
		in
		  do {
			(compoundPtr,hPtr,vPtr) <- osCreateCompoundControl wMetrics wPtr (toTuple parentPos) show able False pos' size' hScroll vScroll;
			itemHs                  <- sequenceMap (createWElementHandle wMetrics okId cancelId show able pos compoundPtr) (wItems itemH);
			let compoundInfo = info { compoundHScroll=fmap (setScrollbarPtr hPtr) (compoundHScroll info)
			                        , compoundVScroll=fmap (setScrollbarPtr vPtr) (compoundVScroll info)
			                        }
			    itemH1       = itemH {wItemInfo=WCompoundInfo compoundInfo,wItemPtr=compoundPtr,wItems=itemHs}
			in  if   hasTip
			    then osAddControlToolTip wPtr compoundPtr tip >> return itemH1
			    else return itemH1
		  }


	| wItemKind==IsLayoutControl
		= do {
			itemHs <- sequenceMap (createWElementHandle wMetrics okId cancelId show able parentPos wPtr) (wItems itemH);
			return itemH{wItems=itemHs}
		  }

	| otherwise
		= return itemH
	where
		show            = showContext && wItemShow   itemH
		able            = ableContext && wItemSelect itemH
		pos             = wItemPos itemH
		size            = wItemSize itemH
		pos'            = toTuple pos
		size'           = toTuple size
		atts            = wItemAtts itemH
		(hasTip,tipAtt) = cselect isControlTip undefined atts
		tip             = getControlTipAtt tipAtt
