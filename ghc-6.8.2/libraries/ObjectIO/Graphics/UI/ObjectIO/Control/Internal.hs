-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Internal
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Control.Internal contains all control update functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Control.Internal
		( enablecontrols, disablecontrols
		, setcontrolsshowstate, setcontrolsmarkstate
		, setcontroltexts, seteditcontrolcursor
		, setcontrolslook, drawincontrol
		, setsliderstates, selectcontrolitem
		, openpopupitems, closepopupitems
		, openlistboxitems, closelistboxitems 
		, movecontrolviewframe
		, setcontrolviewdomain, setcontrolscrollfun
		) where



import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.StdPicture
import Graphics.UI.ObjectIO.StdControlAttribute
import Graphics.UI.ObjectIO.StdWindowAttribute
import Graphics.UI.ObjectIO.Control.Validate
import Graphics.UI.ObjectIO.Control.Relayout(relayoutControls)
import Graphics.UI.ObjectIO.Control.Layout(layoutControls)
import Graphics.UI.ObjectIO.Control.Draw
import Graphics.UI.ObjectIO.Window.Access
import Graphics.UI.ObjectIO.Window.ClipState(validateCompoundClipState, invalidateCompoundClipState, forceValidCompoundClipState)
import Graphics.UI.ObjectIO.Window.Update(updateWindowBackgrounds)
import Graphics.UI.ObjectIO.Window.Validate(validateViewDomain)
import Graphics.UI.ObjectIO.OS.Window
import Graphics.UI.ObjectIO.OS.ToolTip
import Graphics.UI.ObjectIO.OS.Rgn
import Control.Monad(when)


--	General occurrence tests on Id.

maybeRemoveCheck :: (Eq x) => Maybe x -> [x] -> (Bool,[x])
maybeRemoveCheck (Just id) ids
	= removeCheck id ids
maybeRemoveCheck nothing ids
	= (False,ids)

removeOnIdOfPair :: Maybe Id -> [(Id,x)] -> (Bool,(Id,x),[(Id,x)])
removeOnIdOfPair (Just id) id_args
	= remove (\(id',_)->id'==id) undefined id_args
removeOnIdOfPair nothing id_args
	= (False,undefined,id_args)

removeOnIdOfTriple :: Maybe Id -> [(Id,x,y)] -> (Bool,(Id,x,y),[(Id,x,y)])
removeOnIdOfTriple (Just id) id_args
	= remove (\(id',_,_)->id'==id) undefined id_args
removeOnIdOfTriple nothing id_args
	= (False,undefined,id_args)


{-	getContentRect returns the content rect of the window. 
	Because WindowInfo is not yet incorporated it boils down to the second alternative.
-}
getContentRect :: OSWindowMetrics -> WindowInfo -> Size -> Rect
getContentRect wMetrics (WindowInfo {windowDomain=domainRect,windowHScroll=wHScroll,windowVScroll=wVScroll}) size =
	getWindowContentRect wMetrics visScrolls (sizeToRect size)
	where		
		hasScrolls = (isJust wHScroll,isJust wVScroll)
		visScrolls = osScrollbarsAreVisible wMetrics domainRect (toTuple size) hasScrolls
getContentRect _ NoWindowInfo size = sizeToRect size


{-	Calculate the intersection of the given Rect with the content of a CompoundControl. -}

intersectRectContent :: OSWindowMetrics -> Rect -> CompoundInfo -> Point2 -> Size -> Rect
intersectRectContent wMetrics clipRect info itemPos itemSize =
	intersectRects clipRect contentRect
	where
		hasScrolls  = (isJust (compoundHScroll info),isJust (compoundVScroll info))
		domainRect  = compoundDomain info
		itemRect    = posSizeToRect itemPos itemSize
		visScrolls  = osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
		contentRect = getCompoundContentRect wMetrics visScrolls itemRect


{-	Enable the controls and provide proper feedback.
	The [Id] argument contains the Ids of the controls that should be enabled.
	The Boolean argument controls the new SelectState. 
		If the Boolean argument is False, then SelectState is the new SelectState of the indicated controls.
		If the Boolean argument is True,  then SelectState is the new SelectState of the indicated controls 
										  and  all other controls. 
-}

enablecontrols :: [Id] -> Bool -> OSWindowMetrics -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
enablecontrols ids overrule wMetrics wPtr wH@(WindowHandle {whItems=whItems,whShow=whShow,whSelect=whSelect,whSize=whSize,whDefaultId=whDefaultId}) = do
	(itemHs,_) <- setAllWElements (enableWItemHandle wMetrics wPtr whDefaultId overrule whSelect whShow clipRect) whItems ids
	return (wH{whItems=itemHs})
	where
		clipRect = getContentRect wMetrics (whWindowInfo wH) whSize

		enableWItemHandle :: OSWindowMetrics -> OSWindowPtr -> Maybe Id -> Bool -> Bool -> Bool -> Rect -> WElementHandle ls ps -> [Id] -> IO (WElementHandle ls ps,[Id])
		enableWItemHandle wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH ids
			| Just systemOSAction <- mb_action =
				let (found,ids1) = maybeRemoveCheck (wItemId itemH) ids
				in if found
				   then do
				   	systemOSAction contextSelect
				   	return (itemH{wItemSelect=True},ids1)
				   else if overrule
				   	then do
				   		systemOSAction (contextSelect && itemSelect)
				   		return (itemH,ids)
					else return (itemH,ids)
			where
				itemPtr		= wItemPtr itemH
				itemSelect	= wItemSelect itemH
				mb_action	= case wItemKind itemH of
					IsRadioControl	-> Just (\able->mapM_ (\itemH->osSetRadioControlSelect wPtr (radioItemPtr itemH) clipRect able) (radioItems (getWItemRadioInfo (wItemInfo itemH))))
					IsCheckControl	-> Just (\able->mapM_ (\itemH->osSetCheckControlSelect wPtr (checkItemPtr itemH) clipRect able) (checkItems (getWItemCheckInfo (wItemInfo itemH))))
					IsPopUpControl	-> Just (osSetPopUpControlSelect   wPtr itemPtr clipRect)
					IsListBoxControl-> Just (osSetListBoxControlSelect wPtr itemPtr clipRect)
					IsSliderControl	-> Just (osSetSliderControlSelect  wPtr itemPtr clipRect)
					IsTextControl	-> Just (osSetTextControlSelect    wPtr itemPtr clipRect)
					IsEditControl	-> Just (osSetEditControlSelect    wPtr itemPtr clipRect)
					IsButtonControl	-> Just (osSetButtonControlSelect  wPtr itemPtr clipRect)
					_		-> Nothing

		enableWItemHandle wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH ids
			| customControl =
				let (found,ids1) = maybeRemoveCheck (wItemId itemH) ids
				in if found
				   then do
					customOSAction contextSelect
					itemH <- customDraw contextSelect wPtr clipRect itemH{wItemSelect=True}
					return (itemH,ids1)
				   else if overrule
				        then do
				        	let select = contextSelect && itemSelect
						itemH <- customDraw select wPtr clipRect itemH
						customOSAction select
						return (itemH,ids)
					else return (itemH,ids)
			where
				itemPtr		= wItemPtr itemH
				itemSelect	= wItemSelect itemH
				(customControl,customDraw,customOSAction) = case wItemKind itemH of
					IsCustomButtonControl	-> (True,drawCustomButtonLook,osSetCustomButtonControlSelect wPtr itemPtr clipRect)
					IsCustomControl		-> (True,drawCustomLook,      osSetCustomControlSelect       wPtr itemPtr clipRect)
					_			-> (False,undefined,undefined)

		enableWItemHandle wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH ids
			| wItemKind itemH == IsCompoundControl =
				let (found,ids1) = maybeRemoveCheck (wItemId itemH) ids
				in if found
				   then do
					itemH <- validateCompoundClipState wMetrics False wPtr defId contextShow itemH
					itemH <- drawCompoundLook wMetrics contextSelect wPtr clipRect1 itemH
					(itemHs,ids2) <- setAllWElements (enableWItemHandle wMetrics wPtr defId True contextSelect contextShow1 clipRect1) (wItems itemH) ids1
					osSetCompoundSelect wPtr itemPtr clipRect scrollInfo contextSelect
				  	return (itemH{wItemSelect=True,wItems=itemHs}, ids2)
				   else if overrule
				        then do
						itemH <- validateCompoundClipState wMetrics False wPtr defId contextShow itemH
						itemH <- drawCompoundLook wMetrics contextSelect1 wPtr clipRect1 itemH
						(itemHs,ids2) <- setAllWElements (enableWItemHandle wMetrics wPtr defId overrule contextSelect1 contextShow1 clipRect1) (wItems itemH) ids1
						osSetCompoundSelect wPtr itemPtr clipRect scrollInfo contextSelect1
				  		return (itemH{wItems=itemHs},ids2)
					else do
						(itemHs,ids) <- setAllWElements (enableWItemHandle wMetrics wPtr defId overrule contextSelect1 contextShow1 clipRect1) (wItems itemH) ids
				  		return (itemH{wItems=itemHs},ids)
			where
				itemPtr		= wItemPtr itemH
				itemSelect	= wItemSelect itemH
				info		= getWItemCompoundInfo (wItemInfo itemH)
				scrollInfo	= (isJust (compoundHScroll info),isJust (compoundVScroll info))
				itemSize	= wItemSize itemH
				contextSelect1	= contextSelect && itemSelect
				contextShow1	= contextShow && wItemShow itemH
				clipRect1	= intersectRectContent wMetrics clipRect info (wItemPos itemH) itemSize

		enableWItemHandle wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH ids
			| wItemKind itemH == IsLayoutControl =
				let 
					(found,ids1) = maybeRemoveCheck (wItemId itemH) ids
			  		contextSelect1 = if found then contextSelect else contextSelect && itemSelect
			  	in do
			  		(itemHs,ids2) <- setAllWElements (enableWItemHandle wMetrics wPtr defId (overrule || found) contextSelect1 contextShow1 clipRect1) (wItems itemH) ids1
			  		return (itemH{wItemSelect=found || itemSelect,wItems=itemHs},ids)
			where
				itemSelect	= wItemSelect itemH
				contextShow1	= contextShow && (wItemShow itemH)
				clipRect1	= intersectRects clipRect (posSizeToRect (wItemPos itemH) (wItemSize itemH))

		enableWItemHandle _ _ _ _ _ _ _ itemH ids
			| wItemKind itemH == IsReceiverControl =
				let (found,ids1) = maybeRemoveCheck (wItemId itemH) ids
				in if found then return (itemH{wItemSelect=True},ids)
				   else return (itemH,ids)

{-	Disable the controls and provide proper feedback.
	The [Id] argument contains the Ids of the controls that should be (dis/en)abled.
	The Boolean argument controls the new SelectState. 
		If the Boolean argument is False, then SelectState is the new SelectState of the indicated controls.
		If the Boolean argument is True,  then SelectState is the new SelectState of the indicated controls 
										  and  all other controls. 
-}
disablecontrols :: [Id] -> Bool -> OSWindowMetrics -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle  ls ps)
disablecontrols ids overrule wMetrics wPtr wH@(WindowHandle {whItems=itemHs,whShow=whShow,whSelect=whSelect,whSize=whSize,whDefaultId=whDefaultId}) = do
	(itemHs,_) <- setAllWElements (disableWItemHandle wMetrics wPtr whDefaultId overrule whSelect whShow clipRect) itemHs ids
	return wH{whItems=itemHs}
	where
		clipRect			= getContentRect wMetrics (whWindowInfo wH) whSize
		
		disableWItemHandle :: OSWindowMetrics -> OSWindowPtr -> Maybe Id -> Bool -> Bool -> Bool -> Rect -> WElementHandle ls ps -> [Id] -> IO (WElementHandle ls ps,[Id])
		disableWItemHandle wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH@(WItemHandle {wItemKind=wItemKind}) ids
			| Just systemOSAction <- mb_action =
				let (found,ids1) = maybeRemoveCheck (wItemId itemH) ids
				in if found then do
					systemOSAction False
					return (itemH{wItemSelect=False},ids1)
				   else if overrule
					then do
						systemOSAction (contextSelect && itemSelect)
						return (itemH,ids)
					else return (itemH,ids)
			where
				itemPtr		= wItemPtr itemH
				itemSelect	= wItemSelect itemH
				mb_action	= case wItemKind of
					IsRadioControl	-> Just (\able->mapM_ (\itemH->osSetRadioControlSelect wPtr (radioItemPtr itemH) clipRect able) (radioItems (getWItemRadioInfo (wItemInfo itemH))))
					IsCheckControl	-> Just (\able->mapM_ (\itemH->osSetCheckControlSelect wPtr (checkItemPtr itemH) clipRect able) (checkItems (getWItemCheckInfo (wItemInfo itemH))))
					IsPopUpControl	-> Just (osSetPopUpControlSelect   wPtr itemPtr clipRect)
					IsListBoxControl-> Just (osSetListBoxControlSelect wPtr itemPtr clipRect)
					IsSliderControl	-> Just (osSetSliderControlSelect  wPtr itemPtr clipRect)
					IsTextControl	-> Just (osSetTextControlSelect    wPtr itemPtr clipRect)
					IsEditControl	-> Just (osSetEditControlSelect    wPtr itemPtr clipRect)
					IsButtonControl	-> Just (osSetButtonControlSelect  wPtr itemPtr clipRect)
					_		-> Nothing
		
		disableWItemHandle wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH@(WItemHandle {wItemKind=wItemKind}) ids
			| customControl =
				let (found,ids1) = maybeRemoveCheck (wItemId itemH) ids
				in if found then do
					customOSAction False
					itemH <- customDraw False wPtr clipRect (itemH{wItemSelect=False})
					return (itemH,ids1)
				   else if overrule
					then do				
						let select = contextSelect && itemSelect
						itemH <- customDraw select wPtr clipRect itemH
						customOSAction select
						return (itemH,ids)
					else return (itemH,ids)
			where
				itemPtr				= wItemPtr itemH
				itemSelect			= wItemSelect itemH
				(customControl,customDraw,customOSAction)
									= case wItemKind of
										IsCustomButtonControl	-> (True,drawCustomButtonLook,osSetCustomButtonControlSelect wPtr itemPtr clipRect)
										IsCustomControl		-> (True,drawCustomLook,      osSetCustomControlSelect       wPtr itemPtr clipRect)
										_			-> (False,undefined,undefined)
		
		disableWItemHandle wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH@(WItemHandle {wItemKind=wItemKind}) ids
			| wItemKind == IsCompoundControl =
				let (found,ids1) = maybeRemoveCheck (wItemId itemH) ids
				in if found then do
					itemH <- validateCompoundClipState wMetrics False wPtr defId contextShow (itemH{wItemSelect=False})
					itemH <- drawCompoundLook wMetrics False wPtr clipRect1 itemH
					(itemHs,ids) <- setAllWElements (disableWItemHandle wMetrics wPtr defId True False contextShow1 clipRect1) (wItems itemH) ids
					osSetCompoundSelect wPtr itemPtr clipRect scrollInfo False
					return (itemH{wItems=itemHs}, ids1)
				   else if overrule
					then do
						itemH <- validateCompoundClipState wMetrics False wPtr defId contextShow itemH
						itemH <- drawCompoundLook wMetrics contextSelect1 wPtr clipRect1 itemH
						(itemHs,ids) <- setAllWElements (disableWItemHandle wMetrics wPtr defId overrule contextSelect1 contextShow1 clipRect1) (wItems itemH) ids
						osSetCompoundSelect wPtr itemPtr clipRect scrollInfo contextSelect1
						return (itemH{wItems=itemHs}, ids)
					else do
						(itemHs,ids) <- setAllWElements (disableWItemHandle wMetrics wPtr defId overrule contextSelect1 contextShow1 clipRect1) (wItems itemH) ids
						return (itemH{wItems=itemHs},ids)
			where
				itemPtr					= wItemPtr itemH
				itemSelect				= wItemSelect itemH
				itemSize				= wItemSize itemH
				contextSelect1			= contextSelect && itemSelect
				contextShow1			= contextShow && wItemShow itemH
				info					= getWItemCompoundInfo (wItemInfo itemH)
				scrollInfo				= (isJust (compoundHScroll info),isJust (compoundVScroll info))
				clipRect1				= intersectRectContent wMetrics clipRect info (wItemPos itemH) itemSize
		
		disableWItemHandle wMetrics wPtr defId overrule contextSelect contextShow clipRect itemH@(WItemHandle {wItemKind=wItemKind}) ids
			| wItemKind == IsLayoutControl =
				let (found,ids1) = maybeRemoveCheck (wItemId itemH) ids
				in do
					let contextSelect1 = (not found) && (contextSelect && itemSelect)
					(itemHs,ids) <- setAllWElements (disableWItemHandle wMetrics wPtr defId (found || overrule) contextSelect1 contextShow1 clipRect1) (wItems itemH) ids1
					return (itemH{wItemSelect=not found && itemSelect,wItems=itemHs}, ids)
			where
				itemSelect				= wItemSelect itemH
				contextShow1			= contextShow && (wItemShow itemH)
				clipRect1				= intersectRects clipRect (posSizeToRect (wItemPos itemH) (wItemSize itemH))
		
		disableWItemHandle _ _ _ _ _ _ _ itemH@(WItemHandle {wItemKind=wItemKind}) ids
			| wItemKind == IsReceiverControl =
				let (found,ids1) = maybeRemoveCheck (wItemId itemH) ids
				in if found	then return (itemH{wItemSelect=False},ids1)
				   else return (itemH,ids)


--	Set the show state of the controls and provide proper feedback.

setcontrolsshowstate :: [Id] -> Bool -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> IO (WindowHandle ls ps)
setcontrolsshowstate ids itemsShow wMetrics wids wH@(WindowHandle {whItems=itemHs,whSelect=whSelect,whSize=whSize,whWindowInfo=whWindowInfo}) = do
	(itemHs,_)	<- setArgWElements (setWItemShowStates wMetrics (wPtr wids) overrule itemsShow contextShow contextSelect clipRect) itemHs ids
	return (wH{whItems=itemHs})
	where
		clipRect		= getContentRect wMetrics whWindowInfo whSize
		overrule		= False
		contextShow		= True
		contextSelect		= if whSelect then Able else Unable
		
		setWItemShowStates :: OSWindowMetrics -> OSWindowPtr -> Bool -> Bool -> Bool -> SelectState -> Rect -> WElementHandle ls ps -> [Id] -> IO (WElementHandle ls ps,[Id])
		
		setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect itemH@(WItemHandle  {wItemKind=IsRadioControl}) ids =
			let (found,ids1) = maybeRemoveCheck (wItemId itemH) ids
			in if not found && not overrule
			   then return (itemH,ids)
			   else let 
						osShow	= (not overrule || contextShow) && itemsShow
						info	= getWItemRadioInfo (wItemInfo itemH)
					in do
						mapM_ (setradio osShow clipRect) (radioItems info)
						return (if found then itemH{wItemShow=itemsShow} else itemH,ids1)
			where
				setradio :: Bool -> Rect -> RadioItemInfo ls ps -> IO ()
				setradio osShow clipRect (RadioItemInfo {radioItemPtr=itemPtr}) =
					osSetRadioControlShow wPtr itemPtr clipRect osShow
		
		setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect itemH@(WItemHandle {wItemKind=IsCheckControl}) ids =
			let (found,ids1) = maybeRemoveCheck (wItemId itemH) ids
			in if not found && not overrule
			   then return (itemH,ids)
			   else let
						osShow = (not overrule || contextShow) && itemsShow			  
						info   = getWItemCheckInfo (wItemInfo itemH)
					in do
						mapM_ (setcheck osShow clipRect) (checkItems info)
						return (if found then itemH{wItemShow=itemsShow} else itemH,ids1)
			where
				setcheck :: Bool -> Rect -> CheckItemInfo ls ps -> IO ()
				setcheck osShow clipRect (CheckItemInfo {checkItemPtr=itemPtr}) =
					osSetRadioControlShow wPtr itemPtr clipRect osShow
		
		setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect itemH@(WItemHandle {wItemKind=wItemKind}) ids
			| Just osAction <- mb_action =
				let (found,ids1) = maybeRemoveCheck (wItemId itemH) ids
				in if not found && not overrule
				   then return (itemH,ids)
				   else let
							osShow = (not overrule || contextShow) && itemsShow
						in do
							osAction wPtr (wItemPtr itemH) clipRect osShow
							return (if found then itemH{wItemShow=itemsShow} else itemH,ids1)
			where
				mb_action = case wItemKind of
					IsPopUpControl		-> Just osSetPopUpControlShow
					IsListBoxControl	-> Just osSetListBoxControlShow
					IsSliderControl		-> Just osSetSliderControlShow
					IsTextControl		-> Just osSetTextControlShow
					IsEditControl		-> Just osSetEditControlShow
					IsButtonControl		-> Just osSetButtonControlShow
					_			-> Nothing
		
		setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect itemH@(WItemHandle {wItemKind=wItemKind}) ids
			| isCustom =
				let (found,ids1) = maybeRemoveCheck (wItemId itemH) ids
				in if not found && not overrule
				   then return (itemH,ids)
				   else let			
							osShow = (not overrule || contextShow) && itemsShow
						in do
							osAction wPtr (wItemPtr itemH) clipRect osShow
							itemH <- (if osShow then customDraw (wItemSelect itemH) wPtr clipRect itemH else return itemH)
							return (if found then itemH{wItemShow=itemsShow} else itemH,ids1)
			where
				(isCustom,customDraw,osAction) = case wItemKind of
					IsCustomButtonControl	-> (True,drawCustomButtonLook,osSetCustomButtonControlShow)
					IsCustomControl			-> (True,drawCustomLook,      osSetCustomControlShow)
					_						-> (False,undefined,undefined)
		
		setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect itemH@(WItemHandle {wItemKind=IsCompoundControl}) ids =
			let
				(found,ids1) = maybeRemoveCheck (wItemId itemH) ids
				contextShow1 = contextShow && (if found then itemsShow else wItemShow itemH)
				overrule1	 = overrule || found && (wItemShow itemH /= itemsShow)
			in do
				(itemHs,ids) <- setAllWElements (setWItemShowStates wMetrics wPtr overrule1 itemsShow contextShow1 contextSelect1 clipRect1) (wItems itemH) ids1
				(if not found && not overrule
				 then return (itemH{wItems=itemHs},ids)
				 else do
					osSetCompoundShow wPtr (wItemPtr itemH) clipRect itemsShow
					let itemH1 = (if found then itemH{wItems=itemHs, wItemShow=itemsShow} else itemH{wItems=itemHs})
					let itemH2 = invalidateCompoundClipState itemH1
					return (itemH2,ids))
			where
				contextSelect1	= if enabled contextSelect then (if wItemSelect itemH then Able else Unable) else contextSelect
				info			= getWItemCompoundInfo (wItemInfo itemH)
				itemSize		= wItemSize itemH
				clipRect1		= intersectRectContent wMetrics clipRect info (wItemPos itemH) itemSize
		
		setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect itemH@(WItemHandle {wItemKind=IsLayoutControl}) ids =
			let
				(found,ids1) = maybeRemoveCheck (wItemId itemH) ids
				contextShow1 = contextShow && (if found then itemsShow else itemShow)
				overrule1	 = overrule || found && (itemShow /= itemsShow)
			in do
				(itemHs,ids) <- setAllWElements (setWItemShowStates wMetrics wPtr overrule1 itemsShow contextShow1 contextSelect1 clipRect) (wItems itemH) ids1
				return (itemH{wItemShow=if found then itemsShow else itemShow,wItems=itemHs},ids)
			where
				itemShow		= wItemShow itemH
				contextSelect1	= if enabled contextSelect then (if wItemSelect itemH then Able else Unable) else contextSelect
		
		setWItemShowStates _ _ _ _ _ _ _ itemH@(WItemHandle {wItemKind=IsReceiverControl}) ids =
			let 
				(_,ids1) = maybeRemoveCheck (wItemId itemH) ids
			in
				return (itemH,ids1)


--	Set the MarkState of the controls and provide proper feedback.

setcontrolsmarkstate :: Id -> MarkState -> [Index] -> OSWindowMetrics -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
setcontrolsmarkstate id mark indices wMetrics wPtr wH@(WindowHandle {whItems=itemHs,whSize=whSize,whWindowInfo=whWindowInfo}) = do
	(_,itemHs,_) <- setWElement (setWItemMarks wMetrics wPtr mark indices) id itemHs ()
	return (wH{whItems=itemHs})
	where
		setWItemMarks :: OSWindowMetrics -> OSWindowPtr -> MarkState -> [Index] -> Id -> WElementHandle ls ps -> s -> IO (Bool,WElementHandle ls ps,s)
		setWItemMarks wMetrics wPtr mark indices id itemH@(WItemHandle {wItemKind=IsCheckControl}) s
			| identifyMaybeId id (wItemId itemH) =
				let
					info = getWItemCheckInfo (wItemInfo itemH)
					items = checkItems info
					nrCheckItems = length items
					indices1 = filter (\index->isBetween index 1 nrCheckItems) indices
				in
					if null indices1 then return (True,itemH,s)
					else do
						items <- foldrM (setCheckMark wPtr mark) items indices1
						return (True,itemH{wItemInfo=WCheckInfo info{checkItems=items}},s)
			| otherwise =
				return (False,itemH,s)
			where
				setCheckMark :: OSWindowPtr -> MarkState -> Index -> [CheckItemInfo ls ps] -> IO [CheckItemInfo ls ps]
				setCheckMark wPtr mark index checkItems =
					let
						(before,(item:after)) 	= split (index-1) checkItems
						(title,width,_,f)	= checkItem item
						checkItems1		= before++(item{checkItem=(title,width,mark,f)}:after)
					in do
						osCheckCheckControl wPtr (checkItemPtr item) (marked mark)
						return checkItems1
						
		setWItemMarks wMetrics wPtr mark indices id itemH@(WItemHandle {wItemKind=IsListBoxControl}) s
			| identifyMaybeId id (wItemId itemH) =
				let
					info    = getWItemListBoxInfo (wItemInfo itemH)
					items   = listBoxInfoItems info
					nrItems = length items
					indices1= filter (\index->isBetween index 1 nrItems) indices
				in
					if null indices1 then return (True,itemH,s)
					else do
						items <- foldrM (setItemMark wPtr mark) items indices1
						return (True,itemH{wItemInfo=WListBoxInfo info{listBoxInfoItems=items}},s)
			| otherwise =
				return (False,itemH,s)
			where
				setItemMark :: OSWindowPtr -> MarkState -> Index -> [ListBoxControlItem ls ps] -> IO [ListBoxControlItem ls ps]
				setItemMark wPtr mark index items =
					let
						(before,((title,_,f):after)) = split (index-1) items
						items1	= before++(title,mark,f):after
					in do
						osMarkListBoxControlItem wPtr (wItemPtr itemH) index (marked mark)
						return items1
		
		setWItemMarks wMetrics wPtr mark indices id itemH@(WItemHandle {wItemKind=wItemKind}) s
			| wItemKind == IsCompoundControl = do
				(found,itemHs,s) <- setWElement (setWItemMarks wMetrics wPtr mark indices) id (wItems itemH) s
				return (found,itemH{wItems=itemHs},s)
				
			| wItemKind == IsLayoutControl = do
				(found,itemHs,s) <- setWElement (setWItemMarks wMetrics wPtr mark indices) id (wItems itemH) s
				return (found,itemH{wItems=itemHs},s)

			| otherwise =
				return (False,itemH,s)

--	Set the text of the controls and provide proper feedback.

setcontroltexts :: [(Id,String)] -> OSWindowMetrics -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
setcontroltexts id_texts wMetrics wPtr wH
	= do {
		(itemHs,_) <- setArgWElements (setControlText wMetrics wPtr True clipRect) (whItems wH) id_texts;
		return wH{whItems=itemHs}
	  }
	where
		clipRect = getContentRect wMetrics (whWindowInfo wH) (whSize wH)
		
		setControlText :: OSWindowMetrics -> OSWindowPtr -> Bool -> Rect -> WElementHandle ls ps -> [(Id,String)] -> IO (WElementHandle ls ps,[(Id,String)])
		
		setControlText wMetrics wPtr shownContext clipRect itemH@(WItemHandle {wItemKind=IsEditControl}) id_texts
			| not found
				= return (itemH,id_texts1)
			| otherwise
				= osSetEditControlText wPtr (wItemPtr itemH1) clipRect itemRect shownContext1 text >> return (itemH1,id_texts1)
			where
				(found,id_text,id_texts1) = removeOnIdOfPair (wItemId itemH) id_texts
				shownContext1             = shownContext && wItemShow itemH
				(_,text)                  = id_text
				editInfo                  = getWItemEditInfo (wItemInfo itemH)
				itemH1                    = itemH {wItemInfo=WEditInfo editInfo {editInfoText=text}}
				itemRect                  = posSizeToRect (wItemPos itemH) (wItemSize itemH)
		
		setControlText wMetrics wPtr shownContext clipRect itemH@(WItemHandle {wItemKind=IsTextControl}) id_texts
			| not found
				= return (itemH,id_texts1)
			| otherwise
				= osSetTextControlText wPtr (wItemPtr itemH) clipRect itemRect shownContext1 text >> return (itemH1,id_texts1)
			where
				(found,id_text,id_texts1) = removeOnIdOfPair (wItemId itemH) id_texts
				(_,text)                  = id_text
				shownContext1             = shownContext && wItemShow itemH
				textInfo                  = getWItemTextInfo (wItemInfo itemH)
				itemH1                    = itemH {wItemInfo=WTextInfo textInfo {textInfoText=text}}
				itemRect                  = posSizeToRect (wItemPos itemH) (wItemSize itemH)
		
		setControlText wMetrics wPtr _ clipRect itemH@(WItemHandle {wItemKind=IsButtonControl}) id_texts
			| not found
				= return (itemH,id_texts1)
			| otherwise
				= osSetButtonControlText wPtr (wItemPtr itemH) clipRect (validateControlTitle text) >> return (itemH1,id_texts1)
			where
				(found,id_text,id_texts1) = removeOnIdOfPair (wItemId itemH) id_texts
				(_,text)                  = id_text
				buttonInfo                = getWItemButtonInfo (wItemInfo itemH)
				itemH1                    = itemH {wItemInfo=WButtonInfo buttonInfo {buttonInfoText=text}}
		
		setControlText wMetrics wPtr shownContext clipRect itemH@(WItemHandle {wItemKind=IsCompoundControl}) id_texts
			= do {
				(itemHs,id_texts1) <- setArgWElements (setControlText wMetrics wPtr shownContext1 clipRect1) (wItems itemH) id_texts;
				return (itemH {wItems=itemHs},id_texts1)
			  }
			where
				info                      = getWItemCompoundInfo (wItemInfo itemH)
				clipRect1                 = intersectRectContent wMetrics clipRect info (wItemPos itemH) (wItemSize itemH)
				shownContext1             = shownContext && wItemShow itemH

		setControlText wMetrics wPtr shownContext clipRect itemH@(WItemHandle {wItemKind=IsLayoutControl}) id_texts
			= do {
				(itemHs,id_texts1) <- setArgWElements (setControlText wMetrics wPtr shownContext1 clipRect1) (wItems itemH) id_texts;
				return (itemH{wItems=itemHs},id_texts1)
			  }
			where
				clipRect1                 = intersectRects clipRect (posSizeToRect (wItemPos itemH) (wItemSize itemH))
				shownContext1             = shownContext && wItemShow itemH
	
		setControlText _ _ _ _ itemH id_texts
			= return (itemH,id_texts)


--	Set the cursor position of an EditControl, and handle proper feedback.

seteditcontrolcursor :: Id -> Int -> OSWindowMetrics -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
seteditcontrolcursor id pos wMetrics wPtr wH@(WindowHandle {whItems=itemHs,whSize=whSize,whWindowInfo=whWindowInfo}) = do
	(_,itemHs,_) <- setWElement (setEditCursor wMetrics wPtr True clipRect pos) id itemHs ()
	return (wH{whItems=itemHs})
	where
		clipRect = getContentRect wMetrics whWindowInfo whSize
		
		setEditCursor :: OSWindowMetrics -> OSWindowPtr -> Bool -> Rect -> Int -> Id -> WElementHandle ls ps -> s -> IO (Bool,WElementHandle ls ps,s)
		setEditCursor wMetrics wPtr shownContext clipRect pos id itemH@(WItemHandle {wItemKind=IsEditControl}) s
			| not (identifyMaybeId id (wItemId itemH)) =
				return (False,itemH,s)
			| otherwise =
				let 
					itemRect = posSizeToRect (wItemPos itemH) (wItemSize itemH)
				in do
					osSetEditControlCursor wPtr (wItemPtr itemH) clipRect itemRect pos
					return (True,itemH,s)
		
		setEditCursor wMetrics wPtr shownContext clipRect pos id itemH@(WItemHandle {wItems=itemHs, wItemKind=IsCompoundControl}) s = do
			(found,itemHs,s) <- setWElement (setEditCursor wMetrics wPtr shownContext1 clipRect1 pos) id itemHs s
			return (found,itemH{wItems=itemHs},s)
			where
				info			= getWItemCompoundInfo (wItemInfo itemH)
				clipRect1		= intersectRectContent wMetrics clipRect info (wItemPos itemH) (wItemSize itemH)
				shownContext1	= shownContext && (wItemShow itemH)
		
		setEditCursor wMetrics wPtr shownContext clipRect pos id itemH@(WItemHandle {wItems=itemHs, wItemKind=IsLayoutControl}) s = do
			(found,itemHs,s) <- setWElement (setEditCursor wMetrics wPtr shownContext1 clipRect1 pos) id itemHs s
			return (found,itemH{wItems=itemHs},s)
			where
				clipRect1			= intersectRects clipRect (posSizeToRect (wItemPos itemH) (wItemSize itemH))
				shownContext1		= shownContext && wItemShow itemH
		
		setEditCursor _ _ _ _ _ _ itemH s = do
			return (False,itemH,s)


--	Set the look of a control, and handle proper feedback.

setcontrolslook :: [(Id,Bool,(Bool,Look))] -> OSWindowMetrics -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
setcontrolslook looks wMetrics wPtr wH@(WindowHandle {whItems=itemHs,whSelect=whSelect,whDefaultId=whDefaultId,whSize=whSize,whWindowInfo=whWindowInfo}) = do
	(itemHs, _) <- setAllWElements (setWItemLook wMetrics wPtr whSelect True resizeable whDefaultId clipRect) itemHs looks
	return (wH{whItems=itemHs})
	where
		clipRect	= getContentRect wMetrics whWindowInfo whSize
		resizeable	= True
		
		setWItemLook :: OSWindowMetrics -> OSWindowPtr -> Bool -> Bool -> Bool -> Maybe Id -> Rect -> WElementHandle ls ps -> [(Id,Bool,(Bool,Look))] -> IO (WElementHandle ls ps,[(Id,Bool,(Bool,Look))])
		setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect itemH@(WItemHandle {wItemId=wItemId,wItemKind=IsCompoundControl,wItemSize=wItemSize}) looks =
			let
				(found,look,looks1)	= removeOnIdOfTriple wItemId looks
				(_,redraw,(sysLook,cLook)) = look
			in do
				(itemHs,looks) <- setAllWElements (setWItemLook wMetrics wPtr ableContext1 shownContext1 resizeable defId clipRect1) (wItems itemH) looks1
				(if not found then return (itemH{wItems=itemHs},looks)
				 else
					let 
						info1  = info{compoundLookInfo=lookInfo{compoundLook = LookInfo
											{ lookFun		= cLook
		  									, lookPen		= pen
		  									, lookSysUpdate	= sysLook
		  									}}}
						itemH1 = itemH{wItems=itemHs, wItemInfo=WCompoundInfo info1}
					in 
						if not redraw || not shownContext1
						then return (itemH1,looks)
						else do						
							itemH <- validateCompoundClipState wMetrics False wPtr defId shownContext itemH1
							itemH <- drawCompoundLook wMetrics ableContext1 wPtr clipRect1 itemH
							return (itemH,looks))
			where
				info				= getWItemCompoundInfo (wItemInfo itemH)
				hasScrolls			= (isJust (compoundHScroll info),isJust (compoundVScroll info))
				lookInfo			= compoundLookInfo info
				pen					= lookPen (compoundLook lookInfo)
				visScrolls			= osScrollbarsAreVisible wMetrics (compoundDomain info) (toTuple wItemSize) hasScrolls
				itemRect			= posSizeToRect (wItemPos itemH) wItemSize
				contentRect			= getCompoundContentRect wMetrics visScrolls itemRect
				clipRect1			= intersectRects clipRect contentRect
				ableContext1		= ableContext  && wItemSelect itemH
				shownContext1		= shownContext && wItemShow itemH

		setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect itemH@(WItemHandle {wItemId=wItemId,wItemKind=IsCustomButtonControl}) looks =
			let
				(found,look,looks1)			= removeOnIdOfTriple wItemId looks
				(_,redraw,(sysLook,cLook))	= look
			in 
				if not found
				then return (itemH,looks1)
				else 
					let
						info1  = info{cButtonInfoLook=itemLook{lookFun=cLook,lookSysUpdate=sysLook}}
						itemH1 = itemH{wItemInfo=WCustomButtonInfo info1}
					in
						if not redraw || not shownContext1 then return (itemH1,looks1)
						else do
							itemH2 <- drawCustomButtonLook ableContext1 wPtr clipRect itemH1
							return (itemH2,looks1)
			where
				info			= getWItemCustomButtonInfo (wItemInfo itemH)
				itemLook		= cButtonInfoLook info
				ableContext1	= ableContext  && wItemSelect itemH
				shownContext1	= shownContext && wItemShow   itemH
		
		setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect itemH@(WItemHandle {wItemId=wItemId,wItemKind=IsCustomControl}) looks =
			let
				(found,look,looks1)	= removeOnIdOfTriple wItemId looks
				(_,redraw,(sysLook,cLook)) = look
			in
				if not found
				then return (itemH,looks1)
				else
					let
						info1  = info{customInfoLook=itemLook{lookFun=cLook,lookSysUpdate=sysLook}}
						itemH1 = itemH{wItemInfo=WCustomInfo info1}
					in
						if not redraw || not shownContext1 then return (itemH1,looks1)
						else do
							itemH2 <- drawCustomLook ableContext1 wPtr clipRect itemH1
							return (itemH2,looks1)
			where
				info			= getWItemCustomInfo (wItemInfo itemH)
				itemLook		= customInfoLook info
				ableContext1	= ableContext  && wItemSelect itemH
				shownContext1	= shownContext && wItemShow   itemH
		
		setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect itemH@(WItemHandle {wItemId=wItemId,wItems=itemHs,wItemKind=IsLayoutControl}) looks =
			let
				(_,_,looks1) = removeOnIdOfTriple wItemId looks
			in do
				(itemHs,looks) <- setAllWElements (setWItemLook wMetrics wPtr ableContext1 shownContext1 resizeable defId clipRect1) itemHs looks1
				return (itemH{wItems=itemHs},looks1)
			where
				clipRect1		= intersectRects clipRect (posSizeToRect (wItemPos itemH) (wItemSize itemH))
				ableContext1	= ableContext  && wItemSelect itemH
				shownContext1	= shownContext && wItemShow   itemH
		
		setWItemLook _ _ _ _ _ _ _ itemH@(WItemHandle {wItemId=wItemId}) looks =
			let
				(_,_,looks1)	= removeOnIdOfTriple wItemId looks
			in
				return (itemH,looks)


--	Draw in a customised control.

drawincontrol :: Id -> Draw x -> OSWindowMetrics -> OSWindowPtr -> WindowHandle ls ps -> IO (Maybe x,WindowHandle ls ps)
drawincontrol controlId drawfun wMetrics wPtr wH@(WindowHandle {whItems=itemHs,whDefaultId=whDefaultId,whShow=whShow,whSize=whSize,whWindowInfo=whWindowInfo}) = do
	(_,itemHs,mb_x) <- setWElement (drawInWItem wMetrics wPtr resizeable whDefaultId whShow clipRect drawfun) controlId itemHs Nothing
	return (mb_x,wH{whItems=itemHs})
	where
		clipRect	= getContentRect wMetrics whWindowInfo whSize
		resizeable	= True

		drawInWItem :: OSWindowMetrics -> OSWindowPtr -> Bool -> Maybe Id -> Bool -> Rect -> Draw x -> Id -> WElementHandle ls ps -> Maybe x -> IO (Bool,WElementHandle ls ps,Maybe x)
		drawInWItem wMetrics wPtr resizeable defId contextShow clipRect drawfun id itemH@(WItemHandle {wItemId=itemId,wItemPtr=itemPtr,wItems=itemHs, wItemKind=IsCompoundControl}) mb_x
			| not (identifyMaybeId id itemId) = do
				(found,itemHs,mb_x) <- setWElement (drawInWItem wMetrics wPtr resizeable defId itemShow clipRect1 drawfun) id itemHs mb_x
				return (found,itemH{wItems=itemHs},mb_x)
			| otherwise = do				
				itemH <- validateCompoundClipState wMetrics False wPtr defId contextShow itemH
				(x,itemH) <- drawInCompound wPtr drawfun clipRect1 itemH
				return (True,itemH,Just x)
			where
				itemShow	= contextShow && wItemShow itemH
				info		= getWItemCompoundInfo (wItemInfo itemH)
				domainRect	= compoundDomain info
				hasScrolls	= (isJust (compoundHScroll info),isJust (compoundVScroll info))
				itemPos		= wItemPos itemH
				itemSize	= wItemSize itemH
				itemRect	= posSizeToRect itemPos itemSize
				visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
				contentRect	= getCompoundContentRect wMetrics visScrolls itemRect
				clipRect1	= intersectRects clipRect contentRect

		drawInWItem _ wPtr _ _ _ clipRect drawfun id itemH@(WItemHandle {wItemId=itemId,wItemKind=IsCustomButtonControl}) mb_x
			| not (identifyMaybeId id itemId) =
				return (False,itemH,mb_x)
			| otherwise = do
				(x,itemH) <- drawInCustomButton wPtr drawfun clipRect itemH
				return (True,itemH,Just x)

		drawInWItem _ wPtr _ _ _ clipRect drawfun id itemH@(WItemHandle {wItemId=itemId,wItemKind=IsCustomControl}) mb_x
			| not (identifyMaybeId id itemId) =
				return (False,itemH,mb_x)
			| otherwise = do
				(x,itemH) <- drawInCustom wPtr drawfun clipRect itemH
				return (True,itemH,Just x)

		drawInWItem wMetrics wPtr resizeable defId contextShow clipRect drawfun id itemH@(WItemHandle {wItemId=itemId,wItems=itemHs,wItemKind=IsLayoutControl}) mb_x
			| identifyMaybeId id itemId =
				return (True,itemH,mb_x)
			| otherwise = do
				(found,itemHs,mb_x) <- setWElement (drawInWItem wMetrics wPtr resizeable defId itemShow clipRect1 drawfun) id itemHs mb_x
				return (found,itemH{wItems=itemHs},mb_x)
			where
				itemShow	= contextShow && wItemShow itemH
				clipRect1	= intersectRects clipRect (posSizeToRect (wItemPos itemH) (wItemSize itemH))

		drawInWItem _ _ _ _ _ _ _ id itemH@(WItemHandle {wItemId=itemId}) mb_x =
			return (identifyMaybeId id itemId,itemH,mb_x)

--	Change the state of the slider and handle proper feedback.

setsliderstates :: [(Id,IdFun SliderState)] -> OSWindowMetrics -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
setsliderstates id_fs wMetrics wPtr wH@(WindowHandle {whItems=itemHs,whSize=whSize,whWindowInfo=whWindowInfo}) = do
	(itemHs,_) <- setAllWElements (setSliderState wMetrics wPtr clipRect) itemHs id_fs
	return (wH{whItems=itemHs})
	where
		clipRect	= getContentRect wMetrics whWindowInfo whSize
		
		setSliderState :: OSWindowMetrics -> OSWindowPtr -> Rect -> WElementHandle ls ps -> [(Id,IdFun SliderState)] -> IO (WElementHandle ls ps, [(Id,IdFun SliderState)])
		setSliderState wMetrics wPtr clipRect itemH@(WItemHandle {wItemId=itemId,wItemKind=IsSliderControl,wItemPtr=itemPtr}) id_fs =
			let
				(found,id_f,id_fs1) = removeOnIdOfPair itemId id_fs
				(_,f) = id_f
			in
				if not found then return (itemH,id_fs1)
				else
					let
						info = getWItemSliderInfo (wItemInfo itemH)
						oldState	= sliderInfoState info
						newState	= validateSliderState (f oldState)
						itemH1		= itemH{wItemInfo=WSliderInfo info{sliderInfoState=newState}}
						(tbMin,tbThumb,tbMax,_) = toOSscrollbarRange (sliderMin newState,sliderThumb newState,sliderMax newState) 0
					in do
						osSetSliderThumb wPtr itemPtr clipRect (not (isEmptyRect clipRect)) (tbMin,tbThumb,tbMax)
						return (itemH1,id_fs1)
		
		setSliderState wMetrics wPtr clipRect itemH@(WItemHandle {wItemKind=IsCompoundControl,wItems=itemHs}) id_fs =
			let
				(_,_,id_fs1) = removeOnIdOfPair (wItemId itemH) id_fs
			in do
				(itemHs,id_fs2) <- setAllWElements (setSliderState wMetrics wPtr clipRect1) itemHs id_fs1
				return (itemH{wItems=itemHs},id_fs2)
			where
				info		= getWItemCompoundInfo (wItemInfo itemH)
				clipRect1	= intersectRectContent wMetrics clipRect info (wItemPos itemH) (wItemSize itemH)
		
		setSliderState wMetrics wPtr clipRect itemH@(WItemHandle {wItemKind=IsLayoutControl,wItems=itemHs}) id_fs =
			let
				(_,_,id_fs1) = removeOnIdOfPair (wItemId itemH) id_fs
			in do
				(itemHs,id_fs2)	<- setAllWElements (setSliderState wMetrics wPtr clipRect1) itemHs id_fs1
				return (itemH{wItems=itemHs},id_fs2)
			where
				clipRect1 = intersectRects clipRect (posSizeToRect (wItemPos itemH) (wItemSize itemH))
		
		setSliderState _ _ _ itemH id_fs =
			let
				(_,_,id_fs1) = removeOnIdOfPair (wItemId itemH) id_fs
			in
				return (itemH,id_fs1)


--	Select a PopUpControl item.
	
selectcontrolitem :: Id -> Index -> OSWindowMetrics -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
selectcontrolitem id index wMetrics wPtr wH@(WindowHandle {whItems=itemHs,whSize=whSize,whWindowInfo=whWindowInfo}) = do
	(_,itemHs,_) <- setWElement (selectWItem wMetrics wPtr index) id itemHs ()
	return (wH{whItems=itemHs})
	where
		selectWItem :: OSWindowMetrics -> OSWindowPtr -> Index -> Id -> WElementHandle ls ps -> s -> IO (Bool,WElementHandle ls ps,s)
		selectWItem wMetrics wPtr index id itemH@(WItemHandle {wItemKind=IsPopUpControl}) s
			| not (identifyMaybeId id (wItemId itemH)) =
				return (False,itemH,s)
			| otherwise =
				let
					info	 	= getWItemPopUpInfo (wItemInfo itemH)
			  		curindex 	= popUpInfoIndex info
			  		nrPopUps 	= length (popUpInfoItems info)
			  		newindex	= setBetween index 1 nrPopUps
					itemH1	 	= itemH{wItemInfo=WPopUpInfo info{popUpInfoIndex=index}}
			  	in
			  		if curindex==newindex then return (True,itemH1,s)
					else do
						osSelectPopUpControlItem wPtr (wItemPtr itemH) newindex
						return (True,itemH1,s)
						
		selectWItem wMetrics wPtr index id itemH@(WItemHandle {wItemKind=IsListBoxControl}) s
			| not (identifyMaybeId id (wItemId itemH)) =
				return (False,itemH,s)
			| otherwise =
				let
					info	 	= getWItemListBoxInfo (wItemInfo itemH)
					items		= listBoxInfoItems info
					nrItems 	= length items
					newindex	= setBetween index 1 nrItems
					items' 		= updateItems items newindex
					itemH1	 	= itemH{wItemInfo=WListBoxInfo info{listBoxInfoItems=items'}}
					
					updateItems [] n = []
					updateItems ((title,mark,f):items) n
						| n==1      = (title,Mark  ,f):items'
						| otherwise = (title,NoMark,f):items'
						where
							items' = updateItems items (n-1)
				in do
					osSelectListBoxControlItem wPtr (wItemPtr itemH) newindex
					return (True,itemH1,s)

		selectWItem wMetrics wPtr index id itemH@(WItemHandle {wItemId=itemId,wItemKind=IsRadioControl}) s
			| not (identifyMaybeId id itemId) =
				return (False,itemH,s)
			| otherwise =
				let
					info		= getWItemRadioInfo (wItemInfo itemH)
					curindex	= radioIndex info
					items		= radioItems info
					nrItems		= length items
					newindex	= setBetween index 1 nrItems					
					itemH1		= itemH{wItemInfo=WRadioInfo info{radioIndex=index}}
				in
					if newindex==curindex then return (True,itemH1,s)
					else do
						osCheckRadioControl wPtr (radioItemPtr (items!!(curindex-1))) False
						osCheckRadioControl wPtr (radioItemPtr (items!!(newindex-1))) True
						return (True,itemH1,s)

		selectWItem wMetrics wPtr index id itemH@(WItemHandle {wItemKind=IsCompoundControl, wItems=itemHs}) s
			| identifyMaybeId id (wItemId itemH) =
				return (True,itemH,s)
			| otherwise = do
				(found,itemHs,s) <- setWElement (selectWItem wMetrics wPtr index) id itemHs s
				return (found,itemH{wItems=itemHs},s)

		selectWItem wMetrics wPtr index id itemH@(WItemHandle {wItemKind=IsLayoutControl, wItems=itemHs}) s
			| identifyMaybeId id (wItemId itemH) =
				return (True,itemH,s)
			| otherwise = do
				(found,itemHs,s) <- setWElement (selectWItem wMetrics wPtr index) id itemHs s
				return (found,itemH{wItems=itemHs},s)

		selectWItem _ _ _ id itemH s =
			return (identifyMaybeId id (wItemId itemH),itemH,s)

--	Add new items to a PopUpControl. 

openpopupitems :: Id -> Index -> [PopUpControlItem ps ps] -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
openpopupitems id index newItems wPtr wH@(WindowHandle {whItems=itemHs}) = do
	(_,itemHs) <- openWElementsPopUpItems wPtr index newItems id itemHs
	return (wH{whItems=itemHs})
	where
		openWElementsPopUpItems :: OSWindowPtr -> Index -> [PopUpControlItem ps ps] -> Id -> [WElementHandle ls ps] -> IO (Bool,[WElementHandle ls ps])
		openWElementsPopUpItems wPtr index newItems id [] = return (False,[])
		openWElementsPopUpItems wPtr index newItems id (itemH:itemHs) = do
			(done,itemH) <- openWElementPopUpItems wPtr index newItems id itemH
			(if done then return (done,itemH:itemHs)
			 else do
				(done,itemHs) <- openWElementsPopUpItems wPtr index newItems id itemHs
				return (done,itemH:itemHs))
			where
				openWElementPopUpItems :: OSWindowPtr -> Index -> [PopUpControlItem ps ps] -> Id -> WElementHandle ls ps -> IO (Bool,WElementHandle ls ps)
				openWElementPopUpItems wPtr id index newItems itemH@(WItemHandle {}) = do
					openWItemPopUpItems wPtr id index newItems itemH
					where
						openWItemPopUpItems :: OSWindowPtr -> Index -> [PopUpControlItem ps ps] -> Id -> WElementHandle ls ps -> IO (Bool,WElementHandle ls ps)
						openWItemPopUpItems wPtr index items id itemH@(WItemHandle {wItemKind=IsPopUpControl})
							| not (identifyMaybeId id (wItemId itemH)) =
								return (False,itemH)
							| otherwise = do
								(newPopUpPtr,editPtr) <- osCreateEmptyPopUpControl wPtr (0,0) (wItemShow itemH) ableContext (toTuple popUpPos) (toTuple popUpSize) (length newItems) isEditable
								foldrM (appendPopUp newPopUpPtr newIndex) 1 newItems
								osStackWindow newPopUpPtr popUpPtr (\x -> return ())
								osDestroyPopUpControl popUpPtr
							  	let newPopUpInfo = PopUpInfo
							  		{ popUpInfoItems = newItems
									, popUpInfoIndex = newIndex
									, popUpInfoEdit  = if isEditable then Just curEditInfo{popUpEditPtr=editPtr} else Nothing
									}
							  	let itemH1 = itemH{wItemInfo=WPopUpInfo newPopUpInfo,wItemPtr=newPopUpPtr}
								(if not hasTip then return (True,itemH1)
								 else do							
									osAddControlToolTip wPtr newPopUpPtr (getControlTipAtt tipAtt)
									return (True,itemH1))
							where
								(hasTip,tipAtt)	= cselect isControlTip undefined (wItemAtts itemH)
								isEditable	= any isControlKeyboard (wItemAtts itemH)
								ableContext	= wItemSelect itemH
								popUpPtr	= wItemPtr itemH
								popUpSize	= wItemSize itemH
								popUpPos	= wItemPos itemH
								popUpInfo	= getWItemPopUpInfo (wItemInfo itemH)
								curEditInfo	= fromJust (popUpInfoEdit popUpInfo)
								curIndex	= popUpInfoIndex popUpInfo
								curItems	= popUpInfoItems popUpInfo
								newItems	= before++[(title,noLS f) | (title,f)<-items]++after
								newIndex	= if curIndex<=index then curIndex else curIndex+index
								(before,after)	= split index curItems

								appendPopUp :: OSWindowPtr -> Index -> PopUpControlItem ps st -> Int -> IO Int
								appendPopUp popUpPtr index (title,_) itemNr = do
									osAddPopUpControlItem popUpPtr title (index==itemNr)
									return (itemNr+1)

						openWItemPopUpItems wPtr index newItems id itemH@(WItemHandle {wItemId=itemId,wItems=itemHs})
							| identifyMaybeId id itemId =
								return (True,itemH)
							| otherwise = do
								(done,itemHs) <- openWElementsPopUpItems wPtr index newItems id itemHs
								return (done,itemH{wItems=itemHs})

				openWElementPopUpItems wPtr index newItems id (WListLSHandle itemHs) = do
					(done,itemHs) <- openWElementsPopUpItems wPtr index newItems id itemHs
					return (done,WListLSHandle itemHs)

				openWElementPopUpItems wPtr index newItems id (WExtendLSHandle exLS itemHs) = do
					(done,itemHs) <- openWElementsPopUpItems wPtr index newItems id itemHs
					return (done,WExtendLSHandle exLS itemHs)

				openWElementPopUpItems wPtr index newItems id (WChangeLSHandle chLS itemHs) = do
					(done,itemHs) <- openWElementsPopUpItems wPtr index newItems id itemHs
					return (done,WChangeLSHandle chLS itemHs)


--	Remove items from a PopUpControl. 

closepopupitems :: Id -> [Index] -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
closepopupitems id indices wPtr wH@(WindowHandle {whItems=itemHs}) = do
	(_,itemHs) <- closeWElementsPopUpItems wPtr indices id itemHs
	return (wH{whItems=itemHs})
	where
		closeWElementsPopUpItems :: OSWindowPtr -> [Index] -> Id -> [WElementHandle ls ps] -> IO (Bool,[WElementHandle ls ps])
		closeWElementsPopUpItems wPtr indices id [] 		= return (False,[])
		closeWElementsPopUpItems wPtr indices id (itemH:itemHs)  = do
			(done,itemH) <- closeWElementPopUpItems wPtr indices id itemH
			(if done then return (done,itemH:itemHs)
			 else do
				(done,itemHs) <- closeWElementsPopUpItems wPtr indices id itemHs
				return (done,itemH:itemHs))
			where
				closeWElementPopUpItems :: OSWindowPtr -> [Index] -> Id -> WElementHandle ls ps -> IO (Bool,WElementHandle ls ps)
				closeWElementPopUpItems wPtr indices id itemH@(WItemHandle {wItemKind=itemKind,wItems=itemHs})
					| itemKind == IsPopUpControl =
						if not (identifyMaybeId id (wItemId itemH)) then return (False,itemH)
						else do
							(newPopUpPtr,editPtr) <- osCreateEmptyPopUpControl wPtr (0,0) (wItemShow itemH) ableContext (toTuple popUpPos) (toTuple popUpSize) (length newItems) isEditable
							foldrM (appendPopUp newPopUpPtr newIndex) 1 newItems
							osStackWindow newPopUpPtr popUpPtr (\x -> return ())
							osDestroyPopUpControl popUpPtr
							let newPopUpInfo = PopUpInfo
								{ popUpInfoItems = newItems
								, popUpInfoIndex = newIndex
								, popUpInfoEdit  = if isEditable then Just curEditInfo{popUpEditPtr=editPtr} else Nothing
								}
							let itemH1 = itemH{wItemInfo=WPopUpInfo newPopUpInfo,wItemPtr=newPopUpPtr}
							(if not hasTip then return (True,itemH1)
							 else do
								osAddControlToolTip wPtr newPopUpPtr (getControlTipAtt tipAtt)
								return (True,itemH1))
					| otherwise = 
						if identifyMaybeId id (wItemId itemH) then return (True,itemH)
						else do
						  	(done,itemHs) <- closeWElementsPopUpItems wPtr indices id itemHs
							return (done,itemH{wItems=itemHs})
					where
						(hasTip,tipAtt)	= cselect isControlTip undefined (wItemAtts itemH)
						isEditable	= any isControlKeyboard (wItemAtts itemH)
						ableContext	= wItemSelect itemH
						popUpPtr	= wItemPtr itemH
						popUpSize	= wItemSize itemH
						popUpPos	= wItemPos itemH
						popUpInfo	= getWItemPopUpInfo (wItemInfo itemH)
						curEditInfo	= fromJust (popUpInfoEdit popUpInfo)
						curIndex	= popUpInfoIndex popUpInfo
						curItems	= popUpInfoItems popUpInfo
						newItems	= map snd (filter (\(i,_)->not (i `elem` indices)) (zip [1..] curItems))
						nrNewItems	= length newItems
						newIndex	= if curIndex `elem` indices then 1 else min nrNewItems curIndex

						appendPopUp :: OSWindowPtr -> Index -> PopUpControlItem ps st -> Int -> IO Int
						appendPopUp popUpPtr index (title,_) itemNr = do
							osAddPopUpControlItem popUpPtr title (index==itemNr)
							return (itemNr+1)					

				closeWElementPopUpItems wPtr indices id (WListLSHandle itemHs) = do
					(done,itemHs) <- closeWElementsPopUpItems wPtr indices id itemHs
					return (done,WListLSHandle itemHs)

				closeWElementPopUpItems wPtr indices id (WExtendLSHandle exLS itemHs) = do
					(done,itemHs) <- closeWElementsPopUpItems wPtr indices id itemHs
					return (done,WExtendLSHandle exLS itemHs)

				closeWElementPopUpItems wPtr indices id (WChangeLSHandle chLS itemHs) = do
					(done,itemHs) <- closeWElementsPopUpItems wPtr indices id itemHs
					return (done,WChangeLSHandle chLS itemHs)


--	Add new items to a ListBoxControl. 

openlistboxitems :: Id -> Index -> [ListBoxControlItem ps ps] -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
openlistboxitems id index newItems wPtr wH@(WindowHandle {whItems=itemHs}) = do
	(_,itemHs) <- openWElementsListBoxItems wPtr index newItems id itemHs
	return (wH{whItems=itemHs})
	where
		openWElementsListBoxItems :: OSWindowPtr -> Index -> [ListBoxControlItem ps ps] -> Id -> [WElementHandle ls ps] -> IO (Bool,[WElementHandle ls ps])
		openWElementsListBoxItems wPtr index newItems id [] = return (False,[])
		openWElementsListBoxItems wPtr index newItems id (itemH:itemHs) = do
			(done,itemH) <- openWElementListBoxItems wPtr index newItems id itemH
			(if done then return (done,itemH:itemHs)
			 else do
				(done,itemHs) <- openWElementsListBoxItems wPtr index newItems id itemHs
				return (done,itemH:itemHs))
			where
				openWElementListBoxItems :: OSWindowPtr -> Index -> [ListBoxControlItem ps ps] -> Id -> WElementHandle ls ps -> IO (Bool,WElementHandle ls ps)
				openWElementListBoxItems wPtr id index newItems itemH@(WItemHandle {}) = do
					openWItemListBoxItems wPtr id index newItems itemH
					where
						openWItemListBoxItems :: OSWindowPtr -> Index -> [ListBoxControlItem ps ps] -> Id -> WElementHandle ls ps -> IO (Bool,WElementHandle ls ps)
						openWItemListBoxItems wPtr index items id itemH@(WItemHandle {wItemKind=IsListBoxControl})
							| not (identifyMaybeId id (wItemId itemH)) =
								return (False,itemH)
							| otherwise = do
								newLBoxPtr <- osCreateEmptyListBoxControl wPtr (0,0) (wItemShow itemH) ableContext (toTuple lboxPos) (toTuple lboxSize) (listBoxInfoMultiSel info)
								mapM_ (appendListBox newLBoxPtr) newItems
								osStackWindow newLBoxPtr lboxPtr (\x -> return ())
								osDestroyListBoxControl lboxPtr
							  	let newListBoxInfo = info{listBoxInfoItems = newItems}
							  	let itemH1 = itemH{wItemInfo=WListBoxInfo newListBoxInfo,wItemPtr=newLBoxPtr}
								(if not hasTip then return (True,itemH1)
								 else do							
									osAddControlToolTip wPtr newLBoxPtr (getControlTipAtt tipAtt)
									return (True,itemH1))
							where
								(hasTip,tipAtt)	= cselect isControlTip undefined (wItemAtts itemH)
								ableContext	= wItemSelect itemH
								lboxPtr		= wItemPtr itemH
								lboxSize	= wItemSize itemH
								lboxPos		= wItemPos itemH
								info		= getWItemListBoxInfo (wItemInfo itemH)
								curItems	= listBoxInfoItems info
								newItems	= before++[(title,mark,noLS f) | (title,mark,f)<-items]++after
								(before,after)	= split index curItems

								appendListBox :: OSWindowPtr -> ListBoxControlItem ps st -> IO ()
								appendListBox lboxPtr (title,mark,_) = do
									osAddListBoxControlItem lboxPtr title (marked mark)
									return ()

						openWItemListBoxItems wPtr index newItems id itemH@(WItemHandle {wItemId=itemId,wItems=itemHs})
							| identifyMaybeId id itemId =
								return (True,itemH)
							| otherwise = do
								(done,itemHs) <- openWElementsListBoxItems wPtr index newItems id itemHs
								return (done,itemH{wItems=itemHs})

				openWElementListBoxItems wPtr index newItems id (WListLSHandle itemHs) = do
					(done,itemHs) <- openWElementsListBoxItems wPtr index newItems id itemHs
					return (done,WListLSHandle itemHs)

				openWElementListBoxItems wPtr index newItems id (WExtendLSHandle exLS itemHs) = do
					(done,itemHs) <- openWElementsListBoxItems wPtr index newItems id itemHs
					return (done,WExtendLSHandle exLS itemHs)

				openWElementListBoxItems wPtr index newItems id (WChangeLSHandle chLS itemHs) = do
					(done,itemHs) <- openWElementsListBoxItems wPtr index newItems id itemHs
					return (done,WChangeLSHandle chLS itemHs)


--	Remove items from a PopUpControl. 

closelistboxitems :: Id -> [Index] -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
closelistboxitems id indices wPtr wH@(WindowHandle {whItems=itemHs}) = do
	(_,itemHs) <- closeWElementsListBoxItems wPtr indices id itemHs
	return (wH{whItems=itemHs})
	where
		closeWElementsListBoxItems :: OSWindowPtr -> [Index] -> Id -> [WElementHandle ls ps] -> IO (Bool,[WElementHandle ls ps])
		closeWElementsListBoxItems wPtr indices id [] 		= return (False,[])
		closeWElementsListBoxItems wPtr indices id (itemH:itemHs)  = do
			(done,itemH) <- closeWElementListBoxItems wPtr indices id itemH
			(if done then return (done,itemH:itemHs)
			 else do
				(done,itemHs) <- closeWElementsListBoxItems wPtr indices id itemHs
				return (done,itemH:itemHs))
			where
				closeWElementListBoxItems :: OSWindowPtr -> [Index] -> Id -> WElementHandle ls ps -> IO (Bool,WElementHandle ls ps)
				closeWElementListBoxItems wPtr indices id itemH@(WItemHandle {wItemKind=itemKind,wItems=itemHs})
					| itemKind == IsListBoxControl =
						if not (identifyMaybeId id (wItemId itemH)) then return (False,itemH)
						else do
							newLBoxPtr <- osCreateEmptyListBoxControl wPtr (0,0) (wItemShow itemH) ableContext (toTuple lboxPos) (toTuple lboxSize) (listBoxInfoMultiSel info)
							mapM_ (appendPopUp newLBoxPtr) newItems
							osStackWindow newLBoxPtr lboxPtr (\x -> return ())
							osDestroyListBoxControl lboxPtr
							let newListBoxInfo = info{listBoxInfoItems = newItems}
							let itemH1 = itemH{wItemInfo=WListBoxInfo newListBoxInfo,wItemPtr=newLBoxPtr}
							(if not hasTip then return (True,itemH1)
							 else do
								osAddControlToolTip wPtr newLBoxPtr (getControlTipAtt tipAtt)
								return (True,itemH1))
					| otherwise = 
						if identifyMaybeId id (wItemId itemH) then return (True,itemH)
						else do
						  	(done,itemHs) <- closeWElementsListBoxItems wPtr indices id itemHs
							return (done,itemH{wItems=itemHs})
					where
						(hasTip,tipAtt)	= cselect isControlTip undefined (wItemAtts itemH)						
						ableContext	= wItemSelect itemH
						lboxPtr		= wItemPtr itemH
						lboxSize	= wItemSize itemH
						lboxPos		= wItemPos itemH
						info		= getWItemListBoxInfo (wItemInfo itemH)
						curItems	= listBoxInfoItems info
						newItems	= map snd (filter (\(i,_)->not (i `elem` indices)) (zip [1..] curItems))
						nrNewItems	= length newItems

						appendPopUp :: OSWindowPtr -> ListBoxControlItem ps st -> IO ()
						appendPopUp lboxPtr (title,mark,_) = do
							osAddListBoxControlItem lboxPtr title (marked mark)
							return ()

				closeWElementListBoxItems wPtr indices id (WListLSHandle itemHs) = do
					(done,itemHs) <- closeWElementsListBoxItems wPtr indices id itemHs
					return (done,WListLSHandle itemHs)

				closeWElementListBoxItems wPtr indices id (WExtendLSHandle exLS itemHs) = do
					(done,itemHs) <- closeWElementsListBoxItems wPtr indices id itemHs
					return (done,WExtendLSHandle exLS itemHs)

				closeWElementListBoxItems wPtr indices id (WChangeLSHandle chLS itemHs) = do
					(done,itemHs) <- closeWElementsListBoxItems wPtr indices id itemHs
					return (done,WChangeLSHandle chLS itemHs)


{-	The record MetricsInfo and the functions shiftControls` and setsliderthumb are used by
	movecontrolviewframe and setcontrolviewdomain.
-}

data MetricsInfo
   = MetricsInfo
	{ miOSMetrics	:: !OSWindowMetrics
	, miHMargins	:: !(Int,Int)
	, miVMargins	:: !(Int,Int)
	, miItemSpaces	:: !(Int,Int)
	, miOrientation	:: ![(ViewDomain,Origin)]
	}

shiftControls :: Vector2 -> [WElementHandle ls ps] -> [WElementHandle ls ps]
shiftControls v [] = []
shiftControls v (itemH:itemHs) = shiftControl v itemH : shiftControls v itemHs
	where
		shiftControl :: Vector2 -> WElementHandle ls ps -> WElementHandle ls ps
		shiftControl v itemH@(WItemHandle {wItemPos=itemPos,wItems=itemHs}) =
			itemH{wItemPos=movePoint v itemPos, wItems=shiftControls v itemHs}
		shiftControl v (WListLSHandle itemHs) =
			WListLSHandle (shiftControls v itemHs)
		shiftControl v (WExtendLSHandle exLS itemHs) =
			WExtendLSHandle exLS (shiftControls v itemHs)
		shiftControl v (WChangeLSHandle chLS itemHs) =
			WChangeLSHandle chLS (shiftControls v itemHs)

setsliderthumb :: Bool -> OSWindowMetrics -> OSWindowPtr -> Bool -> (Int,Int,Int) -> Int -> Size -> IO ()
setsliderthumb hasScroll wMetrics itemPtr isHScroll scrollValues viewSize maxcoords
	| not hasScroll	= return ()
	| otherwise	= osSetCompoundSlider wMetrics itemPtr isHScroll (toOSscrollbarRange scrollValues viewSize) (toTuple maxcoords)


{-	Move the ViewFrame of a CompoundControl. (In future version also customised controls.)
-}
movecontrolviewframe :: Id -> Vector2 -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> IO (WindowHandle ls ps)
movecontrolviewframe id v wMetrics wids wH@(WindowHandle {whKind=whKind,whItems=itemHs,whSize=whSize,whAtts=whAtts,whSelect=whSelect,whDefaultId=whDefaultId,whWindowInfo=windowInfo}) = do
	let metricsInfo	= MetricsInfo {miOSMetrics=wMetrics,miHMargins=hMargins,miVMargins=vMargins,miItemSpaces=spaces,miOrientation=orientation}
	(_,itemHs,mb_updRgn) <- setWElement (moveWItemFrame metricsInfo (wPtr wids) whDefaultId True whSelect clipRect v) id itemHs Nothing
	let wH1 = wH{whItems=itemHs}
	(if isNothing mb_updRgn then return wH1
	 else do
		let updRgn = fromJust mb_updRgn
		empty <- osIsEmptyRgn updRgn
		(if empty
		 then do
			osDisposeRgn updRgn
			return wH1
		 else updateWindowBackgrounds wMetrics updRgn wids wH1))
	where
		(domainRect,origin,defHMargin,defVMargin) = 
			if whKind==IsDialog
			then (sizeToRect whSize,zero,osmHorMargin wMetrics,osmVerMargin wMetrics)
			else (windowDomain windowInfo,windowOrigin windowInfo,0,0)
		(defHSpace, defVSpace)	= (osmHorItemSpace wMetrics,osmVerItemSpace wMetrics)
		hMargins		= getWindowHMarginAtt   (snd (cselect isWindowHMargin   (WindowHMargin defHMargin defHMargin) whAtts))
		vMargins		= getWindowVMarginAtt   (snd (cselect isWindowVMargin   (WindowVMargin defVMargin defVMargin) whAtts))
		spaces			= getWindowItemSpaceAtt (snd (cselect isWindowItemSpace (WindowItemSpace defHSpace defVSpace) whAtts))
		clipRect		= getContentRect wMetrics windowInfo whSize
		orientation		= [(rectToRectangle domainRect,origin)]

		moveWItemFrame :: MetricsInfo -> OSWindowPtr -> Maybe Id -> Bool -> Bool -> Rect -> Vector2 -> Id -> WElementHandle ls ps -> Maybe OSRgnHandle -> IO (Bool,WElementHandle ls ps,Maybe OSRgnHandle)
		moveWItemFrame metricsInfo@(MetricsInfo {miOSMetrics=miOSMetrics,miHMargins=miHMargins,miVMargins=miVMargins,miItemSpaces=miItemSpaces,miOrientation=miOrientation}) wPtr defaultId shownContext ableContext clipRect v id itemH@(WItemHandle {wItemId=itemId,wItemKind=itemKind}) updRgn
			| not (isRecursiveControl itemKind) =
				return (identifyMaybeId id itemId,itemH,updRgn)
			| itemKind==IsLayoutControl =
				if identifyMaybeId id itemId
				then return (True,itemH,updRgn)
				else do
					let metricsInfo1 = metricsInfo{miHMargins=hMargins,miVMargins=vMargins,miItemSpaces=spaces}
					let clipRect1	 = intersectRects clipRect (posSizeToRect itemPos itemSize)
					(done,itemHs,updRgn) <- setWElement (moveWItemFrame metricsInfo1 wPtr defaultId shownContext1 ableContext1 clipRect1 v) id (wItems itemH) updRgn
					return (done,itemH{wItems=itemHs},updRgn)
			| not (identifyMaybeId id itemId) = do
				let orientation1 = (domain,oldOrigin):miOrientation
				let clipRect1	 = intersectRects contentRect clipRect
				let metricsInfo1 = metricsInfo{miHMargins=hMargins,miVMargins=vMargins,miItemSpaces=spaces,miOrientation=orientation}
				(done,itemHs,updRgn) <- setWElement (moveWItemFrame metricsInfo1 wPtr defaultId shownContext1 ableContext1 clipRect1 v) id (wItems itemH) updRgn
				return (done,itemH{wItems=itemHs},updRgn)
			| newOrigin==oldOrigin =
				return (True,itemH,updRgn)
			| otherwise = do
				setsliderthumb (hasHScroll && x newOrigin /= x oldOrigin) miOSMetrics itemPtr True  (minx,x newOrigin,maxx) viewx itemSize
				setsliderthumb (hasVScroll && y newOrigin /= y oldOrigin) miOSMetrics itemPtr False (miny,y newOrigin,maxy) viewy itemSize
			  	let info1 = info{compoundOrigin=newOrigin}
			  	let clipRect1 = intersectRects contentRect clipRect
				(if null (wItems itemH)
				 then do
					let itemH1 = itemH{wItemInfo=WCompoundInfo info1}
					itemH2 <- drawCompoundLook miOSMetrics ableContext1 wPtr clipRect1 itemH1
					return (True,itemH2,updRgn)
				 else do
					let oldItems	= wItems itemH
				  	let orientation	= (domain,newOrigin):miOrientation
					(_,newItems) <- layoutControls miOSMetrics hMargins vMargins spaces itemSize itemSize orientation oldItems
				  	let newItems = shiftControls (toVector itemPos) newItems
				  	let itemH1 = itemH{wItems=newItems,wItemInfo=WCompoundInfo info}
				  	maybe (return ()) osDisposeRgn updRgn
					itemH2 <- forceValidCompoundClipState miOSMetrics True wPtr defaultId shownContext itemH1
					updRgn <- relayoutControls miOSMetrics ableContext1 shownContext1 contentRect contentRect itemPos itemPos itemPtr defaultId oldItems (wItems itemH)
					itemH3 <- drawCompoundLook miOSMetrics ableContext1 wPtr clipRect1 itemH2
					return (True,itemH3,Just updRgn))
			where
				info			= getWItemCompoundInfo (wItemInfo itemH)
				oldOrigin		= compoundOrigin info
				domainRect		= compoundDomain info
				domain			= rectToRectangle domainRect
				itemPtr			= wItemPtr itemH
				itemPos			= wItemPos itemH
				itemSize		= wItemSize itemH
				itemAtts		= wItemAtts itemH
				(hasHScroll,hasVScroll)	= (isJust (compoundHScroll info),isJust (compoundVScroll info))
				visScrolls		= osScrollbarsAreVisible miOSMetrics domainRect (toTuple itemSize) (hasHScroll,hasVScroll)
				contentRect		= getCompoundContentRect miOSMetrics visScrolls (posSizeToRect itemPos itemSize)
				contentSize		= rectSize contentRect
				shownContext1		= if shownContext then wItemShow itemH else shownContext
				ableContext1		= ableContext && wItemSelect itemH
				hMargins		= getControlHMarginAtt (snd (cselect isControlHMargin (ControlHMargin (fst miHMargins) (snd miHMargins)) itemAtts))
				vMargins		= getControlVMarginAtt (snd (cselect isControlVMargin (ControlVMargin (fst miVMargins) (snd miVMargins)) itemAtts))
				spaces			= getControlItemSpaceAtt (snd (cselect isControlItemSpace (ControlItemSpace (fst miItemSpaces) (snd miItemSpaces)) itemAtts))
				(minx,maxx,viewx)	= (rleft domainRect,rright  domainRect, w contentSize)
				(miny,maxy,viewy)	= (rtop  domainRect,rbottom domainRect, h contentSize)
				newOrigin		= Point2{x=setBetween (x oldOrigin+vx v) minx (maxx-viewx),y=setBetween (y oldOrigin+vy v) miny (maxy-viewy)}


--	Set the ViewDomain of a CompoundControl. (In future versions also customised controls.)

setcontrolviewdomain :: Id -> ViewDomain -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> IO (WindowHandle ls ps)
setcontrolviewdomain id newDomain wMetrics wids wH@(WindowHandle {whKind=whKind,whItems=itemHs,whSize=whSize,whAtts=whAtts,whSelect=whSelect,whDefaultId=whDefaultId,whWindowInfo=windowInfo}) = do
	let metricsInfo	= MetricsInfo {miOSMetrics=wMetrics,miHMargins=hMargins,miVMargins=vMargins,miItemSpaces=spaces,miOrientation=orientation}
	(_,itemHs,mb_updRgn) <- setWElement (setWItemDomain metricsInfo (wPtr wids) whDefaultId True whSelect clipRect (validateViewDomain newDomain)) id itemHs Nothing
	let wH1 = wH{whItems=itemHs}
	(case mb_updRgn of
		Nothing -> return wH
		Just updRgn -> do
			empty <- osIsEmptyRgn updRgn
			(if empty then do
				osDisposeRgn updRgn
				return wH1
			 else updateWindowBackgrounds wMetrics updRgn wids wH1))
	where
		(domainRect,origin,defHMargin,defVMargin)
				= if whKind==IsDialog
				  then (sizeToRect whSize,zero,osmHorMargin wMetrics,osmVerMargin wMetrics)
				  else (windowDomain windowInfo,windowOrigin windowInfo,0,0)
		(defHSpace, defVSpace)	= (osmHorItemSpace wMetrics,osmVerItemSpace wMetrics)
		hMargins	= getWindowHMarginAtt (snd (cselect isWindowHMargin (WindowHMargin defHMargin defHMargin) whAtts))
		vMargins	= getWindowVMarginAtt (snd (cselect isWindowVMargin (WindowVMargin defVMargin defVMargin) whAtts))
		spaces		= getWindowItemSpaceAtt (snd (cselect isWindowItemSpace (WindowItemSpace defHSpace defVSpace) whAtts))
		clipRect	= getContentRect wMetrics windowInfo whSize
		orientation	= [(rectToRectangle domainRect,origin)]

		setWItemDomain :: MetricsInfo -> OSWindowPtr -> Maybe Id -> Bool -> Bool -> Rect -> ViewDomain -> Id -> WElementHandle ls ps -> Maybe OSRgnHandle -> IO (Bool,WElementHandle ls ps ,Maybe OSRgnHandle)
		setWItemDomain metricsInfo@(MetricsInfo {miOSMetrics=miOSMetrics,miHMargins=miHMargins,miVMargins=miVMargins,miItemSpaces=miItemSpaces,miOrientation=miOrientation}) wPtr defaultId shownContext ableContext clipRect newDomain id itemH@(WItemHandle {wItemId=itemId,wItemKind=wItemKind}) mb_updRgn
			| not (isRecursiveControl wItemKind) =
				return (identifyMaybeId id itemId,itemH,mb_updRgn)
			| wItemKind == IsLayoutControl =
				if identifyMaybeId id itemId 
				then return (True,itemH,mb_updRgn)
				else do
					let metricsInfo1 = metricsInfo{miHMargins=hMargins,miVMargins=vMargins,miItemSpaces=spaces}
					let clipRect1 = intersectRects clipRect (posSizeToRect itemPos itemSize)
					(done,itemHs,mb_updRgn) <- setWElement (setWItemDomain metricsInfo1 wPtr defaultId shownContext1 ableContext1 clipRect1 newDomain) id (wItems itemH) mb_updRgn
					return (done,itemH{wItems=itemHs},mb_updRgn)
			| not (identifyMaybeId id (wItemId itemH)) =
				let
					orientation  = (oldDomain,oldOrigin):miOrientation
					clipRect1    = intersectRects oldContentRect clipRect
					metricsInfo1 = metricsInfo{miHMargins=hMargins,miVMargins=vMargins,miItemSpaces=spaces,miOrientation=orientation}
				in do
					(done,itemHs,mb_updRgn) <- setWElement (setWItemDomain metricsInfo1 wPtr defaultId shownContext1 ableContext1 clipRect1 newDomain) id (wItems itemH) mb_updRgn
					return (done,itemH{wItems=itemHs},mb_updRgn)
			| newDomain == oldDomain =
				return (True,itemH,mb_updRgn)
			| otherwise = do
				let (minx,maxx,viewx) = (rleft newDomainRect,rright newDomainRect, w newContentSize)
			  	let (miny,maxy,viewy) = (rtop newDomainRect, rbottom newDomainRect,h newContentSize)
			  	let newOrigin	      = Point2 {x=setBetween (x oldOrigin) minx (max minx (maxx-viewx)),y=setBetween (y oldOrigin) miny (max miny (maxy-viewy))}
			  	let info1 = info{compoundOrigin=newOrigin,compoundDomain=newDomainRect}
				setsliderthumb hasHScroll miOSMetrics itemPtr True  (minx,x newOrigin,maxx) viewx itemSize
				setsliderthumb hasVScroll miOSMetrics itemPtr False (miny,y newOrigin,maxy) viewy itemSize
			  	let oldItems = wItems itemH
				(if null oldItems		-- CompoundControl has no controls
				 then let itemH1 = itemH{wItemInfo=WCompoundInfo info1}
				      in if shownContext1
					 then do
						itemH2 <- drawCompoundLook miOSMetrics ableContext1 wPtr (intersectRects newContentRect clipRect) itemH
						return (True,itemH2,mb_updRgn)
					 else return (True,itemH1,mb_updRgn)
				 else do				-- CompoundControl has controls
					let orientation = (newDomain,newOrigin):miOrientation
					(_,newItems) <- layoutControls miOSMetrics hMargins vMargins spaces itemSize itemSize orientation oldItems
			  		let newItems1 = shiftControls (toVector itemPos) newItems
			  		let itemH1 = itemH{wItems=newItems1,wItemInfo=WCompoundInfo info1}
					maybe (return ()) osDisposeRgn mb_updRgn
					itemH2 <- forceValidCompoundClipState miOSMetrics True wPtr defaultId shownContext itemH1
					updRgn <- relayoutControls miOSMetrics ableContext1 shownContext1 newContentRect newContentRect itemPos itemPos itemPtr defaultId oldItems (wItems itemH)
					(if shownContext1
					 then do
						itemH <- drawCompoundLook miOSMetrics ableContext1 wPtr (intersectRects newContentRect clipRect) itemH
						return (True,itemH,Just updRgn)
					 else
						return (True,itemH,Just updRgn)))
			where
				info			= getWItemCompoundInfo (wItemInfo itemH)
				oldOrigin		= compoundOrigin info
				oldDomainRect		= compoundDomain info
				oldDomain		= rectToRectangle oldDomainRect
				newDomainRect		= rectangleToRect newDomain
				itemPtr			= wItemPtr itemH
				itemPos			= wItemPos itemH
				itemSize		= wItemSize itemH				
				itemAtts		= wItemAtts itemH
				itemRect		= posSizeToRect itemPos itemSize
				(hasHScroll,hasVScroll)	= (isJust (compoundHScroll info),isJust (compoundVScroll info))
				oldVisScrolls		= osScrollbarsAreVisible miOSMetrics oldDomainRect (toTuple itemSize) (hasHScroll,hasVScroll)
				newVisScrolls		= osScrollbarsAreVisible miOSMetrics newDomainRect (toTuple itemSize) (hasHScroll,hasVScroll)
				oldContentRect		= getCompoundContentRect miOSMetrics oldVisScrolls itemRect
				newContentRect		= getCompoundContentRect miOSMetrics newVisScrolls itemRect
				newContentSize		= rectSize newContentRect
				shownContext1		= if shownContext then wItemShow itemH else shownContext
				ableContext1		= ableContext && wItemSelect itemH
				hMargins		= getControlHMarginAtt (snd (cselect isControlHMargin   (ControlHMargin   (fst miHMargins)   (snd miHMargins))   itemAtts))
				vMargins		= getControlVMarginAtt (snd (cselect isControlVMargin   (ControlVMargin   (fst miVMargins)   (snd miVMargins))   itemAtts))
				spaces			= getControlItemSpaceAtt (snd (cselect isControlItemSpace (ControlItemSpace (fst miItemSpaces) (snd miItemSpaces)) itemAtts))

setcontrolscrollfun	:: Id -> Direction -> ScrollFunction -> WindowHandle ls ps -> IO (WindowHandle ls ps)
setcontrolscrollfun id direction scrollFun wH@(WindowHandle {whItems=itemHs}) = do
	(_,itemHs,_) <- setWElement (setCompoundScrollFun direction scrollFun) id itemHs 0
	return wH{whItems=itemHs}
	where
		setCompoundScrollFun :: Direction -> ScrollFunction -> Id -> WElementHandle ls ps -> s -> IO (Bool,WElementHandle ls ps,s)
		setCompoundScrollFun direction scrollFun id itemH@(WItemHandle {wItemId=itemId,wItemKind=IsCompoundControl}) s
			| not (identifyMaybeId id itemId) = do
				(found,itemHs,s) <- setWElement (setCompoundScrollFun direction scrollFun) id (wItems itemH) s
				return (found,itemH{wItems=itemHs},s)
			| direction==Horizontal && isJust hScroll = do
				let info1 = info{compoundHScroll=fmap (setScrollFun scrollFun) hScroll}
				return (True,itemH{wItemInfo=WCompoundInfo info1},s)
			| direction==Vertical && isJust vScroll = do
				let info1 = info{compoundVScroll=fmap (setScrollFun scrollFun) vScroll}
				return (True,itemH{wItemInfo=WCompoundInfo info1},s)
			| otherwise =
				return (True,itemH,s)
			where
				info	= getWItemCompoundInfo (wItemInfo itemH)
				hScroll	= compoundHScroll info
				vScroll	= compoundVScroll info

				setScrollFun :: ScrollFunction -> ScrollInfo -> ScrollInfo
				setScrollFun f scrollInfo = scrollInfo{scrollFunction=f}

		setCompoundScrollFun direction scrollFun id itemH@(WItemHandle {wItemId=itemId,wItems=itemHs}) s
			| identifyMaybeId id itemId =
				return (True,itemH,s)
			| otherwise = do
				(found,itemHs,s) <- setWElement (setCompoundScrollFun direction scrollFun) id itemHs s
				return (found,itemH{wItems=itemHs},s)



--	Higher order monadic access functions on [WElementHandle ls ps]

type MapFunction  ps s = forall ls .       WElementHandle ls ps -> s -> IO (     WElementHandle ls ps,s)
type Map2Function ps s = forall ls . Id -> WElementHandle ls ps -> s -> IO (Bool,WElementHandle ls ps,s)

setAllWElements :: MapFunction ps s -> [WElementHandle ls ps] -> s -> IO ([WElementHandle ls ps],s)
setAllWElements f (itemH:itemHs) s = do
	(itemH, s) <- setWElement     f itemH  s
	(itemHs,s) <- setAllWElements f itemHs s
	return (itemH:itemHs,s)
	where
		setWElement :: MapFunction ps s -> WElementHandle ls ps -> s -> IO (WElementHandle ls ps, s)
		setWElement f itemH@(WItemHandle {}) s = f itemH s
		setWElement f (WListLSHandle itemHs) s = do
			(itemHs,s) <- setAllWElements f itemHs s
			return (WListLSHandle itemHs,s)
		setWElement f (WChangeLSHandle chLS itemHs) s = do
			(itemHs,s) <- setAllWElements f itemHs s
			return (WChangeLSHandle chLS itemHs,s)
		setWElement f (WExtendLSHandle exLS itemHs) s = do
			(itemHs,s) <- setAllWElements f itemHs s
			return (WExtendLSHandle exLS itemHs,s)
setAllWElements _ _ s = return ([],s)

setArgWElements :: MapFunction ps [arg] -> [WElementHandle ls ps] -> [arg] -> IO ([WElementHandle ls ps],[arg])
setArgWElements f itemHs args
	| null args || null itemHs
		= return (itemHs,args)
	| otherwise
		= do {
			(itemH1, args1) <- setArgWElements' f itemH   args;
			(itemHs2,args2) <- setArgWElements  f itemHs1 args1;
			return (itemH1 : itemHs2, args2)
		  }
	where
		(itemH:itemHs1)  = itemHs
		
		setArgWElements' :: MapFunction ps [arg] -> WElementHandle ls ps -> [arg] -> IO (WElementHandle ls ps, [arg])
		setArgWElements' f itemH@(WItemHandle {}) args = f itemH args
		setArgWElements' f (WListLSHandle itemHs) args = do
			(itemHs1,args1) <- setArgWElements f itemHs args
			return (WListLSHandle itemHs1,args1)
		setArgWElements' f (WChangeLSHandle chLS itemHs) args = do
			(itemHs1,args1) <- setArgWElements f itemHs args
			return (WChangeLSHandle chLS itemHs1,args1)
		setArgWElements' f (WExtendLSHandle exLS itemHs) args = do
			(itemHs1,args1) <- setArgWElements f itemHs args
			return (WExtendLSHandle exLS itemHs1,args1)


setWElement :: Map2Function ps s -> Id -> [WElementHandle ls ps] -> s -> IO (Bool,[WElementHandle ls ps],s)
setWElement f id [] s = return (False,[],s)
setWElement f id (itemH:itemHs) s
		= do {
			(done1,itemH1,s) <- setWElement' f id itemH s;
			if   done1
			then return (done1,itemH1:itemHs,s)
			else do {
					(done2,itemHs2,s) <- setWElement f id itemHs s;
					return (done2,itemH1:itemHs2,s);
			     }
		  }
	where
		setWElement' :: Map2Function ps s -> Id -> WElementHandle ls ps -> s -> IO (Bool,WElementHandle ls ps,s)
		setWElement' f id itemH@(WItemHandle {}) s = f id itemH s
		setWElement' f id (WListLSHandle itemHs) s = do
			(done,itemHs1,s) <- setWElement f id itemHs s
			return (done,WListLSHandle itemHs1,s)
		setWElement' f id (WChangeLSHandle chLS itemHs) s = do
			(done,itemHs1,s) <- setWElement f id itemHs s
			return (done,WChangeLSHandle chLS itemHs1,s)
		setWElement' f id (WExtendLSHandle exLS itemHs) s = do
			(done,itemHs1,s) <- setWElement f id itemHs s
			return (done,WExtendLSHandle exLS itemHs1,s)