-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Relayout
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Control.Relayout where


import Graphics.UI.ObjectIO.Relayout
import Graphics.UI.ObjectIO.Window.Access
import Graphics.UI.ObjectIO.Window.ClipState
import Graphics.UI.ObjectIO.OS.Rgn(OSRgnHandle)
import Graphics.UI.ObjectIO.OS.System(OSWindowMetrics)


{-	relayoutControls wMetrics isAble oldFrame newFrame oldParentPos newParentPos parentPtr defaultId old new
	resizes, moves, and updates changed WElementHandle(`)s. 
		isAble							is True iff the parent window/compound is Able.
		oldFrame						is the clipping rect of the parent window/compound at the original location and size.
		newFrame						is the clipping rect of the parent window/compound at the new location and size.
		oldParentPos and newParentPos	are the positions of the respective parent window/compound of the elements.
		parentPtr						is the parent window/dialog.
		defaultId						is the optional Id of the default control.
		old								contains the elements at their original location and size.
		new								contains the elements at their new location and size.
	relayoutControls assumes that the two lists contain elements that are identical except for size and position.
		If this is not the case, a runtime error will occur.
	relayoutControls assumes that the ClipStates of all compound elements are valid.
	The return OSRgnHandle is the area of the window that requires to be updated (use updatewindowbackgrounds [windowupdate] for this purpose).
-}
relayoutControls :: OSWindowMetrics -> Bool -> Bool -> Rect -> Rect -> Point2 -> Point2 -> OSWindowPtr -> Maybe Id ->
					[WElementHandle ls ps] -> [WElementHandle ls ps] -> IO OSRgnHandle
relayoutControls wMetrics isAble isVisible oldFrame newFrame oldParentPos newParentPos wPtr defaultId oldHs newHs =
     relayoutItems wMetrics oldFrame newFrame oldParentPos newParentPos wPtr
					(wElementHandlesToRelayoutItems isAble isVisible oldHs [])
					(wElementHandlesToRelayoutItems isAble isVisible newHs [])
     where
	wElementHandlesToRelayoutItems :: Bool -> Bool -> [WElementHandle ls ps] -> [RelayoutItem] -> [RelayoutItem]
	wElementHandlesToRelayoutItems isAble isVisible (itemH:itemHs) items =
	    wElementHandleToRelayoutItems isAble isVisible itemH (wElementHandlesToRelayoutItems isAble isVisible itemHs items)
	    where
		wElementHandleToRelayoutItems :: Bool -> Bool -> WElementHandle ls ps -> [RelayoutItem] -> [RelayoutItem]
		wElementHandleToRelayoutItems isAble isVisible itemH@(WItemHandle {wItemKind=wItemKind}) items =
		    wItemHandleToRelayoutItems wItemKind isAble isVisible itemH items
		    where
		      wItemHandleToRelayoutItems :: ControlKind -> Bool -> Bool -> WElementHandle ls ps -> [RelayoutItem] -> [RelayoutItem]
		      wItemHandleToRelayoutItems controlKind@IsRadioControl isAble isVisible itemH@(WItemHandle {wItemSelect=wItemSelect,wItemShow=wItemShow}) items =
			  radioItemToRelayoutItems (isAble && wItemSelect) (isVisible && wItemShow) (radioItems (getWItemRadioInfo (wItemInfo itemH))) items
			  where
			    radioItemToRelayoutItems :: Bool -> Bool -> [RadioItemInfo ls ps] -> [RelayoutItem] -> [RelayoutItem]
			    radioItemToRelayoutItems isAble isVisible (radio:radios) items =					
				radioItemInfoToRelayoutItem isAble isVisible radio :
				radioItemToRelayoutItems isAble isVisible radios items
				where
				  radioItemInfoToRelayoutItem :: Bool -> Bool -> RadioItemInfo ls ps -> RelayoutItem
				  radioItemInfoToRelayoutItem isAble isVisible info =
				      RelayoutItem
					  { rliItemKind	= controlKind
					  , rliItemPtr	= radioItemPtr  info
					  , rliItemPos	= radioItemPos  info
					  , rliItemSize	= radioItemSize info
					  , rliItemSelect=isAble
					  , rliItemShow	= isVisible
					  , rliItemInfo	= undefined
					  , rliItemLook	= undefined
					  , rliItems	= []
					  }
			    radioItemToRelayoutItems _ _ _ items = items

		      wItemHandleToRelayoutItems controlKind@IsCheckControl isAble isVisible itemH@(WItemHandle {wItemSelect=wItemSelect,wItemShow=wItemShow}) items =
			  checkItemToRelayoutItems (isAble && wItemSelect) (isVisible && wItemShow) (checkItems (getWItemCheckInfo (wItemInfo itemH))) items
			  where
			    checkItemToRelayoutItems :: Bool -> Bool -> [CheckItemInfo ls ps] -> [RelayoutItem] -> [RelayoutItem]
			    checkItemToRelayoutItems isAble isVisible (check:checks) items =
				checkItemInfoToRelayoutItem isAble isVisible check :
				checkItemToRelayoutItems isAble isVisible checks items
				where
				  checkItemInfoToRelayoutItem :: Bool -> Bool -> CheckItemInfo ls ps -> RelayoutItem
				  checkItemInfoToRelayoutItem isAble isVisible info =
				      RelayoutItem
					  { rliItemKind	= controlKind
					  , rliItemPtr	= checkItemPtr info
					  , rliItemPos	= checkItemPos info
					  , rliItemSize	= checkItemSize info
					  , rliItemSelect=isAble
					  , rliItemShow	= isVisible
					  , rliItemInfo	= undefined
					  , rliItemLook	= undefined
					  , rliItems	= []
					  }
			    checkItemToRelayoutItems _ _ _ items = items

		      wItemHandleToRelayoutItems controlKind isAble isVisible itemH items =
			  let item = RelayoutItem
				  { rliItemKind	= controlKind
				  , rliItemPtr	= wItemPtr  itemH
				  , rliItemPos	= wItemPos  itemH
				  , rliItemSize	= wItemSize itemH
				  , rliItemSelect=isAble'
				  , rliItemShow	= isVisible'
				  , rliItemInfo	= info
				  , rliItemLook	= look
				  , rliItems	= items'
				  }
			  in (item:items)
			  where
			      isAble' = isAble && wItemSelect itemH
			      isVisible' = isVisible && wItemShow itemH
			      (info,look,items') = getInfo controlKind isAble' isVisible' itemH

			      getInfo :: ControlKind -> Bool -> Bool -> WElementHandle ls ps -> (CompoundInfo,LookInfo,[RelayoutItem])
			      getInfo IsCompoundControl isAble isVisible (WItemHandle {wItemInfo=wItemInfo,wItems=wItems}) =
				  (info,compoundLook (compoundLookInfo info),wElementHandlesToRelayoutItems isAble isVisible wItems [])
				  where
				     info	= getWItemCompoundInfo wItemInfo
			      getInfo IsCustomButtonControl _ _ (WItemHandle {wItemInfo=wItemInfo}) =
				  (undefined,cButtonInfoLook (getWItemCustomButtonInfo wItemInfo),[])
			      getInfo IsCustomControl _ _ (WItemHandle {wItemInfo=wItemInfo}) =
				  (undefined,customInfoLook (getWItemCustomInfo wItemInfo),[])
			      getInfo IsLayoutControl isAble isVisible (WItemHandle {wItems=wItems}) =
				  (undefined,undefined,wElementHandlesToRelayoutItems isAble isVisible wItems [])
			      getInfo _ _ _ _ = (undefined,undefined,[])
		
		wElementHandleToRelayoutItems isAble isVisible (WListLSHandle itemHs) items =
		    wElementHandlesToRelayoutItems isAble isVisible itemHs items
		
		wElementHandleToRelayoutItems isAble isVisible (WExtendLSHandle exLS itemHs) items =
		    wElementHandlesToRelayoutItems isAble isVisible itemHs items
		
		wElementHandleToRelayoutItems isAble isVisible (WChangeLSHandle chLS itemHs) items =
		    wElementHandlesToRelayoutItems isAble isVisible itemHs items
	
	wElementHandlesToRelayoutItems _ _ _ items = items