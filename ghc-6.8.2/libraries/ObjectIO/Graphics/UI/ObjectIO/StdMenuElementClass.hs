-----------------------------------------------------------------------------
-- |
-- Module      :  StdMenuElementClass
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Definition of the MenuElements class for menu elements.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdMenuElementClass(MenuElements(..), PopUpMenuElements(..)) where


import  Graphics.UI.ObjectIO.StdIOBasic(TupLS(..))
import  Graphics.UI.ObjectIO.CommonDef
import  Graphics.UI.ObjectIO.Process.IOState
import  Graphics.UI.ObjectIO.StdMenuAttribute
import	Graphics.UI.ObjectIO.Menu.DefAccess
import	Graphics.UI.ObjectIO.Menu.Handle
import	Graphics.UI.ObjectIO.OS.Menu
import	Graphics.UI.ObjectIO.OS.Types(osNoMenu,osNoMenuItem)

-- | Translating menu elements into the internal representation.
-- There are instances for combinator types: 'AddLS', 'NewLS',
-- 'ListLS', 'NilLS' and 'TupLS' and for the concrete menu elements:
-- 'MenuItem', 'MenuSeparator', 'SubMenu' and 'RadioMenu'
class MenuElements m where
	menuElementToHandles	:: m ls ps -> GUI ps [MenuElementState ls ps]	


{-	Fields which values can not be determined now are filled with dummy values.
	These are the following:
	-	SubMenuHandle
		-	mSubHandle			(the handle to the sub menu)
	-	MenuItemHandle
		-	mOSMenuItem			(the handle to the item)
	-	MenuSeparatorHandle
		-	mOSMenuSeparator	(the handle to the item)
	The remaining attributes are copied.
-}
instance MenuElements m => MenuElements (AddLS m) where
	menuElementToHandles (AddLS addLS addDef) = do
		ms <- menuElementToHandles addDef
		return  [menuElementHandleToMenuElementState
				(MenuExtendLSHandle addLS (map menuElementStateToMenuElementHandle ms))
			]

instance MenuElements m => MenuElements (NewLS m) where
	menuElementToHandles (NewLS newLS newDef) = do
		ms <- menuElementToHandles newDef
		return  [menuElementHandleToMenuElementState
				(MenuChangeLSHandle newLS (map menuElementStateToMenuElementHandle ms))
			]

instance MenuElements m => MenuElements (ListLS m) where	
	menuElementToHandles (ListLS mDefs) = do
		mss <- mapM menuElementToHandles mDefs
		return  [menuElementHandleToMenuElementState
			  	(MenuListLSHandle (map menuElementStateToMenuElementHandle (concat mss)))
			]

instance MenuElements NilLS where	
	menuElementToHandles NilLS = do
		return [menuElementHandleToMenuElementState (MenuListLSHandle [])]

instance (MenuElements m1, MenuElements m2) => MenuElements (TupLS m1 m2) where	
	menuElementToHandles (m1:+:m2) = do
		ms1 <- menuElementToHandles m1
		ms2 <- menuElementToHandles m2
		return (ms1 ++ ms2)	

instance MenuElements m => MenuElements (SubMenu m) where	
	menuElementToHandles (SubMenu title items atts) = do
		ms <- menuElementToHandles items
		let (selectAtt,atts1) = validateSelectState atts
		let (idAtt,    atts2) = validateId          atts1
		return [menuElementHandleToMenuElementState
			  (SubMenuHandle 
			  	{ mSubHandle	= osNoMenu
				, mSubMenuId	= idAtt
				, mSubItems	= map menuElementStateToMenuElementHandle ms
				, mSubTitle	= title
				, mSubSelect	= enabled selectAtt
				, mSubAtts	= atts
				}
			  )
			]		  

instance MenuElements RadioMenu where	
	menuElementToHandles (RadioMenu items index atts) = do
		let nrRadios = length items
		let validIndex = if nrRadios==0 then 0 else (setBetween index 1 nrRadios)
		let itemHs = validateRadioMenuIndex validIndex (map radioMenuItemToMenuElementHandle items)
		let (selectAtt,atts1) = validateSelectState atts
		let (idAtt,    atts2) = validateId          atts1
		return [menuElementHandleToMenuElementState
			  (RadioMenuHandle 
			  	{ mRadioId	= idAtt
				, mRadioIndex	= validIndex
				, mRadioItems	= itemHs
				, mRadioSelect	= enabled selectAtt
				, mRadioAtts	= atts
				}
			  )
			]

instance MenuElements MenuItem where	
	menuElementToHandles (MenuItem title atts) = do
		let (selectAtt,atts1)	= validateSelectState atts
		let (markAtt,  atts2)	= validateMarkState   atts1
		let (keyAtt,   atts3)	= validateShortKey    atts2
		let (idAtt,    atts4)	= validateId          atts3
		return [menuElementHandleToMenuElementState
			  (MenuItemHandle
			  	{ mItemId	= idAtt
				, mItemKey	= keyAtt
				, mItemTitle	= title
				, mItemSelect	= enabled selectAtt
				, mItemMark	= marked  markAtt
				, mItemAtts	= atts
				, mOSMenuItem	= osNoMenuItem
				}
			  )
			]

instance MenuElements MenuSeparator where	
	menuElementToHandles (MenuSeparator atts) = do
		let (idAtt,_) = validateId atts
		return [menuElementHandleToMenuElementState 
			  (MenuSeparatorHandle
			  	{ mSepId	   = idAtt
				, mOSMenuSeparator = osNoMenuItem
				}
			  )
			]		  


--	Obtain the SelectState attribute from the attribute list:
validateSelectState :: [MenuAttribute ls ps] -> (SelectState,[MenuAttribute ls ps])
validateSelectState atts =
	let (found,selectAtt,atts1)= remove isMenuSelectState undefined atts
	in if found then (getMenuSelectStateAtt selectAtt,atts1)
	   else (Able,atts)

--	Obtain the MarkState attribute from the attribute list:
validateMarkState :: [MenuAttribute ls ps] -> (MarkState,[MenuAttribute ls ps])
validateMarkState atts =
	let (found,markAtt,atts1) = remove isMenuMarkState undefined atts
	in if found then (getMenuMarkStateAtt markAtt,atts1)
	   else (NoMark,atts)

--	Obtain the Id attribute from the attribute list:
validateId :: [MenuAttribute ls ps] -> (Maybe Id,[MenuAttribute ls ps])
validateId atts =
	let (found,idAtt,atts1)	= remove isMenuId undefined atts
	in if found then (Just (getMenuIdAtt idAtt),atts1)
	   else (Nothing,atts)

--	Obtain the ShortKey attribute from the attribute list:
validateShortKey :: [MenuAttribute ls ps] -> (Maybe Char,[MenuAttribute ls ps])
validateShortKey atts =
	let (hasKey,keyAtt,atts1) = remove isMenuShortKey undefined atts
	in if hasKey then (Just (getMenuShortKeyAtt keyAtt),atts1)
	   else (Nothing,atts)

--	validateRadioMenuIndex ensures that only the element at the valid index position of the RadioMenu
--	has a check mark and all others don't.
validateRadioMenuIndex :: Int -> [MenuElementHandle ls ps] -> [MenuElementHandle ls ps]
validateRadioMenuIndex index itemHs =
	fst (stateMap (\itemH i -> (itemH{mItemMark=i==index},i+1)) itemHs 1)


{-	Menu elements for PopUpMenus: -}

-- | Translating menu elements into the internal representation.
-- For the PopUpMenuElements class there are instances for same types as 'MenuElements'
class PopUpMenuElements m where
	popUpMenuElementToHandles :: m ls ps -> GUI ps [MenuElementState ls ps]

instance PopUpMenuElements m => PopUpMenuElements (AddLS m) where
	popUpMenuElementToHandles (AddLS addLS addDef) = do
		ms <- popUpMenuElementToHandles addDef
		return  [menuElementHandleToMenuElementState
				(MenuExtendLSHandle addLS (map menuElementStateToMenuElementHandle ms))
			]

instance PopUpMenuElements m => PopUpMenuElements (NewLS m) where
	popUpMenuElementToHandles (NewLS newLS newDef) = do
		ms <- popUpMenuElementToHandles newDef
		return  [menuElementHandleToMenuElementState
				(MenuChangeLSHandle newLS (map menuElementStateToMenuElementHandle ms))
			]

instance PopUpMenuElements m => PopUpMenuElements (ListLS m) where
	popUpMenuElementToHandles (ListLS mDefs) = do
		mss <- mapM popUpMenuElementToHandles mDefs
		return  [menuElementHandleToMenuElementState
			  	(MenuListLSHandle (map menuElementStateToMenuElementHandle (concat mss)))
			]

instance PopUpMenuElements NilLS where
	popUpMenuElementToHandles NilLS = do
		return [menuElementHandleToMenuElementState (MenuListLSHandle [])]

instance (PopUpMenuElements m1, PopUpMenuElements m2) => PopUpMenuElements (TupLS m1 m2) where	
	popUpMenuElementToHandles (m1:+:m2) = do
		ms1 <- popUpMenuElementToHandles m1
		ms2 <- popUpMenuElementToHandles m2
		return (ms1 ++ ms2)

instance PopUpMenuElements RadioMenu where	
	popUpMenuElementToHandles  = menuElementToHandles

instance PopUpMenuElements MenuItem where	
	popUpMenuElementToHandles  = menuElementToHandles

instance PopUpMenuElements MenuSeparator where	
	popUpMenuElementToHandles  = menuElementToHandles
