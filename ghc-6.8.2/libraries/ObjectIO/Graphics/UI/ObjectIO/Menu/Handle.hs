-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Menu.Handle
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Clean to Haskell Standard Object I\/O library, version 1.2
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Menu.Handle where

import	Graphics.UI.ObjectIO.StdMenuDef
import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.Receiver.Handle
import  Graphics.UI.ObjectIO.SystemId(SystemId)
import  Graphics.UI.ObjectIO.Id(IdParent(..), IdTable, okMembersIdTable)
import  Graphics.UI.ObjectIO.Device.Types(Device(..))
import	Graphics.UI.ObjectIO.OS.Menu
import	Graphics.UI.ObjectIO.OS.Types(OSMenu,OSMenuItem,osNoMenuItem)
import  qualified Data.Map as Map

type MenuElementState ls ps = MenuElementHandle ls ps		-- The internal implementation of a menu element is a MenuElementHandle

data MenuHandles ps
   = MenuHandles
   	{ mMenus	:: ![MenuStateHandle ps]		-- The menus and their elements of a process
	, mKeys		:: ![Char]				-- All shortcut keys of the menus
	, mEnabled	:: !Bool				-- Flag: the whole menusystem is enabled
	, mNrMenuBound	:: !Bound				-- The maximum number of menus that are allowed to be opened
	, mPopUpId	:: !(Maybe Id)				-- The Id of the PopUpMenu (Nothing if open; (Just id) if available)
	}
data MenuStateHandle ps
   = forall ls . MenuStateHandle (MenuLSHandle ls ps)

data MenuLSHandle ls ps
   = MenuLSHandle						-- A menu with local state
      	{ mlsState	:: ls					-- The local state of this menu
   	, mlsHandle	:: MenuHandle ls ps			-- The menu implementation
	}
data MenuHandle ls ps
   = MenuHandle
   	{ mHandle	:: !OSMenu				-- The handle to the menu as created by the OS
	, mMenuId	:: !Id					-- The menu id
	, mTitle	:: !String				-- The title of the menu
	, mSelect	:: !Bool				-- The MenuSelect==Able (by default True)
	, mItems	:: ![MenuElementHandle ls ps]		-- The menu elements of this menu
	}
data MenuElementHandle ls ps 
        = MenuItemHandle
	    { mItemId		:: !(Maybe Id)
	    , mItemKey		:: !(Maybe Char)
	    , mItemTitle	:: !Title
	    , mItemSelect	:: !Bool
	    , mItemMark		:: !Bool
	    , mItemAtts		:: ![MenuAttribute ls ps]
	    , mOSMenuItem	:: !OSMenuItem
	    }	
	| MenuReceiverHandle
	    { mReceiverHandle	:: !(ReceiverHandle ls ps)
	    , mReceiverAtts	:: ![MenuAttribute ls ps]
	    }
	| SubMenuHandle
	    { mSubHandle	:: !OSMenu
	    , mSubMenuId	:: !(Maybe Id)
	    , mSubItems		:: ![MenuElementHandle ls ps]
	    , mSubTitle		:: !Title
	    , mSubSelect	:: !Bool
	    , mSubAtts		:: ![MenuAttribute ls ps]
	    }
	| RadioMenuHandle
	    { mRadioId		:: !(Maybe Id)
	    , mRadioIndex	:: !Int
	    , mRadioItems	:: ![MenuElementHandle ls ps]
	    , mRadioSelect	:: !Bool
	    , mRadioAtts	:: ![MenuAttribute ls ps]
	    }
	| MenuSeparatorHandle
	    { mSepId		:: !(Maybe Id)
	    , mOSMenuSeparator  :: !OSMenuItem
	    }
	| MenuListLSHandle	![MenuElementHandle ls ps]
	| forall ls1 . 
	  MenuExtendLSHandle ls1 ![MenuElementHandle (ls1,ls) ps]
	| forall ls1 .
	  MenuChangeLSHandle ls1 ![MenuElementHandle ls1 ps]

menuHandleFatalError :: String -> String -> x
menuHandleFatalError function error
	= dumpFatalError function "menuhandle" error


--	Conversion functions from MenuElementState to MenuElementHandle, and vice versa:
menuElementHandleToMenuElementState :: MenuElementHandle ls ps -> MenuElementState ls ps
menuElementHandleToMenuElementState mH = mH

menuElementStateToMenuElementHandle :: MenuElementState ls ps  -> MenuElementHandle ls ps
menuElementStateToMenuElementHandle mH = mH

{-	menuIdsAreConsistent checks whether the MenuElementHandles contain (R(2))Ids that have already been
	associated with open receivers and if there are no duplicate Ids. 
	Neither the ReceiverTable nor the IdTable are changed if there are duplicate (R(2))Ids; 
	otherwise all (R(2))Ids have been bound.
-}
menuIdsAreConsistent :: SystemId -> Id -> [MenuElementHandle ls ps] -> IdTable -> Maybe IdTable
menuIdsAreConsistent ioId menuId itemHs it =
    let ids	= foldr getMenuElementMenuId [] itemHs
    in if not (okMembersIdTable ids it) then Nothing
       else Just (foldr insMap it ids)
    where
        insMap id tbl = Map.insert id IdParent {idpIOId=ioId, idpDevice=MenuDevice, idpId=menuId} tbl
	getMenuElementMenuId :: MenuElementHandle ls ps -> [Id] -> [Id]
	getMenuElementMenuId itemH@(MenuItemHandle {mItemId=mItemId}) ids =
	    case mItemId of
		Nothing -> ids
		Just id -> id : ids
	getMenuElementMenuId itemH@(MenuReceiverHandle rH _) ids = rId rH : ids
	getMenuElementMenuId itemH@(SubMenuHandle {mSubMenuId=mSubMenuId,mSubItems=itemHs}) ids =
	    let ids1 = foldr getMenuElementMenuId ids itemHs
	    in case mSubMenuId of
		   Nothing -> ids1
		   Just id -> id : ids1
	getMenuElementMenuId itemH@(RadioMenuHandle {mRadioId=mRadioId,mRadioItems=itemHs}) ids =
	    let ids1 = foldr getMenuElementMenuId ids itemHs
	    in case mRadioId of
		   Nothing -> ids1
		   Just id -> id : ids1
	getMenuElementMenuId itemH@(MenuSeparatorHandle {mSepId=mSepId}) ids =
	    case mSepId of
	    	Nothing -> ids
		Just id -> id : ids
	getMenuElementMenuId (MenuListLSHandle   itemHs) ids =
	    foldr getMenuElementMenuId ids itemHs
	getMenuElementMenuId (MenuExtendLSHandle _ itemHs) ids =
	    foldr getMenuElementMenuId ids itemHs
	getMenuElementMenuId (MenuChangeLSHandle _ itemHs) ids =
	    foldr getMenuElementMenuId ids itemHs
	

--	Convert a RadioMenuItem to the MenuItemHandle alternative of MenuElementHandle:
radioMenuItemToMenuElementHandle :: MenuRadioItem (ls,ps) ps -> MenuElementHandle ls ps
radioMenuItemToMenuElementHandle (title,optId,optShortKey,f)
	= MenuItemHandle
		{ mItemId	= optId
		, mItemKey	= optShortKey
		, mItemTitle	= title
		, mItemSelect	= True
		, mItemMark	= False
		, mItemAtts	= [MenuFunction f]
		, mOSMenuItem	= osNoMenuItem
		}
