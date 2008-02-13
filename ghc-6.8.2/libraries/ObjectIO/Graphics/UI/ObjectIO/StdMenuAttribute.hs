-----------------------------------------------------------------------------
-- |
-- Module      :  StdMenuAttribute
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdMenuAttribute specifies which MenuAttributes are valid for each of the
-- standard menus and menu elements.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdMenuAttribute
		( module Graphics.UI.ObjectIO.StdMenuAttribute
		, module Graphics.UI.ObjectIO.StdMenuDef
		) where


import Graphics.UI.ObjectIO.StdMenuDef
import Graphics.UI.ObjectIO.Id

isValidMenuAttribute :: MenuAttribute ls ps -> Bool
isValidMenuAttribute (MenuId          _)	= True
isValidMenuAttribute (MenuIndex       _)	= True
isValidMenuAttribute (MenuInit        _)	= True
isValidMenuAttribute (MenuSelectState _)	= True
isValidMenuAttribute _				= False

isValidSubMenuAttribute :: MenuAttribute ls ps -> Bool
isValidSubMenuAttribute (MenuId          _)	= True
isValidSubMenuAttribute (MenuSelectState _)	= True
isValidSubMenuAttribute _			= False

isValidRadioMenuAttribute :: MenuAttribute ls ps -> Bool
isValidRadioMenuAttribute (MenuId          _)	= True
isValidRadioMenuAttribute (MenuSelectState _)	= True
isValidRadioMenuAttribute _			= False

isValidMenuItemAttribute :: MenuAttribute ls ps -> Bool
isValidMenuItemAttribute (MenuIndex _)	= False
isValidMenuItemAttribute (MenuInit  _)	= False
isValidMenuItemAttribute _		= True

isValidMenuSeparatorAttribute :: MenuAttribute ls ps -> Bool
isValidMenuSeparatorAttribute (MenuId _)	= True
isValidMenuSeparatorAttribute _			= False

isMenuFunction	:: MenuAttribute ls ps -> Bool
isMenuFunction	(MenuFunction _)	= True
isMenuFunction	_			= False

isMenuId :: MenuAttribute ls ps -> Bool
isMenuId (MenuId _)	= True
isMenuId _		= False

isMenuIndex :: MenuAttribute ls ps -> Bool
isMenuIndex (MenuIndex _)	= True
isMenuIndex _			= False

isMenuInit :: MenuAttribute ls ps -> Bool
isMenuInit (MenuInit _)	= True
isMenuInit _		= False

isMenuMarkState	:: MenuAttribute ls ps -> Bool
isMenuMarkState	(MenuMarkState _)	= True
isMenuMarkState	_			= False

isMenuModsFunction :: MenuAttribute ls ps -> Bool
isMenuModsFunction (MenuModsFunction _)	= True
isMenuModsFunction _			= False

isMenuSelectState :: MenuAttribute ls ps -> Bool
isMenuSelectState (MenuSelectState _)	= True
isMenuSelectState _			= False

isMenuShortKey :: MenuAttribute ls ps -> Bool
isMenuShortKey (MenuShortKey _)		= True
isMenuShortKey _			= False


getMenuFun :: MenuAttribute ls ps -> GUIFun ls ps
getMenuFun (MenuFunction f) = f

getMenuIdAtt :: MenuAttribute ls ps -> Id
getMenuIdAtt (MenuId id) = id

getMenuIndexAtt :: MenuAttribute ls ps -> Index
getMenuIndexAtt (MenuIndex index) = index

getMenuInitFun :: MenuAttribute ls ps -> ps -> GUI ps ps
getMenuInitFun (MenuInit f) = f

getMenuMarkStateAtt :: MenuAttribute ls ps -> MarkState
getMenuMarkStateAtt (MenuMarkState mark) = mark

getMenuModsFun :: MenuAttribute ls ps -> ModifiersFunction ls ps
getMenuModsFun (MenuModsFunction f) = f

getMenuSelectStateAtt :: MenuAttribute ls ps -> SelectState
getMenuSelectStateAtt (MenuSelectState select) = select

getMenuShortKeyAtt :: MenuAttribute ls ps -> Char
getMenuShortKeyAtt (MenuShortKey key) = key
