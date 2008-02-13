-----------------------------------------------------------------------------
-- |
-- Module      :  StdMenuDef
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Definition of Menus and MenuElements:
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdMenuDef 
		(
		-- * Menus
		  Menu(..), PopUpMenu(..),
		  
		-- * Menu items
		  MenuItem(..), MenuSeparator(..)
		, RadioMenu(..), MenuRadioItem(..)
		, SubMenu(..),
		
		-- * Menu attributes
		  MenuAttribute(..),
		  
		-- * A visible modules
		  module Graphics.UI.ObjectIO.StdGUI 
		, module Graphics.UI.ObjectIO.StdIOCommon
		) where


import  Graphics.UI.ObjectIO.StdGUI
import	Graphics.UI.ObjectIO.StdIOCommon


--	Menus:

-- | The standard menus that are usually placed at the top of the process window and
-- can be selected at any time
data	Menu        m ls ps = Menu        Title         (m ls ps)        [MenuAttribute ls ps]

-- | The popup menus can be created and shown at any time as a response to
-- any other event (usually to the click of the right mouse button).
data	PopUpMenu   m ls ps = PopUpMenu                 (m ls ps)


--	Menu elements:

-- | The simple menu item is just an item with a specified title and an event handler
-- which is called when the item is clicked
data	MenuItem      ls ps = MenuItem    Title                       	   [MenuAttribute ls ps]

-- | The menu separator is nonselectable item which can be used to
-- separate menu items in different groups
data	MenuSeparator ls ps = MenuSeparator                           	   [MenuAttribute ls ps]

-- | The radio menu is a group of items which can be used as 'RadioControl'
data	RadioMenu     ls ps = RadioMenu   [MenuRadioItem (ls,ps) ps] Index [MenuAttribute ls ps]

-- | The sub menu item is an item which shows a sub menu when the user selects it.
data	SubMenu     m ls ps = SubMenu     Title        (m ls ps)       	   [MenuAttribute ls ps]

type	MenuRadioItem st ps = (Title,Maybe Id,Maybe Char,st -> GUI ps st)

data	MenuAttribute ls ps							-- Default:
 --	Attributes for Menus and MenuElements:
	=	MenuId			Id					-- no Id
	|	MenuSelectState		SelectState				-- menu(item) Able
 --	Attributes only for Menus:
	|	MenuIndex		Int					-- at the end of the current menu list
	|	MenuInit		(ps -> GUI ps ps)			-- no actions after opening menu
 --	Attributes ignored by (Sub)Menus:
	|	MenuFunction		(GUIFun ls ps)				-- return
	|	MenuMarkState		MarkState				-- NoMark
	|	MenuModsFunction	(ModifiersFunction ls ps)		-- MenuFunction
	|	MenuShortKey		Char					-- no ShortKey

