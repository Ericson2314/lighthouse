-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Menu.DefAccess
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Menu.DefAccess where


import Graphics.UI.ObjectIO.StdMenuAttribute
import Graphics.UI.ObjectIO.CommonDef
import Data.List(find)


menuDefGetMenuId :: Menu m ls ps -> Maybe Id
menuDefGetMenuId menu@(Menu _ _ atts) =
	fmap getMenuIdAtt (find isMenuId atts)	

menuDefGetSelectState :: Menu m ls ps -> SelectState
menuDefGetSelectState menu@(Menu _ _ atts)
	= getMenuSelectStateAtt (snd (cselect isMenuSelectState (MenuSelectState Able) atts))

menuDefSetAbility :: Menu m ls ps -> SelectState -> Menu m ls ps
menuDefSetAbility (Menu name items atts) able =
	Menu name items (setSelectState able atts)
	where
	    setSelectState :: SelectState -> [MenuAttribute ls ps] -> [MenuAttribute ls ps]
	    setSelectState select atts
		    | found		= atts1
		    | otherwise		= att : atts1
	    	    where
			  att		= MenuSelectState select
			  (found,atts1)	= creplace isMenuSelectState att atts

menuDefGetTitle :: Menu m ls ps -> Title
menuDefGetTitle (Menu name _ _) = name

menuDefGetElements :: Menu m ls ps -> m ls ps
menuDefGetElements (Menu _ items _) = items

menuDefSetElements :: Menu m ls ps -> m ls ps -> Menu m ls ps
menuDefSetElements (Menu name _ atts) items = Menu name items atts

menuDefGetIndex :: Menu m ls ps -> Maybe Index
menuDefGetIndex menu@(Menu _ _ atts) =
	fmap getMenuIndexAtt (find isMenuIndex atts)
