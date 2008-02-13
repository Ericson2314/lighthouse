-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  KeyFocus
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.KeyFocus where



import	Graphics.UI.ObjectIO.CommonDef
import  Data.List(find)


data	KeyFocus
	= KeyFocus
		{ kfItem    :: !(Maybe Int)	-- Case (Just nr): the item with (wItemNr nr) has the keyboard input focus; Nothing: no item has focus
		, kfItems   :: ![FocusItem]	-- The items of the window that can have the keyboard input focus
		}
data	FocusItem
	= FocusItem
		{ focusNr   :: !Int		-- The item nr of the item
		, focusShow :: !Bool		-- Flag: True iff item is visible
		}

isShownFocusItem :: FocusItem -> Bool
isShownFocusItem = focusShow

eqFocusItemNr :: Int -> FocusItem -> Bool
eqFocusItemNr nr item = nr == focusNr item

newFocusItems :: [FocusItem] -> KeyFocus
newFocusItems items
	= KeyFocus
		{ kfItem  = fmap focusNr (find isShownFocusItem items)
		, kfItems = items
		}

openFocusItems :: (Maybe Int) -> [FocusItem] -> KeyFocus -> KeyFocus
openFocusItems (Just behind) new kf@(KeyFocus{kfItems=kfItems})
	= kf{kfItems=openFocusItems' behind new kfItems}
	where
		openFocusItems' :: Int -> [FocusItem] -> [FocusItem] -> [FocusItem]
		openFocusItems' behind new ((item@(FocusItem{focusNr=focusNr})):items)
			| behind==focusNr	= (item : (new++items))
			| otherwise		= (item : openFocusItems' behind new items)
		openFocusItems' _ new _
			= new
openFocusItems _ items kf@(KeyFocus{kfItems=kfItems})
	= kf{kfItems = kfItems++items}

closeFocusItems :: [Int] -> KeyFocus -> KeyFocus
closeFocusItems nrs kf@(KeyFocus{kfItem=kfItem,kfItems=kfItems})
	= kf{ kfItems = closeFocusItems' nrs kfItems
	    , kfItem  = case kfItem of
	    		  Nothing     -> Nothing
	    		  Just kfItem -> if kfItem `elem` nrs then Nothing else Just kfItem
	    }
	where
		closeFocusItems' :: [Int] -> [FocusItem] -> [FocusItem]
		closeFocusItems' []   _ = []
		closeFocusItems' nrs [] = []
		closeFocusItems' nrs (item:items) =
		    if found then closeFocusItems' nrs1 items
		    else (item : closeFocusItems' nrs1 items)
		    where
			(found,nrs1) = removeCheck (focusNr item) nrs

showFocusItems :: [Int] -> KeyFocus -> KeyFocus
showFocusItems nrs kf@(KeyFocus{kfItems=kfItems})
	= kf{kfItems=setShowFocusItems True nrs kfItems}

setShowFocusItems :: Bool -> [Int] -> [FocusItem] -> [FocusItem]
setShowFocusItems show  [] _  = []
setShowFocusItems show nrs [] = []
setShowFocusItems show nrs (item:items) =	
	(if found then item{focusShow=show} else item) : setShowFocusItems show nrs1 items
	where
	   (found,nrs1)	= removeCheck (focusNr item) nrs
	   

hideFocusItems :: [Int] -> KeyFocus -> KeyFocus
hideFocusItems nrs kf@(KeyFocus {kfItem=kfItem,kfItems=kfItems})
	= kf{ kfItems	= setShowFocusItems False nrs kfItems
	    , kfItem	= case kfItem of 
	     		    Nothing 	-> Nothing
	     		    Just kfItem -> if kfItem `elem` nrs then Nothing else Just kfItem
	    }

getCurrentFocusItem :: KeyFocus -> Maybe Int
getCurrentFocusItem = kfItem

setNoFocusItem :: KeyFocus -> KeyFocus
setNoFocusItem kf = kf{kfItem=Nothing}

setNewFocusItem :: Int -> KeyFocus -> KeyFocus
setNewFocusItem new kf
	= kf{kfItem=Just new}

setNextFocusItem :: (Maybe Int) -> KeyFocus -> (Maybe Int,KeyFocus)
setNextFocusItem (Just behind) kf@(KeyFocus {kfItems=kfItems}) =
	let (before,item_after)	= span (not . (eqFocusItemNr behind)) kfItems
	in 
	  if null item_after then (Nothing,kf)
	  else
	     case find isShownFocusItem (tail item_after) of
	       Just (item@(FocusItem {focusNr=nr})) -> (Just nr, kf{kfItem=Just nr})
	       Nothing -> case find isShownFocusItem before of
	  		    Just (item@(FocusItem {focusNr=nr})) -> (Just nr, kf{kfItem=Just nr})
			    Nothing -> (Nothing, kf)
			 
setNextFocusItem Nothing kf@(KeyFocus {kfItems=kfItems}) =
	case find isShownFocusItem kfItems of
	  Nothing -> (Nothing,kf{kfItem=Nothing})
 	  Just (item@(FocusItem {focusNr=nr})) -> (Just nr, kf{kfItem=Just nr})