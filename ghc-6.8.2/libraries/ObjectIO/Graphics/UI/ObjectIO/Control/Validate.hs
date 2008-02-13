-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Validate
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Control.Validate contains control validation functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Control.Validate
		( validateControlTitle, validateSliderState
		, getWElementControlIds
                , noDuplicateControlIds, disjointControlIds
                , controlIdsAreConsistent
                ) where


import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.Window.Handle
import Graphics.UI.ObjectIO.Receiver.Handle
import Graphics.UI.ObjectIO.OS.Window
import qualified Data.Map as Map


controlvalidateFatalError :: String -> String -> x
controlvalidateFatalError function error
	= dumpFatalError function "ControlValidate" error


--	Validate the title of a control.

validateControlTitle :: String -> String
validateControlTitle string
	= removeSpecialChars osControlTitleSpecialChars string


--	Validate the settings of a slider.

validateSliderState :: SliderState -> SliderState
validateSliderState (SliderState{sliderMin=sMin, sliderMax=sMax, sliderThumb=thumb})
	= SliderState
	  { sliderMin	= min'
	  , sliderMax	= max'
	  , sliderThumb	= setBetween thumb min' max'
	  }
	where
		min' = min sMin sMax
		max' = max sMin sMax
	
--	Collect all Ids of the given [WElementHandle].

getWElementControlIds :: [WElementHandle ls ps] -> [Id]
getWElementControlIds [] = []
getWElementControlIds (itemH:itemHs) = ids1++ids2
	where
		ids1 = getWElementIds itemH
		ids2 = getWElementControlIds itemHs
		

		getWElementIds :: (WElementHandle ls ps) -> [Id]
		getWElementIds (WItemHandle {wItemId=id,wItems=itemHs}) = maybeToList id ++ getWElementControlIds itemHs
		getWElementIds (WListLSHandle itemHs) = getWElementControlIds itemHs
		getWElementIds (WExtendLSHandle addLS itemHs) = getWElementControlIds itemHs
		getWElementIds (WChangeLSHandle newLS itemHs) = getWElementControlIds itemHs			  


--	Id occurrence checks on [WElementHandle ls ps] and [WElementHandle`].

--	There are no duplicate (ControlId id) attributes:


noDuplicateControlIds :: [WElementHandle ls ps] -> Bool
noDuplicateControlIds itemHs = noDuplicates (getWElementControlIds itemHs)
	  

--	The list of Ids does not occur in any (ControlId id) attribute:

disjointControlIds :: [Id] -> [WElementHandle ls ps] -> Bool
disjointControlIds ids itemHs = disjointLists ids (getWElementControlIds itemHs)


{-	controlIdsAreConsistent checks whether the WElementHandles contain (R(2))Ids that have already been
	associated with open receivers or other I/O objects and if there are no duplicate Ids. 
	The ReceiverTable is not changed if there are duplicate (R(2))Ids; otherwise all (R(2))Ids have been bound.
-}

controlIdsAreConsistent :: SystemId -> Id -> [WElementHandle ls ps] -> IdTable -> Maybe IdTable
controlIdsAreConsistent ioId wId itemHs it
	| not (okMembersIdTable ids it) = Nothing	
	| otherwise = Just it1
	where
		ids 	      = getWElementControlIds itemHs
		idParent      = IdParent {idpIOId=ioId,idpDevice=WindowDevice,idpId=wId}
		it1           = foldr (\id tbl -> Map.insert id idParent tbl) it ids
