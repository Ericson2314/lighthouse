-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Timer.Access
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--	
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Timer.Access where


import  Graphics.UI.ObjectIO.Id
import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.Device.SystemState
import	Graphics.UI.ObjectIO.Timer.Handle
import	Graphics.UI.ObjectIO.Timer.Table
import  Graphics.UI.ObjectIO.Receiver.Handle
import  Data.Map as Map


timerAccessFatalError :: String -> String -> x
timerAccessFatalError function error = dumpFatalError function "TimerAccess" error


{-	bindTimerElementIds binds all unbound R(2)Ids and Ids that can be located in the list of TimerElementStates.
	The Boolean result is True only if no bound identification was found, otherwise it is False.
-}
bindTimerElementIds :: SystemId -> Id -> [TimerElementHandle ls ps] -> IdTable -> Maybe IdTable
bindTimerElementIds pid timerid (itemH:itemHs) it =
	bindTimerElementIds' pid timerid itemH it >>= bindTimerElementIds pid timerid itemHs
	where
		bindTimerElementIds' :: SystemId -> Id -> TimerElementHandle ls ps -> IdTable -> Maybe IdTable
		bindTimerElementIds' pid timerid itemH@(TimerReceiverHandle trH _) it
			| Map.member rid it = Nothing
			| otherwise = Just (insert rid (IdParent{idpIOId=pid,idpDevice=TimerDevice,idpId=timerid}) it)
			where
				rid	= rId trH

		bindTimerElementIds' pid timerid (TimerListLSHandle itemHs) it =
			bindTimerElementIds pid timerid itemHs it

		bindTimerElementIds' pid timerid (TimerExtendLSHandle exLS itemHs) it =
			bindTimerElementIds pid timerid itemHs it

		bindTimerElementIds' pid timerid (TimerChangeLSHandle chLS itemHs) it =
			bindTimerElementIds pid timerid itemHs it
		
bindTimerElementIds _ _ itemHs it = Just it


{-	unbindTimerElementIds unbinds all bound R(2)Ids and Ids that can be located in the list of TimerElementStates. -}

unbindTimerElementIds :: SystemId -> [TimerElementHandle ls ps] -> (TimerTable,IdTable) -> (TimerTable,IdTable)
unbindTimerElementIds pid itemHs tables
	= foldr unbindTimerElementIds' tables itemHs
	where
		unbindTimerElementIds' :: TimerElementHandle ls ps -> (TimerTable,IdTable) -> (TimerTable,IdTable)
		unbindTimerElementIds' (TimerReceiverHandle (ReceiverHandle {rId=rId}) _) (tt,it) =
			(removeTimerFromTimerTable teLoc tt,Map.delete rId it)
			where
				teLoc	= TimerLoc{tlIOId=pid,tlDevice=TimerDevice,tlParentId=rId,tlTimerId=rId}
		unbindTimerElementIds' (TimerListLSHandle itemHs) tables =
			foldr unbindTimerElementIds' tables itemHs
		unbindTimerElementIds' (TimerExtendLSHandle exLS itemHs) tables =
			foldr unbindTimerElementIds' tables itemHs
		unbindTimerElementIds' (TimerChangeLSHandle chLS itemHs) tables =
			foldr unbindTimerElementIds' tables itemHs

identifyTimerStateHandle :: Id -> TimerStateHandle ps -> Bool
identifyTimerStateHandle id (TimerStateHandle (TimerLSHandle {tHandle=tH})) = id == tId tH
