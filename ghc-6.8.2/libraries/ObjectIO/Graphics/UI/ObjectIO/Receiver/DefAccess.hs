-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Receiver.DefAccess
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--	
-- Access functions to ReceiverDefinitions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Receiver.DefAccess
		( receiverDefAttributes		, receiver2DefAttributes
		, receiverDefRId		, receiver2DefR2Id
		, receiverDefSelectState	, receiver2DefSelectState
		, receiverDefFunction		, receiver2DefFunction
		, receiverDefSetAbility		, receiver2DefSetAbility
		, receiverDefSetFunction	, receiver2DefSetFunction
		, module Graphics.UI.ObjectIO.StdReceiverDef
		) where



import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.StdReceiverAttribute
import Graphics.UI.ObjectIO.StdReceiverDef


-- Receiver

receiverDefAttributes :: Receiver m ls ps -> [ReceiverAttribute ls ps]
receiverDefAttributes (Receiver _ _ atts) = atts

receiverDefRId :: Receiver m ls ps -> RId m
receiverDefRId (Receiver rid _ _) = rid

receiverDefSelectState :: Receiver m ls ps -> SelectState
receiverDefSelectState (Receiver _ _ atts) = getSelectState atts

receiverDefFunction :: Receiver m ls ps -> ReceiverFunction m ls ps
receiverDefFunction (Receiver _ f _) = f

receiverDefSetAbility :: SelectState -> Receiver m ls ps -> Receiver m ls ps
receiverDefSetAbility ability (Receiver rid f atts) = Receiver rid f (setSelectState ability atts)
	
receiverDefSetFunction :: ReceiverFunction m ls ps -> Receiver m ls ps -> Receiver m ls ps
receiverDefSetFunction f (Receiver rid _ atts) = Receiver rid f atts


-- Receiver2

receiver2DefAttributes :: Receiver2 m r ls ps -> [ReceiverAttribute ls ps]
receiver2DefAttributes (Receiver2 _ _ atts) = atts

receiver2DefR2Id :: Receiver2 m r ls ps -> R2Id m r
receiver2DefR2Id (Receiver2 rid _ _) = rid

receiver2DefSelectState :: Receiver2 m r ls ps -> SelectState
receiver2DefSelectState (Receiver2 _ _ atts) = getSelectState atts

receiver2DefFunction :: Receiver2 m r ls ps -> Receiver2Function m r ls ps
receiver2DefFunction (Receiver2 _ f _) = f

receiver2DefSetAbility :: SelectState -> Receiver2 m r ls ps -> Receiver2 m r ls ps
receiver2DefSetAbility ability (Receiver2 rid f atts) = Receiver2 rid f (setSelectState ability atts)

receiver2DefSetFunction :: Receiver2Function m r ls ps -> Receiver2 m r ls ps -> Receiver2 m r ls ps
receiver2DefSetFunction f (Receiver2 rid _ atts) = Receiver2 rid f atts



setSelectState :: SelectState -> [ReceiverAttribute ls ps] -> [ReceiverAttribute ls ps]
setSelectState ability atts
	= snd (creplace isReceiverSelectState (ReceiverSelectState ability) atts)

getSelectState :: [ReceiverAttribute ls ps] -> SelectState
getSelectState atts
	= getReceiverSelectStateAtt selectAtt
	where
		(_,selectAtt) = cselect isReceiverSelectState defSelect atts
		defSelect     = ReceiverSelectState Able