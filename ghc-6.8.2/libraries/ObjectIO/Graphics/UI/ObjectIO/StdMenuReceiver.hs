-----------------------------------------------------------------------------
-- |
-- Module      :  StdMenuReceiver
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdMenuReceiver defines Receiver(2) instances of 'MenuElements' class.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdMenuReceiver(MenuElements(..)) where


import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.StdReceiverAttribute
import	Graphics.UI.ObjectIO.StdMenuElementClass
import	Graphics.UI.ObjectIO.StdMenuDef
import	Graphics.UI.ObjectIO.Menu.Handle
import	Graphics.UI.ObjectIO.Receiver.Access(newReceiverHandle, newReceiver2Handle)


instance MenuElements (Receiver m) where
	menuElementToHandles (Receiver rid f atts) =
		return [menuElementHandleToMenuElementState
				(MenuReceiverHandle (newReceiverHandle rid (getSelectState atts) f)
						(MenuId (rIdtoId rid):map receiverAttToMenuAtt atts))
			]

instance MenuElements (Receiver2 m r) where
	menuElementToHandles (Receiver2 rid f atts) =
		return [menuElementHandleToMenuElementState
				(MenuReceiverHandle (newReceiver2Handle rid (getSelectState atts) f)
						(MenuId (r2IdtoId rid):map receiverAttToMenuAtt atts))
			]

getSelectState :: [ReceiverAttribute ls ps] -> SelectState
getSelectState rAtts = getReceiverSelectStateAtt (snd (cselect isReceiverSelectState (ReceiverSelectState Able) rAtts))

receiverAttToMenuAtt :: ReceiverAttribute ls ps -> MenuAttribute ls ps
receiverAttToMenuAtt (ReceiverSelectState s) = MenuSelectState s
