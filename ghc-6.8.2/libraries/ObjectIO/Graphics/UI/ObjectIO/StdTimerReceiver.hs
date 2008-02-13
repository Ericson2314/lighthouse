-----------------------------------------------------------------------------
-- |
-- Module      :  StdTimerReceiver
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdTimerReceiver defines Receiver(2) instances of 'TimerElements' class.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdTimerReceiver where


import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.Id
import	Graphics.UI.ObjectIO.Receiver.Access
import	Graphics.UI.ObjectIO.Receiver.DefAccess
import	Graphics.UI.ObjectIO.StdReceiverAttribute
import	Graphics.UI.ObjectIO.StdTimerDef
import	Graphics.UI.ObjectIO.StdTimerElementClass
import	Graphics.UI.ObjectIO.Timer.Handle


instance TimerElements (Receiver m) where	
	timerElementToHandles (Receiver rid f atts) =
		return [timerElementHandleToTimerElementState
				(TimerReceiverHandle (newReceiverHandle rid (getSelectState atts) f)
						     (TimerId (rIdtoId rid):map receiverAttToTimerAtt atts))
			]
			
instance TimerElements (Receiver2 m r) where	
	timerElementToHandles (Receiver2 rid f atts) =
		return [timerElementHandleToTimerElementState
				(TimerReceiverHandle (newReceiver2Handle rid (getSelectState atts) f)
						     (TimerId (r2IdtoId rid):map receiverAttToTimerAtt atts))
			]


receiverAttToTimerAtt :: ReceiverAttribute ls ps -> TimerAttribute ls ps
receiverAttToTimerAtt (ReceiverSelectState s) = TimerSelectState s

getSelectState :: [ReceiverAttribute ls ps] -> SelectState
getSelectState rAtts = getReceiverSelectStateAtt (snd (cselect isReceiverSelectState (ReceiverSelectState Able) rAtts))
