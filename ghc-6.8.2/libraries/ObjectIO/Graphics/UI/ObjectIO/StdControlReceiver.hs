-----------------------------------------------------------------------------
-- |
-- Module      :  StdControlReceiver
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdControlReceiver defines Receiver(2) instances of 'Controls' class.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdControlReceiver where



import	Graphics.UI.ObjectIO.StdControlClass
import	Graphics.UI.ObjectIO.StdReceiverAttribute
import	Graphics.UI.ObjectIO.Window.Handle
import	Graphics.UI.ObjectIO.CommonDef(cselect)
import	Graphics.UI.ObjectIO.Receiver.Access(newReceiverHandle,newReceiver2Handle)
import	Graphics.UI.ObjectIO.OS.Types(osNoWindowPtr)


instance Controls (Receiver m) where	
	controlToHandles (Receiver rid f atts) =
		return [wElementHandleToControlState
				(WItemHandle 
					{ wItemId		= Just (rIdtoId rid)
					, wItemNr		= 0
					, wItemKind		= IsReceiverControl
					, wItemShow		= False
					, wItemSelect		= enabled select
					, wItemInfo		= WReceiverInfo (newReceiverHandle rid select f)
					, wItemAtts		= []
					, wItems		= []
					, wItemVirtual		= True
					, wItemPos		= zero
					, wItemSize		= zero
					, wItemPtr		= osNoWindowPtr
					, wItemLayoutInfo	= LayoutFix
					})
			]		  
		where		
			select	= getReceiverSelectStateAtt (snd (cselect isReceiverSelectState (ReceiverSelectState Able) atts))


instance Controls (Receiver2 m r) where	
	controlToHandles (Receiver2 rid f atts) =
		return [wElementHandleToControlState
				(WItemHandle 
					{ wItemId		= Just (r2IdtoId rid)
					, wItemNr		= 0
					, wItemKind		= IsReceiverControl
					, wItemShow		= False
					, wItemSelect		= enabled select
					, wItemInfo		= WReceiverInfo (newReceiver2Handle rid select f)
					, wItemAtts		= []
					, wItems		= []
					, wItemVirtual		= True
					, wItemPos		= zero
					, wItemSize		= zero
					, wItemPtr		= osNoWindowPtr
					, wItemLayoutInfo	= LayoutFix
					})
			]		  
		where		
			select	= getReceiverSelectStateAtt (snd (cselect isReceiverSelectState (ReceiverSelectState Able) atts))