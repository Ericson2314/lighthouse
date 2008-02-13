-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Receiver.Handle
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--	
-- Receiver.Handle contains the internal data structures that represent the 
-- state of receivers. 
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Receiver.Handle where


import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.StdReceiverDef


data	ReceiverHandles ps
 =	ReceiverHandles
 		{ rReceivers :: [ReceiverStateHandle ps]
		}
data	ReceiverStateHandle ps
 =	forall ls .
	ReceiverStateHandle 
		ls					-- The local state of the receiver
		(ReceiverHandle ls ps)			-- The receiver handle

data	ReceiverHandle ls ps
 =	ReceiverHandle
		{ rId           :: Id			-- The id of the receiver
		, rSelect       :: SelectState		-- The current SelectState of the receiver
		, rFun          :: GUIFun ls ps
		}


receiverIdentified :: Id -> ReceiverHandle ls ps -> Bool
receiverIdentified id rH = id==rId rH

receiverSetSelectState :: SelectState -> ReceiverStateHandle ps -> ReceiverStateHandle ps
receiverSetSelectState select rsH@(ReceiverStateHandle ls rH) = ReceiverStateHandle ls (rH {rSelect=select})
