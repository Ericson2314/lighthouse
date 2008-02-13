-----------------------------------------------------------------------------
-- |
-- Module      :  StdReceiverDef
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdReceiverDef contains the types to define the standard set of receivers.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdReceiverDef 
		( 
		-- * Definitions
		  module Graphics.UI.ObjectIO.StdReceiverDef,
		  
		-- * A visible modules
		  module Graphics.UI.ObjectIO.StdIOCommon
		, module Graphics.UI.ObjectIO.StdGUI
		) where


import Graphics.UI.ObjectIO.StdGUI
import Graphics.UI.ObjectIO.StdIOCommon


-- | Uni-directional receiver. The uni-directional receiver can receive
-- messages but cann\'t respond to it.
data	Receiver         m ls ps = Receiver (RId m) (ReceiverFunction m ls ps) [ReceiverAttribute ls ps]
type	ReceiverFunction m ls ps = m -> GUIFun ls ps

-- | Bi-directional can receive messages and then must respond to it.
data	Receiver2         m r ls ps = Receiver2 (R2Id m r) (Receiver2Function m r ls ps) [ReceiverAttribute ls ps]
type	Receiver2Function m r ls ps = m -> (ls,ps) -> GUI ps (r,(ls,ps))

data	ReceiverAttribute ls ps				-- Default:
 =	ReceiverInit               (GUIFun ls ps)	-- no actions after opening receiver
 |	ReceiverSelectState        SelectState		-- receiver Able
