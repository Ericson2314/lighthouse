-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Device.Types
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Device defines the set of devices and their priority.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Device.Types where


data	Device				-- The set of devices
	= TimerDevice
	| MenuDevice
	| WindowDevice
	| ReceiverDevice
	| ProcessDevice
	deriving (Eq,Show)

priorityDevice :: Device -> Int
priorityDevice ReceiverDevice = 6
priorityDevice TimerDevice    = 4
priorityDevice MenuDevice     = 3
priorityDevice WindowDevice   = 2
priorityDevice ProcessDevice  = 0

devices = [ ReceiverDevice
	  , TimerDevice
	  , MenuDevice
	  , WindowDevice
	  , ProcessDevice
	  ]
