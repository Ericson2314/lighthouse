-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Device.Events
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Device.SystemState defines the device administration types and unwrapper 
-- functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Device.SystemState
		( module Graphics.UI.ObjectIO.Device.SystemState
		, module Graphics.UI.ObjectIO.Device.Types
		) where


import Graphics.UI.ObjectIO.Device.Types
import Graphics.UI.ObjectIO.Receiver.Handle
import Graphics.UI.ObjectIO.Window.Handle
import Graphics.UI.ObjectIO.Timer.Handle
import Graphics.UI.ObjectIO.Menu.Handle


data	DeviceSystemState ps
	= MenuSystemState	(MenuHandles		ps)
	| ReceiverSystemState   (ReceiverHandles 	ps)
	| TimerSystemState	(TimerHandles		ps)
	| WindowSystemState     (WindowHandles 		ps)

toDevice :: DeviceSystemState ps -> Device
toDevice (ReceiverSystemState _) = ReceiverDevice
toDevice (WindowSystemState   _) = WindowDevice
toDevice (TimerSystemState   _)  = TimerDevice
toDevice (MenuSystemState     _) = MenuDevice


receiverSystemStateGetReceiverHandles :: DeviceSystemState ps -> ReceiverHandles ps
receiverSystemStateGetReceiverHandles (ReceiverSystemState rsHs) = rsHs 

windowSystemStateGetWindowHandles :: DeviceSystemState ps -> WindowHandles ps
windowSystemStateGetWindowHandles (WindowSystemState wsHs) = wsHs

timerSystemStateGetTimerHandles :: DeviceSystemState ps -> TimerHandles ps
timerSystemStateGetTimerHandles (TimerSystemState tsHs) = tsHs

menuSystemStateGetMenuHandles :: DeviceSystemState ps -> MenuHandles ps
menuSystemStateGetMenuHandles (MenuSystemState msHs) = msHs

