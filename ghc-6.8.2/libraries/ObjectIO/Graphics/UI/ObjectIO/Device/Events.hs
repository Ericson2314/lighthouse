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
-- Device.Events contains all type definitions for OS independent events.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Device.Events where



import Graphics.UI.ObjectIO.StdIOCommon
import Graphics.UI.ObjectIO.Window.Handle(WIDS(..))
import Graphics.UI.ObjectIO.Timer.Table(TimerEvent(..))
import Graphics.UI.ObjectIO.OS.Event
import Graphics.UI.ObjectIO.OS.Types(Rect, OSWindowPtr, OSPictContext)

data	DeviceEvent
 --	Menu events:
 =	MenuTraceEvent		!MenuTraceInfo			-- Menu item has been selected
 |	ToolbarSelection	!Int				-- Toolbar item has been selected (consist item nr)
 --	Receiver events:
 |	ReceiverEvent           !Id				-- A (bi/uni)directional (a)synchronous message event
 --	Timer events:
 |	TimerDeviceEvent	!TimerEvent			-- A timer event
 --	Window/Dialog events:
 |	CompoundScrollAction	!CompoundScrollActionInfo	-- Scrolling should occur in a compound control
 |	ControlGetKeyFocus	!ControlKeyFocusInfo		-- Control has obtained keyboard focus	
 |	ControlLooseKeyFocus	!ControlKeyFocusInfo		-- Control has lost keyboard focus
 |	ControlMouseAction	!ControlMouseActionInfo		-- Mouse action in a control	
 |	ControlSliderAction	!ControlSliderInfo		-- Slider control has been selected	
 |	WindowCANCEL		!WIDS				-- The Cancel button has been pressed
 |	WindowKeyboardAction	!WindowKeyboardActionInfo	-- Keyboard action in a window
 |	WindowMouseAction	!WindowMouseActionInfo		-- Mouse action in a window
 |	WindowOK		!WIDS				-- The Ok button has been pressed	
 |	WindowScrollAction	!WindowScrollActionInfo		-- Scrolling should occur in a window	
 |	WindowUpdate		!UpdateInfo			-- Window and its controls should be updated	
 |	ControlKeyboardAction   !ControlKeyboardActionInfo	-- Keyboard action in a control
 |	ControlSelection        !ControlSelectInfo		-- Control has been selected
 |	WindowActivation        !WIDS				-- Window with id has been made activate
 |	WindowCreateControls    !WIDS				-- Window with id can create its controls
 |	WindowDeactivation      !WIDS				-- Window with id has been made inactivate
 |	WindowInitialise        !WIDS				-- Window with id can evaluate its initialisation action
 |	WindowRequestClose      !WIDS				-- Window with id should be closed
 |	WindowSizeAction        !WindowSizeActionInfo		-- Window has obtained a new size
 --	Process events:
 |	ProcessInitialise					-- The initial event that allows process initialisation
 |	ProcessRequestClose					-- The process should be closed
 |	ProcessRequestOpenFiles	![String]			-- The process should open files
 
data 	MenuTraceInfo
 =	MenuTraceInfo
 		{ mtId		  :: !Id			-- The Id of the menu that contains the menu item
	  	, mtParents	  :: ![Int]			-- The submenus starting from mtId that contain the menu item (zero based index)
		, mtItemNr	  :: !Int			-- The menu item that has been selected (zero based index)
		, mtModifiers	  :: !Modifiers			-- The modifiers that were pressed at the moment of selection
		}
data	UpdateInfo
 =	UpdateInfo
		{ updWIDS         :: !WIDS			-- The Id of the window/dialogue to be updated
		, updWindowArea   :: !Rect			-- The area of the window/dialogue to be updated (case zero, no update)
		, updControls     :: ![ControlUpdateInfo]	-- For each control to be updated: its item nr and area (in window coordinates)
		, updGContext     :: !(Maybe OSPictContext)	-- The graphics context to be used
		}
data	ControlUpdateInfo
 =	ControlUpdateInfo
		{ cuItemNr        :: !Int			-- The wItemNr of the control
		, cuItemPtr       :: !OSWindowPtr		-- The wItemPtr to the control (can be OSNoWindowPtr)
		, cuArea          :: !Rect			-- The update area of the control (in window coordinates)
		} 
data    CompoundScrollActionInfo
 =	CompoundScrollActionInfo
 		{ csaWIDS	  :: !WIDS			-- The Id/Ptr of the window/dialogue that contains the compound control
		, csaItemNr	  :: !Int			-- The wItemNr  of the compound control
		, csaItemPtr	  :: !OSWindowPtr		-- The wItemPtr of the compound control
		, csaSliderMove	  :: !SliderMove		-- The user action on the compound control
		, csaDirection	  :: !Direction			-- The direction of the scrollbar that is being selected
		}
data	ControlKeyFocusInfo
 =	ControlKeyFocusInfo
 		{ ckfWIDS	  :: !WIDS			-- The Id/Ptr of the window/dialogue that contains the control
		, ckfItemNr	  :: !Int			-- The wItemNr  of the control
		, ckfItemPtr	  :: !OSWindowPtr		-- The wItemPtr of the control
		}
data	ControlKeyboardActionInfo
 =	ControlKeyboardActionInfo
 		{ ckWIDS	  :: !WIDS			-- The Id/Ptr of the window/dialogue that contains the control
		, ckItemNr	  :: !Int			-- The wItemNr  of the control
		, ckItemPtr	  :: !OSWindowPtr		-- The wItemPtr of the control
		, ckKeyboardState :: !KeyboardState		-- The KeyboardState of the action
		}
data	ControlMouseActionInfo
 =	ControlMouseActionInfo
 		{ cmWIDS	  :: !WIDS			-- The Id/Ptr of the window/dialogue that contains the control
		, cmItemNr        :: !Int			-- The wItemNr  of the control
		, cmItemPtr       :: !OSWindowPtr		-- The wItemPtr of the control
		, cmMouseState    :: !MouseState		-- The MouseState of the action
		}
data	ControlSelectInfo
 =	ControlSelectInfo
 		{ csWIDS	  :: !WIDS			-- The Id/Ptr of the window/dialogue that contains the control
		, csItemNr	  :: !Int			-- The wItemNr  of the selected control
		, csItemPtr	  :: !OSWindowPtr		-- The wItemPtr of the selected control
		, csMoreData	  :: !Int			-- Additional data (index in case of PopUpControls; otherwise zero)
		, csModifiers	  :: !Modifiers			-- The modifiers that were active when the control was selected
		}
data	ControlSliderInfo
 =	ControlSliderInfo
 		{ cslWIDS	  :: !WIDS			-- The Id/Ptr of the window/dialogue that contains the slider
		, cslItemNr	  :: !Int			-- The wItemNr  of the selected slider
		, cslItemPtr	  :: !OSWindowPtr		-- The wItemPtr of the selected slider
		, cslSliderMove	  :: !SliderMove		-- The user action on the slider
		}
data	WindowKeyboardActionInfo
 =	WindowKeyboardActionInfo
 		{ wkWIDS	  :: !WIDS			-- The Id/Ptr of the window
		, wkKeyboardState :: !KeyboardState		-- The KeyboardState of the action
		}
data	WindowMouseActionInfo
 =	WindowMouseActionInfo
 		{ wmWIDS	  :: !WIDS			-- The Id/Ptr of the window
		, wmMouseState	  :: !MouseState		-- The MouseState of the action
		}
data	WindowScrollActionInfo
 =	WindowScrollActionInfo
 		{ wsaWIDS	  :: !WIDS			-- The Id/Ptr of the window
		, wsaSliderMove	  :: !SliderMove		-- The user action on the window
		, wsaDirection	  :: !Direction			-- The direction of the scrollbar that is being selected
		}
data	WindowSizeActionInfo
 =	WindowSizeActionInfo
 		{ wsWIDS	  :: !WIDS			-- The Id/Ptr of the window
		, wsSize	  :: !Size			-- The new size of the window (including scrollbars)
		, wsUpdateAll	  :: !Bool			-- The complete content of the window must be redrawn
		}