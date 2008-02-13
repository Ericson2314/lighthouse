-----------------------------------------------------------------------------
-- |
-- Module      :  StdControlDef
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdControlDef contains the types to define the standard set of controls.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdControlDef
		( module Graphics.UI.ObjectIO.StdControlDef
		, module Graphics.UI.ObjectIO.StdIOCommon
		, module Graphics.UI.ObjectIO.StdGUI
		) where


import Graphics.UI.ObjectIO.StdGUI
import Graphics.UI.ObjectIO.StdIOCommon
import Graphics.UI.ObjectIO.StdPicture(PenAttribute(..), Look)


data	ButtonControl ls ps
 =	ButtonControl String                      				[ControlAttribute ls ps]

data	CheckControl  ls ps
 = 	CheckControl  [CheckControlItem ps (ls,ps)]  RowsOrColumns       	[ControlAttribute ls ps]

-- | The compound control is a control that contains other controls. It introduces a new
-- layout scope like 'LayoutControl' but it provides programmers with a lot more functionality.
-- Just like the windows,  the compound controls have a view domain and can have its own Look 
-- function. If we add 'ControlHScroll' or 'ControlVScroll' attribute then the control will be decorated with scroll bars.
data	CompoundControl c ls ps
 = 	CompoundControl (c ls ps)                                		[ControlAttribute ls ps]

-- | CustomButtonControl is like the 'ButtonControl' but has its own 'Look' and 
-- doesn\'t accept the 'ControlTitle' attribute.
data	CustomButtonControl ls ps
 = 	CustomButtonControl Size Look                                        	[ControlAttribute ls ps]

-- | CustomControl allows the programmer to design his\/her own controls.
data	CustomControl       ls ps
 = 	CustomControl       Size Look                                        	[ControlAttribute ls ps]

-- | An edit control is a rectangular box in which the user can enter text.
data	EditControl   ls ps
 =	EditControl   String ControlWidth NrLines 				[ControlAttribute ls ps]

-- | The layout control is a control that contains other controls. It introduces a new layout
-- scope: i.e. the controls inside it are positioned in relation to the bounds of the layout control.
data	LayoutControl     c ls ps
 = 	LayoutControl     (c ls ps)                                  		[ControlAttribute ls ps]

-- | A popup control consists of a list box combined with simple text control.
-- The list-box portion of the control drop down when the user selects the drop-down arrow next to the control.
data	PopUpControl 	ls ps
 = 	PopUpControl   	[PopUpControlItem ps (ls,ps)] Index 			[ControlAttribute ls ps]

-- | The control is a rectangle containing a list of strings from which the user can select.
data	ListBoxControl 	ls ps
 = 	ListBoxControl  [ListBoxControlItem ps (ls,ps)] NrLines Bool		[ControlAttribute ls ps]

data	RadioControl   	ls ps
 = 	RadioControl    [RadioControlItem ps (ls,ps)] RowsOrColumns Index 	[ControlAttribute ls ps]

data	SliderControl  	ls ps
 = 	SliderControl   Direction ControlWidth SliderState  (SliderAction  ls ps) [ControlAttribute ls ps]

-- | This is a simple control that just displays its caption.
data	TextControl   ls ps
 =	TextControl   String                      				[ControlAttribute ls ps]


type	CheckControlItem   ps st = (String, Maybe ControlWidth, MarkState, st -> GUI ps st)
type	PopUpControlItem   ps st = (String,                                st -> GUI ps st)
type	ListBoxControlItem ps st = (String, MarkState,                     st -> GUI ps st)
type	RadioControlItem   ps st = (String, Maybe ControlWidth,            st -> GUI ps st)


type	NrLines
 =	Int
data	RowsOrColumns
	= Rows       Int
	| Columns    Int
data	ControlWidth                                -- The width of the control:
 =	PixelWidth   Int                            -- the exact number of pixels
 |	TextWidth    String                         -- the exact string width in dialog font
 |	ContentWidth String                         -- width of the control as if string is its content

data	ControlAttribute ls ps            	    -- Default:
 -- General control attributes:
 =	ControlActivate     (GUIFun ls ps)          -- return
 |	ControlDeactivate   (GUIFun ls ps)   	    -- return
 |	ControlFunction (GUIFun ls ps)              -- (\st->return st)
 |	ControlHide                                 -- initially visible
 |	ControlId       Id                          -- no id
 |	ControlKeyboard KeyboardStateFilter SelectState (KeyboardFunction ls ps)
	                                            -- no keyboard input/overruled
 |	ControlMinimumSize  Size                    -- zero
 |	ControlModsFunction (ModifiersFunction ls ps)
                                                    -- ControlFunction
 |	ControlMouse        MouseStateFilter    SelectState (MouseFunction ls ps)
		                                    -- no mouse input/overruled
 |	ControlPen	    [PenAttribute]	    -- default pen attributes
 |	ControlPos          ItemPos                 -- (RightTo previous,zero)
 |	ControlResize       ControlResizeFunction   -- no resize
 |	ControlSelectState  SelectState             -- control Able
 |	ControlTip          String                  -- no tip
 |	ControlWidth        ControlWidth            -- system derived
 --	For CompoundControls only:
 |	ControlHMargin      Int Int                 -- system dependent
 |	ControlHScroll      ScrollFunction          -- no horizontal scrolling
 |	ControlItemSpace    Int Int                 -- system dependent
 |	ControlLook         Bool Look               -- control is transparant
 |	ControlOrigin       Point2                  -- Left top of ViewDomain
 |	ControlOuterSize    Size		    -- enclose elements
 |	ControlViewDomain   ViewDomain              -- {zero,max range}
 |	ControlViewSize     Size                    -- enclose elements
 |	ControlVMargin      Int Int                 -- system dependent
 |	ControlVScroll      ScrollFunction          -- no vertical   scrolling
 |      ControlDoubleBuffered




type ControlResizeFunction =
	Size ->                                     		-- current control outer size
	Size ->                                     		-- old     parent  view  size
	Size ->                                     		-- new     parent  view  size
	Size                                        		-- new     control outer size



type ControlType = String