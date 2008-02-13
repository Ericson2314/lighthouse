-----------------------------------------------------------------------------
-- |
-- Module      :  StdWindowDef
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdWindowDef contains the types to define the standard set of dialogs.
--
-----------------------------------------------------------------------------


module Graphics.UI.ObjectIO.StdWindowDef
		(
		-- | Data type definitions
		  module Graphics.UI.ObjectIO.StdWindowDef
		, Look, PenAttribute(..),
		-- A visible modules
		  module Graphics.UI.ObjectIO.StdIOCommon
		, module Graphics.UI.ObjectIO.StdGUI		
		) where


import Graphics.UI.ObjectIO.StdGUI
import Graphics.UI.ObjectIO.StdIOCommon
import Graphics.UI.ObjectIO.StdPicture(PenAttribute(..), Look)

-- | The dialogs are nonresizable modal or nonmodal windows. They adjust their size to the common size
-- of the contained controls. They usually have two special buttons called \"Ok\" and
-- \"Cancel\". When the user presses Enter or Esc keys, the dialog interprets this event as
-- clicking on \"Ok\" or \"Cancel\".
data	Dialog c ls ps = Dialog Title (c ls ps) [WindowAttribute ls ps]

-- | The windows are resizable and one can draw in the view domain.This can be programmed as a Haskell\'s function.
-- They also can have vertical and horizontal scroll bars, which extend logical view frame of the windows.
data	Window c ls ps = Window Title (c ls ps) [WindowAttribute ls ps]

data	WindowAttribute ls ps                        -- Default:
 --	Attributes for Windows and Dialogs:
 =	WindowActivate   (GUIFun ls ps)              -- id
 |	WindowClose      (GUIFun ls ps)              -- user can't close window
 |	WindowDeactivate (GUIFun ls ps)              -- id
 |	WindowHMargin	 Int Int		     -- system dependent
 |	WindowId         Id                          -- system defined id
 |	WindowIndex	 Int			     -- open front-most
 |	WindowInit       (GUIFun ls ps)              -- no actions after opening window
 |	WindowInitActive Id			     -- system dependent
 |	WindowItemSpace	 Int Int		     -- system dependent
 |	WindowOuterSize	 Size			     -- screen size
 |	WindowPos	 ItemPos		     -- system dependent
 |	WindowViewSize   Size                        -- screen size
 |	WindowVMargin	 Int Int		     -- system dependent
 --	Attributes for Dialog only:	
 |	WindowCancel	 Id			     -- no cancel  (Custom)ButtonControl
 |	WindowOk	 Id			     -- no default (Custom)ButtonControl
 --	Attributes for Windows only:	
 | 	WindowCursor	 CursorShape		     -- no change of cursor
 |	WindowHScroll	 ScrollFunction		     -- no horizontal scrolling
 |	WindowKeyboard	 KeyboardStateFilter SelectState (KeyboardFunction ls ps) -- no keyboard input
 |	WindowLook	 Bool Look		     -- show system dependent background
 |	WindowMouse	 MouseStateFilter SelectState (MouseFunction ls ps) -- no mouse input
 |	WindowOrigin	 Point2			     -- left top of picture domain
 |	WindowPen	 [PenAttribute]		     -- default pen attributes
 |	WindowSelectState	SelectState	     -- Able
 |	WindowViewDomain	ViewDomain	     -- {zero,max range}
 |	WindowVScroll		ScrollFunction	     -- no vertical scrolling
 |	WindowCaret	Point2 Size		     -- no caret
 |	WindowResize    (WindowResizeFunction ls ps) -- no function
 |	WindowDoubleBuffered
 
 
type WindowResizeFunction ls ps = Size -> Size -> GUIFun ls ps