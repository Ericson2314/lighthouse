-----------------------------------------------------------------------------
-- |
-- Module      :  StdIOCommon
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdIOCommon defines common types and access functions for the I\/O library.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdIOCommon
		   ( module Graphics.UI.ObjectIO.StdIOCommon
                   , module Graphics.UI.ObjectIO.StdIOBasic
                   , module Graphics.UI.ObjectIO.StdKey
                   , module Data.Maybe
                   , Graphics.UI.ObjectIO.Id.Id
                   , Graphics.UI.ObjectIO.Id.R2Id
                   , Graphics.UI.ObjectIO.Id.RId
                   , Graphics.UI.ObjectIO.Id.rIdtoId
                   , Graphics.UI.ObjectIO.Id.r2IdtoId
                   ) where



import Graphics.UI.ObjectIO.Id (Id, RId, R2Id, rIdtoId, r2IdtoId)
import Graphics.UI.ObjectIO.StdIOBasic
import Graphics.UI.ObjectIO.StdKey
import Data.Maybe
import Data.Dynamic


{-	The SelectState and MarkState types.			-}

-- | 'SelectState' is similar to Bool but it shows whether an object is enabled or disabled.
data	SelectState
	= Able | Unable
	deriving (Eq,Show)
	
-- | 'MarkState' is similar to Bool but it shows whether the 'CheckControl' is marked.	
data	MarkState
	= Mark | NoMark
	deriving (Eq,Show)

enabled :: SelectState -> Bool					-- @1 == Able
enabled Able   = True
enabled unable = False

marked :: MarkState -> Bool					-- @1 == Able
marked Mark   = True
marked unable = False

instance Toggle SelectState where				-- Able <-> Unable
	toggle Able = Unable
	toggle _    = Able
	
	
instance Toggle MarkState where					-- Mark <-> NoMark
	toggle Mark = NoMark
	toggle _    = Mark


-- | 'KeyboardState' is passed to keyboard handler for every keyboard event.
data	KeyboardState
	= CharKey    Char       KeyState			-- ^ ASCII character input
	| SpecialKey SpecialKey KeyState Modifiers		-- ^ Special key input
	| KeyLost						-- ^ Key input lost while key was down
	deriving (Eq,Show)
	
-- | The KeyState type
data	KeyState
	= KeyDown    IsRepeatKey				-- ^ Key is down
	| KeyUp							-- ^ Key goes up
	deriving (Eq,Show)
	
-- | Flag on key down (True iff key is repeating)
type	IsRepeatKey
	= Bool
data	Key
	= IsCharKey    Char
	| IsSpecialKey SpecialKey
	
-- | Predicate on KeyboardState
type	KeyboardStateFilter = KeyboardState -> Bool

-- | getKeyboardStateKeyState gets KeyState from KeyboardState (KeyUp if KeyboardState is KeyLost)
getKeyboardStateKeyState:: KeyboardState -> KeyState
getKeyboardStateKeyState (CharKey _ keyState) 		= keyState
getKeyboardStateKeyState (SpecialKey _ keyState _)	= keyState
getKeyboardStateKeyState KeyLost			= KeyUp

-- | getKeyboardStateKey gets Key value from KeyboardState (Nothing if KeyboardState is KeyLost)
getKeyboardStateKey :: KeyboardState -> Maybe Key
getKeyboardStateKey (CharKey char _)		= Just (IsCharKey char)
getKeyboardStateKey (SpecialKey special _ _)	= Just (IsSpecialKey special)
getKeyboardStateKey KeyLost			= Nothing
	
	
-- | The MouseState type.
data	MouseState
	= MouseMove	Point2 Modifiers		-- ^ Mouse is up     (position,modifiers)
	| MouseDown	Point2 Modifiers Int		-- ^ Mouse goes down (and nr down)
	| MouseDrag	Point2 Modifiers		-- ^ Mouse is down   (position,modifiers)
	| MouseUp	Point2 Modifiers		-- ^ Mouse goes up   (position,modifiers)
	| MouseLost					-- ^ Mouse input lost while mouse was down
	deriving (Eq, Show)
	
-- | The ButtonState type.
data	ButtonState
 	= ButtonStillUp					-- ^ MouseMove
 	| ButtonDown					-- ^ MouseDown  _ _ 1
	| ButtonDoubleDown				-- ^		_ _ 2
	| ButtonTripleDown				-- ^            _ _ >2
	| ButtonStillDown				-- ^ MouseDrag
 	| ButtonUp					-- ^ MouseUp\/MouseLost
 	deriving (Eq, Show)
 	
-- | Predicate on MouseState
type	MouseStateFilter = MouseState -> Bool


getMouseStatePos :: MouseState -> Point2
getMouseStatePos (MouseMove pos _)	= pos
getMouseStatePos (MouseDown pos _ _)	= pos
getMouseStatePos (MouseDrag pos _)	= pos
getMouseStatePos (MouseUp   pos _)	= pos
getMouseStatePos MouseLost		= zero

getMouseStateModifiers :: MouseState -> Modifiers
getMouseStateModifiers (MouseMove _ mods)	= mods
getMouseStateModifiers (MouseDown _ mods _)	= mods
getMouseStateModifiers (MouseDrag _ mods)	= mods
getMouseStateModifiers (MouseUp   _ mods)	= mods
getMouseStateModifiers MouseLost		= noModifiers

getMouseStateButtonState:: MouseState	-> ButtonState
getMouseStateButtonState (MouseMove _ _)	= ButtonStillUp
getMouseStateButtonState (MouseDown _ _ nr)	= 
	case nr of
	  1 -> ButtonDown 
	  2 -> ButtonDoubleDown
	  _ -> ButtonTripleDown

getMouseStateButtonState (MouseDrag _ _)	= ButtonStillDown
getMouseStateButtonState (MouseUp   _ _)	= ButtonUp
getMouseStateButtonState MouseLost		= ButtonUp


{-	The SliderState type.					-}

data	SliderState 
	= SliderState
		{ sliderMin	:: !Int
		, sliderMax	:: !Int
		, sliderThumb	:: !Int
		}
	deriving (Eq, Show)
		
		
{-	The UpdateState type.					-}
data	UpdateState
	= UpdateState
		{ oldFrame	:: !ViewFrame
		, newFrame	:: !ViewFrame
		, updArea	:: !UpdateArea
		}
	deriving (Show)
	
-- | ViewDomain is the 'Rectangle', which specifies the logical drawing area of the CompoundControl or Window.
type	ViewDomain = Rectangle

-- | ViewFrame is the current visible 'Rectangle' of CompoundControl or Window.
-- When there are horizontal and vertical scroll bars then the changing of 
-- the scroller thumb will change the 'ViewFrame'.
type	ViewFrame  = Rectangle

type	UpdateArea = [ViewFrame]

rectangleToUpdateState :: Rectangle -> UpdateState
rectangleToUpdateState frame
	= UpdateState {oldFrame=frame,newFrame=frame,updArea=[frame]}


-- | viewDomainRange defines the minimum and maximum values for ViewDomains
viewDomainRange :: ViewDomain
viewDomainRange
	= Rectangle
		{ corner1 = Point2 {x = -1073741824,y = -1073741824}
		, corner2 = Point2 {x =  1073741824,y =  1073741824}
		}

-- | viewFrameRange defines the minimum and maximum values for ViewFrames.
viewFrameRange :: ViewFrame
viewFrameRange
	= Rectangle
		{ corner1 = Point2 {x = 2147483647,y = 2147483647}
		, corner2 = Point2 {x = 2147483647,y = 2147483647}
		}


-- | Modifiers indicates the meta keys that have been pressed (True) or not (False).
data	Modifiers
	= Modifiers
		{ shiftDown	:: !Bool			-- ^ True iff shift   down
		, optionDown	:: !Bool			-- ^ True iff option  down
		, commandDown	:: !Bool			-- ^ True iff command down
		, controlDown	:: !Bool			-- ^ True iff control down
		, altDown	:: !Bool			-- ^ True iff alt     down
		}
	deriving (Eq,Show)


--	Constants to check which of the Modifiers are down.

noModifiers = Modifiers {shiftDown = False, optionDown = False, commandDown = False, controlDown = False, altDown = False}
shiftOnly   = Modifiers {shiftDown = True,  optionDown = False, commandDown = False, controlDown = False, altDown = False}
optionOnly  = Modifiers {shiftDown = False, optionDown = True,  commandDown = False, controlDown = False, altDown = True }
commandOnly = Modifiers {shiftDown = False, optionDown = False, commandDown = True,  controlDown = True,  altDown = False}
controlOnly = Modifiers {shiftDown = False, optionDown = False, commandDown = True,  controlDown = True,  altDown = False}
altOnly     = Modifiers {shiftDown = False, optionDown = True,  commandDown = False, controlDown = False, altDown = True }


-- | 	The layout language used for windows and controls.	-}
type	ItemPos
	=	( ItemLoc
		, ItemOffset
		)
data	ItemLoc
 --	Absolute:
	= Fix
 --	Relative to corner:
	| LeftTop
	| RightTop
	| LeftBottom
	| RightBottom
 --	Relative in next line:
	| Left
	| Center
	| Right
 --	Relative to other item:
	| LeftOf  Id
	| RightTo Id
	| Above   Id
	| Below   Id
 --	Relative to previous item:
	| LeftOfPrev
	| RightToPrev
	| AbovePrev
	| BelowPrev
	deriving (Eq,Show)
type	ItemOffset
	= Vector2						-- A constant offset vector
	
	
{-	The Direction type.					-}

data	Direction
	= Horizontal
	| Vertical
	deriving Eq

{-	The CursorShape type.					-}

data	CursorShape
	= StandardCursor
	| BusyCursor
	| IBeamCursor
	| CrossCursor
	| FatCrossCursor
	| ArrowCursor
	| HiddenCursor
	deriving Eq

-- | The document interface type of interactive processes.
data	DocumentInterface
	= NDI				-- ^ No document interface
	| SDI				-- ^ Single document interface
	| MDI				-- ^ Multiple document interface
	deriving (Eq,Show)

data	SliderMove
	= SliderIncSmall
	| SliderDecSmall
	| SliderIncLarge
	| SliderDecLarge
	| SliderThumb Int
	deriving Show


-- | Common error report type.
data	ErrorReport						-- Usual cause:	
	= ErrorViolateDI					-- ^ Violation against DocumentInterface
	| ErrorIdsInUse						-- ^ Object contains Ids that are bound
	| ErrorUnknownObject					-- ^ Object can not be found
	| ErrorNotifierOpen					-- ^ It was tried to open a second send notifier
	| ErrorUnableReceiver					-- ^ Sending to receiver that exists, but its ReceiverSelectState is Unable. 
	| OtherError !String					-- ^ Some other kind of error
	deriving (Eq,Show)

{-# NOINLINE errTy #-}
errTy = mkTyConApp (mkTyCon "ErrorReport") []

instance Typeable ErrorReport where
	typeOf _ = errTy

handleErrorReport :: Monad m => ErrorReport -> m a
handleErrorReport ErrorViolateDI = fail "Object I/O: Violation against DocumentInterface"
handleErrorReport ErrorIdsInUse	= fail "Object I/O: Object contains Ids that are bound"
handleErrorReport ErrorUnknownObject = fail "Object I/O: Object can not be found"
handleErrorReport ErrorNotifierOpen = fail "Object I/O: It was tried to open a second send notifier"
handleErrorReport ErrorUnableReceiver = fail "Object I/O: unable receiver"
handleErrorReport (OtherError msg) = fail ("Object I/O: " ++ msg)