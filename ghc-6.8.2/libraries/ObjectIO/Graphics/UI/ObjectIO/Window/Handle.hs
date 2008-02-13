-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Window.Handle
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Window.Handle contains the internal data structures that represent the 
-- state of windows. 
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Window.Handle
		( ControlState, WindowHandles(..)
		, Graphics.UI.ObjectIO.CommonDef.Bound(..), WindowStateHandle(..), WIDS(..)
		, WindowLSHandle(..)
		, ScrollInfo(..),  ClipState(..)
		, WindowHandle(..), WindowKind(..), WindowMode(..)
		, WElementHandle(..)
		, LayoutInfo(..), WItemInfo(..)
		, RadioInfo(..), RadioItemInfo(..), CheckInfo(..)
		, CheckItemInfo(..), PopUpInfo(..), PopUpEditInfo(..), ListBoxInfo(..)
		, SliderInfo(..), ButtonInfo(..), EditInfo(..), TextInfo(..)
		, ControlKind(..), FinalModalLS(..)
		, wElementHandleToControlState, controlStateToWElementHandle
		, Graphics.UI.ObjectIO.OS.Picture.OSPictContext
		, Graphics.UI.ObjectIO.OS.Picture.Origin
		, Graphics.UI.ObjectIO.OS.Picture.Pen(..)
		, Graphics.UI.ObjectIO.OS.Picture.Font(..)
		, Graphics.UI.ObjectIO.OS.Picture.Colour(..)
		, module Graphics.UI.ObjectIO.StdControlDef
		, module Graphics.UI.ObjectIO.StdWindowDef
		, module Graphics.UI.ObjectIO.OS.Types
		, isCustomisedControl, isRecursiveControl
		, LookInfo(..), WindowInfo(..)
		, CustomButtonInfo(..), CustomInfo(..)
		, CompoundInfo(..), CompoundLookInfo(..)
		) where
                    
                    

import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.StdControlDef
import Graphics.UI.ObjectIO.StdWindowDef
import Graphics.UI.ObjectIO.KeyFocus
import Graphics.UI.ObjectIO.Receiver.Handle(ReceiverHandle(..))
import Graphics.UI.ObjectIO.OS.Picture
import Graphics.UI.ObjectIO.OS.Types
import Graphics.UI.ObjectIO.OS.Rgn(OSRgnHandle)


type ControlState ls ps
   = WElementHandle ls ps            -- is a WElementHandle

data WindowHandles ps
   = WindowHandles
        { whsWindows      :: [WindowStateHandle ps]   -- The windows and their controls of a process
        , whsNrWindowBound:: !Bound             -- The maximum number of windows that are allowed to be opened
        , whsModal	  :: Bool		-- Flag: the window system is modal (used in combination with modal dialogues)
	, whsFinalModalLS :: [FinalModalLS]	-- The final local states of terminated modal dialogs
        }

data FinalModalLS = forall ls . FinalModalLS WIDS ls

data WindowStateHandle ps
   = forall ls . 
      WindowStateHandle 
            WIDS                            	-- A window is identified by an Id and an OSWindowPtr
        (Maybe (WindowLSHandle ls ps))          -- If used as placeholder, Nothing; otherwise window with local state
data WindowLSHandle ls ps
   = WindowLSHandle
        { wlsState        :: ls                 -- The local state of this window
        , wlsHandle       :: WindowHandle ls ps -- The window implementation
        }
data WIDS
   = WIDS
        { wId             :: Id                 -- Id  of window
        , wPtr            :: !OSWindowPtr     	-- Ptr of window
        , wActive         :: !Bool              -- The window is the active window (True) or not (False)
        }
data WindowHandle ls ps
   = WindowHandle
        { whMode      :: !WindowMode            -- The window mode (Modal or Modeless)
        , whKind      :: !WindowKind      	-- The window kind (Window or Dialog)
        , whTitle     :: !Title             	-- The window title
        , whItemNrs   :: [Int]              	-- The list of free system item numbers for all controls
        , whKeyFocus  :: KeyFocus         	-- The item that has the keyboard input focus
        , whWindowInfo:: WindowInfo		-- Additional information about the window
        , whItems     :: [WElementHandle ls ps] -- The window controls
        , whShow      :: Bool			-- The visibility of the window (True iff visible)
        , whSelect    :: Bool			-- The WindowSelectState==Able (by default True)
        , whAtts      :: ![WindowAttribute ls ps] -- The window attributes
        , whDefaultId :: Maybe Id		-- The Id of the optional default button
	, whCancelId  :: Maybe Id		-- The Id of the optional cancel  button
        , whSize      :: !Size              	-- The exact size of the window        
        , whClosing   :: !Bool              	-- Flag: the window is being closed (True)
        }
        
data LookInfo
   = LookInfo
   	{ lookFun	:: Look			-- The Look function
	, lookPen	:: Pen			-- The settings of the Pen
	, lookSysUpdate	:: Bool			-- The system handles updates as much as possible
	}

data WindowInfo
    = WindowInfo
	{ windowDomain	:: Rect			-- The optional view domain of the window
	, windowOrigin	:: Point2		-- The Origin of the view domain
	, windowHScroll	:: Maybe ScrollInfo	-- The scroll data of the WindowHScroll attribute
	, windowVScroll	:: Maybe ScrollInfo	-- The scroll data of the WindowVScroll attribute
	, windowLook	:: LookInfo		-- The look and pen of the window
	, windowClip	:: ClipState		-- The clipped elements of the window
	}
    | NoWindowInfo
        
data WindowMode                          	-- Modality of the window
    = Modal                           		-- Modal window (only for dialogs)
    | Modeless                        		-- Modeless window
    deriving (Eq)
    
data WindowKind
    = IsWindow                          	-- Window kind
    | IsDialog                          	-- Dialog kind
    deriving (Eq)
    
data ScrollInfo
   = ScrollInfo	
      {	scrollFunction	:: ScrollFunction	-- The ScrollFunction of the (horizontal/vertical) scroll attribute
      ,	scrollItemPos	:: Point2		-- The exact position of the scrollbar
      ,	scrollItemSize	:: Size			-- The exact size of the scrollbar
      ,	scrollItemPtr	:: OSWindowPtr		-- The OSWindowPtr of the scrollbar
      }

data ClipState 
   = ClipState
      { clipRgn     :: OSRgnHandle              -- The clipping region
      , clipOk      :: Bool                 	-- Flag: the clipping region is valid
      }

data    WElementHandle ls ps
    = WListLSHandle       [WElementHandle  ls      ps]
    | forall ls1 . 
      WExtendLSHandle ls1 [WElementHandle (ls1,ls) ps]
    | forall ls1 . 
      WChangeLSHandle ls1 [WElementHandle  ls1     ps]
    | WItemHandle
        { wItemId         :: Maybe Id           -- If the control has a (ControlId id) attribute, then Just id; Nothing
        , wItemNr         :: Int                -- The internal nr of this control  (generated from whItemNrs)
        , wItemKind       :: ControlKind        -- The sort of control
        , wItemShow       :: Bool               -- The visibility of the control (True iff visible)
        , wItemSelect     :: Bool               -- The ControlSelectState==Able  (by default True)
        , wItemInfo       :: WItemInfo ls ps  	-- Additional information of the control
        , wItemAtts       :: [ControlAttribute ls ps] -- The control attributes                   
        , wItems          :: [WElementHandle ls ps]   -- In case of   CompoundControl : its control elements
                                    		      -- Otherwise        	      : []
        , wItemVirtual    :: Bool               -- The control is virtual (True) and should not be layn out
        , wItemPos        :: Point2             -- The exact position of the item
        , wItemSize       :: Size               -- The exact size of the item
        , wItemPtr        :: OSWindowPtr        -- The ptr to the item (osNoWindowPtr if no handle)
        , wItemLayoutInfo :: LayoutInfo         -- Additional information on layout
        }
        
        
data LayoutInfo                          	-- The layout attribute of the layout root control is:
    = LayoutFix                             	-- ItemPos    = Fix
    | LayoutFrame                           	-- any other attribute
    deriving (Eq)
    
data WItemInfo ls ps
    = WButtonInfo ButtonInfo                    -- In case of   ButtonControl   : the button information
    | WCheckInfo  (CheckInfo ls ps)  		-- In case of   CheckControl    : the check items information
    | WCompoundInfo CompoundInfo		-- In case of	CompoundControl	: the compound control information
    | WCustomButtonInfo	CustomButtonInfo	-- In case of	CustomButtonControl	: the custom button information
    | WCustomInfo CustomInfo			-- In case of	CustomControl		: the custom information
    | WEditInfo   EditInfo                      -- In case of   EditControl     : the edit text information
    | WPopUpInfo  (PopUpInfo ls ps)  		-- In case of   PopUpControl    : the pop up information
    | WListBoxInfo (ListBoxInfo ls ps)		-- In case of   ListBoxControl  : the list box information
    | WRadioInfo  (RadioInfo ls ps)  		-- In case of   RadioControl    : the radio items information   
    | WReceiverInfo (ReceiverHandle ls ps)	-- In case of	ReceiverControl	: the receiver information
    | WSliderInfo (SliderInfo ls ps)  		-- In case of   SliderControl   : the slider information
    | WTextInfo   TextInfo                      -- In case of   TextControl     : the text information  
    | NoWItemInfo                           	-- No additional information
    
data RadioInfo ls ps
   = RadioInfo
        { radioItems        :: [RadioItemInfo ls ps]  		-- The radio items and their exact position (initially zero)
        , radioLayout       :: RowsOrColumns        		-- The layout of the radio items
        , radioIndex        :: Int              		-- The currently selected radio item (1<=radioIndex<=length radioItems)
        }
data RadioItemInfo ls ps
   = RadioItemInfo
        { radioItem         :: (String,Int,GUIFun ls ps) 	-- The RadioItem of the definition (Int field redundant)
        , radioItemPos      :: !Point2          		-- The exact position of the item
        , radioItemSize     :: Size             		-- The exact size of the item
        , radioItemPtr      :: OSWindowPtr          		-- The OSWindowPtr of the item
        }
data CheckInfo ls ps
   = CheckInfo
        { checkItems        :: [CheckItemInfo ls ps]		-- The check items and their exact position (initially zero)
        , checkLayout       :: RowsOrColumns    		-- The layout of the check items
        }
data CheckItemInfo ls ps
   = CheckItemInfo
        { checkItem         :: (String,Int,MarkState,GUIFun ls ps) -- The CheckItem of the definition (Int field redundant)
        , checkItemPos      :: !Point2              		-- The exact position of the item
        , checkItemSize     :: Size                 		-- The exact size of the item
        , checkItemPtr      :: OSWindowPtr          		-- The OSWindowPtr of the item
        }
data PopUpInfo ls ps
   = PopUpInfo
        { popUpInfoItems    :: [PopUpControlItem ps (ls,ps)]	-- The pop up items
        , popUpInfoIndex    :: Index                		-- The currently selected pop up item (1<=popUpInfoIndex<=length popUpInfoItems)
        , popUpInfoEdit     :: Maybe PopUpEditInfo  		-- If the pop up is editable: the PopUpEditInfo, otherwise Nothing
        }
data PopUpEditInfo
   = PopUpEditInfo
        { popUpEditText     :: String               		-- The current content of the editable pop up
        , popUpEditPtr      :: OSWindowPtr          		-- The OSWindowPtr of the editable pop up
        }
data ListBoxInfo ls ps
   = ListBoxInfo
        { listBoxInfoItems  :: [ListBoxControlItem ps (ls,ps)]	-- The list box items
        , listBoxNrLines    :: NrLines
        , listBoxInfoMultiSel :: Bool
        }
data SliderInfo ls ps
   = SliderInfo
        { sliderInfoDir     :: Direction            -- The direction of the slider
        , sliderInfoLength  :: Int                  -- The length (in pixels) of the slider
        , sliderInfoState   :: SliderState          -- The current slider state
        , sliderInfoAction  :: SliderAction ls ps   -- The action of the slider
        }
data ButtonInfo
   = ButtonInfo
        { buttonInfoText  :: String                 -- The title of the button control
        }
data CustomButtonInfo
   = CustomButtonInfo
   	{ cButtonInfoLook :: LookInfo	    	    -- The look of the custom button control
	}
data CustomInfo
   = CustomInfo
   	{ customInfoLook :: LookInfo		    -- The look of the custom control
	}
data  CompoundInfo
    = CompoundInfo
    	{ compoundDomain  :: Rect                   -- The optional view domain of the compound control
        , compoundOrigin  :: Point2                 -- The Origin of the view domain
        , compoundHScroll :: Maybe ScrollInfo       -- The scroll data of the ControlHScroll attribute
        , compoundVScroll :: Maybe ScrollInfo       -- The scroll data of the ControlVScroll attribute
        , compoundLookInfo:: CompoundLookInfo       -- The look information of the compound control
        }
data  CompoundLookInfo
    = CompoundLookInfo
    	{ compoundLook    :: LookInfo               -- The look of the compound control
        , compoundClip    :: ClipState              -- The clipped elements of the compound control
        }
data    EditInfo
    = EditInfo
        { editInfoText    :: !String                -- The content of the edit control
        , editInfoWidth   :: Int                    -- The width (in pixels) of the edit item
        , editInfoNrLines :: Int                    -- The nr of complete visible lines of the edit item
        }
data    TextInfo
    = TextInfo
        { textInfoText    :: String                 -- The content of the text control
        }
data    ControlKind
    = IsButtonControl
    | IsCheckControl
    | IsCompoundControl
    | IsCustomButtonControl
    | IsCustomControl
    | IsEditControl
    | IsLayoutControl
    | IsPopUpControl
    | IsListBoxControl
    | IsRadioControl
    | IsSliderControl
    | IsTextControl
    | IsReceiverControl 				-- Of other controls the ControlType

    deriving (Eq,Show)


instance Eq WIDS where
    (==) wids wids' = wPtr wids==wPtr wids' && wId wids==wId wids'


--  The given ControlKind corresponds with a custom-drawn control.

isCustomisedControl :: ControlKind -> Bool
isCustomisedControl IsCustomButtonControl = True
isCustomisedControl IsCustomControl   = True
isCustomisedControl _             = False

--  The given ControlKind corresponds with a control that contains other controls (CompoundControl).

isRecursiveControl :: ControlKind -> Bool
isRecursiveControl IsCompoundControl    = True
isRecursiveControl IsLayoutControl  = True
isRecursiveControl _            = False


--  Conversion functions from ControlState to WElementHandle, and vice versa:

wElementHandleToControlState :: WElementHandle ls ps -> ControlState ls ps
wElementHandleToControlState wH = wH

controlStateToWElementHandle :: ControlState ls ps -> WElementHandle ls ps
controlStateToWElementHandle wH = wH
