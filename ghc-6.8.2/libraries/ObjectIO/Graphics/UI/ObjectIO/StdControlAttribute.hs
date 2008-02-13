-----------------------------------------------------------------------------
-- |
-- Module      :  StdControlAttribute
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdControlAttribute specifies which ControlAttributes are valid for each
-- of the standard controls.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdControlAttribute
		( module Graphics.UI.ObjectIO.StdControlAttribute
		, module Graphics.UI.ObjectIO.StdControlDef
		) where



import Graphics.UI.ObjectIO.StdControlDef
import Graphics.UI.ObjectIO.StdPicture(PenAttribute(..), Look)



isValidButtonControlAttribute :: ControlAttribute ls ps -> Bool
isValidButtonControlAttribute (ControlFunction     _) = True
isValidButtonControlAttribute (ControlHide	    ) = True
isValidButtonControlAttribute (ControlId           _) = True
isValidButtonControlAttribute (ControlModsFunction _) = True
isValidButtonControlAttribute (ControlPos          _) = True
isValidButtonControlAttribute (ControlSelectState  _) = True
isValidButtonControlAttribute (ControlWidth        _) = True
isValidButtonControlAttribute _                       = False

isValidCheckControlAttribute :: ControlAttribute ls ps -> Bool
isValidCheckControlAttribute (ControlHide	  ) = True
isValidCheckControlAttribute (ControlId          _) = True
isValidCheckControlAttribute (ControlPos         _) = True
isValidCheckControlAttribute (ControlSelectState _) = True
isValidCheckControlAttribute (ControlTip         _) = True
isValidCheckControlAttribute _			    = False

isValidCompoundControlAttribute :: (ControlAttribute ls ps) -> Bool
isValidCompoundControlAttribute (ControlFunction     _)	= False
isValidCompoundControlAttribute (ControlModsFunction _)	= False
isValidCompoundControlAttribute _			= True

isValidEditControlAttribute :: ControlAttribute ls ps -> Bool
isValidEditControlAttribute (ControlActivate    _)  = True
isValidEditControlAttribute (ControlDeactivate  _)  = True
isValidEditControlAttribute (ControlHide	 )  = True
isValidEditControlAttribute (ControlId          _)  = True
isValidEditControlAttribute (ControlKeyboard _ _ _) = True
isValidEditControlAttribute (ControlPos         _)  = True
isValidEditControlAttribute (ControlResize      _)  = True
isValidEditControlAttribute (ControlSelectState _)  = True
isValidEditControlAttribute (ControlTip         _)  = True
isValidEditControlAttribute _                       = False

isValidLayoutControlAttribute :: ControlAttribute ls ps -> Bool
isValidLayoutControlAttribute (ControlHide	   ) = True
isValidLayoutControlAttribute (ControlHMargin   _ _) = True
isValidLayoutControlAttribute (ControlId          _) = True
isValidLayoutControlAttribute (ControlItemSpace _ _) = True
isValidLayoutControlAttribute (ControlMinimumSize _) = True
isValidLayoutControlAttribute (ControlOuterSize   _) = True
isValidLayoutControlAttribute (ControlPos         _) = True
isValidLayoutControlAttribute (ControlResize      _) = True
isValidLayoutControlAttribute (ControlSelectState _) = True
isValidLayoutControlAttribute (ControlViewSize    _) = True
isValidLayoutControlAttribute (ControlVMargin   _ _) = True
isValidLayoutControlAttribute _			     = False

isValidPopUpControlAttribute :: ControlAttribute ls ps -> Bool
isValidPopUpControlAttribute (ControlActivate    _) = True
isValidPopUpControlAttribute (ControlDeactivate  _) = True
isValidPopUpControlAttribute (ControlHide	  ) = True
isValidPopUpControlAttribute (ControlId          _) = True
isValidPopUpControlAttribute (ControlPos         _) = True
isValidPopUpControlAttribute (ControlSelectState _) = True
isValidPopUpControlAttribute (ControlTip         _) = True
isValidPopUpControlAttribute (ControlWidth	 _) = True
isValidPopUpControlAttribute _			    = False

isValidRadioControlAttribute :: ControlAttribute ls ps -> Bool
isValidRadioControlAttribute (ControlHide	  ) = True
isValidRadioControlAttribute (ControlId          _) = True
isValidRadioControlAttribute (ControlPos         _) = True
isValidRadioControlAttribute (ControlSelectState _) = True
isValidRadioControlAttribute (ControlTip         _) = True
isValidRadioControlAttribute _			    = False

isValidSliderControlAttribute :: ControlAttribute ls ps -> Bool
isValidSliderControlAttribute (ControlHide	   ) = True
isValidSliderControlAttribute (ControlId          _) = True
isValidSliderControlAttribute (ControlPos         _) = True
isValidSliderControlAttribute (ControlResize      _) = True
isValidSliderControlAttribute (ControlSelectState _) = True
isValidSliderControlAttribute (ControlTip         _) = True
isValidSliderControlAttribute _			     = False

isValidTextControlAttribute :: ControlAttribute ls ps -> Bool
isValidTextControlAttribute (ControlId    _) = True
isValidTextControlAttribute (ControlPos   _) = True
isValidTextControlAttribute (ControlWidth _) = True
isValidTextControlAttribute _                = False


isControlActivate 	:: ControlAttribute ls ps -> Bool
isControlActivate 	(ControlActivate _) 	= True
isControlActivate 	_			= False

isControlDeactivate 	:: ControlAttribute ls ps -> Bool
isControlDeactivate 	(ControlDeactivate _) = True
isControlDeactivate 	_			  = False


isControlFunction 	:: ControlAttribute ls ps -> Bool
isControlFunction 	(ControlFunction _)   = True
isControlFunction 	_                     = False

isControlHide 		:: ControlAttribute ls ps -> Bool
isControlHide 		ControlHide		= True
isControlHide 		_				= False

isControlHMargin 	:: ControlAttribute ls ps -> Bool
isControlHMargin 	(ControlHMargin _ _)	= True
isControlHMargin 	_			= False

isControlHScroll 	:: ControlAttribute ls ps -> Bool
isControlHScroll 	(ControlHScroll _)	= True
isControlHScroll 	_			= False

isControlId 		:: ControlAttribute ls ps -> Bool
isControlId 		(ControlId _)         	= True
isControlId 		_                     	= False

isControlItemSpace 	:: ControlAttribute ls ps -> Bool
isControlItemSpace 	(ControlItemSpace _ _) 	= True
isControlItemSpace 	_			= False

isControlKeyboard 	:: ControlAttribute ls ps -> Bool
isControlKeyboard 	(ControlKeyboard _ _ _) = True
isControlKeyboard 	_                       = False

isControlLook		:: ControlAttribute ls ps -> Bool
isControlLook		(ControlLook _ _)	= True
isControlLook		_			= False

isControlMinimumSize	:: ControlAttribute ls ps -> Bool
isControlMinimumSize	(ControlMinimumSize _)	= True
isControlMinimumSize	_			= False

isControlModsFunction 	:: ControlAttribute ls ps -> Bool
isControlModsFunction 	(ControlModsFunction _) = True
isControlModsFunction 	_                       = False

isControlMouse 		:: ControlAttribute ls ps -> Bool
isControlMouse 		(ControlMouse _ _ _)	= True
isControlMouse 		_			= False

isControlOrigin 	:: ControlAttribute ls ps -> Bool
isControlOrigin 	(ControlOrigin _)	= True
isControlOrigin		_			= False

isControlOuterSize 	:: ControlAttribute ls ps -> Bool
isControlOuterSize 	(ControlOuterSize _)	= True
isControlOuterSize 	_			= False

isControlPen 		:: ControlAttribute ls ps -> Bool
isControlPen 		(ControlPen _)		= True
isControlPen 		_			= False

isControlPos 		:: ControlAttribute ls ps -> Bool
isControlPos 		(ControlPos _)   	= True
isControlPos 		_                	= False

isControlResize		:: ControlAttribute ls ps -> Bool
isControlResize		(ControlResize _)	= True
isControlResize		_			= False

isControlSelectState	:: ControlAttribute ls ps -> Bool
isControlSelectState	(ControlSelectState _)	= True
isControlSelectState	_						= False

isControlTip		:: ControlAttribute ls ps -> Bool
isControlTip		(ControlTip _)		= True
isControlTip		_			= False

isControlViewDomain	:: ControlAttribute ls ps -> Bool
isControlViewDomain	(ControlViewDomain _)	= True
isControlViewDomain	_			= False

isControlViewSize	:: ControlAttribute ls ps -> Bool
isControlViewSize	(ControlViewSize _)	= True
isControlViewSize	_			= False

isControlVMargin	:: ControlAttribute ls ps -> Bool
isControlVMargin	(ControlVMargin _ _)	= True
isControlVMargin	_			= False

isControlVScroll	:: ControlAttribute ls ps -> Bool
isControlVScroll	(ControlVScroll _)	= True
isControlVScroll	_			= False

isControlWidth    	:: ControlAttribute ls ps -> Bool
isControlWidth    	(ControlWidth _)        = True
isControlWidth    	_                       = False

isControlDoubleBuffered :: ControlAttribute ls ps -> Bool
isControlDoubleBuffered ControlDoubleBuffered = True
isControlDoubleBuffered _                     = False

getControlActivateFun :: ControlAttribute ls ps -> GUIFun ls ps
getControlActivateFun (ControlActivate f) = f

getControlDeactivateFun :: ControlAttribute ls ps -> GUIFun ls ps
getControlDeactivateFun (ControlDeactivate f) = f

getControlFun :: ControlAttribute ls ps -> GUIFun ls ps
getControlFun (ControlFunction f) = f

getControlHMarginAtt :: ControlAttribute ls ps -> (Int,Int)
getControlHMarginAtt (ControlHMargin left right) = (left,right)

getControlHScrollFun :: ControlAttribute ls ps -> ScrollFunction
getControlHScrollFun (ControlHScroll f) = f

getControlIdAtt :: ControlAttribute ls ps -> Id
getControlIdAtt (ControlId id) = id

getControlItemSpaceAtt :: ControlAttribute ls ps -> (Int,Int)
getControlItemSpaceAtt (ControlItemSpace hspace vspace) = (hspace,vspace)

getControlKeyboardAtt :: ControlAttribute ls ps -> (KeyboardStateFilter,SelectState,KeyboardFunction ls ps)
getControlKeyboardAtt (ControlKeyboard filter s f) = (filter,s,f)

getControlLookAtt :: ControlAttribute ls ps -> (Bool,Look)
getControlLookAtt (ControlLook systemLook f) = (systemLook,f)

getControlMinimumSizeAtt :: ControlAttribute ls ps -> Size
getControlMinimumSizeAtt (ControlMinimumSize size) = size

getControlModsFun :: ControlAttribute ls ps -> ModifiersFunction ls ps
getControlModsFun (ControlModsFunction f) = f

getControlMouseAtt :: ControlAttribute ls ps -> (MouseStateFilter,SelectState,MouseFunction ls ps)
getControlMouseAtt (ControlMouse filter s f) = (filter,s,f)

getControlOriginAtt :: ControlAttribute ls ps -> Point2
getControlOriginAtt (ControlOrigin p) = p

getControlOuterSizeAtt :: ControlAttribute ls ps -> Size
getControlOuterSizeAtt (ControlOuterSize size) = size

getControlPenAtt :: ControlAttribute ls ps -> [PenAttribute]
getControlPenAtt (ControlPen atts) = atts

getControlPosAtt :: ControlAttribute ls ps -> ItemPos
getControlPosAtt (ControlPos itemPos) = itemPos

getControlResizeFun :: ControlAttribute ls ps -> ControlResizeFunction
getControlResizeFun (ControlResize f) = f

getControlSelectStateAtt :: ControlAttribute ls ps -> SelectState
getControlSelectStateAtt (ControlSelectState s) = s

getControlTipAtt :: ControlAttribute ls ps -> String
getControlTipAtt (ControlTip tip) = tip

getControlViewDomainAtt :: ControlAttribute ls ps -> ViewDomain
getControlViewDomainAtt (ControlViewDomain pd) = pd

getControlViewSizeAtt :: ControlAttribute ls ps -> Size
getControlViewSizeAtt (ControlViewSize size) = size

getControlVMarginAtt :: ControlAttribute ls ps -> (Int,Int)
getControlVMarginAtt (ControlVMargin top bottom) = (top,bottom)

getControlVScrollFun :: ControlAttribute ls ps -> ScrollFunction
getControlVScrollFun (ControlVScroll f) = f

getControlWidthAtt :: ControlAttribute ls ps -> ControlWidth
getControlWidthAtt (ControlWidth w) = w

