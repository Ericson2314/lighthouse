-----------------------------------------------------------------------------
-- |
-- Module      :  StdWindowAttribute
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdWindowAttribute specifies which WindowAttributes are valid for Windows.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdWindowAttribute
		( isValidWindowAttribute, isValidDialogAttribute
		, isWindowActivate,   	getWindowActivateFun
		, isWindowCancel,	getWindowCancelAtt
		, isWindowClose,      	getWindowCloseFun
		, isWindowCursor,	getWindowCursorAtt
		, isWindowDeactivate, 	getWindowDeactivateFun
		, isWindowHMargin,	getWindowHMarginAtt
		, isWindowHScroll,	getWindowHScrollFun
		, isWindowId,         	getWindowIdAtt
		, isWindowIndex,	getWindowIndexAtt
		, isWindowInit,       	getWindowInitFun
		, isWindowInitActive, 	getWindowInitActiveAtt
		, isWindowItemSpace,  	getWindowItemSpaceAtt
		, isWindowKeyboard,	getWindowKeyboardAtt
		, isWindowLook,		getWindowLookAtt
		, isWindowMouse,	getWindowMouseAtt
		, isWindowOk,		getWindowOkAtt
		, isWindowOrigin,	getWindowOriginAtt
		, isWindowOuterSize,	getWindowOuterSizeAtt
		, isWindowPen,		getWindowPenAtt
		, isWindowPos,		getWindowPosAtt
		, isWindowSelectState,	getWindowSelectStateAtt
		, isWindowViewDomain, 	getWindowViewDomainAtt
		, isWindowViewSize,   	getWindowViewSizeAtt
		, isWindowVMargin,	getWindowVMarginAtt
		, isWindowVScroll,	getWindowVScrollFun
		, isWindowCaret,	getWindowCaretAtt
		, isWindowResize, 	getWindowResizeAtt
		, isWindowDoubleBuffered
		, module Graphics.UI.ObjectIO.StdWindowDef
		) where


import Graphics.UI.ObjectIO.StdWindowDef


isValidWindowAttribute :: WindowAttribute ls ps -> Bool
isValidWindowAttribute att = isAllWindowsAttribute att || isWindowOnlyAttribute att

isValidDialogAttribute :: WindowAttribute ls ps -> Bool
isValidDialogAttribute att = isAllWindowsAttribute att || isDialogOnlyAttribute att

isAllWindowsAttribute :: WindowAttribute ls ps -> Bool
isAllWindowsAttribute (WindowActivate   _) = True
isAllWindowsAttribute (WindowClose      _) = True
isAllWindowsAttribute (WindowDeactivate _) = True
isAllWindowsAttribute (WindowHMargin  _ _) = True
isAllWindowsAttribute (WindowId         _) = True
isAllWindowsAttribute (WindowIndex	_) = True
isAllWindowsAttribute (WindowInit       _) = True
isAllWindowsAttribute (WindowInitActive _) = True
isAllWindowsAttribute (WindowItemSpace _ _)= True
isAllWindowsAttribute (WindowOuterSize	_) = True
isAllWindowsAttribute (WindowPos	_) = True
isAllWindowsAttribute (WindowViewSize   _) = True
isAllWindowsAttribute (WindowVMargin  _ _) = True
isAllWindowsAttribute _                    = False

isWindowOnlyAttribute :: WindowAttribute ls ps -> Bool
isWindowOnlyAttribute (WindowCursor	  _) = True
isWindowOnlyAttribute (WindowHScroll	  _) = True
isWindowOnlyAttribute (WindowKeyboard _ _ _) = True
isWindowOnlyAttribute (WindowLook       _ _) = True
isWindowOnlyAttribute (WindowMouse    _ _ _) = True
isWindowOnlyAttribute (WindowOrigin	  _) = True
isWindowOnlyAttribute (WindowPen          _) = True
isWindowOnlyAttribute (WindowSelectState  _) = True
isWindowOnlyAttribute (WindowViewDomain	  _) = True
isWindowOnlyAttribute (WindowVScroll	  _) = True
isWindowOnlyAttribute (WindowCaret      _ _) = True
isWindowOnlyAttribute (WindowResize       _) = True
isWindowOnlyAttribute (WindowDoubleBuffered) = True
isWindowOnlyAttribute _			     = False

isDialogOnlyAttribute :: WindowAttribute ls ps -> Bool
isDialogOnlyAttribute (WindowCancel	  _) = True
isDialogOnlyAttribute (WindowOk		  _) = True
isDialogOnlyAttribute _			     = False

isWindowActivate :: WindowAttribute ls ps -> Bool
isWindowActivate (WindowActivate _) = True
isWindowActivate _                  = False

isWindowCancel	 :: WindowAttribute ls ps -> Bool
isWindowCancel	(WindowCancel _) = True
isWindowCancel	_		 = False

isWindowClose    :: WindowAttribute ls ps -> Bool
isWindowClose    (WindowClose _)    = True
isWindowClose    _                  = False

isWindowCursor	:: WindowAttribute ls ps -> Bool
isWindowCursor	(WindowCursor _) = True
isWindowCursor	_		 = False

isWindowDeactivate :: WindowAttribute ls ps -> Bool
isWindowDeactivate (WindowDeactivate _) = True
isWindowDeactivate _                    = False

isWindowHMargin	:: WindowAttribute ls ps -> Bool
isWindowHMargin	(WindowHMargin _ _) = True
isWindowHMargin	_		    = False

isWindowHScroll	:: WindowAttribute ls ps -> Bool
isWindowHScroll	(WindowHScroll _) = True
isWindowHScroll	_		  = False

isWindowId :: WindowAttribute ls ps -> Bool
isWindowId (WindowId _) = True
isWindowId _            = False

isWindowIndex :: WindowAttribute ls ps -> Bool
isWindowIndex (WindowIndex _) = True
isWindowIndex _		      = False

isWindowInit :: WindowAttribute ls ps -> Bool
isWindowInit (WindowInit _) = True
isWindowInit _              = False

isWindowInitActive :: WindowAttribute ls ps -> Bool
isWindowInitActive (WindowInitActive _)	= True
isWindowInitActive _			= False

isWindowItemSpace :: WindowAttribute ls ps -> Bool
isWindowItemSpace (WindowItemSpace _ _)	= True
isWindowItemSpace _			= False

isWindowKeyboard :: WindowAttribute ls ps -> Bool
isWindowKeyboard (WindowKeyboard _ _ _)	= True
isWindowKeyboard _						= False

isWindowLook :: WindowAttribute ls ps -> Bool
isWindowLook (WindowLook _ _) = True
isWindowLook _		      = False

isWindowMouse :: WindowAttribute ls ps -> Bool
isWindowMouse (WindowMouse _ _ _) = True
isWindowMouse _			  = False

isWindowOk :: WindowAttribute ls ps -> Bool
isWindowOk (WindowOk _)	= True
isWindowOk _		= False

isWindowOrigin :: WindowAttribute ls ps -> Bool
isWindowOrigin (WindowOrigin _)	= True
isWindowOrigin _		= False

isWindowOuterSize :: WindowAttribute ls ps -> Bool
isWindowOuterSize (WindowOuterSize _) = True
isWindowOuterSize _		      = False

isWindowPen :: WindowAttribute ls ps -> Bool
isWindowPen (WindowPen _) = True
isWindowPen _		  = False

isWindowPos :: WindowAttribute ls ps -> Bool
isWindowPos (WindowPos _) = True
isWindowPos _		  = False

isWindowSelectState :: WindowAttribute ls ps -> Bool
isWindowSelectState (WindowSelectState _) = True
isWindowSelectState _			  = False

isWindowViewDomain :: WindowAttribute ls ps -> Bool
isWindowViewDomain (WindowViewDomain _)	= True
isWindowViewDomain _			= False

isWindowViewSize :: WindowAttribute ls ps -> Bool
isWindowViewSize (WindowViewSize _) = True
isWindowViewSize _                  = False

isWindowVMargin	:: WindowAttribute ls ps -> Bool
isWindowVMargin	(WindowVMargin _ _) = True
isWindowVMargin	_		    = False

isWindowVScroll	:: WindowAttribute ls ps -> Bool
isWindowVScroll	(WindowVScroll _) = True
isWindowVScroll	_		  = False

isWindowCaret :: WindowAttribute ls ps -> Bool
isWindowCaret (WindowCaret _ _) = True
isWindowCaret _			= False

isWindowResize :: WindowAttribute ls ps -> Bool
isWindowResize (WindowResize _) = True
isWindowResize _		= False

isWindowDoubleBuffered :: WindowAttribute ls ps -> Bool
isWindowDoubleBuffered WindowDoubleBuffered 	= True
isWindowDoubleBuffered _			= False

getWindowActivateFun :: WindowAttribute ls ps -> GUIFun ls ps
getWindowActivateFun (WindowActivate f) = f

getWindowCancelAtt :: WindowAttribute ls ps -> Id
getWindowCancelAtt (WindowCancel id) = id

getWindowCloseFun :: WindowAttribute ls ps -> GUIFun ls ps
getWindowCloseFun (WindowClose f) = f

getWindowCursorAtt :: WindowAttribute ls ps -> CursorShape
getWindowCursorAtt (WindowCursor cShape) = cShape

getWindowDeactivateFun :: WindowAttribute ls ps -> GUIFun ls ps
getWindowDeactivateFun (WindowDeactivate f) = f

getWindowHMarginAtt :: WindowAttribute ls ps -> (Int,Int)
getWindowHMarginAtt (WindowHMargin left right) = (left,right)

getWindowHScrollFun :: WindowAttribute ls ps -> ScrollFunction
getWindowHScrollFun (WindowHScroll f) = f

getWindowIdAtt :: WindowAttribute ls ps -> Id
getWindowIdAtt (WindowId id) = id

getWindowIndexAtt :: WindowAttribute ls ps -> Int
getWindowIndexAtt (WindowIndex index) = index

getWindowInitFun :: WindowAttribute ls ps -> GUIFun ls ps
getWindowInitFun (WindowInit init) = init

getWindowInitActiveAtt :: WindowAttribute ls ps -> Id
getWindowInitActiveAtt (WindowInitActive id) = id

getWindowItemSpaceAtt :: WindowAttribute ls ps -> (Int,Int)
getWindowItemSpaceAtt (WindowItemSpace hspace vspace) = (hspace,vspace)

getWindowKeyboardAtt :: WindowAttribute ls ps -> (KeyboardStateFilter,SelectState,KeyboardFunction ls ps)
getWindowKeyboardAtt (WindowKeyboard filter select keysF) = (filter,select,keysF)

getWindowLookAtt :: WindowAttribute ls ps -> (Bool,Look)
getWindowLookAtt (WindowLook systemLook f) = (systemLook,f)

getWindowMouseAtt :: WindowAttribute ls ps  -> (MouseStateFilter,SelectState,MouseFunction ls ps)
getWindowMouseAtt (WindowMouse filter select mouseF) = (filter,select,mouseF)

getWindowOkAtt :: WindowAttribute ls ps -> Id
getWindowOkAtt (WindowOk id) = id

getWindowOriginAtt :: WindowAttribute ls ps -> Point2
getWindowOriginAtt (WindowOrigin origin) = origin

getWindowOuterSizeAtt :: WindowAttribute ls ps -> Size
getWindowOuterSizeAtt (WindowOuterSize size) = size

getWindowPenAtt :: WindowAttribute ls ps -> [PenAttribute]
getWindowPenAtt (WindowPen pen) = pen

getWindowPosAtt :: WindowAttribute ls ps -> ItemPos
getWindowPosAtt (WindowPos pos) = pos

getWindowSelectStateAtt :: WindowAttribute ls ps -> SelectState
getWindowSelectStateAtt (WindowSelectState select) = select

getWindowViewDomainAtt :: WindowAttribute ls ps -> ViewDomain
getWindowViewDomainAtt (WindowViewDomain d) = d

getWindowViewSizeAtt :: WindowAttribute ls ps -> Size
getWindowViewSizeAtt (WindowViewSize size) = size

getWindowVMarginAtt :: WindowAttribute ls ps -> (Int,Int)
getWindowVMarginAtt (WindowVMargin top bottom) = (top,bottom)

getWindowVScrollFun :: WindowAttribute ls ps -> ScrollFunction
getWindowVScrollFun (WindowVScroll f) = f

getWindowCaretAtt :: WindowAttribute ls ps -> (Point2,Size)
getWindowCaretAtt (WindowCaret pos size) = (pos,size)

getWindowResizeAtt :: WindowAttribute ls ps -> WindowResizeFunction ls ps
getWindowResizeAtt (WindowResize f) = f
