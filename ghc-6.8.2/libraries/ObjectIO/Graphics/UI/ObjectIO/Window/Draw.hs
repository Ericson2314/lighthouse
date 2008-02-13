-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Window.Draw
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Window.Draw contains the window drawing functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Window.Draw where


import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Control.Layout
import Graphics.UI.ObjectIO.StdPicture
import Graphics.UI.ObjectIO.Window.Access
import Graphics.UI.ObjectIO.OS.Picture
import Graphics.UI.ObjectIO.OS.Rgn
import Graphics.UI.ObjectIO.OS.Window
import Graphics.UI.ObjectIO.StdWindowAttribute(isWindowDoubleBuffered)


{-	drawWindowLook wPtr includeBackground window
		applies the Look function of window.
	The wPtr argument must be the OSWindowPtr of window.
	It is assumed that window refers to a Window with a valid ClipState.
	If includeBackground is True then also the background outside the WindowViewDomain is drawn.
-}
drawWindowLook :: OSWindowMetrics -> OSWindowPtr -> Draw () -> UpdateState -> WindowHandle ls ps -> IO (WindowHandle ls ps)
drawWindowLook wMetrics wPtr drawFirst updState wH@(WindowHandle {whSelect=whSelect,whSize=whSize,whWindowInfo=info,whAtts=attrs}) = do
    osPict <- osGrabWindowPictContext wPtr    
    (_,_,pen,_) <- doDraw origin (lookPen look) True clip' osPict (any isWindowDoubleBuffered attrs) draw
    osReleaseWindowPictContext wPtr osPict
    osValidateWindowRgn wPtr clip' 		-- PA: added to eliminate update of window (in drawing part)	
    return (wH{whWindowInfo=info{windowLook=look{lookPen=pen}}})
    where
        draw = drawFirst >> accClipPicture (toRegion (if lookSysUpdate look then updArea updState else [wFrame])) (lookFun look select updState)
           
	select		= if whSelect then Able else Unable
	domainRect	= windowDomain info
	origin		= windowOrigin info
	look		= windowLook info
	clip		= windowClip info
	clip'		= clipRgn clip
	hasScrolls	= (isJust (windowHScroll info),isJust (windowVScroll info))
	visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
	Size {w=w,h=h}	= rectSize (getWindowContentRect wMetrics visScrolls (sizeToRect whSize))
	wFrame		= Rectangle{corner1=origin,corner2=Point2{x=x origin + w,y=y origin + h}}


drawWindowLook' :: OSWindowMetrics -> OSWindowPtr -> Draw [Rect] -> UpdateState -> WindowHandle ls ps -> IO (WindowHandle ls ps)
drawWindowLook' wMetrics wPtr drawFirst updState wH@(WindowHandle {whSelect=whSelect,whSize=whSize,whWindowInfo=info,whAtts=attrs}) = do
    osPict <- osGrabWindowPictContext wPtr    
    (_,_,pen,_) <- doDraw origin (lookPen look) True clip' osPict (any isWindowDoubleBuffered attrs) draw
    osReleaseWindowPictContext wPtr osPict
    osValidateWindowRgn wPtr clip' 		-- PA: added to eliminate update of window (in drawing part)	
    return (wH{whWindowInfo=info{windowLook=look{lookPen=pen}}})
    where
        draw = do
            additionalUpdateArea <- drawFirst
            let updState1 = updState{updArea = [rectToRectangle r | r<-additionalUpdateArea, not (isEmptyRect r)] ++ (updArea updState)}
            accClipPicture (toRegion (if lookSysUpdate look then updArea updState1 else [wFrame])) (lookFun look select updState1)
            return ()
        
	select		= if whSelect then Able else Unable
	domainRect	= windowDomain info
	origin		= windowOrigin info
	look		= windowLook info
	clip		= windowClip info
	clip'		= clipRgn clip
	hasScrolls	= (isJust (windowHScroll info),isJust (windowVScroll info))
	visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
	Size {w=w,h=h}	= rectSize (getWindowContentRect wMetrics visScrolls (sizeToRect whSize))
	wFrame		= Rectangle{corner1=origin,corner2=Point2{x=x origin + w,y=y origin + h}}


{-	drawInWindow wPtr drawfun window
		applies the drawing function to the picture of the window.
	The wPtr argument must be the OSWindowPtr of the window.
	It is assumed that window refers to a Window with a valid ClipState.
-}
drawInWindow :: OSWindowMetrics -> OSWindowPtr -> Draw a -> (WindowHandle ls ps) -> IO (a, WindowHandle ls ps)
drawInWindow wMetrics wPtr drawFun wH@(WindowHandle {whSize=whSize,whWindowInfo=info,whAtts=attrs}) = do
    domainRgn <- osNewRectRgn contentRect
    clip <- osSectRgn domainRgn (clipRgn (windowClip info))
    osPict <- osGrabWindowPictContext wPtr    
    (x,_,pen,_) <- doDraw (windowOrigin info) (lookPen look) True clip osPict (any isWindowDoubleBuffered attrs) drawFun
    osReleaseWindowPictContext wPtr osPict
    mapM_ osDisposeRgn [domainRgn,clip]
    return (x,wH{whWindowInfo=info{windowLook=look{lookPen=pen}}})
    where
	look		= windowLook info
	hasScrolls	= (isJust (windowHScroll info),isJust (windowVScroll info))
	visScrolls	= osScrollbarsAreVisible wMetrics (windowDomain info) (toTuple whSize) hasScrolls
	contentRect	= getWindowContentRect wMetrics visScrolls (sizeToRect whSize)
