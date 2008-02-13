-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Pos
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Control.Pos(moveWindowViewFrame) where


import	Graphics.UI.ObjectIO.CommonDef
import  Graphics.UI.ObjectIO.StdWindowAttribute
import  Graphics.UI.ObjectIO.Control.Layout(layoutControls)
import  Graphics.UI.ObjectIO.Control.Relayout(relayoutControls)
import  Graphics.UI.ObjectIO.Window.Access
import  Graphics.UI.ObjectIO.Window.ClipState(forceValidWindowClipState)
import 	Graphics.UI.ObjectIO.Window.Draw(drawWindowLook')
import  Graphics.UI.ObjectIO.Window.Update(updateWindowBackgrounds)
import  Graphics.UI.ObjectIO.OS.Picture(pictScroll, Draw(..))
import  Graphics.UI.ObjectIO.OS.Rgn(osGetRgnBox)
import  Graphics.UI.ObjectIO.OS.Types(Rect)
import  Graphics.UI.ObjectIO.OS.Window(OSWindowMetrics, osScrollbarsAreVisible, osSetWindowSliderThumb, toOSscrollbarRange, osMinWindowSize, osSetCaretPos)
import  Control.Monad(when)


controlPosFatalError :: String -> String -> x
controlPosFatalError function error
	= dumpFatalError function "controlpos" error

{-	moveWindowViewFrame moves the current view frame of the WindowHandle by the given Vector2. 
	moveWindowViewFrame assumes that the argument WindowHandle is a Window.
-}
moveWindowViewFrame :: OSWindowMetrics -> Vector2 -> WIDS -> WindowHandle ls ps -> IO (WindowHandle ls ps)
moveWindowViewFrame wMetrics v wids@(WIDS {wPtr=wPtr}) wH@(WindowHandle {whItems=oldItems,whSize=whSize,whAtts=whAtts,whSelect=whSelect,whShow=whShow}) =
	if newOrigin == oldOrigin then return wH		-- origin has not changed
	else do	 
		setSliderThumb (hasHScroll && (x newOrigin) /= (x oldOrigin)) wMetrics wPtr True  (minx,x newOrigin,maxx) vieww (toTuple whSize)
		setSliderThumb (hasVScroll && (y newOrigin) /= (y oldOrigin)) wMetrics wPtr False (miny,y newOrigin,maxy) viewh (toTuple whSize)
		when hasCaret (osSetCaretPos wPtr (x caretPos - x newOrigin) (y caretPos - y newOrigin))
		(if null oldItems then		-- there are no controls: do only visual updates
		  let wH1 = wH {whWindowInfo=windowInfo{windowOrigin=newOrigin}}
		      (updArea,updAction) = 
		          if (not (lookSysUpdate lookInfo) || toMuch)
		          then ([newFrame],return [])
		          else calcScrollUpdateArea oldOrigin newOrigin contentRect
		      updState = UpdateState{oldFrame=posSizeToRectangle oldOrigin contentSize,newFrame=newFrame,updArea=updArea}
		  in drawWindowLook' wMetrics wPtr updAction updState wH1
		 else do
		     let reqSize = Size   -- there are controls: recalculate layout and do visual updates
		             { w=(w contentSize)-(fst hMargins)-(snd hMargins)
		     	     , h=(h contentSize)-(fst vMargins)-(snd vMargins)
		     	     }
		     (_, newItems) <- layoutControls wMetrics hMargins vMargins spaces reqSize minSize [(domain,newOrigin)] oldItems
		     let wH1 = wH{whItems=newItems,whWindowInfo=windowInfo{windowOrigin=newOrigin}}
		     wH2 <- forceValidWindowClipState wMetrics True wPtr wH1
		     (isRect,areaRect) <- (case whWindowInfo wH2 of
					      WindowInfo{windowClip=ClipState{clipRgn=clipRgn}} -> osGetRgnBox clipRgn
					      NoWindowInfo -> controlPosFatalError "moveWindowViewFrame" "unexpected whWindowInfo field")
		     updRgn <- relayoutControls wMetrics whSelect whShow contentRect contentRect zero zero wPtr (whDefaultId wH2) oldItems (whItems wH2)
		     wH3 <- updateWindowBackgrounds wMetrics updRgn wids wH2
		     let (updArea,updAction) = 
		     	     if (not (lookSysUpdate lookInfo) || toMuch || not isRect)
		     	     then ([newFrame],return [])
		     	     else calcScrollUpdateArea oldOrigin newOrigin areaRect
		     let updState = UpdateState{oldFrame=posSizeToRectangle oldOrigin contentSize,newFrame=newFrame,updArea=updArea}
		     drawWindowLook' wMetrics wPtr updAction updState wH3)
	where
		windowInfo = whWindowInfo wH
		(oldOrigin,domainRect,hasHScroll,hasVScroll,lookInfo)
			= ( windowOrigin windowInfo
			  , windowDomain windowInfo
			  , isJust (windowHScroll windowInfo)
			  , isJust (windowVScroll windowInfo)
			  , windowLook windowInfo
			  )		
			  
		domain = rectToRectangle domainRect
		visScrolls = osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)
		contentRect = getWindowContentRect wMetrics visScrolls (sizeToRect whSize)
		contentSize = rectSize contentRect
		Size {w=w',h=h'}  = contentSize
		(minx,maxx,vieww) = (rleft domainRect, rright  domainRect, w contentSize)
		(miny,maxy,viewh) = (rtop  domainRect, rbottom domainRect, h contentSize)
		newOrigin = Point2
			{ x = setBetween ((x oldOrigin)+(vx v)) minx (max minx (maxx-vieww))
			, y = setBetween ((y oldOrigin)+(vy v)) miny (max miny (maxy-viewh))
			}
		newFrame		= posSizeToRectangle newOrigin contentSize
		toMuch			= (abs ((x newOrigin)-(x oldOrigin))>=w') || (abs ((y newOrigin)-(y oldOrigin))>=h')
		(defMinW,defMinH)	= osMinWindowSize
		minSize			= Size{w=defMinW,h=defMinH}
		hMargins		= getWindowHMargins   IsWindow wMetrics whAtts
		vMargins		= getWindowVMargins   IsWindow wMetrics whAtts
		spaces			= getWindowItemSpaces IsWindow wMetrics whAtts
		(hasCaret,catt)  = cselect isWindowCaret undefined whAtts
		(caretPos,_)     = getWindowCaretAtt catt

		setSliderThumb :: Bool -> OSWindowMetrics -> OSWindowPtr -> Bool -> (Int,Int,Int) -> Int -> (Int,Int) -> IO ()
		setSliderThumb hasScroll wMetrics wPtr isHScroll scrollValues viewSize maxcoords
			| hasScroll	= osSetWindowSliderThumb wMetrics wPtr isHScroll osThumb maxcoords True
			| otherwise	= return ()
			where
				(_,osThumb,_,_)	= toOSscrollbarRange scrollValues viewSize
	
{-	calcScrollUpdateArea p1 p2 area calculates the new update area that has to be updated. 
	Assumptions: p1 is the origin before scrolling,
	             p2 is the origin after  scrolling,
	             area is the visible area of the window view frame.
-}
calcScrollUpdateArea :: Point2 -> Point2 -> Rect -> ([Rectangle],Draw [Rect])
calcScrollUpdateArea oldOrigin newOrigin areaRect =
    (map rectToRectangle updArea,scroll newOriginAreaRect{rright=rright+1,rbottom=rbottom+1} restArea v)
    where
	newOriginAreaRect		= addVector (toVector newOrigin) areaRect
	Rect{rleft=rleft,rtop=rtop,rright=rright,rbottom=rbottom}	= newOriginAreaRect
	v				= toVector (oldOrigin-newOrigin)
	Vector2{vx=vx,vy=vy}		= v
	(updArea,restArea)		= 
		if vx<=0 && vy<=0 then
		    (	[newOriginAreaRect{rleft=rright+vx,rbottom=rbottom+vy},newOriginAreaRect{rtop=rbottom+vy}]
		    ,	 newOriginAreaRect{rright=rright+vx,rbottom=rbottom+vy}
		    )
		else if vx<=0 && vy>=0 then
		    (	[newOriginAreaRect{rbottom=rtop+vy},newOriginAreaRect{rleft=rright+vx,rtop=rtop+vy}]
		    ,	 newOriginAreaRect{rtop=rtop+vy,rright=rright+vx}
		    )
		else if vx>=0 && vy<=0 then
		    (	[newOriginAreaRect{rright=rleft+vx,rbottom=rbottom+vy},newOriginAreaRect{rtop=rbottom+vy}]
		    ,	 newOriginAreaRect{rleft=rleft+vx,rbottom=rbottom+vy}
		    )
		else
		    (	[newOriginAreaRect{rbottom=rtop+vy},newOriginAreaRect{rtop=rtop+vy,rright=rleft+vx}]
		    ,	 newOriginAreaRect{rleft=rleft+vx,rtop=rtop+vy}
		    )

	scroll :: Rect -> Rect -> Vector2 -> Draw [Rect]
	scroll scrollRect restRect v = do
		updRect <- pictScroll scrollRect v
		return (if updRect==zero then [] else [restRect])
