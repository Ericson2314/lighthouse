{-# OPTIONS_GHC -#include "cpicture_121.h" #-}

-- #hide
-----------------------------------------------------------------------------
-- Module      :  OS.Picture
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- OS.Picture contains drawing functions and other operations on Pictures.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.Picture
		( Picture(..), Origin, OSPictContext, Pen(..), Graphics.UI.ObjectIO.OS.Font.Font(..)
		, peekOSPictContext
		, pictGetStringWidth, pictGetCharWidth
		, defaultPen, dialogPen, setPenAttribute
		, Draw(..), doDraw, doScreenDraw
		, getPictOrigin, setPictOrigin, getPictPen, setPictPen, setPictPenPos, getPictPenPos
		, movePictPenPos, setPictPenSize, getPictPenSize, setPictPenColour, setPictBackColour 
		, getPictPenColour, getPictBackColour, setPictPenFont, getPictPenFont, setPictPenDefaultFont 
		, setPictXorMode, setPictHiliteMode, setPictNormalMode, pictDrawPoint
		, pictDrawLine, pictUndrawLine, pictDrawChar, pictUndrawChar, pictDrawString, pictUndrawString 
		, pictDrawOval, pictUndrawOval, pictFillOval, pictUnfillOval, pictDrawCurve, pictUndrawCurve 
		, pictFillCurve, pictUnfillCurve, pictDrawRect, pictUndrawRect, pictFillRect, pictUnfillRect 
		, pictScroll, pictDrawPolygon, pictUndrawPolygon, pictFillPolygon, pictUnfillPolygon, pictGetClipRgn 
		, pictSetClipRgn, pictAndClipRgn
		, module Graphics.UI.ObjectIO.StdPictureDef
		, getResolutionC, getPictureScalingFactors
		) where


import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.StdIOBasic
import Graphics.UI.ObjectIO.StdPictureDef
import Graphics.UI.ObjectIO.OS.Types
import Graphics.UI.ObjectIO.OS.Rgn
import Graphics.UI.ObjectIO.OS.Font
import Graphics.UI.ObjectIO.OS.Cutil_12
import Control.Monad(when)

data	Picture
	= Picture
		{ pictContext   :: !OSPictContext	-- The context for drawing operations
		, pictOrigin    :: !Origin		-- The current origin of the picture
		, pictPen       :: !Pen			-- The current state of the pen
		, pictToScreen  :: !Bool		-- Flag: the output goes to screen (True) or printer (False)
		}
		
type	Origin = Point2

data  Pen
	= Pen
		{ penSize       :: !Int			-- The width and height of the pen
  		, penForeColour :: !Colour		-- The drawing colour of the pen
		, penBackColour :: !Colour		-- The background colour of the pen
		, penPos        :: !Point2		-- The pen position in local coordinates
		, penFont       :: !Font		-- The font information to draw text and characters
		}


iModeNotBic, iModeNotXor, iModeNotOr, iModeNotCopy, iModeBic, iModeXor, iModeOr, iModeCopy :: Int
iModeNotBic	= 7
iModeNotXor	= 6
iModeNotOr	= 5
iModeNotCopy	= 4
iModeBic	= 3
iModeXor	= 2
iModeOr		= 1
iModeCopy	= 0


pictGetStringWidth :: String -> Draw Int
pictGetStringWidth s = Draw (\picture@(Picture {pictContext=context}) -> do	
	w <- withCString s (\s -> cWinGetPicStringWidth s context)
	return (w, picture))
	
foreign import ccall "cpicture_121.h WinGetPicStringWidth" cWinGetPicStringWidth :: CString -> OSPictContext -> IO Int

pictGetCharWidth :: Char -> Draw Int
pictGetCharWidth c = Draw (\picture@(Picture {pictContext=context}) -> do
	w <- cWinGetPicCharWidth c context
	return (w, picture))
	
foreign import ccall "cpicture_121.h WinGetPicCharWidth" cWinGetPicCharWidth :: Char -> OSPictContext -> IO Int	
		
--	Conversion operations to and from Picture

peekOSPictContext :: Draw OSPictContext
peekOSPictContext = Draw (\picture ->
	return (pictContext picture,picture))
	
defaultPen :: Pen
defaultPen
	= Pen
		{ penSize       = 1
		, penForeColour = black
		, penBackColour = white
		, penPos        = Point2 {x=0,y=0}
		, penFont       = defaultFont
		}

dialogPen :: Pen
dialogPen
	= Pen
		{ penSize       = 1
		, penForeColour = black
		, penBackColour = white
		, penPos        = Point2 {x=0,y=0}
		, penFont       = dialogFont
		}

setPenAttribute :: PenAttribute -> Pen -> Pen
setPenAttribute (PenSize   size)   pen = pen {penSize      =max 1 size}
setPenAttribute (PenPos    pos)    pen = pen {penPos       =pos       }
setPenAttribute (PenColour colour) pen = pen {penForeColour=colour    }
setPenAttribute (PenBack   colour) pen = pen {penBackColour=colour    }
setPenAttribute (PenFont   font)   pen = pen {penFont      =font      }


-- Drawing monads

-- | While drawing we need the current state called 'Picture' state.
-- The functions that are used for drawing need a 'Picture' state as an argument and return the new updated state.
-- The Draw monad is defined so that it would be easier for the user to write the functions.
-- The Draw monad is in fact an IO monad, but it also takes care of the management of the 'Picture' state.
-- The definition of Draw is abstract.
newtype Draw a = Draw (Picture -> IO (a, Picture))

instance Monad Draw where
   return x = Draw (\pict -> return (x, pict))
   (Draw f) >>= g = Draw (\pict -> do
   	(x, pict) <- f pict
   	let (Draw f2) = g x
   	f2 pict)
   	
instance  Functor Draw where
   fmap f x = x >>= (return . f)

instance IOMonad Draw where
	liftIO f = Draw (\picture -> f >>= \x -> return (x, picture))

doDraw :: Origin -> Pen -> Bool -> OSRgnHandle -> OSPictContext -> Bool -> Draw a -> IO (a, Origin, Pen, Bool)
doDraw origin pen@(Pen {penSize=penSize,penForeColour=penForeColour,penBackColour=penBackColour,penFont=penFont}) isScreenOutput clipRgn context isBuffered (Draw drawf) = do
    let (fname,fstyles,fsize) = osFontGetImp penFont
    withCString fname (\fontname -> winInitPicture
	      penSize
	      iModeCopy
	      (r penForeColour) (g penForeColour) (b penForeColour)
	      (r penBackColour) (g penBackColour) (b penBackColour)
	      fontname
	      fstyles
	      fsize
	      clipRgn
	      isBuffered
	      context)
    (res, picture) <- drawf (Picture
	    { pictContext = context
	    , pictOrigin  = origin
	    , pictPen     = pen
	    , pictToScreen= isScreenOutput	    
	    }
	   )
    winDonePicture context
    return (res, pictOrigin picture,pictPen picture,pictToScreen picture)
foreign import ccall "cpicture_121.h WinInitPicture" winInitPicture :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> CString -> Int -> Int -> OSRgnHandle -> Bool -> OSPictContext -> IO ()
foreign import ccall "cpicture_121.h WinDonePicture" winDonePicture :: OSPictContext -> IO ()
		
doScreenDraw :: Draw x -> IO x
doScreenDraw f = do
	hdc <- winCreateScreenHDC
	(x,_,_,_) <- doDraw zero defaultPen True osNoRgn hdc False f	
	winDestroyScreenHDC hdc
	return x
foreign import ccall "cpicture_121.h WinCreateScreenHDC"  winCreateScreenHDC  :: IO OSPictContext
foreign import ccall "cpicture_121.h WinDestroyScreenHDC" winDestroyScreenHDC :: OSPictContext -> IO ()


-- Attribute functions.

--	Access to Origin and Pen:
getPictOrigin :: Draw Origin
getPictOrigin = Draw (\picture -> return (pictOrigin picture,picture))

setPictOrigin :: Origin -> Draw ()
setPictOrigin origin = Draw (\picture -> return ((), picture{pictOrigin=origin}))

getPictPen :: Draw Pen
getPictPen = Draw (\picture -> return (pictPen picture,picture))

setPictPen :: Pen -> Draw ()
setPictPen pen = do
	setPictPenSize    (penSize pen)
	setPictPenColour  (penForeColour pen)
	setPictBackColour (penBackColour pen)
	setPictPenPos     (penPos pen)
	setPictPenFont    (penFont pen)


--	Change the pen position:
setPictPenPos :: Point2 -> Draw ()
setPictPenPos newpos = Draw (\picture@Picture{pictOrigin=origin,pictPen=pen@(Pen {penPos=pos}),pictContext=context} ->
	return ((), picture{pictPen=pen{penPos=newpos}}))

getPictPenPos :: Draw Point2
getPictPenPos = Draw (\picture -> return (penPos (pictPen picture),picture))

movePictPenPos :: Vector2 -> Draw ()
movePictPenPos v@(Vector2{vx=vx,vy=vy}) = Draw (\picture@(Picture{pictPen=pen@(Pen{penPos=Point2{x=x,y=y}}),pictContext=context}) ->
	return ((), picture{pictPen=pen{penPos=Point2{x=x+vx,y=y+vy}}}))

--	Change the pen size:
setPictPenSize :: Int -> Draw ()
setPictPenSize w = Draw (\picture@(Picture {pictContext=context,pictPen=pen}) ->
	let w' = max 1 w
	in  if w' == penSize pen then return ((), picture)
	    else do
		    winSetPenSize w' context		  
		    return ((), picture{pictPen=pen{penSize=w'}}))
		    
foreign import ccall "cpicture_121.h WinSetPenSize" winSetPenSize :: Int -> OSPictContext -> IO ()


getPictPenSize :: Draw Int
getPictPenSize = Draw (\picture -> return (penSize (pictPen picture),picture))


--	Change the PenColour:
setPictPenColour :: Colour -> Draw ()
setPictPenColour colour = Draw (\picture@(Picture {pictPen=pen,pictContext=context}) ->	
    if colour == penForeColour pen then return ((), picture)
    else do
	    winSetPenColor (r colour) (g colour) (b colour) context
	    return ((), picture{pictPen=pen{penForeColour=colour}}))
		    
foreign import ccall "cpicture_121.h WinSetPenColor" winSetPenColor :: Int -> Int -> Int -> OSPictContext -> IO ()
	

setPictBackColour :: Colour -> Draw ()
setPictBackColour colour = Draw (\picture@(Picture {pictPen=pen,pictContext=context}) ->
     if colour == penBackColour pen then return ((), picture)
     else do
	     winSetBackColor (r colour) (g colour) (b colour) context
	     return ((), picture{pictPen=pen{penBackColour=colour}}))
	     
foreign import ccall "cpicture_121.h WinSetBackColor" winSetBackColor :: Int -> Int -> Int -> OSPictContext -> IO ()
	

getPictPenColour :: Draw Colour
getPictPenColour = Draw (\picture -> return (penForeColour (pictPen picture), picture))

getPictBackColour :: Draw Colour
getPictBackColour = Draw (\picture -> return (penBackColour (pictPen picture), picture))


--	Change the font attributes:
setPictPenFont :: Font -> Draw ()
setPictPenFont font = Draw (\picture@(Picture{pictContext=context,pictPen=pen}) ->
	let (fname,fstyles,fsize) = osFontGetImp font
	in
	  if font == penFont pen then return ((), picture)
	  else do
		  withCString fname (\s -> winSetFont s fstyles fsize context)
		  return ((), picture{pictPen=pen{penFont=font}}))
		  
getPictPenFont :: Draw Font
getPictPenFont = Draw (\picture -> return (penFont (pictPen picture),picture))

setPictPenDefaultFont :: Draw ()
setPictPenDefaultFont = Draw (\picture@(Picture{pictContext=context,pictPen=pen}) -> do
	let (fname,fstyles,fsize) = osFontGetImp dialogFont
	withCString fname (\s -> winSetFont s fstyles fsize context)
	return ((), picture{pictPen=pen{penFont=dialogFont}}))
	
foreign import ccall "cpicture_121.h WinSetFont" winSetFont :: CString -> Int -> Int -> OSPictContext -> IO ()


--	Drawing mode setting functions.

setPictXorMode :: Draw ()
setPictXorMode = Draw (\picture@(Picture {pictContext=context}) -> do
	winSetMode iModeXor context
	return ((), picture))
	
setPictHiliteMode :: Draw ()
setPictHiliteMode = Draw (\picture@(Picture {pictContext=context}) -> do
	winSetMode iModeXor context
	return ((), picture))
	
setPictNormalMode :: Draw ()
setPictNormalMode = Draw (\picture@(Picture {pictContext=context}) -> do
	winSetMode iModeCopy context
	return ((), picture))
	
foreign import ccall "cpicture_121.h WinSetMode" winSetMode :: Int -> OSPictContext -> IO ()


{-	Point2 drawing operations.
	pictDrawPoint
		only draws a point at that position. The pen position is not changed.
-}
pictDrawPoint :: Point2 -> Draw ()
pictDrawPoint pos = Draw (\picture@(Picture{pictPen=pen,pictOrigin=origin,pictContext=context}) ->
	let psize = penSize pen		
	    Point2 x' y' = (pos-origin)
	in do
	   (if psize==1
	    then winDrawPoint x' y'
	    else winFillRectangle x' y' (x'+psize) (y'+psize)) context
	   return ((), picture))
foreign import ccall "cpicture_121.h WinDrawPoint" winDrawPoint :: Int -> Int -> OSPictContext -> IO ()

{-	Line drawing operations.
	pictDrawLine
		draws a line from the first point to the second point. The pen position
		is not changed.
-}

pictDrawLine :: Point2 -> Point2 -> Draw ()
pictDrawLine a b = Draw (\picture@(Picture {pictContext=context,pictOrigin=origin}) -> do
	let Point2 ax ay = (a-origin)
	let Point2 bx by = (b-origin)
	winDrawLine ax ay bx by context
	return ((), picture))
foreign import ccall "cpicture_121.h WinDrawLine" winDrawLine :: Int -> Int -> Int -> Int -> OSPictContext -> IO ()

pictUndrawLine :: Point2 -> Point2 -> Draw ()
pictUndrawLine a b = Draw (\picture@(Picture {pictContext=context,pictOrigin=origin}) -> do
	let Point2 ax ay = (a-origin)
	let Point2 bx by = (b-origin)
	winUndrawLine ax ay bx by context
	return ((), picture))
foreign import ccall "cpicture_121.h WinUndrawLine" winUndrawLine :: Int -> Int -> Int -> Int -> OSPictContext -> IO ()

{-	Text drawing operations.
	pictDraw(char/string) draws a char/string at the current pen position. The new
		pen position is immediately after the drawn char/string.
-}
pictDrawChar :: Point2 -> Char -> Draw ()
pictDrawChar pos char = Draw (\picture@(Picture {pictContext=context,pictOrigin=origin}) -> do
	let Point2 x y = (pos-origin)
	winDrawChar x y char context
	return ((), picture))	
foreign import ccall "cpicture_121.h WinDrawChar" winDrawChar :: Int -> Int -> Char -> OSPictContext -> IO ()

pictUndrawChar :: Point2 -> Char -> Draw ()
pictUndrawChar pos char = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let Point2 x y = (pos-origin)
	winUndrawChar x y char context
	return ((), picture))	
foreign import ccall "cpicture_121.h WinUndrawChar" winUndrawChar :: Int -> Int -> Char -> OSPictContext -> IO ()

pictDrawString :: Point2 -> String -> Draw ()
pictDrawString pos string = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let Point2 x y = (pos-origin)
	withCString string (\s -> winDrawString x y s context)
	return ((), picture))	
foreign import ccall "cpicture_121.h WinDrawString" winDrawString :: Int -> Int -> CString -> OSPictContext -> IO ()

pictUndrawString :: Point2 -> String -> Draw ()
pictUndrawString pos string = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let Point2 x y = (pos-origin)
	withCString string (\s -> winUndrawString x y s context)
	return ((), picture))
foreign import ccall "cpicture_121.h WinUndrawString" winUndrawString :: Int -> Int -> CString -> OSPictContext -> IO ()


{-	Oval drawing operations.
	pict(Draw/Fill)oval center oval 
		draws/fills an oval at center with horizontal and vertical radius. The new
		pen position is not changed.
-}
pictDrawOval :: Point2 -> Oval -> Draw ()
pictDrawOval center oval = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let Rect{rleft=l,rtop=t,rright=r,rbottom=b} = ovalToRect (center-origin) oval
	winDrawOval l t r b context
	return ((), picture))
foreign import ccall "cpicture_121.h WinDrawOval" winDrawOval :: Int -> Int -> Int -> Int -> OSPictContext -> IO ()


pictUndrawOval :: Point2 -> Oval -> Draw ()
pictUndrawOval center oval = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let Rect{rleft=l,rtop=t,rright=r,rbottom=b} = ovalToRect (center-origin) oval
	winUndrawOval l t r b context
	return ((), picture))
foreign import ccall "cpicture_121.h WinUndrawOval" winUndrawOval :: Int -> Int -> Int -> Int -> OSPictContext -> IO ()


pictFillOval :: Point2 -> Oval -> Draw ()
pictFillOval center oval = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let Rect{rleft=l,rtop=t,rright=r,rbottom=b} = ovalToRect (center-origin) oval
	winFillOval l t r b context
	return ((), picture))
foreign import ccall "cpicture_121.h WinFillOval" winFillOval :: Int -> Int -> Int -> Int -> OSPictContext -> IO ()


pictUnfillOval :: Point2 -> Oval -> Draw ()
pictUnfillOval center oval = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	let Rect{rleft=l,rtop=t,rright=r,rbottom=b} = ovalToRect (center-origin) oval
	winUnfillOval l t r b context
	return ((), picture))
foreign import ccall "cpicture_121.h WinEraseOval" winUnfillOval :: Int -> Int -> Int -> Int -> OSPictContext -> IO ()


ovalToRect :: Point2 -> Oval -> Rect
ovalToRect (Point2{x=x,y=y}) (Oval{oval_rx=oval_rx,oval_ry=oval_ry}) =
	Rect{rleft=x-rx,rtop=y-ry,rright=x+rx,rbottom=y+ry}
	where
	   rx = abs oval_rx
	   ry = abs oval_ry


{-	Curve drawing operations.
	pict(Draw/Fill)curve movePen point curve
		draws/fills a curve starting at point with a shape defined by curve. If movePen
		is True, then the new pen position is at the end of the curve, otherwise it does
		not change.
-}
pictDrawCurve :: Point2 -> Curve -> Draw ()
pictDrawCurve start curve = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let Point2 x y = start-origin
	let oval = curve_oval curve
	winDrawCurve x y (oval_rx oval) (oval_ry oval) (curve_from curve) (curve_to curve) (curve_clockwise curve) context
	return ((), picture))
foreign import ccall "cpicture_121.h WinDrawCurve" winDrawCurve :: Int -> Int -> Int -> Int -> Float -> Float -> Bool -> OSPictContext -> IO ()


pictUndrawCurve :: Point2 -> Curve -> Draw ()
pictUndrawCurve start curve = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let Point2 x y = start-origin
	let oval = curve_oval curve
	winUndrawCurve x y (oval_rx oval) (oval_ry oval) (curve_from curve) (curve_to curve) (curve_clockwise curve) context
	return ((), picture))
foreign import ccall "cpicture_121.h WinUndrawCurve" winUndrawCurve :: Int -> Int -> Int -> Int -> Float -> Float -> Bool -> OSPictContext -> IO ()
	

pictFillCurve :: Point2 -> Curve -> Draw ()
pictFillCurve start curve = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let Point2 x y = start-origin
	let oval = curve_oval curve
	winFillWedge x y (oval_rx oval) (oval_ry oval) (curve_from curve) (curve_to curve) (curve_clockwise curve) context
	return ((), picture))
foreign import ccall "cpicture_121.h WinFillWedge" winFillWedge :: Int -> Int -> Int -> Int -> Float -> Float -> Bool -> OSPictContext -> IO ()
	

pictUnfillCurve :: Point2 -> Curve -> Draw ()
pictUnfillCurve start curve = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	let Point2 x y = start-origin
	let oval = curve_oval curve
	winEraseWedge x y (oval_rx oval) (oval_ry oval) (curve_from curve) (curve_to curve) (curve_clockwise curve) context
	return ((), picture))
foreign import ccall "cpicture_121.h WinEraseWedge" winEraseWedge :: Int -> Int -> Int -> Int -> Float -> Float -> Bool -> OSPictContext -> IO ()
	

{-	Rect drawing operations.
	pict(draw/fill)rect rect
		draws/fills a rect. The pen position is not changed.
-}
pictDrawRect :: Rect -> Draw ()
pictDrawRect rect = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let (Rect {rleft=l,rtop=t,rright=r,rbottom=b}) = subVector (toVector origin) rect
	winDrawRectangle l t r b context
	return ((), picture))
foreign import ccall "cpicture_121.h WinDrawRectangle" winDrawRectangle :: Int -> Int -> Int -> Int -> OSPictContext -> IO ()

pictUndrawRect :: Rect -> Draw ()
pictUndrawRect rect = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	let (Rect {rleft=l,rtop=t,rright=r,rbottom=b}) = subVector (toVector origin) rect
	winUndrawRectangle l t r b context
	return ((), picture))
foreign import ccall "cpicture_121.h WinUndrawRectangle" winUndrawRectangle :: Int -> Int -> Int -> Int -> OSPictContext -> IO ()


pictFillRect :: Rect -> Draw ()
pictFillRect rect = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let (Rect {rleft=l,rtop=t,rright=r,rbottom=b}) = subVector (toVector origin) rect
	winFillRectangle l t r b context
	return ((), picture))
foreign import ccall "cpicture_121.h WinFillRectangle" winFillRectangle :: Int -> Int -> Int -> Int -> OSPictContext -> IO ()

pictUnfillRect :: Rect -> Draw ()
pictUnfillRect rect = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let (Rect {rleft=l,rtop=t,rright=r,rbottom=b}) = subVector (toVector origin) rect
	winEraseRectangle l t r b context
	return ((), picture))
foreign import ccall "cpicture_121.h WinEraseRectangle" winEraseRectangle :: Int -> Int -> Int -> Int -> OSPictContext -> IO ()



{-	Scrolling operation (handle with care).
-}
pictScroll :: Rect -> Vector2 -> Draw Rect
pictScroll rect v = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let (Rect {rleft=l,rtop=t,rright=r,rbottom=b}) = subVector (toVector origin) rect
	let Vector2 vx vy = v
	-- Marshal arguments:
	o1 <- malloc;
	o2 <- malloc;
	o3 <- malloc;
	o4 <- malloc;
	-- Call C:
	winScrollRectangle l t r b vx vy context o1 o2 o3 o4;
	-- Read/free:
	l <- fpeek o1;
	t <- fpeek o2;
	r <- fpeek o3;
	b <- fpeek o4;	
	return (Rect {rleft=l,rtop=t,rright=r,rbottom=b}, picture))
foreign import ccall "cpicture_121.h WinScrollRectangle" winScrollRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> OSPictContext -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()

{-	Polygon drawing operations.
	pict(Draw/Fill)polygon point polygon
		draws/fills a polygon starting at point. The pen position is not changed.
-}
pictDrawPolygon :: Point2 -> Polygon -> Draw ()
pictDrawPolygon start (Polygon{polygon_shape=shape}) = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	transferPolygon (start-origin) shape
	winDrawPolygon context
	winEndPolygon
	return ((), picture))
foreign import ccall "cpicture_121.h WinDrawPolygon"   winDrawPolygon   :: OSPictContext -> IO ()

pictUndrawPolygon :: Point2 -> Polygon -> Draw ()
pictUndrawPolygon start (Polygon{polygon_shape=shape}) = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	transferPolygon (start-origin) shape	
	winUndrawPolygon context
	winEndPolygon
	return ((), picture))
foreign import ccall "cpicture_121.h WinUndrawPolygon" winUndrawPolygon :: OSPictContext -> IO ()

pictFillPolygon :: Point2 -> Polygon -> Draw ()
pictFillPolygon start (Polygon{polygon_shape=shape}) = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	transferPolygon (start-origin) shape	
	winFillPolygon context
	winEndPolygon
	return ((), picture))
foreign import ccall "cpicture_121.h WinFillPolygon"   winFillPolygon   :: OSPictContext -> IO ()

pictUnfillPolygon :: Point2 -> Polygon -> Draw ()
pictUnfillPolygon start (Polygon{polygon_shape=shape}) = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	transferPolygon (start-origin) shape
	winErasePolygon  context
	winEndPolygon
	return ((), picture))
foreign import ccall "cpicture_121.h WinErasePolygon"  winErasePolygon  :: OSPictContext -> IO ()

transferPolygon :: Point2 -> [Vector2] -> IO ()
transferPolygon start vs = do
	winStartPolygon (1 + length vs)
	winAddPolygonPoint x y
	transferShape x y vs
	where
	  Point2 x y = start

	  transferShape :: Int -> Int -> [Vector2] -> IO ()
	  transferShape x y ((Vector2 {vx=vx,vy=vy}):vs) = do
	  	winAddPolygonPoint new_x new_y
	  	transferShape new_x new_y vs
	  	where
	  		new_x = x+vx
	  		new_y = y+vy
	  transferShape _ _ [] = return ()

foreign import ccall "cpicture_121.h WinStartPolygon"    winStartPolygon :: Int -> IO ()
foreign import ccall "cpicture_121.h WinAddPolygonPoint" winAddPolygonPoint :: Int -> Int -> IO ()
foreign import ccall "cpicture_121.h WinEndPolygon"      winEndPolygon :: IO ()

{-	Clipping operations.
	pictGetClipRgn gets the current clipping region.
	pictSetClipRgn sets the given clipping region.
	pictAndClipRgn takes the intersection of the current clipping region and the argument region.
-}
--	Operation to retrieve the current clipping region.
pictGetClipRgn :: Draw OSRgnHandle
pictGetClipRgn = Draw (\picture@(Picture{pictContext=context}) -> do
	clipRgn <- winGetClipRgnPicture context
	return (clipRgn, picture))
foreign import ccall "cpicture_121.h WinGetClipRgnPicture" winGetClipRgnPicture :: OSPictContext -> IO OSRgnHandle

--	Operation to set the complete clipping region.
pictSetClipRgn :: OSRgnHandle -> Draw ()
pictSetClipRgn clipRgn = Draw (\picture@(Picture{pictContext=context}) -> do
	winSetClipRgnPicture clipRgn context
	return ((), picture))
foreign import ccall "cpicture_121.h WinSetClipRgnPicture" winSetClipRgnPicture :: OSRgnHandle -> OSPictContext -> IO ()

--	Operation to set the clipping region.
pictAndClipRgn :: OSRgnHandle -> Draw ()
pictAndClipRgn clipRgn = Draw (\picture@(Picture{pictContext=context}) -> do
	winClipRgnPicture clipRgn context
	return ((), picture))
foreign import ccall "cpicture_121.h WinClipRgnPicture" winClipRgnPicture :: OSRgnHandle -> OSPictContext -> IO ()

getResolutionC :: OSPictContext -> IO (Int, Int)
getResolutionC a1 = do
	o1 <- malloc
	o2 <- malloc
	cgetResolutionC a1 o1 o2
	r1 <- fpeek o1
	r2 <- fpeek o2
	return (r1, r2)

foreign import ccall "cpicture_121.h getResolutionC" cgetResolutionC :: OSPictContext -> Ptr Int -> Ptr Int -> IO ()


getPictureScalingFactors :: OSPictContext -> IO ((Int, Int), (Int, Int))
getPictureScalingFactors a1 = do
	o1 <- malloc
	o2 <- malloc
	o3 <- malloc
	o4 <- malloc
	cWinGetPictureScaleFactor a1 o1 o2 o3 o4
	r1 <- fpeek o1
	r2 <- fpeek o2
	r3 <- fpeek o3
	r4 <- fpeek o4
	return ((r1, r2), (r3, r4))
	
foreign import ccall "cpicture_121.h WinGetPictureScaleFactor" cWinGetPictureScaleFactor :: OSPictContext -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()