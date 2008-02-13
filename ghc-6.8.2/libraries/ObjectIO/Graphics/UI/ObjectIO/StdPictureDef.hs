-----------------------------------------------------------------------------
-- |
-- Module      :  StdPictureDef
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdPictureDef contains the predefined figures that can be drawn.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdPictureDef(module Graphics.UI.ObjectIO.StdPictureDef, Font(..)) where


import Graphics.UI.ObjectIO.StdIOBasic
import Graphics.UI.ObjectIO.OS.Font (Font(..))


data	Line2						-- A line connects two points
	= Line2
		{ line_end1       :: !Point2		-- The first  point
		, line_end2       :: !Point2		-- The second point
		}
data	Box						-- A box is a rectangle
	= Box
		{ box_w           :: !Int		-- The width  of the box
		, box_h           :: !Int		-- The height of the box
		}
data	Oval						-- An oval is a stretched unit circle
	= Oval
		{ oval_rx         :: !Int		-- The horizontal radius (stretch)
		, oval_ry         :: !Int		-- The vertical   radius (stretch)
		}
data	Curve						-- A curve is a slice of an oval
	= Curve
		{ curve_oval      :: !Oval		-- The source oval
		, curve_from      :: !Float		-- Starting angle (in radians)
		, curve_to        :: !Float		-- Ending   angle (in radians)
		, curve_clockwise :: !Bool		-- Direction: True iff clockwise
		}
data	Polygon						-- A polygon is an outline shape
	= Polygon
		{ polygon_shape   :: ![Vector2]		-- The shape of the polygon
		}
data	FontDef
	= FontDef
		{ fName           :: !FontName		-- Name of the font
		, fStyles         :: ![FontStyle]	-- Stylistic variations
		, fSize           :: !FontSize		-- Size in points
		}
data	FontMetrics
	= FontMetrics
		{ fAscent         :: !Int		-- Distance between top    and base line
		, fDescent        :: !Int		-- Distance between bottom and base line
		, fLeading        :: !Int		-- Distance between two text lines
		, fMaxWidth       :: !Int		-- Max character width including spacing
		}
		
type	FontName  = String
type	FontStyle = String
type	FontSize  = Int

data	Colour
	= RGB 
	    { r	:: !Int				-- The contribution of red
	    , g	:: !Int				-- The contribution of green
	    , b	:: !Int				-- The contribution of blue
	    } deriving Eq	

data	PenAttribute					-- Default:
	= PenSize	Int				-- 1
	| PenPos	Point2				-- zero
	| PenColour	Colour				-- Black
	| PenBack	Colour				-- White
	| PenFont	Font				-- defaultFont

--	Colour constants:
minRGB			= 0   :: Int
maxRGB			= 255 :: Int

black     = RGB {r=minRGB,g=minRGB,b=minRGB} :: Colour
darkGrey  = RGB {r=round((fromIntegral maxRGB)/4.0), g=round((fromIntegral maxRGB)/4.0), b=round((fromIntegral maxRGB)/4.0)} :: Colour
grey      = RGB {r=round((fromIntegral maxRGB)/2.0), g=round((fromIntegral maxRGB)/2.0), b=round((fromIntegral maxRGB)/2.0)} :: Colour
lightGrey = RGB {r=round((fromIntegral maxRGB)*0.75),g=round((fromIntegral maxRGB)*0.75),b=round((fromIntegral maxRGB)*0.75)} :: Colour
white     = RGB {r=maxRGB,g=maxRGB,b=maxRGB} :: Colour
red       = RGB {r=maxRGB,g=minRGB,b=minRGB} :: Colour
green     = RGB {r=minRGB,g=maxRGB,b=minRGB} :: Colour
blue      = RGB {r=minRGB,g=minRGB,b=maxRGB} :: Colour
cyan      = RGB {r=minRGB,g=maxRGB,b=maxRGB} :: Colour
magenta   = RGB {r=maxRGB,g=minRGB,b=maxRGB} :: Colour
yellow    = RGB {r=maxRGB,g=maxRGB,b=minRGB} :: Colour


--	Standard lineheight of a font is the sum of its leading, ascent and descent:
fontLineHeight fMetrics	= fLeading fMetrics + fAscent fMetrics + fDescent fMetrics
