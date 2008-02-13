{-# OPTIONS_GHC -#include "cCCallWindows_121.h" #-}

-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.System
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- OS.System defines system dependent functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.System where


import Graphics.UI.ObjectIO.OS.ClCCall_12
import Graphics.UI.ObjectIO.OS.ClCrossCall_12
import Graphics.UI.ObjectIO.OS.DocumentInterface
import Graphics.UI.ObjectIO.OS.Font
import Graphics.UI.ObjectIO.OS.Types(Rect(..))
import Graphics.UI.ObjectIO.OS.WindowCrossCall_12
import Data.Maybe


data	OSWindowMetrics
	= OSWindowMetrics
		{ osmFont          :: Font		-- The internal Font used in Windows for controls
		, osmFontMetrics   :: (Int,Int,Int)	-- The ascent, descent, leading of osmFont
		, osmHeight        :: Int		-- The height of the internal Font
		, osmHorMargin     :: Int		-- The default horizontal margin
		, osmVerMargin     :: Int		-- The default vertical   margin
		, osmHorItemSpace  :: Int		-- The default horizontal item space
		, osmVerItemSpace  :: Int		-- The default vertical   item space
		, osmHSliderHeight :: Int		-- The default height of a horizontal slider control
		, osmVSliderWidth  :: Int		-- The default width  of a vertical   slider control
		}


osNewLineChars = "\r\n"					-- OS newline convention

osTicksPerSecond = 1000	:: Int				-- OS max resolution of ticks per second

foreign import ccall "cpicture_121.h OsMMtoHPixels" osMMtoHPixels :: Float -> Int
foreign import ccall "cpicture_121.h OsMMtoVPixels" osMMtoVPixels :: Float -> Int

osMaxScrollWindowSize :: (Int,Int)
osMaxScrollWindowSize = winMaxScrollWindowSize

osMaxFixedWindowSize :: (Int,Int)
osMaxFixedWindowSize = winMaxFixedWindowSize

osScreenRect :: IO Rect
osScreenRect
	= do {
		screenWidth  <- winScreenXSize;
		screenHeight <- winScreenYSize;
		return (Rect {rleft=0,rtop=0,rright=screenWidth,rbottom=screenHeight})
	  }
	  
osPrintSetupTypical :: Bool -- MW11++
osPrintSetupTypical = False

osGetProcessWindowDimensions :: OSDInfo -> IO Rect
osGetProcessWindowDimensions osdinfo
	| isNothing maybeOSInfo
		= osScreenRect
	| otherwise
		= let osinfo = fromJust maybeOSInfo
		  in  do {
		  		(x,y) <- winGetWindowPos  (osFrame  osinfo);
				(w,h) <- winGetClientSize (osClient osinfo);
				return (Rect {rleft=x,rtop=y,rright=x+w,rbottom=y+h})
		      }
	where
		maybeOSInfo = getOSDInfoOSInfo osdinfo

osDefaultWindowMetrics :: IO OSWindowMetrics
osDefaultWindowMetrics
	= do {
		(ascent,descent,leading,_) <- osGetFontMetrics Nothing dialogFont;
		(scrollWidth,scrollHeight) <- winScrollbarSize;
		let
			height              = ascent+descent+leading
			unit                = (fromIntegral height)/8.0
			margin              = round (unit*7.0)
			itemspace           = round (unit*4.0)
		in return (OSWindowMetrics
				{ osmFont          = dialogFont
				, osmFontMetrics   = (ascent,descent,leading)
				, osmHeight        = height
				, osmHorMargin     = margin
				, osmVerMargin     = margin
				, osmHorItemSpace  = itemspace
				, osmVerItemSpace  = itemspace
				, osmHSliderHeight = scrollHeight
				, osmVSliderWidth  = scrollWidth
				}
			  )
	  }

{-	osStripOuterSize isMDI isResizable (width,height)
		returns (dw,dh) required to add/subtract to view size/outer size in order to obtain
		outer size/view size.
-}
osStripOuterSize :: Bool -> Bool -> IO (Int,Int)
osStripOuterSize isMDI isResizable
	| isMDI
		= winMDIClientToOuterSizeDims styleFlags
	| otherwise
		= winSDIClientToOuterSizeDims styleFlags
	where
		styleFlags = if isResizable then ws_THICKFRAME else 0
