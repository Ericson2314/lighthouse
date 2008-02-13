{-# OPTIONS_GHC -#include "cpicture_121.h" #-}

-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.Rgn
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- OS.Rgn contains OS operations to manage regions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.Rgn
		( OSRgnHandle
		, osNewRgn, osNewRectRgn, osDisposeRgn, osPolyRgn
		, osSectRgn, osUnionRgn, osDiffRgn, osXorRgn
		, osGetRgnBox, osIsEmptyRgn, osNoRgn
		) where



import Graphics.UI.ObjectIO.OS.Types
import Graphics.UI.ObjectIO.OS.Cutil_12
import Foreign.Ptr(Ptr(..), nullPtr)


--	Constants for drawing polygons.
alternate, winding :: Int
alternate	= 1
winding		= 2

--	Region creation and disposal operations.
foreign import ccall "cpicture_121.h WinCreateEmptyRgn" osNewRgn :: IO OSRgnHandle

osNewRectRgn :: Rect -> IO OSRgnHandle
osNewRectRgn (Rect {rleft=l,rtop=t,rright=r,rbottom=b}) = winCreateRectRgn l t r b
foreign import ccall "cpicture_121.h WinCreateRectRgn" winCreateRectRgn :: Int -> Int -> Int -> Int -> IO OSRgnHandle

osPolyRgn :: (Int,Int) -> [(Int,Int)] -> IO OSRgnHandle
osPolyRgn base shape
	= do {		
		if   len==0
		then osNewRgn
		else 
		do {
			shapeH <- winAllocPolyShape len;
			setPolyShape shapeH 0 base shape;
			prgn   <- winCreatePolygonRgn shapeH len winding;
			winFreePolyShape shapeH;
			return prgn
		}
	  }
	where
		len = length shape
		
		setPolyShape :: Ptr Int -> Int -> (Int,Int) -> [(Int,Int)] -> IO ()
		setPolyShape shapeH i (x,y) ((vx,vy):vs)
			= winSetPolyPoint i x y shapeH >> setPolyShape shapeH (i+1) (x+vx,y+vy) vs
		setPolyShape _ _ _ _ = return ()
foreign import ccall "cpicture_121.h WinAllocPolyShape" winAllocPolyShape :: Int -> IO (Ptr Int)
foreign import ccall "cpicture_121.h WinSetPolyPoint"   winSetPolyPoint   :: Int -> Int -> Int -> Ptr Int -> IO ()
foreign import ccall "cpicture_121.h WinFreePolyShape"  winFreePolyShape  :: Ptr Int -> IO ()
foreign import ccall "cpicture_121.h WinCreatePolygonRgn" winCreatePolygonRgn :: Ptr Int -> Int -> Int -> IO OSRgnHandle
		
foreign import ccall "cpicture_121.h WinDisposeRgn" osDisposeRgn :: OSRgnHandle -> IO ()


--	Combining the shapes of two Regions into a new Region.
foreign import ccall "cpicture_121.h WinSectRgn" osSectRgn :: OSRgnHandle -> OSRgnHandle -> IO OSRgnHandle
foreign import ccall "cpicture_121.h WinUnionRgn" osUnionRgn :: OSRgnHandle -> OSRgnHandle -> IO OSRgnHandle
foreign import ccall "cpicture_121.h WinDiffRgn" osDiffRgn :: OSRgnHandle -> OSRgnHandle -> IO OSRgnHandle
foreign import ccall "cpicture_121.h WinXorRgn" osXorRgn :: OSRgnHandle -> OSRgnHandle -> IO OSRgnHandle


--	Region property access functions.
osGetRgnBox :: OSRgnHandle -> IO (Bool,Rect)
osGetRgnBox rgn = do
	-- Marshal arguments:
	o1 <- malloc
	o2 <- malloc
	o3 <- malloc
	o4 <- malloc
	o5 <- malloc
	-- Call C:
	winGetRgnBox rgn o1 o2 o3 o4 o5
	-- Read/free:
	l <- fpeek o1
	t <- fpeek o2
	r <- fpeek o3
	b <- fpeek o4
    	isRect <- fpeek o5
	return (isRect,Rect {rleft=l,rtop=t,rright=r,rbottom=b})	
foreign import ccall "cpicture_121.h WinGetRgnBox" winGetRgnBox :: OSRgnHandle -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Bool -> IO ()
	
foreign import ccall "cpicture_121.h WinIsEmptyRgn" osIsEmptyRgn :: OSRgnHandle -> IO Bool
