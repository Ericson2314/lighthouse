-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.Bitmap
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------


module Graphics.UI.ObjectIO.OS.Bitmap where


import	Graphics.UI.ObjectIO.OS.Picture
import	Graphics.UI.ObjectIO.OS.Types
import  Graphics.UI.ObjectIO.OS.Cutil_12


data OSBitmap
   = OSBitmap
   	{ originalSize	:: !(Int,Int)		-- The size of the bitmap
	, reSize	:: !(Int,Int)		-- to store values passed to resizeBitmap
	, bitmapHandle	:: !OSBmpHandle		-- The handle to the screen bitmap (for screen)
	}

osReadBitmap :: FilePath -> IO (Maybe OSBitmap)
osReadBitmap name = do
	ptr <- newCString name
	pWidth <- malloc
	pHeight <- malloc
	hbmp <- winCreateBitmap ptr pWidth pHeight
	w <- fpeek pWidth
	h <- fpeek pHeight	
	free ptr
	(if hbmp == nullPtr then return Nothing
	 else return (Just (OSBitmap {originalSize=(w,h),reSize=(w,h),bitmapHandle=hbmp})))
foreign import ccall "cpicture_121.h WinCreateBitmap" winCreateBitmap :: CString -> Ptr Int -> Ptr Int -> IO OSBmpHandle

osDisposeBitmap :: OSBitmap -> IO ()
osDisposeBitmap (OSBitmap {bitmapHandle=handle}) = winDisposeBitmap handle
foreign import ccall "cpicture_121.h WinDisposeBitmap" winDisposeBitmap :: OSBmpHandle -> IO ()

-- osGetBitmapSize returns the size of the bitmap.
osGetBitmapSize :: OSBitmap -> (Int,Int)
osGetBitmapSize = reSize

{-	osResizeBitmap (w,h) bitmap
		resizes the argument bitmap to the given size.
	It is assumed that w and h are not negative.
-}
osResizeBitmap :: (Int,Int) -> OSBitmap -> OSBitmap
osResizeBitmap size bitmap = bitmap{reSize=size}

{-	osDrawBitmap bitmap pos origin pictContext
		draws the argument bitmap with the left top corner at pos, given the current origin and drawing context.
-}
osDrawBitmap :: OSBitmap -> (Int,Int) -> (Int,Int) -> Draw ()
osDrawBitmap (OSBitmap {originalSize=originalSize,reSize=reSize,bitmapHandle=bitmapHandle}) pos@(px,py) origin@(ox,oy) =
	Draw (\picture@(Picture {pictToScreen=isScreenOutput,pictContext=context}) ->
		(if originalSize==reSize then
		      winDrawBitmap  (fst originalSize) (snd originalSize) (px-ox) (py-oy) bitmapHandle context
		 else
		      winDrawResizedBitmap  (fst originalSize) (snd originalSize) (px-ox) (py-oy) (fst reSize) (snd reSize) bitmapHandle context) >>
		return ((), picture))
	where
		destination	= (px-ox,py-oy)
foreign import ccall "cpicture_121.h WinDrawBitmap" winDrawBitmap :: Int -> Int -> Int -> Int -> OSBmpHandle -> OSPictContext -> IO ()
foreign import ccall "cpicture_121.h WinDrawResizedBitmap" winDrawResizedBitmap :: Int -> Int -> Int -> Int -> Int -> Int -> OSBmpHandle -> OSPictContext -> IO ()
