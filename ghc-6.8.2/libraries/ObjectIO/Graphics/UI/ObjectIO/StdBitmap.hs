-----------------------------------------------------------------------------
-- |
-- Module      :  StdBitmap
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Interface functions for drawing bitmaps.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdBitmap where

import	Graphics.UI.ObjectIO.OS.Bitmap
import	Graphics.UI.ObjectIO.OS.Picture
import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.StdPicture

-- | An abstract type which represent the bitmap. There is instance of 'Drawables' class
type Bitmap = OSBitmap

-- | creates a bitmap from the given file name
openBitmap :: IOMonad m => FilePath -> m (Maybe Bitmap)
openBitmap name = liftIO (osReadBitmap name)

-- | Warning: Allways dispose of the bitmap when it is no longer needed
disposeBitmap :: IOMonad m => Bitmap -> m ()
disposeBitmap bmp = liftIO (osDisposeBitmap bmp)

-- | returns the current bitmap size
getBitmapSize :: Bitmap -> Size
getBitmapSize bitmap = fromTuple (osGetBitmapSize bitmap)	

-- | resizes the bitmap. The resizing of the bitmap affects only the data that is in the memory. The visual effect will appear when the bitmap is displayed
resizeBitmap :: Size -> Bitmap -> Bitmap
resizeBitmap size@(Size {w=w,h=h}) bitmap
	| w<0 || h<0 = error "resizeBitmap" "StdBitmap" "a Size record with negative components was passed"
	| otherwise  = osResizeBitmap (w,h) bitmap

instance Drawables Bitmap where
	draw bitmap = do		
		  penPos <- getPenPos
		  origin <- getPictOrigin
		  osDrawBitmap bitmap (toTuple penPos) (toTuple origin)
	
	drawAt pos bitmap = do
		origin <- getPictOrigin
		osDrawBitmap bitmap (toTuple pos) (toTuple origin)		
	
	undraw bitmap =
		unfill (Box {box_w=w,box_h=h})
		where
			(w,h)	= osGetBitmapSize bitmap
	
	undrawAt pos bitmap =
		unfillAt pos (Box {box_w=w,box_h=h})
		where
			(w,h)	= osGetBitmapSize bitmap
