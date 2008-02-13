-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.WindowCCall_12
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- WindowCCall_12 contains C calls for handling windows. 
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.WindowCCall_12
		( winInvalidateWindow, winInvalidateRect
		, winValidateRect, winValidateRgn
		, winGetDC, winReleaseDC                      
		, module Graphics.UI.ObjectIO.OS.Types
		) where



import Graphics.UI.ObjectIO.OS.Cutil_12
import Graphics.UI.ObjectIO.OS.Types


foreign import ccall  "cCCallWindows_121.h WinInvalidateWindow" winInvalidateWindow :: OSWindowPtr -> IO ()

winInvalidateRect :: OSWindowPtr -> (Int,Int,Int,Int) -> IO ()
winInvalidateRect a1 (a2,a3,a4,a5)
	= cWinInvalidateRect a1 a2 a3 a4 a5
foreign import ccall "cCCallWindows_121.h WinInvalidateRect" cWinInvalidateRect :: OSWindowPtr -> Int -> Int -> Int -> Int -> IO ()

winValidateRect :: OSWindowPtr -> (Int,Int,Int,Int) -> IO ()
winValidateRect a1 (a2,a3,a4,a5)
	= cWinValidateRect a1 a2 a3 a4 a5
foreign import ccall "cCCallWindows_121.h WinValidateRect" cWinValidateRect :: OSWindowPtr -> Int -> Int -> Int -> Int -> IO ()


foreign import ccall "cCCallWindows_121.h WinValidateRgn" winValidateRgn :: OSWindowPtr -> OSRgnHandle -> IO ()
foreign import ccall "cpicture_121.h WinGetDC" winGetDC :: OSWindowPtr -> IO OSPictContext
foreign import ccall "cpicture_121.h WinReleaseDC" winReleaseDC :: OSWindowPtr -> OSPictContext -> IO ()