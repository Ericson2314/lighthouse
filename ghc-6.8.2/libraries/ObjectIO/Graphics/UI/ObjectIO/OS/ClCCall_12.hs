-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.ClCCall_12
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- ClCCall_12 collects all the C call routines related to windows.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.ClCCall_12
		( ACCLPTR
		, winHelpKey, winEscapeKey, winReturnKey, winTabKey, winDelKey, winBackSpKey
		, winEndKey, winBeginKey, winPgDownKey, winPgUpKey, winRightKey, winLeftKey, winDownKey, winUpKey
		, winF1Key, winF2Key, winF3Key, winF4Key, winF5Key, winF6Key
		, winF7Key, winF8Key, winF9Key, winF10Key, winF11Key, winF12Key
		, ctrlBIT, altBIT, shiftBIT
		, keyREPEAT, keyUP, keyDOWN
		, buttonUP, buttonSTILLDOWN, buttonTRIPLEDOWN, buttonDOUBLEDOWN, buttonDOWN, buttonSTILLUP                  
		, winBeep
		, winMaxFixedWindowSize, winMaxScrollWindowSize
		, winScreenYSize, winScreenXSize
		, winMinimumWinSize
		, winScrollbarSize
		, winMDIClientToOuterSizeDims, winSDIClientToOuterSizeDims
		, winPlaySound
		) where


import Graphics.UI.ObjectIO.OS.Cutil_12
import System.IO.Unsafe


type	ACCLPTR		= Int

winHelpKey, winEscapeKey, winReturnKey, winTabKey, winDelKey, winBackSpKey, winEndKey, winBeginKey
	  , winPgDownKey, winPgUpKey, winRightKey, winLeftKey, winDownKey, winUpKey
	  , winF1Key, winF2Key, winF3Key, winF4Key, winF5Key, winF6Key, winF7Key
	  , winF8Key, winF9Key, winF10Key, winF11Key, winF12Key :: Int

winEscapeKey		= 27
winReturnKey		= 13
winTabKey		= 9
winBackSpKey		= 8
winF1Key		= 1001
winF2Key		= 1002
winF3Key		= 1003
winF4Key		= 1004
winF5Key		= 1005
winF6Key		= 1006
winF7Key		= 1007
winF8Key		= 1008
winF9Key		= 1009
winF10Key		= 1010
winF11Key		= 1011
winF12Key		= 1012
winHelpKey		= 1013
winDelKey		= 1014
winEndKey		= 1015
winBeginKey		= 1016
winPgDownKey		= 1017
winPgUpKey		= 1018
winRightKey		= 1019
winLeftKey		= 1020
winDownKey		= 1021
winUpKey		= 1022

ctrlBIT, altBIT, shiftBIT :: Int
ctrlBIT			= 4
altBIT			= 2
shiftBIT		= 1

keyREPEAT, keyUP, keyDOWN :: Int
keyREPEAT		= 4
keyUP			= 2
keyDOWN			= 1

buttonUP, buttonSTILLDOWN, buttonTRIPLEDOWN, buttonDOUBLEDOWN, buttonDOWN, buttonSTILLUP :: Int
buttonUP		= 50
buttonSTILLDOWN		= 40
buttonTRIPLEDOWN	= 3
buttonDOUBLEDOWN	= 2
buttonDOWN		= 1
buttonSTILLUP		= 0		{- constant for passing mouse move events. -}

foreign import ccall "cCCallSystem_121.h WinBeep" winBeep :: IO ()

winMaxFixedWindowSize :: (Int,Int)
winMaxFixedWindowSize
	= unsafePerformIO winMaxFixedWindowSize'
	where
		winMaxFixedWindowSize' :: IO (Int,Int)
		winMaxFixedWindowSize'
			= do {
				-- Marshal arguments:
				o1 <- malloc;
				o2 <- malloc;
				-- Call C:
				cWinMaxFixedWindowSize o1 o2;
				-- Read/free:
				r1 <- fpeek o1;
				r2 <- fpeek o2;
				return (r1,r2)
			  }
foreign import ccall "cCCallWindows_121.h WinMaxFixedWindowSize" cWinMaxFixedWindowSize :: Ptr Int -> Ptr Int -> IO ()

winMaxScrollWindowSize :: (Int,Int)
winMaxScrollWindowSize
	= unsafePerformIO winMaxScrollWindowSize'
	where
		winMaxScrollWindowSize' :: IO (Int,Int)
		winMaxScrollWindowSize'
			= do {
				-- Marshal arguments:
				o1 <- malloc;
				o2 <- malloc;
				-- Call C:
				cWinMaxScrollWindowSize o1 o2;
				-- Read/free:
				r1 <- fpeek o1;
				r2 <- fpeek o2;
				return (r1,r2)
			  }
foreign import ccall "cCCallWindows_121.h WinMaxScrollWindowSize" cWinMaxScrollWindowSize :: Ptr Int -> Ptr Int -> IO ()

foreign import ccall "cCCallWindows_121.h WinScreenYSize" winScreenYSize :: IO Int

foreign import ccall "cCCallWindows_121.h WinScreenXSize" winScreenXSize :: IO Int

winMinimumWinSize :: (Int,Int)
winMinimumWinSize
	= unsafePerformIO winMinimumWinSize'
	where
		winMinimumWinSize' :: IO (Int,Int)
		winMinimumWinSize'
			= do {
				-- Marshal arguments:
				o1 <- malloc;
				o2 <- malloc;
				-- Call C:
				cWinMinimumWinSize o1 o2;
				-- Read/free:
				r1 <- fpeek o1;
				r2 <- fpeek o2;
				return (r1, r2)
			  }
foreign import ccall "cCCallWindows_121.h WinMinimumWinSize" cWinMinimumWinSize :: Ptr Int -> Ptr Int -> IO ()

winScrollbarSize :: IO (Int,Int)
winScrollbarSize
	= do {
		-- Marshal arguments:
		o1 <- malloc;
		o2 <- malloc;
		-- Call C:
		cWinScrollbarSize o1 o2;
		-- Read/free:
		r1 <- fpeek o1;
		r2 <- fpeek o2;
		return (r1, r2)
	  }
foreign import ccall "cCCallWindows_121.h WinScrollbarSize" cWinScrollbarSize :: Ptr Int -> Ptr Int -> IO ()

{-	The routines (win(M/S)DIClientToOuterSizeDims convert between the
		client and outer size of (M/S)DI windows. The Int argument contains the style flags 
		of the window.
-}
winMDIClientToOuterSizeDims :: Int -> IO (Int,Int)
winMDIClientToOuterSizeDims a1
	= do {
		-- Marshal arguments:
		o1 <- malloc;
		o2 <- malloc;
		-- Call C:
		cWinMDIClientToOuterSizeDims a1 o1 o2;
		-- Read/free:
		r1 <- fpeek o1;
		r2 <- fpeek o2;
		return (r1, r2)
	  }
foreign import ccall "cCCallWindows_121.h WinMDIClientToOuterSizeDims" cWinMDIClientToOuterSizeDims :: Int -> Ptr Int -> Ptr Int -> IO ()

winSDIClientToOuterSizeDims :: Int -> IO (Int,Int)
winSDIClientToOuterSizeDims a1
	= do {
		-- Marshal arguments:
		o1 <- malloc;
		o2 <- malloc;
		-- Call C:
		cWinSDIClientToOuterSizeDims a1 o1 o2;
		-- Read/free:
		r1 <- fpeek o1;
		r2 <- fpeek o2;
		return (r1, r2)
	  }
foreign import ccall "cCCallWindows_121.h WinSDIClientToOuterSizeDims" cWinSDIClientToOuterSizeDims :: Int -> Ptr Int -> Ptr Int -> IO ()

winPlaySound :: String -> IO Bool
winPlaySound a1 = withCString a1 cWinPlaySound

foreign import ccall "cCCallSystem_121.h WinPlaySound" cWinPlaySound :: CString -> IO Bool
