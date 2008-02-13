-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Process.Toolbar
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Process.Toolbar(openToolbar) where


import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.Window.SDISize
import Graphics.UI.ObjectIO.StdGUI
import Graphics.UI.ObjectIO.StdProcessAttribute
import Graphics.UI.ObjectIO.StdBitmap(getBitmapSize)
import Graphics.UI.ObjectIO.OS.Bitmap
import Graphics.UI.ObjectIO.OS.DocumentInterface
import Graphics.UI.ObjectIO.OS.ToolBar
import Graphics.UI.ObjectIO.OS.Types


toolbarFatalError :: String -> String -> a
toolbarFatalError function error = dumpFatalError function "Toolbar" error

checkToolbarFatalError :: Bool -> String -> String -> GUI ps ()
checkToolbarFatalError False function error = return ()
checkToolbarFatalError True  function error = dumpFatalError function "Toolbar" error


openToolbar :: GUI ps ()
openToolbar = do
	osdInfo <- accIOEnv ioStGetOSDInfo
	let di = getOSDInfoDocumentInterface osdInfo
	(if di == NDI
	 then return ()
	 else do
		atts <- accIOEnv ioStGetProcessAttributes
		let (hasToolbarAtt,toolbarAtt) = cselect isProcessToolbar undefined atts
		let toolbar = getProcessToolbarAtt toolbarAtt
		(if not hasToolbarAtt
		 then return ()
		 else (if di==SDI then openSDIToolbar else openMDIToolbar) toolbar osdInfo))
	where
		openSDIToolbar :: [ToolbarItem ps] -> OSDInfo -> GUI ps ()
		openSDIToolbar items osdInfo = do
			checkToolbarFatalError (isJust osToolbar) "openSDIToolbar" "toolbar already present"
			(oldSize,_) <- getSDIWindowSize		
			(tbPtr,tbHeight) <- liftIO (osCreateToolbar False osFrame (toTuple reqSize))
			checkToolbarFatalError (tbPtr==osNoWindowPtr) "openSDIToolbar" "toolbar could not be created"
			liftIO (foldrM (openToolbarItem tbPtr) 1 items)
			let ostoolbar = OSToolbar{toolbarPtr=tbPtr,toolbarHeight=tbHeight}
			let osinfo1   = osinfo{osToolbar=Just ostoolbar}			
			appIOEnv (ioStSetOSDInfo (setOSDInfoOSInfo osinfo1 osdInfo))
			resizeSDIWindow osFrame oldSize oldSize{h=(h oldSize)-tbHeight}
			where
				reqSize	= getBitmapsSize items
				osinfo	= case (getOSDInfoOSInfo osdInfo) of
						Just info -> info
						Nothing   -> toolbarFatalError "openSDIToolbar" "could not retrieve OSInfo from OSDInfo"
				OSInfo{osFrame=osFrame,osToolbar=osToolbar} = osinfo
	
		openMDIToolbar :: [ToolbarItem ps] -> OSDInfo -> GUI ps ()
		openMDIToolbar items osdInfo = do
			checkToolbarFatalError (isJust osToolbar) "openMDIToolbar" "toolbar already present"
			(tbPtr,tbHeight) <- liftIO (osCreateToolbar True osFrame (toTuple reqSize))
			checkToolbarFatalError (tbPtr == osNoWindowPtr) "openMDIToolbar" "toolbar could not be created"
			liftIO (foldrM (openToolbarItem tbPtr) 1 items)
			let ostoolbar = OSToolbar{toolbarPtr=tbPtr,toolbarHeight=tbHeight}
			let osinfo1 = osinfo{osToolbar=Just ostoolbar}
			appIOEnv (ioStSetOSDInfo (setOSDInfoOSInfo osinfo1 osdInfo))
			where
				reqSize	= getBitmapsSize items
				osinfo	= case (getOSDInfoOSInfo osdInfo) of
						Just info -> info
						Nothing   -> toolbarFatalError "openMDIToolbar" "could not retrieve OSInfo from OSDInfo"
				OSInfo{osFrame=osFrame,osToolbar=osToolbar} = osinfo

		openToolbarItem	:: OSToolbarHandle -> ToolbarItem ps -> Int -> IO Int
		openToolbarItem tbPtr (ToolbarItem bitmap tooltip _) index = do
			osCreateBitmapToolbarItem tbPtr bitmap index
			return (index+1)
		openToolbarItem tbPtr ToolbarSeparator index = do
			osCreateToolbarSeparator tbPtr
			return index

getBitmapsSize :: [ToolbarItem ps] -> Size
getBitmapsSize items =
	foldr maxBitmapSize (Size {w=osDefaultToolbarHeight,h=osDefaultToolbarHeight}) items
	where
		maxBitmapSize :: (ToolbarItem ps) -> Size -> Size
		maxBitmapSize item size = Size{w=max (w itemsize) (w size),h=max (h itemsize) (h size)}
			where
				itemsize = case item of
					ToolbarItem bitmap _ _	-> getBitmapSize bitmap
					_			-> zero
