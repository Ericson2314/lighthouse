-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Window.SDISize
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Window.SDISize(getSDIWindowSize, resizeSDIWindow) where



import	Graphics.UI.ObjectIO.Process.IOState
import	Graphics.UI.ObjectIO.Window.Access
import	Graphics.UI.ObjectIO.Window.Update
import	Graphics.UI.ObjectIO.CommonDef(dumpFatalError)
import	Graphics.UI.ObjectIO.OS.Window
import 	Graphics.UI.ObjectIO.OS.Types

sdiSizeFatalError :: String -> String -> x
sdiSizeFatalError rule error = dumpFatalError rule "Window.SDISize" error


--	getSDIWindowSize retrieves the current size of the WindowViewFrame if this is a SDI process
getSDIWindowSize :: GUI ps (Size,OSWindowPtr)
getSDIWindowSize = do
	osdInfo <- accIOEnv ioStGetOSDInfo	
	let wPtr  = case getOSDInfoOSInfo osdInfo of
		Just info -> osFrame info
		_         -> osNoWindowPtr
	(if getOSDInfoDocumentInterface osdInfo /= SDI then return (zero,wPtr)
	 else do
		-- PA: here we have to use osGetWindowViewFrameSize, because it is the only reliable way to determine proper viewframe size. 
		(w,h) <- liftIO (osGetWindowViewFrameSize wPtr)
		return (Size{w=w,h=h},wPtr))

{-	resizeSDIWindow wPtr oldviewframesize newviewframesize
		resizes the SDI window so the viewframe does not change in size. 
		oldviewframesize is the size of the ViewFrame(!) before the menu/toolbar was created.
		newviewframesize is the size of the ViewFrame(!) after  the menu/toolbar was created.
		Note that:
			h oldviewframesize /= h newviewframesize && w oldviewframesize == w newviewframesize
-}
resizeSDIWindow :: OSWindowPtr -> Size -> Size -> GUI ps ()
resizeSDIWindow wPtr (Size{h=oldHeight}) newFrameSize@(Size {h=newHeight}) = do {
	osdInfo <- accIOEnv ioStGetOSDInfo;
	if getOSDInfoDocumentInterface osdInfo /= SDI then sdiSizeFatalError "resizeSDIWindow" "not an SDI process"
	else 
		let (framePtr,clientPtr) = case getOSDInfoOSInfo osdInfo of
	  		Just osinfo -> (osFrame osinfo,osClient osinfo)
	  		_           -> (osNoWindowPtr,osNoWindowPtr)
		in
		    if wPtr /= framePtr then sdiSizeFatalError "resizeSDIWindow" "SDIWindow frame could not be located"	
		    else do {
		    		(oldw,oldh) <- liftIO (osGetWindowSize framePtr);
				liftIO (osSetWindowSize framePtr (oldw,oldh+oldHeight-newHeight) True);
				if newHeight > oldHeight then return ()	-- menus take up less space
				else do {
					(ok,wDevice) <- accIOEnv (ioStGetDevice WindowDevice);
					if not ok then return ()
					else 
						let 	windows = windowSystemStateGetWindowHandles wDevice
	  					    	(found,wsH,windows1) = getWindowHandlesWindow (toWID clientPtr) windows
	  					in 
							if not found then appIOEnv (ioStSetDevice (WindowSystemState windows1))
							else do	{
								wMetrics <- accIOEnv ioStGetOSWindowMetrics;
								wsH <- liftIO (updateSDIWindow wMetrics oldHeight newFrameSize wsH);
								let windows2 = setWindowHandlesWindow wsH windows1
								in appIOEnv (ioStSetDevice (WindowSystemState windows2));
							};
				};
			};
	}		
	where
		-- Note that oldH > h newSize
		updateSDIWindow :: OSWindowMetrics -> Int -> Size -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		updateSDIWindow wMetrics oldH newSize (WindowStateHandle wids@(WIDS {wPtr=wPtr}) (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
			wH <- updateWindow wMetrics updateInfo wH		-- Update the background
			wH <- updateRectControls wMetrics newArea wPtr wH	-- Update the controls
			return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
			where			
				newArea		= zero{rright=w newSize,rbottom=oldH}
				updateInfo	= UpdateInfo 
							{ updWIDS	= wids
							, updWindowArea	= newArea
							, updControls	= []
							, updGContext	= Nothing
							}
		updateSDIWindow _ _ _ _ = sdiSizeFatalError "updateSDIWindow" "unexpected window placeholder"
