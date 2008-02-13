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
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Process.Device (processFunctions) where


import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.StdProcessAttribute
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.Process.Toolbar
import Graphics.UI.ObjectIO.OS.DocumentInterface
import Graphics.UI.ObjectIO.OS.ProcessEvent


processdeviceFatalError :: String -> String -> x
processdeviceFatalError rule error
	= dumpFatalError rule "ProcessDevice" error


processFunctions :: DeviceFunctions ps
processFunctions
	= DeviceFunctions
		{ dDevice = ProcessDevice
		, dShow	  = return
		, dHide	  = return
		, dEvent  = processEvent
		, dDoIO   = processIO
		, dOpen   = processOpen
		, dClose  = processClose
		}

processOpen :: ps -> GUI ps ps
processOpen ps
	= do {
		hasProcess <- accIOEnv (ioStHasDevice ProcessDevice);
		if   hasProcess
		then return ps
		else 
		do {
			appIOEnv (ioStSetDeviceFunctions processFunctions);
			osdinfo <- accIOEnv ioStGetOSDInfo;
			createOSDInfo osdinfo;
			return ps
		   }
	     }
	where
		createOSDInfo :: OSDInfo -> GUI ps ()
		createOSDInfo emptyOSDInfo
			| di==NDI = appIOEnv (ioStSetOSDInfo emptyOSDInfo)
			| di==MDI = do
				atts <- accIOEnv ioStGetProcessAttributes
				let acceptOpenFiles	= any isProcessOpenFiles atts
				let hasToolbarAtt	= any isProcessToolbar   atts
				osdinfo <- liftIO (osOpenMDI (not hasToolbarAtt) acceptOpenFiles)
				appIOEnv (ioStSetOSDInfo osdinfo)
				openToolbar
			| di==SDI = do
				atts <- accIOEnv ioStGetProcessAttributes
				let acceptOpenFiles = any isProcessOpenFiles atts					
				osdinfo <- liftIO (osOpenSDI acceptOpenFiles)
				appIOEnv (ioStSetOSDInfo osdinfo)
				openToolbar
			where
				di = getOSDInfoDocumentInterface emptyOSDInfo

processClose :: ps -> GUI ps ps
processClose ps
	= do {
		accIOEnv (ioStGetDevice ProcessDevice);
		appIOEnv (ioStRemoveDeviceFunctions ProcessDevice);
		return ps
	  }

processIO :: DeviceEvent -> ps -> GUI ps ps

{-	ProcessInitialise is the first event sent to an interactive process.
	This allows the system to evaluate its initialisation action. No further
	actions are required.
-}
processIO ProcessInitialise ps
	= return ps

processIO ProcessRequestClose ps
	= do {
		atts                 <- accIOEnv ioStGetProcessAttributes;
		let (hasCloseAtt,att) = cselect isProcessClose undefined atts
		in
		if   not hasCloseAtt
		then return ps
		else getProcessCloseFun att ps
	  }
processIO (ProcessRequestOpenFiles openFilesInfo) ps = do
	atts                 <- accIOEnv ioStGetProcessAttributes;
	let (hasFilesOpenAtt,att) = cselect isProcessOpenFiles undefined atts
	(if not hasFilesOpenAtt
	 then return ps
	 else getProcessOpenFilesFun att openFilesInfo ps)

processIO _ _
	= processdeviceFatalError "processIO" "unexpected DeviceEvent"
