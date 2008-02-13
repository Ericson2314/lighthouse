-----------------------------------------------------------------------------
-- |
-- Module      :  StdFileSelect
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdFileSelect(selectInputFile, selectOutputFile, selectDirectory) where



import Graphics.UI.ObjectIO.Process.Scheduler
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.OS.FileSelect
import Graphics.UI.ObjectIO.OS.Event

-- | The selectInputFile function opens a dialog for file selecting and returns the selected file name.
-- If the file doesn\'t exist, the function shows a popup message box with a warning message.
selectInputFile :: ps -> GUI ps (Maybe String, ps)
selectInputFile ps = do
	context <- accIOEnv ioStGetContext
	liftContextIO (\context -> osSelectInputFile (handleOSEvent context)) ps


-- | The selectOutputFile function opens a dialog for file selecting and returns the selected file name.
-- If the file already exists, the function shows a popup message box with a warning message.
selectOutputFile :: String -> String -> ps -> GUI ps (Maybe String, ps)
selectOutputFile prompt filename ps = do
	context <- accIOEnv ioStGetContext
	liftContextIO (\context -> osSelectOutputFile (handleOSEvent context) prompt filename) ps

-- | The selectDirectory opens a dialog for directory selecting.
selectDirectory :: ps -> GUI ps (Maybe String, ps)
selectDirectory ps = do
	context <- accIOEnv ioStGetContext
	liftContextIO (\context -> osSelectDirectory (handleOSEvent context)) ps

handleOSEvent :: Context -> OSEvent -> IO ()
handleOSEvent context osEvent = do
	handleContextOSEvent context (ScheduleOSEvent osEvent [])
	return ()
