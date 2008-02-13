-----------------------------------------------------------------------------
-- |
-- Module      :  StdProcess
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdProcess contains the process creation and manipulation functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdProcess
		( 
		-- * Definitions
		  Processes(..)
		, startIO, closeProcess
		, getProcessWindowPos, getProcessWindowSize,
		-- * A visible module
		  module Graphics.UI.ObjectIO.StdProcessDef
		) where


import Graphics.UI.ObjectIO.CommonDef (dumpFatalError, Rect(..), rectSize)
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.Process.Device
import Graphics.UI.ObjectIO.Process.Scheduler
import Graphics.UI.ObjectIO.StdProcessDef
import Graphics.UI.ObjectIO.OS.System(osGetProcessWindowDimensions)


stdprocessFatalError :: String -> String -> x
stdprocessFatalError function error
	= dumpFatalError function "StdProcess" error


-- | General process topology creation functions.
-- There are instances for 'Process' and Processes a => [a] types.

class Processes pdef where
	startProcesses :: pdef -> IO ()
	openProcesses  :: pdef -> GUI ps ()

instance Processes Process where
	startProcesses (Process xDI ps_init io_init atts)
		= do {
			initialContext  <- initContext atts io_init1 ps_init xDI;			
			(case initialContext of
				Just context -> handleEvents context >> closeContext context
				initFailed   -> stdprocessFatalError "startProcesses" "should not be evaluated inside GUI context");
		  }
		where
			io_init1 ps = dOpen processFunctions ps >>= io_init
	
	openProcesses (Process xDI ps_init io_init atts)
		= addInteractiveProcess atts io_init1 ps_init xDI
		where
			io_init1 ps = dOpen processFunctions ps >>= io_init


instance Processes pdef => Processes [pdef] where
	startProcesses []
		= return ()
	startProcesses pdefs
		= do {			
			initialContext  <- initContext [] io_init1 () NDI;
			(case initialContext of
				Just context -> handleEvents context >> closeContext context
				initFailed   -> stdprocessFatalError "startProcesses" "should not be evaluated inside GUI context");
		  }
		where
			io_init1 ps = dOpen processFunctions ps >>= (\ps1 -> openProcesses pdefs >> closeProcess ps1)

	openProcesses pdefs
		= sequence_ (map openProcesses pdefs)


--	Specialised process creation functions:

-- | Using of that function is the standard way for a single processed applications to run GUI.
startIO :: DocumentInterface -> ps -> ProcessInit ps -> [ProcessAttribute ps] -> IO ()
startIO xDI ps io_init atts
	= startProcesses (Process xDI ps io_init atts)


--	Close this interactive process.

-- | The function terminates the current process.
closeProcess :: ps -> GUI ps ps
closeProcess ps = quitProcess ps

-- | Get the current position of the ProcessWindow (on Macintosh: zero).
getProcessWindowPos :: GUI ps Point2
getProcessWindowPos = do	
	osdinfo <- accIOEnv ioStGetOSDInfo
	rect <- liftIO (osGetProcessWindowDimensions osdinfo)
	return (Point2{x=rleft rect,y=rtop rect})

-- | Get the current size of the ProcessWindow (on Macintosh: ScreenSize).
getProcessWindowSize :: GUI ps Size
getProcessWindowSize = do
	osdinfo <- accIOEnv ioStGetOSDInfo
	rect <- liftIO (osGetProcessWindowDimensions osdinfo)	
	return (rectSize rect)