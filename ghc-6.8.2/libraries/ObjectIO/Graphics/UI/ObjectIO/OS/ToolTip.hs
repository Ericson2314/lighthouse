-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.ToolTip
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Control.Create contains all control creation functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.ToolTip(osRemoveControlToolTip, osAddControlToolTip) where


--	Operations to add and remove tooltip controls and areas.

{-	Tooltip controls are added and removed by OSaddControlTooltip and OSremoveControlTooltip.
	The first  OSWindowPtr argument identifies the parent window.
	The second OSWindowPtr argument identifies the control.
	The String argument is the tooltip text.
-}

import	Graphics.UI.ObjectIO.OS.ClCrossCall_12
import	Graphics.UI.ObjectIO.OS.Types(OSWindowPtr)
import  Graphics.UI.ObjectIO.OS.Cutil_12(addr2int, newCString, free)

osIgnoreCallback :: CrossCallInfo -> IO CrossCallInfo
osIgnoreCallback _
	= return return0Cci

osAddControlToolTip :: OSWindowPtr -> OSWindowPtr -> String -> IO ()
osAddControlToolTip parentPtr controlPtr tip = do
	textptr <- newCString tip
	let cci = rq3Cci ccRqADDCONTROLTIP parentPtr controlPtr (addr2int textptr)
	issueCleanRequest2 osIgnoreCallback cci
	free textptr
		

osRemoveControlToolTip :: OSWindowPtr -> OSWindowPtr -> IO ()
osRemoveControlToolTip parentPtr controlPtr = do
	issueCleanRequest2 osIgnoreCallback (rq2Cci ccRqDELCONTROLTIP parentPtr controlPtr)
	return ()
