-----------------------------------------------------------------------------
-- |
-- Module      :  StdTimerDef
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdTimerDef contains the types to define the standard set of timers.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdTimerDef
		(
		-- * Definitions
		  module Graphics.UI.ObjectIO.StdTimerDef,
		-- * A visible modules
		  module Graphics.UI.ObjectIO.StdIOCommon
		, module Graphics.UI.ObjectIO.StdGUI
		) where


import	Graphics.UI.ObjectIO.StdIOCommon
import	Graphics.UI.ObjectIO.StdGUI

-- | Standard timer definition type
data Timer t ls ps = Timer TimerInterval (t ls ps) [TimerAttribute ls ps]

-- | Timer activation interval
type TimerInterval = Int

-- | Number of elapsed timer intervals
type NrOfIntervals = Int

data TimerAttribute ls ps					-- Default:
	= TimerFunction		(TimerFunction ls ps)		-- \_ x->x
	| TimerId		Id				-- no Id
	| TimerInit		(GUIFun ls ps)			-- no actions after opening timer
	| TimerSelectState	SelectState			-- timer Able

-- | Type of timer event handler. The argument of the handler
-- is the number of intervals elapsed between current and previous call.
type TimerFunction ls ps = NrOfIntervals -> GUIFun ls ps

