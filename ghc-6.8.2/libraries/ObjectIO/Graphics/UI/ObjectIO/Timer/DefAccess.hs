-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Timer.DefAccess
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--	
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Timer.DefAccess where


import	Graphics.UI.ObjectIO.StdTimerAttribute
import	Graphics.UI.ObjectIO.CommonDef


timerDefGetAttributes :: Timer t ls ps -> [TimerAttribute ls ps]
timerDefGetAttributes (Timer _ _ atts) = atts

timerDefGetElements :: Timer t ls ps -> t ls ps
timerDefGetElements (Timer _ items _) = items

timerDefSetAbility	:: SelectState -> Timer t ls ps -> Timer t ls ps
timerDefSetAbility select (Timer interval items atts) = Timer interval items (setAbility select atts)
	where
		setAbility :: SelectState -> [TimerAttribute ls ps] -> [TimerAttribute ls ps]
		setAbility select atts
			| done		= atts1
			| otherwise	= att:atts
			where
				att			= TimerSelectState select
				(done,atts1)= creplace isTimerSelectState att atts

timerDefSetInterval	:: TimerInterval -> Timer t ls ps -> Timer t ls ps
timerDefSetInterval interval (Timer _ items atts) = Timer interval items atts

timerDefSetFunction	:: TimerFunction ls ps -> Timer t ls ps -> Timer t ls ps
timerDefSetFunction f (Timer interval items atts) = Timer interval items (setFunction f atts)
	where
		setFunction :: TimerFunction ls ps -> [TimerAttribute ls ps] -> [TimerAttribute ls ps]
		setFunction f atts
			| done		= atts1
			| otherwise	= att:atts
			where
				att		= TimerFunction f
				(done,atts1)	= creplace isTimerFunction att atts
