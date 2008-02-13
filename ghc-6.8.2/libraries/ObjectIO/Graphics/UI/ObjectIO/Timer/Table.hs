-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Timer.Table
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--	
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Timer.Table where


import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.SystemId
import Graphics.UI.ObjectIO.Device.Types
import Graphics.UI.ObjectIO.StdTimerDef


type TimerTable = [TimerTableEntry]				-- The currently active timers
data TimerTableEntry
   = TimerTableEntry
	{ tteInterval	:: !Int					-- The TimerInterval of the positive timer
	, tteElapse	:: !Int					-- The elapsed timer interval (may be negative)
	, tteLoc	:: !TimerLoc				-- The location of the positive timer
	}
data TimerLoc
   = TimerLoc
   	{ tlIOId	:: !SystemId				-- Id of parent process
	, tlDevice	:: !Device				-- Device kind of parent
	, tlParentId	:: !Id					-- Id of parent device instance
	, tlTimerId	:: !Id					-- Id of the timer itself
	} deriving Eq
data TimerEvent
   = TimerEvent
   	{ teLoc		:: !TimerLoc				-- The timer that should be evaluated
	, teNrInterval  :: !NrOfIntervals			-- The nr of timer intervals that have elapsed
	}

initialTimerTable :: TimerTable					-- initialTimerTable yields an empty TimerTable
initialTimerTable = []

{-	addTimerToTimerTable adds a new timer entry to the TimerTable.
	The Boolean result is True iff no duplicate timer entry was found, otherwise it is False.
	The TimerInterval argument is set to zero if it less than zero. 
-}
addTimerToTimerTable :: TimerLoc -> TimerInterval -> TimerTable -> TimerTable
addTimerToTimerTable loc interval timers = add loc (max 0 interval) timers
	where
		entry = TimerTableEntry {tteInterval=interval,tteElapse=interval,tteLoc=loc}
		
		add :: TimerLoc -> TimerInterval -> [TimerTableEntry] -> [TimerTableEntry]
		add loc interval (tte:ttes)
			| loc == tteLoc tte = entry : ttes
			| otherwise         = tte : add loc interval ttes
		add loc interval []         = [entry]

{-	removeTimerFromTimerTable removes a timer from the TimerTable. -}
removeTimerFromTimerTable :: TimerLoc -> TimerTable -> TimerTable
removeTimerFromTimerTable loc timers = filter (\tte -> tteLoc tte /= loc) timers

{-	setIntervalInTimerTable changes the timerinterval of the given timer in the TimerTable.
	If the timer was not present in the table, then nothing happens (the Boolean result is False).
	If the timer was present, its entry has been changed (the Boolean result is True).
-}
setIntervalInTimerTable :: TimerLoc -> TimerInterval -> TimerTable -> (Bool,TimerTable)
setIntervalInTimerTable loc interval timers = set loc (max 0 interval) timers
	where
		set :: TimerLoc -> TimerInterval -> [TimerTableEntry] -> (Bool,[TimerTableEntry])
		set loc interval (tte:ttes)
			| loc==tteLoc tte =
				let tte1 = if interval == 0 then tte{tteInterval=interval,tteElapse=0} else tte{tteInterval=interval}
				in (True,tte:ttes)
			| otherwise =
				let (found,ttes1) = set loc interval ttes
				in (found,tte:ttes1)
		set _ _ ttes = (False,ttes)

{-	shiftTimeInTimerTable dt shifts the TimerTable dt (>0) ticks forward in time.  -}
shiftTimeInTimerTable :: Int -> TimerTable -> TimerTable
shiftTimeInTimerTable dt timers
	| dt<=0     = timers
	| otherwise = shiftTimes dt timers
	where
	  shiftTimes :: Int -> [TimerTableEntry] -> [TimerTableEntry]
	  shiftTimes dt (tte@(TimerTableEntry {tteInterval=tteInterval,tteElapse=tteElapse}) : ttes)
		  | tteInterval==0 = tte : shiftTimes dt ttes
		  | otherwise = tte{tteElapse=tteElapse-dt} : shiftTimes dt ttes
	  shiftTimes _ ttes = ttes

{-	getActiveTimerInTimerTable determines the next timer that should be evaluated given the current
	TimerTable. Such a timer is any timer with a negative or zero elapsed time. 
	If such a timer could be found, then getActiveTimerInTimerTable returns its timer location and 
		number of fully elapsed timer intervals. The timer in question is placed behind all further
		timers, creating a round-robin evaluation order.
	If such a timer could not be found, then Nothing is returned. 
-}
getActiveTimerInTimerTable :: TimerTable -> (Maybe TimerEvent,TimerTable)
getActiveTimerInTimerTable (tte@(TimerTableEntry {tteElapse=tteElapse,tteInterval=tteInterval,tteLoc=tteLoc}) : ttes)
	| tteElapse <= 0 =
	 	let
	 		nrTimeInterval	= if tteInterval==0 then 1 else (abs tteElapse) `div` tteInterval+1
			tEvent		= TimerEvent {teLoc=tteLoc,teNrInterval=nrTimeInterval}
			tte'		= tte{tteElapse=tteElapse+nrTimeInterval*tteInterval}
		in (Just tEvent,ttes++[tte'])
	| otherwise =
		let (active,ttes') = getActiveTimerInTimerTable ttes
		in (active,tte:ttes')
getActiveTimerInTimerTable _ = (Nothing,[])

{-	getTimeIntervalFromTimerTable returns the (Just time) interval that can be waited for the next timer to
	become active.
	If there are no timers, then Nothing is returned.
-}
getTimeIntervalFromTimerTable :: TimerTable -> Maybe Int
getTimeIntervalFromTimerTable [] = Nothing
getTimeIntervalFromTimerTable timers = Just (getSleepTime (2^31-1) timers)
	where
		getSleepTime :: Int -> [TimerTableEntry] -> Int
		getSleepTime sleep (tte@(TimerTableEntry {tteElapse=tteElapse}):ttes)
			| tteElapse<=0 = 0
			| otherwise    = getSleepTime (min sleep tteElapse) ttes
		getSleepTime sleep _ = sleep
