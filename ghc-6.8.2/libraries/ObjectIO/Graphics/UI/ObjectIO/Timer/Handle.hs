-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Timer.Handle
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--	
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Timer.Handle where


import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.Receiver.Handle
import	Graphics.UI.ObjectIO.StdTimerDef


type TimerElementState ls ps =							-- The internal implementation of a timer
	TimerElementHandle ls ps						-- is a TimerElementHandle

data TimerHandles ps
   = TimerHandles
   	{ tTimers	:: ![TimerStateHandle ps]				-- The timers of a process
	}
	
data TimerStateHandle ps
   = forall ls . TimerStateHandle (TimerLSHandle ls ps)			-- A timer with local state
   
data TimerLSHandle ls ps
   = TimerLSHandle
	{ tState	:: ls							-- The local state of this timer
	, tHandle	:: TimerHandle ls ps					-- The timer implementation
	}
	
data TimerHandle ls ps
   = TimerHandle
   	{ tId		:: !Id							-- The Id attribute or generated system Id of the timer
	, tSelect	:: !Bool						-- The TimerSelect==Able (by default True)
	, tPeriod	:: !Int							-- The interval time in ticks
	, tFun		:: !(TimerFunction ls ps)				-- The TimerFunction, optionally with local state
	, tItems	:: [TimerElementHandle ls ps]				-- The elements of the timer
	}

data TimerElementHandle ls ps
   	= TimerReceiverHandle
   		{ tReceiverHandle :: !(ReceiverHandle ls ps)
		, tReceiverAtts	  :: ![TimerAttribute ls ps]
		}
	| TimerListLSHandle	[TimerElementHandle	ls ps]
	| forall exLS .
	  TimerExtendLSHandle	exLS  [TimerElementHandle (exLS,ls) ps]
	| forall chLS .
	  TimerChangeLSHandle	chLS  [TimerElementHandle chLS ps]
	


--	Conversion functions from TimerElementState to TimerElementHandle, and vice versa:

timerElementHandleToTimerElementState :: TimerElementHandle ls ps -> TimerElementState ls ps
timerElementHandleToTimerElementState tHs = tHs

timerElementStateToTimerElementHandle :: TimerElementState ls ps -> TimerElementHandle ls ps
timerElementStateToTimerElementHandle tHs = tHs
