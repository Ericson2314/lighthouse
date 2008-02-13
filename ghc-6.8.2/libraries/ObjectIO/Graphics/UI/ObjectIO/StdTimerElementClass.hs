-----------------------------------------------------------------------------
-- |
-- Module      :  StdTimerElementClass
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Definition of the TimerElements class for timer elements.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdTimerElementClass where



import	Graphics.UI.ObjectIO.StdTimerDef
import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.Process.IOState
import	Graphics.UI.ObjectIO.Timer.Handle

-- | Translating timer elements into the internal representation.
-- There are instances for combinator types: 'AddLS', 'NewLS',
-- 'ListLS', 'NilLS' and 'TupLS'
class TimerElements t where
	timerElementToHandles	:: t ls ps -> GUI ps [TimerElementState ls ps]

instance TimerElements t => TimerElements (AddLS t) where
	timerElementToHandles (AddLS addLS addDef) = do
		ts <- timerElementToHandles addDef
		return [timerElementHandleToTimerElementState 
				(TimerExtendLSHandle addLS
						     (map timerElementStateToTimerElementHandle ts))
			]


instance TimerElements t => TimerElements (NewLS t) where
	timerElementToHandles (NewLS newLS newDef) = do
		ts <- timerElementToHandles newDef
		return [timerElementHandleToTimerElementState
				(TimerChangeLSHandle newLS
						     (map timerElementStateToTimerElementHandle ts))
			]


instance TimerElements t => TimerElements (ListLS t) where	
	timerElementToHandles (ListLS tDefs) = do
		tss <- mapM timerElementToHandles tDefs
		return [timerElementHandleToTimerElementState 
				(TimerListLSHandle (map timerElementStateToTimerElementHandle (concat tss)))
			]

instance TimerElements NilLS where	
	timerElementToHandles NilLS =
		return [timerElementHandleToTimerElementState (TimerListLSHandle [])]

instance (TimerElements t1, TimerElements t2) => TimerElements (TupLS t1 t2) where
	timerElementToHandles (t1:+:t2) = do
		ts1 <- timerElementToHandles t1
		ts2 <- timerElementToHandles t2
		return (ts1 ++ ts2)