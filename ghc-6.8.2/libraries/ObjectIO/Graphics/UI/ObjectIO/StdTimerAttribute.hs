-----------------------------------------------------------------------------
-- |
-- Module      :  StdTimerAttribute
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdTimerAttribute specifies which TimerAttributes are valid for each of 
-- the standard timers. Basic comparison operations and retrieval functions 
-- are also included.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdTimerAttribute
		( isValidTimerAttribute
		, isTimerFunction
		, isTimerId
		, isTimerInit
		, isTimerSelectState
		, getTimerFun
		, getTimerIdAtt
		, getTimerInitFun
		, getTimerSelectStateAtt
		, module Graphics.UI.ObjectIO.StdTimerDef
		) where


import Graphics.UI.ObjectIO.StdTimerDef
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.Id

{-	The following functions specify the valid attributes for each standard timer. -}


isValidTimerAttribute :: TimerAttribute ls ps -> Bool
isValidTimerAttribute _ = True


isTimerFunction	:: TimerAttribute ls ps -> Bool
isTimerFunction	(TimerFunction _)	= True
isTimerFunction	_			= False

isTimerId	:: TimerAttribute ls ps -> Bool
isTimerId	(TimerId _)		= True
isTimerId	_			= False

isTimerInit	:: TimerAttribute ls ps -> Bool
isTimerInit	(TimerInit _)		= True
isTimerInit	_			= False

isTimerSelectState	:: TimerAttribute ls ps -> Bool
isTimerSelectState	(TimerSelectState _)	= True
isTimerSelectState	_			= False

getTimerFun :: TimerAttribute ls ps -> TimerFunction ls ps
getTimerFun (TimerFunction f) = f

getTimerIdAtt :: TimerAttribute ls ps -> Id
getTimerIdAtt (TimerId id) = id

getTimerInitFun :: TimerAttribute ls ps -> GUIFun ls ps
getTimerInitFun (TimerInit f) = f

getTimerSelectStateAtt :: TimerAttribute ls ps -> SelectState
getTimerSelectStateAtt (TimerSelectState s) = s
