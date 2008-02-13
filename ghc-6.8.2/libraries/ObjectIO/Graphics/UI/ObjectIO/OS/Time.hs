-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.Time
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.Time where


type OSTime = Int

osMaxTime :: OSTime
osMaxTime = 2^31-1

foreign import ccall "cCCallSystem_121.h WinGetTickCount" osGetTime ::  IO OSTime
foreign import ccall "cCCallSystem_121.h WinGetBlinkTime" osGetBlinkInterval :: IO Int