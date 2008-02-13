-----------------------------------------------------------------------------
-- |
-- Module      :  StdSound
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdSound specifies sound playing functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdSound(playSoundFile) where


import 	Graphics.UI.ObjectIO.OS.ClCCall_12(winPlaySound)

playSoundFile :: String -> IO Bool
playSoundFile soundFileName = winPlaySound soundFileName
