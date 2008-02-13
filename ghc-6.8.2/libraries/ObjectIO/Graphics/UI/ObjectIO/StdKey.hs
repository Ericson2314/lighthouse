-----------------------------------------------------------------------------
-- |
-- Module      :  StdKey
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdKey defines the special keys for the Object I\/O library.
-- Keyboard event handlers receive 'KeyboardState' as a parameter.
-- When the special key (like F1, F2, PgUp, PgDn and other) is pressed, then 
-- the 'KeyboardState' contains a value of 'SpecialKey' type. Here is a list
-- of all predefined values.
--
-----------------------------------------------------------------------------



module Graphics.UI.ObjectIO.StdKey
              (	SpecialKey, 
		backSpaceKey, beginKey, 
		clearKey, 
		deleteKey, downKey, 
		endKey, enterKey, escapeKey, 
		f1Key,  f2Key,  f3Key,  f4Key,  f5Key,  
		f6Key,  f7Key,  f8Key,  f9Key,  f10Key,
		f11Key, f12Key, f13Key, f14Key, f15Key, 
		helpKey, 
		leftKey, 
		pgDownKey, pgUpKey, 
		rightKey, 
		upKey
              ) where



import Graphics.UI.ObjectIO.Key hiding (toSpecialKey, isSpecialKey)
