-----------------------------------------------------------------------------
-- |
-- Module      :  ObjectIO
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- ObjectIO contains all definition modules of the Object I\/O library.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO
		( module Graphics.UI.ObjectIO.StdId
             	, module Graphics.UI.ObjectIO.StdIOBasic
             	, module Graphics.UI.ObjectIO.StdIOCommon
             	, module Graphics.UI.ObjectIO.StdKey
             	, module Graphics.UI.ObjectIO.StdGUI
             	, module Graphics.UI.ObjectIO.StdPicture
             	, module Graphics.UI.ObjectIO.StdBitmap
             	, module Graphics.UI.ObjectIO.StdProcess
             	, module Graphics.UI.ObjectIO.StdControl             	
	     	, module Graphics.UI.ObjectIO.StdControlReceiver
             	, module Graphics.UI.ObjectIO.StdReceiver
             	, module Graphics.UI.ObjectIO.StdWindow
             	, module Graphics.UI.ObjectIO.StdMenu
             	, module Graphics.UI.ObjectIO.StdMenuReceiver
             	, module Graphics.UI.ObjectIO.StdMenuElement
             	, module Graphics.UI.ObjectIO.StdTimer
             	, module Graphics.UI.ObjectIO.StdTimerReceiver
             	, module Graphics.UI.ObjectIO.StdFileSelect
             	, module Graphics.UI.ObjectIO.StdSound
             	, module Graphics.UI.ObjectIO.StdClipboard
             	, module Graphics.UI.ObjectIO.StdSystem
             	) where

import Graphics.UI.ObjectIO.StdId		-- The operations that generate identification values
import Graphics.UI.ObjectIO.StdIOBasic		-- Function and type definitions used in the library
import Graphics.UI.ObjectIO.StdIOCommon		-- Function and type definitions used in the library
import Graphics.UI.ObjectIO.StdKey		-- Function and type definitions on keyboard
import Graphics.UI.ObjectIO.StdGUI		-- Type definitions used in the library

import Graphics.UI.ObjectIO.StdPicture		-- Picture handling operations
import Graphics.UI.ObjectIO.StdBitmap		-- Defines an instance for drawing bitmaps

import Graphics.UI.ObjectIO.StdProcess		-- Process handling operations

import Graphics.UI.ObjectIO.StdControl		-- Control handling operations
import Graphics.UI.ObjectIO.StdControlReceiver

import Graphics.UI.ObjectIO.StdReceiver		-- Receiver handling operations

import Graphics.UI.ObjectIO.StdWindow		-- Window handling operations

import Graphics.UI.ObjectIO.StdMenu
import Graphics.UI.ObjectIO.StdMenuReceiver
import Graphics.UI.ObjectIO.StdMenuElement

import Graphics.UI.ObjectIO.StdTimer
import Graphics.UI.ObjectIO.StdTimerReceiver

import Graphics.UI.ObjectIO.StdFileSelect

import Graphics.UI.ObjectIO.StdSound

import Graphics.UI.ObjectIO.StdClipboard

import Graphics.UI.ObjectIO.StdSystem
