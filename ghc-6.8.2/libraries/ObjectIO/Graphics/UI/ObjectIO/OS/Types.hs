-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.Types
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- OS.Types defines standard types for the OS.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.Types where

import Foreign.Ptr

data	Rect					-- A Rect is supposed to be an ordered rectangle with
	= Rect
		{ rleft   :: !Int		-- rleft<=rright && rtop<=rbottom
		, rtop    :: !Int
		, rright  :: !Int
		, rbottom :: !Int
		}
	deriving (Eq)
	
type	OSWindowPtr = Int
osNoWindowPtr = 0 :: OSWindowPtr

type OSRgnHandle = Ptr ()
osNoRgn = nullPtr :: OSRgnHandle

type	OSPictContext = Ptr ()

type OSBmpHandle = Ptr ()

type	OSMenu		= Int
osNoMenu 		= 0 :: OSMenu

type	OSMenuItem	= Int
osNoMenuItem 		= 0 :: OSMenuItem


data	DelayActivationInfo
	= DelayActivatedWindow    OSWindowPtr			-- the window has become active
	| DelayDeactivatedWindow  OSWindowPtr			-- the window has become inactive
	| DelayActivatedControl   OSWindowPtr OSWindowPtr	-- the control (@2) in window (@1) has become active
	| DelayDeactivatedControl OSWindowPtr OSWindowPtr	-- the control (@2) in window (@1) has become inactive
