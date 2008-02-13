-- #hide
-----------------------------------------------------------------------------
-- Module      :  Key
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Key defines the key identifiers for keyboard input and two conversion functions
-- that should be used only internally.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Key
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
		upKey,
		toSpecialKey, isSpecialKey
	   ) where


data	SpecialKey = SpecialKey { virtual :: !Int }
	deriving (Eq)

backSpaceVirtualCode	=   8
beginVirtualCode	= 115
clearVirtualCode	=  71
deleteVirtualCode	= 117
downVirtualCode		= 125
endVirtualCode		= 119
enterVirtualCode	=  13
escapeVirtualCode	=  53
f1VirtualCode		= 122
f2VirtualCode		= 120
f3VirtualCode		=  99
f4VirtualCode		= 118
f5VirtualCode		=  96
f6VirtualCode		=  97
f7VirtualCode		=  98
f8VirtualCode		= 100
f9VirtualCode		= 101
f10VirtualCode		= 109
f11VirtualCode		= 103
f12VirtualCode		= 111
f13VirtualCode		= 105
f14VirtualCode		= 107
f15VirtualCode		= 113
helpVirtualCode		= 114
leftVirtualCode		= 123
pgDownVirtualCode	= 121
pgUpVirtualCode		= 116
rightVirtualCode	= 124
upVirtualCode		= 126

instance Show SpecialKey where		-- Name of the SpecialKey
	show key
		= if code==backSpaceVirtualCode then "BackSpaceKey" else -- BackSpace
		  if code==beginVirtualCode     then "BeginKey"     else -- Begin of text
		  if code==clearVirtualCode     then "ClearKey"     else -- Clear
		  if code==deleteVirtualCode    then "DeleteKey"    else -- Delete
		  if code==downVirtualCode      then "DownKey"      else -- Arrow down
		  if code==endVirtualCode       then "EndKey"       else -- End of text
		  if code==enterVirtualCode     then "EnterKey"     else -- Enter
		  if code==escapeVirtualCode    then "EscapeKey"    else -- Escape
		  if code==f1VirtualCode        then "F1Key"        else -- Function 1
		  if code==f2VirtualCode        then "F2Key"        else -- Function 2
		  if code==f3VirtualCode        then "F3Key"        else -- Function 3
		  if code==f4VirtualCode        then "F4Key"        else -- Function 4
		  if code==f5VirtualCode        then "F5Key"        else -- Function 5
		  if code==f6VirtualCode        then "F6Key"        else -- Function 6
		  if code==f7VirtualCode        then "F7Key"        else -- Function 7
		  if code==f8VirtualCode        then "F8Key"        else -- Function 8
		  if code==f9VirtualCode        then "F9Key"        else -- Function 9
		  if code==f10VirtualCode       then "F10Key"       else -- Function 10
		  if code==f11VirtualCode       then "F11Key"       else -- Function 11
		  if code==f12VirtualCode       then "F12Key"       else -- Function 12
		  if code==f13VirtualCode       then "F13Key"       else -- Function 13
		  if code==f14VirtualCode       then "F14Key"       else -- Function 14
		  if code==f15VirtualCode       then "F15Key"       else -- Function 15
		  if code==helpVirtualCode      then "HelpKey"      else -- Help
		  if code==leftVirtualCode      then "LeftKey"      else -- Arrow left
		  if code==pgDownVirtualCode    then "PgDownKey"    else -- Page down
		  if code==pgUpVirtualCode      then "PgUpKey"      else -- Page up
		  if code==rightVirtualCode     then "RightKey"     else -- Arrow right
		  if code==upVirtualCode        then "UpKey"        else --Arrow up
		                                     "toSpecialKey " ++ show code
		where
			code = virtual key

backSpaceKey	:: SpecialKey;		backSpaceKey	= SpecialKey {virtual=backSpaceVirtualCode}	-- BackSpace
beginKey	:: SpecialKey;		beginKey	= SpecialKey {virtual=beginVirtualCode}		-- Begin of text
clearKey	:: SpecialKey;		clearKey	= SpecialKey {virtual=clearVirtualCode}		-- Clear
deleteKey	:: SpecialKey;		deleteKey	= SpecialKey {virtual=deleteVirtualCode}	-- Delete
downKey		:: SpecialKey;		downKey		= SpecialKey {virtual=downVirtualCode}		-- Arrow down
endKey		:: SpecialKey;		endKey		= SpecialKey {virtual=endVirtualCode}		-- End of text
enterKey	:: SpecialKey;		enterKey	= SpecialKey {virtual=enterVirtualCode}		-- Enter
escapeKey	:: SpecialKey;		escapeKey	= SpecialKey {virtual=escapeVirtualCode}	-- Escape
f1Key		:: SpecialKey;		f1Key		= SpecialKey {virtual=f1VirtualCode}		-- Function 1
f2Key		:: SpecialKey;		f2Key		= SpecialKey {virtual=f2VirtualCode}		-- Function 2
f3Key		:: SpecialKey;		f3Key		= SpecialKey {virtual=f3VirtualCode}		-- Function 3
f4Key		:: SpecialKey;		f4Key		= SpecialKey {virtual=f4VirtualCode}		-- Function 4
f5Key		:: SpecialKey;		f5Key		= SpecialKey {virtual=f5VirtualCode}		-- Function 5
f6Key		:: SpecialKey;		f6Key		= SpecialKey {virtual=f6VirtualCode}		-- Function 6
f7Key		:: SpecialKey;		f7Key		= SpecialKey {virtual=f7VirtualCode}		-- Function 7
f8Key		:: SpecialKey;		f8Key		= SpecialKey {virtual=f8VirtualCode}		-- Function 8
f9Key		:: SpecialKey;		f9Key		= SpecialKey {virtual=f9VirtualCode}		-- Function 9
f10Key		:: SpecialKey;		f10Key		= SpecialKey {virtual=f10VirtualCode}		-- Function 10
f11Key		:: SpecialKey;		f11Key		= SpecialKey {virtual=f11VirtualCode}		-- Function 11
f12Key		:: SpecialKey;		f12Key		= SpecialKey {virtual=f12VirtualCode}		-- Function 12
f13Key		:: SpecialKey;		f13Key		= SpecialKey {virtual=f13VirtualCode}		-- Function 13
f14Key		:: SpecialKey;		f14Key		= SpecialKey {virtual=f14VirtualCode}		-- Function 14
f15Key		:: SpecialKey;		f15Key		= SpecialKey {virtual=f15VirtualCode}		-- Function 15
helpKey		:: SpecialKey;		helpKey		= SpecialKey {virtual=helpVirtualCode}		-- Help
leftKey		:: SpecialKey;		leftKey		= SpecialKey {virtual=leftVirtualCode}		-- Arrow left
pgDownKey	:: SpecialKey;		pgDownKey	= SpecialKey {virtual=pgDownVirtualCode}	-- Page down
pgUpKey		:: SpecialKey;		pgUpKey		= SpecialKey {virtual=pgUpVirtualCode}		-- Page up
rightKey	:: SpecialKey;		rightKey	= SpecialKey {virtual=rightVirtualCode}		-- Arrow right
upKey		:: SpecialKey;		upKey		= SpecialKey {virtual=upVirtualCode}		-- Arrow up

toSpecialKey :: Int -> SpecialKey	-- Convert Int to SpecialKey
toSpecialKey specialkey
	= SpecialKey {virtual=specialkey}

isSpecialKey :: Int -> Bool		-- Check for one of the upper SpecialKeys
isSpecialKey specialKey
	= containsSorted specialKey virtualKeyCodes
	where
		containsSorted :: Int -> [Int] -> Bool
		containsSorted x (y:ys)
			| x>y       = containsSorted x ys
			| otherwise = x==y
		containsSorted _ _
			= False

virtualKeyCodes :: [Int]		-- The < sorted list of virtual key codes
virtualKeyCodes
	= [ backSpaceVirtualCode	--   8
	  , escapeVirtualCode		--  53
	  , clearVirtualCode		--  71
	  , enterVirtualCode		--  76
	  , f5VirtualCode		--  96
	  , f6VirtualCode		--  97
	  , f7VirtualCode		--  98
	  , f3VirtualCode		--  99
	  , f8VirtualCode		-- 100
	  , f9VirtualCode		-- 101
	  , f11VirtualCode		-- 103
	  , f13VirtualCode		-- 105
	  , f14VirtualCode		-- 107
	  , f10VirtualCode		-- 109
	  , f12VirtualCode		-- 111
	  , f15VirtualCode		-- 113
	  , helpVirtualCode		-- 114
	  , beginVirtualCode		-- 115
	  , pgUpVirtualCode		-- 116
	  , deleteVirtualCode		-- 117
	  , f4VirtualCode		-- 118
	  , endVirtualCode		-- 119
	  , f2VirtualCode		-- 120
	  , pgDownVirtualCode		-- 121
	  , f1VirtualCode		-- 122
	  , leftVirtualCode		-- 123
	  , rightVirtualCode		-- 124
	  , downVirtualCode		-- 125
	  , upVirtualCode		-- 126
	  ]
