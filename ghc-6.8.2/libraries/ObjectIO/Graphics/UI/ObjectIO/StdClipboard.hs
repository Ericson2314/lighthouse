-----------------------------------------------------------------------------
-- |
-- Module      :  StdClipboard
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdClipboard specifies all functions on the clipboard.
-- The current clipboard implementation supports only string data type.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdClipboard
		( ClipboardItem
		, Clipboard(..)
		, setClipboard
		, getClipboard
		) where

import	Data.Maybe
import	Graphics.UI.ObjectIO.OS.Clipboard
import	Graphics.UI.ObjectIO.CommonDef(dumpFatalError, remove)
import	Graphics.UI.ObjectIO.Process.IOState(GUI(..), liftIO)


stdClipboardFatalError :: String -> String -> x
stdClipboardFatalError function error
	= dumpFatalError function "StdClipboard" error


--	The clipboard item type:

data ClipboardItem
	= ClipboardString String		-- Support for strings
--	| ClipboardPict	Handle			-- Support for pictures (PA: not supported yet)

class Clipboard item where
	toClipboard	:: item			-> ClipboardItem
	fromClipboard	:: ClipboardItem	-> Maybe item

instance Clipboard String where
	toClipboard string = ClipboardString string
	fromClipboard (ClipboardString string) = Just string
	


--	Reading and writing the value of the selection to the clipboard:

setClipboard :: [ClipboardItem] -> GUI ps ()
setClipboard clipItems = liftIO (mapM_ clipboardItemToScrap singleItems)
    where
	singleItems = removeDuplicateClipItems clipItems

	removeDuplicateClipItems :: [ClipboardItem] -> [ClipboardItem]
	removeDuplicateClipItems (item:items) =
	    let (_,_,items1)	= remove (eqClipboardType item) undefined items
	    in (item:removeDuplicateClipItems items1)
	    where
		eqClipboardType :: ClipboardItem -> ClipboardItem -> Bool
		eqClipboardType (ClipboardString _) item	= case item of
			(ClipboardString _)	-> True
			_			-> False
	removeDuplicateClipItems [] = []

	clipboardItemToScrap :: ClipboardItem -> IO ()
	clipboardItemToScrap (ClipboardString text) = osSetClipboardText text

getClipboard :: GUI ps [ClipboardItem]
getClipboard = do
    contents <- liftIO (osGetClipboardContent)
    let contents1 = filter ((==) osClipboardText) contents
    liftIO (mapM scrapToClipboardItem contents1)
    where
	scrapToClipboardItem :: Int -> IO ClipboardItem
	scrapToClipboardItem clipType
		| clipType == osClipboardText = do
			text <- osGetClipboardText
		 	return (ClipboardString text)
		| otherwise = stdClipboardFatalError "getClipboard" ("unimplemented clipboard content of type: " ++ show clipType)
