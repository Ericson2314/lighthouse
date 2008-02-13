-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.Clipboard
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- ClCrossCall_12 contains the operations to communicate between
-- Clipboard operations.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.Clipboard where


import	Graphics.UI.ObjectIO.OS.ClCrossCall_12
import	Graphics.UI.ObjectIO.OS.ClCCall_12
import	Graphics.UI.ObjectIO.OS.Cutil_12(int2addr, addr2int)
import  Foreign.Marshal.Utils
import  Foreign.Marshal.Alloc(free)
import  Foreign.C.String

type OSClipboardItemType = Int

osClipboardText = 1

osHasClipboardText :: IO Bool
osHasClipboardText = do
	rcci <- issueCleanRequest2 (errorCallback2 "osHasClipboardText") (rq0Cci ccRqCLIPBOARDHASTEXT)
	let ok =
		if      ccMsg rcci == ccRETURN1 then toBool (p1 rcci)
	  	else if ccMsg rcci == ccWASQUIT	then False
	  	else 				error "[winHasClipboardText] expected ccRETURN1 value."
	return ok

osSetClipboardText :: String -> IO ()
osSetClipboardText text = do
	textptr <- newCString text
	issueCleanRequest2 (errorCallback2 "osSetClipboardText") (rq1Cci ccRqSETCLIPBOARDTEXT (addr2int textptr))
	free textptr

osGetClipboardText :: IO String
osGetClipboardText = do
	rcci <- issueCleanRequest2 (errorCallback2 "osGetClipboardText") (rq0Cci ccRqGETCLIPBOARDTEXT)	
	(if ccMsg rcci == ccRETURN1 then do
		let ptr = (int2addr (p1 rcci))
		s <- peekCString ptr
		free ptr
		return s
	 else if ccMsg rcci == ccWASQUIT	then return ""
	 else error "[winGetClipboardText] expected ccRETURN1 value.\n")

osGetClipboardContent :: IO [OSClipboardItemType]
osGetClipboardContent = do
	hasText <- osHasClipboardText
	return (if hasText then [osClipboardText] else [])
