-----------------------------------------------------------------------------
-- |
-- Module      :  StdProcessAttribute
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdProcessAttribute specifies which ProcessAttributes are valid for 
-- each of the standard interactive processes. Basic comparison operations 
-- and retrieval functions are also included.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdProcessAttribute
		( isProcessKindAttribute
		, isProcessActivate, 	getProcessActivateFun 
		, isProcessClose, 	getProcessCloseFun
		, isProcessDeactivate, 	getProcessDeactivateFun
		, isProcessNoWindowMenu
		, isProcessOpenFiles, 	getProcessOpenFilesFun
		, isProcessToolbar, 	getProcessToolbarAtt
		, isProcessWindowPos, 	getProcessWindowPosAtt
		, isProcessWindowResize,getProcessWindowResizeFun
		, isProcessWindowSize, 	getProcessWindowSizeAtt
		, module Graphics.UI.ObjectIO.StdProcessDef
		) where


import Graphics.UI.ObjectIO.StdProcessDef


isProcessKindAttribute :: DocumentInterface -> ProcessAttribute ps -> Bool 
isProcessKindAttribute di (ProcessActivate     _) = True
isProcessKindAttribute di (ProcessClose        _) = True
isProcessKindAttribute di (ProcessDeactivate   _) = True
isProcessKindAttribute di (ProcessNoWindowMenu  ) = di == MDI
isProcessKindAttribute di (ProcessOpenFiles    _) = di /= NDI
isProcessKindAttribute di (ProcessToolbar      _) = di /= NDI
isProcessKindAttribute di (ProcessWindowPos    _) = di /= NDI
isProcessKindAttribute di (ProcessWindowResize _) = di /= NDI
isProcessKindAttribute di (ProcessWindowSize   _) = di /= NDI


isProcessActivate :: ProcessAttribute ps -> Bool 
isProcessActivate (ProcessActivate _) = True
isProcessActivate _ = False

isProcessClose :: ProcessAttribute ps -> Bool 
isProcessClose (ProcessClose _) = True
isProcessClose _ = False

isProcessDeactivate :: ProcessAttribute ps -> Bool 
isProcessDeactivate (ProcessDeactivate _) = True
isProcessDeactivate _ = False

isProcessNoWindowMenu :: ProcessAttribute ps -> Bool 
isProcessNoWindowMenu ProcessNoWindowMenu = True
isProcessNoWindowMenu _ = False

isProcessOpenFiles :: ProcessAttribute ps -> Bool 
isProcessOpenFiles (ProcessOpenFiles _) = True
isProcessOpenFiles _ = False

isProcessToolbar :: ProcessAttribute ps -> Bool 
isProcessToolbar (ProcessToolbar _) = True
isProcessToolbar _ = False

isProcessWindowPos :: ProcessAttribute ps -> Bool 
isProcessWindowPos (ProcessWindowPos _) = True
isProcessWindowPos _ = False

isProcessWindowResize :: ProcessAttribute ps -> Bool 
isProcessWindowResize (ProcessWindowResize _) = True
isProcessWindowResize _ = False

isProcessWindowSize :: ProcessAttribute ps -> Bool 
isProcessWindowSize (ProcessWindowSize _) = True
isProcessWindowSize _ = False

getProcessActivateFun :: ProcessAttribute ps -> (ps -> GUI ps ps)
getProcessActivateFun (ProcessActivate f) = f

getProcessCloseFun :: ProcessAttribute ps -> ps -> GUI ps ps
getProcessCloseFun (ProcessClose f) = f

getProcessDeactivateFun :: ProcessAttribute ps -> (ps -> GUI ps ps)
getProcessDeactivateFun (ProcessDeactivate f) = f

getProcessOpenFilesFun :: ProcessAttribute ps -> ProcessOpenFilesFunction ps
getProcessOpenFilesFun (ProcessOpenFiles f) = f

getProcessToolbarAtt :: ProcessAttribute ps -> [ToolbarItem ps]
getProcessToolbarAtt (ProcessToolbar t) = t

getProcessWindowPosAtt :: ProcessAttribute ps -> ItemPos
getProcessWindowPosAtt (ProcessWindowPos pos) = pos

getProcessWindowResizeFun :: ProcessAttribute ps -> ProcessWindowResizeFunction ps
getProcessWindowResizeFun (ProcessWindowResize f) = f

getProcessWindowSizeAtt :: ProcessAttribute ps -> Size
getProcessWindowSizeAtt (ProcessWindowSize size) = size

