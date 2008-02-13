-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.Menu
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.Menu where


import Graphics.UI.ObjectIO.OS.DocumentInterface(OSMenuBar(..))
import Graphics.UI.ObjectIO.OS.Types(OSWindowPtr, osNoWindowPtr)
import Graphics.UI.ObjectIO.OS.ClCrossCall_12
import Graphics.UI.ObjectIO.OS.Cutil_12
import Graphics.UI.ObjectIO.OS.Types(OSWindowPtr,OSMenu,osNoMenu,OSMenuItem,osNoMenuItem)
import Foreign.Marshal.Utils(fromBool)
import Control.Monad(when)
import Data.Char(ord)


--	Enabling and disabling menus and menu elements:

osDisableMenu :: OSMenu -> OSMenuBar -> IO ()
osDisableMenu submenu osMenuBar@(OSMenuBar {menuBar=menuBar}) = do
	issueCleanRequest2 (errorCallback2 "osDisableMenu") (rq3Cci ccRqMENUENABLE menuBar submenu (fromBool False))
	return ()

osEnableMenu :: OSMenu -> OSMenuBar -> IO ()
osEnableMenu submenu osMenuBar@(OSMenuBar {menuBar=menuBar}) = do
	issueCleanRequest2 (errorCallback2 "osEnableMenu") (rq3Cci ccRqMENUENABLE menuBar submenu (fromBool True))
	return ()

osEnableMenuItem :: OSMenu -> OSMenuItem -> IO ()
osEnableMenuItem menuHandle item = do	
	issueCleanRequest2 (errorCallback2 "osEnableMenuItem") (rq3Cci ccRqITEMENABLE menuHandle item (fromBool True))
	return ()

osDisableMenuItem :: OSMenu -> OSMenuItem -> IO ()
osDisableMenuItem menuHandle item = do
	issueCleanRequest2 (errorCallback2 "osEnableMenuItem") (rq3Cci ccRqITEMENABLE menuHandle item (fromBool False))
	return ()


--	Changing and updating the menu bar:

drawMenuBar :: OSMenuBar -> IO ()
drawMenuBar (OSMenuBar {menuWindow=menuWindow,menuClient=menuClient}) = do
	issueCleanRequest2 (errorCallback2 "drawMenuBar") (rq2Cci ccRqDRAWMBAR menuWindow menuClient)
	return ()

osMenuBarClear :: IO ()
osMenuBarClear = return ()

osMenuBarSet :: OSMenuBar -> IO ()
osMenuBarSet menuBar = return ()
	
osMenuInsert :: OSMenuBar -> Int -> String -> Bool -> IO OSMenu
osMenuInsert (OSMenuBar {menuBar=menuBar}) index title able = do
	textPtr <- newCString title
	rcci <- issueCleanRequest2 (errorCallback2 "osMenuInsert") (rq4Cci ccRqINSERTMENU (fromBool able) menuBar (addr2int textPtr) index)
	free textPtr
	(if ccMsg rcci == ccRETURN1
	 then return (p1 rcci)
	 else if ccMsg rcci == ccWASQUIT
	      then return osNoMenu
	      else error "[osMenuInsert] expected CcRETURN1 value.")
	
osSubMenuInsert :: OSMenu -> Int -> String -> Bool -> IO OSMenu
osSubMenuInsert parentMenu index title able = do
	textPtr <- newCString title
	rcci <- issueCleanRequest2 (errorCallback2 "osSubMenuInsert") (rq4Cci ccRqINSERTMENU (fromBool able) parentMenu (addr2int textPtr) index)
	free textPtr
	(if ccMsg rcci == ccRETURN1 
	 then return (p1 rcci)
	 else if ccMsg rcci == ccWASQUIT 
	      then return osNoMenu
	      else error "[osSubMenuInsert] expected CcRETURN1 value.")

osMenuRemove :: OSMenu -> OSMenuBar -> IO ()
osMenuRemove menu (OSMenuBar {menuBar=menuBar}) = do	
	issueCleanRequest2 (errorCallback2 "osMenuRemove") (rq2Cci ccRqDELETEMENU menuBar menu)	
	return ()	

osSubMenuRemove :: OSMenu -> OSMenu -> IO ()
osSubMenuRemove subMenu parentMenu = do
	issueCleanRequest2 (errorCallback2 "osMenuRemove") (rq2Cci ccRqDELETEMENU parentMenu subMenu)
	return ()

osCreatePopUpMenu :: IO OSMenu
osCreatePopUpMenu = do
	rcci <- issueCleanRequest2 (errorCallback2 "CreatePopupMenuHandle ") (rq0Cci ccRqCREATEPOPMENU)
	let menu =
		if ccMsg rcci == ccRETURN1 then p1 rcci else
		if ccMsg rcci == ccWASQUIT then 0	else
						error "[CreatePopupMenuHandle] expected CcRETURN1 value."
	return menu

osTrackPopUpMenu :: OSMenu -> OSWindowPtr -> IO Bool
osTrackPopUpMenu menu framePtr = do
	rcci <- issueCleanRequest2 (errorCallback2 "osTrackPopUpMenu") (rq2Cci ccRqTRACKPOPMENU menu framePtr)
	let ok = 
		if ccMsg rcci == ccRETURN1 then p1 rcci /= 0 else
	  	if ccMsg rcci == ccWASQUIT then False	     else
	  				        error "[osTrackPopUpMenu] expected CcRETURN1 value."
	return ok


--	Changing (sub)menus:
osMenuItemInsert :: OSMenuBar -> OSMenu -> Int -> String -> Bool -> Bool -> Char -> IO OSMenuItem
osMenuItemInsert (OSMenuBar {menuWindow=menuWindow}) menu index title able mark key = do
	textPtr <- newCString title
	let insertCci = rq6Cci ccRqINSERTMENUITEM ((fromBool able)*2 + (fromBool mark)) menuWindow menu (addr2int textPtr) (ord key) index
	rcci <- issueCleanRequest2 (errorCallback2 "osAppendMenuItem") insertCci
	let item =
		if ccMsg rcci == ccRETURN1 then p1 rcci      else
		if ccMsg rcci == ccWASQUIT then osNoMenuItem else
						error "[osAppendMenuItem] expected CcRETURN1 value."
	free textPtr
	return item

osMenuSeparatorInsert :: OSMenu -> Int -> IO OSMenuItem
osMenuSeparatorInsert menu index = do
	rcci <- issueCleanRequest2 (errorCallback2 "osAppendMenuSeparator") (rq2Cci ccRqINSERTSEPARATOR menu index)
	let hitem =
		if ccMsg rcci == ccRETURN1 then p1 rcci      else
		if ccMsg rcci == ccWASQUIT then osNoMenuItem else
					        error "[osAppendMenuSeparator] expected CcRETURN1 value."
	return hitem

osChangeMenuTitle :: OSMenuBar -> OSMenu -> String -> IO ()
osChangeMenuTitle (OSMenuBar {menuBar=menuBar}) menu title = do
	textPtr <- newCString title
	issueCleanRequest2 (errorCallback2 "ModifyMenu") (rq3Cci ccRqMODIFYMENU menu menuBar (addr2int textPtr))
	free textPtr

osChangeMenuItemTitle :: OSMenu -> OSMenuItem -> String -> IO ()
osChangeMenuItemTitle menu item title = do
	textPtr <- newCString title
	issueCleanRequest2 (errorCallback2 "ModifyMenuItem") (rq3Cci ccRqMODIFYMENUITEM item menu (addr2int textPtr))
	free textPtr

osMenuItemCheck :: Bool -> OSMenu -> OSMenuItem -> IO ()
osMenuItemCheck check menu item = do
	issueCleanRequest2 (errorCallback2 "CheckMenuItem") (rq3Cci ccRqCHECKMENUITEM menu item (fromBool check))
	return ()

osMenuRemoveItem :: OSMenuItem -> OSMenu -> IO ()
osMenuRemoveItem item menu = do
	issueCleanRequest2 (errorCallback2 "RemoveMenuItem") (rq2Cci ccRqREMOVEMENUITEM menu item)
	return ()

osRemoveMenuShortKey :: OSWindowPtr -> Int -> IO ()
osRemoveMenuShortKey framePtr cmd = do
	issueCleanRequest2 (errorCallback2 "osRemoveMenuShortKey") (rq2Cci ccRqREMOVEMENUSHORTKEY framePtr cmd)
	return ()