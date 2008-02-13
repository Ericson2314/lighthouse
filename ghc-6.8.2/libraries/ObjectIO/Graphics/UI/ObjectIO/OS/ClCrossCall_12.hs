-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.ClCrossCall_12
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- ClCrossCall_12 contains the operations to communicate between
-- Haskell and OS thread.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.ClCrossCall_12 ( module Graphics.UI.ObjectIO.OS.ClCrossCall_12 ) where



import Graphics.UI.ObjectIO.OS.Types
import Graphics.UI.ObjectIO.OS.Cutil_12
import System.IO.Unsafe
import Data.IORef


--	********************************************************************************
--	Crosscall infrastructure
--	********************************************************************************

--	CrossCallInfo is the basic record that is passed between the Clean thread and the OS thread:
data	CrossCallInfo
	= CrossCallInfo
		{ ccMsg :: !Int		-- The message nr: Clean->OS use ccRq...; OS->Clean use ccWm...
		, p1    :: !Int
		, p2    :: !Int
		, p3    :: !Int
		, p4    :: !Int
		, p5    :: !Int
		, p6    :: !Int
		}


--	2 versions of IssueCleanRequest: first with state parameter, second without.

issueCleanRequest :: (CrossCallInfo -> s -> IO (CrossCallInfo,s))
                   -> CrossCallInfo -> s -> IO (CrossCallInfo,s)
issueCleanRequest callback cci s
	= do {
		reply <- winKickOsThread cci;
		handleCallBacks callback reply s
	  }
	where
		handleCallBacks :: (CrossCallInfo -> s -> IO (CrossCallInfo,s))
		                 -> CrossCallInfo -> s -> IO (CrossCallInfo,s)
		handleCallBacks callback cci s
			| ccMsg cci>2000
				= error ("handleCallBacks " ++ show (ccMsg cci))
			| isReturnOrQuitCci (ccMsg cci)
				= return (cci,s)
			| otherwise
				= do {
					(returnCci,s1) <- callback cci s;
					replyCci       <- winKickOsThread returnCci;
					handleCallBacks callback replyCci s1
				  }

issueCleanRequest2 :: (CrossCallInfo -> IO CrossCallInfo)
                    -> CrossCallInfo -> IO CrossCallInfo
issueCleanRequest2 callback cci
	= do {
		reply <- winKickOsThread cci;
		handleCallBacks callback reply;
	  }
	where
		handleCallBacks :: (CrossCallInfo -> IO CrossCallInfo)
		                 -> CrossCallInfo -> IO CrossCallInfo
		handleCallBacks callback cci
			| ccMsg cci>2000
				= error ("handleCallBacks " ++ show (ccMsg cci))
			| isReturnOrQuitCci (ccMsg cci)
				= return cci
			| otherwise
				= do {
					returnCci <- callback cci;
					replyCci  <- winKickOsThread returnCci;
					handleCallBacks callback replyCci;
				  }

--	Functions for returning proper number of arguments within a CrossCallInfo.
rq0Cci msg = CrossCallInfo {ccMsg=msg,p1=0,p2=0,p3=0,p4=0,p5=0,p6=0}
rq1Cci msg v1 = CrossCallInfo {ccMsg=msg,p1=v1,p2=0,p3=0,p4=0,p5=0,p6=0}
rq2Cci msg v1 v2 = CrossCallInfo {ccMsg=msg,p1=v1,p2=v2,p3=0,p4=0,p5=0,p6=0}
rq3Cci msg v1 v2 v3 = CrossCallInfo {ccMsg=msg,p1=v1,p2=v2,p3=v3,p4=0,p5=0,p6=0}
rq4Cci msg v1 v2 v3 v4 = CrossCallInfo {ccMsg=msg,p1=v1,p2=v2,p3=v3,p4=v4,p5=0,p6=0}
rq5Cci msg v1 v2 v3 v4 v5 = CrossCallInfo {ccMsg=msg,p1=v1,p2=v2,p3=v3,p4=v4,p5=v5,p6=0}
rq6Cci msg v1 v2 v3 v4 v5 v6 = CrossCallInfo {ccMsg=msg,p1=v1,p2=v2,p3=v3,p4=v4,p5=v5,p6=v6}

return0Cci :: CrossCallInfo
return0Cci = rq0Cci ccRETURN0

return1Cci :: Int -> CrossCallInfo
return1Cci v = rq1Cci ccRETURN1 v

return2Cci :: Int -> Int -> CrossCallInfo
return2Cci v1 v2 = rq2Cci ccRETURN2 v1 v2

return3Cci :: Int -> Int -> Int -> CrossCallInfo
return3Cci v1 v2 v3 = rq3Cci ccRETURN3 v1 v2 v3

return4Cci :: Int -> Int -> Int -> Int -> CrossCallInfo
return4Cci v1 v2 v3 v4 = rq4Cci ccRETURN4 v1 v2 v3 v4

return5Cci :: Int -> Int -> Int -> Int -> Int -> CrossCallInfo
return5Cci v1 v2 v3 v4 v5 = rq5Cci ccRETURN5 v1 v2 v3 v4 v5

return6Cci :: Int -> Int -> Int -> Int -> Int -> Int -> CrossCallInfo
return6Cci v1 v2 v3 v4 v5 v6 = rq6Cci ccRETURN6 v1 v2 v3 v4 v5 v6

isReturnOrQuitCci :: Int -> Bool
isReturnOrQuitCci mess
	= mess==ccWASQUIT || (mess<=ccRETURNmax && mess>=ccRETURNmin)


{-	Two error callback routines that do not nothing. They can be used
	conveniently as argument of issueCleanRequest(2).
-}
errorCallback :: String -> CrossCallInfo -> s -> IO (CrossCallInfo,s)
errorCallback source cci s = return (return0Cci,s)

errorCallback2 :: String -> CrossCallInfo -> IO CrossCallInfo
errorCallback2 source cci = return return0Cci


--	********************************************************************************
--	Synchronisation operations between the Clean thread and OS thread.
--	********************************************************************************

{-# NOINLINE gEventsInited #-}
gEventsInited = unsafePerformIO (newIORef False) :: IORef Bool

osInitToolbox :: IO Bool
osInitToolbox = do
	eventsInited <- readIORef gEventsInited
	if not eventsInited
	  then do
		winStartOsThread
		osInstallFont
		osInitialiseFileSelectors
		osInstallMenus
		osInstallClipboard
		osInstallPrinter
		osInstallWindows
		osInstallDI
		writeIORef gEventsInited True
		return True
	  else return False
	  
osCloseToolbox = do
	eventsInited <- readIORef gEventsInited
	if eventsInited
	  then do
	  	winKillOsThread
		writeIORef gEventsInited False
		return True
	  else return False

foreign import ccall "cCrossCallFont_121.h InstallCrossCallFont" 		osInstallFont :: IO ()
foreign import ccall "cCrossCallFileSelectors_121.h InstallCrossCallFileSelectors" 	osInitialiseFileSelectors :: IO ()
foreign import ccall "cCrossCallMenus_121.h InstallCrossCallMenus" 		osInstallMenus :: IO ()
foreign import ccall "cCrossCallClipboard_121.h InstallCrossCallClipboard" 	osInstallClipboard :: IO ()
foreign import ccall "cCrossCallPrinter_121.h InstallCrossCallPrinter" 	osInstallPrinter :: IO ()
foreign import ccall "cCrossCallWindows_121.h InstallCrossCallWindows" 	osInstallWindows :: IO ()
foreign import ccall "cCrossCallxDI_121.h InstallCrossCallxDI" 		osInstallDI :: IO ()

winKickOsThread :: CrossCallInfo -> IO CrossCallInfo
winKickOsThread cci@(CrossCallInfo {ccMsg=ccMsg,p1=p1,p2=p2,p3=p3,p4=p4,p5=p5,p6=p6})
	= do {
		-- Marshal out parameters:
		omess <- malloc;
		op1 <- malloc;
		op2 <- malloc;
		op3 <- malloc;
		op4 <- malloc;
		op5 <- malloc;
		op6 <- malloc;
		-- Call C routine:
		cWinKickOsThread ccMsg p1 p2 p3 p4 p5 p6 omess op1 op2 op3 op4 op5 op6;
		-- Read/free:
		omess' <- fpeek omess;
		op1' <- fpeek op1;
		op2' <- fpeek op2;
		op3' <- fpeek op3;
		op4' <- fpeek op4;
		op5' <- fpeek op5;
		op6' <- fpeek op6;
		return (CrossCallInfo omess' op1' op2' op3' op4' op5' op6')
	  }
foreign import ccall "WinKickOsThread" cWinKickOsThread :: Int -> Int -> Int -> Int -> Int -> Int -> Int
                                   -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()

foreign import ccall "WinKillOsThread" winKillOsThread :: IO ()
foreign import ccall "WinStartOsThread" winStartOsThread :: IO ()


--	********************************************************************************
--	The message numbers for communication from Clean/Haskell to OS (ccMsg field)
--	********************************************************************************

{- Game cross call codes -}
ccRqUSERGAMEEVENT		= 1905
ccRqCREATEGAMEOBJECT		= 1904
ccRqPLAYSOUNDSAMPLE		= 1903

ccRqRUNGAME			= 1901
ccRqCREATEGAMEWINDOW		= 1900

{- Print cross call codes -}
ccRqDO_PRINT_SETUP		= 1828
ccRqDO_HTML_HELP		= 1827

ccRqGET_PRINTER_DC		= 1824
ccRqDISPATCH_MESSAGES_WHILE_PRINTING
				= 1823
ccRqENDDOC			= 1822
ccRqSTARTDOC			= 1821

{- TCP cross call codes -}
ccRqCREATETCPWINDOW		= 1820		{- Create TCP window -}

{- GUI cross call codes -}
ccRqDESTROYMDIDOCWINDOW 	= 1817		{- Destroy MDI document window -}
ccRqCREATESDIDOCWINDOW		= 1816		{- Create SDI document window  -}
ccRqCREATEMDIDOCWINDOW		= 1815		{- Create MDI document window  -}
ccRqCREATEMDIFRAMEWINDOW	= 1814		{- Create MDI frame window     -}
ccRqCREATESDIFRAMEWINDOW	= 1813		{- Create SDI frame window     -}
ccRqCLIPBOARDHASTEXT		= 1812
ccRqGETCLIPBOARDTEXT		= 1811
ccRqSETCLIPBOARDTEXT		= 1810

ccRqDIRECTORYDIALOG		= 1802		{- Create directory selector dialog. -}
ccRqFILESAVEDIALOG		= 1801
ccRqFILEOPENDIALOG		= 1800

ccRqUPDATEDESKTOP		= 1790		{- Force refresh of desktop. -}

ccRqSHOWCONTROL			= 1755
ccRqSELECTPOPUPITEM		= 1754
ccRqADDTOPOPUP			= 1752
ccRqSETITEMCHECK		= 1751
ccRqENABLECONTROL		= 1750

ccRqCREATECOMPOUND		= 1729
ccRqCREATESCROLLBAR		= 1728
ccRqCREATECUSTOM		= 1727
ccRqCREATEICONBUT		= 1726
ccRqCREATEPOPUP			= 1725
ccRqCREATECHECKBOX		= 1724
ccRqCREATERADIOBUT		= 1723
ccRqCREATEEDITTXT		= 1722
ccRqCREATESTATICTXT		= 1721
ccRqCREATEBUTTON		= 1720

ccRqCREATEMODALDIALOG		= 1701		{- Create modal dialog. -}
ccRqCREATEDIALOG		= 1700

ccRqCREATETOOLBARSEPARATOR	= 1603		{- Create a toolbar separator item.    -}
ccRqCREATETOOLBARITEM		= 1602		{- Create a toolbar bitmap item.       -}
ccRqCREATEMDITOOLBAR		= 1601		{- Create a toolbar for a MDI process. -}
ccRqCREATESDITOOLBAR		= 1600		{- Create a toolbar.                   -}

ccCbFONTSIZE			= 1530

ccCbFONTNAME			= 1520

ccRqGETFONTSIZES		= 1510

ccRqGETFONTNAMES		= 1500

ccRqSETCLIENTSIZE		= 1438		{- Set client size.                    -}
ccRqDELCONTROLTIP		= 1437		{- Remove controls from tooltip areas. -}
ccRqADDCONTROLTIP		= 1436		{- Add controls to tooltip areas.      -}
ccRqGETWINDOWSIZE		= 1435		{- Retrieve bounding size of windows.  -}
ccRqRESTACKWINDOW		= 1434		{- Restack windows.                    -}
ccRqSHOWWINDOW			= 1433		{- (hide/show) windows.                -}
ccRqSETWINDOWSIZE		= 1432		{- Resize windows/controls.            -}
ccRqSETSELECTWINDOW		= 1431		{- (en/dis)able windows.               -}
ccRqSETWINDOWPOS		= 1430		{- Move windows/controls.              -}

ccRqSETEDITSELECTION		= 1428		{- Handling edit control selections. -}
ccRqSETSCROLLSIZE		= 1427		{- Setting thumb size of scrollbar.  -}
ccRqSETSCROLLPOS		= 1426		{- Setting thumb of scrollbar.       -}
ccRqSETSCROLLRANGE		= 1425		{- Setting range of scrollbar.       -}
ccRqOBSCURECURSOR		= 1422
ccRqCHANGEWINDOWCURSOR		= 1421
ccRqACTIVATEWINDOW		= 1420		{- Activating window.   -}
ccRqACTIVATECONTROL		= 1419		{- Activating controls. -}

ccRqCREATECARET			= 1610
ccRqSETCARETPOS			= 1611
ccRqDESTROYCARET		= 1612

ccRqGETWINDOWPOS		= 1416
ccRqGETCLIENTSIZE		= 1415

ccRqUPDATEWINDOWRECT		= 1412		{- Updating rect part of a window/control. -}
ccRqGETWINDOWTEXT		= 1411
ccRqSETWINDOWTITLE		= 1410

ccRqFAKEPAINT			= 1405		{- Combination of BeginPaint; EndPaint; InvalidateRect; -}
ccRqENDPAINT			= 1404
ccRqBEGINPAINT			= 1403
ccRqDESTROYWINDOW		= 1402
ccRqDESTROYMODALDIALOG		= 1401		{- Destroy modal dialog. -}

ccRqDRAWMBAR			= 1265

ccRqTRACKPOPMENU		= 1256		{- Handling pop up menu. -}
ccRqCREATEPOPMENU		= 1255

ccRqINSERTSEPARATOR		= 1245

ccRqMENUENABLE			= 1235

ccRqMODIFYMENU			= 1230

ccRqINSERTMENU			= 1226		{- Inserting a new menu into the menu bar -}

ccRqITEMENABLE			= 1220

ccRqREMOVEMENUSHORTKEY		= 1217		{- Removing a shortkey of a menu item -}
ccRqMODIFYMENUITEM		= 1215
ccRqDELETEMENU			= 1213		{- Deleting a menu logically      -}
ccRqREMOVEMENUITEM		= 1212

ccRqCHECKMENUITEM		= 1210

ccRqINSERTMENUITEM		= 1205

ccRqCREATELISTBOX 		= 1206
ccRqADDTOLISTBOX 		= 1207
ccRqSELECTLISTBOXITEM		= 1208
ccRqMARKLISTBOXITEM		= 1209

ccRqDOMESSAGE			= 1100

----------------------------------------------------------------------------
--  The message numbers for communication from OS to Clean (ccMsg field)  --
----------------------------------------------------------------------------
ccWINMESSmax			= 999

{- Game cross calls: 500-599 -}
ccWmCHECKQUIT			= 513		{- Mike: check user's quit function   -}
ccWmUSEREVENT			= 512		{- Mike: user defined event           -}
ccWmSTATISTICS			= 511		{- Mike: request for statistics       -}
ccWmOBJECTKEYUP			= 510		{- Mike: key released                 -}
ccWmOBJECTKEYDOWN		= 509		{- Mike: key pressed for object       -}
ccWmOBJECTTIMER			= 508		{- Mike: framecounter reached 0       -}
ccWmANIMATION			= 507		{- Mike: animation sequence ended     -}
ccWmCOLLISION			= 506		{- Mike: collision of two objects     -}
ccWmTOUCHBOUND			= 505		{- Mike: object touches bound or code -}
ccWmOBJECTDONE			= 504		{- Mike: object is destroyed          -}
ccWmMOVEOBJECT			= 503		{- Mike: move object                  -}
ccWmINITOBJECT			= 502		{- Mike: initialize new object        -}
ccWmSCROLL			= 501		{- Mike: calculate layer position     -}
ccWmGAMEKEYBOARD		= 500		{- Mike: keyboard input for game      -}

{- TCP cross calls -}
ccWmINETEVENT			= 140

{- GUI cross calls -}
ccWmZEROTIMER			= 136		{- Sequence of zero timer events (generated only by Clean). -}
ccWmLOSTKEY			= 135		{- Loosing keyboard input (generated only by Clean). -}
ccWmLOSTMOUSE			= 134		{- Loosing mouse input    (generated only by Clean). -}
ccWmSPECIALBUTTON		= 133		{- Info about OK/CANCEL button selected. -}
ccWmPROCESSDROPFILES		= 132		{- Requesting opening of files. -}
ccWmGETTOOLBARTIPTEXT		= 131		{- Getting tooltip text. -}
ccWmSETFOCUS			= 130		{- Notifying obtaining keyboard input focus. -}
ccWmKILLFOCUS			= 129		{- Notifying loss of keyboard input focus. -}

ccWmPROCESSCLOSE		= 127		{- Requesting closing of process. -}
ccWmDRAWCLIPBOARD		= 126		{- Clipboard handling. Copied from Ronny. -}
ccWmGETSCROLLBARINFO		= 125		{- Info about scrollbars. -}
ccWmSCROLLBARACTION		= 124		{- Scrollbar handling. -}
ccWmDDEEXECUTE			= 123

ccWmIDLEDIALOG			= 121		{- Initialising modal dialogues. -}
ccWmDRAWCONTROL			= 120
ccWmITEMSELECT			= 119
ccWmBUTTONCLICKED		= 118
ccWmINITDIALOG			= 117
ccWmIDLETIMER			= 116
ccWmTIMER			= 115
ccWmNEWVTHUMB			= 114
ccWmNEWHTHUMB			= 113
ccWmGETVSCROLLVAL		= 112
ccWmGETHSCROLLVAL		= 111
ccWmSIZE			= 110		{- Passing resize information. -}
ccWmMOUSE			= 109
ccWmKEYBOARD			= 108
ccWmDEACTIVATE			= 107
ccWmACTIVATE			= 106
ccWmCLOSE			= 105
ccWmCOMMAND			= 103
ccWmCHAR			= 102
ccWmCREATE			= 101
ccWmPAINT			= 100

ccWINMESSmin			= 100

ccWmNOTIFY			= 78

ccRETURNmax			= 19

ccRETURN6			= 16
ccRETURN5			= 15
ccRETURN4			= 14
ccRETURN3			= 13
ccRETURN2			= 12
ccRETURN1			= 11
ccRETURN0			= 10

ccRETURNmin			= 10

ccWASQUIT			= 1

{-	All of the above ccXXX values are supposed to be Int.
-}
ccRqUSERGAMEEVENT,
	ccRqCREATEGAMEOBJECT,
	ccRqPLAYSOUNDSAMPLE,
	ccRqRUNGAME,
	ccRqCREATEGAMEWINDOW,
	ccRqDO_PRINT_SETUP,
	ccRqDO_HTML_HELP,
	ccRqGET_PRINTER_DC,
	ccRqDISPATCH_MESSAGES_WHILE_PRINTING,
	ccRqENDDOC,
	ccRqSTARTDOC,
	ccRqCREATETCPWINDOW,
	ccRqDESTROYMDIDOCWINDOW,
	ccRqCREATESDIDOCWINDOW,
	ccRqCREATEMDIDOCWINDOW,
	ccRqCREATEMDIFRAMEWINDOW,
	ccRqCREATESDIFRAMEWINDOW,
	ccRqCLIPBOARDHASTEXT,
	ccRqGETCLIPBOARDTEXT,
	ccRqSETCLIPBOARDTEXT,
	ccRqDIRECTORYDIALOG,
	ccRqFILESAVEDIALOG,
	ccRqFILEOPENDIALOG,
	ccRqUPDATEDESKTOP,
	ccRqSHOWCONTROL,
	ccRqSELECTPOPUPITEM,
	ccRqADDTOPOPUP,
	ccRqSETITEMCHECK,
	ccRqENABLECONTROL,
	ccRqCREATECOMPOUND,
	ccRqCREATESCROLLBAR,
	ccRqCREATECUSTOM,
	ccRqCREATEICONBUT,
	ccRqCREATEPOPUP,
	ccRqCREATECHECKBOX,
	ccRqCREATERADIOBUT,
	ccRqCREATEEDITTXT,
	ccRqCREATESTATICTXT,
	ccRqCREATEBUTTON,
	ccRqCREATEMODALDIALOG,
	ccRqCREATEDIALOG,
	ccRqCREATETOOLBARSEPARATOR,
	ccRqCREATETOOLBARITEM,
	ccRqCREATEMDITOOLBAR,
	ccRqCREATESDITOOLBAR,
	ccCbFONTSIZE,
	ccCbFONTNAME,
	ccRqGETFONTSIZES,
	ccRqGETFONTNAMES,
	ccRqSETCLIENTSIZE,
	ccRqDELCONTROLTIP,
	ccRqADDCONTROLTIP,
	ccRqGETWINDOWSIZE,
	ccRqRESTACKWINDOW,
	ccRqSHOWWINDOW,
	ccRqSETWINDOWSIZE,
	ccRqSETSELECTWINDOW,
	ccRqSETWINDOWPOS,
	ccRqSETEDITSELECTION,
	ccRqSETSCROLLSIZE,
	ccRqSETSCROLLPOS,
	ccRqSETSCROLLRANGE,
	ccRqOBSCURECURSOR,
	ccRqCHANGEWINDOWCURSOR,
	ccRqACTIVATEWINDOW,
	ccRqACTIVATECONTROL,
	ccRqCREATECARET,
	ccRqSETCARETPOS,
	ccRqDESTROYCARET,
	ccRqGETWINDOWPOS,
	ccRqGETCLIENTSIZE,
	ccRqUPDATEWINDOWRECT,
	ccRqGETWINDOWTEXT,
	ccRqSETWINDOWTITLE,
	ccRqFAKEPAINT,
	ccRqENDPAINT,
	ccRqBEGINPAINT,
	ccRqDESTROYWINDOW,
	ccRqDESTROYMODALDIALOG,
	ccRqDRAWMBAR,
	ccRqTRACKPOPMENU,
	ccRqCREATEPOPMENU,
	ccRqINSERTSEPARATOR,
	ccRqMENUENABLE,
	ccRqMODIFYMENU,	
	ccRqINSERTMENU,
	ccRqITEMENABLE,
	ccRqREMOVEMENUSHORTKEY,
	ccRqMODIFYMENUITEM,
	ccRqDELETEMENU,
	ccRqREMOVEMENUITEM,
	ccRqCHECKMENUITEM,
	ccRqINSERTMENUITEM,
	ccRqCREATELISTBOX,
	ccRqADDTOLISTBOX,
	ccRqSELECTLISTBOXITEM,
	ccRqMARKLISTBOXITEM,
	ccRqDOMESSAGE,
	ccWINMESSmax,
	ccWmCHECKQUIT,
	ccWmUSEREVENT,
	ccWmSTATISTICS,
	ccWmOBJECTKEYUP,
	ccWmOBJECTKEYDOWN,
	ccWmOBJECTTIMER,
	ccWmANIMATION,
	ccWmCOLLISION,
	ccWmTOUCHBOUND,
	ccWmOBJECTDONE,
	ccWmMOVEOBJECT,
	ccWmINITOBJECT,
	ccWmSCROLL,
	ccWmGAMEKEYBOARD,
	ccWmINETEVENT,
	ccWmZEROTIMER,
	ccWmLOSTKEY,
	ccWmLOSTMOUSE,
	ccWmSPECIALBUTTON,
	ccWmPROCESSDROPFILES,
	ccWmGETTOOLBARTIPTEXT,
	ccWmSETFOCUS,
	ccWmKILLFOCUS,
	ccWmPROCESSCLOSE,
	ccWmDRAWCLIPBOARD,
	ccWmGETSCROLLBARINFO,
	ccWmSCROLLBARACTION,
	ccWmDDEEXECUTE,
	ccWmIDLEDIALOG,
	ccWmDRAWCONTROL,
	ccWmITEMSELECT,
	ccWmBUTTONCLICKED,
	ccWmINITDIALOG,
	ccWmIDLETIMER,
	ccWmTIMER,
	ccWmNEWVTHUMB,
	ccWmNEWHTHUMB,
	ccWmGETVSCROLLVAL,
	ccWmGETHSCROLLVAL,
	ccWmSIZE,
	ccWmMOUSE,
	ccWmKEYBOARD,
	ccWmDEACTIVATE,
	ccWmACTIVATE,
	ccWmCLOSE,
	ccWmCOMMAND,
	ccWmCHAR,
	ccWmCREATE,
	ccWmPAINT,
	ccWINMESSmin,
	ccWmNOTIFY,
	ccRETURNmax,
	ccRETURN6,
	ccRETURN5,
	ccRETURN4,
	ccRETURN3,
	ccRETURN2,
	ccRETURN1,
	ccRETURN0,
	ccRETURNmin,
	ccWASQUIT
	:: Int
