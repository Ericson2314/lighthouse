-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.FileSelect
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.FileSelect(osSelectInputFile, osSelectOutputFile, osSelectDirectory) where


import	Graphics.UI.ObjectIO.CommonDef (dumpFatalError)
import	Graphics.UI.ObjectIO.OS.ClCrossCall_12
import	Graphics.UI.ObjectIO.OS.Event
import	Graphics.UI.ObjectIO.OS.Cutil_12(int2addr, addr2int, free)
import  Foreign.C.String(newCString, peekCString)


osFileSelectFatalError :: String -> String -> x
osFileSelectFatalError function error
	= dumpFatalError function "OSFileSelect" error

osSelectInputFile :: (OSEvent->IO ()) -> IO (Maybe String)
osSelectInputFile handleOSEvent =
	issueCleanRequest2 (callback handleOSEvent) (rq0Cci ccRqFILEOPENDIALOG) >>= getInputFileName
	where
		getInputFileName :: CrossCallInfo -> IO (Maybe String)
		getInputFileName cci
			| ccMsg cci == ccRETURN2 =
				if p1 cci == 0
				then return Nothing
				else do
					let pathnamePtr = (int2addr (p2 cci))
					pathname <- peekCString pathnamePtr
					free pathnamePtr
					return (Just pathname)
			| ccMsg cci == ccWASQUIT =
				return Nothing
			| otherwise =
				osFileSelectFatalError "osSelectInputFile" ("unexpected ccMsg field of return CrossCallInfo ("++show (ccMsg cci)++")")

osSelectOutputFile :: (OSEvent->IO ()) -> String -> String -> IO (Maybe String)
osSelectOutputFile handleOSEvent prompt filename = do
	promptPtr <- newCString prompt
	filenamePtr <- newCString filename
	rcci <- issueCleanRequest2 (callback handleOSEvent) (rq2Cci ccRqFILESAVEDIALOG (addr2int promptPtr) (addr2int filenamePtr))
	free promptPtr
	free filenamePtr
	getOutputFileName rcci
	where
		getOutputFileName :: CrossCallInfo -> IO (Maybe String)
		getOutputFileName cci
			| ccMsg cci == ccRETURN2 =
				if p1 cci == 0
				then return Nothing
				else do
					let pathPtr = (int2addr (p2 cci))
					path <- peekCString pathPtr
					free pathPtr
					return (Just path)
			| ccMsg cci == ccWASQUIT =
				return Nothing
			| otherwise =
				osFileSelectFatalError "osSelectOutputFile" ("unexpected ccMsg field of return CrossCallInfo ("++show (ccMsg cci)++")")

osSelectDirectory :: (OSEvent->IO ()) -> IO (Maybe String)
osSelectDirectory handleOSEvent =
	issueCleanRequest2 (callback handleOSEvent) (rq0Cci ccRqDIRECTORYDIALOG) >>= getInputFileName
	where
		getInputFileName :: CrossCallInfo -> IO (Maybe String)
		getInputFileName cci
			| ccMsg cci == ccRETURN2 =
				if p1 cci == 0
				then return Nothing
				else do
					let pathnamePtr = (int2addr (p2 cci))
					pathname <- peekCString pathnamePtr
					free pathnamePtr
					return (Just pathname)
			| ccMsg cci == ccWASQUIT =
				return Nothing
			| otherwise =
				osFileSelectFatalError "osSelectDirectory" ("unexpected ccMsg field of return CrossCallInfo ("++show (ccMsg cci)++")")

--	callback lifts a function::(OSEvent -> IO ()) to
--        a crosscallfunction::(CrossCallInfo -> IO CrossCallInfo)
callback :: (OSEvent->IO ()) -> CrossCallInfo -> IO CrossCallInfo
callback handleOSEvent cci = handleOSEvent cci >> return return0Cci
