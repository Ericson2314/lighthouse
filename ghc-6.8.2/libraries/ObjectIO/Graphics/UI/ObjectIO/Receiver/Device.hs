-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Receiver.Device
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--	
-- Receiver.Device defines the receiver device event handlers.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Receiver.Device (receiverFunctions) where



import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.Receiver.Handle
import Graphics.UI.ObjectIO.OS.ReceiverEvent
import System.IO(fixIO)
import qualified Data.Map as Map

receiverdeviceFatalError :: String -> String -> x
receiverdeviceFatalError rule error
	= dumpFatalError rule "ReceiverDevice" error

receiverFunctions :: DeviceFunctions ps
receiverFunctions
	= DeviceFunctions
		{ dDevice = ReceiverDevice
		, dShow	  = return
		, dHide	  = return
		, dEvent  = receiverEvent
		, dDoIO   = receiverIO
		, dOpen   = receiverOpen
		, dClose  = receiverClose
		}

receiverOpen :: ps -> GUI ps ps
receiverOpen ps
	= do {
		hasReceiver <- accIOEnv (ioStHasDevice ReceiverDevice);
		if   hasReceiver
		then return ps
		else do {
			appIOEnv (ioStSetDevice (ReceiverSystemState (ReceiverHandles {rReceivers=[]})));
			appIOEnv (ioStSetDeviceFunctions receiverFunctions);
			return ps
		     }
	  }

receiverClose :: ps -> GUI ps ps
receiverClose ps
	= do {
		(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
		if   not found
		then return ps
		else 
		let  rHs  = rReceivers $ receiverSystemStateGetReceiverHandles rDevice
		     rIds = map (\(ReceiverStateHandle _ rH) -> rId rH) rHs
		in
		do {
			idtable <- ioStGetIdTable;
			ioStSetIdTable (foldr Map.delete idtable rIds);
			appIOEnv (ioStRemoveDevice ReceiverDevice);
			appIOEnv (ioStRemoveDeviceFunctions ReceiverDevice);
			return ps
		}
	  }

{-	The receiver handles one message event:
	- ASyncMessage:
		this is a request to handle the first asynchronous message available in the
		asynchronous message queue. Globally the size of the asynchronous message queue
		has already been decreased.
	The QASyncMessage has become superfluous as it is replaced by Channel operations.
	The SyncMessage is currently ignored as synchronous message passing is not yet implemented.
-}
receiverIO :: DeviceEvent -> ps -> GUI ps ps
receiverIO deviceEvent ps
	= do {
		(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
		if   not found		-- This condition should not occur: dDoIO function should be applied only iff dEvent filters message
		then receiverdeviceFatalError "receiverIO" "could not retrieve ReceiverSystemState from IOSt"
		else 
		let  rsHs = rReceivers $ receiverSystemStateGetReceiverHandles rDevice
		in   receiverIO deviceEvent rsHs ps
	  }
	where
		receiverIO :: DeviceEvent -> [ReceiverStateHandle ps] -> ps -> GUI ps ps
		
		receiverIO (ReceiverEvent rId) rsHs ps = do
			it <- ioStGetIdTable
			let Just idParent = Map.lookup rId it
			let dummy           = receiverdeviceFatalError "receiverIO (ReceiverEvent (ASyncMessage _))" "receiver could not be found"
			let (_,rsH,rsHs1)   = remove (identifyReceiverStateHandle (idpId idParent)) dummy rsHs
			toGUI (letReceiverDoIO rsH rsHs1 ps)
			where						
				letReceiverDoIO :: ReceiverStateHandle ps -> [ReceiverStateHandle ps] -> ps -> IOSt ps -> IO (ps,IOSt ps)
				letReceiverDoIO (ReceiverStateHandle ls rH@(ReceiverHandle {rFun=rFun})) rsHs ps ioState
					= do {
						r   <- fixIO (\st -> fromGUI (rFun (ls,ps))
									     (ioStSetDevice 
										 (ReceiverSystemState
										     (ReceiverHandles 
											 {rReceivers=rsHs ++ [ReceiverStateHandle (fst (fst st)) rH]}))
										 ioState));
						let ((_,ps1),ioState1) = r
						in  return (ps1,ioState1)
					  }

		receiverIO _ _ _ = receiverdeviceFatalError "receiverIO" "device event passed receiver event filter without handling"


identifyReceiverStateHandle :: Id -> ReceiverStateHandle ps -> Bool
identifyReceiverStateHandle id (ReceiverStateHandle _ (ReceiverHandle {rId=rId})) = id==rId
