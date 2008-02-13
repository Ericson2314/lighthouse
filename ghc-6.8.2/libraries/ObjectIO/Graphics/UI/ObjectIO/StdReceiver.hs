-----------------------------------------------------------------------------
-- |
-- Module      :  StdReceiver
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdReceiver specifies all receiver operations.
-- The receiver is a component which allows the communication between 
-- different interactive process or between different devices 
-- (For example from Timer device to Menu device) by user defined message events.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdReceiver
		(
		-- * Opening receivers
		  Receivers(..),
		-- * Closing receivers
		  closeReceiver,
		-- * Get the Ids of all current receivers
		  getReceivers,
		-- * Enabling\/Disabling of receivers
		  enableReceivers, disableReceivers, getReceiverSelectState,
		-- * Message passing
		  asyncSend, syncSend, syncSend2,
		-- * A visible module
		  module Graphics.UI.ObjectIO.StdReceiverDef
		) where



import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Device.Events
import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.Process.Scheduler
import Graphics.UI.ObjectIO.Receiver.Access
import Graphics.UI.ObjectIO.Receiver.DefAccess
import Graphics.UI.ObjectIO.Receiver.Device
import Graphics.UI.ObjectIO.Receiver.Handle
import Graphics.UI.ObjectIO.StdReceiverAttribute
import Graphics.UI.ObjectIO.StdReceiverDef
import Graphics.UI.ObjectIO.OS.Event
import Control.Monad(unless)
import System.IO(fixIO)
import Data.IORef
import qualified Data.Map as Map


stdReceiverFatalError :: String -> String -> x
stdReceiverFatalError rule error
	= dumpFatalError rule "StdReceiver" error


--	Open one-way receiver:

receiverStateIdentified :: Id -> ReceiverStateHandle ps -> Bool
receiverStateIdentified id (ReceiverStateHandle _ rH) = receiverIdentified id rH

class Receivers rdef where
	openReceiver :: ls -> rdef ls ps -> ps -> GUI ps ps


instance Receivers (Receiver m) where
	openReceiver ls rDef ps
		= do {
			ps1 <- dOpen receiverFunctions ps;
			idtable      <-  ioStGetIdTable;
			if   Map.member id idtable
			then throwGUI ErrorIdsInUse
			else 
			do {
				(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
				if   not found		-- This condition should not occur: ReceiverDevice has just been 'installed'
				then stdReceiverFatalError "openReceiver (Receiver)" "could not retrieve ReceiverSystemState from IOSt"
				else
				let  rsHs  = rReceivers (receiverSystemStateGetReceiverHandles rDevice)
				in
				do {
					ioId <- accIOEnv ioStGetIOId;
					ioStSetIdTable (Map.insert id (IdParent {idpIOId=ioId,idpDevice=ReceiverDevice,idpId=id}) idtable);
					ps2  <- toGUI (doInitIO (receiverInit (ls,ps1))
								(\ls ioState -> ioStSetDevice
										    (ReceiverSystemState
											 (ReceiverHandles
											     {rReceivers = (ReceiverStateHandle ls (newReceiverHandle rid select f)) : rsHs}))
										    ioState));
					return ps2
				}
			}
		  }
		where
			rid            = receiverDefRId        rDef
			select         = receiverDefSelectState rDef
			f              = receiverDefFunction    rDef
			rDefAttributes = receiverDefAttributes  rDef
			receiverInit   = getReceiverInitFun (snd (cselect isReceiverInit (ReceiverInit return) rDefAttributes))
			id             = rIdtoId rid
			
			
instance Receivers (Receiver2 m r) where
	openReceiver ls rDef ps
		= do {
			ps1 <- dOpen receiverFunctions ps;
			idtable      <-  ioStGetIdTable;
			if   Map.member id idtable
			then throwGUI ErrorIdsInUse
			else 
			do {
				(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
				if   not found		-- This condition should not occur: ReceiverDevice has just been 'installed'
				then stdReceiverFatalError "openReceiver (Receiver)" "could not retrieve ReceiverSystemState from IOSt"
				else
				let  rsHs  = rReceivers (receiverSystemStateGetReceiverHandles rDevice)
				in
				do {					
					ioId <- accIOEnv ioStGetIOId;
					ioStSetIdTable (Map.insert id (IdParent {idpIOId=ioId,idpDevice=ReceiverDevice,idpId=id}) idtable);
					ps2  <- toGUI (doInitIO (receiverInit (ls,ps1))
								(\ls ioState -> ioStSetDevice
										    (ReceiverSystemState
											 (ReceiverHandles
											     {rReceivers = (ReceiverStateHandle ls (newReceiver2Handle rid select f)) : rsHs}))
										    ioState));
					return ps2
				}
			}
		  }
		where
			rid            = receiver2DefR2Id        rDef
			select         = receiver2DefSelectState rDef
			f              = receiver2DefFunction    rDef
			rDefAttributes = receiver2DefAttributes  rDef
			receiverInit   = getReceiverInitFun (snd (cselect isReceiverInit (ReceiverInit return) rDefAttributes))
			id             = r2IdtoId rid


doInitIO :: GUI ps (ls,ps) -> (ls -> IOSt ps -> IOSt ps) -> IOSt ps -> IO (ps,IOSt ps)
doInitIO initGUI setReceiverLS ioState
	= do {
		r <- fixIO (\st -> fromGUI initGUI (setReceiverLS (fst (fst st)) ioState));
		let ((_,ps1),ioState1) = r
		in  return (ps1,ioState1)
	  }


--	Closing receivers.

closeReceiver :: Id -> GUI ps ()
closeReceiver id
	= do {
		closed <- accIOEnv ioStClosed;
		if   closed
		then return ()
		else
		do {
			(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
			if   not found
			then return ()
			else 
			let  rsHs              = rReceivers (receiverSystemStateGetReceiverHandles rDevice)
			     (found,rsH,rsHs1) = remove (receiverStateIdentified id) undefined rsHs
			in
			do {
				appIOEnv (ioStSetDevice (ReceiverSystemState (ReceiverHandles {rReceivers=rsHs1})));
				if   not found
				then return ()
				else 
				do {
					idtable <- ioStGetIdTable;
					ioStSetIdTable (Map.delete id idtable);
				}
			}
		}
	  }


--	Get the Ids of all current receivers:

getReceivers :: GUI ps [Id]
getReceivers
	= do {
		(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
		if   not found
		then return []
		else return (map  getreceiver (rReceivers (receiverSystemStateGetReceiverHandles rDevice)))
	  }
	where		
		getreceiver :: ReceiverStateHandle ps -> Id
		getreceiver rsH@(ReceiverStateHandle _ (ReceiverHandle {rId=rId})) = rId
		

--	Changing attributes:

enableReceivers  :: [Id] -> GUI ps ()
enableReceivers  ids = changeReceivers (receiverSetSelectState Able)   ids

disableReceivers :: [Id] -> GUI ps ()
disableReceivers ids = changeReceivers (receiverSetSelectState Unable) ids

changeReceivers :: IdFun (ReceiverStateHandle ps) -> [Id] -> GUI ps ()
changeReceivers changeReceiverState []  = return ()
changeReceivers changeReceiverState ids = 
		do {
			(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
			if   not found
			then return ()
			else
			let  rsHs  = rReceivers $ receiverSystemStateGetReceiverHandles rDevice
			     rsHs1 = changereceiverstates changeReceiverState ids rsHs
			in
			do appIOEnv (ioStSetDevice (ReceiverSystemState (ReceiverHandles {rReceivers=rsHs1})));
		  }
	where
		changereceiverstates :: IdFun (ReceiverStateHandle ps) -> [Id] -> [ReceiverStateHandle ps] -> [ReceiverStateHandle ps]
		changereceiverstates _ [] _ = []
		changereceiverstates _ _ [] = []
		changereceiverstates f ids (rsH@(ReceiverStateHandle _ (ReceiverHandle {rId=rId})) : rsHs)
			| hadId       = (f rsH) : rsHs1
			| otherwise   =    rsH  : rsHs1
			where
				(hadId,_,ids1) = remove ((==) rId) (dummy "changereceiverstates") ids
				rsHs1 = changereceiverstates f ids1 rsHs
		


--	Get the SelectState of a receiver:

getReceiverSelectState :: Id -> GUI ps (Maybe SelectState)
getReceiverSelectState id
		= do {
			(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
			if   not found
			then return Nothing
			else 
			let  rsHs           = rReceivers $ receiverSystemStateGetReceiverHandles rDevice
			     (select,rsHs1) = getselectstate id rsHs
			in   appIOEnv (ioStSetDevice (ReceiverSystemState (ReceiverHandles {rReceivers=rsHs1}))) >> return select
		  }
	where
		getselectstate :: Id -> [ReceiverStateHandle ps] -> (Maybe SelectState,[ReceiverStateHandle ps])
		getselectstate id (rsH@(ReceiverStateHandle _ rH@(ReceiverHandle {rSelect=rSelect})) : rsHs)
			| receiverIdentified id rH = (Just rSelect, rsH:rsHs )
			| otherwise                = (select,       rsH:rsHs1)
			where
				(select,rsHs1)     = getselectstate id rsHs
		getselectstate _ _ = (Nothing,[])


--	Message passing:


{-	Asynchronous, uni-directional, message passing.
	If the receiver could not be found in the global ReceiverTable, 
		then return SendUnknownReceiver and do nothing.
	If the receiver could be found, then increase the length of the 
		asynchronous message queue of the receiver in the global 
		ReceiverTable.
		Add the message to the asynchronous message queue of the 
		receiver using the scheduler.
-}

asyncSend :: RId msg -> msg -> GUI ps ()
asyncSend rid msg = do
	it <- ioStGetIdTable
	unless (Map.member (rIdtoId rid) it) (throwGUI ErrorUnknownObject)
	liftIO (modifyIORef (getRIdIn rid) ((:) msg))	-- Put the msg in the async queue of the receiver
	ioStAppendEvents [ScheduleMsgEvent (rIdtoId rid)]
	return ()

{-	Synchronous message passing.
	If the receiver could not be found in the global ReceiverTable,
		then raise ErrorUnknownObject exception.
	If the receiver could be found, then let the receiver handle the
		synchronous message using the scheduler.
-}
syncSend :: RId msg -> msg -> ps -> GUI ps ps
syncSend rid msg ps = do
	it <- ioStGetIdTable
	unless (Map.member (rIdtoId rid) it) (throwGUI ErrorUnknownObject)
	liftIO (modifyIORef (getRIdIn rid) ((:) msg))	-- Put the msg in the async queue of the receiver
	(_,ps) <- liftContextIO (handleMsgEvent (rIdtoId rid)) ps
	return ps

{-	Synchronous bi-directional message passing.
	If the receiver could not be found in the global ReceiverTable,
		then raise ErrorUnknownObject exception.
	If the receiver could be found, then let the receiver handle the
		synchronous message using the scheduler.
-}
syncSend2 :: R2Id msg resp -> msg -> ps -> GUI ps (resp,ps)
syncSend2 rid msg ps = do
	it <- ioStGetIdTable
	unless (Map.member (r2IdtoId rid) it) (throwGUI ErrorUnknownObject)
	liftIO (writeIORef (getR2IdIn rid) msg)	-- Put the msg in the input of the receiver
	(_,ps) <- liftContextIO (handleMsgEvent (r2IdtoId rid)) ps
	out <- liftIO (readIORef (getR2IdOut rid))
	liftIO (writeIORef (getR2IdOut rid) undefined)  -- clearing receiver output
	return (out,ps)

handleMsgEvent :: Id -> Context -> IO [Int]
handleMsgEvent rId context = handleContextOSEvent context (ScheduleMsgEvent rId)
