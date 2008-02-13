-----------------------------------------------------------------------------
-- |
-- Module      :  StdId
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdId specifies the generation functions for identification values.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdId
		( openId,  openRId,  openR2Id
	     	, openIds, openRIds, openR2Ids
             	, getParentId
             	, Graphics.UI.ObjectIO.Id.Id
             	) where


import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.StdIOBasic(IOMonad(..))
import Graphics.UI.ObjectIO.Process.IOState(GUI(..),ioStGetIdTable)
import Data.Maybe
import Data.Unique
import Data.IORef
import qualified Data.Map as Map
import System.IO.Unsafe

------------
------------

-- | The openId function generates one new id
openId  :: IOMonad m => m Id
openId  = liftIO (fmap toId newUnique)

-- | The openIds function returns a list of N new items i.e. [id1, id2..idN]
openIds   :: IOMonad m => Int -> m [Id]
openIds n = sequence (replicate n openId)

-- | The openRId function is the same as openId but they return 
-- special bi-receiver id (see "Graphics.UI.ObjectIO.StdReceiver").
openR2Id  :: IOMonad m => m (R2Id msg resp)
openR2Id  = liftIO $ do
	u   <- newUnique
	cIn  <- newIORef undefined
	cOut <- newIORef undefined
	return (toR2Id u cIn cOut)

-- | The openR2Ids function returns a list of N new bi-receiver ids i.e. [rId1, rId2..rIdN]
openR2Ids :: IOMonad m => Int -> m [R2Id msg resp]
openR2Ids n = sequence (replicate n openR2Id)

-- | The openRId function is the same as openId but they return 
-- special bi-receiver id (see "Graphics.UI.ObjectIO.StdReceiver").
openRId  :: IOMonad m => m (RId msg)	
openRId = liftIO $ do
	u   <- newUnique
	cIn  <- newIORef []
	return (toRId u cIn)

-- | The openRIds function returns a list of N new receiver ids i.e. [rId1, rId2..rIdN]
openRIds :: IOMonad m => Int -> m [RId msg]
openRIds n = sequence (replicate n openRId)

-- | The getParentId function retrieves an id of the specified window\'s parent.
getParentId :: Id -> GUI ps (Maybe Id)
getParentId id = do
	it <- ioStGetIdTable
	return (fmap idpId (Map.lookup id it))
