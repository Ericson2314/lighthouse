-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Id
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Id defines the various kinds of identification values that identify GUI
-- objects. In addition, an IdTable (implemented as 'FiniteMap') is defined
-- in which all bound GUI objects are administered. 
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Id
          ( Id, RId, R2Id, IdTable
          , IdParent (..)          
          , toId, toRId, toR2Id
          , r2IdtoId, rIdtoId
          , getRIdIn, getR2IdIn, getR2IdOut
          , okMembersIdTable
          , module Graphics.UI.ObjectIO.Device.Types
          , module Graphics.UI.ObjectIO.SystemId
          ) where



import Graphics.UI.ObjectIO.Device.Types
import Graphics.UI.ObjectIO.SystemId
import Data.IORef
import Data.Unique
import qualified Data.Map as Map

type	Id = Unique

data	RId mess					-- The identification of bi-directional receivers:
 =	RId
 		{ rid        :: !Unique			-- The identification value
 		, ridIn      :: !(IORef [mess])		-- The message input  channel
 		}
 		
data	R2Id mess resp					-- The identification of bi-directional receivers:
 =	R2Id
 		{ r2id       :: !Unique			-- The identification value
 		, r2idIn     :: !(IORef mess)		-- The message input
 		, r2idOut    :: !(IORef resp)		-- The message output
 		}
 		
type	IdTable = Map.Map Unique IdParent		-- all Id entries

data	IdParent
	= IdParent
		{ idpIOId     :: !SystemId		-- Id of parent process
		, idpDevice   :: !Device		-- Device kind of parent GUI object
		, idpId       :: !Id			-- Id of parent GUI object
		}


toId :: Unique -> Id
toId i = i

toRId :: Unique -> IORef [mess] -> RId mess
toRId i cIn = RId {rid=i,ridIn=cIn}

toR2Id :: Unique -> IORef mess -> IORef resp -> R2Id mess resp
toR2Id i cIn cOut = R2Id {r2id=i,r2idIn=cIn,r2idOut=cOut}

instance Eq (RId mess) where
	(==) rid1 rid2 = rid rid1 == rid rid2

instance Eq (R2Id mess resp) where
	(==) rid1 rid2 = r2id rid1 == r2id rid2


rIdtoId :: RId mess -> Id
rIdtoId id = rid id

r2IdtoId :: R2Id mess resp -> Id
r2IdtoId id = r2id id


instance Show Id where
	show id = "Id"


{-	Additional R(2)Id access operations:
-}

getRIdIn :: RId msg -> IORef [msg]
getRIdIn id = ridIn id

getR2IdIn :: R2Id msg resp -> IORef msg
getR2IdIn id = r2idIn id

getR2IdOut :: R2Id msg resp -> IORef resp
getR2IdOut id = r2idOut id


--	IdTable operations:

okMembersIdTable :: [Id] -> IdTable -> Bool
okMembersIdTable ids tbl
	= noDuplicates ids && not (any (\key -> elem key $ Map.keys tbl) ids)
	where
		noDuplicates :: (Eq x) => [x] -> Bool
		noDuplicates (x:xs) = not (x `elem` xs) && noDuplicates xs
		noDuplicates _      = True



		





