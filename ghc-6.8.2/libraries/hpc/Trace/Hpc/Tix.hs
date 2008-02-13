------------------------------------------------------------
-- Andy Gill and Colin Runciman, June 2006
------------------------------------------------------------

-- | Datatypes and file-access routines for the tick data file 
-- used by HPC. (.tix)
module Trace.Hpc.Tix(Tix(..), TixModule(..), 
		     tixModuleName, tixModuleHash, tixModuleTixs,
		     readTix, writeTix, getTixFileName) where

import Data.List (isSuffixOf)
import Trace.Hpc.Util(Hash)

-- 'Tix ' is the storage format for our dynamic imformation about what
-- boxes are ticked.
data Tix = Tix [TixModule]
	deriving (Read, Show, Eq)

data TixModule = TixModule 
		 String    -- module name
		 Hash	   -- hash number
		 Int 	   -- length of tix list (allows pre-allocation at parse time).
		 [Integer] --  actual ticks
	deriving (Read, Show, Eq)

tixModuleName :: TixModule -> String
tixModuleName (TixModule nm _ _ _) = nm
tixModuleHash :: TixModule -> Hash
tixModuleHash (TixModule _ h  _ _) = h
tixModuleTixs :: TixModule -> [Integer]
tixModuleTixs (TixModule  _ _ _ tixs) = tixs

-- We /always/ read and write Tix from the current working directory.

-- read a Tix File.
readTix :: String
	-> IO (Maybe Tix)
readTix tix_filename = 
  catch (do contents <- readFile $ tix_filename
	    return $ Just $ read contents)
	(\ _ -> return $ Nothing)

-- write a Tix File.
writeTix :: String 
	 -> Tix 
	 -> IO ()
writeTix name tix = 
  writeFile name (show tix)

{-
tixName :: String -> String
tixName name = name ++ ".tix"
-}

-- getTixFullName takes a binary or .tix-file name,
-- and normalizes it into a .tix-file name.
getTixFileName :: String -> String
getTixFileName str | ".tix" `isSuffixOf` str 
		   = str
		   | otherwise
		   = str ++ ".tix"

