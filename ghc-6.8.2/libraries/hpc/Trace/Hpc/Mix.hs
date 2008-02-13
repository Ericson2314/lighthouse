{-# OPTIONS -cpp #-}
---------------------------------------------------------------
-- Colin Runciman and Andy Gill, June 2006
---------------------------------------------------------------

-- |Datatypes and file-access routines for the per-module (.mix) 
--  indexes used by Hpc.
module Trace.Hpc.Mix
	( Mix(..)
	, MixEntry
	, BoxLabel(..)
	, CondBox(..)
	, mixCreate
	, readMix
	, Trace.Hpc.Mix.getModificationTime
	, createMixEntryDom
	, MixEntryDom
	)
  where

import System.Time (ClockTime(..))
import System.Directory (getModificationTime)
import System.IO   (FilePath)
import Data.Maybe (catMaybes)
import Data.Tree
import Data.Char

-- a module index records the attributes of each tick-box that has
-- been introduced in that module, accessed by tick-number position
-- in the list

import Trace.Hpc.Util (HpcPos, insideHpcPos, Hash, HpcHash(..))
import Trace.Hpc.Tix 

-- | 'Mix' is the information about a modules static properties, like 
-- location of Tix's in a file.
-- tab stops are the size of a tab in the provided line:colunm values.
--  * In GHC, this is 1 (a tab is just a character)
--  * With hpc-tracer, this is 8 (a tab represents several spaces).

data Mix = Mix 
	     FilePath		-- location of original file
	     Integer		-- time (in seconds) of original file's last update, since 1970.
	     Hash		-- hash of mix entry + timestamp
	     Int 		-- tab stop value.
	     [MixEntry] 	-- entries
	deriving (Show,Read)

-- We would rather use ClockTime in Mix, but ClockTime has no Read instance in 6.4 and before,
-- but does in 6.6. Definining the instance for ClockTime here is the Wrong Thing to do,
-- because if some other program also defined that instance, we will not be able to compile.

type MixEntry = (HpcPos, BoxLabel)

data BoxLabel = ExpBox  Bool -- isAlt 
              | TopLevelBox [String]
              | LocalBox [String]
	      | BinBox CondBox Bool
              deriving (Read, Show, Eq, Ord)

data CondBox = GuardBinBox
             | CondBinBox
             | QualBinBox
              deriving (Read, Show, Eq, Ord)

instance HpcHash BoxLabel where
   toHash (ExpBox b)       = 0x100 + toHash b
   toHash (TopLevelBox nm) = 0x200 + toHash nm
   toHash (LocalBox nm)    = 0x300 + toHash nm
   toHash (BinBox cond b)  = 0x400 + toHash (cond,b) 

instance HpcHash CondBox where
   toHash GuardBinBox = 0x10 
   toHash CondBinBox  = 0x20  
   toHash QualBinBox  = 0x30

                         
-- | Create is mix file.
mixCreate :: String -- ^ Dir Name
	  -> String -- ^ module Name
	  -> Mix    -- ^ Mix DataStructure
	  -> IO ()
mixCreate dirName modName mix =
   writeFile (mixName dirName modName) (show mix)

-- | Read a mix file.
readMix :: [String]   	    	    -- ^ Dir Names
	-> Either String TixModule  -- ^ module wanted
	-> IO Mix
readMix dirNames mod' = do
   let modName = case mod' of
		    Left str -> str
		    Right tix -> tixModuleName tix
   res <- sequence [ (do contents <- readFile (mixName dirName modName)
		         case reads contents of
			   [(r@(Mix _ _ h _ _),cs)] 
				| all isSpace cs 
			       && (case mod' of
				     Left  _   -> True
		                     Right tix -> h == tixModuleHash tix
		                  ) -> return $ Just r
			   _ -> return $ Nothing) `catch` (\ _ -> return $ Nothing)			
		   | dirName <- dirNames
		   ] 
   case catMaybes res of
     [r] -> return r
     xs@(_:_) -> error $ "found " ++ show(length xs) ++ " instances of " ++ modName ++ " in " ++ show dirNames
     _        -> error $ "can not find " ++ modName ++ " in " ++ show dirNames	

mixName :: FilePath -> String -> String
mixName dirName name = dirName ++ "/" ++ name ++ ".mix"

-- | Get modification time of a file.

getModificationTime :: FilePath -> IO Integer
getModificationTime file = do
  (TOD sec _) <- System.Directory.getModificationTime file
  return $ sec

------------------------------------------------------------------------------

type MixEntryDom a = Tree (HpcPos,a)

-- A good tree has all its children fully inside its parents HpcPos.
-- No child should have the *same* HpcPos.
-- There is no ordering to the children

isGoodNode :: MixEntryDom a -> Bool
isGoodNode (Node (pos,_) sub_nodes) = 
      and [ pos' `insideHpcPos` pos  | Node(pos',_)  _ <- sub_nodes ] 
   && and [ pos' /= pos | Node(pos',_) _ <- sub_nodes ] 
   && isGoodForest sub_nodes

-- all sub-trees are good trees, and no two HpcPos are inside each other.
isGoodForest :: [MixEntryDom a] -> Bool
isGoodForest sub_nodes =
   all isGoodNode sub_nodes
   && and [  not (pos1 `insideHpcPos` pos2 ||
      	     	  pos2 `insideHpcPos` pos1)
	  | (Node (pos1,_) _,n1) <- zip sub_nodes [0..]
	  , (Node (pos2,_) _,n2) <- zip sub_nodes [0..]
	  , (n1 :: Int) /= n2 ]

addNodeToTree :: (Show a) => (HpcPos,a) -> MixEntryDom [a] -> MixEntryDom [a]
addNodeToTree (new_pos,new_a) (Node (pos,a) children) 
  | pos == new_pos = Node (pos,new_a : a) children
  | new_pos `insideHpcPos` pos = 
       Node (pos,a) (addNodeToList (new_pos,new_a) children)
  | pos `insideHpcPos` new_pos =
       error "precondition not met inside addNodeToNode"
  | otherwise = error "something impossible happened in addNodeToTree"

addNodeToList :: Show a => (HpcPos,a) -> [MixEntryDom [a]] -> [MixEntryDom [a]]
addNodeToList (new_pos,new_a) entries 
  | otherwise =
  if length [ () 
          | (am_inside,am_outside,_) <- entries'
          , am_inside || am_outside 
	  ] == 0
     -- The case where we have a new HpcPos range
     then Node (new_pos,[new_a]) [] : entries else
  if length [ () 
     	    | (am_inside,_,_) <- entries' 
     	    , am_inside
	    ] > 0
     -- The case where we are recursing into a tree
     -- Note we can recurse down many branches, in the case of
     -- overlapping ranges.
     -- Assumes we have captures the new HpcPos 
     -- (or the above conditional would be true)
     then [ if i_am_inside  -- or the same as 
     	    then addNodeToTree (new_pos,new_a) node
	    else node
	  | (i_am_inside,_,node) <- entries'
	  ] else
     -- The case of a super-range.
     ( Node (new_pos,[new_a]) 
	     [ node | (_,True,node) <- entries' ] :
       [ node | (_,False,node) <- entries' ]
     )
  where
    entries' = [ ( new_pos `insideHpcPos` pos
    	         , pos  `insideHpcPos` new_pos
		 , node)
    	       | node@(Node (pos,_) _) <- entries
	       ]

createMixEntryDom :: (Show a) => [(HpcPos,a)] -> [MixEntryDom [a]]
createMixEntryDom entries 
    | isGoodForest forest = forest
    | otherwise = error "createMixEntryDom: bad forest"
  where forest = foldr addNodeToList [] entries
