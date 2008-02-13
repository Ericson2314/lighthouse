{-# OPTIONS -fno-warn-missing-signatures #-}
-- | Clean out unneeded spill/reload instrs
--
-- * Handling of join points
--
--   B1:                          B2:
--    ...                          ...
--       RELOAD SLOT(0), %r1          RELOAD SLOT(0), %r1
--       ... A ...                    ... B ...
--       jump B3                      jump B3
--
--                B3: ... C ...
--                    RELOAD SLOT(0), %r1
--                    ...
--
-- the plan:
--	So long as %r1 hasn't been written to in A, B or C then we don't need the
--	reload in B3.
--
--	What we really care about here is that on the entry to B3, %r1 will always
--	have the same value that is in SLOT(0) (ie, %r1 is _valid_)
--
--	This also works if the reloads in B1/B2 were spills instead, because
--	spilling %r1 to a slot makes that slot have the same value as %r1.
--

module RegSpillClean (
	cleanSpills
)
where

import RegLiveness
import RegAllocInfo
import MachRegs
import MachInstrs
import Cmm

import UniqSet
import UniqFM
import Unique
import State
import Outputable

import Data.Maybe
import Data.List

#if __GLASGOW_HASKELL__ < 603
import Util (foldl1')
#endif

--
type Slot = Int


-- | Clean out unneeded spill/reloads from this top level thing.
cleanSpills :: LiveCmmTop -> LiveCmmTop
cleanSpills cmm
	= evalState (cleanSpin 0 cmm) initCleanS

-- | do one pass of cleaning
cleanSpin :: Int -> LiveCmmTop -> CleanM LiveCmmTop

{-
cleanSpin spinCount code
 = do	jumpValid	<- gets sJumpValid
	pprTrace "cleanSpin"
	 	(  int spinCount
		$$ text "--- code"
		$$ ppr code
		$$ text "--- joins"
		$$ ppr jumpValid)
	 $ cleanSpin' spinCount code
-}

cleanSpin spinCount code
 = do
 	-- init count of cleaned spills/reloads
	modify $ \s -> s
		{ sCleanedSpillsAcc	= 0
		, sCleanedReloadsAcc	= 0
		, sReloadedBy		= emptyUFM }

 	code_forward	<- mapBlockTopM cleanBlockForward  code
	code_backward	<- mapBlockTopM cleanBlockBackward code_forward

	-- During the cleaning of each block we collected information about what regs
	--	were valid across each jump. Based on this, work out whether it will be
	--	safe to erase reloads after join points for the next pass.
	collateJoinPoints

	-- remember how many spills/reloads we cleaned in this pass
	spills		<- gets sCleanedSpillsAcc
	reloads		<- gets sCleanedReloadsAcc
	modify $ \s -> s
		{ sCleanedCount	= (spills, reloads) : sCleanedCount s }

	-- if nothing was cleaned in this pass or the last one
	--	then we're done and it's time to bail out
	cleanedCount	<- gets sCleanedCount
	if take 2 cleanedCount == [(0, 0), (0, 0)]
	   then return code

	-- otherwise go around again
	   else cleanSpin (spinCount + 1) code_backward


-- | Clean one basic block
cleanBlockForward :: LiveBasicBlock -> CleanM LiveBasicBlock
cleanBlockForward (BasicBlock blockId instrs)
 = do
 	-- see if we have a valid association for the entry to this block
 	jumpValid	<- gets sJumpValid
 	let assoc	= case lookupUFM jumpValid blockId of
				Just assoc	-> assoc
				Nothing		-> emptyAssoc

 	instrs_reload	<- cleanForward    blockId assoc [] instrs
	return	$ BasicBlock blockId instrs_reload


cleanBlockBackward :: LiveBasicBlock -> CleanM LiveBasicBlock
cleanBlockBackward (BasicBlock blockId instrs)
 = do	instrs_spill	<- cleanBackward  emptyUniqSet  [] instrs
	return	$ BasicBlock blockId instrs_spill




-- | Clean out unneeded reload instructions.
--	Walking forwards across the code
--	  On a reload, if we know a reg already has the same value as a slot
--	  then we don't need to do the reload.
--
cleanForward
	:: BlockId		-- ^ the block that we're currently in
	-> Assoc Store	 	-- ^ two store locations are associated if they have the same value
	-> [LiveInstr]		-- ^ acc
	-> [LiveInstr] 		-- ^ instrs to clean (in backwards order)
	-> CleanM [LiveInstr]	-- ^ cleaned instrs  (in forward   order)

cleanForward _ _ acc []
	= return acc

-- write out live range joins via spill slots to just a spill and a reg-reg move
--	hopefully the spill will be also be cleaned in the next pass
--
cleanForward blockId assoc acc (Instr i1 live1 : Instr i2 _ : instrs)

	| SPILL  reg1  slot1	<- i1
	, RELOAD slot2 reg2	<- i2
	, slot1 == slot2
	= do
		modify $ \s -> s { sCleanedReloadsAcc = sCleanedReloadsAcc s + 1 }
		cleanForward blockId assoc acc
			(Instr i1 live1 : Instr (mkRegRegMoveInstr reg1 reg2) Nothing : instrs)


cleanForward blockId assoc acc (li@(Instr i1 _) : instrs)
	| Just (r1, r2)	<- isRegRegMove i1
	= if r1 == r2
		-- erase any left over nop reg reg moves while we're here
		--	this will also catch any nop moves that the "write out live range joins" case above
		--	happens to add
		then cleanForward blockId assoc acc instrs

		-- if r1 has the same value as some slots and we copy r1 to r2,
		--	then r2 is now associated with those slots instead
		else do	let assoc'	= addAssoc (SReg r1) (SReg r2)
					$ delAssoc (SReg r2)
					$ assoc

			cleanForward blockId assoc' (li : acc) instrs


cleanForward blockId assoc acc (li@(Instr instr _) : instrs)

	-- update association due to the spill
	| SPILL reg slot	<- instr
	= let	assoc'	= addAssoc (SReg reg)  (SSlot slot)
			$ delAssoc (SSlot slot)
			$ assoc
	  in	cleanForward blockId assoc' (li : acc) instrs

	-- clean a reload instr
	| RELOAD{}		<- instr
	= do	(assoc', mli)	<- cleanReload blockId assoc li
		case mli of
		 Nothing	-> cleanForward blockId assoc' acc 		instrs
		 Just li'	-> cleanForward blockId assoc' (li' : acc)	instrs

	-- remember the association over a jump
	| targets	<- jumpDests instr []
	, not $ null targets
	= do	mapM_ (accJumpValid assoc) targets
		cleanForward blockId assoc (li : acc) instrs

	-- writing to a reg changes its value.
	| RU _ written	<- regUsage instr
	= let assoc'	= foldr delAssoc assoc (map SReg $ nub written)
	  in  cleanForward blockId assoc' (li : acc) instrs


-- | Try and rewrite a reload instruction to something more pleasing
--
cleanReload :: BlockId -> Assoc Store -> LiveInstr -> CleanM (Assoc Store, Maybe LiveInstr)
cleanReload blockId assoc li@(Instr (RELOAD slot reg) _)

	-- if the reg we're reloading already has the same value as the slot
	--	then we can erase the instruction outright
	| elemAssoc (SSlot slot) (SReg reg) assoc
	= do 	modify 	$ \s -> s { sCleanedReloadsAcc = sCleanedReloadsAcc s + 1 }
	   	return	(assoc, Nothing)

	-- if we can find another reg with the same value as this slot then
	--	do a move instead of a reload.
	| Just reg2	<- findRegOfSlot assoc slot
	= do	modify $ \s -> s { sCleanedReloadsAcc = sCleanedReloadsAcc s + 1 }

		let assoc'	= addAssoc (SReg reg) (SReg reg2)
				$ delAssoc (SReg reg)
				$ assoc

		return	(assoc', Just $ Instr (mkRegRegMoveInstr reg2 reg) Nothing)

	-- gotta keep this instr
	| otherwise
	= do	-- update the association
		let assoc'	= addAssoc (SReg reg)  (SSlot slot)	-- doing the reload makes reg and slot the same value
				$ delAssoc (SReg reg)			-- reg value changes on reload
				$ assoc

		-- remember that this block reloads from this slot
		accBlockReloadsSlot blockId slot

	    	return	(assoc', Just li)

cleanReload _ _ _
	= panic "RegSpillClean.cleanReload: unhandled instr"


-- | Clean out unneeded spill instructions.
--
--	 If there were no reloads from a slot between a spill and the last one
--	 then the slot was never read and we don't need the spill.
--
--	SPILL   r0 -> s1
--	RELOAD  s1 -> r2
--	SPILL   r3 -> s1	<--- don't need this spill
--	SPILL   r4 -> s1
--	RELOAD  s1 -> r5
--
--	Maintain a set of
--		"slots which were spilled to but not reloaded from yet"
--
--	Walking backwards across the code:
--	 a) On a reload from a slot, remove it from the set.
--
--	 a) On a spill from a slot
--		If the slot is in set then we can erase the spill,
--			because it won't be reloaded from until after the next spill.
--
--		otherwise
--			keep the spill and add the slot to the set
--
-- TODO: This is mostly inter-block
--	 we should really be updating the noReloads set as we cross jumps also.
--
cleanBackward
	:: UniqSet Int 		-- ^ slots that have been spilled, but not reloaded from
	-> [LiveInstr]		-- ^ acc
	-> [LiveInstr]		-- ^ instrs to clean (in forwards order)
	-> CleanM [LiveInstr]	-- ^ cleaned instrs  (in backwards order)


cleanBackward noReloads acc lis
 = do	reloadedBy	<- gets sReloadedBy
 	cleanBackward' reloadedBy noReloads acc lis

cleanBackward' _ _      acc []
	= return  acc

cleanBackward' reloadedBy noReloads acc (li@(Instr instr _) : instrs)

	-- if nothing ever reloads from this slot then we don't need the spill
	| SPILL _ slot	<- instr
	, Nothing	<- lookupUFM reloadedBy (SSlot slot)
	= do	modify $ \s -> s { sCleanedSpillsAcc = sCleanedSpillsAcc s + 1 }
		cleanBackward noReloads acc instrs

	| SPILL _ slot	<- instr
	= if elementOfUniqSet slot noReloads

	   -- we can erase this spill because the slot won't be read until after the next one
	   then do
		modify $ \s -> s { sCleanedSpillsAcc = sCleanedSpillsAcc s + 1 }
	   	cleanBackward noReloads acc instrs

	   else do
		-- this slot is being spilled to, but we haven't seen any reloads yet.
		let noReloads'	= addOneToUniqSet noReloads slot
	   	cleanBackward noReloads' (li : acc) instrs

	-- if we reload from a slot then it's no longer unused
	| RELOAD slot _		<- instr
	, noReloads'		<- delOneFromUniqSet noReloads slot
	= cleanBackward noReloads' (li : acc) instrs

	-- some other instruction
	| otherwise
	= cleanBackward noReloads (li : acc) instrs


-- collateJoinPoints:
--
-- | combine the associations from all the inward control flow edges.
--
collateJoinPoints :: CleanM ()
collateJoinPoints
 = modify $ \s -> s
 	{ sJumpValid	= mapUFM intersects (sJumpValidAcc s)
	, sJumpValidAcc	= emptyUFM }

intersects :: [Assoc Store]	-> Assoc Store
intersects []		= emptyAssoc
intersects assocs	= foldl1' intersectAssoc assocs


-- | See if we have a reg with the same value as this slot in the association table.
findRegOfSlot :: Assoc Store -> Int -> Maybe Reg
findRegOfSlot assoc slot
	| close			<- closeAssoc (SSlot slot) assoc
	, Just (SReg reg)	<- find isStoreReg $ uniqSetToList close
	= Just reg

	| otherwise
	= Nothing


---------------
type CleanM = State CleanS
data CleanS
	= CleanS
	{ -- regs which are valid at the start of each block.
	  sJumpValid		:: UniqFM (Assoc Store)

 	  -- collecting up what regs were valid across each jump.
	  --	in the next pass we can collate these and write the results
	  --	to sJumpValid.
	, sJumpValidAcc		:: UniqFM [Assoc Store]

	  -- map of (slot -> blocks which reload from this slot)
	  --	used to decide if whether slot spilled to will ever be
	  --	reloaded from on this path.
	, sReloadedBy		:: UniqFM [BlockId]

	  -- spills/reloads cleaned each pass (latest at front)
	, sCleanedCount		:: [(Int, Int)]

	  -- spills/reloads that have been cleaned in this pass so far.
	, sCleanedSpillsAcc	:: Int
	, sCleanedReloadsAcc	:: Int }

initCleanS :: CleanS
initCleanS
	= CleanS
	{ sJumpValid		= emptyUFM
	, sJumpValidAcc		= emptyUFM

	, sReloadedBy		= emptyUFM

	, sCleanedCount		= []

	, sCleanedSpillsAcc	= 0
	, sCleanedReloadsAcc	= 0 }


-- | Remember the associations before a jump
accJumpValid :: Assoc Store -> BlockId -> CleanM ()
accJumpValid assocs target
 = modify $ \s -> s {
	sJumpValidAcc = addToUFM_C (++)
				(sJumpValidAcc s)
				target
				[assocs] }


accBlockReloadsSlot :: BlockId -> Slot -> CleanM ()
accBlockReloadsSlot blockId slot
 = modify $ \s -> s {
 	sReloadedBy = addToUFM_C (++)
				(sReloadedBy s)
				(SSlot slot)
				[blockId] }


--------------
-- A store location can be a stack slot or a register
--
data Store
	= SSlot Int
	| SReg  Reg

-- | Check if this is a reg store
isStoreReg :: Store -> Bool
isStoreReg ss
 = case ss of
 	SSlot _	-> False
	SReg  _	-> True

-- spill cleaning is only done once all virtuals have been allocated to realRegs
--
instance Uniquable Store where
    getUnique (SReg  r)
	| RealReg i	<- r
	= mkUnique 'R' i

	| otherwise
	= error "RegSpillClean.getUnique: found virtual reg during spill clean, only real regs expected."

    getUnique (SSlot i)			= mkUnique 'S' i

instance Outputable Store where
	ppr (SSlot i)	= text "slot" <> int i
	ppr (SReg  r)	= ppr r


--------------
-- Association graphs.
--	In the spill cleaner, two store locations are associated if they are known
--	to hold the same value.
--
type Assoc a	= UniqFM (UniqSet a)

-- | an empty association
emptyAssoc :: Assoc a
emptyAssoc	= emptyUFM


-- | add an association between these two things
addAssoc :: Uniquable a
	 => a -> a -> Assoc a -> Assoc a

addAssoc a b m
 = let	m1	= addToUFM_C unionUniqSets m  a (unitUniqSet b)
 	m2	= addToUFM_C unionUniqSets m1 b (unitUniqSet a)
   in	m2


-- | delete all associations to a node
delAssoc :: (Outputable a, Uniquable a)
 	 => a -> Assoc a -> Assoc a

delAssoc a m
	| Just aSet	<- lookupUFM  m a
	, m1		<- delFromUFM m a
	= foldUniqSet (\x m -> delAssoc1 x a m) m1 aSet

	| otherwise	= m


-- | delete a single association edge (a -> b)
delAssoc1 :: Uniquable a
	=> a -> a -> Assoc a -> Assoc a

delAssoc1 a b m
	| Just aSet	<- lookupUFM m a
	= addToUFM m a (delOneFromUniqSet aSet b)

	| otherwise	= m


-- | check if these two things are associated
elemAssoc :: (Outputable a, Uniquable a)
	  => a -> a -> Assoc a -> Bool

elemAssoc a b m
	= elementOfUniqSet b (closeAssoc a m)

-- | find the refl. trans. closure of the association from this point
closeAssoc :: (Outputable a, Uniquable a)
	=> a -> Assoc a -> UniqSet a

closeAssoc a assoc
 = 	closeAssoc' assoc emptyUniqSet (unitUniqSet a)
 where
	closeAssoc' assoc visited toVisit
	 = case uniqSetToList toVisit of

		-- nothing else to visit, we're done
	 	[]	-> visited

		(x:_)

		 -- we've already seen this node
		 |  elementOfUniqSet x visited
		 -> closeAssoc' assoc visited (delOneFromUniqSet toVisit x)

		 -- haven't seen this node before,
		 --	remember to visit all its neighbors
		 |  otherwise
		 -> let neighbors
		 	 = case lookupUFM assoc x of
				Nothing		-> emptyUniqSet
				Just set	-> set

		   in closeAssoc' assoc
			(addOneToUniqSet visited x)
			(unionUniqSets   toVisit neighbors)

-- | intersect
intersectAssoc
	:: Uniquable a
	=> Assoc a -> Assoc a -> Assoc a

intersectAssoc a b
 	= intersectUFM_C (intersectUniqSets) a b

