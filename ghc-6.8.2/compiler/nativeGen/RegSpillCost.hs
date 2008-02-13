
module RegSpillCost (
	SpillCostRecord,
	plusSpillCostRecord,
	pprSpillCostRecord,

	SpillCostInfo,
	zeroSpillCostInfo,
	plusSpillCostInfo,

	slurpSpillCostInfo,
	chooseSpill,

	lifeMapFromSpillCostInfo
)

where

import GraphBase
import RegLiveness
import RegAllocInfo
import MachInstrs
import MachRegs
import Cmm

import UniqFM
import UniqSet
import Outputable
import State

import Data.List	(nub, minimumBy)
import Data.Maybe
import Control.Monad

type SpillCostRecord
 = 	( Reg	-- register name
	, Int	-- number of writes to this reg
	, Int	-- number of reads from this reg
	, Int)	-- number of instrs this reg was live on entry to

type SpillCostInfo
	= UniqFM SpillCostRecord


zeroSpillCostInfo :: SpillCostInfo
zeroSpillCostInfo	= emptyUFM

-- | Add two spillCostInfos
plusSpillCostInfo :: SpillCostInfo -> SpillCostInfo -> SpillCostInfo
plusSpillCostInfo sc1 sc2
	= plusUFM_C plusSpillCostRecord sc1 sc2

plusSpillCostRecord :: SpillCostRecord -> SpillCostRecord -> SpillCostRecord
plusSpillCostRecord (r1, a1, b1, c1) (r2, a2, b2, c2)
	| r1 == r2	= (r1, a1 + a2, b1 + b2, c1 + c2)
	| otherwise	= error "RegSpillCost.plusRegInt: regs don't match"


-- | Slurp out information used for determining spill costs
--	for each vreg, the number of times it was written to, read from,
--	and the number of instructions it was live on entry to (lifetime)
--
slurpSpillCostInfo
	:: LiveCmmTop
	-> SpillCostInfo

slurpSpillCostInfo cmm
	= execState (countCmm cmm) zeroSpillCostInfo
 where
	countCmm CmmData{}		= return ()
	countCmm (CmmProc info _ _ (ListGraph blocks))
		= mapM_ (countComp info) blocks

	countComp info (BasicBlock _ blocks)
		= mapM_ (countBlock info) blocks

	-- lookup the regs that are live on entry to this block in
	--	the info table from the CmmProc
 	countBlock info (BasicBlock blockId instrs)
		| LiveInfo _ _ blockLive	<- info
		, Just rsLiveEntry		<- lookupUFM blockLive blockId
		= countLIs rsLiveEntry instrs

		| otherwise
		= error "RegLiveness.slurpSpillCostInfo: bad block"

	countLIs _      []
		= return ()

	-- skip over comment and delta pseudo instrs
	countLIs rsLive (Instr instr Nothing : lis)
		| COMMENT{}	<- instr
		= countLIs rsLive lis

		| DELTA{}	<- instr
		= countLIs rsLive lis

		| otherwise
		= pprPanic "RegSpillCost.slurpSpillCostInfo"
			(text "no liveness information on instruction " <> ppr instr)

	countLIs rsLiveEntry (Instr instr (Just live) : lis)
	 = do
		-- increment the lifetime counts for regs live on entry to this instr
		mapM_ incLifetime $ uniqSetToList rsLiveEntry

		-- increment counts for what regs were read/written from
		let (RU read written)	= regUsage instr
		mapM_ incUses	$ filter (not . isRealReg) $ nub read
		mapM_ incDefs 	$ filter (not . isRealReg) $ nub written

		-- compute liveness for entry to next instruction.
	 	let rsLiveAcross
			= rsLiveEntry `minusUniqSet` (liveDieRead live)

	 	let rsLiveNext
			= (rsLiveAcross `unionUniqSets` (liveBorn     live))
				        `minusUniqSet` (liveDieWrite live)

	 	countLIs rsLiveNext lis

	incDefs     reg = modify $ \s -> addToUFM_C plusSpillCostRecord s reg (reg, 1, 0, 0)
	incUses	    reg	= modify $ \s -> addToUFM_C plusSpillCostRecord s reg (reg, 0, 1, 0)
	incLifetime reg	= modify $ \s -> addToUFM_C plusSpillCostRecord s reg (reg, 0, 0, 1)


-- | Choose a node to spill from this graph

chooseSpill
	:: SpillCostInfo
	-> Graph Reg RegClass Reg
	-> Reg

chooseSpill info graph
 = let	cost	= spillCost_length info graph
 	node	= minimumBy (\n1 n2 -> compare (cost $ nodeId n1) (cost $ nodeId n2))
		$ eltsUFM $ graphMap graph

   in	nodeId node



-- | Chaitins spill cost function is:
--
--          cost =     sum         loadCost * freq (u)  +    sum        storeCost * freq (d)
--                  u <- uses (v)                         d <- defs (v)
--
--	There are no loops in our code at the momemnt, so we can set the freq's to 1
--	We divide this by the degree if t
--
--
--  If we don't have live range splitting then Chaitins function performs badly if we have
--	lots of nested live ranges and very few registers.
--
--		 v1 v2 v3
--	def v1	 .
--	use v1   .
--	def v2   .  .
--	def v3   .  .  .
--	use v1   .  .  .
--	use v3   .  .  .
--	use v2   .  .
--	use v1   .
--
--
--           defs uses degree   cost
--	v1:  1     3     3      1.5
--	v2:  1     2     3      1.0
--	v3:  1     1     3      0.666
--
--  	v3 has the lowest cost, but if we only have 2 hardregs and we insert spill code for v3
--	then this isn't going to improve the colorability of the graph.
--
--  When compiling SHA1, which as very long basic blocks and some vregs with very long live ranges
--	the allocator seems to try and spill from the inside out and eventually run out of stack slots.
--
--  Without live range splitting, its's better to spill from the outside in so set the cost of very
--	long live ranges to zero
--
{-
spillCost_chaitin
	:: SpillCostInfo
	-> Graph Reg RegClass Reg
	-> Reg
	-> Float

spillCost_chaitin info graph reg
	-- Spilling a live range that only lives for 1 instruction isn't going to help
	--	us at all - and we definately want to avoid trying to re-spill previously
	--	inserted spill code.
	| lifetime <= 1		= 1/0

	-- It's unlikely that we'll find a reg for a live range this long
	--	better to spill it straight up and not risk trying to keep it around
	--	and have to go through the build/color cycle again.
	| lifetime > allocatableRegsInClass (regClass reg) * 10
	= 0

	-- otherwise revert to chaitin's regular cost function.
	| otherwise	= fromIntegral (uses + defs) / fromIntegral (nodeDegree graph reg)
	where (_, defs, uses, lifetime)
		= fromMaybe (reg, 0, 0, 0) $ lookupUFM info reg
-}

-- Just spill the longest live range.
spillCost_length
	:: SpillCostInfo
	-> Graph Reg RegClass Reg
	-> Reg
	-> Float

spillCost_length info _ reg
	| lifetime <= 1		= 1/0
	| otherwise		= 1 / fromIntegral lifetime
	where (_, _, _, lifetime)
		= fromMaybe (reg, 0, 0, 0) $ lookupUFM info reg



lifeMapFromSpillCostInfo :: SpillCostInfo -> UniqFM (Reg, Int)
lifeMapFromSpillCostInfo info
 	= listToUFM
	$ map (\(r, _, _, life)	-> (r, (r, life)))
	$ eltsUFM info


-- | Work out the degree (number of neighbors) of this node which have the same class.
nodeDegree :: Graph Reg RegClass Reg -> Reg -> Int
nodeDegree graph reg
	| Just node	<- lookupUFM (graphMap graph) reg
	, virtConflicts	<- length 	$ filter (\r -> regClass r == regClass reg)
					$ uniqSetToList $ nodeConflicts node
	= virtConflicts + sizeUniqSet (nodeExclusions node)

	| otherwise
	= 0


-- | Show a spill cost record, including the degree from the graph and final calulated spill cos
pprSpillCostRecord :: Graph Reg RegClass Reg -> SpillCostRecord -> SDoc
pprSpillCostRecord graph (reg, uses, defs, life)
 	=  hsep
	[ ppr reg
	, ppr uses
	, ppr defs
	, ppr life
	, ppr $ nodeDegree graph reg
	, text $ show $ (fromIntegral (uses + defs) / fromIntegral (nodeDegree graph reg) :: Float) ]
