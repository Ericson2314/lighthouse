-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1993-2004
-- 
-- This is the top-level module in the native code generator.
--
-- -----------------------------------------------------------------------------

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module AsmCodeGen ( nativeCodeGen ) where

#include "HsVersions.h"
#include "nativeGen/NCG.h"

import MachInstrs
import MachRegs
import MachCodeGen
import PprMach
import RegAllocInfo
import NCGMonad
import PositionIndependentCode
import RegLiveness
import RegCoalesce
import qualified RegAllocLinear	as Linear
import qualified RegAllocColor	as Color
import qualified RegAllocStats	as Color
import qualified GraphColor	as Color

import Cmm
import CmmOpt		( cmmMiniInline, cmmMachOpFold )
import PprCmm		( pprStmt, pprCmms, pprCmm )
import MachOp
import CLabel
import State

import UniqFM
import Unique		( Unique, getUnique )
import UniqSupply
import FastTypes
import List		( groupBy, sortBy )
import ErrUtils		( dumpIfSet_dyn )
import DynFlags
import StaticFlags	( opt_Static, opt_PIC )
import Util
import Config           ( cProjectVersion )
import Module

import Digraph
import qualified Pretty
import Outputable
import FastString
import UniqSet
import ErrUtils

-- DEBUGGING ONLY
--import OrdList

import Data.List
import Data.Int
import Data.Word
import Data.Bits
import Data.Maybe
import GHC.Exts
import Control.Monad
import System.IO

{-
The native-code generator has machine-independent and
machine-dependent modules.

This module ("AsmCodeGen") is the top-level machine-independent
module.  Before entering machine-dependent land, we do some
machine-independent optimisations (defined below) on the
'CmmStmts's.

We convert to the machine-specific 'Instr' datatype with
'cmmCodeGen', assuming an infinite supply of registers.  We then use
a machine-independent register allocator ('regAlloc') to rejoin
reality.  Obviously, 'regAlloc' has machine-specific helper
functions (see about "RegAllocInfo" below).

Finally, we order the basic blocks of the function so as to minimise
the number of jumps between blocks, by utilising fallthrough wherever
possible.

The machine-dependent bits break down as follows:

  * ["MachRegs"]  Everything about the target platform's machine
    registers (and immediate operands, and addresses, which tend to
    intermingle/interact with registers).

  * ["MachInstrs"]  Includes the 'Instr' datatype (possibly should
    have a module of its own), plus a miscellany of other things
    (e.g., 'targetDoubleSize', 'smStablePtrTable', ...)

  * ["MachCodeGen"]  is where 'Cmm' stuff turns into
    machine instructions.

  * ["PprMach"] 'pprInstr' turns an 'Instr' into text (well, really
    a 'Doc').

  * ["RegAllocInfo"] In the register allocator, we manipulate
    'MRegsState's, which are 'BitSet's, one bit per machine register.
    When we want to say something about a specific machine register
    (e.g., ``it gets clobbered by this instruction''), we set/unset
    its bit.  Obviously, we do this 'BitSet' thing for efficiency
    reasons.

    The 'RegAllocInfo' module collects together the machine-specific
    info needed to do register allocation.

   * ["RegisterAlloc"] The (machine-independent) register allocator.
-}

-- -----------------------------------------------------------------------------
-- Top-level of the native codegen

--------------------
nativeCodeGen :: DynFlags -> Handle -> UniqSupply -> [RawCmm] -> IO ()
nativeCodeGen dflags h us cmms
 = do
	let split_cmms	= concat $ map add_split cmms

 	(imports, prof)
		<- cmmNativeGens dflags h us split_cmms [] [] 0

	let (native, colorStats, linearStats)
		= unzip3 prof

	-- dump native code
	dumpIfSet_dyn dflags
		Opt_D_dump_asm "Asm code"
		(vcat $ map (docToSDoc . pprNatCmmTop) $ concat native)

	-- dump global NCG stats for graph coloring allocator
	(case concat $ catMaybes colorStats of
	  []	-> return ()
	  stats	-> do	
	   	-- build the global register conflict graph
		let graphGlobal	
			= foldl Color.union Color.initGraph
			$ [ Color.raGraph stat
				| stat@Color.RegAllocStatsStart{} <- stats]
	   
	   	dumpSDoc dflags Opt_D_dump_asm_stats "NCG stats"
			$ Color.pprStats stats graphGlobal

		dumpIfSet_dyn dflags
			Opt_D_dump_asm_conflicts "Register conflict graph"
			$ Color.dotGraph Color.regDotColor trivColorable
			$ graphGlobal)


	-- dump global NCG stats for linear allocator
	(case concat $ catMaybes linearStats of
		[]	-> return ()
		stats	-> dumpSDoc dflags Opt_D_dump_asm_stats "NCG stats"
				$ Linear.pprStats (concat native) stats)

	-- write out the imports
	Pretty.printDoc Pretty.LeftMode h
		$ makeImportsDoc (concat imports)

	return	()

 where	add_split (Cmm tops)
		| dopt Opt_SplitObjs dflags = split_marker : tops
		| otherwise		    = tops

	split_marker = CmmProc [] mkSplitMarkerLabel [] (ListGraph [])


-- | Do native code generation on all these cmms.
--
cmmNativeGens dflags h us [] impAcc profAcc count
	= return (reverse impAcc, reverse profAcc)

cmmNativeGens dflags h us (cmm : cmms) impAcc profAcc count
 = do
 	(us', native, imports, colorStats, linearStats)
		<- cmmNativeGen dflags us cmm count

	Pretty.printDoc Pretty.LeftMode h
		$ {-# SCC "pprNativeCode" #-} Pretty.vcat $ map pprNatCmmTop native

	let lsPprNative =
		if  dopt Opt_D_dump_asm       dflags
	         || dopt Opt_D_dump_asm_stats dflags
			then native
			else []

	let count'	= count + 1;


	-- force evaulation all this stuff to avoid space leaks
	seqString (showSDoc $ vcat $ map ppr imports) `seq` return ()
	lsPprNative 	`seq` return ()
	count'		`seq` return ()

	cmmNativeGens dflags h us' cmms
			(imports : impAcc)
			((lsPprNative, colorStats, linearStats) : profAcc)
			count'

 where	seqString []		= ()
	seqString (x:xs)	= x `seq` seqString xs `seq` ()


-- | Complete native code generation phase for a single top-level chunk of Cmm.
--	Dumping the output of each stage along the way.
--	Global conflict graph and NGC stats
cmmNativeGen 
	:: DynFlags
	-> UniqSupply
	-> RawCmmTop				-- ^ the cmm to generate code for
	-> Int					-- ^ sequence number of this top thing
	-> IO	( UniqSupply
		, [NatCmmTop]			-- native code
		, [CLabel]			-- things imported by this cmm
		, Maybe [Color.RegAllocStats]	-- stats for the coloring register allocator
		, Maybe [Linear.RegAllocStats])	-- stats for the linear register allocators

cmmNativeGen dflags us cmm count
 = do

	-- rewrite assignments to global regs
 	let (fixed_cmm, usFix)	=
		{-# SCC "fixAssignsTop" #-}
		initUs us $ fixAssignsTop cmm

	-- cmm to cmm optimisations
	let (opt_cmm, imports) =
		{-# SCC "cmmToCmm" #-}
		cmmToCmm dflags fixed_cmm

	dumpIfSet_dyn dflags
		Opt_D_dump_opt_cmm "Optimised Cmm"
		(pprCmm $ Cmm [opt_cmm])

	-- generate native code from cmm
	let ((native, lastMinuteImports), usGen) =
		{-# SCC "genMachCode" #-}
		initUs usFix $ genMachCode dflags opt_cmm

	dumpIfSet_dyn dflags
		Opt_D_dump_asm_native "Native code"
		(vcat $ map (docToSDoc . pprNatCmmTop) native)


	-- tag instructions with register liveness information
	let (withLiveness, usLive) =
		{-# SCC "regLiveness" #-}
		initUs usGen $ mapUs regLiveness native

	dumpIfSet_dyn dflags
		Opt_D_dump_asm_liveness "Liveness annotations added"
		(vcat $ map ppr withLiveness)

		
	-- allocate registers
	(alloced, usAlloc, ppr_raStatsColor, ppr_raStatsLinear) <-
	 if ( dopt Opt_RegsGraph dflags
	   || dopt Opt_RegsIterative dflags)
	  then do
	  	-- the regs usable for allocation
		let alloc_regs
			= foldr (\r -> plusUFM_C unionUniqSets
					$ unitUFM (regClass r) (unitUniqSet r))
				emptyUFM
			$ map RealReg allocatableRegs

		-- graph coloring register allocation
		let ((alloced, regAllocStats), usAlloc)
			= {-# SCC "RegAlloc" #-}
			  initUs usLive
			  $ Color.regAlloc
				dflags
				alloc_regs
				(mkUniqSet [0..maxSpillSlots])
				withLiveness

		-- dump out what happened during register allocation
		dumpIfSet_dyn dflags
			Opt_D_dump_asm_regalloc "Registers allocated"
			(vcat $ map (docToSDoc . pprNatCmmTop) alloced)

		dumpIfSet_dyn dflags
			Opt_D_dump_asm_regalloc_stages "Build/spill stages"
			(vcat 	$ map (\(stage, stats)
					-> text "# --------------------------"
					$$ text "#  cmm " <> int count <> text " Stage " <> int stage
					$$ ppr stats)
				$ zip [0..] regAllocStats)

		let mPprStats =
			if dopt Opt_D_dump_asm_stats dflags
			 then Just regAllocStats else Nothing

		-- force evaluation of the Maybe to avoid space leak
		mPprStats `seq` return ()

		return	( alloced, usAlloc
			, mPprStats
			, Nothing)

	  else do
	  	-- do linear register allocation
		let ((alloced, regAllocStats), usAlloc) 
			= {-# SCC "RegAlloc" #-}
  			  initUs usLive
 			  $ liftM unzip
			  $ mapUs Linear.regAlloc withLiveness

		dumpIfSet_dyn dflags
			Opt_D_dump_asm_regalloc "Registers allocated"
			(vcat $ map (docToSDoc . pprNatCmmTop) alloced)

		let mPprStats =
			if dopt Opt_D_dump_asm_stats dflags
			 then Just (catMaybes regAllocStats) else Nothing

		-- force evaluation of the Maybe to avoid space leak
		mPprStats `seq` return ()

		return	( alloced, usAlloc
			, Nothing
			, mPprStats)

	---- shortcut branches
	let shorted	=
	 	{-# SCC "shortcutBranches" #-}
	 	shortcutBranches dflags alloced

	---- sequence blocks
	let sequenced	=
	 	{-# SCC "sequenceBlocks" #-}
	 	map sequenceTop shorted

	---- x86fp_kludge
	let final_mach_code =
#if i386_TARGET_ARCH
	 	{-# SCC "x86fp_kludge" #-}
	 	map x86fp_kludge sequenced
#else
		sequenced
#endif

	return 	( usAlloc
		, final_mach_code
		, lastMinuteImports ++ imports
		, ppr_raStatsColor
		, ppr_raStatsLinear)


#if i386_TARGET_ARCH
x86fp_kludge :: NatCmmTop -> NatCmmTop
x86fp_kludge top@(CmmData _ _) = top
x86fp_kludge top@(CmmProc info lbl params (ListGraph code)) = 
	CmmProc info lbl params (ListGraph $ map bb_i386_insert_ffrees code)
	where
		bb_i386_insert_ffrees (BasicBlock id instrs) =
			BasicBlock id (i386_insert_ffrees instrs)
#endif


-- | Build a doc for all the imports.
--
makeImportsDoc :: [CLabel] -> Pretty.Doc
makeImportsDoc imports
 = dyld_stubs imports

#if HAVE_SUBSECTIONS_VIA_SYMBOLS
                -- On recent versions of Darwin, the linker supports
                -- dead-stripping of code and data on a per-symbol basis.
                -- There's a hack to make this work in PprMach.pprNatCmmTop.
            Pretty.$$ Pretty.text ".subsections_via_symbols"
#endif
#if HAVE_GNU_NONEXEC_STACK
                -- On recent GNU ELF systems one can mark an object file
                -- as not requiring an executable stack. If all objects
                -- linked into a program have this note then the program
                -- will not use an executable stack, which is good for
                -- security. GHC generated code does not need an executable
                -- stack so add the note in:
            Pretty.$$ Pretty.text ".section .note.GNU-stack,\"\",@progbits"
#endif
#if !defined(darwin_TARGET_OS)
                -- And just because every other compiler does, lets stick in
		-- an identifier directive: .ident "GHC x.y.z"
	    Pretty.$$ let compilerIdent = Pretty.text "GHC" Pretty.<+>
	                                  Pretty.text cProjectVersion
                       in Pretty.text ".ident" Pretty.<+>
                          Pretty.doubleQuotes compilerIdent
#endif

 where
	-- Generate "symbol stubs" for all external symbols that might
	-- come from a dynamic library.
	dyld_stubs :: [CLabel] -> Pretty.Doc
{-      dyld_stubs imps = Pretty.vcat $ map pprDyldSymbolStub $
				    map head $ group $ sort imps-}

	-- (Hack) sometimes two Labels pretty-print the same, but have
	-- different uniques; so we compare their text versions...
	dyld_stubs imps
		| needImportedSymbols
		= Pretty.vcat $
			(pprGotDeclaration :) $
			map (pprImportedSymbol . fst . head) $
			groupBy (\(_,a) (_,b) -> a == b) $
			sortBy (\(_,a) (_,b) -> compare a b) $
			map doPpr $
			imps
		| otherwise
		= Pretty.empty

	doPpr lbl = (lbl, Pretty.render $ pprCLabel lbl astyle)
	astyle = mkCodeStyle AsmStyle


-- -----------------------------------------------------------------------------
-- Sequencing the basic blocks

-- Cmm BasicBlocks are self-contained entities: they always end in a
-- jump, either non-local or to another basic block in the same proc.
-- In this phase, we attempt to place the basic blocks in a sequence
-- such that as many of the local jumps as possible turn into
-- fallthroughs.

sequenceTop :: NatCmmTop -> NatCmmTop
sequenceTop top@(CmmData _ _) = top
sequenceTop (CmmProc info lbl params (ListGraph blocks)) = 
  CmmProc info lbl params (ListGraph $ makeFarBranches $ sequenceBlocks blocks)

-- The algorithm is very simple (and stupid): we make a graph out of
-- the blocks where there is an edge from one block to another iff the
-- first block ends by jumping to the second.  Then we topologically
-- sort this graph.  Then traverse the list: for each block, we first
-- output the block, then if it has an out edge, we move the
-- destination of the out edge to the front of the list, and continue.

sequenceBlocks :: [NatBasicBlock] -> [NatBasicBlock]
sequenceBlocks [] = []
sequenceBlocks (entry:blocks) = 
  seqBlocks (mkNode entry : reverse (flattenSCCs (sccBlocks blocks)))
  -- the first block is the entry point ==> it must remain at the start.

sccBlocks :: [NatBasicBlock] -> [SCC (NatBasicBlock,Unique,[Unique])]
sccBlocks blocks = stronglyConnCompR (map mkNode blocks)

getOutEdges :: [Instr] -> [Unique]
getOutEdges instrs = case jumpDests (last instrs) [] of
			[one] -> [getUnique one]
			_many -> []
		-- we're only interested in the last instruction of
		-- the block, and only if it has a single destination.

mkNode block@(BasicBlock id instrs) = (block, getUnique id, getOutEdges instrs)

seqBlocks [] = []
seqBlocks ((block,_,[]) : rest)
  = block : seqBlocks rest
seqBlocks ((block@(BasicBlock id instrs),_,[next]) : rest)
  | can_fallthrough = BasicBlock id (init instrs) : seqBlocks rest'
  | otherwise       = block : seqBlocks rest'
  where
	(can_fallthrough, rest') = reorder next [] rest
	  -- TODO: we should do a better job for cycles; try to maximise the
	  -- fallthroughs within a loop.
seqBlocks _ = panic "AsmCodegen:seqBlocks"

reorder id accum [] = (False, reverse accum)
reorder id accum (b@(block,id',out) : rest)
  | id == id'  = (True, (block,id,out) : reverse accum ++ rest)
  | otherwise  = reorder id (b:accum) rest


-- -----------------------------------------------------------------------------
-- Making far branches

-- Conditional branches on PowerPC are limited to +-32KB; if our Procs get too
-- big, we have to work around this limitation.

makeFarBranches :: [NatBasicBlock] -> [NatBasicBlock]

#if powerpc_TARGET_ARCH
makeFarBranches blocks
    | last blockAddresses < nearLimit = blocks
    | otherwise = zipWith handleBlock blockAddresses blocks
    where
        blockAddresses = scanl (+) 0 $ map blockLen blocks
        blockLen (BasicBlock _ instrs) = length instrs
        
        handleBlock addr (BasicBlock id instrs)
                = BasicBlock id (zipWith makeFar [addr..] instrs)
        
        makeFar addr (BCC ALWAYS tgt) = BCC ALWAYS tgt
        makeFar addr (BCC cond tgt)
            | abs (addr - targetAddr) >= nearLimit
            = BCCFAR cond tgt
            | otherwise
            = BCC cond tgt
            where Just targetAddr = lookupUFM blockAddressMap tgt
        makeFar addr other            = other
        
        nearLimit = 7000 -- 8192 instructions are allowed; let's keep some
                         -- distance, as we have a few pseudo-insns that are
                         -- pretty-printed as multiple instructions,
                         -- and it's just not worth the effort to calculate
                         -- things exactly
        
        blockAddressMap = listToUFM $ zip (map blockId blocks) blockAddresses
#else
makeFarBranches = id
#endif

-- -----------------------------------------------------------------------------
-- Shortcut branches

shortcutBranches :: DynFlags -> [NatCmmTop] -> [NatCmmTop]
shortcutBranches dflags tops
  | optLevel dflags < 1 = tops    -- only with -O or higher
  | otherwise           = map (apply_mapping mapping) tops'
  where
    (tops', mappings) = mapAndUnzip build_mapping tops
    mapping = foldr plusUFM emptyUFM mappings

build_mapping top@(CmmData _ _) = (top, emptyUFM)
build_mapping (CmmProc info lbl params (ListGraph []))
  = (CmmProc info lbl params (ListGraph []), emptyUFM)
build_mapping (CmmProc info lbl params (ListGraph (head:blocks)))
  = (CmmProc info lbl params (ListGraph (head:others)), mapping)
        -- drop the shorted blocks, but don't ever drop the first one,
        -- because it is pointed to by a global label.
  where
    -- find all the blocks that just consist of a jump that can be
    -- shorted.
    (shortcut_blocks, others) = partitionWith split blocks
    split (BasicBlock id [insn]) | Just dest <- canShortcut insn 
                                 = Left (id,dest)
    split other = Right other

    -- build a mapping from BlockId to JumpDest for shorting branches
    mapping = foldl add emptyUFM shortcut_blocks
    add ufm (id,dest) = addToUFM ufm id dest
    
apply_mapping ufm (CmmData sec statics) 
  = CmmData sec (map (shortcutStatic (lookupUFM ufm)) statics)
  -- we need to get the jump tables, so apply the mapping to the entries
  -- of a CmmData too.
apply_mapping ufm (CmmProc info lbl params (ListGraph blocks))
  = CmmProc info lbl params (ListGraph $ map short_bb blocks)
  where
    short_bb (BasicBlock id insns) = BasicBlock id $! map short_insn insns
    short_insn i = shortcutJump (lookupUFM ufm) i
                 -- shortcutJump should apply the mapping repeatedly,
                 -- just in case we can short multiple branches.

-- -----------------------------------------------------------------------------
-- Instruction selection

-- Native code instruction selection for a chunk of stix code.  For
-- this part of the computation, we switch from the UniqSM monad to
-- the NatM monad.  The latter carries not only a Unique, but also an
-- Int denoting the current C stack pointer offset in the generated
-- code; this is needed for creating correct spill offsets on
-- architectures which don't offer, or for which it would be
-- prohibitively expensive to employ, a frame pointer register.  Viz,
-- x86.

-- The offset is measured in bytes, and indicates the difference
-- between the current (simulated) C stack-ptr and the value it was at
-- the beginning of the block.  For stacks which grow down, this value
-- should be either zero or negative.

-- Switching between the two monads whilst carrying along the same
-- Unique supply breaks abstraction.  Is that bad?

genMachCode :: DynFlags -> RawCmmTop -> UniqSM ([NatCmmTop], [CLabel])

genMachCode dflags cmm_top
  = do	{ initial_us <- getUs
	; let initial_st           = mkNatM_State initial_us 0 dflags
	      (new_tops, final_st) = initNat initial_st (cmmTopCodeGen cmm_top)
	      final_delta          = natm_delta final_st
	      final_imports        = natm_imports final_st
	; if   final_delta == 0
          then return (new_tops, final_imports)
          else pprPanic "genMachCode: nonzero final delta" (int final_delta)
    }

-- -----------------------------------------------------------------------------
-- Fixup assignments to global registers so that they assign to 
-- locations within the RegTable, if appropriate.

-- Note that we currently don't fixup reads here: they're done by
-- the generic optimiser below, to avoid having two separate passes
-- over the Cmm.

fixAssignsTop :: RawCmmTop -> UniqSM RawCmmTop
fixAssignsTop top@(CmmData _ _) = returnUs top
fixAssignsTop (CmmProc info lbl params (ListGraph blocks)) =
  mapUs fixAssignsBlock blocks `thenUs` \ blocks' ->
  returnUs (CmmProc info lbl params (ListGraph blocks'))

fixAssignsBlock :: CmmBasicBlock -> UniqSM CmmBasicBlock
fixAssignsBlock (BasicBlock id stmts) =
  fixAssigns stmts `thenUs` \ stmts' ->
  returnUs (BasicBlock id stmts')

fixAssigns :: [CmmStmt] -> UniqSM [CmmStmt]
fixAssigns stmts =
  mapUs fixAssign stmts `thenUs` \ stmtss ->
  returnUs (concat stmtss)

fixAssign :: CmmStmt -> UniqSM [CmmStmt]
fixAssign (CmmAssign (CmmGlobal reg) src)
  | Left  realreg <- reg_or_addr
  = returnUs [CmmAssign (CmmGlobal reg) src]
  | Right baseRegAddr <- reg_or_addr
  = returnUs [CmmStore baseRegAddr src]
           -- Replace register leaves with appropriate StixTrees for
           -- the given target. GlobalRegs which map to a reg on this
           -- arch are left unchanged.  Assigning to BaseReg is always
           -- illegal, so we check for that.
  where
	reg_or_addr = get_GlobalReg_reg_or_addr reg

fixAssign other_stmt = returnUs [other_stmt]

-- -----------------------------------------------------------------------------
-- Generic Cmm optimiser

{-
Here we do:

  (a) Constant folding
  (b) Simple inlining: a temporary which is assigned to and then
      used, once, can be shorted.
  (c) Replacement of references to GlobalRegs which do not have
      machine registers by the appropriate memory load (eg.
      Hp ==>  *(BaseReg + 34) ).
  (d) Position independent code and dynamic linking
        (i)  introduce the appropriate indirections
             and position independent refs
        (ii) compile a list of imported symbols

Ideas for other things we could do (ToDo):

  - shortcut jumps-to-jumps
  - eliminate dead code blocks
  - simple CSE: if an expr is assigned to a temp, then replace later occs of
    that expr with the temp, until the expr is no longer valid (can push through
    temp assignments, and certain assigns to mem...)
-}

cmmToCmm :: DynFlags -> RawCmmTop -> (RawCmmTop, [CLabel])
cmmToCmm _ top@(CmmData _ _) = (top, [])
cmmToCmm dflags (CmmProc info lbl params (ListGraph blocks)) = runCmmOpt dflags $ do
  blocks' <- mapM cmmBlockConFold (cmmMiniInline blocks)
  return $ CmmProc info lbl params (ListGraph blocks')

newtype CmmOptM a = CmmOptM (([CLabel], DynFlags) -> (# a, [CLabel] #))

instance Monad CmmOptM where
  return x = CmmOptM $ \(imports, _) -> (# x,imports #)
  (CmmOptM f) >>= g =
    CmmOptM $ \(imports, dflags) ->
                case f (imports, dflags) of
                  (# x, imports' #) ->
                    case g x of
                      CmmOptM g' -> g' (imports', dflags)

addImportCmmOpt :: CLabel -> CmmOptM ()
addImportCmmOpt lbl = CmmOptM $ \(imports, dflags) -> (# (), lbl:imports #)

getDynFlagsCmmOpt :: CmmOptM DynFlags
getDynFlagsCmmOpt = CmmOptM $ \(imports, dflags) -> (# dflags, imports #)

runCmmOpt :: DynFlags -> CmmOptM a -> (a, [CLabel])
runCmmOpt dflags (CmmOptM f) = case f ([], dflags) of
                        (# result, imports #) -> (result, imports)

cmmBlockConFold :: CmmBasicBlock -> CmmOptM CmmBasicBlock
cmmBlockConFold (BasicBlock id stmts) = do
  stmts' <- mapM cmmStmtConFold stmts
  return $ BasicBlock id stmts'

cmmStmtConFold stmt
   = case stmt of
        CmmAssign reg src
           -> do src' <- cmmExprConFold DataReference src
                 return $ case src' of
		   CmmReg reg' | reg == reg' -> CmmNop
		   new_src -> CmmAssign reg new_src

        CmmStore addr src
           -> do addr' <- cmmExprConFold DataReference addr
                 src'  <- cmmExprConFold DataReference src
                 return $ CmmStore addr' src'

        CmmJump addr regs
           -> do addr' <- cmmExprConFold JumpReference addr
                 return $ CmmJump addr' regs

	CmmCall target regs args srt returns
	   -> do target' <- case target of
			      CmmCallee e conv -> do
			        e' <- cmmExprConFold CallReference e
			        return $ CmmCallee e' conv
			      other -> return other
                 args' <- mapM (\(arg, hint) -> do
                                  arg' <- cmmExprConFold DataReference arg
                                  return (arg', hint)) args
	         return $ CmmCall target' regs args' srt returns

        CmmCondBranch test dest
           -> do test' <- cmmExprConFold DataReference test
	         return $ case test' of
		   CmmLit (CmmInt 0 _) -> 
		     CmmComment (mkFastString ("deleted: " ++ 
					showSDoc (pprStmt stmt)))

		   CmmLit (CmmInt n _) -> CmmBranch dest
		   other -> CmmCondBranch test' dest

	CmmSwitch expr ids
	   -> do expr' <- cmmExprConFold DataReference expr
	         return $ CmmSwitch expr' ids

        other
           -> return other


cmmExprConFold referenceKind expr
   = case expr of
        CmmLoad addr rep
           -> do addr' <- cmmExprConFold DataReference addr
                 return $ CmmLoad addr' rep

        CmmMachOp mop args
           -- For MachOps, we first optimize the children, and then we try 
           -- our hand at some constant-folding.
           -> do args' <- mapM (cmmExprConFold DataReference) args
                 return $ cmmMachOpFold mop args'

        CmmLit (CmmLabel lbl)
           -> do
		dflags <- getDynFlagsCmmOpt
		cmmMakeDynamicReference dflags addImportCmmOpt referenceKind lbl
        CmmLit (CmmLabelOff lbl off)
           -> do
		 dflags <- getDynFlagsCmmOpt
		 dynRef <- cmmMakeDynamicReference dflags addImportCmmOpt referenceKind lbl
                 return $ cmmMachOpFold (MO_Add wordRep) [
                     dynRef,
                     (CmmLit $ CmmInt (fromIntegral off) wordRep)
                   ]

#if powerpc_TARGET_ARCH
           -- On powerpc (non-PIC), it's easier to jump directly to a label than
           -- to use the register table, so we replace these registers
           -- with the corresponding labels:
        CmmReg (CmmGlobal GCEnter1)
          | not opt_PIC
          -> cmmExprConFold referenceKind $
             CmmLit (CmmLabel (mkRtsCodeLabel SLIT( "__stg_gc_enter_1"))) 
        CmmReg (CmmGlobal GCFun)
          | not opt_PIC
          -> cmmExprConFold referenceKind $
             CmmLit (CmmLabel (mkRtsCodeLabel SLIT( "__stg_gc_fun")))
#endif

        CmmReg (CmmGlobal mid)
           -- Replace register leaves with appropriate StixTrees for
           -- the given target.  MagicIds which map to a reg on this
           -- arch are left unchanged.  For the rest, BaseReg is taken
           -- to mean the address of the reg table in MainCapability,
           -- and for all others we generate an indirection to its
           -- location in the register table.
           -> case get_GlobalReg_reg_or_addr mid of
                 Left  realreg -> return expr
                 Right baseRegAddr 
                    -> case mid of 
                          BaseReg -> cmmExprConFold DataReference baseRegAddr
                          other   -> cmmExprConFold DataReference
                                        (CmmLoad baseRegAddr (globalRegRep mid))
	   -- eliminate zero offsets
	CmmRegOff reg 0
	   -> cmmExprConFold referenceKind (CmmReg reg)

        CmmRegOff (CmmGlobal mid) offset
           -- RegOf leaves are just a shorthand form. If the reg maps
           -- to a real reg, we keep the shorthand, otherwise, we just
           -- expand it and defer to the above code. 
           -> case get_GlobalReg_reg_or_addr mid of
                Left  realreg -> return expr
                Right baseRegAddr
                   -> cmmExprConFold DataReference (CmmMachOp (MO_Add wordRep) [
                                        CmmReg (CmmGlobal mid),
                                        CmmLit (CmmInt (fromIntegral offset)
                                                       wordRep)])
        other
           -> return other

-- -----------------------------------------------------------------------------
-- Utils

bind f x = x $! f

\end{code}

