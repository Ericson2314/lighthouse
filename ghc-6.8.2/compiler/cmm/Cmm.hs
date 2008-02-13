-----------------------------------------------------------------------------
--
-- Cmm data types
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module Cmm ( 
	GenCmm(..), Cmm, RawCmm,
	GenCmmTop(..), CmmTop, RawCmmTop,
        ListGraph(..),
	CmmInfo(..), UpdateFrame(..),
        CmmInfoTable(..), ClosureTypeInfo(..), ProfilingInfo(..), ClosureTypeTag,
	GenBasicBlock(..), CmmBasicBlock, blockId, blockStmts, mapBlockStmts,
        CmmReturnInfo(..),
	CmmStmt(..), CmmActuals, CmmFormal, CmmFormals, CmmHintFormals,
        CmmSafety(..),
	CmmCallTarget(..),
	CmmStatic(..), Section(..),
	CmmExpr(..), cmmExprRep, 
	CmmReg(..), cmmRegRep,
	CmmLit(..), cmmLitRep,
	LocalReg(..), localRegRep, localRegGCFollow, Kind(..),
	BlockId(..), BlockEnv,
	GlobalReg(..), globalRegRep,

	node, nodeReg, spReg, hpReg, spLimReg
  ) where

#include "HsVersions.h"

import MachOp
import CLabel
import ForeignCall
import SMRep
import ClosureInfo
import Unique
import UniqFM
import FastString

import Data.Word

-----------------------------------------------------------------------------
--		Cmm, CmmTop, CmmBasicBlock
-----------------------------------------------------------------------------

-- A file is a list of top-level chunks.  These may be arbitrarily
-- re-orderd during code generation.

-- GenCmm is abstracted over
--   d, the type of static data elements in CmmData
--   h, the static info preceding the code of a CmmProc
--   g, the control-flow graph of a CmmProc
--
-- We expect there to be two main instances of this type:
--   (a) C--, i.e. populated with various C-- constructs
--		(Cmm and RawCmm below)
--   (b) Native code, populated with data/instructions
--
newtype GenCmm d h g = Cmm [GenCmmTop d h g]

-- | A top-level chunk, abstracted over the type of the contents of
-- the basic blocks (Cmm or instructions are the likely instantiations).
data GenCmmTop d h g
  = CmmProc	-- A procedure
     h	               -- Extra header such as the info table
     CLabel            -- Used to generate both info & entry labels
     CmmFormals        -- Argument locals live on entry (C-- procedure params)
     g                 -- Control-flow graph for the procedure's code

  | CmmData 	-- Static data
	Section 
	[d]

-- | A control-flow graph represented as a list of extended basic blocks.
newtype ListGraph i = ListGraph [GenBasicBlock i] 
   -- ^ Code, may be empty.  The first block is the entry point.  The
   -- order is otherwise initially unimportant, but at some point the
   -- code gen will fix the order.

   -- BlockIds must be unique across an entire compilation unit, since
   -- they are translated to assembly-language labels, which scope
   -- across a whole compilation unit.

-- | Cmm with the info table as a data type
type Cmm    = GenCmm    CmmStatic CmmInfo (ListGraph CmmStmt)
type CmmTop = GenCmmTop CmmStatic CmmInfo (ListGraph CmmStmt)

-- | Cmm with the info tables converted to a list of 'CmmStatic'
type RawCmm    = GenCmm    CmmStatic [CmmStatic] (ListGraph CmmStmt)
type RawCmmTop = GenCmmTop CmmStatic [CmmStatic] (ListGraph CmmStmt)


-- A basic block containing a single label, at the beginning.
-- The list of basic blocks in a top-level code block may be re-ordered.
-- Fall-through is not allowed: there must be an explicit jump at the
-- end of each basic block, but the code generator might rearrange basic
-- blocks in order to turn some jumps into fallthroughs.

data GenBasicBlock i = BasicBlock BlockId [i]
type CmmBasicBlock   = GenBasicBlock CmmStmt

blockId :: GenBasicBlock i -> BlockId
-- The branch block id is that of the first block in 
-- the branch, which is that branch's entry point
blockId (BasicBlock blk_id _ ) = blk_id

blockStmts :: GenBasicBlock i -> [i]
blockStmts (BasicBlock _ stmts) = stmts

mapBlockStmts :: (i -> i') -> GenBasicBlock i -> GenBasicBlock i'
mapBlockStmts f (BasicBlock id bs) = BasicBlock id (map f bs)

-----------------------------------------------------------------------------
--     Info Tables
-----------------------------------------------------------------------------

data CmmInfo
  = CmmInfo
      (Maybe BlockId)     -- GC target. Nothing <=> CPS won't do stack check
      (Maybe UpdateFrame) -- Update frame
      CmmInfoTable        -- Info table

-- Info table as a haskell data type
data CmmInfoTable
  = CmmInfoTable
      ProfilingInfo
      ClosureTypeTag -- Int
      ClosureTypeInfo
  | CmmNonInfoTable   -- Procedure doesn't need an info table

-- TODO: The GC target shouldn't really be part of CmmInfo
-- as it doesn't appear in the resulting info table.
-- It should be factored out.

data ClosureTypeInfo
  = ConstrInfo ClosureLayout ConstrTag ConstrDescription
  | FunInfo ClosureLayout C_SRT FunType FunArity ArgDescr SlowEntry
  | ThunkInfo ClosureLayout C_SRT
  | ThunkSelectorInfo SelectorOffset C_SRT
  | ContInfo
      [Maybe LocalReg]  -- Forced stack parameters
      C_SRT

data CmmReturnInfo = CmmMayReturn
                   | CmmNeverReturns

-- TODO: These types may need refinement
data ProfilingInfo = ProfilingInfo CmmLit CmmLit -- closure_type, closure_desc
type ClosureTypeTag = StgHalfWord
type ClosureLayout = (StgHalfWord, StgHalfWord) -- ptrs, nptrs
type ConstrTag = StgHalfWord
type ConstrDescription = CmmLit
type FunType = StgHalfWord
type FunArity = StgHalfWord
type SlowEntry = CmmLit
  -- ^We would like this to be a CLabel but
  -- for now the parser sets this to zero on an INFO_TABLE_FUN.
type SelectorOffset = StgWord

-- | A frame that is to be pushed before entry to the function.
-- Used to handle 'update' frames.
data UpdateFrame =
    UpdateFrame
      CmmExpr    -- Frame header.  Behaves like the target of a 'jump'.
      [CmmExpr]  -- Frame remainder.  Behaves like the arguments of a 'jump'.

-----------------------------------------------------------------------------
--		CmmStmt
-- A "statement".  Note that all branches are explicit: there are no
-- control transfers to computed addresses, except when transfering
-- control to a new function.
-----------------------------------------------------------------------------

data CmmStmt
  = CmmNop
  | CmmComment FastString

  | CmmAssign CmmReg CmmExpr	 -- Assign to register

  | CmmStore CmmExpr CmmExpr     -- Assign to memory location.  Size is
                                 -- given by cmmExprRep of the rhs.

  | CmmCall	 		 -- A call (forign, native or primitive), with 
     CmmCallTarget
     CmmHintFormals		 -- zero or more results
     CmmActuals			 -- zero or more arguments
     CmmSafety			 -- whether to build a continuation
     CmmReturnInfo

  | CmmBranch BlockId             -- branch to another BB in this fn

  | CmmCondBranch CmmExpr BlockId -- conditional branch

  | CmmSwitch CmmExpr [Maybe BlockId]   -- Table branch
	-- The scrutinee is zero-based; 
	--	zero -> first block
	--	one  -> second block etc
	-- Undefined outside range, and when there's a Nothing

  | CmmJump CmmExpr      -- Jump to another C-- function,
      CmmActuals         -- with these parameters.

  | CmmReturn            -- Return from a native C-- function,
      CmmActuals         -- with these return values.

type CmmActual      = CmmExpr
type CmmActuals     = [(CmmActual,MachHint)]
type CmmFormal      = LocalReg
type CmmHintFormals = [(CmmFormal,MachHint)]
type CmmFormals     = [CmmFormal]
data CmmSafety      = CmmUnsafe | CmmSafe C_SRT

{-
Discussion
~~~~~~~~~~

One possible problem with the above type is that the only way to do a
non-local conditional jump is to encode it as a branch to a block that
contains a single jump.  This leads to inefficient code in the back end.

One possible way to fix this would be:

data CmmStat = 
  ...
  | CmmJump CmmBranchDest
  | CmmCondJump CmmExpr CmmBranchDest
  ...

data CmmBranchDest
  = Local BlockId
  | NonLocal CmmExpr [LocalReg]

In favour:

+ one fewer constructors in CmmStmt
+ allows both cond branch and switch to jump to non-local destinations

Against:

- not strictly necessary: can already encode as branch+jump
- not always possible to implement any better in the back end
- could do the optimisation in the back end (but then plat-specific?)
- C-- doesn't have it
- back-end optimisation might be more general (jump shortcutting)

So we'll stick with the way it is, and add the optimisation to the NCG.
-}

-----------------------------------------------------------------------------
--		CmmCallTarget
--
-- The target of a CmmCall.
-----------------------------------------------------------------------------

data CmmCallTarget
  = CmmCallee		-- Call a function (foreign or native)
	CmmExpr 		-- literal label <=> static call
				-- other expression <=> dynamic call
	CCallConv		-- The calling convention

  | CmmPrim		-- Call a "primitive" (eg. sin, cos)
	CallishMachOp		-- These might be implemented as inline
				-- code by the backend.

-----------------------------------------------------------------------------
--		CmmExpr
-- An expression.  Expressions have no side effects.
-----------------------------------------------------------------------------

data CmmExpr
  = CmmLit CmmLit               -- Literal
  | CmmLoad CmmExpr MachRep     -- Read memory location
  | CmmReg CmmReg		-- Contents of register
  | CmmMachOp MachOp [CmmExpr]  -- Machine operation (+, -, *, etc.)
  | CmmRegOff CmmReg Int	
	-- CmmRegOff reg i
	--        ** is shorthand only, meaning **
	-- CmmMachOp (MO_S_Add rep (CmmReg reg) (CmmLit (CmmInt i rep)))
	--	where rep = cmmRegRep reg
  deriving Eq

data CmmReg 
  = CmmLocal  LocalReg
  | CmmGlobal GlobalReg
  deriving( Eq )

-- | Whether a 'LocalReg' is a GC followable pointer
data Kind = KindPtr | KindNonPtr deriving (Eq)

data LocalReg
  = LocalReg
      !Unique   -- ^ Identifier
      MachRep   -- ^ Type
      Kind      -- ^ Should the GC follow as a pointer

data CmmLit
  = CmmInt Integer  MachRep
	-- Interpretation: the 2's complement representation of the value
	-- is truncated to the specified size.  This is easier than trying
	-- to keep the value within range, because we don't know whether
	-- it will be used as a signed or unsigned value (the MachRep doesn't
	-- distinguish between signed & unsigned).
  | CmmFloat  Rational MachRep
  | CmmLabel    CLabel			-- Address of label
  | CmmLabelOff CLabel Int		-- Address of label + byte offset
  
        -- Due to limitations in the C backend, the following
        -- MUST ONLY be used inside the info table indicated by label2
        -- (label2 must be the info label), and label1 must be an
        -- SRT, a slow entrypoint or a large bitmap (see the Mangler)
        -- Don't use it at all unless tablesNextToCode.
        -- It is also used inside the NCG during when generating
        -- position-independent code. 
  | CmmLabelDiffOff CLabel CLabel Int   -- label1 - label2 + offset
  deriving Eq

instance Eq LocalReg where
  (LocalReg u1 _ _) == (LocalReg u2 _ _) = u1 == u2

instance Uniquable LocalReg where
  getUnique (LocalReg uniq _ _) = uniq

-----------------------------------------------------------------------------
--		MachRep
-----------------------------------------------------------------------------
cmmExprRep :: CmmExpr -> MachRep
cmmExprRep (CmmLit lit)      = cmmLitRep lit
cmmExprRep (CmmLoad _ rep)   = rep
cmmExprRep (CmmReg reg)      = cmmRegRep reg
cmmExprRep (CmmMachOp op _)  = resultRepOfMachOp op
cmmExprRep (CmmRegOff reg _) = cmmRegRep reg

cmmRegRep :: CmmReg -> MachRep
cmmRegRep (CmmLocal  reg) 	= localRegRep reg
cmmRegRep (CmmGlobal reg)	= globalRegRep reg

localRegRep :: LocalReg -> MachRep
localRegRep (LocalReg _ rep _) = rep

localRegGCFollow :: LocalReg -> Kind
localRegGCFollow (LocalReg _ _ p) = p

cmmLitRep :: CmmLit -> MachRep
cmmLitRep (CmmInt _ rep)    = rep
cmmLitRep (CmmFloat _ rep)  = rep
cmmLitRep (CmmLabel _)      = wordRep
cmmLitRep (CmmLabelOff _ _) = wordRep
cmmLitRep (CmmLabelDiffOff _ _ _) = wordRep

-----------------------------------------------------------------------------
-- A local label.

-- Local labels must be unique within a single compilation unit.

newtype BlockId = BlockId Unique
  deriving (Eq,Ord)

instance Uniquable BlockId where
  getUnique (BlockId u) = u

type BlockEnv a = UniqFM {- BlockId -} a

-----------------------------------------------------------------------------
--		Static Data
-----------------------------------------------------------------------------

data Section
  = Text
  | Data
  | ReadOnlyData
  | RelocatableReadOnlyData
  | UninitialisedData
  | ReadOnlyData16	-- .rodata.cst16 on x86_64, 16-byte aligned
  | OtherSection String

data CmmStatic
  = CmmStaticLit CmmLit	
	-- a literal value, size given by cmmLitRep of the literal.
  | CmmUninitialised Int
	-- uninitialised data, N bytes long
  | CmmAlign Int
	-- align to next N-byte boundary (N must be a power of 2).
  | CmmDataLabel CLabel
	-- label the current position in this section.
  | CmmString [Word8]
	-- string of 8-bit values only, not zero terminated.

-----------------------------------------------------------------------------
--		Global STG registers
-----------------------------------------------------------------------------

data GlobalReg
  -- Argument and return registers
  = VanillaReg			-- pointers, unboxed ints and chars
	{-# UNPACK #-} !Int	-- its number

  | FloatReg		-- single-precision floating-point registers
	{-# UNPACK #-} !Int	-- its number

  | DoubleReg		-- double-precision floating-point registers
	{-# UNPACK #-} !Int	-- its number

  | LongReg	        -- long int registers (64-bit, really)
	{-# UNPACK #-} !Int	-- its number

  -- STG registers
  | Sp			-- Stack ptr; points to last occupied stack location.
  | SpLim		-- Stack limit
  | Hp			-- Heap ptr; points to last occupied heap location.
  | HpLim		-- Heap limit register
  | CurrentTSO		-- pointer to current thread's TSO
  | CurrentNursery	-- pointer to allocation area
  | HpAlloc		-- allocation count for heap check failure

		-- We keep the address of some commonly-called 
		-- functions in the register table, to keep code
		-- size down:
  | GCEnter1		-- stg_gc_enter_1
  | GCFun		-- stg_gc_fun

  -- Base offset for the register table, used for accessing registers
  -- which do not have real registers assigned to them.  This register
  -- will only appear after we have expanded GlobalReg into memory accesses
  -- (where necessary) in the native code generator.
  | BaseReg

  -- Base Register for PIC (position-independent code) calculations
  -- Only used inside the native code generator. It's exact meaning differs
  -- from platform to platform (see module PositionIndependentCode).
  | PicBaseReg

  deriving( Eq
#ifdef DEBUG
	, Show
#endif
	 )

-- convenient aliases
spReg, hpReg, spLimReg, nodeReg :: CmmReg
spReg = CmmGlobal Sp
hpReg = CmmGlobal Hp
spLimReg = CmmGlobal SpLim
nodeReg = CmmGlobal node

node :: GlobalReg
node = VanillaReg 1

globalRegRep :: GlobalReg -> MachRep
globalRegRep (VanillaReg _) 	= wordRep
globalRegRep (FloatReg _) 	= F32
globalRegRep (DoubleReg _) 	= F64
globalRegRep (LongReg _) 	= I64
globalRegRep _			= wordRep
