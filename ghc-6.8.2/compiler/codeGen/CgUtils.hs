{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-----------------------------------------------------------------------------
--
-- Code generator utilities; mostly monadic
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module CgUtils (
	addIdReps,
	cgLit,
	emitDataLits, mkDataLits,
        emitRODataLits, mkRODataLits,
        emitIf, emitIfThenElse,
	emitRtsCall, emitRtsCallWithVols, emitRtsCallWithResult,
	assignNonPtrTemp, newNonPtrTemp,
	assignPtrTemp, newPtrTemp,
	emitSimultaneously,
	emitSwitch, emitLitSwitch,
	tagToClosure,

        callerSaveVolatileRegs, get_GlobalReg_addr,

	cmmAndWord, cmmOrWord, cmmNegate, cmmEqWord, cmmNeWord,
        cmmUGtWord,
	cmmOffsetExprW, cmmOffsetExprB,
	cmmRegOffW, cmmRegOffB,
	cmmLabelOffW, cmmLabelOffB,
	cmmOffsetW, cmmOffsetB,
	cmmOffsetLitW, cmmOffsetLitB,
	cmmLoadIndexW,
        cmmConstrTag, cmmConstrTag1,

        tagForCon, tagCons, isSmallFamily,
        cmmUntag, cmmIsTagged, cmmGetTag,

	addToMem, addToMemE,
	mkWordCLit,
	mkStringCLit, mkByteStringCLit,
	packHalfWordsCLit,
	blankWord,

	getSRTInfo
  ) where

#include "HsVersions.h"
#include "../includes/MachRegs.h"

import CgMonad
import TyCon
import DataCon
import Id
import Constants
import SMRep
import PprCmm		( {- instances -} )
import Cmm
import CLabel
import CmmUtils
import MachOp
import ForeignCall
import ClosureInfo
import StgSyn (SRT(..))
import Literal
import Digraph
import ListSetOps
import Util
import DynFlags
import FastString
import PackageConfig
import Outputable

import Data.Char
import Data.Bits
import Data.Word
import Data.Maybe

-------------------------------------------------------------------------
--
--	Random small functions
--
-------------------------------------------------------------------------

addIdReps :: [Id] -> [(CgRep, Id)]
addIdReps ids = [(idCgRep id, id) | id <- ids]

-------------------------------------------------------------------------
--
--	Literals
--
-------------------------------------------------------------------------

cgLit :: Literal -> FCode CmmLit
cgLit (MachStr s) = mkByteStringCLit (bytesFS s)
 -- not unpackFS; we want the UTF-8 byte stream.
cgLit other_lit   = return (mkSimpleLit other_lit)

mkSimpleLit :: Literal -> CmmLit
mkSimpleLit (MachChar	c)    = CmmInt (fromIntegral (ord c)) wordRep
mkSimpleLit MachNullAddr      = zeroCLit
mkSimpleLit (MachInt i)       = CmmInt i wordRep
mkSimpleLit (MachInt64 i)     = CmmInt i I64
mkSimpleLit (MachWord i)      = CmmInt i wordRep
mkSimpleLit (MachWord64 i)    = CmmInt i I64
mkSimpleLit (MachFloat r)     = CmmFloat r F32
mkSimpleLit (MachDouble r)    = CmmFloat r F64
mkSimpleLit (MachLabel fs ms) = CmmLabel (mkForeignLabel fs ms is_dyn)
			      where
				is_dyn = False	-- ToDo: fix me
	
mkLtOp :: Literal -> MachOp
-- On signed literals we must do a signed comparison
mkLtOp (MachInt _)    = MO_S_Lt wordRep
mkLtOp (MachFloat _)  = MO_S_Lt F32
mkLtOp (MachDouble _) = MO_S_Lt F64
mkLtOp lit	      = MO_U_Lt (cmmLitRep (mkSimpleLit lit))


---------------------------------------------------
--
--	Cmm data type functions
--
---------------------------------------------------

-----------------------
-- The "B" variants take byte offsets
cmmRegOffB :: CmmReg -> ByteOff -> CmmExpr
cmmRegOffB = cmmRegOff

cmmOffsetB :: CmmExpr -> ByteOff -> CmmExpr
cmmOffsetB = cmmOffset

cmmOffsetExprB :: CmmExpr -> CmmExpr -> CmmExpr
cmmOffsetExprB = cmmOffsetExpr

cmmLabelOffB :: CLabel -> ByteOff -> CmmLit
cmmLabelOffB = cmmLabelOff

cmmOffsetLitB :: CmmLit -> ByteOff -> CmmLit
cmmOffsetLitB = cmmOffsetLit

-----------------------
-- The "W" variants take word offsets
cmmOffsetExprW :: CmmExpr -> CmmExpr -> CmmExpr
-- The second arg is a *word* offset; need to change it to bytes
cmmOffsetExprW e (CmmLit (CmmInt n _)) = cmmOffsetW e (fromInteger n)
cmmOffsetExprW e wd_off = cmmIndexExpr wordRep e wd_off

cmmOffsetW :: CmmExpr -> WordOff -> CmmExpr
cmmOffsetW e n = cmmOffsetB e (wORD_SIZE * n)

cmmRegOffW :: CmmReg -> WordOff -> CmmExpr
cmmRegOffW reg wd_off = cmmRegOffB reg (wd_off * wORD_SIZE)

cmmOffsetLitW :: CmmLit -> WordOff -> CmmLit
cmmOffsetLitW lit wd_off = cmmOffsetLitB lit (wORD_SIZE * wd_off)

cmmLabelOffW :: CLabel -> WordOff -> CmmLit
cmmLabelOffW lbl wd_off = cmmLabelOffB lbl (wORD_SIZE * wd_off)

cmmLoadIndexW :: CmmExpr -> Int -> CmmExpr
cmmLoadIndexW base off
  = CmmLoad (cmmOffsetW base off) wordRep

-----------------------
cmmNeWord, cmmEqWord, cmmOrWord, cmmAndWord :: CmmExpr -> CmmExpr -> CmmExpr
cmmOrWord  e1 e2 = CmmMachOp mo_wordOr  [e1, e2]
cmmAndWord e1 e2 = CmmMachOp mo_wordAnd [e1, e2]
cmmNeWord  e1 e2 = CmmMachOp mo_wordNe  [e1, e2]
cmmEqWord  e1 e2 = CmmMachOp mo_wordEq  [e1, e2]
cmmULtWord e1 e2 = CmmMachOp mo_wordULt [e1, e2]
cmmUGeWord e1 e2 = CmmMachOp mo_wordUGe [e1, e2]
cmmUGtWord e1 e2 = CmmMachOp mo_wordUGt [e1, e2]
--cmmShlWord e1 e2 = CmmMachOp mo_wordShl [e1, e2]
--cmmUShrWord e1 e2 = CmmMachOp mo_wordUShr [e1, e2]
cmmSubWord e1 e2 = CmmMachOp mo_wordSub [e1, e2]

cmmNegate :: CmmExpr -> CmmExpr
cmmNegate (CmmLit (CmmInt n rep)) = CmmLit (CmmInt (-n) rep)
cmmNegate e			  = CmmMachOp (MO_S_Neg (cmmExprRep e)) [e]

blankWord :: CmmStatic
blankWord = CmmUninitialised wORD_SIZE

-- Tagging --
-- Tag bits mask
--cmmTagBits = CmmLit (mkIntCLit tAG_BITS)
cmmTagMask = CmmLit (mkIntCLit tAG_MASK)
cmmPointerMask = CmmLit (mkIntCLit (complement tAG_MASK))

-- Used to untag a possibly tagged pointer
-- A static label need not be untagged
cmmUntag e@(CmmLit (CmmLabel _)) = e
-- Default case
cmmUntag e = (e `cmmAndWord` cmmPointerMask)

cmmGetTag e = (e `cmmAndWord` cmmTagMask)

-- Test if a closure pointer is untagged
cmmIsTagged e = (e `cmmAndWord` cmmTagMask)
                 `cmmNeWord` CmmLit zeroCLit

cmmConstrTag e = (e `cmmAndWord` cmmTagMask) `cmmSubWord` (CmmLit (mkIntCLit 1))
-- Get constructor tag, but one based.
cmmConstrTag1 e = e `cmmAndWord` cmmTagMask

{-
   The family size of a data type (the number of constructors)
   can be either:
    * small, if the family size < 2**tag_bits
    * big, otherwise.

   Small families can have the constructor tag in the tag
   bits.
   Big families only use the tag value 1 to represent
   evaluatedness.
-}
isSmallFamily fam_size = fam_size <= mAX_PTR_TAG

tagForCon con = tag
    where
    con_tag           = dataConTagZ con
    fam_size   = tyConFamilySize (dataConTyCon con)
    tag | isSmallFamily fam_size = con_tag + 1
        | otherwise              = 1

--Tag an expression, to do: refactor, this appears in some other module.
tagCons con expr = cmmOffsetB expr (tagForCon con)

-- Copied from CgInfoTbls.hs
-- We keep the *zero-indexed* tag in the srt_len field of the info
-- table of a data constructor.
dataConTagZ :: DataCon -> ConTagZ
dataConTagZ con = dataConTag con - fIRST_TAG

-----------------------
--	Making literals

mkWordCLit :: StgWord -> CmmLit
mkWordCLit wd = CmmInt (fromIntegral wd) wordRep

packHalfWordsCLit :: (Integral a, Integral b) => a -> b -> CmmLit
-- Make a single word literal in which the lower_half_word is
-- at the lower address, and the upper_half_word is at the 
-- higher address
-- ToDo: consider using half-word lits instead
-- 	 but be careful: that's vulnerable when reversed
packHalfWordsCLit lower_half_word upper_half_word
#ifdef WORDS_BIGENDIAN
   = mkWordCLit ((fromIntegral lower_half_word `shiftL` hALF_WORD_SIZE_IN_BITS)
		 .|. fromIntegral upper_half_word)
#else 
   = mkWordCLit ((fromIntegral lower_half_word) 
		 .|. (fromIntegral upper_half_word `shiftL` hALF_WORD_SIZE_IN_BITS))
#endif

--------------------------------------------------------------------------
--
-- Incrementing a memory location
--
--------------------------------------------------------------------------

addToMem :: MachRep 	-- rep of the counter
	 -> CmmExpr	-- Address
	 -> Int		-- What to add (a word)
	 -> CmmStmt
addToMem rep ptr n = addToMemE rep ptr (CmmLit (CmmInt (toInteger n) rep))

addToMemE :: MachRep 	-- rep of the counter
	  -> CmmExpr	-- Address
	  -> CmmExpr	-- What to add (a word-typed expression)
	  -> CmmStmt
addToMemE rep ptr n
  = CmmStore ptr (CmmMachOp (MO_Add rep) [CmmLoad ptr rep, n])

-------------------------------------------------------------------------
--
--	Converting a closure tag to a closure for enumeration types
--      (this is the implementation of tagToEnum#).
--
-------------------------------------------------------------------------

tagToClosure :: TyCon -> CmmExpr -> CmmExpr
tagToClosure tycon tag
  = CmmLoad (cmmOffsetExprW closure_tbl tag) wordRep
  where closure_tbl = CmmLit (CmmLabel lbl)
	lbl = mkClosureTableLabel (tyConName tycon)

-------------------------------------------------------------------------
--
--	Conditionals and rts calls
--
-------------------------------------------------------------------------

emitIf :: CmmExpr 	-- Boolean
       -> Code		-- Then part
       -> Code		
-- Emit (if e then x)
-- ToDo: reverse the condition to avoid the extra branch instruction if possible
-- (some conditionals aren't reversible. eg. floating point comparisons cannot
-- be inverted because there exist some values for which both comparisons
-- return False, such as NaN.)
emitIf cond then_part
  = do { then_id <- newLabelC
       ; join_id <- newLabelC
       ; stmtC (CmmCondBranch cond then_id)
       ; stmtC (CmmBranch join_id)
       ; labelC then_id
       ; then_part
       ; labelC join_id
       }

emitIfThenElse :: CmmExpr 	-- Boolean
       		-> Code		-- Then part
       		-> Code		-- Else part
       		-> Code		
-- Emit (if e then x else y)
emitIfThenElse cond then_part else_part
  = do { then_id <- newLabelC
       ; else_id <- newLabelC
       ; join_id <- newLabelC
       ; stmtC (CmmCondBranch cond then_id)
       ; else_part
       ; stmtC (CmmBranch join_id)
       ; labelC then_id
       ; then_part
       ; labelC join_id
       }

emitRtsCall :: LitString -> [(CmmExpr,MachHint)] -> Bool -> Code
emitRtsCall fun args safe = emitRtsCall' [] fun args Nothing safe
   -- The 'Nothing' says "save all global registers"

emitRtsCallWithVols :: LitString -> [(CmmExpr,MachHint)] -> [GlobalReg] -> Bool -> Code
emitRtsCallWithVols fun args vols safe
   = emitRtsCall' [] fun args (Just vols) safe

emitRtsCallWithResult :: LocalReg -> MachHint -> LitString
	-> [(CmmExpr,MachHint)] -> Bool -> Code
emitRtsCallWithResult res hint fun args safe
   = emitRtsCall' [(res,hint)] fun args Nothing safe

-- Make a call to an RTS C procedure
emitRtsCall'
   :: CmmHintFormals
   -> LitString
   -> [(CmmExpr,MachHint)]
   -> Maybe [GlobalReg]
   -> Bool -- True <=> CmmSafe call
   -> Code
emitRtsCall' res fun args vols safe = do
  safety <- if safe
            then getSRTInfo >>= (return . CmmSafe)
            else return CmmUnsafe
  stmtsC caller_save
  stmtC (CmmCall target res args safety CmmMayReturn)
  stmtsC caller_load
  where
    (caller_save, caller_load) = callerSaveVolatileRegs vols
    target   = CmmCallee fun_expr CCallConv
    fun_expr = mkLblExpr (mkRtsCodeLabel fun)

-----------------------------------------------------------------------------
--
--	Caller-Save Registers
--
-----------------------------------------------------------------------------

-- Here we generate the sequence of saves/restores required around a
-- foreign call instruction.

-- TODO: reconcile with includes/Regs.h
--  * Regs.h claims that BaseReg should be saved last and loaded first
--    * This might not have been tickled before since BaseReg is callee save
--  * Regs.h saves SparkHd, ParkT1, SparkBase and SparkLim
callerSaveVolatileRegs :: Maybe [GlobalReg] -> ([CmmStmt], [CmmStmt])
callerSaveVolatileRegs vols = (caller_save, caller_load)
  where
    caller_save = foldr ($!) [] (map callerSaveGlobalReg    regs_to_save)
    caller_load = foldr ($!) [] (map callerRestoreGlobalReg regs_to_save)

    system_regs = [Sp,SpLim,Hp,HpLim,CurrentTSO,CurrentNursery,
		   {-SparkHd,SparkTl,SparkBase,SparkLim,-}BaseReg ]

    regs_to_save = system_regs ++ vol_list

    vol_list = case vols of Nothing -> all_of_em; Just regs -> regs

    all_of_em = [ VanillaReg n | n <- [0..mAX_Vanilla_REG] ]
	     ++ [ FloatReg   n | n <- [0..mAX_Float_REG] ]
	     ++ [ DoubleReg  n | n <- [0..mAX_Double_REG] ]
	     ++ [ LongReg    n | n <- [0..mAX_Long_REG] ]

    callerSaveGlobalReg reg next
	| callerSaves reg = 
		CmmStore (get_GlobalReg_addr reg) 
			 (CmmReg (CmmGlobal reg)) : next
	| otherwise = next

    callerRestoreGlobalReg reg next
	| callerSaves reg = 
		CmmAssign (CmmGlobal reg)
			  (CmmLoad (get_GlobalReg_addr reg) (globalRegRep reg))
			: next
	| otherwise = next

-- -----------------------------------------------------------------------------
-- Global registers

-- We map STG registers onto appropriate CmmExprs.  Either they map
-- to real machine registers or stored as offsets from BaseReg.  Given
-- a GlobalReg, get_GlobalReg_addr always produces the 
-- register table address for it.
-- (See also get_GlobalReg_reg_or_addr in MachRegs)

get_GlobalReg_addr              :: GlobalReg -> CmmExpr
get_GlobalReg_addr BaseReg = regTableOffset 0
get_GlobalReg_addr mid     = get_Regtable_addr_from_offset 
				(globalRegRep mid) (baseRegOffset mid)

-- Calculate a literal representing an offset into the register table.
-- Used when we don't have an actual BaseReg to offset from.
regTableOffset n = 
  CmmLit (CmmLabelOff mkMainCapabilityLabel (oFFSET_Capability_r + n))

get_Regtable_addr_from_offset   :: MachRep -> Int -> CmmExpr
get_Regtable_addr_from_offset rep offset =
#ifdef REG_Base
  CmmRegOff (CmmGlobal BaseReg) offset
#else
  regTableOffset offset
#endif


-- | Returns 'True' if this global register is stored in a caller-saves
-- machine register.

callerSaves :: GlobalReg -> Bool

#ifdef CALLER_SAVES_Base
callerSaves BaseReg		= True
#endif
#ifdef CALLER_SAVES_R1
callerSaves (VanillaReg 1)	= True
#endif
#ifdef CALLER_SAVES_R2
callerSaves (VanillaReg 2)	= True
#endif
#ifdef CALLER_SAVES_R3
callerSaves (VanillaReg 3)	= True
#endif
#ifdef CALLER_SAVES_R4
callerSaves (VanillaReg 4)	= True
#endif
#ifdef CALLER_SAVES_R5
callerSaves (VanillaReg 5)	= True
#endif
#ifdef CALLER_SAVES_R6
callerSaves (VanillaReg 6)	= True
#endif
#ifdef CALLER_SAVES_R7
callerSaves (VanillaReg 7)	= True
#endif
#ifdef CALLER_SAVES_R8
callerSaves (VanillaReg 8)	= True
#endif
#ifdef CALLER_SAVES_F1
callerSaves (FloatReg 1)	= True
#endif
#ifdef CALLER_SAVES_F2
callerSaves (FloatReg 2)	= True
#endif
#ifdef CALLER_SAVES_F3
callerSaves (FloatReg 3)	= True
#endif
#ifdef CALLER_SAVES_F4
callerSaves (FloatReg 4)	= True
#endif
#ifdef CALLER_SAVES_D1
callerSaves (DoubleReg 1)	= True
#endif
#ifdef CALLER_SAVES_D2
callerSaves (DoubleReg 2)	= True
#endif
#ifdef CALLER_SAVES_L1
callerSaves (LongReg 1)		= True
#endif
#ifdef CALLER_SAVES_Sp
callerSaves Sp			= True
#endif
#ifdef CALLER_SAVES_SpLim
callerSaves SpLim		= True
#endif
#ifdef CALLER_SAVES_Hp
callerSaves Hp			= True
#endif
#ifdef CALLER_SAVES_HpLim
callerSaves HpLim		= True
#endif
#ifdef CALLER_SAVES_CurrentTSO
callerSaves CurrentTSO		= True
#endif
#ifdef CALLER_SAVES_CurrentNursery
callerSaves CurrentNursery	= True
#endif
callerSaves _			= False


-- -----------------------------------------------------------------------------
-- Information about global registers

baseRegOffset :: GlobalReg -> Int

baseRegOffset (VanillaReg 1)      = oFFSET_StgRegTable_rR1
baseRegOffset (VanillaReg 2)      = oFFSET_StgRegTable_rR2
baseRegOffset (VanillaReg 3)      = oFFSET_StgRegTable_rR3
baseRegOffset (VanillaReg 4)      = oFFSET_StgRegTable_rR4
baseRegOffset (VanillaReg 5)      = oFFSET_StgRegTable_rR5
baseRegOffset (VanillaReg 6)      = oFFSET_StgRegTable_rR6
baseRegOffset (VanillaReg 7)      = oFFSET_StgRegTable_rR7
baseRegOffset (VanillaReg 8)      = oFFSET_StgRegTable_rR8
baseRegOffset (VanillaReg 9)      = oFFSET_StgRegTable_rR9
baseRegOffset (VanillaReg 10)     = oFFSET_StgRegTable_rR10
baseRegOffset (FloatReg  1)       = oFFSET_StgRegTable_rF1
baseRegOffset (FloatReg  2)       = oFFSET_StgRegTable_rF2
baseRegOffset (FloatReg  3)       = oFFSET_StgRegTable_rF3
baseRegOffset (FloatReg  4)       = oFFSET_StgRegTable_rF4
baseRegOffset (DoubleReg 1)       = oFFSET_StgRegTable_rD1
baseRegOffset (DoubleReg 2)       = oFFSET_StgRegTable_rD2
baseRegOffset Sp		  = oFFSET_StgRegTable_rSp
baseRegOffset SpLim		  = oFFSET_StgRegTable_rSpLim
baseRegOffset (LongReg 1)         = oFFSET_StgRegTable_rL1
baseRegOffset Hp		  = oFFSET_StgRegTable_rHp
baseRegOffset HpLim		  = oFFSET_StgRegTable_rHpLim
baseRegOffset CurrentTSO	  = oFFSET_StgRegTable_rCurrentTSO
baseRegOffset CurrentNursery	  = oFFSET_StgRegTable_rCurrentNursery
baseRegOffset HpAlloc		  = oFFSET_StgRegTable_rHpAlloc
baseRegOffset GCEnter1		  = oFFSET_stgGCEnter1
baseRegOffset GCFun		  = oFFSET_stgGCFun
#ifdef DEBUG
baseRegOffset BaseReg		  = panic "baseRegOffset:BaseReg"
baseRegOffset _			  = panic "baseRegOffset:other"
#endif


-------------------------------------------------------------------------
--
--	Strings generate a top-level data block
--
-------------------------------------------------------------------------

emitDataLits :: CLabel -> [CmmLit] -> Code
-- Emit a data-segment data block
emitDataLits lbl lits
  = emitData Data (CmmDataLabel lbl : map CmmStaticLit lits)

mkDataLits :: CLabel -> [CmmLit] -> GenCmmTop CmmStatic info graph
-- Emit a data-segment data block
mkDataLits lbl lits
  = CmmData Data (CmmDataLabel lbl : map CmmStaticLit lits)

emitRODataLits :: CLabel -> [CmmLit] -> Code
-- Emit a read-only data block
emitRODataLits lbl lits
  = emitData section (CmmDataLabel lbl : map CmmStaticLit lits)
  where section | any needsRelocation lits = RelocatableReadOnlyData
                | otherwise                = ReadOnlyData
        needsRelocation (CmmLabel _)      = True
        needsRelocation (CmmLabelOff _ _) = True
        needsRelocation _                 = False

mkRODataLits :: CLabel -> [CmmLit] -> GenCmmTop CmmStatic info graph
mkRODataLits lbl lits
  = CmmData section (CmmDataLabel lbl : map CmmStaticLit lits)
  where section | any needsRelocation lits = RelocatableReadOnlyData
                | otherwise                = ReadOnlyData
        needsRelocation (CmmLabel _)      = True
        needsRelocation (CmmLabelOff _ _) = True
        needsRelocation _                 = False

mkStringCLit :: String -> FCode CmmLit
-- Make a global definition for the string,
-- and return its label
mkStringCLit str = mkByteStringCLit (map (fromIntegral.ord) str)

mkByteStringCLit :: [Word8] -> FCode CmmLit
mkByteStringCLit bytes
  = do 	{ uniq <- newUnique
	; let lbl = mkStringLitLabel uniq
	; emitData ReadOnlyData [CmmDataLabel lbl, CmmString bytes]
	; return (CmmLabel lbl) }

-------------------------------------------------------------------------
--
--	Assigning expressions to temporaries
--
-------------------------------------------------------------------------

assignNonPtrTemp :: CmmExpr -> FCode CmmExpr
-- For a non-trivial expression, e, create a local
-- variable and assign the expression to it
assignNonPtrTemp e 
  | isTrivialCmmExpr e = return e
  | otherwise 	       = do { reg <- newNonPtrTemp (cmmExprRep e) 
			    ; stmtC (CmmAssign (CmmLocal reg) e)
			    ; return (CmmReg (CmmLocal reg)) }

assignPtrTemp :: CmmExpr -> FCode CmmExpr
-- For a non-trivial expression, e, create a local
-- variable and assign the expression to it
assignPtrTemp e 
  | isTrivialCmmExpr e = return e
  | otherwise 	       = do { reg <- newPtrTemp (cmmExprRep e) 
			    ; stmtC (CmmAssign (CmmLocal reg) e)
			    ; return (CmmReg (CmmLocal reg)) }

newNonPtrTemp :: MachRep -> FCode LocalReg
newNonPtrTemp rep = do { uniq <- newUnique; return (LocalReg uniq rep KindNonPtr) }

newPtrTemp :: MachRep -> FCode LocalReg
newPtrTemp rep = do { uniq <- newUnique; return (LocalReg uniq rep KindPtr) }


-------------------------------------------------------------------------
--
--	Building case analysis
--
-------------------------------------------------------------------------

emitSwitch
	:: CmmExpr  		  -- Tag to switch on
	-> [(ConTagZ, CgStmts)]	  -- Tagged branches
	-> Maybe CgStmts	  -- Default branch (if any)
	-> ConTagZ -> ConTagZ	  -- Min and Max possible values; behaviour
				  -- 	outside this range is undefined
	-> Code

-- ONLY A DEFAULT BRANCH: no case analysis to do
emitSwitch tag_expr [] (Just stmts) _ _
  = emitCgStmts stmts

-- Right, off we go
emitSwitch tag_expr branches mb_deflt lo_tag hi_tag
  = 	-- Just sort the branches before calling mk_sritch
    do	{ mb_deflt_id <-
		case mb_deflt of
		  Nothing    -> return Nothing
		  Just stmts -> do id <- forkCgStmts stmts; return (Just id)

	; dflags <- getDynFlags
	; let via_C | HscC <- hscTarget dflags = True
		    | otherwise                = False

	; stmts <- mk_switch tag_expr (sortLe le branches) 
			mb_deflt_id lo_tag hi_tag via_C
	; emitCgStmts stmts
	}
  where
    (t1,_) `le` (t2,_) = t1 <= t2


mk_switch :: CmmExpr -> [(ConTagZ, CgStmts)]
	  -> Maybe BlockId -> ConTagZ -> ConTagZ -> Bool
	  -> FCode CgStmts

-- SINGLETON TAG RANGE: no case analysis to do
mk_switch tag_expr [(tag,stmts)] _ lo_tag hi_tag via_C
  | lo_tag == hi_tag
  = ASSERT( tag == lo_tag )
    return stmts

-- SINGLETON BRANCH, NO DEFUALT: no case analysis to do
mk_switch tag_expr [(tag,stmts)] Nothing lo_tag hi_tag via_C
  = return stmts
	-- The simplifier might have eliminated a case
	-- 	 so we may have e.g. case xs of 
	--				 [] -> e
	-- In that situation we can be sure the (:) case 
	-- can't happen, so no need to test

-- SINGLETON BRANCH: one equality check to do
mk_switch tag_expr [(tag,stmts)] (Just deflt) lo_tag hi_tag via_C
  = return (CmmCondBranch cond deflt `consCgStmt` stmts)
  where
    cond  =  cmmNeWord tag_expr (CmmLit (mkIntCLit tag))
	-- We have lo_tag < hi_tag, but there's only one branch, 
	-- so there must be a default

-- ToDo: we might want to check for the two branch case, where one of
-- the branches is the tag 0, because comparing '== 0' is likely to be
-- more efficient than other kinds of comparison.

-- DENSE TAG RANGE: use a switch statment.
--
-- We also use a switch uncoditionally when compiling via C, because
-- this will get emitted as a C switch statement and the C compiler
-- should do a good job of optimising it.  Also, older GCC versions
-- (2.95 in particular) have problems compiling the complicated
-- if-trees generated by this code, so compiling to a switch every
-- time works around that problem.
--
mk_switch tag_expr branches mb_deflt lo_tag hi_tag via_C
  | use_switch 	-- Use a switch
  = do	{ branch_ids <- mapM forkCgStmts (map snd branches)
	; let 
		tagged_blk_ids = zip (map fst branches) (map Just branch_ids)

		find_branch :: ConTagZ -> Maybe BlockId
		find_branch i = assocDefault mb_deflt tagged_blk_ids i

		-- NB. we have eliminated impossible branches at
		-- either end of the range (see below), so the first
		-- tag of a real branch is real_lo_tag (not lo_tag).
		arms = [ find_branch i | i <- [real_lo_tag..real_hi_tag]]

	        switch_stmt = CmmSwitch (cmmOffset tag_expr (- real_lo_tag)) arms

	; ASSERT(not (all isNothing arms)) 
	  return (oneCgStmt switch_stmt)
	}

  -- if we can knock off a bunch of default cases with one if, then do so
  | Just deflt <- mb_deflt, (lowest_branch - lo_tag) >= n_branches
  = do { (assign_tag, tag_expr') <- assignNonPtrTemp' tag_expr
       ; let cond = cmmULtWord tag_expr' (CmmLit (mkIntCLit lowest_branch))
	     branch = CmmCondBranch cond deflt
       ; stmts <- mk_switch tag_expr' branches mb_deflt 
			lowest_branch hi_tag via_C
       ; return (assign_tag `consCgStmt` (branch `consCgStmt` stmts))
       }

  | Just deflt <- mb_deflt, (hi_tag - highest_branch) >= n_branches
  = do { (assign_tag, tag_expr') <- assignNonPtrTemp' tag_expr
       ; let cond = cmmUGtWord tag_expr' (CmmLit (mkIntCLit highest_branch))
	     branch = CmmCondBranch cond deflt
       ; stmts <- mk_switch tag_expr' branches mb_deflt 
			lo_tag highest_branch via_C
       ; return (assign_tag `consCgStmt` (branch `consCgStmt` stmts))
       }

  | otherwise	-- Use an if-tree
  = do	{ (assign_tag, tag_expr') <- assignNonPtrTemp' tag_expr
		-- To avoid duplication
	; lo_stmts <- mk_switch tag_expr' lo_branches mb_deflt 
				lo_tag (mid_tag-1) via_C
	; hi_stmts <- mk_switch tag_expr' hi_branches mb_deflt 
				mid_tag hi_tag via_C
	; hi_id <- forkCgStmts hi_stmts
	; let cond = cmmUGeWord tag_expr' (CmmLit (mkIntCLit mid_tag))
	      branch_stmt = CmmCondBranch cond hi_id
	; return (assign_tag `consCgStmt` (branch_stmt `consCgStmt` lo_stmts)) 
	}
	-- we test (e >= mid_tag) rather than (e < mid_tag), because
	-- the former works better when e is a comparison, and there
	-- are two tags 0 & 1 (mid_tag == 1).  In this case, the code
	-- generator can reduce the condition to e itself without
	-- having to reverse the sense of the comparison: comparisons
	-- can't always be easily reversed (eg. floating
	-- pt. comparisons).
  where
    use_switch 	 = {- pprTrace "mk_switch" (
			ppr tag_expr <+> text "n_tags:" <+> int n_tags <+>
                        text "branches:" <+> ppr (map fst branches) <+>
			text "n_branches:" <+> int n_branches <+>
			text "lo_tag:" <+> int lo_tag <+>
			text "hi_tag:" <+> int hi_tag <+>
			text "real_lo_tag:" <+> int real_lo_tag <+>
			text "real_hi_tag:" <+> int real_hi_tag) $ -}
		   ASSERT( n_branches > 1 && n_tags > 1 ) 
		   n_tags > 2 && (via_C || (dense && big_enough))
		 -- up to 4 branches we use a decision tree, otherwise
                 -- a switch (== jump table in the NCG).  This seems to be
                 -- optimal, and corresponds with what gcc does.
    big_enough 	 = n_branches > 4
    dense      	 = n_branches > (n_tags `div` 2)
    n_branches   = length branches
    
    -- ignore default slots at each end of the range if there's 
    -- no default branch defined.
    lowest_branch  = fst (head branches)
    highest_branch = fst (last branches)

    real_lo_tag
	| isNothing mb_deflt = lowest_branch
	| otherwise          = lo_tag

    real_hi_tag
	| isNothing mb_deflt = highest_branch
	| otherwise          = hi_tag

    n_tags = real_hi_tag - real_lo_tag + 1

	-- INVARIANT: Provided hi_tag > lo_tag (which is true)
	--	lo_tag <= mid_tag < hi_tag
	--	lo_branches have tags <  mid_tag
	--	hi_branches have tags >= mid_tag

    (mid_tag,_) = branches !! (n_branches `div` 2)
	-- 2 branches => n_branches `div` 2 = 1
	--	      => branches !! 1 give the *second* tag
	-- There are always at least 2 branches here

    (lo_branches, hi_branches) = span is_lo branches
    is_lo (t,_) = t < mid_tag


assignNonPtrTemp' e
  | isTrivialCmmExpr e = return (CmmNop, e)
  | otherwise          = do { reg <- newNonPtrTemp (cmmExprRep e)
                            ; return (CmmAssign (CmmLocal reg) e, CmmReg (CmmLocal reg)) }

emitLitSwitch :: CmmExpr			-- Tag to switch on
	      -> [(Literal, CgStmts)]		-- Tagged branches
	      -> CgStmts			-- Default branch (always)
	      -> Code				-- Emit the code
-- Used for general literals, whose size might not be a word, 
-- where there is always a default case, and where we don't know
-- the range of values for certain.  For simplicity we always generate a tree.
--
-- ToDo: for integers we could do better here, perhaps by generalising
-- mk_switch and using that.  --SDM 15/09/2004
emitLitSwitch scrut [] deflt 
  = emitCgStmts deflt
emitLitSwitch scrut branches deflt_blk
  = do	{ scrut' <- assignNonPtrTemp scrut
	; deflt_blk_id <- forkCgStmts deflt_blk
	; blk <- mk_lit_switch scrut' deflt_blk_id (sortLe le branches)
	; emitCgStmts blk }
  where
    le (t1,_) (t2,_) = t1 <= t2

mk_lit_switch :: CmmExpr -> BlockId 
 	      -> [(Literal,CgStmts)]
	      -> FCode CgStmts
mk_lit_switch scrut deflt_blk_id [(lit,blk)] 
  = return (consCgStmt if_stmt blk)
  where
    cmm_lit = mkSimpleLit lit
    rep     = cmmLitRep cmm_lit
    cond    = CmmMachOp (MO_Ne rep) [scrut, CmmLit cmm_lit]
    if_stmt = CmmCondBranch cond deflt_blk_id

mk_lit_switch scrut deflt_blk_id branches
  = do	{ hi_blk <- mk_lit_switch scrut deflt_blk_id hi_branches
 	; lo_blk <- mk_lit_switch scrut deflt_blk_id lo_branches
	; lo_blk_id <- forkCgStmts lo_blk
	; let if_stmt = CmmCondBranch cond lo_blk_id
	; return (if_stmt `consCgStmt` hi_blk) }
  where
    n_branches = length branches
    (mid_lit,_) = branches !! (n_branches `div` 2)
	-- See notes above re mid_tag

    (lo_branches, hi_branches) = span is_lo branches
    is_lo (t,_) = t < mid_lit

    cond    = CmmMachOp (mkLtOp mid_lit) 
			[scrut, CmmLit (mkSimpleLit mid_lit)]

-------------------------------------------------------------------------
--
--	Simultaneous assignment
--
-------------------------------------------------------------------------


emitSimultaneously :: CmmStmts -> Code
-- Emit code to perform the assignments in the
-- input simultaneously, using temporary variables when necessary.
--
-- The Stmts must be:
--	CmmNop, CmmComment, CmmAssign, CmmStore
-- and nothing else


-- We use the strongly-connected component algorithm, in which
--	* the vertices are the statements
--	* an edge goes from s1 to s2 iff
--		s1 assigns to something s2 uses
--	  that is, if s1 should *follow* s2 in the final order

type CVertex = (Int, CmmStmt)	-- Give each vertex a unique number,
				-- for fast comparison

emitSimultaneously stmts
  = codeOnly $
    case filterOut isNopStmt (stmtList stmts) of 
	-- Remove no-ops
      []     	-> nopC
      [stmt] 	-> stmtC stmt	-- It's often just one stmt
      stmt_list -> doSimultaneously1 (zip [(1::Int)..] stmt_list)

doSimultaneously1 :: [CVertex] -> Code
doSimultaneously1 vertices
  = let
	edges = [ (vertex, key1, edges_from stmt1)
		| vertex@(key1, stmt1) <- vertices
		]
	edges_from stmt1 = [ key2 | (key2, stmt2) <- vertices, 
				    stmt1 `mustFollow` stmt2
			   ]
	components = stronglyConnComp edges

	-- do_components deal with one strongly-connected component
	-- Not cyclic, or singleton?  Just do it
	do_component (AcyclicSCC (n,stmt))  = stmtC stmt
	do_component (CyclicSCC [(n,stmt)]) = stmtC stmt

		-- Cyclic?  Then go via temporaries.  Pick one to
		-- break the loop and try again with the rest.
	do_component (CyclicSCC ((n,first_stmt) : rest))
	  = do	{ from_temp <- go_via_temp first_stmt
		; doSimultaneously1 rest
		; stmtC from_temp }

	go_via_temp (CmmAssign dest src)
	  = do	{ tmp <- newNonPtrTemp (cmmRegRep dest) -- TODO FIXME NOW if the pair of assignments move across a call this will be wrong
		; stmtC (CmmAssign (CmmLocal tmp) src)
		; return (CmmAssign dest (CmmReg (CmmLocal tmp))) }
	go_via_temp (CmmStore dest src)
	  = do	{ tmp <- newNonPtrTemp (cmmExprRep src) -- TODO FIXME NOW if the pair of assignemnts move across a call this will be wrong
		; stmtC (CmmAssign (CmmLocal tmp) src)
		; return (CmmStore dest (CmmReg (CmmLocal tmp))) }
    in
    mapCs do_component components

mustFollow :: CmmStmt -> CmmStmt -> Bool
CmmAssign reg _  `mustFollow` stmt = anySrc (reg `regUsedIn`) stmt
CmmStore loc e   `mustFollow` stmt = anySrc (locUsedIn loc (cmmExprRep e)) stmt
CmmNop           `mustFollow` stmt = False
CmmComment _     `mustFollow` stmt = False


anySrc :: (CmmExpr -> Bool) -> CmmStmt -> Bool
-- True if the fn is true of any input of the stmt
anySrc p (CmmAssign _ e)    = p e
anySrc p (CmmStore e1 e2)   = p e1 || p e2	-- Might be used in either side
anySrc p (CmmComment _)	    = False
anySrc p CmmNop		    = False
anySrc p other		    = True		-- Conservative

regUsedIn :: CmmReg -> CmmExpr -> Bool
reg `regUsedIn` CmmLit _ 	 = False
reg `regUsedIn` CmmLoad e  _ 	 = reg `regUsedIn` e
reg `regUsedIn` CmmReg reg' 	 = reg == reg'
reg `regUsedIn` CmmRegOff reg' _ = reg == reg'
reg `regUsedIn` CmmMachOp _ es   = any (reg `regUsedIn`) es

locUsedIn :: CmmExpr -> MachRep -> CmmExpr -> Bool
-- (locUsedIn a r e) checks whether writing to r[a] could affect the value of
-- 'e'.  Returns True if it's not sure.
locUsedIn loc rep (CmmLit _) 	     = False
locUsedIn loc rep (CmmLoad e ld_rep) = possiblySameLoc loc rep e ld_rep
locUsedIn loc rep (CmmReg reg')      = False
locUsedIn loc rep (CmmRegOff reg' _) = False
locUsedIn loc rep (CmmMachOp _ es)   = any (locUsedIn loc rep) es

possiblySameLoc :: CmmExpr -> MachRep -> CmmExpr -> MachRep -> Bool
-- Assumes that distinct registers (eg Hp, Sp) do not 
-- point to the same location, nor any offset thereof.
possiblySameLoc (CmmReg r1)       rep1 (CmmReg r2)      rep2  = r1==r2
possiblySameLoc (CmmReg r1)       rep1 (CmmRegOff r2 0) rep2  = r1==r2
possiblySameLoc (CmmRegOff r1 0)  rep1 (CmmReg r2)      rep2  = r1==r2
possiblySameLoc (CmmRegOff r1 start1) rep1 (CmmRegOff r2 start2) rep2 
  = r1==r2 && end1 > start2 && end2 > start1
  where
    end1 = start1 + machRepByteWidth rep1
    end2 = start2 + machRepByteWidth rep2

possiblySameLoc l1 rep1 (CmmLit _) rep2 = False
possiblySameLoc l1 rep1 l2	   rep2 = True	-- Conservative

-------------------------------------------------------------------------
--
--	Static Reference Tables
--
-------------------------------------------------------------------------

-- There is just one SRT for each top level binding; all the nested
-- bindings use sub-sections of this SRT.  The label is passed down to
-- the nested bindings via the monad.

getSRTInfo :: FCode C_SRT
getSRTInfo = do
  srt_lbl <- getSRTLabel
  srt <- getSRT
  case srt of
    -- TODO: Should we panic in this case?
    -- Someone obviously thinks there should be an SRT
    NoSRT -> return NoC_SRT
    SRT off len bmp
      | len > hALF_WORD_SIZE_IN_BITS || bmp == [fromIntegral srt_escape]
      -> do id <- newUnique
            let srt_desc_lbl = mkLargeSRTLabel id
	    emitRODataLits srt_desc_lbl
             ( cmmLabelOffW srt_lbl off
	       : mkWordCLit (fromIntegral len)
	       : map mkWordCLit bmp)
	    return (C_SRT srt_desc_lbl 0 srt_escape)

    SRT off len bmp
      | otherwise 
      -> return (C_SRT srt_lbl off (fromIntegral (head bmp)))
		-- The fromIntegral converts to StgHalfWord

srt_escape = (-1) :: StgHalfWord
