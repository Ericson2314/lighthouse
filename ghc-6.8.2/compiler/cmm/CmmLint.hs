{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2004-2006
--
-- CmmLint: checking the correctness of Cmm statements and expressions
--
-----------------------------------------------------------------------------

module CmmLint (
  cmmLint, cmmLintTop
  ) where

#include "HsVersions.h"

import Cmm
import CLabel
import MachOp
import Outputable
import PprCmm
import Unique
import Constants

import Control.Monad

-- -----------------------------------------------------------------------------
-- Exported entry points:

cmmLint :: GenCmm d h (ListGraph CmmStmt) -> Maybe SDoc
cmmLint (Cmm tops) = runCmmLint $ mapM_ lintCmmTop tops

cmmLintTop :: GenCmmTop d h (ListGraph CmmStmt) -> Maybe SDoc
cmmLintTop top = runCmmLint $ lintCmmTop top

runCmmLint :: CmmLint a -> Maybe SDoc
runCmmLint l = 
   case unCL l of
	Left err -> Just (ptext SLIT("Cmm lint error:") $$ nest 2 err)
	Right _  -> Nothing

lintCmmTop (CmmProc _ lbl _ (ListGraph blocks))
  = addLintInfo (text "in proc " <> pprCLabel lbl) $
	mapM_ lintCmmBlock blocks
lintCmmTop _other
  = return ()

lintCmmBlock (BasicBlock id stmts)
  = addLintInfo (text "in basic block " <> ppr (getUnique id)) $
	mapM_ lintCmmStmt stmts

-- -----------------------------------------------------------------------------
-- lintCmmExpr

-- Checks whether a CmmExpr is "type-correct", and check for obvious-looking
-- byte/word mismatches.

lintCmmExpr :: CmmExpr -> CmmLint MachRep
lintCmmExpr (CmmLoad expr rep) = do
  lintCmmExpr expr
  when (machRepByteWidth rep >= wORD_SIZE) $
     cmmCheckWordAddress expr
  return rep
lintCmmExpr expr@(CmmMachOp op args) = do
  mapM_ lintCmmExpr args
  if map cmmExprRep args == machOpArgReps op
  	then cmmCheckMachOp op args
	else cmmLintMachOpErr expr (map cmmExprRep args) (machOpArgReps op)
lintCmmExpr (CmmRegOff reg offset)
  = lintCmmExpr (CmmMachOp (MO_Add rep) 
		[CmmReg reg, CmmLit (CmmInt (fromIntegral offset) rep)])
  where rep = cmmRegRep reg
lintCmmExpr lit@(CmmLit (CmmInt _ rep))
  | isFloatingRep rep
  = cmmLintErr (text "integer literal with floating MachRep: " <> ppr lit)
lintCmmExpr expr = 
  return (cmmExprRep expr)

-- Check for some common byte/word mismatches (eg. Sp + 1)
cmmCheckMachOp  op args@[CmmReg reg, CmmLit (CmmInt i _)]
  | isWordOffsetReg reg && isOffsetOp op && i `rem` fromIntegral wORD_SIZE /= 0
  = cmmLintDubiousWordOffset (CmmMachOp op args)
cmmCheckMachOp op [lit@(CmmLit (CmmInt i _)), reg@(CmmReg _)]
  = cmmCheckMachOp op [reg, lit]
cmmCheckMachOp op@(MO_U_Conv from to) args
  | isFloatingRep from || isFloatingRep to
  = cmmLintErr (text "unsigned conversion from/to floating rep: " 
		<> ppr (CmmMachOp op args))
cmmCheckMachOp op args
  = return (resultRepOfMachOp op)

isWordOffsetReg (CmmGlobal Sp) = True
-- No warnings for unaligned arithmetic, which is used to tag dynamic constructor closures.
--isWordOffsetReg (CmmGlobal Hp) = True
isWordOffsetReg _ = False

isOffsetOp (MO_Add _) = True
isOffsetOp (MO_Sub _) = True
isOffsetOp _ = False

-- This expression should be an address from which a word can be loaded:
-- check for funny-looking sub-word offsets.
cmmCheckWordAddress e@(CmmMachOp op [arg, CmmLit (CmmInt i _)])
  | isOffsetOp op && notNodeReg arg && i `rem` fromIntegral wORD_SIZE /= 0
  = cmmLintDubiousWordOffset e
cmmCheckWordAddress e@(CmmMachOp op [CmmLit (CmmInt i _), arg])
  | isOffsetOp op && notNodeReg arg && i `rem` fromIntegral wORD_SIZE /= 0
  = cmmLintDubiousWordOffset e
cmmCheckWordAddress _
  = return ()

-- No warnings for unaligned arithmetic with the node register,
-- which is used to extract fields from tagged constructor closures.
notNodeReg (CmmReg reg) | reg == nodeReg = False
notNodeReg _                             = True

lintCmmStmt :: CmmStmt -> CmmLint ()
lintCmmStmt stmt@(CmmAssign reg expr) = do
  erep <- lintCmmExpr expr
  if (erep == cmmRegRep reg)
	then return ()
	else cmmLintAssignErr stmt
lintCmmStmt (CmmStore l r) = do
  lintCmmExpr l
  lintCmmExpr r
  return ()
lintCmmStmt (CmmCall _target _res args _ _) = mapM_ (lintCmmExpr.fst) args
lintCmmStmt (CmmCondBranch e _id)   = lintCmmExpr e >> checkCond e >> return ()
lintCmmStmt (CmmSwitch e _branches) = do
  erep <- lintCmmExpr e
  if (erep == wordRep)
    then return ()
    else cmmLintErr (text "switch scrutinee is not a word: " <> ppr e)
lintCmmStmt (CmmJump e _args)       = lintCmmExpr e >> return ()
lintCmmStmt _other 		    = return ()

checkCond (CmmMachOp mop _) | isComparisonMachOp mop = return ()
checkCond expr = cmmLintErr (hang (text "expression is not a conditional:") 2
				    (ppr expr))

-- -----------------------------------------------------------------------------
-- CmmLint monad

-- just a basic error monad:

newtype CmmLint a = CmmLint { unCL :: Either SDoc a }

instance Monad CmmLint where
  CmmLint m >>= k = CmmLint $ case m of 
				Left e -> Left e
				Right a -> unCL (k a)
  return a = CmmLint (Right a)

cmmLintErr :: SDoc -> CmmLint a
cmmLintErr msg = CmmLint (Left msg)

addLintInfo :: SDoc -> CmmLint a -> CmmLint a
addLintInfo info thing = CmmLint $ 
   case unCL thing of
	Left err -> Left (hang info 2 err)
	Right a  -> Right a

cmmLintMachOpErr :: CmmExpr -> [MachRep] -> [MachRep] -> CmmLint a
cmmLintMachOpErr expr argsRep opExpectsRep
     = cmmLintErr (text "in MachOp application: " $$ 
					nest 2 (pprExpr expr) $$
				        (text "op is expecting: " <+> ppr opExpectsRep) $$
					(text "arguments provide: " <+> ppr argsRep))

cmmLintAssignErr :: CmmStmt -> CmmLint a
cmmLintAssignErr stmt = cmmLintErr (text "in assignment: " $$ 
					nest 2 (pprStmt stmt))

cmmLintDubiousWordOffset :: CmmExpr -> CmmLint a
cmmLintDubiousWordOffset expr
   = cmmLintErr (text "offset is not a multiple of words: " $$
			nest 2 (pprExpr expr))
