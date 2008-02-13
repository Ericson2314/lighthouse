{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-----------------------------------------------------------------------------
--
-- Cmm optimisation
--
-- (c) The University of Glasgow 2006
--
-----------------------------------------------------------------------------

module CmmOpt (
	cmmMiniInline,
	cmmMachOpFold,
	cmmLoopifyForC,
 ) where

#include "HsVersions.h"

import Cmm
import CmmUtils
import CLabel
import MachOp
import StaticFlags

import UniqFM
import Unique

import Outputable

import Data.Bits
import Data.Word
import Data.Int
import GHC.Exts

-- -----------------------------------------------------------------------------
-- The mini-inliner

{-
This pass inlines assignments to temporaries that are used just
once.  It works as follows:

  - count uses of each temporary
  - for each temporary that occurs just once:
	- attempt to push it forward to the statement that uses it
        - only push forward past assignments to other temporaries
	  (assumes that temporaries are single-assignment)
	- if we reach the statement that uses it, inline the rhs
	  and delete the original assignment.

Possible generalisations: here is an example from factorial

Fac_zdwfac_entry:
    cmG:
        _smi = R2;
        if (_smi != 0) goto cmK;
        R1 = R3;
        jump I64[Sp];
    cmK:
        _smn = _smi * R3;
        R2 = _smi + (-1);
        R3 = _smn;
        jump Fac_zdwfac_info;

We want to inline _smi and _smn.  To inline _smn:

   - we must be able to push forward past assignments to global regs.
     We can do this if the rhs of the assignment we are pushing
     forward doesn't refer to the global reg being assigned to; easy
     to test.

To inline _smi:

   - It is a trivial replacement, reg for reg, but it occurs more than
     once.
   - We can inline trivial assignments even if the temporary occurs
     more than once, as long as we don't eliminate the original assignment
     (this doesn't help much on its own).
   - We need to be able to propagate the assignment forward through jumps;
     if we did this, we would find that it can be inlined safely in all
     its occurrences.
-}

cmmMiniInline :: [CmmBasicBlock] -> [CmmBasicBlock]
cmmMiniInline blocks = map do_inline blocks 
  where 
	blockUses (BasicBlock _ stmts)
	 = foldr (plusUFM_C (+)) emptyUFM (map getStmtUses stmts)

	uses = foldr (plusUFM_C (+)) emptyUFM (map blockUses blocks)

	do_inline (BasicBlock id stmts)
	 = BasicBlock id (cmmMiniInlineStmts uses stmts)


cmmMiniInlineStmts :: UniqFM Int -> [CmmStmt] -> [CmmStmt]
cmmMiniInlineStmts uses [] = []
cmmMiniInlineStmts uses (stmt@(CmmAssign (CmmLocal (LocalReg u _ _)) expr) : stmts)
  | Just 1 <- lookupUFM uses u,
    Just stmts' <- lookForInline u expr stmts
  = 
#ifdef NCG_DEBUG
     trace ("nativeGen: inlining " ++ showSDoc (pprStmt stmt)) $
#endif
     cmmMiniInlineStmts uses stmts'

cmmMiniInlineStmts uses (stmt:stmts)
  = stmt : cmmMiniInlineStmts uses stmts


-- Try to inline a temporary assignment.  We can skip over assignments to
-- other tempoararies, because we know that expressions aren't side-effecting
-- and temporaries are single-assignment.
lookForInline u expr (stmt@(CmmAssign (CmmLocal (LocalReg u' _ _)) rhs) : rest)
  | u /= u' 
  = case lookupUFM (getExprUses rhs) u of
	Just 1 -> Just (inlineStmt u expr stmt : rest)
	_other -> case lookForInline u expr rest of
		     Nothing    -> Nothing
		     Just stmts -> Just (stmt:stmts)

lookForInline u expr (CmmNop : rest)
  = lookForInline u expr rest

lookForInline u expr (stmt:stmts)
  = case lookupUFM (getStmtUses stmt) u of
	Just 1 | ok_to_inline -> Just (inlineStmt u expr stmt : stmts)
	_other -> Nothing
  where
	-- we don't inline into CmmCall if the expression refers to global
	-- registers.  This is a HACK to avoid global registers clashing with
	-- C argument-passing registers, really the back-end ought to be able
	-- to handle it properly, but currently neither PprC nor the NCG can
	-- do it.  See also CgForeignCall:load_args_into_temps.
    ok_to_inline = case stmt of
		     CmmCall{} -> hasNoGlobalRegs expr
		     _ -> True

-- -----------------------------------------------------------------------------
-- Boring Cmm traversals for collecting usage info and substitutions.

getStmtUses :: CmmStmt -> UniqFM Int
getStmtUses (CmmAssign _ e) = getExprUses e
getStmtUses (CmmStore e1 e2) = plusUFM_C (+) (getExprUses e1) (getExprUses e2)
getStmtUses (CmmCall target _ es _ _)
   = plusUFM_C (+) (uses target) (getExprsUses (map fst es))
   where uses (CmmCallee e _) = getExprUses e
	 uses _ = emptyUFM
getStmtUses (CmmCondBranch e _) = getExprUses e
getStmtUses (CmmSwitch e _) = getExprUses e
getStmtUses (CmmJump e _) = getExprUses e
getStmtUses _ = emptyUFM

getExprUses :: CmmExpr -> UniqFM Int
getExprUses (CmmReg (CmmLocal (LocalReg u _ _))) = unitUFM u 1
getExprUses (CmmRegOff (CmmLocal (LocalReg u _ _)) _) = unitUFM u 1
getExprUses (CmmLoad e _) = getExprUses e
getExprUses (CmmMachOp _ es) = getExprsUses es
getExprUses _other = emptyUFM

getExprsUses es = foldr (plusUFM_C (+)) emptyUFM (map getExprUses es)

inlineStmt :: Unique -> CmmExpr -> CmmStmt -> CmmStmt
inlineStmt u a (CmmAssign r e) = CmmAssign r (inlineExpr u a e)
inlineStmt u a (CmmStore e1 e2) = CmmStore (inlineExpr u a e1) (inlineExpr u a e2)
inlineStmt u a (CmmCall target regs es srt ret)
   = CmmCall (infn target) regs es' srt ret
   where infn (CmmCallee fn cconv) = CmmCallee fn cconv
	 infn (CmmPrim p) = CmmPrim p
	 es' = [ (inlineExpr u a e, hint) | (e,hint) <- es ]
inlineStmt u a (CmmCondBranch e d) = CmmCondBranch (inlineExpr u a e) d
inlineStmt u a (CmmSwitch e d) = CmmSwitch (inlineExpr u a e) d
inlineStmt u a (CmmJump e d) = CmmJump (inlineExpr u a e) d
inlineStmt u a other_stmt = other_stmt

inlineExpr :: Unique -> CmmExpr -> CmmExpr -> CmmExpr
inlineExpr u a e@(CmmReg (CmmLocal (LocalReg u' _ _)))
  | u == u' = a
  | otherwise = e
inlineExpr u a e@(CmmRegOff (CmmLocal (LocalReg u' rep _)) off)
  | u == u' = CmmMachOp (MO_Add rep) [a, CmmLit (CmmInt (fromIntegral off) rep)]
  | otherwise = e
inlineExpr u a (CmmLoad e rep) = CmmLoad (inlineExpr u a e) rep
inlineExpr u a (CmmMachOp op es) = CmmMachOp op (map (inlineExpr u a) es)
inlineExpr u a other_expr = other_expr

-- -----------------------------------------------------------------------------
-- MachOp constant folder

-- Now, try to constant-fold the MachOps.  The arguments have already
-- been optimized and folded.

cmmMachOpFold
    :: MachOp	    	-- The operation from an CmmMachOp
    -> [CmmExpr]   	-- The optimized arguments
    -> CmmExpr

cmmMachOpFold op arg@[CmmLit (CmmInt x rep)]
  = case op of
      MO_S_Neg r -> CmmLit (CmmInt (-x) rep)
      MO_Not r   -> CmmLit (CmmInt (complement x) rep)

	-- these are interesting: we must first narrow to the 
	-- "from" type, in order to truncate to the correct size.
	-- The final narrow/widen to the destination type
	-- is implicit in the CmmLit.
      MO_S_Conv from to
	   | isFloatingRep to -> CmmLit (CmmFloat (fromInteger x) to)
	   | otherwise        -> CmmLit (CmmInt (narrowS from x) to)
      MO_U_Conv from to -> CmmLit (CmmInt (narrowU from x) to)

      _ -> panic "cmmMachOpFold: unknown unary op"


-- Eliminate conversion NOPs
cmmMachOpFold (MO_S_Conv rep1 rep2) [x] | rep1 == rep2 = x
cmmMachOpFold (MO_U_Conv rep1 rep2) [x] | rep1 == rep2 = x

-- Eliminate nested conversions where possible
cmmMachOpFold conv_outer args@[CmmMachOp conv_inner [x]]
  | Just (rep1,rep2,signed1) <- isIntConversion conv_inner,
    Just (_,   rep3,signed2) <- isIntConversion conv_outer
  = case () of
	-- widen then narrow to the same size is a nop
      _ | rep1 < rep2 && rep1 == rep3 -> x
	-- Widen then narrow to different size: collapse to single conversion
	-- but remember to use the signedness from the widening, just in case
	-- the final conversion is a widen.
	| rep1 < rep2 && rep2 > rep3 ->
	    cmmMachOpFold (intconv signed1 rep1 rep3) [x]
	-- Nested widenings: collapse if the signedness is the same
	| rep1 < rep2 && rep2 < rep3 && signed1 == signed2 ->
	    cmmMachOpFold (intconv signed1 rep1 rep3) [x]
	-- Nested narrowings: collapse
	| rep1 > rep2 && rep2 > rep3 ->
	    cmmMachOpFold (MO_U_Conv rep1 rep3) [x]
	| otherwise ->
	    CmmMachOp conv_outer args
  where
	isIntConversion (MO_U_Conv rep1 rep2) 
	  | not (isFloatingRep rep1) && not (isFloatingRep rep2) 
	  = Just (rep1,rep2,False)
	isIntConversion (MO_S_Conv rep1 rep2)
	  | not (isFloatingRep rep1) && not (isFloatingRep rep2) 
	  = Just (rep1,rep2,True)
	isIntConversion _ = Nothing

	intconv True  = MO_S_Conv
	intconv False = MO_U_Conv

-- ToDo: a narrow of a load can be collapsed into a narrow load, right?
-- but what if the architecture only supports word-sized loads, should
-- we do the transformation anyway?

cmmMachOpFold mop args@[CmmLit (CmmInt x xrep), CmmLit (CmmInt y _)]
  = case mop of
	-- for comparisons: don't forget to narrow the arguments before
	-- comparing, since they might be out of range.
    	MO_Eq r   -> CmmLit (CmmInt (if x_u == y_u then 1 else 0) wordRep)
    	MO_Ne r   -> CmmLit (CmmInt (if x_u /= y_u then 1 else 0) wordRep)

    	MO_U_Gt r -> CmmLit (CmmInt (if x_u >  y_u then 1 else 0) wordRep)
    	MO_U_Ge r -> CmmLit (CmmInt (if x_u >= y_u then 1 else 0) wordRep)
    	MO_U_Lt r -> CmmLit (CmmInt (if x_u <  y_u then 1 else 0) wordRep)
    	MO_U_Le r -> CmmLit (CmmInt (if x_u <= y_u then 1 else 0) wordRep)

    	MO_S_Gt r -> CmmLit (CmmInt (if x_s >  y_s then 1 else 0) wordRep) 
    	MO_S_Ge r -> CmmLit (CmmInt (if x_s >= y_s then 1 else 0) wordRep)
    	MO_S_Lt r -> CmmLit (CmmInt (if x_s <  y_s then 1 else 0) wordRep)
    	MO_S_Le r -> CmmLit (CmmInt (if x_s <= y_s then 1 else 0) wordRep)

    	MO_Add r -> CmmLit (CmmInt (x + y) r)
    	MO_Sub r -> CmmLit (CmmInt (x - y) r)
    	MO_Mul r -> CmmLit (CmmInt (x * y) r)
    	MO_S_Quot r | y /= 0 -> CmmLit (CmmInt (x `quot` y) r)
    	MO_S_Rem  r | y /= 0 -> CmmLit (CmmInt (x `rem` y) r)

	MO_And   r -> CmmLit (CmmInt (x .&. y) r)
	MO_Or    r -> CmmLit (CmmInt (x .|. y) r)
	MO_Xor   r -> CmmLit (CmmInt (x `xor` y) r)

        MO_Shl   r -> CmmLit (CmmInt (x `shiftL` fromIntegral y) r)
        MO_U_Shr r -> CmmLit (CmmInt (x_u `shiftR` fromIntegral y) r)
        MO_S_Shr r -> CmmLit (CmmInt (x `shiftR` fromIntegral y) r)

	other      -> CmmMachOp mop args

   where
	x_u = narrowU xrep x
	y_u = narrowU xrep y
	x_s = narrowS xrep x
	y_s = narrowS xrep y
	

-- When possible, shift the constants to the right-hand side, so that we
-- can match for strength reductions.  Note that the code generator will
-- also assume that constants have been shifted to the right when
-- possible.

cmmMachOpFold op [x@(CmmLit _), y]
   | not (isLit y) && isCommutableMachOp op 
   = cmmMachOpFold op [y, x]

-- Turn (a+b)+c into a+(b+c) where possible.  Because literals are
-- moved to the right, it is more likely that we will find
-- opportunities for constant folding when the expression is
-- right-associated.
--
-- ToDo: this appears to introduce a quadratic behaviour due to the
-- nested cmmMachOpFold.  Can we fix this?
--
-- Why do we check isLit arg1?  If arg1 is a lit, it means that arg2
-- is also a lit (otherwise arg1 would be on the right).  If we
-- put arg1 on the left of the rearranged expression, we'll get into a
-- loop:  (x1+x2)+x3 => x1+(x2+x3)  => (x2+x3)+x1 => x2+(x3+x1) ...
--
-- Also don't do it if arg1 is PicBaseReg, so that we don't separate the
-- PicBaseReg from the corresponding label (or label difference).
--
cmmMachOpFold mop1 [CmmMachOp mop2 [arg1,arg2], arg3]
   | mop1 == mop2 && isAssociativeMachOp mop1
     && not (isLit arg1) && not (isPicReg arg1)
   = cmmMachOpFold mop1 [arg1, cmmMachOpFold mop2 [arg2,arg3]]

-- Make a RegOff if we can
cmmMachOpFold (MO_Add _) [CmmReg reg, CmmLit (CmmInt n rep)]
  = CmmRegOff reg (fromIntegral (narrowS rep n))
cmmMachOpFold (MO_Add _) [CmmRegOff reg off, CmmLit (CmmInt n rep)]
  = CmmRegOff reg (off + fromIntegral (narrowS rep n))
cmmMachOpFold (MO_Sub _) [CmmReg reg, CmmLit (CmmInt n rep)]
  = CmmRegOff reg (- fromIntegral (narrowS rep n))
cmmMachOpFold (MO_Sub _) [CmmRegOff reg off, CmmLit (CmmInt n rep)]
  = CmmRegOff reg (off - fromIntegral (narrowS rep n))

-- Fold label(+/-)offset into a CmmLit where possible

cmmMachOpFold (MO_Add _) [CmmLit (CmmLabel lbl), CmmLit (CmmInt i rep)]
  = CmmLit (CmmLabelOff lbl (fromIntegral (narrowU rep i)))
cmmMachOpFold (MO_Add _) [CmmLit (CmmInt i rep), CmmLit (CmmLabel lbl)]
  = CmmLit (CmmLabelOff lbl (fromIntegral (narrowU rep i)))
cmmMachOpFold (MO_Sub _) [CmmLit (CmmLabel lbl), CmmLit (CmmInt i rep)]
  = CmmLit (CmmLabelOff lbl (fromIntegral (negate (narrowU rep i))))


-- Comparison of literal with narrowed/widened operand: perform
-- the comparison at a different width, as long as the literal is
-- within range.

#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
-- powerPC NCG has a TODO for I8/I16 comparisons, so don't try

cmmMachOpFold cmp [CmmMachOp conv [x], CmmLit (CmmInt i _)]
  | Just (rep, narrow) <- maybe_conversion conv,
    Just narrow_cmp <- maybe_comparison cmp rep,
    let narrow_i = narrow rep i,
    narrow_i == i
  = cmmMachOpFold narrow_cmp [x, CmmLit (CmmInt narrow_i rep)]
 where
    maybe_conversion (MO_U_Conv from _) = Just (from, narrowU)
    maybe_conversion (MO_S_Conv from _)
        | not (isFloatingRep from) = Just (from, narrowS)
        -- don't attempt to apply this optimisation when the source
        -- is a float; see #1916
    maybe_conversion _ = Nothing
    
    maybe_comparison (MO_U_Gt _) rep = Just (MO_U_Gt rep)
    maybe_comparison (MO_U_Ge _) rep = Just (MO_U_Ge rep)
    maybe_comparison (MO_U_Lt _) rep = Just (MO_U_Lt rep)
    maybe_comparison (MO_U_Le _) rep = Just (MO_U_Le rep)
    maybe_comparison (MO_S_Gt _) rep = Just (MO_S_Gt rep)
    maybe_comparison (MO_S_Ge _) rep = Just (MO_S_Ge rep)
    maybe_comparison (MO_S_Lt _) rep = Just (MO_S_Lt rep)
    maybe_comparison (MO_S_Le _) rep = Just (MO_S_Le rep)
    maybe_comparison (MO_Eq   _) rep = Just (MO_Eq   rep)
    maybe_comparison _ _ = Nothing

#endif

-- We can often do something with constants of 0 and 1 ...

cmmMachOpFold mop args@[x, y@(CmmLit (CmmInt 0 _))]
  = case mop of
    	MO_Add   r -> x
    	MO_Sub   r -> x
    	MO_Mul   r -> y
    	MO_And   r -> y
    	MO_Or    r -> x
    	MO_Xor   r -> x
    	MO_Shl   r -> x
    	MO_S_Shr r -> x
    	MO_U_Shr r -> x
        MO_Ne    r | isComparisonExpr x -> x
	MO_Eq    r | Just x' <- maybeInvertConditionalExpr x -> x'
	MO_U_Gt  r | isComparisonExpr x -> x
	MO_S_Gt  r | isComparisonExpr x -> x
	MO_U_Lt  r | isComparisonExpr x -> CmmLit (CmmInt 0 wordRep)
	MO_S_Lt  r | isComparisonExpr x -> CmmLit (CmmInt 0 wordRep)
	MO_U_Ge  r | isComparisonExpr x -> CmmLit (CmmInt 1 wordRep)
	MO_S_Ge  r | isComparisonExpr x -> CmmLit (CmmInt 1 wordRep)
	MO_U_Le  r | Just x' <- maybeInvertConditionalExpr x -> x'
	MO_S_Le  r | Just x' <- maybeInvertConditionalExpr x -> x'
    	other    -> CmmMachOp mop args

cmmMachOpFold mop args@[x, y@(CmmLit (CmmInt 1 rep))]
  = case mop of
    	MO_Mul    r -> x
    	MO_S_Quot r -> x
    	MO_U_Quot r -> x
    	MO_S_Rem  r -> CmmLit (CmmInt 0 rep)
    	MO_U_Rem  r -> CmmLit (CmmInt 0 rep)
        MO_Ne    r | Just x' <- maybeInvertConditionalExpr x -> x'
	MO_Eq    r | isComparisonExpr x -> x
	MO_U_Lt  r | Just x' <- maybeInvertConditionalExpr x -> x'
	MO_S_Lt  r | Just x' <- maybeInvertConditionalExpr x -> x'
	MO_U_Gt  r | isComparisonExpr x -> CmmLit (CmmInt 0 wordRep)
	MO_S_Gt  r | isComparisonExpr x -> CmmLit (CmmInt 0 wordRep)
	MO_U_Le  r | isComparisonExpr x -> CmmLit (CmmInt 1 wordRep)
	MO_S_Le  r | isComparisonExpr x -> CmmLit (CmmInt 1 wordRep)
	MO_U_Ge  r | isComparisonExpr x -> x
	MO_S_Ge  r | isComparisonExpr x -> x
    	other       -> CmmMachOp mop args

-- Now look for multiplication/division by powers of 2 (integers).

cmmMachOpFold mop args@[x, y@(CmmLit (CmmInt n _))]
  = case mop of
    	MO_Mul rep
	   | Just p <- exactLog2 n ->
                 CmmMachOp (MO_Shl rep) [x, CmmLit (CmmInt p rep)]
    	MO_S_Quot rep
	   | Just p <- exactLog2 n, 
	     CmmReg _ <- x ->	-- We duplicate x below, hence require
				-- it is a reg.  FIXME: remove this restriction.
		-- shift right is not the same as quot, because it rounds
		-- to minus infinity, whereasq uot rounds toward zero.
		-- To fix this up, we add one less than the divisor to the
		-- dividend if it is a negative number.
		--
		-- to avoid a test/jump, we use the following sequence:
		-- 	x1 = x >> word_size-1  (all 1s if -ve, all 0s if +ve)
		--      x2 = y & (divisor-1)
		--      result = (x+x2) >>= log2(divisor)
		-- this could be done a bit more simply using conditional moves,
		-- but we're processor independent here.
		--
		-- we optimise the divide by 2 case slightly, generating
		--      x1 = x >> word_size-1  (unsigned)
		--      return = (x + x1) >>= log2(divisor)
		let 
		    bits = fromIntegral (machRepBitWidth rep) - 1
		    shr = if p == 1 then MO_U_Shr rep else MO_S_Shr rep
		    x1 = CmmMachOp shr [x, CmmLit (CmmInt bits rep)]
		    x2 = if p == 1 then x1 else
			 CmmMachOp (MO_And rep) [x1, CmmLit (CmmInt (n-1) rep)]
		    x3 = CmmMachOp (MO_Add rep) [x, x2]
		in
                CmmMachOp (MO_S_Shr rep) [x3, CmmLit (CmmInt p rep)]
    	other
           -> unchanged
    where
       unchanged = CmmMachOp mop args

-- Anything else is just too hard.

cmmMachOpFold mop args = CmmMachOp mop args

-- -----------------------------------------------------------------------------
-- exactLog2

-- This algorithm for determining the $\log_2$ of exact powers of 2 comes
-- from GCC.  It requires bit manipulation primitives, and we use GHC
-- extensions.  Tough.
-- 
-- Used to be in MachInstrs --SDM.
-- ToDo: remove use of unboxery --SDM.

w2i x = word2Int# x
i2w x = int2Word# x

exactLog2 :: Integer -> Maybe Integer
exactLog2 x
  = if (x <= 0 || x >= 2147483648) then
       Nothing
    else
       case fromInteger x of { I# x# ->
       if (w2i ((i2w x#) `and#` (i2w (0# -# x#))) /=# x#) then
	  Nothing
       else
	  Just (toInteger (I# (pow2 x#)))
       }
  where
    pow2 x# | x# ==# 1# = 0#
            | otherwise = 1# +# pow2 (w2i (i2w x# `shiftRL#` 1#))


-- -----------------------------------------------------------------------------
-- widening / narrowing

narrowU :: MachRep -> Integer -> Integer
narrowU I8  x = fromIntegral (fromIntegral x :: Word8)
narrowU I16 x = fromIntegral (fromIntegral x :: Word16)
narrowU I32 x = fromIntegral (fromIntegral x :: Word32)
narrowU I64 x = fromIntegral (fromIntegral x :: Word64)
narrowU _ _ = panic "narrowTo"

narrowS :: MachRep -> Integer -> Integer
narrowS I8  x = fromIntegral (fromIntegral x :: Int8)
narrowS I16 x = fromIntegral (fromIntegral x :: Int16)
narrowS I32 x = fromIntegral (fromIntegral x :: Int32)
narrowS I64 x = fromIntegral (fromIntegral x :: Int64)
narrowS _ _ = panic "narrowTo"

-- -----------------------------------------------------------------------------
-- Loopify for C

{-
 This is a simple pass that replaces tail-recursive functions like this:

   fac() {
     ...
     jump fac();
   }

 with this:

  fac() {
   L:
     ...
     goto L;
  }

  the latter generates better C code, because the C compiler treats it
  like a loop, and brings full loop optimisation to bear.

  In my measurements this makes little or no difference to anything
  except factorial, but what the hell.
-}

cmmLoopifyForC :: RawCmmTop -> RawCmmTop
cmmLoopifyForC p@(CmmProc info entry_lbl [] (ListGraph blocks@(BasicBlock top_id _ : _)))
  | null info = p  -- only if there's an info table, ignore case alts
  | otherwise =  
--  pprTrace "jump_lbl" (ppr jump_lbl <+> ppr entry_lbl) $
  CmmProc info entry_lbl [] (ListGraph blocks')
  where blocks' = [ BasicBlock id (map do_stmt stmts)
		  | BasicBlock id stmts <- blocks ]

        do_stmt (CmmJump (CmmLit (CmmLabel lbl)) _) | lbl == jump_lbl
		= CmmBranch top_id
	do_stmt stmt = stmt

	jump_lbl | tablesNextToCode = entryLblToInfoLbl entry_lbl
		 | otherwise        = entry_lbl

cmmLoopifyForC top = top

-- -----------------------------------------------------------------------------
-- Utils

isLit (CmmLit _) = True
isLit _          = False

isComparisonExpr :: CmmExpr -> Bool
isComparisonExpr (CmmMachOp op _) = isComparisonMachOp op
isComparisonExpr _other 	    = False

maybeInvertConditionalExpr :: CmmExpr -> Maybe CmmExpr
maybeInvertConditionalExpr (CmmMachOp op args) 
  | Just op' <- maybeInvertComparison op = Just (CmmMachOp op' args)
maybeInvertConditionalExpr _ = Nothing

isPicReg (CmmReg (CmmGlobal PicBaseReg)) = True
isPicReg _ = False
