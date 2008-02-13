{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-----------------------------------------------------------------------------
--
-- Code generation for ticky-ticky profiling
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module CgTicky (
	emitTickyCounter,

	tickyDynAlloc,
	tickyAllocHeap,
	tickyAllocPrim,
	tickyAllocThunk,
	tickyAllocPAP,

	tickyPushUpdateFrame,
	tickyUpdateFrameOmitted,

	tickyEnterDynCon,
	tickyEnterStaticCon,
	tickyEnterViaNode,

	tickyEnterFun, 
	tickyEnterThunk,

	tickyUpdateBhCaf,
	tickyBlackHole,
	tickyUnboxedTupleReturn, tickyVectoredReturn,
	tickyReturnOldCon, tickyReturnNewCon,

	tickyKnownCallTooFewArgs, tickyKnownCallExact, tickyKnownCallExtraArgs,
	tickyUnknownCall, tickySlowCallPat,

       staticTickyHdr,
  ) where

#include "HsVersions.h"
#include "../includes/DerivedConstants.h"
	-- For REP_xxx constants, which are MachReps

import ClosureInfo
import CgUtils
import CgMonad
import SMRep

import Cmm
import MachOp
import CmmUtils
import CLabel

import Name
import Id
import StaticFlags
import BasicTypes
import FastString
import Constants
import Outputable

-- Turgid imports for showTypeCategory
import PrelNames
import TcType
import TyCon

import Data.Maybe

-----------------------------------------------------------------------------
--
--		Ticky-ticky profiling
--
-----------------------------------------------------------------------------

staticTickyHdr :: [CmmLit]
-- krc: not using this right now --
-- in the new version of ticky-ticky, we
-- don't change the closure layout.
-- leave it defined, though, to avoid breaking
-- other things.
staticTickyHdr = []

emitTickyCounter :: ClosureInfo -> [Id] -> Int -> Code
emitTickyCounter cl_info args on_stk
  = ifTicky $
    do	{ mod_name <- getModuleName
	; fun_descr_lit <- mkStringCLit (fun_descr mod_name)
	; arg_descr_lit <- mkStringCLit arg_descr
	; emitDataLits ticky_ctr_label 	-- Must match layout of StgEntCounter
-- krc: note that all the fields are I32 now; some were I16 before, 
-- but the code generator wasn't handling that properly and it led to chaos, 
-- panic and disorder.
	    [ mkIntCLit 0,
	      mkIntCLit (length args),-- Arity
	      mkIntCLit on_stk,	-- Words passed on stack
	      fun_descr_lit,
	      arg_descr_lit,
	      zeroCLit, 		-- Entry count
	      zeroCLit, 		-- Allocs
	      zeroCLit 			-- Link
	    ] }
  where
    name = closureName cl_info
    ticky_ctr_label = mkRednCountsLabel name
    arg_descr = map (showTypeCategory . idType) args
    fun_descr mod_name = ppr_for_ticky_name mod_name name

-- When printing the name of a thing in a ticky file, we want to
-- give the module name even for *local* things.   We print
-- just "x (M)" rather that "M.x" to distinguish them from the global kind.
ppr_for_ticky_name mod_name name
  | isInternalName name = showSDocDebug (ppr name <+> (parens (ppr mod_name)))
  | otherwise	        = showSDocDebug (ppr name)

-- -----------------------------------------------------------------------------
-- Ticky stack frames

tickyPushUpdateFrame    = ifTicky $ bumpTickyCounter SLIT("UPDF_PUSHED_ctr")
tickyUpdateFrameOmitted = ifTicky $ bumpTickyCounter SLIT("UPDF_OMITTED_ctr")

-- -----------------------------------------------------------------------------
-- Ticky entries

tickyEnterDynCon      = ifTicky $ bumpTickyCounter SLIT("ENT_DYN_CON_ctr")
tickyEnterDynThunk    = ifTicky $ bumpTickyCounter SLIT("ENT_DYN_THK_ctr")
tickyEnterStaticCon   = ifTicky $ bumpTickyCounter SLIT("ENT_STATIC_CON_ctr")
tickyEnterStaticThunk = ifTicky $ bumpTickyCounter SLIT("ENT_STATIC_THK_ctr")
tickyEnterViaNode     = ifTicky $ bumpTickyCounter SLIT("ENT_VIA_NODE_ctr")

tickyEnterThunk :: ClosureInfo -> Code
tickyEnterThunk cl_info
  | isStaticClosure cl_info = tickyEnterStaticThunk
  | otherwise		    = tickyEnterDynThunk

tickyBlackHole :: Bool{-updatable-} -> Code
tickyBlackHole updatable
  = ifTicky (bumpTickyCounter ctr)
  where
    ctr | updatable = SLIT("UPD_BH_SINGLE_ENTRY_ctr")
	| otherwise = SLIT("UPD_BH_UPDATABLE_ctr")

tickyUpdateBhCaf cl_info
  = ifTicky (bumpTickyCounter ctr)
  where
    ctr | closureUpdReqd cl_info = SLIT("UPD_CAF_BH_SINGLE_ENTRY_ctr")
	| otherwise	         = SLIT("UPD_CAF_BH_UPDATABLE_ctr")

tickyEnterFun :: ClosureInfo -> Code
tickyEnterFun cl_info
  = ifTicky $ 
    do 	{ bumpTickyCounter ctr
	; fun_ctr_lbl <- getTickyCtrLabel
	; registerTickyCtr fun_ctr_lbl
	; bumpTickyCounter' (cmmLabelOffB fun_ctr_lbl oFFSET_StgEntCounter_entry_count)
        }
  where
    ctr | isStaticClosure cl_info = SLIT("ENT_STATIC_FUN_DIRECT_ctr")
	| otherwise		  = SLIT("ENT_DYN_FUN_DIRECT_ctr")

registerTickyCtr :: CLabel -> Code
-- Register a ticky counter
--   if ( ! f_ct.registeredp ) {
--	    f_ct.link = ticky_entry_ctrs; 	/* hook this one onto the front of the list */
--	    ticky_entry_ctrs = & (f_ct);	/* mark it as "registered" */
--	    f_ct.registeredp = 1 }
registerTickyCtr ctr_lbl
  = emitIf test (stmtsC register_stmts)
  where
    -- krc: code generator doesn't handle Not, so we test for Eq 0 instead
    test = CmmMachOp (MO_Eq wordRep)
              [CmmLoad (CmmLit (cmmLabelOffB ctr_lbl 
				oFFSET_StgEntCounter_registeredp)) wordRep,
               CmmLit (mkIntCLit 0)]
    register_stmts
      =	[ CmmStore (CmmLit (cmmLabelOffB ctr_lbl oFFSET_StgEntCounter_link))
		   (CmmLoad ticky_entry_ctrs wordRep)
	, CmmStore ticky_entry_ctrs (mkLblExpr ctr_lbl)
	, CmmStore (CmmLit (cmmLabelOffB ctr_lbl 
				oFFSET_StgEntCounter_registeredp))
		   (CmmLit (mkIntCLit 1)) ]
    ticky_entry_ctrs = mkLblExpr (mkRtsDataLabel SLIT("ticky_entry_ctrs"))

tickyReturnOldCon, tickyReturnNewCon :: Arity -> Code
tickyReturnOldCon arity 
  = ifTicky $ do { bumpTickyCounter SLIT("RET_OLD_ctr")
	         ; bumpHistogram SLIT("RET_OLD_hst") arity }
tickyReturnNewCon arity 
  | not opt_DoTickyProfiling = nopC
  | otherwise
  = ifTicky $ do { bumpTickyCounter SLIT("RET_NEW_ctr")
	         ; bumpHistogram SLIT("RET_NEW_hst") arity }

tickyUnboxedTupleReturn :: Int -> Code
tickyUnboxedTupleReturn arity
  = ifTicky $ do { bumpTickyCounter SLIT("RET_UNBOXED_TUP_ctr")
 	         ; bumpHistogram SLIT("RET_UNBOXED_TUP_hst") arity }

tickyVectoredReturn :: Int -> Code
tickyVectoredReturn family_size 
  = ifTicky $ do { bumpTickyCounter SLIT("VEC_RETURN_ctr")
		 ; bumpHistogram SLIT("RET_VEC_RETURN_hst") family_size }

-- -----------------------------------------------------------------------------
-- Ticky calls

-- Ticks at a *call site*:
tickyKnownCallTooFewArgs = ifTicky $ bumpTickyCounter SLIT("KNOWN_CALL_TOO_FEW_ARGS_ctr")
tickyKnownCallExact = ifTicky $ bumpTickyCounter SLIT("KNOWN_CALL_ctr")
tickyKnownCallExtraArgs = ifTicky $ bumpTickyCounter SLIT("KNOWN_CALL_EXTRA_ARGS_ctr")
tickyUnknownCall = ifTicky $ bumpTickyCounter SLIT("UNKNOWN_CALL_ctr")

-- Tick for the call pattern at slow call site (i.e. in addition to
-- tickyUnknownCall, tickyKnownCallExtraArgs, etc.)
tickySlowCallPat :: [CgRep] -> Code
tickySlowCallPat args = return ()
{- LATER: (introduces recursive module dependency now).
  case callPattern args of
    (str, True)  -> bumpTickyCounter' (mkRtsSlowTickyCtrLabel pat)
    (str, False) -> bumpTickyCounter  SLIT("TICK_SLOW_CALL_OTHER")

callPattern :: [CgRep] -> (String,Bool)
callPattern reps 
  | match == length reps = (chars, True)
  | otherwise            = (chars, False)
  where (_,match) = findMatch reps
	chars     = map argChar reps

argChar VoidArg   = 'v'
argChar PtrArg    = 'p'
argChar NonPtrArg = 'n'
argChar LongArg   = 'l'
argChar FloatArg  = 'f'
argChar DoubleArg = 'd'
-}

-- -----------------------------------------------------------------------------
-- Ticky allocation

tickyDynAlloc :: ClosureInfo -> Code
-- Called when doing a dynamic heap allocation
tickyDynAlloc cl_info
  = ifTicky $
    case smRepClosureType (closureSMRep cl_info) of
	Just Constr           -> tick_alloc_con
	Just ConstrNoCaf      -> tick_alloc_con
	Just Fun	      -> tick_alloc_fun
	Just Thunk 	      -> tick_alloc_thk
	Just ThunkSelector    -> tick_alloc_thk
        -- black hole
        Nothing               -> return ()
  where
	-- will be needed when we fill in stubs
    cl_size   =	closureSize cl_info
    slop_size = slopSize cl_info

    tick_alloc_thk 
	| closureUpdReqd cl_info = tick_alloc_up_thk
	| otherwise	         = tick_alloc_se_thk

    -- krc: changed from panic to return () 
    -- just to get something working
    tick_alloc_con = return ()
    tick_alloc_fun = return ()
    tick_alloc_up_thk = return ()
    tick_alloc_se_thk = return ()


tickyAllocPrim :: CmmExpr -> CmmExpr -> CmmExpr -> Code
tickyAllocPrim hdr goods slop = ifTicky $ pprTrace "ToDo: tickyAllocPrim" empty (return ())

tickyAllocThunk :: CmmExpr -> CmmExpr -> Code
tickyAllocThunk goods slop = ifTicky $ pprTrace "ToDo: tickyAllocThunk" empty (return ())

tickyAllocPAP :: CmmExpr -> CmmExpr -> Code
tickyAllocPAP goods slop = ifTicky $ pprTrace "ToDo: tickyAllocPAP" empty (return ())

tickyAllocHeap :: VirtualHpOffset -> Code
-- Called when doing a heap check [TICK_ALLOC_HEAP]
tickyAllocHeap hp
  = ifTicky $
    do	{ ticky_ctr <- getTickyCtrLabel
	; stmtsC $
	  if hp == 0 then [] 	-- Inside the stmtC to avoid control
	  else [		-- dependency on the argument
		-- Bump the allcoation count in the StgEntCounter
	    addToMem REP_StgEntCounter_allocs 
			(CmmLit (cmmLabelOffB ticky_ctr 
				oFFSET_StgEntCounter_allocs)) hp,
		-- Bump ALLOC_HEAP_ctr
	    addToMemLbl cLongRep (mkRtsDataLabel SLIT("ALLOC_HEAP_ctr")) 1,
		-- Bump ALLOC_HEAP_tot
	    addToMemLbl cLongRep (mkRtsDataLabel SLIT("ALLOC_HEAP_tot")) hp] }

-- -----------------------------------------------------------------------------
-- Ticky utils

ifTicky :: Code -> Code
ifTicky code
  | opt_DoTickyProfiling = code
  | otherwise		 = nopC

addToMemLbl :: MachRep -> CLabel -> Int -> CmmStmt
addToMemLbl rep lbl n = addToMem rep (CmmLit (CmmLabel lbl)) n

-- All the ticky-ticky counters are declared "unsigned long" in C
bumpTickyCounter :: LitString -> Code
bumpTickyCounter lbl = bumpTickyCounter' (cmmLabelOffB (mkRtsDataLabel lbl) 0)

bumpTickyCounter' :: CmmLit -> Code
-- krc: note that we're incrementing the _entry_count_ field of the ticky counter
bumpTickyCounter' lhs = stmtC (addToMem cLongRep (CmmLit lhs) 1)

addToMemLong = addToMem cLongRep

bumpHistogram :: LitString -> Int -> Code
bumpHistogram lbl n 
--  = bumpHistogramE lbl (CmmLit (CmmInt (fromIntegral n) cLongRep))
    = return ()	   -- TEMP SPJ Apr 07

bumpHistogramE :: LitString -> CmmExpr -> Code
bumpHistogramE lbl n 
  = do  t <- newNonPtrTemp cLongRep
	stmtC (CmmAssign (CmmLocal t) n)
	emitIf (CmmMachOp (MO_U_Le cLongRep) [CmmReg (CmmLocal t), eight]) $
		stmtC (CmmAssign (CmmLocal t) eight)
	stmtC (addToMemLong (cmmIndexExpr cLongRep 
				(CmmLit (CmmLabel (mkRtsDataLabel lbl)))
				(CmmReg (CmmLocal t)))
			    1)
  where 
   eight = CmmLit (CmmInt 8 cLongRep)

------------------------------------------------------------------
-- Showing the "type category" for ticky-ticky profiling

showTypeCategory :: Type -> Char
  {-	{C,I,F,D}   char, int, float, double
	T	    tuple
	S	    other single-constructor type
	{c,i,f,d}   unboxed ditto
	t	    *unpacked* tuple
	s	    *unpacked" single-cons...

	v	    void#
	a	    primitive array

	E	    enumeration type
	+	    dictionary, unless it's a ...
	L	    List
	>	    function
	M	    other (multi-constructor) data-con type
	.	    other type
	-	    reserved for others to mark as "uninteresting"
    -}
showTypeCategory ty
  = if isDictTy ty
    then '+'
    else
      case tcSplitTyConApp_maybe ty of
	Nothing -> if isJust (tcSplitFunTy_maybe ty)
		   then '>'
		   else '.'

	Just (tycon, _) ->
          let utc = getUnique tycon in
	  if	  utc == charDataConKey    then 'C'
	  else if utc == intDataConKey     then 'I'
	  else if utc == floatDataConKey   then 'F'
	  else if utc == doubleDataConKey  then 'D'
	  else if utc == smallIntegerDataConKey ||
		  utc == largeIntegerDataConKey   then 'J'
	  else if utc == charPrimTyConKey  then 'c'
	  else if (utc == intPrimTyConKey || utc == wordPrimTyConKey
		|| utc == addrPrimTyConKey)		   then 'i'
	  else if utc  == floatPrimTyConKey		   then 'f'
	  else if utc  == doublePrimTyConKey		   then 'd'
	  else if isPrimTyCon tycon {- array, we hope -}   then 'A'	-- Bogus
	  else if isEnumerationTyCon tycon		   then 'E'
	  else if isTupleTyCon tycon			   then 'T'
	  else if isJust (maybeTyConSingleCon tycon)       then 'S'
	  else if utc == listTyConKey			   then 'L'
	  else 'M' -- oh, well...
