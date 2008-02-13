%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcExpr]{Typecheck an expression}

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module TcExpr ( tcPolyExpr, tcPolyExprNC, 
		tcMonoExpr, tcInferRho, tcSyntaxOp ) where

#include "HsVersions.h"

#ifdef GHCI 	/* Only if bootstrapped */
import {-# SOURCE #-}	TcSplice( tcSpliceExpr, tcBracket )
import qualified DsMeta
#endif

import HsSyn
import TcHsSyn
import TcRnMonad
import TcUnify
import BasicTypes
import Inst
import TcBinds
import TcEnv
import TcArrows
import TcMatches
import TcHsType
import TcPat
import TcMType
import TcType
import TcIface	( checkWiredInTyCon )
import Id
import DataCon
import Name
import TyCon
import Type
import TypeRep
import Coercion
import Var
import VarSet
import TysWiredIn
import PrelNames
import PrimOp
import DynFlags
import StaticFlags
import HscTypes
import SrcLoc
import Util
import ListSetOps
import Maybes
import Outputable
import FastString
\end{code}

%************************************************************************
%*									*
\subsection{Main wrappers}
%*									*
%************************************************************************

\begin{code}
tcPolyExpr, tcPolyExprNC
	 :: LHsExpr Name		-- Expession to type check
       	 -> BoxySigmaType		-- Expected type (could be a polytpye)
       	 -> TcM (LHsExpr TcId)	-- Generalised expr with expected type

-- tcPolyExpr is a convenient place (frequent but not too frequent) place
-- to add context information.
-- The NC version does not do so, usually because the caller wants
-- to do so himself.

tcPolyExpr expr res_ty 	
  = addErrCtxt (exprCtxt (unLoc expr)) $
    (do {traceTc (text "tcPolyExpr") ; tcPolyExprNC expr res_ty })

tcPolyExprNC expr res_ty 
  | isSigmaTy res_ty
  = do	{ traceTc (text "tcPolyExprNC" <+> ppr res_ty)
	; (gen_fn, expr') <- tcGen res_ty emptyVarSet (\_ -> tcPolyExprNC expr)
		-- Note the recursive call to tcPolyExpr, because the
		-- type may have multiple layers of for-alls
		-- E.g. forall a. Eq a => forall b. Ord b => ....
	; return (mkLHsWrap gen_fn expr') }

  | otherwise
  = tcMonoExpr expr res_ty

---------------
tcPolyExprs :: [LHsExpr Name] -> [TcType] -> TcM [LHsExpr TcId]
tcPolyExprs [] [] = returnM []
tcPolyExprs (expr:exprs) (ty:tys)
 = do 	{ expr'  <- tcPolyExpr  expr  ty
	; exprs' <- tcPolyExprs exprs tys
	; returnM (expr':exprs') }
tcPolyExprs exprs tys = pprPanic "tcPolyExprs" (ppr exprs $$ ppr tys)

---------------
tcMonoExpr :: LHsExpr Name	-- Expression to type check
	   -> BoxyRhoType 	-- Expected type (could be a type variable)
				-- Definitely no foralls at the top
				-- Can contain boxes, which will be filled in
	   -> TcM (LHsExpr TcId)

tcMonoExpr (L loc expr) res_ty
  = ASSERT( not (isSigmaTy res_ty) )
    setSrcSpan loc $
    do	{ expr' <- tcExpr expr res_ty
	; return (L loc expr') }

---------------
tcInferRho :: LHsExpr Name -> TcM (LHsExpr TcId, TcRhoType)
tcInferRho expr	= tcInfer (tcMonoExpr expr)
\end{code}


%************************************************************************
%*									*
	tcExpr: the main expression typechecker
%*									*
%************************************************************************

\begin{code}
tcExpr :: HsExpr Name -> BoxyRhoType -> TcM (HsExpr TcId)
tcExpr (HsVar name)     res_ty = tcId (OccurrenceOf name) name res_ty

tcExpr (HsLit lit)	res_ty = do { let lit_ty = hsLitType lit
				    ; coi <- boxyUnify lit_ty res_ty
				    ; return $ mkHsWrapCoI coi (HsLit lit)
				    }

tcExpr (HsPar expr)     res_ty = do { expr' <- tcMonoExpr expr res_ty
				    ; return (HsPar expr') }

tcExpr (HsSCC lbl expr) res_ty = do { expr' <- tcMonoExpr expr res_ty
				    ; returnM (HsSCC lbl expr') }
tcExpr (HsTickPragma info expr) res_ty 
       		     	       = do { expr' <- tcMonoExpr expr res_ty
				    ; returnM (HsTickPragma info expr') }

tcExpr (HsCoreAnn lbl expr) res_ty 	 -- hdaume: core annotation
  = do	{ expr' <- tcMonoExpr expr res_ty
	; return (HsCoreAnn lbl expr') }

tcExpr (HsOverLit lit) res_ty  
  = do 	{ lit' <- tcOverloadedLit (LiteralOrigin lit) lit res_ty
	; return (HsOverLit lit') }

tcExpr (NegApp expr neg_expr) res_ty
  = do	{ neg_expr' <- tcSyntaxOp (OccurrenceOf negateName) neg_expr
				  (mkFunTy res_ty res_ty)
	; expr' <- tcMonoExpr expr res_ty
	; return (NegApp expr' neg_expr') }

tcExpr (HsIPVar ip) res_ty
  = do	{ 	-- Implicit parameters must have a *tau-type* not a 
		-- type scheme.  We enforce this by creating a fresh
		-- type variable as its type.  (Because res_ty may not
		-- be a tau-type.)
	  ip_ty <- newFlexiTyVarTy argTypeKind	-- argTypeKind: it can't be an unboxed tuple
	; co_fn <- tcSubExp ip_ty res_ty
	; (ip', inst) <- newIPDict (IPOccOrigin ip) ip ip_ty
	; extendLIE inst
	; return (mkHsWrap co_fn (HsIPVar ip')) }

tcExpr (HsApp e1 e2) res_ty 
  = go e1 [e2]
  where
    go :: LHsExpr Name -> [LHsExpr Name] -> TcM (HsExpr TcId)
    go (L _ (HsApp e1 e2)) args = go e1 (e2:args)
    go lfun@(L loc fun) args
	= do { (fun', args') <- -- addErrCtxt (callCtxt lfun args) $
				tcApp fun (length args) (tcArgs lfun args) res_ty
	     ; traceTc (text "tcExpr args': " <+> ppr args')
	     ; return (unLoc (foldl mkHsApp (L loc fun') args')) }

tcExpr (HsLam match) res_ty
  = do	{ (co_fn, match') <- tcMatchLambda match res_ty
	; return (mkHsWrap co_fn (HsLam match')) }

tcExpr in_expr@(ExprWithTySig expr sig_ty) res_ty
 = do	{ sig_tc_ty <- tcHsSigType ExprSigCtxt sig_ty

	-- Remember to extend the lexical type-variable environment
	; (gen_fn, expr') <- tcGen sig_tc_ty emptyVarSet (\ skol_tvs res_ty ->
		             tcExtendTyVarEnv2 (hsExplicitTvs sig_ty `zip` mkTyVarTys skol_tvs) $
			     tcPolyExprNC expr res_ty)

	; co_fn <- tcSubExp sig_tc_ty res_ty
	; return (mkHsWrap co_fn (ExprWithTySigOut (mkLHsWrap gen_fn expr') sig_ty)) }

tcExpr (HsType ty) res_ty
  = failWithTc (text "Can't handle type argument:" <+> ppr ty)
	-- This is the syntax for type applications that I was planning
	-- but there are difficulties (e.g. what order for type args)
	-- so it's not enabled yet.
	-- Can't eliminate it altogether from the parser, because the
	-- same parser parses *patterns*.
\end{code}


%************************************************************************
%*									*
		Infix operators and sections
%*									*
%************************************************************************

\begin{code}
tcExpr in_expr@(OpApp arg1 lop@(L loc op) fix arg2) res_ty
  = do	{ (op', [arg1', arg2']) <- tcApp op 2 (tcArgs lop [arg1,arg2]) res_ty
	; return (OpApp arg1' (L loc op') fix arg2') }

-- Left sections, equivalent to
--	\ x -> e op x,
-- or
--	\ x -> op e x,
-- or just
-- 	op e
--
-- We treat it as similar to the latter, so we don't
-- actually require the function to take two arguments
-- at all.  For example, (x `not`) means (not x);
-- you get postfix operators!  Not really Haskell 98
-- I suppose, but it's less work and kind of useful.

tcExpr in_expr@(SectionL arg1 lop@(L loc op)) res_ty
  = do 	{ (op', [arg1']) <- tcApp op 1 (tcArgs lop [arg1]) res_ty
	; return (SectionL arg1' (L loc op')) }

-- Right sections, equivalent to \ x -> x `op` expr, or
--	\ x -> op x expr
 
tcExpr in_expr@(SectionR lop@(L loc op) arg2) res_ty
  = do	{ (co_fn, (op', arg2')) <- subFunTys doc 1 res_ty $ \ [arg1_ty'] res_ty' ->
				   tcApp op 2 (tc_args arg1_ty') res_ty'
	; return (mkHsWrap co_fn (SectionR (L loc op') arg2')) }
  where
    doc = ptext SLIT("The section") <+> quotes (ppr in_expr)
		<+> ptext SLIT("takes one argument")
    tc_args arg1_ty' qtvs qtys [arg1_ty, arg2_ty] 
	= do { boxyUnify arg1_ty' (substTyWith qtvs qtys arg1_ty)
	     ; arg2' <- tcArg lop 2 arg2 qtvs qtys arg2_ty 
	     ; qtys' <- mapM refineBox qtys	-- c.f. tcArgs 
	     ; return (qtys', arg2') }
    tc_args arg1_ty' _ _ _ = panic "tcExpr SectionR"
\end{code}

\begin{code}
tcExpr (HsLet binds expr) res_ty
  = do	{ (binds', expr') <- tcLocalBinds binds $
			     tcMonoExpr expr res_ty   
	; return (HsLet binds' expr') }

tcExpr (HsCase scrut matches) exp_ty
  = do	{  -- We used to typecheck the case alternatives first.
	   -- The case patterns tend to give good type info to use
	   -- when typechecking the scrutinee.  For example
	   --	case (map f) of
	   --	  (x:xs) -> ...
	   -- will report that map is applied to too few arguments
	   --
	   -- But now, in the GADT world, we need to typecheck the scrutinee
	   -- first, to get type info that may be refined in the case alternatives
	  (scrut', scrut_ty) <- addErrCtxt (caseScrutCtxt scrut)
				 	   (tcInferRho scrut)

	; traceTc (text "HsCase" <+> ppr scrut_ty)
	; matches' <- tcMatchesCase match_ctxt scrut_ty matches exp_ty
	; return (HsCase scrut' matches') }
 where
    match_ctxt = MC { mc_what = CaseAlt,
		      mc_body = tcBody }

tcExpr (HsIf pred b1 b2) res_ty
  = do	{ pred' <- addErrCtxt (predCtxt pred) $
		   tcMonoExpr pred boolTy
	; b1' <- tcMonoExpr b1 res_ty
	; b2' <- tcMonoExpr b2 res_ty
	; return (HsIf pred' b1' b2') }

tcExpr (HsDo do_or_lc stmts body _) res_ty
  = tcDoStmts do_or_lc stmts body res_ty

tcExpr in_expr@(ExplicitList _ exprs) res_ty	-- Non-empty list
  = do 	{ (elt_ty, coi) <- boxySplitListTy res_ty
	; exprs' <- mappM (tc_elt elt_ty) exprs
	; return $ mkHsWrapCoI coi (ExplicitList elt_ty exprs') }
  where
    tc_elt elt_ty expr = tcPolyExpr expr elt_ty

tcExpr in_expr@(ExplicitPArr _ exprs) res_ty	-- maybe empty
  = do	{ (elt_ty, coi) <- boxySplitPArrTy res_ty
    	; exprs' <- mappM (tc_elt elt_ty) exprs	
	; ifM (null exprs) (zapToMonotype elt_ty)
		-- If there are no expressions in the comprehension
		-- we must still fill in the box
		-- (Not needed for [] and () becuase they happen
		--  to parse as data constructors.)
	; return $ mkHsWrapCoI coi (ExplicitPArr elt_ty exprs') }
  where
    tc_elt elt_ty expr = tcPolyExpr expr elt_ty

-- For tuples, take care to preserve rigidity
-- E.g. 	case (x,y) of ....
-- 	   The scrutinee should have a rigid type if x,y do
-- The general scheme is the same as in tcIdApp
tcExpr (ExplicitTuple exprs boxity) res_ty
  = do	{ tvs <- newBoxyTyVars [argTypeKind | e <- exprs]
	; let tup_tc     = tupleTyCon boxity (length exprs)
	      tup_res_ty = mkTyConApp tup_tc (mkTyVarTys tvs)
	; checkWiredInTyCon tup_tc	-- Ensure instances are available
	; arg_tys  <- preSubType tvs (mkVarSet tvs) tup_res_ty res_ty
	; exprs'   <- tcPolyExprs exprs arg_tys
	; arg_tys' <- mapM refineBox arg_tys
	; co_fn    <- tcFunResTy (tyConName tup_tc) (mkTyConApp tup_tc arg_tys') res_ty
	; return (mkHsWrap co_fn (ExplicitTuple exprs' boxity)) }

tcExpr (HsProc pat cmd) res_ty
  = do	{ (pat', cmd', coi) <- tcProc pat cmd res_ty
	; return $ mkHsWrapCoI coi (HsProc pat' cmd') }

tcExpr e@(HsArrApp _ _ _ _ _) _
  = failWithTc (vcat [ptext SLIT("The arrow command"), nest 2 (ppr e), 
                      ptext SLIT("was found where an expression was expected")])

tcExpr e@(HsArrForm _ _ _) _
  = failWithTc (vcat [ptext SLIT("The arrow command"), nest 2 (ppr e), 
                      ptext SLIT("was found where an expression was expected")])
\end{code}

%************************************************************************
%*									*
		Record construction and update
%*									*
%************************************************************************

\begin{code}
tcExpr expr@(RecordCon (L loc con_name) _ rbinds) res_ty
  = do	{ data_con <- tcLookupDataCon con_name

 	-- Check for missing fields
	; checkMissingFields data_con rbinds

	; let arity = dataConSourceArity data_con
	      check_fields qtvs qtys arg_tys 
		  = do	{ let arg_tys' = substTys (zipOpenTvSubst qtvs qtys) arg_tys
			; rbinds' <- tcRecordBinds data_con arg_tys' rbinds
		 	; qtys' <- mapM refineBoxToTau qtys
			; return (qtys', rbinds') }
		-- The refineBoxToTau ensures that all the boxes in arg_tys are indeed
		-- filled, which is the invariant expected by tcIdApp
		-- How could this not be the case?  Consider a record construction
		-- that does not mention all the fields.

	; (con_expr, rbinds') <- tcIdApp con_name arity check_fields res_ty

	; returnM (RecordCon (L loc (dataConWrapId data_con)) con_expr rbinds') }

-- The main complication with RecordUpd is that we need to explicitly
-- handle the *non-updated* fields.  Consider:
--
--	data T a b = MkT1 { fa :: a, fb :: b }
--		   | MkT2 { fa :: a, fc :: Int -> Int }
--		   | MkT3 { fd :: a }
--	
--	upd :: T a b -> c -> T a c
--	upd t x = t { fb = x}
--
-- The type signature on upd is correct (i.e. the result should not be (T a b))
-- because upd should be equivalent to:
--
--	upd t x = case t of 
--			MkT1 p q -> MkT1 p x
--			MkT2 a b -> MkT2 p b
--			MkT3 d   -> error ...
--
-- So we need to give a completely fresh type to the result record,
-- and then constrain it by the fields that are *not* updated ("p" above).
--
-- Note that because MkT3 doesn't contain all the fields being updated,
-- its RHS is simply an error, so it doesn't impose any type constraints
--
-- All this is done in STEP 4 below.
--
-- Note about GADTs
-- ~~~~~~~~~~~~~~~~
-- For record update we require that every constructor involved in the
-- update (i.e. that has all the specified fields) is "vanilla".  I
-- don't know how to do the update otherwise.


tcExpr expr@(RecordUpd record_expr rbinds _ _ _) res_ty
  = 	-- STEP 0
	-- Check that the field names are really field names
    let 
	field_names = hsRecFields rbinds
    in
    ASSERT( notNull field_names )
    mappM tcLookupField field_names	`thenM` \ sel_ids ->
	-- The renamer has already checked that they
	-- are all in scope
    let
	bad_guys = [ setSrcSpan loc $ addErrTc (notSelector field_name) 
		   | (fld, sel_id) <- rec_flds rbinds `zip` sel_ids,
		     not (isRecordSelector sel_id), 	-- Excludes class ops
		     let L loc field_name = hsRecFieldId fld
		   ]
    in
    checkM (null bad_guys) (sequenceM bad_guys `thenM_` failM)	`thenM_`
    
	-- STEP 1
	-- Figure out the tycon and data cons from the first field name
    let
		-- It's OK to use the non-tc splitters here (for a selector)
	sel_id : _   	= sel_ids
	(tycon, _)   	= recordSelectorFieldLabel sel_id	-- We've failed already if
	data_cons    	= tyConDataCons tycon			-- it's not a field label
		-- NB: for a data type family, the tycon is the instance tycon

	relevant_cons   = filter is_relevant data_cons
	is_relevant con = all (`elem` dataConFieldLabels con) field_names
    in

	-- STEP 2
	-- Check that at least one constructor has all the named fields
	-- i.e. has an empty set of bad fields returned by badFields
    checkTc (not (null relevant_cons))
	    (badFieldsUpd rbinds)	`thenM_`

	-- Check that all relevant data cons are vanilla.  Doing record updates on 
	-- GADTs and/or existentials is more than my tiny brain can cope with today
    checkTc (all isVanillaDataCon relevant_cons)
	    (nonVanillaUpd tycon)	`thenM_`

	-- STEP 4
	-- Use the un-updated fields to find a vector of booleans saying
	-- which type arguments must be the same in updatee and result.
	--
	-- WARNING: this code assumes that all data_cons in a common tycon
	-- have FieldLabels abstracted over the same tyvars.
    let
		-- A constructor is only relevant to this process if
		-- it contains *all* the fields that are being updated
	con1 = ASSERT( not (null relevant_cons) ) head relevant_cons	-- A representative constructor
	(con1_tyvars, theta, con1_arg_tys, con1_res_ty) = dataConSig con1
	con1_flds     = dataConFieldLabels con1
	common_tyvars = exactTyVarsOfTypes [ty | (fld,ty) <- con1_flds `zip` con1_arg_tys
					       , not (fld `elem` field_names) ]

 	is_common_tv tv = tv `elemVarSet` common_tyvars

	mk_inst_ty tv result_inst_ty 
	  | is_common_tv tv = returnM result_inst_ty		-- Same as result type
	  | otherwise	    = newFlexiTyVarTy (tyVarKind tv)	-- Fresh type, of correct kind
    in
    ASSERT( null theta )	-- Vanilla datacon
    tcInstTyVars con1_tyvars				`thenM` \ (_, result_inst_tys, result_inst_env) ->
    zipWithM mk_inst_ty con1_tyvars result_inst_tys	`thenM` \ scrut_inst_tys ->

	-- STEP 3: Typecheck the update bindings.
	-- Do this after checking for bad fields in case 
	-- there's a field that doesn't match the constructor.
    let
	result_ty     = substTy result_inst_env con1_res_ty
	con1_arg_tys' = map (substTy result_inst_env) con1_arg_tys
    in
    tcSubExp result_ty res_ty			`thenM` \ co_fn ->
    tcRecordBinds con1 con1_arg_tys' rbinds	`thenM` \ rbinds' ->

	-- STEP 5: Typecheck the expression to be updated
    let
	scrut_inst_env = zipTopTvSubst con1_tyvars scrut_inst_tys
	scrut_ty = substTy scrut_inst_env con1_res_ty
	-- This is one place where the isVanilla check is important
	-- So that inst_tys matches the con1_tyvars
    in
    tcMonoExpr record_expr scrut_ty		`thenM` \ record_expr' ->

	-- STEP 6: Figure out the LIE we need.  
	-- We have to generate some dictionaries for the data type context, 
	-- since we are going to do pattern matching over the data cons.
	--
	-- What dictionaries do we need?  The dataConStupidTheta tells us.
    let
	theta' = substTheta scrut_inst_env (dataConStupidTheta con1)
    in
    instStupidTheta RecordUpdOrigin theta'	`thenM_`

	-- Step 7: make a cast for the scrutinee, in the case that it's from a type family
    let scrut_co | Just co_con <- tyConFamilyCoercion_maybe tycon 
		 = WpCo $ mkTyConApp co_con scrut_inst_tys
		 | otherwise
		 = idHsWrapper
    in
	-- Phew!
    returnM (mkHsWrap co_fn (RecordUpd (mkLHsWrap scrut_co record_expr') rbinds'
				       relevant_cons scrut_inst_tys result_inst_tys))
\end{code}


%************************************************************************
%*									*
	Arithmetic sequences			e.g. [a,b..]
	and their parallel-array counterparts	e.g. [: a,b.. :]
		
%*									*
%************************************************************************

\begin{code}
tcExpr (ArithSeq _ seq@(From expr)) res_ty
  = do	{ (elt_ty, coi) <- boxySplitListTy res_ty
	; expr' <- tcPolyExpr expr elt_ty
	; enum_from <- newMethodFromName (ArithSeqOrigin seq) 
			      elt_ty enumFromName
	; return $ mkHsWrapCoI coi (ArithSeq (HsVar enum_from) (From expr')) }

tcExpr in_expr@(ArithSeq _ seq@(FromThen expr1 expr2)) res_ty
  = do	{ (elt_ty, coi) <- boxySplitListTy res_ty
	; expr1' <- tcPolyExpr expr1 elt_ty
	; expr2' <- tcPolyExpr expr2 elt_ty
	; enum_from_then <- newMethodFromName (ArithSeqOrigin seq) 
			      elt_ty enumFromThenName
	; return $ mkHsWrapCoI coi 
                    (ArithSeq (HsVar enum_from_then) (FromThen expr1' expr2')) }

tcExpr in_expr@(ArithSeq _ seq@(FromTo expr1 expr2)) res_ty
  = do	{ (elt_ty, coi) <- boxySplitListTy res_ty
	; expr1' <- tcPolyExpr expr1 elt_ty
	; expr2' <- tcPolyExpr expr2 elt_ty
	; enum_from_to <- newMethodFromName (ArithSeqOrigin seq) 
		  	      elt_ty enumFromToName
	; return $ mkHsWrapCoI coi 
                     (ArithSeq (HsVar enum_from_to) (FromTo expr1' expr2')) }

tcExpr in_expr@(ArithSeq _ seq@(FromThenTo expr1 expr2 expr3)) res_ty
  = do	{ (elt_ty, coi) <- boxySplitListTy res_ty
	; expr1' <- tcPolyExpr expr1 elt_ty
	; expr2' <- tcPolyExpr expr2 elt_ty
	; expr3' <- tcPolyExpr expr3 elt_ty
	; eft <- newMethodFromName (ArithSeqOrigin seq) 
		      elt_ty enumFromThenToName
	; return $ mkHsWrapCoI coi 
                     (ArithSeq (HsVar eft) (FromThenTo expr1' expr2' expr3')) }

tcExpr in_expr@(PArrSeq _ seq@(FromTo expr1 expr2)) res_ty
  = do	{ (elt_ty, coi) <- boxySplitPArrTy res_ty
	; expr1' <- tcPolyExpr expr1 elt_ty
	; expr2' <- tcPolyExpr expr2 elt_ty
	; enum_from_to <- newMethodFromName (PArrSeqOrigin seq) 
				      elt_ty enumFromToPName
	; return $ mkHsWrapCoI coi 
                     (PArrSeq (HsVar enum_from_to) (FromTo expr1' expr2')) }

tcExpr in_expr@(PArrSeq _ seq@(FromThenTo expr1 expr2 expr3)) res_ty
  = do	{ (elt_ty, coi) <- boxySplitPArrTy res_ty
	; expr1' <- tcPolyExpr expr1 elt_ty
	; expr2' <- tcPolyExpr expr2 elt_ty
	; expr3' <- tcPolyExpr expr3 elt_ty
	; eft <- newMethodFromName (PArrSeqOrigin seq)
		      elt_ty enumFromThenToPName
	; return $ mkHsWrapCoI coi 
                     (PArrSeq (HsVar eft) (FromThenTo expr1' expr2' expr3')) }

tcExpr (PArrSeq _ _) _ 
  = panic "TcExpr.tcMonoExpr: Infinite parallel array!"
    -- the parser shouldn't have generated it and the renamer shouldn't have
    -- let it through
\end{code}


%************************************************************************
%*									*
		Template Haskell
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI	/* Only if bootstrapped */
	-- Rename excludes these cases otherwise
tcExpr (HsSpliceE splice) res_ty = tcSpliceExpr splice res_ty
tcExpr (HsBracket brack)  res_ty = do	{ e <- tcBracket brack res_ty
					; return (unLoc e) }
#endif /* GHCI */
\end{code}


%************************************************************************
%*									*
		Catch-all
%*									*
%************************************************************************

\begin{code}
tcExpr other _ = pprPanic "tcMonoExpr" (ppr other)
\end{code}


%************************************************************************
%*									*
		Applications
%*									*
%************************************************************************

\begin{code}
---------------------------
tcApp :: HsExpr Name				-- Function
      -> Arity					-- Number of args reqd
      -> ArgChecker results
      -> BoxyRhoType				-- Result type
      -> TcM (HsExpr TcId, results)		

-- (tcFun fun n_args arg_checker res_ty)
-- The argument type checker, arg_checker, will be passed exactly n_args types

tcApp (HsVar fun_name) n_args arg_checker res_ty
  = tcIdApp fun_name n_args arg_checker res_ty

tcApp fun n_args arg_checker res_ty	-- The vanilla case (rula APP)
  = do	{ arg_boxes  <- newBoxyTyVars (replicate n_args argTypeKind)
	; fun'       <- tcExpr fun (mkFunTys (mkTyVarTys arg_boxes) res_ty)
	; arg_tys'   <- mapM readFilledBox arg_boxes
	; (_, args') <- arg_checker [] [] arg_tys'	-- Yuk
	; return (fun', args') }

---------------------------
tcIdApp :: Name					-- Function
        -> Arity				-- Number of args reqd
        -> ArgChecker results	-- The arg-checker guarantees to fill all boxes in the arg types
        -> BoxyRhoType				-- Result type
        -> TcM (HsExpr TcId, results)		

-- Call 	(f e1 ... en) :: res_ty
-- Type		f :: forall a b c. theta => fa_1 -> ... -> fa_k -> fres
-- 			(where k <= n; fres has the rest)
-- NB:	if k < n then the function doesn't have enough args, and
--	presumably fres is a type variable that we are going to 
--	instantiate with a function type
--
-- Then		fres <= bx_(k+1) -> ... -> bx_n -> res_ty

tcIdApp fun_name n_args arg_checker res_ty
  = do	{ let orig = OccurrenceOf fun_name
	; (fun, fun_ty) <- lookupFun orig fun_name

	-- Split up the function type
	; let (tv_theta_prs, rho) = tcMultiSplitSigmaTy fun_ty
	      (fun_arg_tys, fun_res_ty) = tcSplitFunTysN rho n_args

	      qtvs = concatMap fst tv_theta_prs		-- Quantified tyvars
	      arg_qtvs = exactTyVarsOfTypes fun_arg_tys
	      res_qtvs = exactTyVarsOfType fun_res_ty
		-- NB: exactTyVarsOfType.  See Note [Silly type synonyms in smart-app]
	      tau_qtvs = arg_qtvs `unionVarSet` res_qtvs
	      k  	     = length fun_arg_tys	-- k <= n_args
	      n_missing_args = n_args - k		-- Always >= 0

	-- Match the result type of the function with the
	-- result type of the context, to get an inital substitution
	; extra_arg_boxes <- newBoxyTyVars (replicate n_missing_args argTypeKind)
	; let extra_arg_tys' = mkTyVarTys extra_arg_boxes
	      res_ty' 	     = mkFunTys extra_arg_tys' res_ty
	; qtys' <- preSubType qtvs tau_qtvs fun_res_ty res_ty'

	-- Typecheck the arguments!
	-- Doing so will fill arg_qtvs and extra_arg_tys'
	; (qtys'', args') <- arg_checker qtvs qtys' (fun_arg_tys ++ extra_arg_tys')

	-- Strip boxes from the qtvs that have been filled in by the arg checking
	; extra_arg_tys'' <- mapM readFilledBox extra_arg_boxes

	-- Result subsumption
	-- This fills in res_qtvs
	; let res_subst = zipOpenTvSubst qtvs qtys''
	      fun_res_ty'' = substTy res_subst fun_res_ty
	      res_ty'' = mkFunTys extra_arg_tys'' res_ty
	; co_fn <- tcFunResTy fun_name fun_res_ty'' res_ty''
			    
	-- And pack up the results
	-- By applying the coercion just to the *function* we can make
	-- tcFun work nicely for OpApp and Sections too
	; fun' <- instFun orig fun res_subst tv_theta_prs
	; co_fn' <- wrapFunResCoercion (substTys res_subst fun_arg_tys) co_fn
	; traceTc (text "tcIdApp: " <+> ppr (mkHsWrap co_fn' fun') <+> ppr tv_theta_prs <+> ppr co_fn' <+> ppr fun')
	; return (mkHsWrap co_fn' fun', args') }
\end{code}

Note [Silly type synonyms in smart-app]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we call sripBoxyType, all of the boxes should be filled
in.  But we need to be careful about type synonyms:
	type T a = Int
	f :: T a -> Int
	...(f x)...
In the call (f x) we'll typecheck x, expecting it to have type
(T box).  Usually that would fill in the box, but in this case not;
because 'a' is discarded by the silly type synonym T.  So we must
use exactTyVarsOfType to figure out which type variables are free 
in the argument type.

\begin{code}
-- tcId is a specialisation of tcIdApp when there are no arguments
-- tcId f ty = do { (res, _) <- tcIdApp f [] (\[] -> return ()) ty
--		  ; return res }

tcId :: InstOrigin
     -> Name					-- Function
     -> BoxyRhoType				-- Result type
     -> TcM (HsExpr TcId)
tcId orig fun_name res_ty
  = do	{ traceTc (text "tcId" <+> ppr fun_name <+> ppr res_ty)
	; (fun, fun_ty) <- lookupFun orig fun_name

	-- Split up the function type
	; let (tv_theta_prs, fun_tau) = tcMultiSplitSigmaTy fun_ty
	      qtvs = concatMap fst tv_theta_prs	-- Quantified tyvars
	      tau_qtvs = exactTyVarsOfType fun_tau	-- Mentioned in the tau part
	; qtv_tys <- preSubType qtvs tau_qtvs fun_tau res_ty

	-- Do the subsumption check wrt the result type
	; let res_subst = zipTopTvSubst qtvs qtv_tys
	      fun_tau'  = substTy res_subst fun_tau

	; co_fn <- tcFunResTy fun_name fun_tau' res_ty

	-- And pack up the results
	; fun' <- instFun orig fun res_subst tv_theta_prs 
	; traceTc (text "tcId yields" <+> ppr (mkHsWrap co_fn fun'))
	; return (mkHsWrap co_fn fun') }

-- 	Note [Push result type in]
--
-- Unify with expected result before (was: after) type-checking the args
-- so that the info from res_ty (was: args) percolates to args (was actual_res_ty).
-- This is when we might detect a too-few args situation.
-- (One can think of cases when the opposite order would give
-- a better error message.)
-- [March 2003: I'm experimenting with putting this first.  Here's an 
--		example where it actually makes a real difference
--    class C t a b | t a -> b
--    instance C Char a Bool
--
--    data P t a = forall b. (C t a b) => MkP b
--    data Q t   = MkQ (forall a. P t a)

--    f1, f2 :: Q Char;
--    f1 = MkQ (MkP True)
--    f2 = MkQ (MkP True :: forall a. P Char a)
--
-- With the change, f1 will type-check, because the 'Char' info from
-- the signature is propagated into MkQ's argument. With the check
-- in the other order, the extra signature in f2 is reqd.]

---------------------------
tcSyntaxOp :: InstOrigin -> HsExpr Name -> TcType -> TcM (HsExpr TcId)
-- Typecheck a syntax operator, checking that it has the specified type
-- The operator is always a variable at this stage (i.e. renamer output)
tcSyntaxOp orig (HsVar op) ty = tcId orig op ty
tcSyntaxOp orig other 	   ty = pprPanic "tcSyntaxOp" (ppr other)

---------------------------
instFun :: InstOrigin
	-> HsExpr TcId
	-> TvSubst		  -- The instantiating substitution
	-> [([TyVar], ThetaType)] -- Stuff to instantiate
	-> TcM (HsExpr TcId)	

instFun orig fun subst []
  = return fun		-- Common short cut

instFun orig fun subst tv_theta_prs
  = do 	{ let ty_theta_prs' = map subst_pr tv_theta_prs
	; traceTc (text "instFun" <+> ppr ty_theta_prs')
                -- Make two ad-hoc checks 
	; doStupidChecks fun ty_theta_prs'

		-- Now do normal instantiation
	; result <- go True fun ty_theta_prs' 
	; traceTc (text "instFun result" <+> ppr result)
	; return result
	}
  where
    subst_pr (tvs, theta) 
	= (substTyVars subst tvs, substTheta subst theta)

    go _ fun [] = do {traceTc (text "go _ fun [] returns" <+> ppr fun) ; return fun }

    go True (HsVar fun_id) ((tys,theta) : prs)
	| want_method_inst theta
	= do { traceTc (text "go (HsVar fun_id) ((tys,theta) : prs) | want_method_inst theta")
	     ; meth_id <- newMethodWithGivenTy orig fun_id tys
	     ; go False (HsVar meth_id) prs }
		-- Go round with 'False' to prevent further use
		-- of newMethod: see Note [Multiple instantiation]

    go _ fun ((tys, theta) : prs)
	= do { co_fn <- instCall orig tys theta
	     ; traceTc (text "go yields co_fn" <+> ppr co_fn)
	     ; go False (HsWrap co_fn fun) prs }

	-- See Note [No method sharing]
    want_method_inst theta =  not (null theta)	-- Overloaded
		   	   && not opt_NoMethodSharing
\end{code}

Note [Multiple instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We are careful never to make a MethodInst that has, as its meth_id, another MethodInst.
For example, consider
	f :: forall a. Eq a => forall b. Ord b => a -> b
At a call to f, at say [Int, Bool], it's tempting to translate the call to 

	f_m1
  where
	f_m1 :: forall b. Ord b => Int -> b
	f_m1 = f Int dEqInt

	f_m2 :: Int -> Bool
	f_m2 = f_m1 Bool dOrdBool

But notice that f_m2 has f_m1 as its meth_id.  Now the danger is that if we do
a tcSimplCheck with a Given f_mx :: f Int dEqInt, we may make a binding
	f_m1 = f_mx
But it's entirely possible that f_m2 will continue to float out, because it
mentions no type variables.  Result, f_m1 isn't in scope.

Here's a concrete example that does this (test tc200):

    class C a where
      f :: Eq b => b -> a -> Int
      baz :: Eq a => Int -> a -> Int

    instance C Int where
      baz = f

Current solution: only do the "method sharing" thing for the first type/dict
application, not for the iterated ones.  A horribly subtle point.

Note [No method sharing]
~~~~~~~~~~~~~~~~~~~~~~~~
The -fno-method-sharing flag controls what happens so far as the LIE
is concerned.  The default case is that for an overloaded function we 
generate a "method" Id, and add the Method Inst to the LIE.  So you get
something like
	f :: Num a => a -> a
	f = /\a (d:Num a) -> let m = (+) a d in \ (x:a) -> m x x
If you specify -fno-method-sharing, the dictionary application 
isn't shared, so we get
	f :: Num a => a -> a
	f = /\a (d:Num a) (x:a) -> (+) a d x x
This gets a bit less sharing, but
	a) it's better for RULEs involving overloaded functions
	b) perhaps fewer separated lambdas

Note [Left to right]
~~~~~~~~~~~~~~~~~~~~
tcArgs implements a left-to-right order, which goes beyond what is described in the
impredicative type inference paper.  In particular, it allows
	runST $ foo
where runST :: (forall s. ST s a) -> a
When typechecking the application of ($)::(a->b) -> a -> b, we first check that
runST has type (a->b), thereby filling in a=forall s. ST s a.  Then we un-box this type
before checking foo.  The left-to-right order really helps here.

\begin{code}
tcArgs :: LHsExpr Name				-- The function (for error messages)
       -> [LHsExpr Name] 			-- Actual args
       -> ArgChecker [LHsExpr TcId]

type ArgChecker results
   = [TyVar] -> [TcSigmaType] 		-- Current instantiation
   -> [TcSigmaType]			-- Expected arg types (**before** applying the instantiation)
   -> TcM ([TcSigmaType], results)	-- Resulting instaniation and args

tcArgs fun args qtvs qtys arg_tys
  = go 1 qtys args arg_tys
  where
    go n qtys [] [] = return (qtys, [])
    go n qtys (arg:args) (arg_ty:arg_tys)
	= do { arg' <- tcArg fun n arg qtvs qtys arg_ty
	     ; qtys' <- mapM refineBox qtys	-- Exploit new info
	     ; (qtys'', args') <- go (n+1) qtys' args arg_tys
	     ; return (qtys'', arg':args') }
    go n qtys args arg_tys = panic "tcArgs"

tcArg :: LHsExpr Name				-- The function
      -> Int					--   and arg number (for error messages)
      -> LHsExpr Name
      -> [TyVar] -> [TcSigmaType] 		-- Instantiate the arg type like this
      -> BoxySigmaType
      -> TcM (LHsExpr TcId)			-- Resulting argument
tcArg fun arg_no arg qtvs qtys ty
  = addErrCtxt (funAppCtxt fun arg arg_no) $
    tcPolyExprNC arg (substTyWith qtvs qtys ty)
\end{code}


Note [tagToEnum#]
~~~~~~~~~~~~~~~~~
Nasty check to ensure that tagToEnum# is applied to a type that is an
enumeration TyCon.  Unification may refine the type later, but this
check won't see that, alas.  It's crude but it works.

Here's are two cases that should fail
 	f :: forall a. a
	f = tagToEnum# 0	-- Can't do tagToEnum# at a type variable

	g :: Int
	g = tagToEnum# 0	-- Int is not an enumeration


\begin{code}
doStupidChecks :: HsExpr TcId
	       -> [([TcType], ThetaType)]
	       -> TcM ()
-- Check two tiresome and ad-hoc cases
-- (a) the "stupid theta" for a data con; add the constraints
--     from the "stupid theta" of a data constructor (sigh)
-- (b) deal with the tagToEnum# problem: see Note [tagToEnum#]

doStupidChecks (HsVar fun_id) ((tys,_):_)
  | Just con <- isDataConId_maybe fun_id   -- (a)
  = addDataConStupidTheta con tys

  | fun_id `hasKey` tagToEnumKey           -- (b)
  = do	{ tys' <- zonkTcTypes tys
	; checkTc (ok tys') (tagToEnumError tys')
	}
  where
    ok [] 	= False
    ok (ty:tys) = case tcSplitTyConApp_maybe ty of
			Just (tc,_) -> isEnumerationTyCon tc
			Nothing     -> False

doStupidChecks fun tv_theta_prs
  = return () -- The common case
				      

tagToEnumError tys
  = hang (ptext SLIT("Bad call to tagToEnum#") <+> at_type)
	 2 (vcat [ptext SLIT("Specify the type by giving a type signature"),
		  ptext SLIT("e.g. (tagToEnum# x) :: Bool")])
  where
    at_type | null tys = empty	-- Probably never happens
	    | otherwise = ptext SLIT("at type") <+> ppr (head tys)
\end{code}

%************************************************************************
%*									*
\subsection{@tcId@ typechecks an identifier occurrence}
%*									*
%************************************************************************

\begin{code}
lookupFun :: InstOrigin -> Name -> TcM (HsExpr TcId, TcType)
lookupFun orig id_name
  = do 	{ thing <- tcLookup id_name
	; case thing of
    	    AGlobal (ADataCon con) -> return (HsVar wrap_id, idType wrap_id)
				   where
				      wrap_id = dataConWrapId con

    	    AGlobal (AnId id) 
	    	| isNaughtyRecordSelector id -> failWithTc (naughtyRecordSel id)
	    	| otherwise		     -> return (HsVar id, idType id)
	    	-- A global cannot possibly be ill-staged
	    	-- nor does it need the 'lifting' treatment

    	    ATcId { tct_id = id, tct_type = ty, tct_co = mb_co, tct_level = lvl }
		-> do { thLocalId orig id ty lvl
		      ; case mb_co of
			  Unrefineable    -> return (HsVar id, ty)
			  Rigid co        -> return (mkHsWrap co (HsVar id), ty) 	
			  Wobbly 	  -> traceTc (text "lookupFun" <+> ppr id) >> return (HsVar id, ty)	-- Wobbly, or no free vars
			  WobblyInvisible -> failWithTc (ppr id_name <+> ptext SLIT(" not in scope because it has a wobbly type (solution: add a type annotation)"))
		      }

    	    other -> failWithTc (ppr other <+> ptext SLIT("used where a value identifer was expected"))
    }

#ifndef GHCI  /* GHCI and TH is off */
--------------------------------------
-- thLocalId : Check for cross-stage lifting
thLocalId orig id id_ty th_bind_lvl
  = return ()

#else	      /* GHCI and TH is on */
thLocalId orig id id_ty th_bind_lvl 
  = do	{ use_stage <- getStage	-- TH case
	; case use_stage of
	    Brack use_lvl ps_var lie_var | use_lvl > th_bind_lvl
		  -> thBrackId orig id ps_var lie_var
	    other -> do { checkWellStaged (quotes (ppr id)) th_bind_lvl use_stage
			; return id }
	}

--------------------------------------
thBrackId orig id ps_var lie_var
  | thTopLevelId id
  =	-- Top-level identifiers in this module,
	-- (which have External Names)
	-- are just like the imported case:
	-- no need for the 'lifting' treatment
	-- E.g.  this is fine:
	--   f x = x
	--   g y = [| f 3 |]
	-- But we do need to put f into the keep-alive
	-- set, because after desugaring the code will
	-- only mention f's *name*, not f itself.
    do	{ keepAliveTc id; return id }

  | otherwise
  = 	-- Nested identifiers, such as 'x' in
	-- E.g. \x -> [| h x |]
	-- We must behave as if the reference to x was
	--	h $(lift x)	
	-- We use 'x' itself as the splice proxy, used by 
	-- the desugarer to stitch it all back together.
	-- If 'x' occurs many times we may get many identical
	-- bindings of the same splice proxy, but that doesn't
	-- matter, although it's a mite untidy.
    do 	{ let id_ty = idType id
	; checkTc (isTauTy id_ty) (polySpliceErr id)
	       -- If x is polymorphic, its occurrence sites might
	       -- have different instantiations, so we can't use plain
	       -- 'x' as the splice proxy name.  I don't know how to 
	       -- solve this, and it's probably unimportant, so I'm
	       -- just going to flag an error for now
   
	; id_ty' <- zapToMonotype id_ty
		-- The id_ty might have an OpenTypeKind, but we
		-- can't instantiate the Lift class at that kind,
		-- so we zap it to a LiftedTypeKind monotype
		-- C.f. the call in TcPat.newLitInst

	; setLIEVar lie_var	$ do
	{ lift <- newMethodFromName orig id_ty' DsMeta.liftName
		   -- Put the 'lift' constraint into the right LIE
	   
		   -- Update the pending splices
	; ps <- readMutVar ps_var
	; writeMutVar ps_var ((idName id, nlHsApp (nlHsVar lift) (nlHsVar id)) : ps)

	; return id } }
#endif /* GHCI */
\end{code}


%************************************************************************
%*									*
\subsection{Record bindings}
%*									*
%************************************************************************

Game plan for record bindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. Find the TyCon for the bindings, from the first field label.

2. Instantiate its tyvars and unify (T a1 .. an) with expected_ty.

For each binding field = value

3. Instantiate the field type (from the field label) using the type
   envt from step 2.

4  Type check the value using tcArg, passing the field type as 
   the expected argument type.

This extends OK when the field types are universally quantified.

	
\begin{code}
tcRecordBinds
	:: DataCon
	-> [TcType]	-- Expected type for each field
	-> HsRecordBinds Name
	-> TcM (HsRecordBinds TcId)

tcRecordBinds data_con arg_tys (HsRecFields rbinds dd)
  = do	{ mb_binds <- mappM do_bind rbinds
	; return (HsRecFields (catMaybes mb_binds) dd) }
  where
    flds_w_tys = zipEqual "tcRecordBinds" (dataConFieldLabels data_con) arg_tys
    do_bind fld@(HsRecField { hsRecFieldId = L loc field_lbl, hsRecFieldArg = rhs })
      | Just field_ty <- assocMaybe flds_w_tys field_lbl
      = addErrCtxt (fieldCtxt field_lbl)	$
	do { rhs'   <- tcPolyExprNC rhs field_ty
	   ; sel_id <- tcLookupField field_lbl
	   ; ASSERT( isRecordSelector sel_id )
	     return (Just (fld { hsRecFieldId = L loc sel_id, hsRecFieldArg = rhs' })) }
      | otherwise
      = do { addErrTc (badFieldCon data_con field_lbl)
	   ; return Nothing }

checkMissingFields :: DataCon -> HsRecordBinds Name -> TcM ()
checkMissingFields data_con rbinds
  | null field_labels 	-- Not declared as a record;
			-- But C{} is still valid if no strict fields
  = if any isMarkedStrict field_strs then
	-- Illegal if any arg is strict
	addErrTc (missingStrictFields data_con [])
    else
	returnM ()
			
  | otherwise		-- A record
  = checkM (null missing_s_fields)
	   (addErrTc (missingStrictFields data_con missing_s_fields))	`thenM_`

    doptM Opt_WarnMissingFields		`thenM` \ warn ->
    checkM (not (warn && notNull missing_ns_fields))
	   (warnTc True (missingFields data_con missing_ns_fields))

  where
    missing_s_fields
	= [ fl | (fl, str) <- field_info,
	  	 isMarkedStrict str,
	  	 not (fl `elem` field_names_used)
	  ]
    missing_ns_fields
	= [ fl | (fl, str) <- field_info,
	  	 not (isMarkedStrict str),
	  	 not (fl `elem` field_names_used)
	  ]

    field_names_used = hsRecFields rbinds
    field_labels     = dataConFieldLabels data_con

    field_info = zipEqual "missingFields"
			  field_labels
	  		  field_strs

    field_strs = dataConStrictMarks data_con
\end{code}

%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

Boring and alphabetical:
\begin{code}
caseScrutCtxt expr
  = hang (ptext SLIT("In the scrutinee of a case expression:")) 4 (ppr expr)

exprCtxt expr
  = hang (ptext SLIT("In the expression:")) 4 (ppr expr)

fieldCtxt field_name
  = ptext SLIT("In the") <+> quotes (ppr field_name) <+> ptext SLIT("field of a record")

funAppCtxt fun arg arg_no
  = hang (hsep [ ptext SLIT("In the"), speakNth arg_no, ptext SLIT("argument of"), 
		    quotes (ppr fun) <> text ", namely"])
	 4 (quotes (ppr arg))

predCtxt expr
  = hang (ptext SLIT("In the predicate expression:")) 4 (ppr expr)

nonVanillaUpd tycon
  = vcat [ptext SLIT("Record update for the non-Haskell-98 data type") 
		<+> quotes (pprSourceTyCon tycon)
		<+> ptext SLIT("is not (yet) supported"),
	  ptext SLIT("Use pattern-matching instead")]
badFieldsUpd rbinds
  = hang (ptext SLIT("No constructor has all these fields:"))
	 4 (pprQuotedList (hsRecFields rbinds))

naughtyRecordSel sel_id
  = ptext SLIT("Cannot use record selector") <+> quotes (ppr sel_id) <+> 
    ptext SLIT("as a function due to escaped type variables") $$ 
    ptext SLIT("Probably fix: use pattern-matching syntax instead")

notSelector field
  = hsep [quotes (ppr field), ptext SLIT("is not a record selector")]

missingStrictFields :: DataCon -> [FieldLabel] -> SDoc
missingStrictFields con fields
  = header <> rest
  where
    rest | null fields = empty	-- Happens for non-record constructors 
				-- with strict fields
	 | otherwise   = colon <+> pprWithCommas ppr fields

    header = ptext SLIT("Constructor") <+> quotes (ppr con) <+> 
	     ptext SLIT("does not have the required strict field(s)") 
	  
missingFields :: DataCon -> [FieldLabel] -> SDoc
missingFields con fields
  = ptext SLIT("Fields of") <+> quotes (ppr con) <+> ptext SLIT("not initialised:") 
	<+> pprWithCommas ppr fields

-- callCtxt fun args = ptext SLIT("In the call") <+> parens (ppr (foldl mkHsApp fun args))

#ifdef GHCI
polySpliceErr :: Id -> SDoc
polySpliceErr id
  = ptext SLIT("Can't splice the polymorphic local variable") <+> quotes (ppr id)
#endif
\end{code}
