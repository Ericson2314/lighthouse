%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Desugaring exporessions.

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module DsExpr ( dsExpr, dsLExpr, dsLocalBinds, dsValBinds, dsLit ) where

#include "HsVersions.h"


import Match
import MatchLit
import DsBinds
import DsGRHSs
import DsListComp
import DsUtils
import DsArrows
import DsMonad
import Name

#ifdef GHCI
import PrelNames
	-- Template Haskell stuff iff bootstrapped
import DsMeta
#endif

import HsSyn
import TcHsSyn

-- NB: The desugarer, which straddles the source and Core worlds, sometimes
--     needs to see source types
import TcType
import Type
import CoreSyn
import CoreUtils

import CostCentre
import Id
import PrelInfo
import DataCon
import TysWiredIn
import BasicTypes
import PrelNames
import SrcLoc
import Util
import Bag
import Outputable
import FastString
\end{code}


%************************************************************************
%*									*
		dsLocalBinds, dsValBinds
%*									*
%************************************************************************

\begin{code}
dsLocalBinds :: HsLocalBinds Id -> CoreExpr -> DsM CoreExpr
dsLocalBinds EmptyLocalBinds	body = return body
dsLocalBinds (HsValBinds binds) body = dsValBinds binds body
dsLocalBinds (HsIPBinds binds)  body = dsIPBinds  binds body

-------------------------
dsValBinds :: HsValBinds Id -> CoreExpr -> DsM CoreExpr
dsValBinds (ValBindsOut binds _) body = foldrDs ds_val_bind body binds

-------------------------
dsIPBinds (IPBinds ip_binds dict_binds) body
  = do	{ prs <- dsLHsBinds dict_binds
	; let inner = Let (Rec prs) body
		-- The dict bindings may not be in 
		-- dependency order; hence Rec
	; foldrDs ds_ip_bind inner ip_binds }
  where
    ds_ip_bind (L _ (IPBind n e)) body
      = dsLExpr e	`thenDs` \ e' ->
	returnDs (Let (NonRec (ipNameName n) e') body)

-------------------------
ds_val_bind :: (RecFlag, LHsBinds Id) -> CoreExpr -> DsM CoreExpr
-- Special case for bindings which bind unlifted variables
-- We need to do a case right away, rather than building
-- a tuple and doing selections.
-- Silently ignore INLINE and SPECIALISE pragmas...
ds_val_bind (NonRecursive, hsbinds) body
  | [L _ (AbsBinds [] [] exports binds)] <- bagToList hsbinds,
    (L loc bind : null_binds) <- bagToList binds,
    isBangHsBind bind
    || isUnboxedTupleBind bind
    || or [isUnLiftedType (idType g) | (_, g, _, _) <- exports]
  = let
      body_w_exports		      = foldr bind_export body exports
      bind_export (tvs, g, l, _) body = ASSERT( null tvs )
				        bindNonRec g (Var l) body
    in
    ASSERT (null null_binds)
	-- Non-recursive, non-overloaded bindings only come in ones
	-- ToDo: in some bizarre case it's conceivable that there
	--       could be dict binds in the 'binds'.  (See the notes
	--	 below.  Then pattern-match would fail.  Urk.)
    putSrcSpanDs loc	$
    case bind of
      FunBind { fun_id = L _ fun, fun_matches = matches, fun_co_fn = co_fn, 
		fun_tick = tick, fun_infix = inf }
	-> matchWrapper (FunRhs (idName fun ) inf) matches 	`thenDs` \ (args, rhs) ->
	   ASSERT( null args )	-- Functions aren't lifted
	   ASSERT( isIdHsWrapper co_fn )
           mkOptTickBox tick rhs 				`thenDs` \ rhs' ->
	   returnDs (bindNonRec fun rhs' body_w_exports)

      PatBind {pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty }
	-> 	-- let C x# y# = rhs in body
		-- ==> case rhs of C x# y# -> body
	   putSrcSpanDs loc			$
	   do { rhs <- dsGuarded grhss ty
	      ; let upat = unLoc pat
		    eqn = EqnInfo { eqn_pats = [upat], 
				    eqn_rhs = cantFailMatchResult body_w_exports }
	      ; var    <- selectMatchVar upat
	      ; result <- matchEquations PatBindRhs [var] [eqn] (exprType body)
	      ; return (scrungleMatch var rhs result) }

      other -> pprPanic "dsLet: unlifted" (pprLHsBinds hsbinds $$ ppr body)


-- Ordinary case for bindings; none should be unlifted
ds_val_bind (is_rec, binds) body
  = do	{ prs <- dsLHsBinds binds
	; ASSERT( not (any (isUnLiftedType . idType . fst) prs) )
	  case prs of
	    []    -> return body
	    other -> return (Let (Rec prs) body) }
	-- Use a Rec regardless of is_rec. 
	-- Why? Because it allows the binds to be all
	-- mixed up, which is what happens in one rare case
	-- Namely, for an AbsBind with no tyvars and no dicts,
	-- 	   but which does have dictionary bindings.
	-- See notes with TcSimplify.inferLoop [NO TYVARS]
	-- It turned out that wrapping a Rec here was the easiest solution
	--
	-- NB The previous case dealt with unlifted bindings, so we
	--    only have to deal with lifted ones now; so Rec is ok

isUnboxedTupleBind :: HsBind Id -> Bool
isUnboxedTupleBind (PatBind { pat_rhs_ty = ty }) = isUnboxedTupleType ty
isUnboxedTupleBind other			 = False

scrungleMatch :: Id -> CoreExpr -> CoreExpr -> CoreExpr
-- Returns something like (let var = scrut in body)
-- but if var is an unboxed-tuple type, it inlines it in a fragile way
-- Special case to handle unboxed tuple patterns; they can't appear nested
-- The idea is that 
--	case e of (# p1, p2 #) -> rhs
-- should desugar to
--	case e of (# x1, x2 #) -> ... match p1, p2 ...
-- NOT
--	let x = e in case x of ....
--
-- But there may be a big 
--	let fail = ... in case e of ...
-- wrapping the whole case, which complicates matters slightly
-- It all seems a bit fragile.  Test is dsrun013.

scrungleMatch var scrut body
  | isUnboxedTupleType (idType var) = scrungle body
  | otherwise			    = bindNonRec var scrut body
  where
    scrungle (Case (Var x) bndr ty alts)
		    | x == var = Case scrut bndr ty alts
    scrungle (Let binds body)  = Let binds (scrungle body)
    scrungle other = panic ("scrungleMatch: tuple pattern:\n" ++ showSDoc (ppr other))

\end{code}	

%************************************************************************
%*									*
\subsection[DsExpr-vars-and-cons]{Variables, constructors, literals}
%*									*
%************************************************************************

\begin{code}
dsLExpr :: LHsExpr Id -> DsM CoreExpr

dsLExpr (L loc e) = putSrcSpanDs loc $ dsExpr e

dsExpr :: HsExpr Id -> DsM CoreExpr
dsExpr (HsPar e) 	      = dsLExpr e
dsExpr (ExprWithTySigOut e _) = dsLExpr e
dsExpr (HsVar var)     	      = returnDs (Var var)
dsExpr (HsIPVar ip)    	      = returnDs (Var (ipNameName ip))
dsExpr (HsLit lit)     	      = dsLit lit
dsExpr (HsOverLit lit) 	      = dsOverLit lit
dsExpr (HsWrap co_fn e)       = dsCoercion co_fn (dsExpr e)

dsExpr (NegApp expr neg_expr) 
  = do	{ core_expr <- dsLExpr expr
	; core_neg  <- dsExpr neg_expr
	; return (core_neg `App` core_expr) }

dsExpr expr@(HsLam a_Match)
  = matchWrapper LambdaExpr a_Match	`thenDs` \ (binders, matching_code) ->
    returnDs (mkLams binders matching_code)

dsExpr expr@(HsApp fun arg)      
  = dsLExpr fun		`thenDs` \ core_fun ->
    dsLExpr arg		`thenDs` \ core_arg ->
    returnDs (core_fun `mkDsApp` core_arg)
\end{code}

Operator sections.  At first it looks as if we can convert
\begin{verbatim}
	(expr op)
\end{verbatim}
to
\begin{verbatim}
	\x -> op expr x
\end{verbatim}

But no!  expr might be a redex, and we can lose laziness badly this
way.  Consider
\begin{verbatim}
	map (expr op) xs
\end{verbatim}
for example.  So we convert instead to
\begin{verbatim}
	let y = expr in \x -> op y x
\end{verbatim}
If \tr{expr} is actually just a variable, say, then the simplifier
will sort it out.

\begin{code}
dsExpr (OpApp e1 op _ e2)
  = dsLExpr op						`thenDs` \ core_op ->
    -- for the type of y, we need the type of op's 2nd argument
    dsLExpr e1				`thenDs` \ x_core ->
    dsLExpr e2				`thenDs` \ y_core ->
    returnDs (mkDsApps core_op [x_core, y_core])
    
dsExpr (SectionL expr op)	-- Desugar (e !) to ((!) e)
  = dsLExpr op				`thenDs` \ core_op ->
    dsLExpr expr			`thenDs` \ x_core ->
    returnDs (mkDsApp core_op x_core)

-- dsLExpr (SectionR op expr)	-- \ x -> op x expr
dsExpr (SectionR op expr)
  = dsLExpr op			`thenDs` \ core_op ->
    -- for the type of x, we need the type of op's 2nd argument
    let
	(x_ty:y_ty:_, _) = splitFunTys (exprType core_op)
	-- See comment with SectionL
    in
    dsLExpr expr				`thenDs` \ y_core ->
    newSysLocalDs x_ty			`thenDs` \ x_id ->
    newSysLocalDs y_ty			`thenDs` \ y_id ->

    returnDs (bindNonRec y_id y_core $
	      Lam x_id (mkDsApps core_op [Var x_id, Var y_id]))

dsExpr (HsSCC cc expr)
  = dsLExpr expr			`thenDs` \ core_expr ->
    getModuleDs			`thenDs` \ mod_name ->
    returnDs (Note (SCC (mkUserCC cc mod_name)) core_expr)


-- hdaume: core annotation

dsExpr (HsCoreAnn fs expr)
  = dsLExpr expr        `thenDs` \ core_expr ->
    returnDs (Note (CoreNote $ unpackFS fs) core_expr)

dsExpr (HsCase discrim matches)
  = dsLExpr discrim			`thenDs` \ core_discrim ->
    matchWrapper CaseAlt matches 	`thenDs` \ ([discrim_var], matching_code) ->
    returnDs (scrungleMatch discrim_var core_discrim matching_code)

-- Pepe: The binds are in scope in the body but NOT in the binding group
--       This is to avoid silliness in breakpoints
dsExpr (HsLet binds body)
  = dsLExpr body `thenDs` \ body' ->
    dsLocalBinds binds body'

-- We need the `ListComp' form to use `deListComp' (rather than the "do" form)
-- because the interpretation of `stmts' depends on what sort of thing it is.
--
dsExpr (HsDo ListComp stmts body result_ty)
  =	-- Special case for list comprehensions
    dsListComp stmts body elt_ty
  where
    [elt_ty] = tcTyConAppArgs result_ty

dsExpr (HsDo DoExpr stmts body result_ty)
  = dsDo stmts body result_ty

dsExpr (HsDo (MDoExpr tbl) stmts body result_ty)
  = dsMDo tbl stmts body result_ty

dsExpr (HsDo PArrComp stmts body result_ty)
  =	-- Special case for array comprehensions
    dsPArrComp (map unLoc stmts) body elt_ty
  where
    [elt_ty] = tcTyConAppArgs result_ty

dsExpr (HsIf guard_expr then_expr else_expr)
  = dsLExpr guard_expr	`thenDs` \ core_guard ->
    dsLExpr then_expr	`thenDs` \ core_then ->
    dsLExpr else_expr	`thenDs` \ core_else ->
    returnDs (mkIfThenElse core_guard core_then core_else)
\end{code}


\noindent
\underline{\bf Various data construction things}
%              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
dsExpr (ExplicitList ty xs)
  = go xs
  where
    go []     = returnDs (mkNilExpr ty)
    go (x:xs) = dsLExpr x				`thenDs` \ core_x ->
		go xs					`thenDs` \ core_xs ->
		returnDs (mkConsExpr ty core_x core_xs)

-- we create a list from the array elements and convert them into a list using
-- `PrelPArr.toP'
--
--  * the main disadvantage to this scheme is that `toP' traverses the list
--   twice: once to determine the length and a second time to put to elements
--   into the array; this inefficiency could be avoided by exposing some of
--   the innards of `PrelPArr' to the compiler (ie, have a `PrelPArrBase') so
--   that we can exploit the fact that we already know the length of the array
--   here at compile time
--
dsExpr (ExplicitPArr ty xs)
  = dsLookupGlobalId toPName				`thenDs` \toP      ->
    dsExpr (ExplicitList ty xs)				`thenDs` \coreList ->
    returnDs (mkApps (Var toP) [Type ty, coreList])

dsExpr (ExplicitTuple expr_list boxity)
  = mappM dsLExpr expr_list	  `thenDs` \ core_exprs  ->
    returnDs (mkConApp (tupleCon boxity (length expr_list))
	    	       (map (Type .  exprType) core_exprs ++ core_exprs))

dsExpr (ArithSeq expr (From from))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsLExpr from	  `thenDs` \ from2 ->
    returnDs (App expr2 from2)

dsExpr (ArithSeq expr (FromTo from two))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsLExpr from	  `thenDs` \ from2 ->
    dsLExpr two		  `thenDs` \ two2 ->
    returnDs (mkApps expr2 [from2, two2])

dsExpr (ArithSeq expr (FromThen from thn))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsLExpr from	  `thenDs` \ from2 ->
    dsLExpr thn		  `thenDs` \ thn2 ->
    returnDs (mkApps expr2 [from2, thn2])

dsExpr (ArithSeq expr (FromThenTo from thn two))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsLExpr from	  `thenDs` \ from2 ->
    dsLExpr thn		  `thenDs` \ thn2 ->
    dsLExpr two		  `thenDs` \ two2 ->
    returnDs (mkApps expr2 [from2, thn2, two2])

dsExpr (PArrSeq expr (FromTo from two))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsLExpr from	  `thenDs` \ from2 ->
    dsLExpr two		  `thenDs` \ two2 ->
    returnDs (mkApps expr2 [from2, two2])

dsExpr (PArrSeq expr (FromThenTo from thn two))
  = dsExpr expr		  `thenDs` \ expr2 ->
    dsLExpr from	  `thenDs` \ from2 ->
    dsLExpr thn		  `thenDs` \ thn2 ->
    dsLExpr two		  `thenDs` \ two2 ->
    returnDs (mkApps expr2 [from2, thn2, two2])

dsExpr (PArrSeq expr _)
  = panic "DsExpr.dsExpr: Infinite parallel array!"
    -- the parser shouldn't have generated it and the renamer and typechecker
    -- shouldn't have let it through
\end{code}

\noindent
\underline{\bf Record construction and update}
%              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For record construction we do this (assuming T has three arguments)
\begin{verbatim}
	T { op2 = e }
==>
	let err = /\a -> recConErr a 
	T (recConErr t1 "M.lhs/230/op1") 
	  e 
	  (recConErr t1 "M.lhs/230/op3")
\end{verbatim}
@recConErr@ then converts its arugment string into a proper message
before printing it as
\begin{verbatim}
	M.lhs, line 230: missing field op1 was evaluated
\end{verbatim}

We also handle @C{}@ as valid construction syntax for an unlabelled
constructor @C@, setting all of @C@'s fields to bottom.

\begin{code}
dsExpr (RecordCon (L _ data_con_id) con_expr rbinds)
  = dsExpr con_expr	`thenDs` \ con_expr' ->
    let
	(arg_tys, _) = tcSplitFunTys (exprType con_expr')
	-- A newtype in the corner should be opaque; 
	-- hence TcType.tcSplitFunTys

	mk_arg (arg_ty, lbl)	-- Selector id has the field label as its name
	  = case findField (rec_flds rbinds) lbl of
	      (rhs:rhss) -> ASSERT( null rhss )
		 	    dsLExpr rhs
	      []         -> mkErrorAppDs rEC_CON_ERROR_ID arg_ty (showSDoc (ppr lbl))
	unlabelled_bottom arg_ty = mkErrorAppDs rEC_CON_ERROR_ID arg_ty ""

	labels = dataConFieldLabels (idDataCon data_con_id)
	-- The data_con_id is guaranteed to be the wrapper id of the constructor
    in

    (if null labels
	then mappM unlabelled_bottom arg_tys
	else mappM mk_arg (zipEqual "dsExpr:RecordCon" arg_tys labels))
	`thenDs` \ con_args ->

    returnDs (mkApps con_expr' con_args)
\end{code}

Record update is a little harder. Suppose we have the decl:
\begin{verbatim}
	data T = T1 {op1, op2, op3 :: Int}
	       | T2 {op4, op2 :: Int}
	       | T3
\end{verbatim}
Then we translate as follows:
\begin{verbatim}
	r { op2 = e }
===>
	let op2 = e in
	case r of
	  T1 op1 _ op3 -> T1 op1 op2 op3
	  T2 op4 _     -> T2 op4 op2
	  other	       -> recUpdError "M.lhs/230"
\end{verbatim}
It's important that we use the constructor Ids for @T1@, @T2@ etc on the
RHSs, and do not generate a Core constructor application directly, because the constructor
might do some argument-evaluation first; and may have to throw away some
dictionaries.

\begin{code}
dsExpr expr@(RecordUpd record_expr (HsRecFields { rec_flds = fields })
		       cons_to_upd in_inst_tys out_inst_tys)
  | null fields
  = dsLExpr record_expr
  | otherwise
  =	-- Record stuff doesn't work for existentials
	-- The type checker checks for this, but we need 
	-- worry only about the constructors that are to be updated
    ASSERT2( notNull cons_to_upd && all isVanillaDataCon cons_to_upd, ppr expr )

    do	{ record_expr' <- dsLExpr record_expr
	; let	-- Awkwardly, for families, the match goes 
		-- from instance type to family type
		tycon     = dataConTyCon (head cons_to_upd)
		in_ty     = mkTyConApp tycon in_inst_tys
		in_out_ty = mkFunTy in_ty
				    (mkFamilyTyConApp tycon out_inst_tys)

		mk_val_arg field old_arg_id 
		  = case findField fields field  of
		      (rhs:rest) -> ASSERT(null rest) rhs
		      []	 -> nlHsVar old_arg_id

		mk_alt con
	 	  = ASSERT( isVanillaDataCon con )
		    do 	{ arg_ids <- newSysLocalsDs (dataConInstOrigArgTys con in_inst_tys)
			-- This call to dataConInstOrigArgTys won't work for existentials
			-- but existentials don't have record types anyway
			; let val_args = zipWithEqual "dsExpr:RecordUpd" mk_val_arg
						(dataConFieldLabels con) arg_ids
			      rhs = foldl (\a b -> nlHsApp a b)
				          (nlHsTyApp (dataConWrapId con) out_inst_tys)
				          val_args
			      pat = mkPrefixConPat con (map nlVarPat arg_ids) in_ty

			; return (mkSimpleMatch [pat] rhs) }

	-- It's important to generate the match with matchWrapper,
	-- and the right hand sides with applications of the wrapper Id
	-- so that everything works when we are doing fancy unboxing on the
	-- constructor aguments.
	; alts <- mapM mk_alt cons_to_upd
	; ([discrim_var], matching_code) <- matchWrapper RecUpd (MatchGroup alts in_out_ty)

	; return (bindNonRec discrim_var record_expr' matching_code) }
\end{code}

Here is where we desugar the Template Haskell brackets and escapes

\begin{code}
-- Template Haskell stuff

#ifdef GHCI	/* Only if bootstrapping */
dsExpr (HsBracketOut x ps) = dsBracket x ps
dsExpr (HsSpliceE s)       = pprPanic "dsExpr:splice" (ppr s)
#endif

-- Arrow notation extension
dsExpr (HsProc pat cmd) = dsProcExpr pat cmd
\end{code}

Hpc Support 

\begin{code}
dsExpr (HsTick ix vars e) = do
  e' <- dsLExpr e
  mkTickBox ix vars e'

-- There is a problem here. The then and else branches
-- have no free variables, so they are open to lifting.
-- We need someway of stopping this.
-- This will make no difference to binary coverage
-- (did you go here: YES or NO), but will effect accurate
-- tick counting.

dsExpr (HsBinTick ixT ixF e) = do
  e2 <- dsLExpr e
  do { ASSERT(exprType e2 `coreEqType` boolTy)
       mkBinaryTickBox ixT ixF e2
     }
\end{code}

\begin{code}

#ifdef DEBUG
-- HsSyn constructs that just shouldn't be here:
dsExpr (ExprWithTySig _ _)  = panic "dsExpr:ExprWithTySig"
#endif


findField :: [HsRecField Id arg] -> Name -> [arg]
findField rbinds lbl 
  = [rhs | HsRecField { hsRecFieldId = id, hsRecFieldArg = rhs } <- rbinds 
	 , lbl == idName (unLoc id) ]
\end{code}

%--------------------------------------------------------------------

Desugar 'do' and 'mdo' expressions (NOT list comprehensions, they're
handled in DsListComp).  Basically does the translation given in the
Haskell 98 report:

\begin{code}
dsDo	:: [LStmt Id]
	-> LHsExpr Id
	-> Type			-- Type of the whole expression
	-> DsM CoreExpr

dsDo stmts body result_ty
  = go (map unLoc stmts)
  where
    go [] = dsLExpr body
    
    go (ExprStmt rhs then_expr _ : stmts)
      = do { rhs2 <- dsLExpr rhs
	   ; then_expr2 <- dsExpr then_expr
	   ; rest <- go stmts
	   ; returnDs (mkApps then_expr2 [rhs2, rest]) }
    
    go (LetStmt binds : stmts)
      = do { rest <- go stmts
	   ; dsLocalBinds binds rest }

    go (BindStmt pat rhs bind_op fail_op : stmts)
      = 
       do { body  <- go stmts
	   ; var   <- selectSimpleMatchVarL pat
	   ; match <- matchSinglePat (Var var) (StmtCtxt DoExpr) pat
    				  result_ty (cantFailMatchResult body)
	   ; match_code <- handle_failure pat match fail_op
           ; rhs'       <- dsLExpr rhs
	   ; bind_op'   <- dsExpr bind_op
	   ; returnDs (mkApps bind_op' [rhs', Lam var match_code]) }
    
    -- In a do expression, pattern-match failure just calls
    -- the monadic 'fail' rather than throwing an exception
    handle_failure pat match fail_op
      | matchCanFail match
      = do { fail_op' <- dsExpr fail_op
	   ; fail_msg <- mkStringExpr (mk_fail_msg pat)
    	   ; extractMatchResult match (App fail_op' fail_msg) }
      | otherwise
      = extractMatchResult match (error "It can't fail") 

mk_fail_msg pat = "Pattern match failure in do expression at " ++ 
		  showSDoc (ppr (getLoc pat))
\end{code}

Translation for RecStmt's: 
-----------------------------
We turn (RecStmt [v1,..vn] stmts) into:
  
  (v1,..,vn) <- mfix (\~(v1,..vn). do stmts
				      return (v1,..vn))

\begin{code}
dsMDo	:: PostTcTable
	-> [LStmt Id]
	-> LHsExpr Id
	-> Type			-- Type of the whole expression
	-> DsM CoreExpr

dsMDo tbl stmts body result_ty
  = go (map unLoc stmts)
  where
    (m_ty, b_ty) = tcSplitAppTy result_ty	-- result_ty must be of the form (m b)
    mfix_id   = lookupEvidence tbl mfixName
    return_id = lookupEvidence tbl returnMName
    bind_id   = lookupEvidence tbl bindMName
    then_id   = lookupEvidence tbl thenMName
    fail_id   = lookupEvidence tbl failMName
    ctxt      = MDoExpr tbl

    go [] = dsLExpr body
    
    go (LetStmt binds : stmts)
      = do { rest <- go stmts
	   ; dsLocalBinds binds rest }

    go (ExprStmt rhs _ rhs_ty : stmts)
      = do { rhs2 <- dsLExpr rhs
	   ; rest <- go stmts
	   ; returnDs (mkApps (Var then_id) [Type rhs_ty, Type b_ty, rhs2, rest]) }
    
    go (BindStmt pat rhs _ _ : stmts)
      = do { body  <- go stmts
	   ; var   <- selectSimpleMatchVarL pat
	   ; match <- matchSinglePat (Var var) (StmtCtxt ctxt) pat
    				  result_ty (cantFailMatchResult body)
	   ; fail_msg   <- mkStringExpr (mk_fail_msg pat)
	   ; let fail_expr = mkApps (Var fail_id) [Type b_ty, fail_msg]
	   ; match_code <- extractMatchResult match fail_expr

	   ; rhs'       <- dsLExpr rhs
	   ; returnDs (mkApps (Var bind_id) [Type (hsLPatType pat), Type b_ty, 
					     rhs', Lam var match_code]) }
    
    go (RecStmt rec_stmts later_ids rec_ids rec_rets binds : stmts)
      = ASSERT( length rec_ids > 0 )
        ASSERT( length rec_ids == length rec_rets )
	go (new_bind_stmt : let_stmt : stmts)
      where
        new_bind_stmt = mkBindStmt (mk_tup_pat later_pats) mfix_app
	let_stmt = LetStmt (HsValBinds (ValBindsOut [(Recursive, binds)] []))

	
		-- Remove the later_ids that appear (without fancy coercions) 
		-- in rec_rets, because there's no need to knot-tie them separately
		-- See Note [RecStmt] in HsExpr
	later_ids'   = filter (`notElem` mono_rec_ids) later_ids
	mono_rec_ids = [ id | HsVar id <- rec_rets ]
    
	mfix_app = nlHsApp (nlHsTyApp mfix_id [tup_ty]) mfix_arg
	mfix_arg = noLoc $ HsLam (MatchGroup [mkSimpleMatch [mfix_pat] body]
					     (mkFunTy tup_ty body_ty))

	-- The rec_tup_pat must bind the rec_ids only; remember that the 
	-- 	trimmed_laters may share the same Names
	-- Meanwhile, the later_pats must bind the later_vars
	rec_tup_pats = map mk_wild_pat later_ids' ++ map nlVarPat rec_ids
	later_pats   = map nlVarPat    later_ids' ++ map mk_later_pat rec_ids
	rets         = map nlHsVar     later_ids' ++ map noLoc rec_rets

	mfix_pat = noLoc $ LazyPat $ mk_tup_pat rec_tup_pats
	body     = noLoc $ HsDo ctxt rec_stmts return_app body_ty
	body_ty = mkAppTy m_ty tup_ty
	tup_ty  = mkCoreTupTy (map idType (later_ids' ++ rec_ids))
		  -- mkCoreTupTy deals with singleton case

	return_app  = nlHsApp (nlHsTyApp return_id [tup_ty]) 
			      (mk_ret_tup rets)

	mk_wild_pat :: Id -> LPat Id 
   	mk_wild_pat v = noLoc $ WildPat $ idType v

	mk_later_pat :: Id -> LPat Id
	mk_later_pat v | v `elem` later_ids' = mk_wild_pat v
		       | otherwise	     = nlVarPat v

 	mk_tup_pat :: [LPat Id] -> LPat Id
  	mk_tup_pat [p] = p
	mk_tup_pat ps  = noLoc $ mkVanillaTuplePat ps Boxed

	mk_ret_tup :: [LHsExpr Id] -> LHsExpr Id
	mk_ret_tup [r] = r
	mk_ret_tup rs  = noLoc $ ExplicitTuple rs Boxed
\end{code}
