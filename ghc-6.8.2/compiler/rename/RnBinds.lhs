%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnBinds]{Renaming and dependency analysis of bindings}

This module does renaming and dependency analysis on value bindings in
the abstract syntax.  It does {\em not} do cycle-checks on class or
type-synonym declarations; those cannot be done at this stage because
they may be affected by renaming (which isn't fully worked out yet).

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module RnBinds (
	rnTopBinds, 
	rnLocalBindsAndThen, rnValBindsAndThen, rnValBinds, trimWith,
	rnMethodBinds, renameSigs, mkSigTvFn,
	rnMatchGroup, rnGRHSs
   ) where

#include "HsVersions.h"

import {-# SOURCE #-} RnExpr( rnLExpr, rnStmts )

import HsSyn
import RdrHsSyn
import RnHsSyn
import TcRnMonad
import RnTypes		( rnHsSigType, rnLHsType, rnHsTypeFVs, 
			  rnLPat, rnPatsAndThen, patSigErr, checkPrecMatch )
import RnEnv		( bindLocatedLocalsRn, lookupLocatedBndrRn, 
			  lookupInstDeclBndr, newIPNameRn,
			  lookupLocatedSigOccRn, bindPatSigTyVarsFV,
			  bindLocalFixities, bindSigTyVarsFV, 
			  warnUnusedLocalBinds, mapFvRn, extendTyVarEnvFVRn,
			)
import DynFlags	( DynFlag(..) )
import Name
import NameEnv
import NameSet
import PrelNames	( isUnboundName )
import RdrName		( RdrName, rdrNameOcc )
import SrcLoc		( Located(..), unLoc )
import ListSetOps	( findDupsEq )
import BasicTypes	( RecFlag(..) )
import Digraph		( SCC(..), stronglyConnComp )
import Bag
import Outputable
import Maybes		( orElse )
import Util		( filterOut )
import Monad		( foldM )
\end{code}

-- ToDo: Put the annotations into the monad, so that they arrive in the proper
-- place and can be used when complaining.

The code tree received by the function @rnBinds@ contains definitions
in where-clauses which are all apparently mutually recursive, but which may
not really depend upon each other. For example, in the top level program
\begin{verbatim}
f x = y where a = x
	      y = x
\end{verbatim}
the definitions of @a@ and @y@ do not depend on each other at all.
Unfortunately, the typechecker cannot always check such definitions.
\footnote{Mycroft, A. 1984. Polymorphic type schemes and recursive
definitions. In Proceedings of the International Symposium on Programming,
Toulouse, pp. 217-39. LNCS 167. Springer Verlag.}
However, the typechecker usually can check definitions in which only the
strongly connected components have been collected into recursive bindings.
This is precisely what the function @rnBinds@ does.

ToDo: deal with case where a single monobinds binds the same variable
twice.

The vertag tag is a unique @Int@; the tags only need to be unique
within one @MonoBinds@, so that unique-Int plumbing is done explicitly
(heavy monad machinery not needed).


%************************************************************************
%*									*
%* naming conventions							*
%*									*
%************************************************************************

\subsection[name-conventions]{Name conventions}

The basic algorithm involves walking over the tree and returning a tuple
containing the new tree plus its free variables. Some functions, such
as those walking polymorphic bindings (HsBinds) and qualifier lists in
list comprehensions (@Quals@), return the variables bound in local
environments. These are then used to calculate the free variables of the
expression evaluated in these environments.

Conventions for variable names are as follows:
\begin{itemize}
\item
new code is given a prime to distinguish it from the old.

\item
a set of variables defined in @Exp@ is written @dvExp@

\item
a set of variables free in @Exp@ is written @fvExp@
\end{itemize}

%************************************************************************
%*									*
%* analysing polymorphic bindings (HsBindGroup, HsBind)
%*									*
%************************************************************************

\subsubsection[dep-HsBinds]{Polymorphic bindings}

Non-recursive expressions are reconstructed without any changes at top
level, although their component expressions may have to be altered.
However, non-recursive expressions are currently not expected as
\Haskell{} programs, and this code should not be executed.

Monomorphic bindings contain information that is returned in a tuple
(a @FlatMonoBinds@) containing:

\begin{enumerate}
\item
a unique @Int@ that serves as the ``vertex tag'' for this binding.

\item
the name of a function or the names in a pattern. These are a set
referred to as @dvLhs@, the defined variables of the left hand side.

\item
the free variables of the body. These are referred to as @fvBody@.

\item
the definition's actual code. This is referred to as just @code@.
\end{enumerate}

The function @nonRecDvFv@ returns two sets of variables. The first is
the set of variables defined in the set of monomorphic bindings, while the
second is the set of free variables in those bindings.

The set of variables defined in a non-recursive binding is just the
union of all of them, as @union@ removes duplicates. However, the
free variables in each successive set of cumulative bindings is the
union of those in the previous set plus those of the newest binding after
the defined variables of the previous set have been removed.

@rnMethodBinds@ deals only with the declarations in class and
instance declarations.	It expects only to see @FunMonoBind@s, and
it expects the global environment to contain bindings for the binders
(which are all class operations).

%************************************************************************
%*									*
\subsubsection{ Top-level bindings}
%*									*
%************************************************************************

@rnTopMonoBinds@ assumes that the environment already
contains bindings for the binders of this particular binding.

\begin{code}
rnTopBinds :: HsValBinds RdrName -> RnM (HsValBinds Name, DefUses)

-- The binders of the binding are in scope already;
-- the top level scope resolution does that

rnTopBinds binds
 =  do	{ is_boot <- tcIsHsBoot
	; if is_boot then rnTopBindsBoot binds
		     else rnTopBindsSrc  binds }

rnTopBindsBoot :: HsValBinds RdrName -> RnM (HsValBinds Name, DefUses)
-- A hs-boot file has no bindings. 
-- Return a single HsBindGroup with empty binds and renamed signatures
rnTopBindsBoot (ValBindsIn mbinds sigs)
  = do	{ checkErr (isEmptyLHsBinds mbinds) (bindsInHsBootFile mbinds)
	; sigs' <- renameSigs okHsBootSig sigs
	; return (ValBindsOut [] sigs', usesOnly (hsSigsFVs sigs')) }

rnTopBindsSrc :: HsValBinds RdrName -> RnM (HsValBinds Name, DefUses)
rnTopBindsSrc binds = rnValBinds noTrim binds
\end{code}



%*********************************************************
%*							*
		HsLocalBinds
%*							*
%*********************************************************

\begin{code}
rnLocalBindsAndThen 
  :: HsLocalBinds RdrName
  -> (HsLocalBinds Name -> RnM (result, FreeVars))
  -> RnM (result, FreeVars)
-- This version (a) assumes that the binding vars are not already in scope
--		(b) removes the binders from the free vars of the thing inside
-- The parser doesn't produce ThenBinds
rnLocalBindsAndThen EmptyLocalBinds thing_inside
  = thing_inside EmptyLocalBinds

rnLocalBindsAndThen (HsValBinds val_binds) thing_inside
  = rnValBindsAndThen val_binds $ \ val_binds' -> 
    thing_inside (HsValBinds val_binds')

rnLocalBindsAndThen (HsIPBinds binds) thing_inside
  = rnIPBinds binds			`thenM` \ (binds',fv_binds) ->
    thing_inside (HsIPBinds binds')	`thenM` \ (thing, fvs_thing) ->
    returnM (thing, fvs_thing `plusFV` fv_binds)

-------------
rnIPBinds (IPBinds ip_binds _no_dict_binds)
  = do	{ (ip_binds', fvs_s) <- mapAndUnzipM (wrapLocFstM rnIPBind) ip_binds
	; return (IPBinds ip_binds' emptyLHsBinds, plusFVs fvs_s) }

rnIPBind (IPBind n expr)
  = newIPNameRn  n		`thenM` \ name ->
    rnLExpr expr		`thenM` \ (expr',fvExpr) ->
    return (IPBind name expr', fvExpr)
\end{code}


%************************************************************************
%*									*
		ValBinds
%*									*
%************************************************************************

\begin{code}
rnValBindsAndThen :: HsValBinds RdrName
	          -> (HsValBinds Name -> RnM (result, FreeVars))
	          -> RnM (result, FreeVars)

rnValBindsAndThen binds@(ValBindsIn mbinds sigs) thing_inside
  =	-- Extract all the binders in this group, and extend the
	-- current scope, inventing new names for the new binders
	-- This also checks that the names form a set
    bindLocatedLocalsRn doc mbinders_w_srclocs			$ \ bndrs ->

	-- Then install local fixity declarations
	-- Notice that they scope over thing_inside too
    bindLocalFixities [sig | L _ (FixSig sig) <- sigs ] 	$

	-- Do the business
    rnValBinds (trimWith bndrs) binds	`thenM` \ (binds, bind_dus) ->

	-- Now do the "thing inside"
    thing_inside binds 			`thenM` \ (result,result_fvs) ->

	-- Final error checking
    let
	all_uses = duUses bind_dus `plusFV` result_fvs
	-- duUses: It's important to return all the uses, not the 'real uses' 
	-- used for warning about unused bindings.  Otherwise consider:
	--	x = 3
	--	y = let p = x in 'x'	-- NB: p not used
	-- If we don't "see" the dependency of 'y' on 'x', we may put the
	-- bindings in the wrong order, and the type checker will complain
	-- that x isn't in scope

	unused_bndrs = [ b | b <- bndrs, not (b `elemNameSet` all_uses)]
    in
    warnUnusedLocalBinds unused_bndrs	`thenM_`

    returnM (result, delListFromNameSet all_uses bndrs)
  where
    mbinders_w_srclocs = collectHsBindLocatedBinders mbinds
    doc = text "In the binding group for:"
	  <+> pprWithCommas ppr (map unLoc mbinders_w_srclocs)

---------------------
rnValBinds :: (FreeVars -> FreeVars)
	   -> HsValBinds RdrName
	   -> RnM (HsValBinds Name, DefUses)
-- Assumes the binders of the binding are in scope already

rnValBinds trim (ValBindsIn mbinds sigs)
  = do	{ sigs' <- rename_sigs sigs

	; binds_w_dus <- mapBagM (rnBind (mkSigTvFn sigs') trim) mbinds

	; let (binds', bind_dus) = depAnalBinds binds_w_dus

	-- We do the check-sigs after renaming the bindings,
	-- so that we have convenient access to the binders
	; check_sigs (okBindSig (duDefs bind_dus)) sigs'

	; return (ValBindsOut binds' sigs', 
		  usesOnly (hsSigsFVs sigs') `plusDU` bind_dus) }


---------------------
depAnalBinds :: Bag (LHsBind Name, [Name], Uses)
	     -> ([(RecFlag, LHsBinds Name)], DefUses)
-- Dependency analysis; this is important so that 
-- unused-binding reporting is accurate
depAnalBinds binds_w_dus
  = (map get_binds sccs, map get_du sccs)
  where
    sccs = stronglyConnComp edges

    keyd_nodes = bagToList binds_w_dus `zip` [0::Int ..]

    edges = [ (node, key, [key | n <- nameSetToList uses,
			         Just key <- [lookupNameEnv key_map n] ])
	    | (node@(_,_,uses), key) <- keyd_nodes ]

    key_map :: NameEnv Int	-- Which binding it comes from
    key_map = mkNameEnv [(bndr, key) | ((_, bndrs, _), key) <- keyd_nodes
				     , bndr <- bndrs ]

    get_binds (AcyclicSCC (bind, _, _)) = (NonRecursive, unitBag bind)
    get_binds (CyclicSCC  binds_w_dus)  = (Recursive, listToBag [b | (b,d,u) <- binds_w_dus])

    get_du (AcyclicSCC (_, bndrs, uses)) = (Just (mkNameSet bndrs), uses)
    get_du (CyclicSCC  binds_w_dus)      = (Just defs, uses)
	where
	  defs = mkNameSet [b | (_,bs,_) <- binds_w_dus, b <- bs]
	  uses = unionManyNameSets [u | (_,_,u) <- binds_w_dus]


---------------------
-- Bind the top-level forall'd type variables in the sigs.
-- E.g 	f :: a -> a
--	f = rhs
--	The 'a' scopes over the rhs
--
-- NB: there'll usually be just one (for a function binding)
--     but if there are many, one may shadow the rest; too bad!
--	e.g  x :: [a] -> [a]
--	     y :: [(a,a)] -> a
--	     (x,y) = e
--      In e, 'a' will be in scope, and it'll be the one from 'y'!

mkSigTvFn :: [LSig Name] -> (Name -> [Name])
-- Return a lookup function that maps an Id Name to the names
-- of the type variables that should scope over its body..
mkSigTvFn sigs
  = \n -> lookupNameEnv env n `orElse` []
  where
    env :: NameEnv [Name]
    env = mkNameEnv [ (name, map hsLTyVarName ltvs)
		    | L _ (TypeSig (L _ name) 
			           (L _ (HsForAllTy Explicit ltvs _ _))) <- sigs]
	-- Note the pattern-match on "Explicit"; we only bind
	-- type variables from signatures with an explicit top-level for-all
				
-- The trimming function trims the free vars we attach to a
-- binding so that it stays reasonably small
noTrim :: FreeVars -> FreeVars
noTrim fvs = fvs	-- Used at top level

trimWith :: [Name] -> FreeVars -> FreeVars
-- Nested bindings; trim by intersection with the names bound here
trimWith bndrs = intersectNameSet (mkNameSet bndrs)

---------------------
rnBind :: (Name -> [Name])		-- Signature tyvar function
       -> (FreeVars -> FreeVars)	-- Trimming function for rhs free vars
       -> LHsBind RdrName
       -> RnM (LHsBind Name, [Name], Uses)
rnBind sig_fn trim (L loc (PatBind { pat_lhs = pat, pat_rhs = grhss }))
  = setSrcSpan loc $ 
    do	{ (pat', pat_fvs) <- rnLPat pat

	; let bndrs = collectPatBinders pat'

	; (grhss', fvs) <- rnGRHSs PatBindRhs grhss
		-- No scoped type variables for pattern bindings

	; return (L loc (PatBind { pat_lhs = pat', pat_rhs = grhss', 
				   pat_rhs_ty = placeHolderType, bind_fvs = trim fvs }), 
		  bndrs, pat_fvs `plusFV` fvs) }

rnBind sig_fn trim (L loc (FunBind { fun_id = name, fun_infix = inf, fun_matches = matches }))
  = setSrcSpan loc $ 
    do	{ new_name <- lookupLocatedBndrRn name
	; let plain_name = unLoc new_name

	; (matches', fvs) <- bindSigTyVarsFV (sig_fn plain_name) $
				-- bindSigTyVars tests for Opt_ScopedTyVars
			     rnMatchGroup (FunRhs plain_name inf) matches

	; checkPrecMatch inf plain_name matches'

	; return (L loc (FunBind { fun_id = new_name, fun_infix = inf, fun_matches = matches',
				   bind_fvs = trim fvs, fun_co_fn = idHsWrapper, fun_tick = Nothing }), 
		  [plain_name], fvs)
      }
\end{code}


@rnMethodBinds@ is used for the method bindings of a class and an instance
declaration.   Like @rnBinds@ but without dependency analysis.

NOTA BENE: we record each {\em binder} of a method-bind group as a free variable.
That's crucial when dealing with an instance decl:
\begin{verbatim}
	instance Foo (T a) where
	   op x = ...
\end{verbatim}
This might be the {\em sole} occurrence of @op@ for an imported class @Foo@,
and unless @op@ occurs we won't treat the type signature of @op@ in the class
decl for @Foo@ as a source of instance-decl gates.  But we should!  Indeed,
in many ways the @op@ in an instance decl is just like an occurrence, not
a binder.

\begin{code}
rnMethodBinds :: Name			-- Class name
	      -> (Name -> [Name])	-- Signature tyvar function
	      -> [Name]			-- Names for generic type variables
	      -> LHsBinds RdrName
	      -> RnM (LHsBinds Name, FreeVars)

rnMethodBinds cls sig_fn gen_tyvars binds
  = foldM do_one (emptyBag,emptyFVs) (bagToList binds)
  where do_one (binds,fvs) bind = do
	   (bind', fvs_bind) <- rnMethodBind cls sig_fn gen_tyvars bind
	   return (bind' `unionBags` binds, fvs_bind `plusFV` fvs)

rnMethodBind cls sig_fn gen_tyvars (L loc (FunBind { fun_id = name, fun_infix = inf, 
					             fun_matches = MatchGroup matches _ }))
  = setSrcSpan loc $ 
    lookupInstDeclBndr cls name			`thenM` \ sel_name -> 
    let plain_name = unLoc sel_name in
	-- We use the selector name as the binder

    bindSigTyVarsFV (sig_fn plain_name)			$
    mapFvRn (rn_match plain_name) matches		`thenM` \ (new_matches, fvs) ->
    let 
	new_group = MatchGroup new_matches placeHolderType
    in
    checkPrecMatch inf plain_name new_group		`thenM_`
    returnM (unitBag (L loc (FunBind { 
				fun_id = sel_name, fun_infix = inf, 
				fun_matches = new_group,
				bind_fvs = fvs, fun_co_fn = idHsWrapper,
				fun_tick = Nothing })), 
	     fvs `addOneFV` plain_name)
	-- The 'fvs' field isn't used for method binds
  where
	-- Truly gruesome; bring into scope the correct members of the generic 
	-- type variables.  See comments in RnSource.rnSourceDecl(ClassDecl)
    rn_match sel_name match@(L _ (Match (L _ (TypePat ty) : _) _ _))
	= extendTyVarEnvFVRn gen_tvs 	$
	  rnMatch (FunRhs sel_name inf) match
	where
	  tvs     = map (rdrNameOcc.unLoc) (extractHsTyRdrTyVars ty)
	  gen_tvs = [tv | tv <- gen_tyvars, nameOccName tv `elem` tvs] 

    rn_match sel_name match = rnMatch (FunRhs sel_name inf) match


-- Can't handle method pattern-bindings which bind multiple methods.
rnMethodBind cls sig_fn gen_tyvars mbind@(L loc (PatBind other_pat _ _ _))
  = addLocErr mbind methodBindErr	`thenM_`
    returnM (emptyBag, emptyFVs) 
\end{code}



%************************************************************************
%*									*
\subsubsection[dep-Sigs]{Signatures (and user-pragmas for values)}
%*									*
%************************************************************************

@renameSigs@ checks for:
\begin{enumerate}
\item more than one sig for one thing;
\item signatures given for things not bound here;
\item with suitably flaggery, that all top-level things have type signatures.
\end{enumerate}
%
At the moment we don't gather free-var info from the types in
signatures.  We'd only need this if we wanted to report unused tyvars.

\begin{code}
renameSigs :: (LSig Name -> Bool) -> [LSig RdrName] -> RnM [LSig Name]
-- Renames the signatures and performs error checks
renameSigs ok_sig sigs 
  = do	{ sigs' <- rename_sigs sigs
	; check_sigs ok_sig sigs'
	; return sigs' }

----------------------
rename_sigs :: [LSig RdrName] -> RnM [LSig Name]
rename_sigs sigs = mappM (wrapLocM renameSig)
			 (filter (not . isFixityLSig) sigs)
		-- Remove fixity sigs which have been dealt with already

----------------------
check_sigs :: (LSig Name -> Bool) -> [LSig Name] -> RnM ()
-- Used for class and instance decls, as well as regular bindings
check_sigs ok_sig sigs 
	-- Check for (a) duplicate signatures
	--	     (b) signatures for things not in this group
  = do	{ mappM_ unknownSigErr (filter (not . ok_sig) sigs')
	; mappM_ dupSigDeclErr (findDupsEq eqHsSig sigs') }
  where
	-- Don't complain about an unbound name again
    sigs' = filterOut bad_name sigs
    bad_name sig = case sigName sig of
			Just n -> isUnboundName n
			other  -> False

-- We use lookupLocatedSigOccRn in the signatures, which is a little bit unsatisfactory
-- because this won't work for:
--	instance Foo T where
--	  {-# INLINE op #-}
--	  Baz.op = ...
-- We'll just rename the INLINE prag to refer to whatever other 'op'
-- is in scope.  (I'm assuming that Baz.op isn't in scope unqualified.)
-- Doesn't seem worth much trouble to sort this.

renameSig :: Sig RdrName -> RnM (Sig Name)
-- FixitSig is renamed elsewhere.
renameSig (TypeSig v ty)
  = lookupLocatedSigOccRn v			`thenM` \ new_v ->
    rnHsSigType (quotes (ppr v)) ty		`thenM` \ new_ty ->
    returnM (TypeSig new_v new_ty)

renameSig (SpecInstSig ty)
  = rnLHsType (text "A SPECIALISE instance pragma") ty `thenM` \ new_ty ->
    returnM (SpecInstSig new_ty)

renameSig (SpecSig v ty inl)
  = lookupLocatedSigOccRn v		`thenM` \ new_v ->
    rnHsSigType (quotes (ppr v)) ty	`thenM` \ new_ty ->
    returnM (SpecSig new_v new_ty inl)

renameSig (InlineSig v s)
  = lookupLocatedSigOccRn v		`thenM` \ new_v ->
    returnM (InlineSig new_v s)
\end{code}


************************************************************************
*									*
\subsection{Match}
*									*
************************************************************************

\begin{code}
rnMatchGroup :: HsMatchContext Name -> MatchGroup RdrName -> RnM (MatchGroup Name, FreeVars)
rnMatchGroup ctxt (MatchGroup ms _)
  = mapFvRn (rnMatch ctxt) ms	`thenM` \ (new_ms, ms_fvs) ->
    returnM (MatchGroup new_ms placeHolderType, ms_fvs)

rnMatch :: HsMatchContext Name -> LMatch RdrName -> RnM (LMatch Name, FreeVars)
rnMatch ctxt  = wrapLocFstM (rnMatch' ctxt)

rnMatch' ctxt match@(Match pats maybe_rhs_sig grhss)
  = 
	-- Deal with the rhs type signature
    bindPatSigTyVarsFV rhs_sig_tys	$ 
    doptM Opt_PatternSignatures `thenM` \ opt_PatternSignatures ->
    (case maybe_rhs_sig of
	Nothing -> returnM (Nothing, emptyFVs)
	Just ty | opt_PatternSignatures -> rnHsTypeFVs doc_sig ty	`thenM` \ (ty', ty_fvs) ->
				     returnM (Just ty', ty_fvs)
		| otherwise	  -> addLocErr ty patSigErr	`thenM_`
				     returnM (Nothing, emptyFVs)
    )					`thenM` \ (maybe_rhs_sig', ty_fvs) ->

	-- Now the main event
    rnPatsAndThen ctxt pats	$ \ pats' ->
    rnGRHSs ctxt grhss		`thenM` \ (grhss', grhss_fvs) ->

    returnM (Match pats' maybe_rhs_sig' grhss', grhss_fvs `plusFV` ty_fvs)
	-- The bindPatSigTyVarsFV and rnPatsAndThen will remove the bound FVs
  where
     rhs_sig_tys =  case maybe_rhs_sig of
			Nothing -> []
			Just ty -> [ty]
     doc_sig = text "In a result type-signature"
\end{code}


%************************************************************************
%*									*
\subsubsection{Guarded right-hand sides (GRHSs)}
%*									*
%************************************************************************

\begin{code}
rnGRHSs :: HsMatchContext Name -> GRHSs RdrName -> RnM (GRHSs Name, FreeVars)

rnGRHSs ctxt (GRHSs grhss binds)
  = rnLocalBindsAndThen binds	$ \ binds' ->
    mapFvRn (rnGRHS ctxt) grhss	`thenM` \ (grhss', fvGRHSs) ->
    returnM (GRHSs grhss' binds', fvGRHSs)

rnGRHS :: HsMatchContext Name -> LGRHS RdrName -> RnM (LGRHS Name, FreeVars)
rnGRHS ctxt = wrapLocFstM (rnGRHS' ctxt)

rnGRHS' ctxt (GRHS guards rhs)
  = do	{ pattern_guards_allowed <- doptM Opt_PatternGuards
	; ((guards', rhs'), fvs) <- rnStmts (PatGuard ctxt) guards $
				    rnLExpr rhs

	; checkM (pattern_guards_allowed || is_standard_guard guards')
	  	 (addWarn (nonStdGuardErr guards'))

	; return (GRHS guards' rhs', fvs) }
  where
	-- Standard Haskell 1.4 guards are just a single boolean
	-- expression, rather than a list of qualifiers as in the
	-- Glasgow extension
    is_standard_guard []                     = True
    is_standard_guard [L _ (ExprStmt _ _ _)] = True
    is_standard_guard other	      	     = False
\end{code}

%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
dupSigDeclErr sigs@(L loc sig : _)
  = addErrAt loc $
	vcat [ptext SLIT("Duplicate") <+> what_it_is <> colon,
	      nest 2 (vcat (map ppr_sig sigs))]
  where
    what_it_is = hsSigDoc sig
    ppr_sig (L loc sig) = ppr loc <> colon <+> ppr sig

unknownSigErr (L loc sig)
  = do	{ mod <- getModule
	; addErrAt loc $
		vcat [sep [ptext SLIT("Misplaced") <+> what_it_is <> colon, ppr sig],
		      extra_stuff mod sig] }
  where
    what_it_is = hsSigDoc sig
    extra_stuff mod  (TypeSig (L _ n) _)
	| nameIsLocalOrFrom mod n
	= ptext SLIT("The type signature must be given where")
		<+> quotes (ppr n) <+> ptext SLIT("is declared")
	| otherwise
	= ptext SLIT("You cannot give a type signature for an imported value")

    extra_stuff mod other = empty

methodBindErr mbind
 =  hang (ptext SLIT("Pattern bindings (except simple variables) not allowed in instance declarations"))
       2 (ppr mbind)

bindsInHsBootFile mbinds
  = hang (ptext SLIT("Bindings in hs-boot files are not allowed"))
       2 (ppr mbinds)

nonStdGuardErr guards
  = hang (ptext SLIT("accepting non-standard pattern guards (use -XPatternGuards to suppress this message)"))
       4 (interpp'SP guards)
\end{code}
