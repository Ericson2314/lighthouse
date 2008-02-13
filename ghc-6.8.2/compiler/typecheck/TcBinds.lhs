%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcBinds]{TcBinds}

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module TcBinds ( tcLocalBinds, tcTopBinds, 
		 tcHsBootSigs, tcMonoBinds, 
		 TcPragFun, tcSpecPrag, tcPrags, mkPragFun, 
		 TcSigInfo(..), TcSigFun, mkTcSigFun,
		 badBootDeclErr ) where

#include "HsVersions.h"

import {-# SOURCE #-} TcMatches ( tcGRHSsPat, tcMatchesFun )
import {-# SOURCE #-} TcExpr  ( tcMonoExpr )

import DynFlags
import HsSyn
import TcHsSyn

import TcRnMonad
import Inst
import TcEnv
import TcUnify
import TcSimplify
import TcHsType
import TcPat
import TcMType
import TcType
import {- Kind parts of -} Type
import Coercion
import VarEnv
import TysPrim
import Id
import IdInfo
import Var ( TyVar, varType )
import Name
import NameSet
import NameEnv
import VarSet
import SrcLoc
import Bag
import ErrUtils
import Digraph
import Maybes
import List
import Util
import BasicTypes
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{Type-checking bindings}
%*									*
%************************************************************************

@tcBindsAndThen@ typechecks a @HsBinds@.  The "and then" part is because
it needs to know something about the {\em usage} of the things bound,
so that it can create specialisations of them.  So @tcBindsAndThen@
takes a function which, given an extended environment, E, typechecks
the scope of the bindings returning a typechecked thing and (most
important) an LIE.  It is this LIE which is then used as the basis for
specialising the things bound.

@tcBindsAndThen@ also takes a "combiner" which glues together the
bindings and the "thing" to make a new "thing".

The real work is done by @tcBindWithSigsAndThen@.

Recursive and non-recursive binds are handled in essentially the same
way: because of uniques there are no scoping issues left.  The only
difference is that non-recursive bindings can bind primitive values.

Even for non-recursive binding groups we add typings for each binder
to the LVE for the following reason.  When each individual binding is
checked the type of its LHS is unified with that of its RHS; and
type-checking the LHS of course requires that the binder is in scope.

At the top-level the LIE is sure to contain nothing but constant
dictionaries, which we resolve at the module level.

\begin{code}
tcTopBinds :: HsValBinds Name -> TcM (LHsBinds TcId, TcLclEnv)
	-- Note: returning the TcLclEnv is more than we really
	--       want.  The bit we care about is the local bindings
	--	 and the free type variables thereof
tcTopBinds binds
  = do	{ (ValBindsOut prs _, env) <- tcValBinds TopLevel binds getLclEnv
	; return (foldr (unionBags . snd) emptyBag prs, env) }
	-- The top level bindings are flattened into a giant 
	-- implicitly-mutually-recursive LHsBinds

tcHsBootSigs :: HsValBinds Name -> TcM [Id]
-- A hs-boot file has only one BindGroup, and it only has type
-- signatures in it.  The renamer checked all this
tcHsBootSigs (ValBindsOut binds sigs)
  = do	{ checkTc (null binds) badBootDeclErr
	; mapM (addLocM tc_boot_sig) (filter isVanillaLSig sigs) }
  where
    tc_boot_sig (TypeSig (L _ name) ty)
      = do { sigma_ty <- tcHsSigType (FunSigCtxt name) ty
	   ; return (mkVanillaGlobal name sigma_ty vanillaIdInfo) }
	-- Notice that we make GlobalIds, not LocalIds
tcHsBootSigs groups = pprPanic "tcHsBootSigs" (ppr groups)

badBootDeclErr :: Message
badBootDeclErr = ptext SLIT("Illegal declarations in an hs-boot file")

------------------------
tcLocalBinds :: HsLocalBinds Name -> TcM thing
	     -> TcM (HsLocalBinds TcId, thing)

tcLocalBinds EmptyLocalBinds thing_inside 
  = do	{ thing <- thing_inside
	; return (EmptyLocalBinds, thing) }

tcLocalBinds (HsValBinds binds) thing_inside
  = do	{ (binds', thing) <- tcValBinds NotTopLevel binds thing_inside
	; return (HsValBinds binds', thing) }

tcLocalBinds (HsIPBinds (IPBinds ip_binds _)) thing_inside
  = do	{ (thing, lie) <- getLIE thing_inside
	; (avail_ips, ip_binds') <- mapAndUnzipM (wrapLocSndM tc_ip_bind) ip_binds

	-- If the binding binds ?x = E, we  must now 
	-- discharge any ?x constraints in expr_lie
	; dict_binds <- tcSimplifyIPs avail_ips lie
	; return (HsIPBinds (IPBinds ip_binds' dict_binds), thing) }
  where
	-- I wonder if we should do these one at at time
	-- Consider	?x = 4
	--		?y = ?x + 1
    tc_ip_bind (IPBind ip expr)
      = newFlexiTyVarTy argTypeKind		`thenM` \ ty ->
  	newIPDict (IPBindOrigin ip) ip ty	`thenM` \ (ip', ip_inst) ->
  	tcMonoExpr expr ty			`thenM` \ expr' ->
  	returnM (ip_inst, (IPBind ip' expr'))

------------------------
tcValBinds :: TopLevelFlag 
	   -> HsValBinds Name -> TcM thing
	   -> TcM (HsValBinds TcId, thing) 

tcValBinds top_lvl (ValBindsIn binds sigs) thing_inside
  = pprPanic "tcValBinds" (ppr binds)

tcValBinds top_lvl (ValBindsOut binds sigs) thing_inside
  = do 	{   	-- Typecheck the signature
	; let { prag_fn = mkPragFun sigs
	      ; ty_sigs = filter isVanillaLSig sigs
	      ; sig_fn  = mkTcSigFun ty_sigs }

	; poly_ids <- mapM tcTySig ty_sigs
		-- No recovery from bad signatures, because the type sigs
		-- may bind type variables, so proceeding without them
		-- can lead to a cascade of errors
		-- ToDo: this means we fall over immediately if any type sig
		-- is wrong, which is over-conservative, see Trac bug #745

		-- Extend the envt right away with all 
		-- the Ids declared with type signatures
	; poly_rec <- doptM Opt_RelaxedPolyRec
  	; (binds', thing) <- tcExtendIdEnv poly_ids $
			     tc_val_binds poly_rec top_lvl sig_fn prag_fn 
					  binds thing_inside

	; return (ValBindsOut binds' sigs, thing) }

------------------------
tc_val_binds :: Bool -> TopLevelFlag -> TcSigFun -> TcPragFun
	     -> [(RecFlag, LHsBinds Name)] -> TcM thing
	     -> TcM ([(RecFlag, LHsBinds TcId)], thing)
-- Typecheck a whole lot of value bindings,
-- one strongly-connected component at a time

tc_val_binds poly_rec top_lvl sig_fn prag_fn [] thing_inside
  = do	{ thing <- thing_inside
	; return ([], thing) }

tc_val_binds poly_rec top_lvl sig_fn prag_fn (group : groups) thing_inside
  = do	{ (group', (groups', thing))
		<- tc_group poly_rec top_lvl sig_fn prag_fn group $ 
		   tc_val_binds poly_rec top_lvl sig_fn prag_fn groups thing_inside
	; return (group' ++ groups', thing) }

------------------------
tc_group :: Bool -> TopLevelFlag -> TcSigFun -> TcPragFun
	 -> (RecFlag, LHsBinds Name) -> TcM thing
 	 -> TcM ([(RecFlag, LHsBinds TcId)], thing)

-- Typecheck one strongly-connected component of the original program.
-- We get a list of groups back, because there may 
-- be specialisations etc as well

tc_group poly_rec top_lvl sig_fn prag_fn (NonRecursive, binds) thing_inside
    	-- A single non-recursive binding
     	-- We want to keep non-recursive things non-recursive
        -- so that we desugar unlifted bindings correctly
 =  do	{ (binds, thing) <- tc_haskell98 top_lvl sig_fn prag_fn NonRecursive binds thing_inside
	; return ([(NonRecursive, b) | b <- binds], thing) }

tc_group poly_rec top_lvl sig_fn prag_fn (Recursive, binds) thing_inside
  | not poly_rec	-- Recursive group, normal Haskell 98 route
  = do	{ (binds1, thing) <- tc_haskell98 top_lvl sig_fn prag_fn Recursive binds thing_inside
	; return ([(Recursive, unionManyBags binds1)], thing) }

  | otherwise		-- Recursive group, with gla-exts
  =	-- To maximise polymorphism (with -fglasgow-exts), we do a new 
	-- strongly-connected-component analysis, this time omitting 
	-- any references to variables with type signatures.
	--
	-- Notice that the bindInsts thing covers *all* the bindings in the original
	-- group at once; an earlier one may use a later one!
    do	{ traceTc (text "tc_group rec" <+> pprLHsBinds binds)
	; (binds1,thing) <- bindLocalInsts top_lvl $
			    go (stronglyConnComp (mkEdges sig_fn binds))
	; return ([(Recursive, unionManyBags binds1)], thing) }
		-- Rec them all together
  where
--  go :: SCC (LHsBind Name) -> TcM ([LHsBind TcId], [TcId], thing)
    go (scc:sccs) = do	{ (binds1, ids1) <- tc_scc scc
			; (binds2, ids2, thing) <- tcExtendIdEnv ids1 $ go sccs
			; return (binds1 ++ binds2, ids1 ++ ids2, thing) }
    go [] 	  = do	{ thing <- thing_inside; return ([], [], thing) }

    tc_scc (AcyclicSCC bind) = tc_sub_group NonRecursive (unitBag bind)
    tc_scc (CyclicSCC binds) = tc_sub_group Recursive    (listToBag binds)

    tc_sub_group = tcPolyBinds top_lvl sig_fn prag_fn Recursive

tc_haskell98 top_lvl sig_fn prag_fn rec_flag binds thing_inside
  = bindLocalInsts top_lvl $ do
    { (binds1, ids) <- tcPolyBinds top_lvl sig_fn prag_fn rec_flag rec_flag binds
    ; thing <- tcExtendIdEnv ids thing_inside
    ; return (binds1, ids, thing) }

------------------------
bindLocalInsts :: TopLevelFlag -> TcM ([LHsBinds TcId], [TcId], a) -> TcM ([LHsBinds TcId], a)
bindLocalInsts top_lvl thing_inside
  | isTopLevel top_lvl = do { (binds, ids, thing) <- thing_inside; return (binds, thing) }
  	-- For the top level don't bother with all this bindInstsOfLocalFuns stuff. 
	-- All the top level things are rec'd together anyway, so it's fine to
	-- leave them to the tcSimplifyTop, and quite a bit faster too

  | otherwise	-- Nested case
  = do	{ ((binds, ids, thing), lie) <- getLIE thing_inside
	; lie_binds <- bindInstsOfLocalFuns lie ids
	; return (binds ++ [lie_binds], thing) }

------------------------
mkEdges :: TcSigFun -> LHsBinds Name
	-> [(LHsBind Name, BKey, [BKey])]

type BKey  = Int -- Just number off the bindings

mkEdges sig_fn binds
  = [ (bind, key, [key | n <- nameSetToList (bind_fvs (unLoc bind)),
			 Just key <- [lookupNameEnv key_map n], no_sig n ])
    | (bind, key) <- keyd_binds
    ]
  where
    no_sig :: Name -> Bool
    no_sig n = isNothing (sig_fn n)

    keyd_binds = bagToList binds `zip` [0::BKey ..]

    key_map :: NameEnv BKey	-- Which binding it comes from
    key_map = mkNameEnv [(bndr, key) | (L _ bind, key) <- keyd_binds
				     , bndr <- bindersOfHsBind bind ]

bindersOfHsBind :: HsBind Name -> [Name]
bindersOfHsBind (PatBind { pat_lhs = pat })  = collectPatBinders pat
bindersOfHsBind (FunBind { fun_id = L _ f }) = [f]

------------------------
tcPolyBinds :: TopLevelFlag -> TcSigFun -> TcPragFun
	    -> RecFlag			-- Whether the group is really recursive
	    -> RecFlag			-- Whether it's recursive after breaking
					-- dependencies based on type signatures
	    -> LHsBinds Name
	    -> TcM ([LHsBinds TcId], [TcId])

-- Typechecks a single bunch of bindings all together, 
-- and generalises them.  The bunch may be only part of a recursive
-- group, because we use type signatures to maximise polymorphism
--
-- Returns a list because the input may be a single non-recursive binding,
-- in which case the dependency order of the resulting bindings is
-- important.  
-- 
-- Knows nothing about the scope of the bindings

tcPolyBinds top_lvl sig_fn prag_fn rec_group rec_tc binds
  = let 
	bind_list    = bagToList binds
        binder_names = collectHsBindBinders binds
	loc          = getLoc (head bind_list)
		-- TODO: location a bit awkward, but the mbinds have been
		--	 dependency analysed and may no longer be adjacent
    in
	-- SET UP THE MAIN RECOVERY; take advantage of any type sigs
    setSrcSpan loc				$
    recoverM (recoveryCode binder_names sig_fn)	$ do 

  { traceTc (ptext SLIT("------------------------------------------------"))
  ; traceTc (ptext SLIT("Bindings for") <+> ppr binder_names)

   	-- TYPECHECK THE BINDINGS
  ; ((binds', mono_bind_infos), lie_req) 
	<- getLIE (tcMonoBinds bind_list sig_fn rec_tc)
  ; traceTc (text "temp" <+> (ppr binds' $$ ppr lie_req))

	-- CHECK FOR UNLIFTED BINDINGS
	-- These must be non-recursive etc, and are not generalised
	-- They desugar to a case expression in the end
  ; zonked_mono_tys <- zonkTcTypes (map getMonoType mono_bind_infos)
  ; is_strict <- checkStrictBinds top_lvl rec_group binds' 
				  zonked_mono_tys mono_bind_infos
  ; if is_strict then
    do	{ extendLIEs lie_req
	; let exports = zipWith mk_export mono_bind_infos zonked_mono_tys
	      mk_export (name, Nothing,  mono_id) mono_ty = ([], mkLocalId name mono_ty, mono_id, [])
	      mk_export (name, Just sig, mono_id) mono_ty = ([], sig_id sig,             mono_id, [])
			-- ToDo: prags for unlifted bindings

	; return ( [unitBag $ L loc $ AbsBinds [] [] exports binds'],
		   [poly_id | (_, poly_id, _, _) <- exports]) }	-- Guaranteed zonked

    else do	-- The normal lifted case: GENERALISE
  { dflags <- getDOpts 
  ; (tyvars_to_gen, dicts, dict_binds)
	<- addErrCtxt (genCtxt (bndrNames mono_bind_infos)) $
	   generalise dflags top_lvl bind_list sig_fn mono_bind_infos lie_req

	-- BUILD THE POLYMORPHIC RESULT IDs
  ; let dict_vars = map instToVar dicts	-- May include equality constraints
  ; exports <- mapM (mkExport top_lvl prag_fn tyvars_to_gen (map varType dict_vars))
		    mono_bind_infos

  ; let	poly_ids = [poly_id | (_, poly_id, _, _) <- exports]
  ; traceTc (text "binding:" <+> ppr (poly_ids `zip` map idType poly_ids))

  ; let abs_bind = L loc $ AbsBinds tyvars_to_gen
	 		            dict_vars exports
	 		    	    (dict_binds `unionBags` binds')

  ; return ([unitBag abs_bind], poly_ids)	-- poly_ids are guaranteed zonked by mkExport
  } }


--------------
mkExport :: TopLevelFlag -> TcPragFun -> [TyVar] -> [TcType]
	 -> MonoBindInfo
	 -> TcM ([TyVar], Id, Id, [LPrag])
-- mkExport generates exports with 
--	zonked type variables, 
--	zonked poly_ids
-- The former is just because no further unifications will change
-- the quantified type variables, so we can fix their final form
-- right now.
-- The latter is needed because the poly_ids are used to extend the
-- type environment; see the invariant on TcEnv.tcExtendIdEnv 

-- Pre-condition: the inferred_tvs are already zonked

mkExport top_lvl prag_fn inferred_tvs dict_tys (poly_name, mb_sig, mono_id)
  = do	{ warn_missing_sigs <- doptM Opt_WarnMissingSigs
	; let warn = isTopLevel top_lvl && warn_missing_sigs
	; (tvs, poly_id) <- mk_poly_id warn mb_sig
		-- poly_id has a zonked type

	; prags <- tcPrags poly_id (prag_fn poly_name)
		-- tcPrags requires a zonked poly_id

	; return (tvs, poly_id, mono_id, prags) }
  where
    poly_ty = mkForAllTys inferred_tvs (mkFunTys dict_tys (idType mono_id))

    mk_poly_id warn Nothing    = do { poly_ty' <- zonkTcType poly_ty
				    ; missingSigWarn warn poly_name poly_ty'
				    ; return (inferred_tvs, mkLocalId poly_name poly_ty') }
    mk_poly_id warn (Just sig) = do { tvs <- mapM zonk_tv (sig_tvs sig)
			            ; return (tvs,  sig_id sig) }

    zonk_tv tv = do { ty <- zonkTcTyVar tv; return (tcGetTyVar "mkExport" ty) }

------------------------
type TcPragFun = Name -> [LSig Name]

mkPragFun :: [LSig Name] -> TcPragFun
mkPragFun sigs = \n -> lookupNameEnv env n `orElse` []
	where
	  prs = [(expectJust "mkPragFun" (sigName sig), sig) 
		| sig <- sigs, isPragLSig sig]
	  env = foldl add emptyNameEnv prs
	  add env (n,p) = extendNameEnv_Acc (:) singleton env n p

tcPrags :: Id -> [LSig Name] -> TcM [LPrag]
tcPrags poly_id prags = mapM (wrapLocM tc_prag) prags
  where
    tc_prag prag = addErrCtxt (pragSigCtxt prag) $ 
		   tcPrag poly_id prag

pragSigCtxt prag = hang (ptext SLIT("In the pragma")) 2 (ppr prag)

tcPrag :: TcId -> Sig Name -> TcM Prag
-- Pre-condition: the poly_id is zonked
-- Reason: required by tcSubExp
tcPrag poly_id (SpecSig orig_name hs_ty inl) = tcSpecPrag poly_id hs_ty inl
tcPrag poly_id (SpecInstSig hs_ty)	     = tcSpecPrag poly_id hs_ty defaultInlineSpec
tcPrag poly_id (InlineSig v inl)             = return (InlinePrag inl)


tcSpecPrag :: TcId -> LHsType Name -> InlineSpec -> TcM Prag
tcSpecPrag poly_id hs_ty inl
  = do	{ spec_ty <- tcHsSigType (FunSigCtxt (idName poly_id)) hs_ty
	; (co_fn, lie) <- getLIE (tcSubExp (idType poly_id) spec_ty)
	; extendLIEs lie
	; let const_dicts = map instToId lie
	; return (SpecPrag (mkHsWrap co_fn (HsVar poly_id)) spec_ty const_dicts inl) }
	-- Most of the work of specialisation is done by 
	-- the desugarer, guided by the SpecPrag
  
--------------
-- If typechecking the binds fails, then return with each
-- signature-less binder given type (forall a.a), to minimise 
-- subsequent error messages
recoveryCode binder_names sig_fn
  = do	{ traceTc (text "tcBindsWithSigs: error recovery" <+> ppr binder_names)
	; poly_ids <- mapM mk_dummy binder_names
	; return ([], poly_ids) }
  where
    mk_dummy name 
	| isJust (sig_fn name) = tcLookupId name	-- Had signature; look it up
	| otherwise	       = return (mkLocalId name forall_a_a)    -- No signature

forall_a_a :: TcType
forall_a_a = mkForAllTy alphaTyVar (mkTyVarTy alphaTyVar)


-- Check that non-overloaded unlifted bindings are
-- 	a) non-recursive,
--	b) not top level, 
--	c) not a multiple-binding group (more or less implied by (a))

checkStrictBinds :: TopLevelFlag -> RecFlag
		 -> LHsBinds TcId -> [TcType] -> [MonoBindInfo]
		 -> TcM Bool
checkStrictBinds top_lvl rec_group mbind mono_tys infos
  | unlifted || bang_pat
  = do 	{ checkTc (isNotTopLevel top_lvl)
	  	  (strictBindErr "Top-level" unlifted mbind)
	; checkTc (isNonRec rec_group)
	  	  (strictBindErr "Recursive" unlifted mbind)
	; checkTc (isSingletonBag mbind)
	    	  (strictBindErr "Multiple" unlifted mbind) 
	; mapM_ check_sig infos
	; return True }
  | otherwise
  = return False
  where
    unlifted = any isUnLiftedType mono_tys
    bang_pat = anyBag (isBangHsBind . unLoc) mbind
    check_sig (_, Just sig, _) = checkTc (null (sig_tvs sig) && null (sig_theta sig))
					 (badStrictSig unlifted sig)
    check_sig other	       = return ()

strictBindErr flavour unlifted mbind
  = hang (text flavour <+> msg <+> ptext SLIT("aren't allowed:")) 
	 4 (pprLHsBinds mbind)
  where
    msg | unlifted  = ptext SLIT("bindings for unlifted types")
	| otherwise = ptext SLIT("bang-pattern bindings")

badStrictSig unlifted sig
  = hang (ptext SLIT("Illegal polymorphic signature in") <+> msg)
	 4 (ppr sig)
  where
    msg | unlifted  = ptext SLIT("an unlifted binding")
	| otherwise = ptext SLIT("a bang-pattern binding")
\end{code}


%************************************************************************
%*									*
\subsection{tcMonoBind}
%*									*
%************************************************************************

@tcMonoBinds@ deals with a perhaps-recursive group of HsBinds.
The signatures have been dealt with already.

\begin{code}
tcMonoBinds :: [LHsBind Name]
	    -> TcSigFun
	    -> RecFlag	-- Whether the binding is recursive for typechecking purposes
			-- i.e. the binders are mentioned in their RHSs, and
			--	we are not resuced by a type signature
	    -> TcM (LHsBinds TcId, [MonoBindInfo])

tcMonoBinds [L b_loc (FunBind { fun_id = L nm_loc name, fun_infix = inf, 
				fun_matches = matches, bind_fvs = fvs })]
	    sig_fn 		-- Single function binding,
	    NonRecursive	-- binder isn't mentioned in RHS,
  | Nothing <- sig_fn name	-- ...with no type signature
  = 	-- In this very special case we infer the type of the
	-- right hand side first (it may have a higher-rank type)
	-- and *then* make the monomorphic Id for the LHS
	-- e.g.		f = \(x::forall a. a->a) -> <body>
	-- 	We want to infer a higher-rank type for f
    setSrcSpan b_loc  	$
    do	{ ((co_fn, matches'), rhs_ty) <- tcInfer (tcMatchesFun name inf matches)

		-- Check for an unboxed tuple type
		--	f = (# True, False #)
		-- Zonk first just in case it's hidden inside a meta type variable
		-- (This shows up as a (more obscure) kind error 
		--  in the 'otherwise' case of tcMonoBinds.)
	; zonked_rhs_ty <- zonkTcType rhs_ty
	; checkTc (not (isUnboxedTupleType zonked_rhs_ty))
		  (unboxedTupleErr name zonked_rhs_ty)

	; mono_name <- newLocalName name
	; let mono_id = mkLocalId mono_name zonked_rhs_ty
	; return (unitBag (L b_loc (FunBind { fun_id = L nm_loc mono_id, fun_infix = inf,
					      fun_matches = matches', bind_fvs = fvs,
					      fun_co_fn = co_fn, fun_tick = Nothing })),
		  [(name, Nothing, mono_id)]) }

tcMonoBinds [L b_loc (FunBind { fun_id = L nm_loc name, fun_infix = inf, 
				fun_matches = matches, bind_fvs = fvs })]
	    sig_fn 		-- Single function binding
	    non_rec	
  | Just scoped_tvs <- sig_fn name	-- ...with a type signature
  = 	-- When we have a single function binding, with a type signature
	-- we can (a) use genuine, rigid skolem constants for the type variables
	--	  (b) bring (rigid) scoped type variables into scope
    setSrcSpan b_loc  	$
    do	{ tc_sig <- tcInstSig True name scoped_tvs
	; mono_name <- newLocalName name
	; let mono_ty = sig_tau tc_sig
	      mono_id = mkLocalId mono_name mono_ty
	      rhs_tvs = [ (name, mkTyVarTy tv)
			| (name, tv) <- sig_scoped tc_sig `zip` sig_tvs tc_sig ]

	; (co_fn, matches') <- tcExtendTyVarEnv2 rhs_tvs    $
		    	       tcMatchesFun mono_name inf matches mono_ty

	; let fun_bind' = FunBind { fun_id = L nm_loc mono_id, 
				    fun_infix = inf, fun_matches = matches',
			            bind_fvs = placeHolderNames, fun_co_fn = co_fn, 
				    fun_tick = Nothing }
	; return (unitBag (L b_loc fun_bind'),
		  [(name, Just tc_sig, mono_id)]) }

tcMonoBinds binds sig_fn non_rec
  = do	{ tc_binds <- mapM (wrapLocM (tcLhs sig_fn)) binds

	-- Bring the monomorphic Ids, into scope for the RHSs
	; let mono_info  = getMonoBindInfo tc_binds
	      rhs_id_env = [(name,mono_id) | (name, Nothing, mono_id) <- mono_info]
			 	-- A monomorphic binding for each term variable that lacks 
				-- a type sig.  (Ones with a sig are already in scope.)

	; binds' <- tcExtendIdEnv2    rhs_id_env $
		    traceTc (text "tcMonoBinds" <+> vcat [ ppr n <+> ppr id <+> ppr (idType id) 
							 | (n,id) <- rhs_id_env]) `thenM_`
		    mapM (wrapLocM tcRhs) tc_binds
	; return (listToBag binds', mono_info) }

------------------------
-- tcLhs typechecks the LHS of the bindings, to construct the environment in which
-- we typecheck the RHSs.  Basically what we are doing is this: for each binder:
--	if there's a signature for it, use the instantiated signature type
--	otherwise invent a type variable
-- You see that quite directly in the FunBind case.
-- 
-- But there's a complication for pattern bindings:
--	data T = MkT (forall a. a->a)
--	MkT f = e
-- Here we can guess a type variable for the entire LHS (which will be refined to T)
-- but we want to get (f::forall a. a->a) as the RHS environment.
-- The simplest way to do this is to typecheck the pattern, and then look up the
-- bound mono-ids.  Then we want to retain the typechecked pattern to avoid re-doing
-- it; hence the TcMonoBind data type in which the LHS is done but the RHS isn't

data TcMonoBind		-- Half completed; LHS done, RHS not done
  = TcFunBind  MonoBindInfo  (Located TcId) Bool (MatchGroup Name) 
  | TcPatBind [MonoBindInfo] (LPat TcId) (GRHSs Name) TcSigmaType

type MonoBindInfo = (Name, Maybe TcSigInfo, TcId)
	-- Type signature (if any), and
	-- the monomorphic bound things

bndrNames :: [MonoBindInfo] -> [Name]
bndrNames mbi = [n | (n,_,_) <- mbi]

getMonoType :: MonoBindInfo -> TcTauType
getMonoType (_,_,mono_id) = idType mono_id

tcLhs :: TcSigFun -> HsBind Name -> TcM TcMonoBind
tcLhs sig_fn (FunBind { fun_id = L nm_loc name, fun_infix = inf, fun_matches = matches })
  = do	{ mb_sig <- tcInstSig_maybe sig_fn name
	; mono_name <- newLocalName name
	; mono_ty   <- mk_mono_ty mb_sig
	; let mono_id = mkLocalId mono_name mono_ty
	; return (TcFunBind (name, mb_sig, mono_id) (L nm_loc mono_id) inf matches) }
  where
    mk_mono_ty (Just sig) = return (sig_tau sig)
    mk_mono_ty Nothing    = newFlexiTyVarTy argTypeKind

tcLhs sig_fn bind@(PatBind { pat_lhs = pat, pat_rhs = grhss })
  = do	{ mb_sigs <- mapM (tcInstSig_maybe sig_fn) names
	; mono_pat_binds <- doptM Opt_MonoPatBinds
		-- With -fmono-pat-binds, we do no generalisation of pattern bindings
		-- But the signature can still be polymoprhic!
		--	data T = MkT (forall a. a->a)
		--	x :: forall a. a->a
		--	MkT x = <rhs>
		-- The function get_sig_ty decides whether the pattern-bound variables
		-- should have exactly the type in the type signature (-fmono-pat-binds), 
		-- or the instantiated version (-fmono-pat-binds)

	; let nm_sig_prs  = names `zip` mb_sigs
	      get_sig_ty | mono_pat_binds = idType . sig_id
			 | otherwise	  = sig_tau
	      tau_sig_env = mkNameEnv [ (name, get_sig_ty sig) 
				      | (name, Just sig) <- nm_sig_prs]
	      sig_tau_fn  = lookupNameEnv tau_sig_env

	      tc_pat exp_ty = tcLetPat sig_tau_fn pat exp_ty $
			      mapM lookup_info nm_sig_prs

		-- After typechecking the pattern, look up the binder
		-- names, which the pattern has brought into scope.
	      lookup_info :: (Name, Maybe TcSigInfo) -> TcM MonoBindInfo
	      lookup_info (name, mb_sig) = do { mono_id <- tcLookupId name
					      ; return (name, mb_sig, mono_id) }

	; ((pat', infos), pat_ty) <- addErrCtxt (patMonoBindsCtxt pat grhss) $
				     tcInfer tc_pat

	; return (TcPatBind infos pat' grhss pat_ty) }
  where
    names = collectPatBinders pat


tcLhs sig_fn other_bind = pprPanic "tcLhs" (ppr other_bind)
	-- AbsBind, VarBind impossible

-------------------
tcRhs :: TcMonoBind -> TcM (HsBind TcId)
tcRhs (TcFunBind info fun'@(L _ mono_id) inf matches)
  = do	{ (co_fn, matches') <- tcMatchesFun (idName mono_id) inf 
				    	    matches (idType mono_id)
	; return (FunBind { fun_id = fun', fun_infix = inf, fun_matches = matches',
			    bind_fvs = placeHolderNames, fun_co_fn = co_fn,
			    fun_tick = Nothing }) }

tcRhs bind@(TcPatBind _ pat' grhss pat_ty)
  = do	{ grhss' <- addErrCtxt (patMonoBindsCtxt pat' grhss) $
		    tcGRHSsPat grhss pat_ty
	; return (PatBind { pat_lhs = pat', pat_rhs = grhss', pat_rhs_ty = pat_ty, 
			    bind_fvs = placeHolderNames }) }


---------------------
getMonoBindInfo :: [Located TcMonoBind] -> [MonoBindInfo]
getMonoBindInfo tc_binds
  = foldr (get_info . unLoc) [] tc_binds
  where
    get_info (TcFunBind info _ _ _)  rest = info : rest
    get_info (TcPatBind infos _ _ _) rest = infos ++ rest
\end{code}


%************************************************************************
%*									*
		Generalisation
%*									*
%************************************************************************

\begin{code}
generalise :: DynFlags -> TopLevelFlag 
	   -> [LHsBind Name] -> TcSigFun 
	   -> [MonoBindInfo] -> [Inst]
	   -> TcM ([TyVar], [Inst], TcDictBinds)
-- The returned [TyVar] are all ready to quantify

generalise dflags top_lvl bind_list sig_fn mono_infos lie_req
  | isMonoGroup dflags bind_list
  = do	{ extendLIEs lie_req
	; return ([], [], emptyBag) }

  | isRestrictedGroup dflags bind_list sig_fn 	-- RESTRICTED CASE
  = 	-- Check signature contexts are empty 
    do	{ checkTc (all is_mono_sig sigs)
	  	  (restrictedBindCtxtErr bndrs)

	-- Now simplify with exactly that set of tyvars
	-- We have to squash those Methods
	; (qtvs, binds) <- tcSimplifyRestricted doc top_lvl bndrs 
						tau_tvs lie_req

   	-- Check that signature type variables are OK
	; final_qtvs <- checkSigsTyVars qtvs sigs

	; return (final_qtvs, [], binds) }

  | null sigs	-- UNRESTRICTED CASE, NO TYPE SIGS
  = tcSimplifyInfer doc tau_tvs lie_req

  | otherwise	-- UNRESTRICTED CASE, WITH TYPE SIGS
  = do	{ sig_lie <- unifyCtxts sigs	-- sigs is non-empty; sig_lie is zonked
	; let	-- The "sig_avails" is the stuff available.  We get that from
		-- the context of the type signature, BUT ALSO the lie_avail
		-- so that polymorphic recursion works right (see Note [Polymorphic recursion])
		local_meths = [mkMethInst sig mono_id | (_, Just sig, mono_id) <- mono_infos]
		sig_avails = sig_lie ++ local_meths
		loc = sig_loc (head sigs)

	-- Check that the needed dicts can be
	-- expressed in terms of the signature ones
	; (qtvs, binds) <- tcSimplifyInferCheck loc tau_tvs sig_avails lie_req
	
   	-- Check that signature type variables are OK
	; final_qtvs <- checkSigsTyVars qtvs sigs

	; returnM (final_qtvs, sig_lie, binds) }
  where
    bndrs   = bndrNames mono_infos
    sigs    = [sig | (_, Just sig, _) <- mono_infos]
    get_tvs | isTopLevel top_lvl = tyVarsOfType	 -- See Note [Silly type synonym] in TcType
	    | otherwise		 = exactTyVarsOfType
    tau_tvs = foldr (unionVarSet . get_tvs . getMonoType) emptyVarSet mono_infos
    is_mono_sig sig = null (sig_theta sig)
    doc = ptext SLIT("type signature(s) for") <+> pprBinders bndrs

    mkMethInst (TcSigInfo { sig_id = poly_id, sig_tvs = tvs, 
		            sig_theta = theta, sig_loc = loc }) mono_id
      = Method {tci_id = mono_id, tci_oid = poly_id, tci_tys = mkTyVarTys tvs,
		tci_theta = theta, tci_loc = loc}
\end{code}

unifyCtxts checks that all the signature contexts are the same
The type signatures on a mutually-recursive group of definitions
must all have the same context (or none).

The trick here is that all the signatures should have the same
context, and we want to share type variables for that context, so that
all the right hand sides agree a common vocabulary for their type
constraints

We unify them because, with polymorphic recursion, their types
might not otherwise be related.  This is a rather subtle issue.

\begin{code}
unifyCtxts :: [TcSigInfo] -> TcM [Inst]
-- Post-condition: the returned Insts are full zonked
unifyCtxts (sig1 : sigs) 	-- Argument is always non-empty
  = do	{ mapM unify_ctxt sigs
	; theta <- zonkTcThetaType (sig_theta sig1)
	; newDictBndrs (sig_loc sig1) theta }
  where
    theta1 = sig_theta sig1
    unify_ctxt :: TcSigInfo -> TcM ()
    unify_ctxt sig@(TcSigInfo { sig_theta = theta })
	= setSrcSpan (instLocSpan (sig_loc sig)) 	$
	  addErrCtxt (sigContextsCtxt sig1 sig)		$
	  do { cois <- unifyTheta theta1 theta
	     ; -- Check whether all coercions are identity coercions
	       -- That can happen if we have, say
	       -- 	  f :: C [a]   => ...
	       -- 	  g :: C (F a) => ...
	       -- where F is a type function and (F a ~ [a])
	       -- Then unification might succeed with a coercion.  But it's much
	       -- much simpler to require that such signatures have identical contexts
	       checkTc (all isIdentityCoercion cois)
		       (ptext SLIT("Mutually dependent functions have syntactically distinct contexts"))
	     }

checkSigsTyVars :: [TcTyVar] -> [TcSigInfo] -> TcM [TcTyVar]
checkSigsTyVars qtvs sigs 
  = do	{ gbl_tvs <- tcGetGlobalTyVars
	; sig_tvs_s <- mappM (check_sig gbl_tvs) sigs

	; let	-- Sigh.  Make sure that all the tyvars in the type sigs
		-- appear in the returned ty var list, which is what we are
		-- going to generalise over.  Reason: we occasionally get
		-- silly types like
		--	type T a = () -> ()
		--	f :: T a
		--	f () = ()
		-- Here, 'a' won't appear in qtvs, so we have to add it
	 	sig_tvs = foldl extendVarSetList emptyVarSet sig_tvs_s
		all_tvs = varSetElems (extendVarSetList sig_tvs qtvs)
	; returnM all_tvs }
  where
    check_sig gbl_tvs (TcSigInfo {sig_id = id, sig_tvs = tvs, 
				  sig_theta = theta, sig_tau = tau})
      = addErrCtxt (ptext SLIT("In the type signature for") <+> quotes (ppr id))	$
	addErrCtxtM (sigCtxt id tvs theta tau)						$
	do { tvs' <- checkDistinctTyVars tvs
	   ; ifM (any (`elemVarSet` gbl_tvs) tvs')
		 (bleatEscapedTvs gbl_tvs tvs tvs') 
	   ; return tvs' }

checkDistinctTyVars :: [TcTyVar] -> TcM [TcTyVar]
-- (checkDistinctTyVars tvs) checks that the tvs from one type signature
-- are still all type variables, and all distinct from each other.  
-- It returns a zonked set of type variables.
-- For example, if the type sig is
--	f :: forall a b. a -> b -> b
-- we want to check that 'a' and 'b' haven't 
--	(a) been unified with a non-tyvar type
--	(b) been unified with each other (all distinct)

checkDistinctTyVars sig_tvs
  = do	{ zonked_tvs <- mapM zonkSigTyVar sig_tvs
	; foldlM check_dup emptyVarEnv (sig_tvs `zip` zonked_tvs)
	; return zonked_tvs }
  where
    check_dup :: TyVarEnv TcTyVar -> (TcTyVar, TcTyVar) -> TcM (TyVarEnv TcTyVar)
	-- The TyVarEnv maps each zonked type variable back to its
	-- corresponding user-written signature type variable
    check_dup acc (sig_tv, zonked_tv)
	= case lookupVarEnv acc zonked_tv of
		Just sig_tv' -> bomb_out sig_tv sig_tv'

		Nothing -> return (extendVarEnv acc zonked_tv sig_tv)

    bomb_out sig_tv1 sig_tv2
       = do { env0 <- tcInitTidyEnv
	    ; let (env1, tidy_tv1) = tidyOpenTyVar env0 sig_tv1
		  (env2, tidy_tv2) = tidyOpenTyVar env1 sig_tv2
	          msg = ptext SLIT("Quantified type variable") <+> quotes (ppr tidy_tv1) 
		         <+> ptext SLIT("is unified with another quantified type variable") 
		         <+> quotes (ppr tidy_tv2)
	    ; failWithTcM (env2, msg) }
       where
\end{code}


@getTyVarsToGen@ decides what type variables to generalise over.

For a "restricted group" -- see the monomorphism restriction
for a definition -- we bind no dictionaries, and
remove from tyvars_to_gen any constrained type variables

*Don't* simplify dicts at this point, because we aren't going
to generalise over these dicts.  By the time we do simplify them
we may well know more.  For example (this actually came up)
	f :: Array Int Int
	f x = array ... xs where xs = [1,2,3,4,5]
We don't want to generate lots of (fromInt Int 1), (fromInt Int 2)
stuff.  If we simplify only at the f-binding (not the xs-binding)
we'll know that the literals are all Ints, and we can just produce
Int literals!

Find all the type variables involved in overloading, the
"constrained_tyvars".  These are the ones we *aren't* going to
generalise.  We must be careful about doing this:

 (a) If we fail to generalise a tyvar which is not actually
	constrained, then it will never, ever get bound, and lands
	up printed out in interface files!  Notorious example:
		instance Eq a => Eq (Foo a b) where ..
	Here, b is not constrained, even though it looks as if it is.
	Another, more common, example is when there's a Method inst in
	the LIE, whose type might very well involve non-overloaded
	type variables.
  [NOTE: Jan 2001: I don't understand the problem here so I'm doing 
	the simple thing instead]

 (b) On the other hand, we mustn't generalise tyvars which are constrained,
	because we are going to pass on out the unmodified LIE, with those
	tyvars in it.  They won't be in scope if we've generalised them.

So we are careful, and do a complete simplification just to find the
constrained tyvars. We don't use any of the results, except to
find which tyvars are constrained.

Note [Polymorphic recursion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The game plan for polymorphic recursion in the code above is 

	* Bind any variable for which we have a type signature
	  to an Id with a polymorphic type.  Then when type-checking 
	  the RHSs we'll make a full polymorphic call.

This fine, but if you aren't a bit careful you end up with a horrendous
amount of partial application and (worse) a huge space leak. For example:

	f :: Eq a => [a] -> [a]
	f xs = ...f...

If we don't take care, after typechecking we get

	f = /\a -> \d::Eq a -> let f' = f a d
			       in
			       \ys:[a] -> ...f'...

Notice the the stupid construction of (f a d), which is of course
identical to the function we're executing.  In this case, the
polymorphic recursion isn't being used (but that's a very common case).
This can lead to a massive space leak, from the following top-level defn
(post-typechecking)

	ff :: [Int] -> [Int]
	ff = f Int dEqInt

Now (f dEqInt) evaluates to a lambda that has f' as a free variable; but
f' is another thunk which evaluates to the same thing... and you end
up with a chain of identical values all hung onto by the CAF ff.

	ff = f Int dEqInt

	   = let f' = f Int dEqInt in \ys. ...f'...

	   = let f' = let f' = f Int dEqInt in \ys. ...f'...
		      in \ys. ...f'...

Etc.

NOTE: a bit of arity anaysis would push the (f a d) inside the (\ys...),
which would make the space leak go away in this case

Solution: when typechecking the RHSs we always have in hand the
*monomorphic* Ids for each binding.  So we just need to make sure that
if (Method f a d) shows up in the constraints emerging from (...f...)
we just use the monomorphic Id.  We achieve this by adding monomorphic Ids
to the "givens" when simplifying constraints.  That's what the "lies_avail"
is doing.

Then we get

	f = /\a -> \d::Eq a -> letrec
				 fm = \ys:[a] -> ...fm...
			       in
			       fm



%************************************************************************
%*									*
		Signatures
%*									*
%************************************************************************

Type signatures are tricky.  See Note [Signature skolems] in TcType

@tcSigs@ checks the signatures for validity, and returns a list of
{\em freshly-instantiated} signatures.  That is, the types are already
split up, and have fresh type variables installed.  All non-type-signature
"RenamedSigs" are ignored.

The @TcSigInfo@ contains @TcTypes@ because they are unified with
the variable's type, and after that checked to see whether they've
been instantiated.

\begin{code}
type TcSigFun = Name -> Maybe [Name]	-- Maps a let-binder to the list of
					-- type variables brought into scope
					-- by its type signature.
					-- Nothing => no type signature

mkTcSigFun :: [LSig Name] -> TcSigFun
-- Search for a particular type signature
-- Precondition: the sigs are all type sigs
-- Precondition: no duplicates
mkTcSigFun sigs = lookupNameEnv env
  where
    env = mkNameEnv [(name, hsExplicitTvs lhs_ty)
		    | L span (TypeSig (L _ name) lhs_ty) <- sigs]
	-- The scoped names are the ones explicitly mentioned
	-- in the HsForAll.  (There may be more in sigma_ty, because
	-- of nested type synonyms.  See Note [Scoped] with TcSigInfo.)
	-- See Note [Only scoped tyvars are in the TyVarEnv]

---------------
data TcSigInfo
  = TcSigInfo {
	sig_id     :: TcId,		--  *Polymorphic* binder for this value...

	sig_scoped :: [Name],		-- Names for any scoped type variables
					-- Invariant: correspond 1-1 with an initial
					-- segment of sig_tvs (see Note [Scoped])

	sig_tvs    :: [TcTyVar],	-- Instantiated type variables
					-- See Note [Instantiate sig]

	sig_theta  :: TcThetaType,	-- Instantiated theta
	sig_tau    :: TcTauType,	-- Instantiated tau
	sig_loc    :: InstLoc	 	-- The location of the signature
    }


--	Note [Only scoped tyvars are in the TyVarEnv]
-- We are careful to keep only the *lexically scoped* type variables in
-- the type environment.  Why?  After all, the renamer has ensured
-- that only legal occurrences occur, so we could put all type variables
-- into the type env.
--
-- But we want to check that two distinct lexically scoped type variables
-- do not map to the same internal type variable.  So we need to know which
-- the lexically-scoped ones are... and at the moment we do that by putting
-- only the lexically scoped ones into the environment.


-- 	Note [Scoped]
-- There may be more instantiated type variables than scoped 
-- ones.  For example:
--	type T a = forall b. b -> (a,b)
--	f :: forall c. T c
-- Here, the signature for f will have one scoped type variable, c,
-- but two instantiated type variables, c' and b'.  
--
-- We assume that the scoped ones are at the *front* of sig_tvs,
-- and remember the names from the original HsForAllTy in sig_scoped

-- 	Note [Instantiate sig]
-- It's vital to instantiate a type signature with fresh variables.
-- For example:
--	type S = forall a. a->a
--	f,g :: S
--	f = ...
--	g = ...
-- Here, we must use distinct type variables when checking f,g's right hand sides.
-- (Instantiation is only necessary because of type synonyms.  Otherwise,
-- it's all cool; each signature has distinct type variables from the renamer.)

instance Outputable TcSigInfo where
    ppr (TcSigInfo { sig_id = id, sig_tvs = tyvars, sig_theta = theta, sig_tau = tau})
	= ppr id <+> ptext SLIT("::") <+> ppr tyvars <+> ppr theta <+> ptext SLIT("=>") <+> ppr tau
\end{code}

\begin{code}
tcTySig :: LSig Name -> TcM TcId
tcTySig (L span (TypeSig (L _ name) ty))
  = setSrcSpan span		$
    do	{ sigma_ty <- tcHsSigType (FunSigCtxt name) ty
	; return (mkLocalId name sigma_ty) }

-------------------
tcInstSig_maybe :: TcSigFun -> Name -> TcM (Maybe TcSigInfo)
-- Instantiate with *meta* type variables; 
-- this signature is part of a multi-signature group
tcInstSig_maybe sig_fn name 
  = case sig_fn name of
	Nothing  -> return Nothing
	Just tvs -> do	{ tc_sig <- tcInstSig False name tvs
			; return (Just tc_sig) }

tcInstSig :: Bool -> Name -> [Name] -> TcM TcSigInfo
-- Instantiate the signature, with either skolems or meta-type variables
-- depending on the use_skols boolean.  This variable is set True
-- when we are typechecking a single function binding; and False for
-- pattern bindings and a group of several function bindings.
-- Reason: in the latter cases, the "skolems" can be unified together, 
-- 	   so they aren't properly rigid in the type-refinement sense.
-- NB: unless we are doing H98, each function with a sig will be done
--     separately, even if it's mutually recursive, so use_skols will be True
--
-- We always instantiate with fresh uniques,
-- although we keep the same print-name
--	
--	type T = forall a. [a] -> [a]
--	f :: T; 
--	f = g where { g :: T; g = <rhs> }
--
-- We must not use the same 'a' from the defn of T at both places!!

tcInstSig use_skols name scoped_names
  = do	{ poly_id <- tcLookupId name	-- Cannot fail; the poly ids are put into 
					-- scope when starting the binding group
	; let skol_info = SigSkol (FunSigCtxt name)
	      inst_tyvars = tcInstSigTyVars use_skols skol_info
	; (tvs, theta, tau) <- tcInstType inst_tyvars (idType poly_id)
	; loc <- getInstLoc (SigOrigin skol_info)
	; return (TcSigInfo { sig_id = poly_id,
			      sig_tvs = tvs, sig_theta = theta, sig_tau = tau, 
			      sig_scoped = final_scoped_names, sig_loc = loc }) }
		-- Note that the scoped_names and the sig_tvs will have
		-- different Names. That's quite ok; when we bring the 
		-- scoped_names into scope, we just bind them to the sig_tvs
  where
	-- We also only have scoped type variables when we are instantiating
	-- with true skolems
    final_scoped_names | use_skols = scoped_names
		       | otherwise = []

-------------------
isMonoGroup :: DynFlags -> [LHsBind Name] -> Bool
-- No generalisation at all
isMonoGroup dflags binds
  = dopt Opt_MonoPatBinds dflags && any is_pat_bind binds
  where
    is_pat_bind (L _ (PatBind {})) = True
    is_pat_bind other	           = False

-------------------
isRestrictedGroup :: DynFlags -> [LHsBind Name] -> TcSigFun -> Bool
isRestrictedGroup dflags binds sig_fn
  = mono_restriction && not all_unrestricted
  where 
    mono_restriction = dopt Opt_MonomorphismRestriction dflags
    all_unrestricted = all (unrestricted . unLoc) binds
    has_sig n = isJust (sig_fn n)

    unrestricted (PatBind {})  					 = False
    unrestricted (VarBind { var_id = v })		         = has_sig v
    unrestricted (FunBind { fun_id = v, fun_matches = matches }) = unrestricted_match matches 
								 || has_sig (unLoc v)

    unrestricted_match (MatchGroup (L _ (Match [] _ _) : _) _) = False
	-- No args => like a pattern binding
    unrestricted_match other	          = True
	-- Some args => a function binding
\end{code}


%************************************************************************
%*									*
\subsection[TcBinds-errors]{Error contexts and messages}
%*									*
%************************************************************************


\begin{code}
-- This one is called on LHS, when pat and grhss are both Name 
-- and on RHS, when pat is TcId and grhss is still Name
patMonoBindsCtxt pat grhss
  = hang (ptext SLIT("In a pattern binding:")) 4 (pprPatBind pat grhss)

-----------------------------------------------
sigContextsCtxt sig1 sig2
  = vcat [ptext SLIT("When matching the contexts of the signatures for"), 
	  nest 2 (vcat [ppr id1 <+> dcolon <+> ppr (idType id1),
			ppr id2 <+> dcolon <+> ppr (idType id2)]),
	  ptext SLIT("The signature contexts in a mutually recursive group should all be identical")]
  where
    id1 = sig_id sig1
    id2 = sig_id sig2


-----------------------------------------------
unboxedTupleErr name ty
  = hang (ptext SLIT("Illegal binding of unboxed tuple"))
	 4 (ppr name <+> dcolon <+> ppr ty)

-----------------------------------------------
restrictedBindCtxtErr binder_names
  = hang (ptext SLIT("Illegal overloaded type signature(s)"))
       4 (vcat [ptext SLIT("in a binding group for") <+> pprBinders binder_names,
		ptext SLIT("that falls under the monomorphism restriction")])

genCtxt binder_names
  = ptext SLIT("When generalising the type(s) for") <+> pprBinders binder_names

missingSigWarn False name ty = return ()
missingSigWarn True  name ty
  = do 	{ env0 <- tcInitTidyEnv
	; let (env1, tidy_ty) = tidyOpenType env0 ty
	; addWarnTcM (env1, mk_msg tidy_ty) }
  where
    mk_msg ty = vcat [ptext SLIT("Definition but no type signature for") <+> quotes (ppr name),
		      sep [ptext SLIT("Inferred type:") <+> ppr name <+> dcolon <+> ppr ty]]
\end{code}
