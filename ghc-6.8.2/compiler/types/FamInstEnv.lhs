%
% (c) The University of Glasgow 2006
%

FamInstEnv: Type checked family instance declarations

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module FamInstEnv (
	FamInst(..), famInstTyCon, famInstTyVars, 
	pprFamInst, pprFamInstHdr, pprFamInsts, 
	famInstHead, mkLocalFamInst, mkImportedFamInst,

	FamInstEnvs, FamInstEnv, emptyFamInstEnv, emptyFamInstEnvs, 
	extendFamInstEnv, extendFamInstEnvList, 
	famInstEnvElts, familyInstances,

	lookupFamInstEnv, lookupFamInstEnvUnify,
	
	-- Normalisation
	topNormaliseType
    ) where

#include "HsVersions.h"

import InstEnv
import Unify
import TcGadt
import TcType
import Type
import TypeRep
import TyCon
import Coercion
import VarSet
import Var
import Name
import OccName
import SrcLoc
import UniqFM
import Outputable
import Maybes
import Util

import Maybe
\end{code}


%************************************************************************
%*									*
\subsection{Type checked family instance heads}
%*									*
%************************************************************************

\begin{code}
data FamInst 
  = FamInst { fi_fam   :: Name		-- Family name
		-- INVARIANT: fi_fam = case tyConFamInst_maybe fi_tycon of
		--			   Just (tc, tys) -> tc

		-- Used for "rough matching"; same idea as for class instances
	    , fi_tcs   :: [Maybe Name]	-- Top of type args
		-- INVARIANT: fi_tcs = roughMatchTcs fi_tys

		-- Used for "proper matching"; ditto
	    , fi_tvs   :: TyVarSet	-- Template tyvars for full match
	    , fi_tys   :: [Type]	-- Full arg types
		-- INVARIANT: fi_tvs = tyConTyVars fi_tycon
		--	      fi_tys = case tyConFamInst_maybe fi_tycon of
		--			   Just (_, tys) -> tys

	    , fi_tycon :: TyCon		-- Representation tycon
	    }

-- Obtain the representation tycon of a family instance.
--
famInstTyCon :: FamInst -> TyCon
famInstTyCon = fi_tycon

famInstTyVars = fi_tvs
\end{code}

\begin{code}
instance NamedThing FamInst where
   getName = getName . fi_tycon

instance Outputable FamInst where
   ppr = pprFamInst

-- Prints the FamInst as a family instance declaration
pprFamInst :: FamInst -> SDoc
pprFamInst famInst
  = hang (pprFamInstHdr famInst)
	2 (ptext SLIT("--") <+> pprNameLoc (getName famInst))

pprFamInstHdr :: FamInst -> SDoc
pprFamInstHdr (FamInst {fi_fam = fam, fi_tys = tys, fi_tycon = tycon})
  = pprTyConSort <+> pprHead
  where
    pprHead = pprTypeApp fam (ppr fam) tys
    pprTyConSort | isDataTyCon tycon = ptext SLIT("data instance")
		 | isNewTyCon  tycon = ptext SLIT("newtype instance")
		 | isSynTyCon  tycon = ptext SLIT("type instance")
		 | otherwise	     = panic "FamInstEnv.pprFamInstHdr"

pprFamInsts :: [FamInst] -> SDoc
pprFamInsts finsts = vcat (map pprFamInst finsts)

famInstHead :: FamInst -> ([TyVar], TyCon, [Type])
famInstHead (FamInst {fi_tycon = tycon})
  = case tyConFamInst_maybe tycon of
      Nothing         -> panic "FamInstEnv.famInstHead"
      Just (fam, tys) -> (tyConTyVars tycon, fam, tys)

-- Make a family instance representation from a tycon.  This is used for local
-- instances, where we can safely pull on the tycon.
--
mkLocalFamInst :: TyCon -> FamInst
mkLocalFamInst tycon
  = case tyConFamInst_maybe tycon of
           Nothing         -> panic "FamInstEnv.mkLocalFamInst"
	   Just (fam, tys) -> 
	     FamInst {
	       fi_fam   = tyConName fam,
	       fi_tcs   = roughMatchTcs tys,
	       fi_tvs   = mkVarSet . tyConTyVars $ tycon,
	       fi_tys   = tys,
	       fi_tycon = tycon
	     }

-- Make a family instance representation from the information found in an
-- unterface file.  In particular, we get the rough match info from the iface
-- (instead of computing it here).
--
mkImportedFamInst :: Name -> [Maybe Name] -> TyCon -> FamInst
mkImportedFamInst fam mb_tcs tycon
  = FamInst {
      fi_fam   = fam,
      fi_tcs   = mb_tcs,
      fi_tvs   = mkVarSet . tyConTyVars $ tycon,
      fi_tys   = case tyConFamInst_maybe tycon of
		   Nothing       -> panic "FamInstEnv.mkImportedFamInst"
		   Just (_, tys) -> tys,
      fi_tycon = tycon
    }
\end{code}


%************************************************************************
%*									*
		FamInstEnv
%*									*
%************************************************************************

InstEnv maps a family name to the list of known instances for that family.

\begin{code}
type FamInstEnv = UniqFM FamilyInstEnv	-- Maps a family to its instances

type FamInstEnvs = (FamInstEnv, FamInstEnv)
 	-- External package inst-env, Home-package inst-env

data FamilyInstEnv
  = FamIE [FamInst]	-- The instances for a particular family, in any order
  	  Bool 		-- True <=> there is an instance of form T a b c
			-- 	If *not* then the common case of looking up
			--	(T a b c) can fail immediately

-- INVARIANTS:
--  * The fs_tvs are distinct in each FamInst
--	of a range value of the map (so we can safely unify them)

emptyFamInstEnvs :: (FamInstEnv, FamInstEnv)
emptyFamInstEnvs = (emptyFamInstEnv, emptyFamInstEnv)

emptyFamInstEnv :: FamInstEnv
emptyFamInstEnv = emptyUFM

famInstEnvElts :: FamInstEnv -> [FamInst]
famInstEnvElts fi = [elt | FamIE elts _ <- eltsUFM fi, elt <- elts]

familyInstances :: (FamInstEnv, FamInstEnv) -> TyCon -> [FamInst]
familyInstances (pkg_fie, home_fie) fam
  = get home_fie ++ get pkg_fie
  where
    get env = case lookupUFM env fam of
		Just (FamIE insts _) -> insts
		Nothing	             -> []

extendFamInstEnvList :: FamInstEnv -> [FamInst] -> FamInstEnv
extendFamInstEnvList inst_env fis = foldl extendFamInstEnv inst_env fis

extendFamInstEnv :: FamInstEnv -> FamInst -> FamInstEnv
extendFamInstEnv inst_env ins_item@(FamInst {fi_fam = cls_nm, fi_tcs = mb_tcs})
  = addToUFM_C add inst_env cls_nm (FamIE [ins_item] ins_tyvar)
  where
    add (FamIE items tyvar) _ = FamIE (ins_item:items)
				      (ins_tyvar || tyvar)
    ins_tyvar = not (any isJust mb_tcs)
\end{code}

%************************************************************************
%*									*
		Looking up a family instance
%*									*
%************************************************************************

@lookupFamInstEnv@ looks up in a @FamInstEnv@, using a one-way match.
Multiple matches are only possible in case of type families (not data
families), and then, it doesn't matter which match we choose (as the
instances are guaranteed confluent).

We return the matching family instances and the type instance at which it
matches.  For example, if we lookup 'T [Int]' and have a family instance

  data instance T [a] = ..

desugared to

  data :R42T a = ..
  coe :Co:R42T a :: T [a] ~ :R42T a

we return the matching instance '(FamInst{.., fi_tycon = :R42T}, Int)'.

\begin{code}
type FamInstMatch = (FamInst, [Type])           -- Matching type instance

lookupFamInstEnv :: FamInstEnvs
	         -> TyCon -> [Type]		-- What we are looking for
	         -> [FamInstMatch] 	        -- Successful matches
lookupFamInstEnv (pkg_ie, home_ie) fam tys
  | not (isOpenTyCon fam) 
  = []
  | otherwise
  = home_matches ++ pkg_matches
  where
    rough_tcs    = roughMatchTcs tys
    all_tvs      = all isNothing rough_tcs
    home_matches = lookup home_ie 
    pkg_matches  = lookup pkg_ie  

    --------------
    lookup env = case lookupUFM env fam of
		   Nothing -> []	-- No instances for this class
		   Just (FamIE insts has_tv_insts)
		       -- Short cut for common case:
		       --   The thing we are looking up is of form (C a
		       --   b c), and the FamIE has no instances of
		       --   that form, so don't bother to search 
		     | all_tvs && not has_tv_insts -> []
		     | otherwise                   -> find insts

    --------------
    find [] = []
    find (item@(FamInst { fi_tcs = mb_tcs, fi_tvs = tpl_tvs, 
			  fi_tys = tpl_tys, fi_tycon = tycon }) : rest)
	-- Fast check for no match, uses the "rough match" fields
      | instanceCantMatch rough_tcs mb_tcs
      = find rest

        -- Proper check
      | Just subst <- tcMatchTys tpl_tvs tpl_tys tys
      = (item, substTyVars subst (tyConTyVars tycon)) : find rest

        -- No match => try next
      | otherwise
      = find rest
\end{code}

While @lookupFamInstEnv@ uses a one-way match, the next function
@lookupFamInstEnvUnify@ uses two-way matching (ie, unification).  This is
needed to check for overlapping instances.

For class instances, these two variants of lookup are combined into one
function (cf, @InstEnv@).  We don't do that for family instances as the
results of matching and unification are used in two different contexts.
Moreover, matching is the wildly more frequently used operation in the case of
indexed synonyms and we don't want to slow that down by needless unification.

\begin{code}
lookupFamInstEnvUnify :: (FamInstEnv, FamInstEnv) -> TyCon -> [Type]
	              -> [(FamInstMatch, TvSubst)]
lookupFamInstEnvUnify (pkg_ie, home_ie) fam tys
  | not (isOpenTyCon fam) 
  = []
  | otherwise
  = home_matches ++ pkg_matches
  where
    rough_tcs    = roughMatchTcs tys
    all_tvs      = all isNothing rough_tcs
    home_matches = lookup home_ie 
    pkg_matches  = lookup pkg_ie  

    --------------
    lookup env = case lookupUFM env fam of
		   Nothing -> []	-- No instances for this class
		   Just (FamIE insts has_tv_insts)
		       -- Short cut for common case:
		       --   The thing we are looking up is of form (C a
		       --   b c), and the FamIE has no instances of
		       --   that form, so don't bother to search 
		     | all_tvs && not has_tv_insts -> []
		     | otherwise                   -> find insts

    --------------
    find [] = []
    find (item@(FamInst { fi_tcs = mb_tcs, fi_tvs = tpl_tvs, 
			  fi_tys = tpl_tys, fi_tycon = tycon }) : rest)
	-- Fast check for no match, uses the "rough match" fields
      | instanceCantMatch rough_tcs mb_tcs
      = find rest

      | otherwise
      = ASSERT2( tyVarsOfTypes tys `disjointVarSet` tpl_tvs,
		 (ppr fam <+> ppr tys <+> ppr all_tvs) $$
		 (ppr tycon <+> ppr tpl_tvs <+> ppr tpl_tys)
		)
		-- Unification will break badly if the variables overlap
		-- They shouldn't because we allocate separate uniques for them
        case tcUnifyTys bind_fn tpl_tys tys of
	    Just subst -> let rep_tys = substTyVars subst (tyConTyVars tycon)
                          in
                          ((item, rep_tys), subst) : find rest
	    Nothing    -> find rest

-- See explanation at @InstEnv.bind_fn@.
--
bind_fn tv | isTcTyVar tv && isExistentialTyVar tv = Skolem
	   | otherwise	 		 	   = BindMe
\end{code}

%************************************************************************
%*									*
		Looking up a family instance
%*									*
%************************************************************************

\begin{code}
topNormaliseType :: FamInstEnvs
		      -> Type
	   	      -> Maybe (Coercion, Type)

-- Get rid of *outermost* (or toplevel) type functions by rewriting them
-- By "outer" we mean that toplevelNormaliseType guarantees to return
-- a type that does not have a reducible redex (F ty1 .. tyn) as its
-- outermost form.  It *can* return something like (Maybe (F ty)), where
-- (F ty) is a redex.

topNormaliseType env ty
  | Just ty' <- tcView ty = topNormaliseType env ty'

topNormaliseType env ty@(TyConApp tc tys)
  | isOpenTyCon tc
  , (ACo co, ty) <- normaliseType env ty
  = Just (co, ty)

topNormaliseType env ty
  = Nothing
	 

normaliseType :: FamInstEnvs 		-- environment with family instances
	      -> Type  			-- old type
	      -> (CoercionI,Type)	-- (coercion,new type), where
					-- co :: old-type ~ new_type
-- Normalise the input type, by eliminating all type-function redexes

normaliseType env ty 
  | Just ty' <- coreView ty = normaliseType env ty' 

normaliseType env ty@(TyConApp tyCon tys)
  = let	-- First normalise the arg types
	(cois, ntys) = mapAndUnzip (normaliseType env) tys
	tycon_coi    = mkTyConAppCoI tyCon ntys cois
    in 	-- Now try the top-level redex
    case lookupFamInstEnv env tyCon ntys of
		-- A matching family instance exists
	[(fam_inst, tys)] -> (fix_coi, nty)
	    where
		rep_tc         = famInstTyCon fam_inst
		co_tycon       = expectJust "lookupFamInst" (tyConFamilyCoercion_maybe rep_tc)
		co	       = mkTyConApp co_tycon tys
		first_coi      = mkTransCoI tycon_coi (ACo co)
		(rest_coi,nty) = normaliseType env (mkTyConApp rep_tc tys)
		fix_coi        = mkTransCoI first_coi rest_coi

		-- No unique matching family instance exists;
		-- we do not do anything
	other -> (tycon_coi, TyConApp tyCon ntys)

  where

normaliseType env ty@(AppTy ty1 ty2)
  =	let (coi1,nty1) = normaliseType env ty1
	    (coi2,nty2) = normaliseType env ty2
	in  (mkAppTyCoI nty1 coi1 nty2 coi2, AppTy nty1 nty2)
normaliseType env ty@(FunTy ty1 ty2)
  =	let (coi1,nty1) = normaliseType env ty1
	    (coi2,nty2) = normaliseType env ty2
	in  (mkFunTyCoI nty1 coi1 nty2 coi2, FunTy nty1 nty2)
normaliseType env ty@(ForAllTy tyvar ty1)
  =	let (coi,nty1) = normaliseType env ty1
	in  (mkForAllTyCoI tyvar coi,ForAllTy tyvar nty1)
normaliseType env ty@(NoteTy note ty1)
  =	let (coi,nty1) = normaliseType env ty1
	in  (mkNoteTyCoI note coi,NoteTy note nty1)
normaliseType env ty@(TyVarTy _)
  =	(IdCo,ty)
normaliseType env (PredTy predty)
  =	normalisePred env predty	

normalisePred :: FamInstEnvs -> PredType -> (CoercionI,Type)
normalisePred env (ClassP cls tys)
  =	let (cois,tys') = mapAndUnzip (normaliseType env) tys
	in  (mkClassPPredCoI cls tys' cois, PredTy $ ClassP cls tys')
normalisePred env (IParam ipn ty)
  = 	let (coi,ty') = normaliseType env ty
	in  (mkIParamPredCoI ipn coi, PredTy $ IParam ipn ty')
normalisePred env (EqPred ty1 ty2)
  = 	let (coi1,ty1') = normaliseType env ty1
            (coi2,ty2') = normaliseType env ty2
	in  (mkEqPredCoI ty1' coi1 ty2' coi2, PredTy $ EqPred ty1' ty2')
\end{code}
