%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module BuildTyCl (
	buildSynTyCon, buildAlgTyCon, buildDataCon,
	buildClass,
	mkAbstractTyConRhs, mkOpenDataTyConRhs, 
	mkNewTyConRhs, mkDataTyConRhs 
    ) where

#include "HsVersions.h"

import IfaceEnv
import TcRnMonad

import DataCon
import Var
import VarSet
import TysWiredIn
import BasicTypes
import Name
import OccName
import MkId
import Class
import TyCon
import Type
import Coercion

import TcRnMonad
import Outputable

import Data.List
\end{code}
	

\begin{code}
------------------------------------------------------
buildSynTyCon :: Name -> [TyVar] 
              -> SynTyConRhs 
	      -> Maybe (TyCon, [Type])  -- family instance if applicable
              -> TcRnIf m n TyCon

buildSynTyCon tc_name tvs rhs@(OpenSynTyCon rhs_ki _) _
  = let
      kind = mkArrowKinds (map tyVarKind tvs) rhs_ki
    in
    return $ mkSynTyCon tc_name kind tvs rhs NoParentTyCon
    
buildSynTyCon tc_name tvs rhs@(SynonymTyCon rhs_ty) mb_family
  = do { -- We need to tie a knot as the coercion of a data instance depends
	 -- on the instance representation tycon and vice versa.
       ; tycon <- fixM (\ tycon_rec -> do 
	 { parent <- mkParentInfo mb_family tc_name tvs tycon_rec
	 ; let { tycon   = mkSynTyCon tc_name kind tvs rhs parent
	       ; kind    = mkArrowKinds (map tyVarKind tvs) (typeKind rhs_ty)
	       }
         ; return tycon
         })
       ; return tycon 
       }

------------------------------------------------------
buildAlgTyCon :: Name -> [TyVar] 
	      -> ThetaType		-- Stupid theta
	      -> AlgTyConRhs
	      -> RecFlag
	      -> Bool			-- True <=> want generics functions
	      -> Bool			-- True <=> was declared in GADT syntax
	      -> Maybe (TyCon, [Type])  -- family instance if applicable
	      -> TcRnIf m n TyCon

buildAlgTyCon tc_name tvs stupid_theta rhs is_rec want_generics gadt_syn
	      mb_family
  = do { -- We need to tie a knot as the coercion of a data instance depends
	 -- on the instance representation tycon and vice versa.
       ; tycon <- fixM (\ tycon_rec -> do 
	 { parent <- mkParentInfo mb_family tc_name tvs tycon_rec
	 ; let { tycon = mkAlgTyCon tc_name kind tvs stupid_theta rhs
				    fields parent is_rec want_generics gadt_syn
	       ; kind    = mkArrowKinds (map tyVarKind tvs) liftedTypeKind
	       ; fields  = mkTyConSelIds tycon rhs
	       }
         ; return tycon
         })
       ; return tycon 
       }

-- If a family tycon with instance types is given, the current tycon is an
-- instance of that family and we need to
--
-- (1) create a coercion that identifies the family instance type and the
--     representation type from Step (1); ie, it is of the form 
--	   `Co tvs :: F ts :=: R tvs', where `Co' is the name of the coercion,
--	   `F' the family tycon and `R' the (derived) representation tycon,
--	   and
-- (2) produce a `TyConParent' value containing the parent and coercion
--     information.
--
mkParentInfo :: Maybe (TyCon, [Type]) 
             -> Name -> [TyVar] 
             -> TyCon 
             -> TcRnIf m n TyConParent
mkParentInfo Nothing                  _       _   _         =
  return NoParentTyCon
mkParentInfo (Just (family, instTys)) tc_name tvs rep_tycon =
  do { -- Create the coercion
     ; co_tycon_name <- newImplicitBinder tc_name mkInstTyCoOcc
     ; let co_tycon = mkFamInstCoercion co_tycon_name tvs
                                        family instTys rep_tycon
     ; return $ FamilyTyCon family instTys co_tycon
     }
    
------------------------------------------------------
mkAbstractTyConRhs :: AlgTyConRhs
mkAbstractTyConRhs = AbstractTyCon

mkOpenDataTyConRhs :: AlgTyConRhs
mkOpenDataTyConRhs = OpenTyCon Nothing

mkDataTyConRhs :: [DataCon] -> AlgTyConRhs
mkDataTyConRhs cons
  = DataTyCon { data_cons = cons, is_enum = all isNullarySrcDataCon cons }

mkNewTyConRhs :: Name -> TyCon -> DataCon -> TcRnIf m n AlgTyConRhs
-- Monadic because it makes a Name for the coercion TyCon
-- We pass the Name of the parent TyCon, as well as the TyCon itself,
-- because the latter is part of a knot, whereas the former is not.
mkNewTyConRhs tycon_name tycon con 
  = do	{ co_tycon_name <- newImplicitBinder tycon_name mkNewTyCoOcc
	; let co_tycon = mkNewTypeCoercion co_tycon_name tycon etad_tvs etad_rhs
              cocon_maybe | all_coercions || isRecursiveTyCon tycon 
		          = Just co_tycon
                	  | otherwise              
                	  = Nothing
	; traceIf (text "mkNewTyConRhs" <+> ppr cocon_maybe)
	; return (NewTyCon { data_con    = con, 
		       	     nt_rhs      = rhs_ty,
		       	     nt_etad_rhs = (etad_tvs, etad_rhs),
 		       	     nt_co 	 = cocon_maybe, 
                             -- Coreview looks through newtypes with a Nothing
                             -- for nt_co, or uses explicit coercions otherwise
		       	     nt_rep = mkNewTyConRep tycon rhs_ty }) }
  where
        -- If all_coercions is True then we use coercions for all newtypes
        -- otherwise we use coercions for recursive newtypes and look through
        -- non-recursive newtypes
    all_coercions = True
    tvs    = tyConTyVars tycon
    rhs_ty = ASSERT(not (null (dataConInstOrigDictsAndArgTys con (mkTyVarTys tvs)))) 
	     -- head (dataConInstOrigArgTys con (mkTyVarTys tvs))
	     head (dataConInstOrigDictsAndArgTys con (mkTyVarTys tvs))
	-- Instantiate the data con with the 
	-- type variables from the tycon
	-- NB: a newtype DataCon has no existentials; hence the
	--     call to dataConInstOrigArgTys has the right type args

    etad_tvs :: [TyVar]	-- Matched lazily, so that mkNewTypeCoercion can
    etad_rhs :: Type	-- return a TyCon without pulling on rhs_ty
			-- See Note [Tricky iface loop] in LoadIface
    (etad_tvs, etad_rhs) = eta_reduce (reverse tvs) rhs_ty
 
    eta_reduce :: [TyVar]		-- Reversed
	       -> Type			-- Rhs type
	       -> ([TyVar], Type)	-- Eta-reduced version (tyvars in normal order)
    eta_reduce (a:as) ty | Just (fun, arg) <- splitAppTy_maybe ty,
			   Just tv <- getTyVar_maybe arg,
			   tv == a,
			   not (a `elemVarSet` tyVarsOfType fun)
			 = eta_reduce as fun
    eta_reduce tvs ty = (reverse tvs, ty)
				

mkNewTyConRep :: TyCon		-- The original type constructor
	      -> Type		-- The arg type of its constructor
	      -> Type		-- Chosen representation type
-- The "representation type" is guaranteed not to be another newtype
-- at the outermost level; but it might have newtypes in type arguments

-- Find the representation type for this newtype TyCon
-- Remember that the representation type is the *ultimate* representation
-- type, looking through other newtypes.
-- 
-- splitTyConApp_maybe no longer looks through newtypes, so we must
-- deal explicitly with this case
-- 
-- The trick is to to deal correctly with recursive newtypes
-- such as	newtype T = MkT T

mkNewTyConRep tc rhs_ty
  | null (tyConDataCons tc) = unitTy
	-- External Core programs can have newtypes with no data constructors
  | otherwise		    = go [tc] rhs_ty
  where
	-- Invariant: tcs have been seen before
    go tcs rep_ty 
	= case splitTyConApp_maybe rep_ty of
	    Just (tc, tys)
		| tc `elem` tcs -> unitTy	-- Recursive loop
		| isNewTyCon tc -> 
                    if isRecursiveTyCon tc then
			go (tc:tcs) (substTyWith tvs tys rhs_ty)
                    else
                        substTyWith tvs tys rhs_ty
		where
		  (tvs, rhs_ty) = newTyConRhs tc

	    other -> rep_ty 

------------------------------------------------------
buildDataCon :: Name -> Bool
	    -> [StrictnessMark] 
	    -> [Name]			-- Field labels
	    -> [TyVar] -> [TyVar]	-- Univ and ext 
            -> [(TyVar,Type)]           -- Equality spec
	    -> ThetaType		-- Does not include the "stupid theta"
					-- or the GADT equalities
	    -> [Type] -> TyCon
	    -> TcRnIf m n DataCon
-- A wrapper for DataCon.mkDataCon that
--   a) makes the worker Id
--   b) makes the wrapper Id if necessary, including
--	allocating its unique (hence monadic)
buildDataCon src_name declared_infix arg_stricts field_lbls
	     univ_tvs ex_tvs eq_spec ctxt arg_tys tycon
  = do	{ wrap_name <- newImplicitBinder src_name mkDataConWrapperOcc
	; work_name <- newImplicitBinder src_name mkDataConWorkerOcc
	-- This last one takes the name of the data constructor in the source
	-- code, which (for Haskell source anyway) will be in the DataName name
	-- space, and puts it into the VarName name space

	; let
		stupid_ctxt = mkDataConStupidTheta tycon arg_tys univ_tvs
		data_con = mkDataCon src_name declared_infix
				     arg_stricts field_lbls
				     univ_tvs ex_tvs eq_spec ctxt
				     arg_tys tycon
				     stupid_ctxt dc_ids
		dc_ids = mkDataConIds wrap_name work_name data_con

	; returnM data_con }


-- The stupid context for a data constructor should be limited to
-- the type variables mentioned in the arg_tys
-- ToDo: Or functionally dependent on?  
--	 This whole stupid theta thing is, well, stupid.
mkDataConStupidTheta tycon arg_tys univ_tvs
  | null stupid_theta = []	-- The common case
  | otherwise 	      = filter in_arg_tys stupid_theta
  where
    tc_subst	 = zipTopTvSubst (tyConTyVars tycon) (mkTyVarTys univ_tvs)
    stupid_theta = substTheta tc_subst (tyConStupidTheta tycon)
	-- Start by instantiating the master copy of the 
	-- stupid theta, taken from the TyCon

    arg_tyvars      = tyVarsOfTypes arg_tys
    in_arg_tys pred = not $ isEmptyVarSet $ 
		      tyVarsOfPred pred `intersectVarSet` arg_tyvars

------------------------------------------------------
mkTyConSelIds :: TyCon -> AlgTyConRhs -> [Id]
mkTyConSelIds tycon rhs
  =  [ mkRecordSelId tycon fld 
     | fld <- nub (concatMap dataConFieldLabels (visibleDataCons rhs)) ]
	-- We'll check later that fields with the same name 
	-- from different constructors have the same type.
\end{code}


------------------------------------------------------
\begin{code}
buildClass :: Name -> [TyVar] -> ThetaType
	   -> [FunDep TyVar]		-- Functional dependencies
	   -> [TyThing]			-- Associated types
	   -> [(Name, DefMeth, Type)]	-- Method info
	   -> RecFlag			-- Info for type constructor
	   -> TcRnIf m n Class

buildClass class_name tvs sc_theta fds ats sig_stuff tc_isrec
  = do	{ traceIf (text "buildClass")
	; tycon_name <- newImplicitBinder class_name mkClassTyConOcc
	; datacon_name <- newImplicitBinder class_name mkClassDataConOcc
		-- The class name is the 'parent' for this datacon, not its tycon,
		-- because one should import the class to get the binding for 
		-- the datacon

	; fixM (\ rec_clas -> do {	-- Only name generation inside loop

	  let { rec_tycon  = classTyCon rec_clas
	      ; op_tys	   = [ty | (_,_,ty) <- sig_stuff]
	      ; op_items   = [ (mkDictSelId op_name rec_clas, dm_info)
			     | (op_name, dm_info, _) <- sig_stuff ] }
	  		-- Build the selector id and default method id

	; dict_con <- buildDataCon datacon_name
				   False 	-- Not declared infix
				   (map (const NotMarkedStrict) op_tys)
				   [{- No labelled fields -}]
				   tvs [{- no existentials -}]
                                   [{- No GADT equalities -}] sc_theta 
                                   op_tys
				   rec_tycon

	; sc_sel_names <- mapM (newImplicitBinder class_name . mkSuperDictSelOcc) 
				[1..length (dataConDictTheta dict_con)]
	      -- We number off the Dict superclass selectors, 1, 2, 3 etc so that we 
	      -- can construct names for the selectors.  Thus
	      --      class (C a, C b) => D a b where ...
	      -- gives superclass selectors
	      --      D_sc1, D_sc2
	      -- (We used to call them D_C, but now we can have two different
	      --  superclasses both called C!)
        ; let sc_sel_ids = [mkDictSelId sc_name rec_clas | sc_name <- sc_sel_names]

		-- Use a newtype if the class constructor has exactly one field:
		-- i.e. exactly one operation or superclass taken together
		-- Watch out: the sc_theta includes equality predicates,
		-- 	      which don't count for this purpose; hence dataConDictTheta
	; rhs <- if ((length $ dataConDictTheta dict_con) + length sig_stuff) == 1
		 then mkNewTyConRhs tycon_name rec_tycon dict_con
		 else return (mkDataTyConRhs [dict_con])

	; let {	clas_kind = mkArrowKinds (map tyVarKind tvs) liftedTypeKind

 	      ; tycon = mkClassTyCon tycon_name clas_kind tvs
 	                             rhs rec_clas tc_isrec
		-- A class can be recursive, and in the case of newtypes 
		-- this matters.  For example
		-- 	class C a where { op :: C b => a -> b -> Int }
		-- Because C has only one operation, it is represented by
		-- a newtype, and it should be a *recursive* newtype.
		-- [If we don't make it a recursive newtype, we'll expand the
		-- newtype like a synonym, but that will lead to an infinite
		-- type]
	      ; atTyCons = [tycon | ATyCon tycon <- ats]

	      ; result = mkClass class_name tvs fds 
			         sc_theta sc_sel_ids atTyCons
				 op_items tycon
	      }
	; traceIf (text "buildClass" <+> ppr tycon) 
	; return result
	})}
\end{code}


