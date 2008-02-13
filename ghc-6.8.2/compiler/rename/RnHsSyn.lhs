%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[RnHsSyn]{Specialisations of the @HsSyn@ syntax for the renamer}

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module RnHsSyn( 
	-- Names
	charTyCon_name, listTyCon_name, parrTyCon_name, tupleTyCon_name,
	extractHsTyVars, extractHsTyNames, extractHsTyNames_s, 
	extractFunDepNames, extractHsCtxtTyNames, extractHsPredTyNames,

	-- Free variables
	hsSigsFVs, hsSigFVs, conDeclFVs, bangTyFVs,
	
	maybeGenericMatch
  ) where

#include "HsVersions.h"

import HsSyn
import Class		( FunDep )
import TysWiredIn	( tupleTyCon, listTyCon, parrTyCon, charTyCon )
import Name		( Name, getName, isTyVarName )
import NameSet
import BasicTypes	( Boxity )
import SrcLoc		( Located(..), unLoc )
\end{code}

%************************************************************************
%*									*
\subsection{Free variables}
%*									*
%************************************************************************

These free-variable finders returns tycons and classes too.

\begin{code}
charTyCon_name, listTyCon_name, parrTyCon_name :: Name
charTyCon_name    = getName charTyCon
listTyCon_name    = getName listTyCon
parrTyCon_name    = getName parrTyCon

tupleTyCon_name :: Boxity -> Int -> Name
tupleTyCon_name boxity n = getName (tupleTyCon boxity n)

extractHsTyVars :: LHsType Name -> NameSet
extractHsTyVars x = filterNameSet isTyVarName (extractHsTyNames x)

extractFunDepNames :: FunDep Name -> NameSet
extractFunDepNames (ns1, ns2) = mkNameSet ns1 `unionNameSets` mkNameSet ns2

extractHsTyNames   :: LHsType Name -> NameSet
extractHsTyNames ty
  = getl ty
  where
    getl (L _ ty) = get ty

    get (HsAppTy ty1 ty2)      = getl ty1 `unionNameSets` getl ty2
    get (HsListTy ty)          = unitNameSet listTyCon_name `unionNameSets` getl ty
    get (HsPArrTy ty)          = unitNameSet parrTyCon_name `unionNameSets` getl ty
    get (HsTupleTy con tys)    = extractHsTyNames_s tys
    get (HsFunTy ty1 ty2)      = getl ty1 `unionNameSets` getl ty2
    get (HsPredTy p)	       = extractHsPredTyNames p
    get (HsOpTy ty1 op ty2)    = getl ty1 `unionNameSets` getl ty2 `unionNameSets` unitNameSet (unLoc op)
    get (HsParTy ty)           = getl ty
    get (HsBangTy _ ty)        = getl ty
    get (HsNumTy n)            = emptyNameSet
    get (HsTyVar tv)	       = unitNameSet tv
    get (HsSpliceTy _)         = emptyNameSet	-- Type splices mention no type variables
    get (HsKindSig ty k)       = getl ty
    get (HsForAllTy _ tvs 
		    ctxt ty)   = (extractHsCtxtTyNames ctxt
					 `unionNameSets` getl ty)
					    `minusNameSet`
				  mkNameSet (hsLTyVarNames tvs)
    get (HsDocTy ty _)         = getl ty

extractHsTyNames_s  :: [LHsType Name] -> NameSet
extractHsTyNames_s tys = foldr (unionNameSets . extractHsTyNames) emptyNameSet tys

extractHsCtxtTyNames :: LHsContext Name -> NameSet
extractHsCtxtTyNames (L _ ctxt)
  = foldr (unionNameSets . extractHsPredTyNames . unLoc) emptyNameSet ctxt

-- You don't import or export implicit parameters,
-- so don't mention the IP names
extractHsPredTyNames (HsClassP cls tys)
  = unitNameSet cls `unionNameSets` extractHsTyNames_s tys
extractHsPredTyNames (HsEqualP ty1 ty2)
  = extractHsTyNames ty1 `unionNameSets` extractHsTyNames ty2
extractHsPredTyNames (HsIParam n ty)
  = extractHsTyNames ty
\end{code}


%************************************************************************
%*									*
\subsection{Free variables of declarations}
%*									*
%************************************************************************

Return the Names that must be in scope if we are to use this declaration.
In all cases this is set up for interface-file declarations:
	- for class decls we ignore the bindings
	- for instance decls likewise, plus the pragmas
	- for rule decls, we ignore HsRules
        - for data decls, we ignore derivings

	*** See "THE NAMING STORY" in HsDecls ****

\begin{code}
----------------
hsSigsFVs :: [LSig Name] -> FreeVars
hsSigsFVs sigs = plusFVs (map (hsSigFVs.unLoc) sigs)

hsSigFVs (TypeSig v ty)     = extractHsTyNames ty
hsSigFVs (SpecInstSig ty)   = extractHsTyNames ty
hsSigFVs (SpecSig v ty inl) = extractHsTyNames ty
hsSigFVs other		    = emptyFVs

----------------
conDeclFVs (L _ (ConDecl { con_qvars = tyvars, con_cxt = context, 
			   con_details = details, con_res = res_ty}))
  = delFVs (map hsLTyVarName tyvars) $
    extractHsCtxtTyNames context  `plusFV`
    conDetailsFVs details	  `plusFV`
    conResTyFVs res_ty

conResTyFVs ResTyH98       = emptyFVs
conResTyFVs (ResTyGADT ty) = extractHsTyNames ty

conDetailsFVs :: HsConDeclDetails Name -> FreeVars
conDetailsFVs details = plusFVs (map bangTyFVs (hsConDeclArgTys details))

bangTyFVs bty = extractHsTyNames (getBangType bty)
\end{code}


%************************************************************************
%*									*
\subsection{A few functions on generic defintions
%*									*
%************************************************************************

These functions on generics are defined over Matches Name, which is
why they are here and not in HsMatches.

\begin{code}
maybeGenericMatch :: LMatch Name -> Maybe (HsType Name, LMatch Name)
  -- Tells whether a Match is for a generic definition
  -- and extract the type from a generic match and put it at the front

maybeGenericMatch (L loc (Match (L _ (TypePat (L _ ty)) : pats) sig_ty grhss))
  = Just (ty, L loc (Match pats sig_ty grhss))

maybeGenericMatch other_match = Nothing
\end{code}
