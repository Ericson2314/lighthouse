%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Id]{@Ids@: Value and constructor identifiers}

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module Id (
	Id, DictId,

	-- Simple construction
	mkGlobalId, mkLocalId, mkLocalIdWithInfo, 
	mkSysLocal, mkUserLocal, mkVanillaGlobal,
	mkTemplateLocals, mkTemplateLocalsNum, mkWildId, mkTemplateLocal,
	mkWorkerId, mkExportedLocalId,

	-- Taking an Id apart
	idName, idType, idUnique, idInfo,
	isId, globalIdDetails, idPrimRep,
	recordSelectorFieldLabel,

	-- Modifying an Id
	setIdName, setIdUnique, Id.setIdType, setIdExported, setIdNotExported, 
	setIdInfo, lazySetIdInfo, modifyIdInfo, maybeModifyIdInfo,
	zapLamIdInfo, zapDemandIdInfo, zapFragileIdInfo,

	-- Predicates
	isImplicitId, isDeadBinder, isDictId, isStrictId,
	isExportedId, isLocalId, isGlobalId,
	isRecordSelector, isNaughtyRecordSelector,
	isClassOpId_maybe,
	isPrimOpId, isPrimOpId_maybe, 
	isFCallId, isFCallId_maybe,
	isDataConWorkId, isDataConWorkId_maybe, isDataConId_maybe, idDataCon,
	isBottomingId, idIsFrom,
        isTickBoxOp, isTickBoxOp_maybe,
	hasNoBinding, 

	-- Inline pragma stuff
	idInlinePragma, setInlinePragma, modifyInlinePragma, 


	-- One shot lambda stuff
	isOneShotBndr, isOneShotLambda, isStateHackType,
	setOneShotLambda, clearOneShotLambda,

	-- IdInfo stuff
	setIdUnfolding,
	setIdArity,
	setIdNewDemandInfo, 
	setIdNewStrictness, zapIdNewStrictness,
	setIdWorkerInfo,
	setIdSpecialisation,
	setIdCafInfo,
	setIdOccInfo,

#ifdef OLD_STRICTNESS
	idDemandInfo, 
	idStrictness, 
	idCprInfo,
	setIdStrictness, 
	setIdDemandInfo, 
	setIdCprInfo,
#endif

	idArity, 
	idNewDemandInfo, idNewDemandInfo_maybe,
	idNewStrictness, idNewStrictness_maybe, 
	idWorkerInfo,
	idUnfolding,
	idSpecialisation, idCoreRules, idHasRules,
	idCafInfo,
	idLBVarInfo,
	idOccInfo,

#ifdef OLD_STRICTNESS
	newStrictnessFromOld 	-- Temporary
#endif

    ) where

#include "HsVersions.h"

import CoreSyn
import BasicTypes
import qualified Var
import Var hiding (mkLocalId, mkGlobalId, mkExportedLocalId)
import TyCon
import Type
import TcType
import TysPrim
import IdInfo 
#ifdef OLD_STRICTNESS
import qualified Demand
#endif
import DataCon
import NewDemand
import Name
import Module
import OccName
import Maybes
import SrcLoc
import Outputable
import Unique
import FastString
import StaticFlags

-- infixl so you can say (id `set` a `set` b)
infixl 	1 `setIdUnfolding`,
	  `setIdArity`,
	  `setIdNewDemandInfo`,
	  `setIdNewStrictness`,
	  `setIdWorkerInfo`,
	  `setIdSpecialisation`,
	  `setInlinePragma`,
	  `idCafInfo`
#ifdef OLD_STRICTNESS
	  ,`idCprInfo`
	  ,`setIdStrictness`
	  ,`setIdDemandInfo`
#endif
\end{code}



%************************************************************************
%*									*
\subsection{Simple Id construction}
%*									*
%************************************************************************

Absolutely all Ids are made by mkId.  It is just like Var.mkId,
but in addition it pins free-tyvar-info onto the Id's type, 
where it can easily be found.

\begin{code}
mkLocalIdWithInfo :: Name -> Type -> IdInfo -> Id
mkLocalIdWithInfo name ty info = Var.mkLocalId name (addFreeTyVars ty) info

mkExportedLocalId :: Name -> Type -> Id
mkExportedLocalId name ty = Var.mkExportedLocalId name (addFreeTyVars ty) vanillaIdInfo

mkGlobalId :: GlobalIdDetails -> Name -> Type -> IdInfo -> Id
mkGlobalId details name ty info = Var.mkGlobalId details name (addFreeTyVars ty) info
\end{code}

\begin{code}
mkLocalId :: Name -> Type -> Id
mkLocalId name ty = mkLocalIdWithInfo name ty vanillaIdInfo

-- SysLocal: for an Id being created by the compiler out of thin air...
mkSysLocal :: FastString -> Unique -> Type -> Id
mkSysLocal fs uniq ty = mkLocalId (mkSystemVarName uniq fs) ty


-- UserLocal: an Id with a name the user might recognize...
mkUserLocal :: OccName -> Unique -> Type -> SrcSpan -> Id
mkVanillaGlobal :: Name -> Type -> IdInfo -> Id

mkUserLocal occ uniq ty loc = mkLocalId (mkInternalName    uniq occ loc) ty
mkVanillaGlobal 	    = mkGlobalId VanillaGlobal
\end{code}

Make some local @Ids@ for a template @CoreExpr@.  These have bogus
@Uniques@, but that's OK because the templates are supposed to be
instantiated before use.
 
\begin{code}
-- "Wild Id" typically used when you need a binder that you don't expect to use
mkWildId :: Type -> Id
mkWildId ty = mkSysLocal FSLIT("wild") (mkBuiltinUnique 1) ty

mkWorkerId :: Unique -> Id -> Type -> Id
-- A worker gets a local name.  CoreTidy will externalise it if necessary.
mkWorkerId uniq unwrkr ty
  = mkLocalId wkr_name ty
  where
    wkr_name = mkInternalName uniq (mkWorkerOcc (getOccName unwrkr)) (getSrcSpan unwrkr)

-- "Template locals" typically used in unfoldings
mkTemplateLocals :: [Type] -> [Id]
mkTemplateLocals tys = zipWith mkTemplateLocal [1..] tys

mkTemplateLocalsNum :: Int -> [Type] -> [Id]
-- The Int gives the starting point for unique allocation
mkTemplateLocalsNum n tys = zipWith mkTemplateLocal [n..] tys

mkTemplateLocal :: Int -> Type -> Id
mkTemplateLocal i ty = mkSysLocal FSLIT("tpl") (mkBuiltinUnique i) ty
\end{code}


%************************************************************************
%*									*
\subsection[Id-general-funs]{General @Id@-related functions}
%*									*
%************************************************************************

\begin{code}
setIdType :: Id -> Type -> Id
	-- Add free tyvar info to the type
setIdType id ty = seqType ty `seq` Var.setIdType id (addFreeTyVars ty)

idPrimRep :: Id -> PrimRep
idPrimRep id = typePrimRep (idType id)
\end{code}


%************************************************************************
%*									*
\subsection{Special Ids}
%*									*
%************************************************************************

\begin{code}
recordSelectorFieldLabel :: Id -> (TyCon, FieldLabel)
recordSelectorFieldLabel id
  = case globalIdDetails id of
	RecordSelId { sel_tycon = tycon, sel_label = lbl } -> (tycon,lbl)
	other -> panic "recordSelectorFieldLabel"

isRecordSelector id = case globalIdDetails id of
			RecordSelId {}  -> True
			other	  	-> False

isNaughtyRecordSelector id = case globalIdDetails id of
			RecordSelId { sel_naughty = n } -> n
			other	  	  	        -> False

isClassOpId_maybe id = case globalIdDetails id of
			ClassOpId cls -> Just cls
			_other        -> Nothing

isPrimOpId id = case globalIdDetails id of
		    PrimOpId op -> True
		    other	-> False

isPrimOpId_maybe id = case globalIdDetails id of
			    PrimOpId op -> Just op
			    other	-> Nothing

isFCallId id = case globalIdDetails id of
		    FCallId call -> True
		    other	 -> False

isFCallId_maybe id = case globalIdDetails id of
			    FCallId call -> Just call
			    other	 -> Nothing

isDataConWorkId id = case globalIdDetails id of
			DataConWorkId _ -> True
			other	        -> False

isDataConWorkId_maybe id = case globalIdDetails id of
			  DataConWorkId con -> Just con
			  other	            -> Nothing

isDataConId_maybe :: Id -> Maybe DataCon
isDataConId_maybe id = case globalIdDetails id of
		 	 DataConWorkId con -> Just con
			 DataConWrapId con -> Just con
			 other		    -> Nothing

idDataCon :: Id -> DataCon
-- Get from either the worker or the wrapper to the DataCon
-- Currently used only in the desugarer
--	 INVARIANT: idDataCon (dataConWrapId d) = d
-- (Remember, dataConWrapId can return either the wrapper or the worker.)
idDataCon id = case globalIdDetails id of
		  DataConWorkId con -> con
		  DataConWrapId con -> con
		  other		    -> pprPanic "idDataCon" (ppr id)


isDictId :: Id -> Bool
isDictId id = isDictTy (idType id)

-- hasNoBinding returns True of an Id which may not have a
-- binding, even though it is defined in this module.  
-- Data constructor workers used to be things of this kind, but
-- they aren't any more.  Instead, we inject a binding for 
-- them at the CorePrep stage. 
-- EXCEPT: unboxed tuples, which definitely have no binding
hasNoBinding id = case globalIdDetails id of
			PrimOpId _  	 -> True
			FCallId _   	 -> True
			DataConWorkId dc -> isUnboxedTupleCon dc
			other	         -> False

isImplicitId :: Id -> Bool
	-- isImplicitId tells whether an Id's info is implied by other
	-- declarations, so we don't need to put its signature in an interface
	-- file, even if it's mentioned in some other interface unfolding.
isImplicitId id
  = case globalIdDetails id of
	RecordSelId {}  -> True
        FCallId _       -> True
        PrimOpId _      -> True
	ClassOpId _	-> True
        DataConWorkId _ -> True
	DataConWrapId _ -> True
		-- These are are implied by their type or class decl;
		-- remember that all type and class decls appear in the interface file.
		-- The dfun id is not an implicit Id; it must *not* be omitted, because 
		-- it carries version info for the instance decl
	other		-> False

idIsFrom :: Module -> Id -> Bool
idIsFrom mod id = nameIsLocalOrFrom mod (idName id)
\end{code}

\begin{code}
isDeadBinder :: Id -> Bool
isDeadBinder bndr | isId bndr = isDeadOcc (idOccInfo bndr)
		  | otherwise = False	-- TyVars count as not dead
\end{code}

\begin{code}
isTickBoxOp :: Id -> Bool
isTickBoxOp id = 
  case globalIdDetails id of
    TickBoxOpId tick -> True
    _                -> False

isTickBoxOp_maybe :: Id -> Maybe TickBoxOp
isTickBoxOp_maybe id = 
  case globalIdDetails id of
    TickBoxOpId tick -> Just tick
    _                -> Nothing
\end{code}

%************************************************************************
%*									*
\subsection{IdInfo stuff}
%*									*
%************************************************************************

\begin{code}
	---------------------------------
	-- ARITY
idArity :: Id -> Arity
idArity id = arityInfo (idInfo id)

setIdArity :: Id -> Arity -> Id
setIdArity id arity = modifyIdInfo (`setArityInfo` arity) id

#ifdef OLD_STRICTNESS
	---------------------------------
	-- (OLD) STRICTNESS 
idStrictness :: Id -> StrictnessInfo
idStrictness id = strictnessInfo (idInfo id)

setIdStrictness :: Id -> StrictnessInfo -> Id
setIdStrictness id strict_info = modifyIdInfo (`setStrictnessInfo` strict_info) id
#endif

-- isBottomingId returns true if an application to n args would diverge
isBottomingId :: Id -> Bool
isBottomingId id = isBottomingSig (idNewStrictness id)

idNewStrictness_maybe :: Id -> Maybe StrictSig
idNewStrictness :: Id -> StrictSig

idNewStrictness_maybe id = newStrictnessInfo (idInfo id)
idNewStrictness       id = idNewStrictness_maybe id `orElse` topSig

setIdNewStrictness :: Id -> StrictSig -> Id
setIdNewStrictness id sig = modifyIdInfo (`setNewStrictnessInfo` Just sig) id

zapIdNewStrictness :: Id -> Id
zapIdNewStrictness id = modifyIdInfo (`setNewStrictnessInfo` Nothing) id
\end{code}

This predicate says whether the id has a strict demand placed on it or
has a type such that it can always be evaluated strictly (e.g., an
unlifted type, but see the comment for isStrictType).  We need to
check separately whether <id> has a so-called "strict type" because if
the demand for <id> hasn't been computed yet but <id> has a strict
type, we still want (isStrictId <id>) to be True.
\begin{code}
isStrictId :: Id -> Bool
isStrictId id
  = ASSERT2( isId id, text "isStrictId: not an id: " <+> ppr id )
           (isStrictDmd (idNewDemandInfo id)) || 
           (isStrictType (idType id))

	---------------------------------
	-- WORKER ID
idWorkerInfo :: Id -> WorkerInfo
idWorkerInfo id = workerInfo (idInfo id)

setIdWorkerInfo :: Id -> WorkerInfo -> Id
setIdWorkerInfo id work_info = modifyIdInfo (`setWorkerInfo` work_info) id

	---------------------------------
	-- UNFOLDING
idUnfolding :: Id -> Unfolding
idUnfolding id = unfoldingInfo (idInfo id)

setIdUnfolding :: Id -> Unfolding -> Id
setIdUnfolding id unfolding = modifyIdInfo (`setUnfoldingInfo` unfolding) id

#ifdef OLD_STRICTNESS
	---------------------------------
	-- (OLD) DEMAND
idDemandInfo :: Id -> Demand.Demand
idDemandInfo id = demandInfo (idInfo id)

setIdDemandInfo :: Id -> Demand.Demand -> Id
setIdDemandInfo id demand_info = modifyIdInfo (`setDemandInfo` demand_info) id
#endif

idNewDemandInfo_maybe :: Id -> Maybe NewDemand.Demand
idNewDemandInfo       :: Id -> NewDemand.Demand

idNewDemandInfo_maybe id = newDemandInfo (idInfo id)
idNewDemandInfo       id = newDemandInfo (idInfo id) `orElse` NewDemand.topDmd

setIdNewDemandInfo :: Id -> NewDemand.Demand -> Id
setIdNewDemandInfo id dmd = modifyIdInfo (`setNewDemandInfo` Just dmd) id

	---------------------------------
	-- SPECIALISATION
idSpecialisation :: Id -> SpecInfo
idSpecialisation id = specInfo (idInfo id)

idCoreRules :: Id -> [CoreRule]
idCoreRules id = specInfoRules (idSpecialisation id)

idHasRules :: Id -> Bool
idHasRules id = not (isEmptySpecInfo (idSpecialisation id))

setIdSpecialisation :: Id -> SpecInfo -> Id
setIdSpecialisation id spec_info = modifyIdInfo (`setSpecInfo` spec_info) id

	---------------------------------
	-- CAF INFO
idCafInfo :: Id -> CafInfo
#ifdef OLD_STRICTNESS
idCafInfo id = case cgInfo (idInfo id) of
		  NoCgInfo -> pprPanic "idCafInfo" (ppr id)
		  info     -> cgCafInfo info
#else
idCafInfo id = cafInfo (idInfo id)
#endif

setIdCafInfo :: Id -> CafInfo -> Id
setIdCafInfo id caf_info = modifyIdInfo (`setCafInfo` caf_info) id

	---------------------------------
	-- CPR INFO
#ifdef OLD_STRICTNESS
idCprInfo :: Id -> CprInfo
idCprInfo id = cprInfo (idInfo id)

setIdCprInfo :: Id -> CprInfo -> Id
setIdCprInfo id cpr_info = modifyIdInfo (`setCprInfo` cpr_info) id
#endif

	---------------------------------
	-- Occcurrence INFO
idOccInfo :: Id -> OccInfo
idOccInfo id = occInfo (idInfo id)

setIdOccInfo :: Id -> OccInfo -> Id
setIdOccInfo id occ_info = modifyIdInfo (`setOccInfo` occ_info) id
\end{code}


	---------------------------------
	-- INLINING
The inline pragma tells us to be very keen to inline this Id, but it's still
OK not to if optimisation is switched off.

\begin{code}
idInlinePragma :: Id -> InlinePragInfo
idInlinePragma id = inlinePragInfo (idInfo id)

setInlinePragma :: Id -> InlinePragInfo -> Id
setInlinePragma id prag = modifyIdInfo (`setInlinePragInfo` prag) id

modifyInlinePragma :: Id -> (InlinePragInfo -> InlinePragInfo) -> Id
modifyInlinePragma id fn = modifyIdInfo (\info -> info `setInlinePragInfo` (fn (inlinePragInfo info))) id
\end{code}


	---------------------------------
	-- ONE-SHOT LAMBDAS
\begin{code}
idLBVarInfo :: Id -> LBVarInfo
idLBVarInfo id = lbvarInfo (idInfo id)

isOneShotBndr :: Id -> Bool
-- This one is the "business end", called externally.
-- Its main purpose is to encapsulate the Horrible State Hack
isOneShotBndr id = isOneShotLambda id || isStateHackType (idType id)

isStateHackType :: Type -> Bool
isStateHackType ty
  | opt_NoStateHack 
  = False
  | otherwise
  = case splitTyConApp_maybe ty of
	Just (tycon,_) -> tycon == statePrimTyCon
        other          -> False
	-- This is a gross hack.  It claims that 
	-- every function over realWorldStatePrimTy is a one-shot
	-- function.  This is pretty true in practice, and makes a big
	-- difference.  For example, consider
	--	a `thenST` \ r -> ...E...
	-- The early full laziness pass, if it doesn't know that r is one-shot
	-- will pull out E (let's say it doesn't mention r) to give
	--	let lvl = E in a `thenST` \ r -> ...lvl...
	-- When `thenST` gets inlined, we end up with
	--	let lvl = E in \s -> case a s of (r, s') -> ...lvl...
	-- and we don't re-inline E.
	--
	-- It would be better to spot that r was one-shot to start with, but
	-- I don't want to rely on that.
	--
	-- Another good example is in fill_in in PrelPack.lhs.  We should be able to
	-- spot that fill_in has arity 2 (and when Keith is done, we will) but we can't yet.


-- The OneShotLambda functions simply fiddle with the IdInfo flag
isOneShotLambda :: Id -> Bool
isOneShotLambda id = case idLBVarInfo id of
                       IsOneShotLambda  -> True
                       NoLBVarInfo      -> False

setOneShotLambda :: Id -> Id
setOneShotLambda id = modifyIdInfo (`setLBVarInfo` IsOneShotLambda) id

clearOneShotLambda :: Id -> Id
clearOneShotLambda id 
  | isOneShotLambda id = modifyIdInfo (`setLBVarInfo` NoLBVarInfo) id
  | otherwise	       = id			

-- But watch out: this may change the type of something else
--	f = \x -> e
-- If we change the one-shot-ness of x, f's type changes
\end{code}

\begin{code}
zapInfo :: (IdInfo -> Maybe IdInfo) -> Id -> Id
zapInfo zapper id = maybeModifyIdInfo (zapper (idInfo id)) id

zapLamIdInfo :: Id -> Id
zapLamIdInfo = zapInfo zapLamInfo

zapDemandIdInfo = zapInfo zapDemandInfo

zapFragileIdInfo :: Id -> Id
zapFragileIdInfo = zapInfo zapFragileInfo 
\end{code}

