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

module VarSet (
	VarSet, IdSet, TyVarSet,
	emptyVarSet, unitVarSet, mkVarSet,
	extendVarSet, extendVarSetList, extendVarSet_C,
	elemVarSet, varSetElems, subVarSet,
	unionVarSet, unionVarSets,
	intersectVarSet, intersectsVarSet, disjointVarSet,
	isEmptyVarSet, delVarSet, delVarSetList, delVarSetByKey,
	minusVarSet, foldVarSet, filterVarSet,
	lookupVarSet, mapVarSet, sizeVarSet, seqVarSet,
	elemVarSetByKey
    ) where

#include "HsVersions.h"

import Var
import Unique
import UniqSet
import UniqFM
\end{code}

%************************************************************************
%*									*
\subsection{@VarSet@s}
%*									*
%************************************************************************

\begin{code}
type VarSet       = UniqSet Var
type IdSet 	  = UniqSet Id
type TyVarSet	  = UniqSet TyVar

emptyVarSet	:: VarSet
intersectVarSet	:: VarSet -> VarSet -> VarSet
unionVarSet	:: VarSet -> VarSet -> VarSet
unionVarSets	:: [VarSet] -> VarSet
varSetElems	:: VarSet -> [Var]
unitVarSet	:: Var -> VarSet
extendVarSet	:: VarSet -> Var -> VarSet
extendVarSetList:: VarSet -> [Var] -> VarSet
elemVarSet	:: Var -> VarSet -> Bool
delVarSet	:: VarSet -> Var -> VarSet
delVarSetList	:: VarSet -> [Var] -> VarSet
minusVarSet	:: VarSet -> VarSet -> VarSet
isEmptyVarSet	:: VarSet -> Bool
mkVarSet	:: [Var] -> VarSet
foldVarSet	:: (Var -> a -> a) -> a -> VarSet -> a
lookupVarSet	:: VarSet -> Var -> Maybe Var
			-- Returns the set element, which may be
			-- (==) to the argument, but not the same as
mapVarSet 	:: (Var -> Var) -> VarSet -> VarSet
sizeVarSet	:: VarSet -> Int
filterVarSet	:: (Var -> Bool) -> VarSet -> VarSet
extendVarSet_C  :: (Var->Var->Var) -> VarSet -> Var -> VarSet

delVarSetByKey	:: VarSet -> Unique -> VarSet
elemVarSetByKey :: Unique -> VarSet -> Bool

emptyVarSet	= emptyUniqSet
unitVarSet	= unitUniqSet
extendVarSet	= addOneToUniqSet
extendVarSetList= addListToUniqSet
intersectVarSet	= intersectUniqSets

intersectsVarSet:: VarSet -> VarSet -> Bool 	-- True if non-empty intersection
disjointVarSet  :: VarSet -> VarSet -> Bool 	-- True if empty intersection
subVarSet	:: VarSet -> VarSet -> Bool	-- True if first arg is subset of second
	-- (s1 `intersectsVarSet` s2) doesn't compute s2 if s1 is empty; 
	-- ditto disjointVarSet, subVarSet

unionVarSet	= unionUniqSets
unionVarSets	= unionManyUniqSets
varSetElems	= uniqSetToList
elemVarSet	= elementOfUniqSet
minusVarSet	= minusUniqSet
delVarSet	= delOneFromUniqSet
delVarSetList	= delListFromUniqSet
isEmptyVarSet	= isEmptyUniqSet
mkVarSet	= mkUniqSet
foldVarSet	= foldUniqSet
lookupVarSet	= lookupUniqSet
mapVarSet	= mapUniqSet
sizeVarSet	= sizeUniqSet
filterVarSet	= filterUniqSet
extendVarSet_C combine s x = addToUFM_C combine s x x
delVarSetByKey	= delFromUFM_Directly	-- Can't be bothered to add this to UniqSet
elemVarSetByKey	= elemUniqSet_Directly
\end{code}

\begin{code}
-- See comments with type signatures
intersectsVarSet s1 s2 = not (s1 `disjointVarSet` s2)
disjointVarSet   s1 s2 = isEmptyVarSet (s1 `intersectVarSet` s2)
subVarSet        s1 s2 = isEmptyVarSet (s1 `minusVarSet` s2)
\end{code}

\begin{code}
seqVarSet :: VarSet -> ()
seqVarSet s = sizeVarSet s `seq` ()
\end{code}

