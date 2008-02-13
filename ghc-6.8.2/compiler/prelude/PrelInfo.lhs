%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrelInfo]{The @PrelInfo@ interface to the compiler's prelude knowledge}

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module PrelInfo (
	module MkId,

	ghcPrimExports,
	wiredInThings, basicKnownKeyNames,
	primOpId,
	
	-- Random other things
	maybeCharLikeCon, maybeIntLikeCon,

	-- Class categories
	isNumericClass, isStandardClass

    ) where

#include "HsVersions.h"

import PrelNames	( basicKnownKeyNames, 
			  hasKey, charDataConKey, intDataConKey,
			  numericClassKeys, standardClassKeys )

import PrimOp		( PrimOp, allThePrimOps, primOpOcc, primOpTag, maxPrimOpTag )
import DataCon		( DataCon )
import Id		( Id, idName )
import MkId		-- All of it, for re-export
import Name		( nameOccName )
import TysPrim		( primTyCons )
import TysWiredIn	( wiredInTyCons )
import HscTypes 	( TyThing(..), implicitTyThings, GenAvailInfo(..), RdrAvailInfo )
import Class	 	( Class, classKey )
import Type		( funTyCon )
import TyCon		( tyConName )
import Util		( isIn )

import Array		( Array, array, (!) )
\end{code}

%************************************************************************
%*									*
\subsection[builtinNameInfo]{Lookup built-in names}
%*									*
%************************************************************************

Notes about wired in things
~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Wired-in things are Ids/TyCons that are completely known to the compiler.
  They are global values in GHC, (e.g.  listTyCon :: TyCon).

* A wired in Name contains the thing itself inside the Name: 
	see Name.wiredInNameTyThing_maybe
  (E.g. listTyConName contains listTyCon. 

* The name cache is initialised with (the names of) all wired-in things

* The type checker sees if the Name is wired in before looking up 
  the name in the type environment.  So the type envt itself contains
  no wired in things.

* MkIface prunes out wired-in things before putting them in an interface file.
  So interface files never contain wired-in things.


\begin{code}
wiredInThings :: [TyThing]
-- This list is used only to initialise HscMain.knownKeyNames
-- to ensure that when you say "Prelude.map" in your source code, you
-- get a Name with the correct known key
wiredInThings		
  = concat
    [		-- Wired in TyCons and their implicit Ids
	  tycon_things
	, concatMap implicitTyThings tycon_things

		-- Wired in Ids
	, map AnId wiredInIds

		-- PrimOps
	, map (AnId . mkPrimOpId) allThePrimOps
    ]
  where
    tycon_things = map ATyCon ([funTyCon] ++ primTyCons ++ wiredInTyCons)
\end{code}

We let a lot of "non-standard" values be visible, so that we can make
sense of them in interface pragmas. It's cool, though they all have
"non-standard" names, so they won't get past the parser in user code.

%************************************************************************
%*									*
		PrimOpIds
%*									*
%************************************************************************

\begin{code}
primOpIds :: Array Int Id	-- Indexed by PrimOp tag
primOpIds = array (1,maxPrimOpTag) [ (primOpTag op, mkPrimOpId op) 
				   | op <- allThePrimOps]

primOpId :: PrimOp -> Id
primOpId op = primOpIds ! primOpTag op
\end{code}


%************************************************************************
%*									*
\subsection{Export lists for pseudo-modules (GHC.Prim)}
%*									*
%************************************************************************

GHC.Prim "exports" all the primops and primitive types, some 
wired-in Ids.

\begin{code}
ghcPrimExports :: [RdrAvailInfo]
ghcPrimExports
 = map (Avail . nameOccName . idName) ghcPrimIds ++
   map (Avail . primOpOcc) allThePrimOps ++
   [ AvailTC occ [occ] |
     n <- funTyCon : primTyCons, let occ = nameOccName (tyConName n) 
   ]
\end{code}


%************************************************************************
%*									*
\subsection{Built-in keys}
%*									*
%************************************************************************

ToDo: make it do the ``like'' part properly (as in 0.26 and before).

\begin{code}
maybeCharLikeCon, maybeIntLikeCon :: DataCon -> Bool
maybeCharLikeCon con = con `hasKey` charDataConKey
maybeIntLikeCon  con = con `hasKey` intDataConKey
\end{code}


%************************************************************************
%*									*
\subsection{Class predicates}
%*									*
%************************************************************************

\begin{code}
isNumericClass, isStandardClass :: Class -> Bool

isNumericClass     clas = classKey clas `is_elem` numericClassKeys
isStandardClass    clas = classKey clas `is_elem` standardClassKeys
is_elem = isIn "is_X_Class"
\end{code}
