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

module UniqSupply (

	UniqSupply,		-- Abstractly

	uniqFromSupply, uniqsFromSupply,	-- basic ops

	UniqSM,		-- type: unique supply monad
	initUs, initUs_, thenUs, thenUs_, returnUs, fixUs, getUs, withUs,
	getUniqueUs, getUniquesUs,
	mapUs, mapAndUnzipUs, mapAndUnzip3Us,
	thenMaybeUs, mapAccumLUs,
	lazyThenUs, lazyMapUs,

	mkSplitUniqSupply,
	splitUniqSupply, listSplitUniqSupply
  ) where

#include "HsVersions.h"

import Unique

import GHC.Exts
import System.IO.Unsafe	( unsafeInterleaveIO )

#if __GLASGOW_HASKELL__ >= 607
import GHC.IOBase (unsafeDupableInterleaveIO)
#else
unsafeDupableInterleaveIO :: IO a -> IO a
unsafeDupableInterleaveIO = unsafeInterleaveIO
#endif

w2i x = word2Int# x
i2w x = int2Word# x
i2w_s x = (x :: Int#)
\end{code}


%************************************************************************
%*									*
\subsection{Splittable Unique supply: @UniqSupply@}
%*									*
%************************************************************************

A value of type @UniqSupply@ is unique, and it can
supply {\em one} distinct @Unique@.  Also, from the supply, one can
also manufacture an arbitrary number of further @UniqueSupplies@,
which will be distinct from the first and from all others.

\begin{code}
data UniqSupply
  = MkSplitUniqSupply Int#	-- make the Unique with this
		   UniqSupply UniqSupply
				-- when split => these two supplies
\end{code}

\begin{code}
mkSplitUniqSupply :: Char -> IO UniqSupply

splitUniqSupply :: UniqSupply -> (UniqSupply, UniqSupply)
listSplitUniqSupply :: UniqSupply -> [UniqSupply]   -- Infinite
uniqFromSupply  :: UniqSupply -> Unique
uniqsFromSupply :: UniqSupply -> [Unique]	-- Infinite
\end{code}

\begin{code}
mkSplitUniqSupply (C# c#)
  = let
	mask# = (i2w (ord# c#)) `uncheckedShiftL#` (i2w_s 24#)
	-- here comes THE MAGIC:

	-- This is one of the most hammered bits in the whole compiler
	mk_supply#
	  = unsafeDupableInterleaveIO (
		genSymZh    >>= \ (I# u#) ->
		mk_supply#  >>= \ s1 ->
		mk_supply#  >>= \ s2 ->
		return (MkSplitUniqSupply (w2i (mask# `or#` (i2w u#))) s1 s2)
	    )
    in
    mk_supply#

foreign import ccall unsafe "genSymZh" genSymZh :: IO Int

splitUniqSupply (MkSplitUniqSupply _ s1 s2) = (s1, s2)
listSplitUniqSupply  (MkSplitUniqSupply _ s1 s2) = s1 : listSplitUniqSupply s2
\end{code}

\begin{code}
uniqFromSupply  (MkSplitUniqSupply n _ _)  = mkUniqueGrimily (I# n)
uniqsFromSupply (MkSplitUniqSupply n _ s2) = mkUniqueGrimily (I# n) : uniqsFromSupply s2
\end{code}

%************************************************************************
%*									*
\subsubsection[UniqSupply-monad]{@UniqSupply@ monad: @UniqSM@}
%*									*
%************************************************************************

\begin{code}
newtype UniqSM result = USM { unUSM :: UniqSupply -> (result, UniqSupply) }

instance Monad UniqSM where
  return = returnUs
  (>>=) = thenUs
  (>>)  = thenUs_

-- the initUs function also returns the final UniqSupply; initUs_ drops it
initUs :: UniqSupply -> UniqSM a -> (a,UniqSupply)
initUs init_us m = case unUSM m init_us of { (r,us) -> (r,us) }

initUs_ :: UniqSupply -> UniqSM a -> a
initUs_ init_us m = case unUSM m init_us of { (r,us) -> r }

{-# INLINE thenUs #-}
{-# INLINE lazyThenUs #-}
{-# INLINE returnUs #-}
{-# INLINE splitUniqSupply #-}
\end{code}

@thenUs@ is where we split the @UniqSupply@.
\begin{code}
fixUs :: (a -> UniqSM a) -> UniqSM a
fixUs m = USM (\us -> let (r,us') = unUSM (m r) us in (r,us'))

thenUs :: UniqSM a -> (a -> UniqSM b) -> UniqSM b
thenUs (USM expr) cont
  = USM (\us -> case (expr us) of 
		   (result, us') -> unUSM (cont result) us')

lazyThenUs :: UniqSM a -> (a -> UniqSM b) -> UniqSM b
lazyThenUs (USM expr) cont
  = USM (\us -> let (result, us') = expr us in unUSM (cont result) us')

thenUs_ :: UniqSM a -> UniqSM b -> UniqSM b
thenUs_ (USM expr) (USM cont)
  = USM (\us -> case (expr us) of { (_, us') -> cont us' })


returnUs :: a -> UniqSM a
returnUs result = USM (\us -> (result, us))

withUs :: (UniqSupply -> (a, UniqSupply)) -> UniqSM a
withUs f = USM (\us -> f us)	-- Ha ha!
		
getUs :: UniqSM UniqSupply
getUs = USM (\us -> splitUniqSupply us)

getUniqueUs :: UniqSM Unique
getUniqueUs = USM (\us -> case splitUniqSupply us of
			   (us1,us2) -> (uniqFromSupply us1, us2))

getUniquesUs :: UniqSM [Unique]
getUniquesUs = USM (\us -> case splitUniqSupply us of
			      (us1,us2) -> (uniqsFromSupply us1, us2))
\end{code}

\begin{code}
mapUs :: (a -> UniqSM b) -> [a] -> UniqSM [b]
mapUs f []     = returnUs []
mapUs f (x:xs)
  = f x         `thenUs` \ r  ->
    mapUs f xs  `thenUs` \ rs ->
    returnUs (r:rs)

lazyMapUs :: (a -> UniqSM b) -> [a] -> UniqSM [b]
lazyMapUs f []     = returnUs []
lazyMapUs f (x:xs)
  = f x             `lazyThenUs` \ r  ->
    lazyMapUs f xs  `lazyThenUs` \ rs ->
    returnUs (r:rs)

mapAndUnzipUs  :: (a -> UniqSM (b,c))   -> [a] -> UniqSM ([b],[c])
mapAndUnzip3Us :: (a -> UniqSM (b,c,d)) -> [a] -> UniqSM ([b],[c],[d])

mapAndUnzipUs f [] = returnUs ([],[])
mapAndUnzipUs f (x:xs)
  = f x		    	`thenUs` \ (r1,  r2)  ->
    mapAndUnzipUs f xs	`thenUs` \ (rs1, rs2) ->
    returnUs (r1:rs1, r2:rs2)

mapAndUnzip3Us f [] = returnUs ([],[],[])
mapAndUnzip3Us f (x:xs)
  = f x		    	`thenUs` \ (r1,  r2,  r3)  ->
    mapAndUnzip3Us f xs	`thenUs` \ (rs1, rs2, rs3) ->
    returnUs (r1:rs1, r2:rs2, r3:rs3)

thenMaybeUs :: UniqSM (Maybe a) -> (a -> UniqSM (Maybe b)) -> UniqSM (Maybe b)
thenMaybeUs m k
  = m	`thenUs` \ result ->
    case result of
      Nothing -> returnUs Nothing
      Just x  -> k x

mapAccumLUs :: (acc -> x -> UniqSM (acc, y))
	    -> acc
	    -> [x]
	    -> UniqSM (acc, [y])

mapAccumLUs f b []     = returnUs (b, [])
mapAccumLUs f b (x:xs)
  = f b x   	        	    `thenUs` \ (b__2, x__2) ->
    mapAccumLUs f b__2 xs   	    `thenUs` \ (b__3, xs__2) ->
    returnUs (b__3, x__2:xs__2)
\end{code}
