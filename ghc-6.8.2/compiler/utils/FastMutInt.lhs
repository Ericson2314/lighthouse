{-# OPTIONS -cpp #-}
--
-- (c) The University of Glasgow 2002-2006
--
-- Unboxed mutable Ints

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module FastMutInt(
	FastMutInt, newFastMutInt,
	readFastMutInt, writeFastMutInt
  ) where

#include "HsVersions.h"

#ifdef __GLASGOW_HASKELL__

#include "MachDeps.h"
#ifndef SIZEOF_HSINT
#define SIZEOF_HSINT  INT_SIZE_IN_BYTES
#endif

import GHC.Base
import GHC.IOBase

#else /* ! __GLASGOW_HASKELL__ */

import Data.IORef

#endif

newFastMutInt :: IO FastMutInt
readFastMutInt :: FastMutInt -> IO Int
writeFastMutInt :: FastMutInt -> Int -> IO ()
\end{code}

\begin{code}
#ifdef __GLASGOW_HASKELL__
data FastMutInt = FastMutInt (MutableByteArray# RealWorld)

newFastMutInt = IO $ \s ->
  case newByteArray# size s of { (# s, arr #) ->
  (# s, FastMutInt arr #) }
  where I# size = SIZEOF_HSINT

readFastMutInt (FastMutInt arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  (# s, I# i #) }

writeFastMutInt (FastMutInt arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of { s ->
  (# s, () #) }
#else /* ! __GLASGOW_HASKELL__ */
--maybe someday we could use
--http://haskell.org/haskellwiki/Library/ArrayRef
--which has an implementation of IOURefs
--that is unboxed in GHC and just strict in all other compilers...
newtype FastMutInt = FastMutInt (IORef Int)

-- If any default value was chosen, it surely would be 0,
-- so we will use that since IORef requires a default value.
-- Or maybe it would be more interesting to package an error,
-- assuming nothing relies on being able to read a bogus Int?
-- That could interfere with its strictness for smart optimizers
-- (are they allowed to optimize a 'newtype' that way?) ...
-- Well, maybe that can be added (in DEBUG?) later.
newFastMutInt = fmap FastMutInt (newIORef 0)

readFastMutInt (FastMutInt ioRefInt) = readIORef ioRefInt

-- FastMutInt is strict in the value it contains.
writeFastMutInt (FastMutInt ioRefInt) i = i `seq` writeIORef ioRefInt i
#endif
\end{code}

