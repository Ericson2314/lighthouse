\begin{code}
{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Conc
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic concurrency stuff.
-- 
-----------------------------------------------------------------------------

-- No: #hide, because bits of this module are exposed by the stm package.
-- However, we don't want this module to be the home location for the
-- bits it exports, we'd rather have Control.Concurrent and the other
-- higher level modules be the home.  Hence:

#include "Typeable.h"

-- #not-home
module GHC.Conc
	(
	  childHandler  -- :: Exception -> IO ()
	, par  		-- :: a -> b -> b
	, pseq 		-- :: a -> b -> b
        ) where

import Prelude(undefined)

import System.Posix.Types
#if !defined(mingw32_HOST_OS) && !defined(house_HOST_OS)
import System.Posix.Internals
#endif
import Foreign
import Foreign.C

#ifndef __HADDOCK__
import {-# SOURCE #-} GHC.TopHandler ( reportError, reportStackOverflow )
#endif

import Data.Maybe

import GHC.Base
import GHC.IOBase
import GHC.Num		( Num(..) )
import GHC.Real		( fromIntegral, div )
#ifndef mingw32_HOST_OS
import GHC.Base		( Int(..) )
#endif
#ifdef mingw32_HOST_OS
import GHC.Read         ( Read )
import GHC.Enum         ( Enum )
#endif
import GHC.Exception
import GHC.Pack		( packCString# )
import GHC.Ptr          ( Ptr(..), plusPtr, FunPtr(..) )
import GHC.STRef
import GHC.Show		( Show(..), showString )
import Data.Typeable

infixr 0 `par`, `pseq`
\end{code}

%************************************************************************
%*									*
\subsection{@par@}
%*									*
%************************************************************************

\begin{code}
childHandler :: Exception -> IO ()
childHandler err = catchException (real_handler err) childHandler

real_handler :: Exception -> IO ()
real_handler ex =
  case ex of
	-- ignore thread GC and killThread exceptions:
	BlockedOnDeadMVar            -> return ()
	BlockedIndefinitely          -> return ()
	AsyncException ThreadKilled  -> return ()

	-- report all others:
	AsyncException StackOverflow -> reportStackOverflow
	other       -> reportError other

-- 	Nota Bene: 'pseq' used to be 'seq'
--		   but 'seq' is now defined in PrelGHC
--
-- "pseq" is defined a bit weirdly (see below)
--
-- The reason for the strange "lazy" call is that
-- it fools the compiler into thinking that pseq  and par are non-strict in
-- their second argument (even if it inlines pseq at the call site).
-- If it thinks pseq is strict in "y", then it often evaluates
-- "y" before "x", which is totally wrong.  

{-# INLINE pseq  #-}
pseq :: a -> b -> b
pseq  x y = x `seq` lazy y

{-# INLINE par  #-}
par :: a -> b -> b
par  x y = case (par# x) of { _ -> lazy y }
\end{code}

