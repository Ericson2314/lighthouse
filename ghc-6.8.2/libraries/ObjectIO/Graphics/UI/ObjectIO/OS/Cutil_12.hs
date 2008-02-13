{-# OPTIONS_GHC -cpp #-}

-- #hide
-----------------------------------------------------------------------------
-- Module      :  OS.Cutil_12
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module contains some additional routines required for marshalling 
-- Haskell arguments to OS C calling routines.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.Cutil_12
		( addr2int, int2addr, fpeek, Storable(..), free, malloc
		, module Data.Int
		, module Data.Bits
		, module Foreign.Ptr
		, module Foreign.C.String
		) where


import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String
import Data.Int
import Data.Bits
import System.IO.Unsafe

import GHC.Ptr		( Ptr(..) )
import GHC.Base

--	Conversion operations:

addr2int :: Ptr a -> Int
addr2int (Ptr x) = I# (addr2Int# x)

int2addr :: Int -> Ptr a
int2addr (I# x) = Ptr (int2Addr# x)


--	fpeek addr first peeks addr, then frees addr:
fpeek :: (Storable a) => Ptr a -> IO a
fpeek addr
	= do {
		x <- peek addr;
		free addr;
		return x
	  }
