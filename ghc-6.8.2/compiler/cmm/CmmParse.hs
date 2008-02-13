{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module CmmParse ( parseCmmFile ) where

import CgMonad
import CgHeapery
import CgUtils
import CgProf
import CgTicky
import CgInfoTbls
import CgForeignCall
import CgTailCall
import CgStackery
import ClosureInfo
import CgCallConv
import CgClosure
import CostCentre

import Cmm
import PprCmm
import CmmUtils
import CmmLex
import CLabel
import MachOp
import SMRep
import Lexer

import ForeignCall
import Literal
import Unique
import UniqFM
import SrcLoc
import DynFlags
import StaticFlags
import ErrUtils
import StringBuffer
import FastString
import Panic
import Constants
import Outputable

import Control.Monad
import Data.Char	( ord )
import System.Exit

#include "HsVersions.h"
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
#else
import Array
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

-- parser produced by Happy Version 1.16

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = GHC.Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: (ExtCode) -> (HappyAbsSyn )
happyIn4 x = unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (ExtCode)
happyOut4 x = unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (ExtCode) -> (HappyAbsSyn )
happyIn5 x = unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (ExtCode)
happyOut5 x = unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (ExtCode) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (ExtCode)
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: ([ExtFCode [CmmStatic]]) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> ([ExtFCode [CmmStatic]])
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (ExtFCode [CmmStatic]) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (ExtFCode [CmmStatic])
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ([ExtFCode CmmExpr]) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ([ExtFCode CmmExpr])
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (ExtCode) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (ExtCode)
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (ExtFCode (CLabel, CmmInfoTable, [Maybe LocalReg])) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (ExtFCode (CLabel, CmmInfoTable, [Maybe LocalReg]))
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (ExtCode) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (ExtCode)
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (ExtCode) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (ExtCode)
happyOut13 x = unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([FastString]) -> (HappyAbsSyn )
happyIn14 x = unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([FastString])
happyOut14 x = unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (ExtCode) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (ExtCode)
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (CmmReturnInfo) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (CmmReturnInfo)
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (ExtFCode BoolExpr) -> (HappyAbsSyn )
happyIn17 x = unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (ExtFCode BoolExpr)
happyOut17 x = unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (ExtFCode BoolExpr) -> (HappyAbsSyn )
happyIn18 x = unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (ExtFCode BoolExpr)
happyOut18 x = unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (CmmSafety) -> (HappyAbsSyn )
happyIn19 x = unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (CmmSafety)
happyOut19 x = unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Maybe [GlobalReg]) -> (HappyAbsSyn )
happyIn20 x = unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Maybe [GlobalReg])
happyOut20 x = unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ([GlobalReg]) -> (HappyAbsSyn )
happyIn21 x = unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ([GlobalReg])
happyOut21 x = unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Maybe (Int,Int)) -> (HappyAbsSyn )
happyIn22 x = unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (Maybe (Int,Int))
happyOut22 x = unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: ([([Int],ExtCode)]) -> (HappyAbsSyn )
happyIn23 x = unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> ([([Int],ExtCode)])
happyOut23 x = unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (([Int],ExtCode)) -> (HappyAbsSyn )
happyIn24 x = unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (([Int],ExtCode))
happyOut24 x = unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: ([Int]) -> (HappyAbsSyn )
happyIn25 x = unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> ([Int])
happyOut25 x = unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (Maybe ExtCode) -> (HappyAbsSyn )
happyIn26 x = unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (Maybe ExtCode)
happyOut26 x = unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (ExtCode) -> (HappyAbsSyn )
happyIn27 x = unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (ExtCode)
happyOut27 x = unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (ExtFCode CmmExpr) -> (HappyAbsSyn )
happyIn28 x = unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (ExtFCode CmmExpr)
happyOut28 x = unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (ExtFCode CmmExpr) -> (HappyAbsSyn )
happyIn29 x = unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (ExtFCode CmmExpr)
happyOut29 x = unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (MachRep) -> (HappyAbsSyn )
happyIn30 x = unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (MachRep)
happyOut30 x = unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: ([ExtFCode (CmmExpr, MachHint)]) -> (HappyAbsSyn )
happyIn31 x = unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> ([ExtFCode (CmmExpr, MachHint)])
happyOut31 x = unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: ([ExtFCode (CmmExpr, MachHint)]) -> (HappyAbsSyn )
happyIn32 x = unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> ([ExtFCode (CmmExpr, MachHint)])
happyOut32 x = unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: ([ExtFCode (CmmExpr, MachHint)]) -> (HappyAbsSyn )
happyIn33 x = unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> ([ExtFCode (CmmExpr, MachHint)])
happyOut33 x = unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (ExtFCode (CmmExpr, MachHint)) -> (HappyAbsSyn )
happyIn34 x = unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (ExtFCode (CmmExpr, MachHint))
happyOut34 x = unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: ([ExtFCode CmmExpr]) -> (HappyAbsSyn )
happyIn35 x = unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> ([ExtFCode CmmExpr])
happyOut35 x = unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: ([ExtFCode CmmExpr]) -> (HappyAbsSyn )
happyIn36 x = unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> ([ExtFCode CmmExpr])
happyOut36 x = unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (ExtFCode CmmExpr) -> (HappyAbsSyn )
happyIn37 x = unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (ExtFCode CmmExpr)
happyOut37 x = unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: ([ExtFCode (CmmFormal, MachHint)]) -> (HappyAbsSyn )
happyIn38 x = unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> ([ExtFCode (CmmFormal, MachHint)])
happyOut38 x = unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: ([ExtFCode (CmmFormal, MachHint)]) -> (HappyAbsSyn )
happyIn39 x = unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> ([ExtFCode (CmmFormal, MachHint)])
happyOut39 x = unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (ExtFCode (CmmFormal, MachHint)) -> (HappyAbsSyn )
happyIn40 x = unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (ExtFCode (CmmFormal, MachHint))
happyOut40 x = unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (ExtFCode LocalReg) -> (HappyAbsSyn )
happyIn41 x = unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (ExtFCode LocalReg)
happyOut41 x = unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (ExtFCode CmmReg) -> (HappyAbsSyn )
happyIn42 x = unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (ExtFCode CmmReg)
happyOut42 x = unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: ([ExtFCode LocalReg]) -> (HappyAbsSyn )
happyIn43 x = unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> ([ExtFCode LocalReg])
happyOut43 x = unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: ([ExtFCode LocalReg]) -> (HappyAbsSyn )
happyIn44 x = unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> ([ExtFCode LocalReg])
happyOut44 x = unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: ([ExtFCode LocalReg]) -> (HappyAbsSyn )
happyIn45 x = unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> ([ExtFCode LocalReg])
happyOut45 x = unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (ExtFCode LocalReg) -> (HappyAbsSyn )
happyIn46 x = unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (ExtFCode LocalReg)
happyOut46 x = unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (ExtFCode (Maybe UpdateFrame)) -> (HappyAbsSyn )
happyIn47 x = unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (ExtFCode (Maybe UpdateFrame))
happyOut47 x = unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (ExtFCode (Maybe BlockId)) -> (HappyAbsSyn )
happyIn48 x = unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (ExtFCode (Maybe BlockId))
happyOut48 x = unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (MachRep) -> (HappyAbsSyn )
happyIn49 x = unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (MachRep)
happyOut49 x = unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: (MachRep) -> (HappyAbsSyn )
happyIn50 x = unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> (MachRep)
happyOut50 x = unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyInTok :: Located CmmToken -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> Located CmmToken
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x3f\x01\x00\x00\x8e\x03\x3f\x01\x00\x00\x00\x00\xc9\x03\x00\x00\x90\x03\x00\x00\xc7\x03\xc4\x03\xb0\x03\xaf\x03\xae\x03\x9b\x03\x75\x03\x61\x03\x64\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\x03\x9d\x03\x4f\x03\x63\x03\x35\x01\x8b\x03\x47\x03\x78\x03\x5a\x03\x3b\x03\x3a\x03\x2c\x03\x29\x03\x28\x03\x22\x03\x4a\x03\x07\x00\x00\x00\x0f\x03\x00\x00\x0c\x03\x00\x00\x45\x03\x44\x03\x43\x03\x3d\x03\x39\x03\x2b\x03\x00\x00\xee\xff\x06\x03\x00\x00\x36\x03\x00\x00\x1d\x03\xf8\x02\x9d\x03\x02\x03\x2d\x03\x00\x00\x26\x03\x5e\x00\xe6\x02\x00\x00\x35\x01\x00\x00\x00\x00\x1e\x03\xee\xff\xff\xff\x1c\x03\x0d\x03\xda\x02\x01\x03\xfe\x02\xd6\x02\xd2\x02\xcc\x02\xc8\x02\xc4\x02\xbc\x02\x00\x00\xf4\x02\x19\x00\xe2\x02\xdb\x02\x0b\x00\xd7\x02\xd4\x02\xcd\x02\x00\x00\x02\x00\xe7\x02\x9d\x02\x9e\x02\x9c\x01\xd1\x02\x00\x00\xd0\x02\x00\x00\x5e\x00\x5e\x00\x92\x02\x5e\x00\x00\x00\x00\x00\x00\x00\xb5\x02\xb5\x02\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x02\x19\x00\xc9\x02\x19\x00\x19\x00\xe5\xff\xbe\x02\x0d\x00\x00\x00\x97\x00\x84\x02\x53\x00\x5e\x00\xb9\x02\xba\x02\x00\x00\x0e\x00\x5e\x00\x5e\x00\x5e\x00\x5e\x00\x5e\x00\x5e\x00\x5e\x00\x5e\x00\x5e\x00\x5e\x00\x5e\x00\x5e\x00\x5e\x00\x5e\x00\x5e\x00\x5e\x00\x5e\x00\x00\x00\x9d\x03\x00\x00\x5a\x00\xb2\x02\x00\x00\x47\x02\x5e\x00\x73\x02\x00\x00\xab\x02\x93\x02\x00\x00\x5c\x02\x8f\x02\x74\x02\x6a\x02\x63\x02\x00\x00\x35\x01\x62\x02\x8e\x02\x5e\x00\x90\x02\x00\x00\x76\x03\x8a\x02\x71\x02\x7e\x02\x6f\x02\x6c\x02\x5b\x02\x68\x02\x67\x02\x57\x02\x5e\x02\x54\x02\xe4\x01\x00\x00\x5e\x00\x00\x00\x9e\x03\x9e\x03\x9e\x03\x9e\x03\xc5\x01\xc5\x01\x9e\x03\x9e\x03\xb2\x03\xb9\x03\x43\x01\x5a\x00\x5a\x00\x00\x00\x00\x00\x00\x00\x62\x03\x55\x02\x00\x00\x00\x00\x5e\x00\x5e\x00\x10\x02\x50\x02\x5e\x00\x16\x02\x04\x00\x00\x00\x8a\x03\x53\x00\x53\x00\x4e\x02\x40\x02\x31\x02\x00\x00\x00\x00\x07\x02\x5e\x00\x5e\x00\x05\x02\x2c\x02\x00\x00\x00\x00\x00\x00\xfa\x01\x5e\x00\x88\x01\xca\x01\x00\x00\x97\x00\x2e\x02\x00\x00\x00\x00\xa3\x00\x30\x02\x47\x02\x19\x00\x53\x00\x53\x00\x2d\x02\x95\x00\x26\x02\x00\x00\x17\x02\x00\x00\x14\x02\xb0\x01\x19\x02\x00\x00\x5e\x00\x18\x02\x00\x00\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc7\x01\xde\x01\xdd\x01\x00\x00\xd2\x01\x00\x00\x00\x00\x00\x02\xff\x01\xfe\x01\xf2\x01\x00\x00\x00\x00\x00\x00\x04\x02\xcf\x01\xbc\x01\x5e\x00\x00\x00\x00\x00\x00\x00\xa3\x00\xdc\x01\xf7\x01\x00\x00\x00\x00\x00\x00\xf1\x01\x00\x00\xfd\x01\xe8\x01\x5e\x00\x5e\x00\x5e\x00\xc6\x01\x00\x00\xe7\x01\xac\x01\xb6\x01\xaa\x01\x00\x00\xa2\x01\x9f\x01\x89\x01\x86\x01\xcb\x01\xb2\x01\xb1\x01\xad\x01\xb3\x01\x8b\x01\x00\x00\xae\x01\x9e\x01\x00\x00\x00\x00\x9d\x01\x74\x01\x85\x01\x84\x01\x4b\x01\x4b\x01\x00\x00\x19\x00\x7a\x01\x00\x00\x26\x01\x5d\x01\x00\x00\x34\x01\x2a\x01\x1c\x01\x51\x01\x42\x01\x37\x01\x19\x00\x00\x00\x19\x00\x46\x01\x44\x01\x00\x00\x44\x01\x45\x01\x06\x00\x12\x01\x00\x00\x3d\x01\x38\x01\xf9\x00\xf5\x00\x00\x00\x09\x00\x30\x01\x00\x00\x00\x00\x2e\x01\xf7\x00\x20\x01\x00\x00\x0c\x01\x00\x00\xde\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xda\x00\x11\x01\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xbf\x00\x00\x00\x00\x00\xb5\x00\x00\x00\x00\x00\xf3\x00\x00\x00\x0b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x01\x00\x00\x08\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xea\x00\x80\x00\xe1\x00\xe0\x00\x3c\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\x00\x00\x00\xce\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\xdf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\xbc\x00\x00\x00\x00\x00\x00\x00\x3f\x03\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x01\x00\x35\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x01\xdb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x31\x03\x27\x03\x00\x00\x23\x03\x00\x00\x00\x00\x00\x00\xcc\x00\xcb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\x00\x00\x00\xf2\x00\xe9\x00\x00\x00\x00\x00\xd3\x00\x00\x00\x11\x00\x00\x00\x3e\x01\x19\x03\xc0\x00\xc6\x00\x00\x00\x00\x00\x7a\x02\x15\x03\x0b\x03\x07\x03\xfd\x02\xf9\x02\xef\x02\xeb\x02\xe1\x02\xdd\x02\xd3\x02\xcf\x02\xc5\x02\xc1\x02\xb7\x02\xb3\x02\xa9\x02\x00\x00\x5b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa5\x02\x00\x00\x00\x00\x00\x00\xcf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\x00\x00\x00\x00\x00\x9b\x02\x00\x00\x00\x00\xcd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x02\x97\x02\x00\x00\x00\x00\x56\x02\xb1\x00\x00\x00\x00\x00\x00\x00\x2d\x01\x27\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa4\x00\x8d\x02\x89\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa1\x00\x00\x00\x00\x00\x00\x00\xf9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd6\x00\x16\x01\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x02\x00\x00\x00\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x5d\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x02\x58\x00\xea\xff\xb0\x00\x00\x00\x00\x00\xa2\x00\x0f\x00\xa8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7b\x00\x78\x00\x00\x00\xd1\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\x00\x00\x00\xc2\x00\x00\x00\x4d\x00\x00\x00\x0a\x00\x00\x00\x3e\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xfe\xff\x00\x00\x00\x00\xfe\xff\xfb\xff\xfc\xff\x7d\xff\xfa\xff\x00\x00\x6f\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\xff\x6e\xff\x6d\xff\x6c\xff\x6b\xff\x6a\xff\x7d\xff\x00\x00\x00\x00\x72\xff\x7b\xff\x00\x00\xda\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x72\xff\xfd\xff\x74\xff\xea\xff\x00\x00\xde\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\xf7\xff\x00\x00\xdc\xff\x00\x00\x7a\xff\x78\xff\x00\x00\x00\x00\x74\xff\x00\x00\xdd\xff\x00\x00\x00\x00\x00\x00\x76\xff\x79\xff\x7c\xff\xd9\xff\x00\x00\xf7\xff\x00\x00\x6f\xff\x00\x00\x00\x00\x70\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x71\xff\x00\x00\xe1\xff\xed\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9f\xff\x9b\xff\x00\x00\xf3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x88\xff\x89\xff\x9c\xff\x97\xff\x97\xff\xf6\xff\xf8\xff\x77\xff\x75\xff\x00\x00\xe1\xff\x00\x00\xe1\xff\xe1\xff\x00\x00\x00\x00\x00\x00\xd8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x95\xff\xbc\xff\x7e\xff\x7f\xff\x8d\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9d\xff\x00\x00\x9e\xff\xa1\xff\x00\x00\xa2\xff\x00\x00\x00\x00\x00\x00\xf4\xff\x00\x00\xed\xff\xef\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\x7b\xff\x00\x00\x00\x00\x00\x00\x00\x00\xeb\xff\xed\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\xff\x8d\xff\x96\xff\xa4\xff\xa3\xff\xa6\xff\xa8\xff\xac\xff\xad\xff\xa5\xff\xa7\xff\xa9\xff\xaa\xff\xab\xff\xae\xff\xaf\xff\xb0\xff\xb1\xff\xb2\xff\x8b\xff\x00\x00\x8c\xff\xd7\xff\x8d\xff\x00\x00\x00\x00\x00\x00\x93\xff\x95\xff\x00\x00\xca\xff\xc9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x85\xff\x82\xff\x80\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdf\xff\xe0\xff\xe9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x81\xff\x84\xff\x00\x00\xd0\xff\xc6\xff\x00\x00\xca\xff\xc9\xff\xe1\xff\x00\x00\x00\x00\x00\x00\x8f\xff\x00\x00\x92\xff\x91\xff\xce\xff\x00\x00\x00\x00\x00\x00\x73\xff\x00\x00\x00\x00\x9a\xff\x00\x00\xf0\xff\xee\xff\xf2\xff\xf1\xff\x00\x00\x00\x00\x00\x00\xe2\xff\x00\x00\xf9\xff\xec\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa0\xff\x99\xff\x8a\xff\x00\x00\xbb\xff\x00\x00\x00\x00\x94\xff\x8e\xff\xcf\xff\xc7\xff\xc8\xff\x00\x00\xc5\xff\x86\xff\x83\xff\x00\x00\xd6\xff\x00\x00\x00\x00\x93\xff\x93\xff\x00\x00\xb4\xff\x90\xff\x00\x00\xb5\xff\xbb\xff\x00\x00\xd2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb8\xff\xba\xff\x00\x00\x00\x00\xbd\xff\xcd\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc4\xff\xc4\xff\xd5\xff\xe1\xff\x00\x00\xd1\xff\x00\x00\x00\x00\xe4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\xff\xb7\xff\xe1\xff\x00\x00\xc2\xff\xc3\xff\xc2\xff\x00\x00\x00\x00\xcc\xff\xb3\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe8\xff\x00\x00\x00\x00\xb9\xff\xb6\xff\x00\x00\x00\x00\x00\x00\xc1\xff\xbf\xff\xd3\xff\x00\x00\xc0\xff\xcb\xff\xd4\xff\xe5\xff\xe7\xff\x00\x00\x00\x00\xbe\xff\xe6\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x18\x00\x19\x00\x03\x00\x04\x00\x07\x00\x03\x00\x06\x00\x02\x00\x0b\x00\x21\x00\x06\x00\x0e\x00\x0f\x00\x01\x00\x22\x00\x08\x00\x05\x00\x08\x00\x2f\x00\x07\x00\x31\x00\x2d\x00\x2e\x00\x2b\x00\x10\x00\x02\x00\x23\x00\x24\x00\x25\x00\x16\x00\x07\x00\x16\x00\x13\x00\x14\x00\x20\x00\x21\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x11\x00\x3f\x00\x2d\x00\x2e\x00\x0c\x00\x03\x00\x04\x00\x2c\x00\x23\x00\x24\x00\x25\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x29\x00\x41\x00\x3e\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x07\x00\x31\x00\x32\x00\x3f\x00\x34\x00\x35\x00\x11\x00\x0e\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x07\x00\x2d\x00\x2e\x00\x10\x00\x0b\x00\x13\x00\x14\x00\x0e\x00\x0f\x00\x19\x00\x15\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x0b\x00\x17\x00\x21\x00\x0e\x00\x0f\x00\x29\x00\x2a\x00\x18\x00\x19\x00\x2d\x00\x2e\x00\x1c\x00\x1d\x00\x1e\x00\x2d\x00\x2e\x00\x21\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x2d\x00\x2e\x00\x0f\x00\x2d\x00\x2e\x00\x0f\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x2d\x00\x2e\x00\x2d\x00\x2e\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x01\x00\x02\x00\x16\x00\x18\x00\x19\x00\x06\x00\x07\x00\x15\x00\x09\x00\x00\x00\x01\x00\x02\x00\x21\x00\x20\x00\x21\x00\x06\x00\x07\x00\x17\x00\x09\x00\x25\x00\x08\x00\x09\x00\x1b\x00\x0b\x00\x2d\x00\x2e\x00\x08\x00\x09\x00\x05\x00\x0b\x00\x05\x00\x40\x00\x3f\x00\x40\x00\x12\x00\x08\x00\x09\x00\x1b\x00\x0b\x00\x0a\x00\x08\x00\x09\x00\x05\x00\x0b\x00\x2d\x00\x2e\x00\x22\x00\x1a\x00\x1a\x00\x2b\x00\x26\x00\x0a\x00\x22\x00\x0a\x00\x2d\x00\x2e\x00\x26\x00\x2d\x00\x2e\x00\x08\x00\x09\x00\x22\x00\x0b\x00\x2d\x00\x2e\x00\x26\x00\x22\x00\x2b\x00\x08\x00\x09\x00\x26\x00\x0b\x00\x2d\x00\x2e\x00\x28\x00\x29\x00\x2a\x00\x2d\x00\x2e\x00\x2d\x00\x2e\x00\x08\x00\x09\x00\x2c\x00\x0b\x00\x22\x00\x2c\x00\x08\x00\x09\x00\x26\x00\x0b\x00\x27\x00\x0a\x00\x0a\x00\x22\x00\x0a\x00\x2d\x00\x2e\x00\x26\x00\x08\x00\x27\x00\x41\x00\x3e\x00\x0d\x00\x0e\x00\x2d\x00\x2e\x00\x22\x00\x16\x00\x0d\x00\x0e\x00\x26\x00\x06\x00\x22\x00\x18\x00\x19\x00\x33\x00\x26\x00\x2d\x00\x2e\x00\x18\x00\x19\x00\x02\x00\x21\x00\x2d\x00\x2e\x00\x0d\x00\x0e\x00\x41\x00\x21\x00\x08\x00\x40\x00\x0d\x00\x0e\x00\x04\x00\x2d\x00\x2e\x00\x18\x00\x19\x00\x04\x00\x30\x00\x2d\x00\x2e\x00\x18\x00\x19\x00\x02\x00\x21\x00\x05\x00\x04\x00\x0d\x00\x0e\x00\x16\x00\x21\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x2d\x00\x2e\x00\x18\x00\x19\x00\x16\x00\x08\x00\x2d\x00\x2e\x00\x40\x00\x1a\x00\x1b\x00\x21\x00\x03\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x41\x00\x29\x00\x2a\x00\x40\x00\x2d\x00\x2e\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x34\x00\x40\x00\x40\x00\x02\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x03\x00\x3f\x00\x40\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x02\x00\x40\x00\x08\x00\x08\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x02\x00\x01\x00\x03\x00\x16\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x04\x00\x03\x00\x01\x00\x08\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x40\x00\x16\x00\x16\x00\x40\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x06\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x41\x00\x16\x00\x40\x00\x37\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x06\x00\x41\x00\x36\x00\x06\x00\x28\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x09\x00\x04\x00\x20\x00\x41\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x07\x00\x36\x00\x02\x00\x40\x00\x16\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x41\x00\x16\x00\x16\x00\x16\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x07\x00\x41\x00\x41\x00\x08\x00\x08\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x18\x00\x16\x00\x08\x00\x02\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x07\x00\x09\x00\x08\x00\x3f\x00\x0e\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x40\x00\x3f\x00\x16\x00\x08\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x08\x00\x02\x00\x41\x00\x02\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x08\x00\x0a\x00\x08\x00\x02\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x18\x00\x19\x00\x02\x00\x02\x00\x1c\x00\x1d\x00\x1e\x00\x18\x00\x19\x00\x21\x00\x16\x00\x1c\x00\x1d\x00\x1e\x00\x18\x00\x19\x00\x21\x00\x18\x00\x19\x00\x1d\x00\x1e\x00\x2d\x00\x2e\x00\x21\x00\x1f\x00\x20\x00\x21\x00\x16\x00\x2d\x00\x2e\x00\x16\x00\x08\x00\x16\x00\x18\x00\x19\x00\x2d\x00\x2e\x00\x02\x00\x2d\x00\x2e\x00\x1f\x00\x20\x00\x21\x00\x18\x00\x19\x00\x04\x00\x06\x00\x08\x00\x18\x00\x19\x00\x1f\x00\x20\x00\x21\x00\x40\x00\x2d\x00\x2e\x00\x20\x00\x21\x00\x18\x00\x19\x00\x41\x00\x41\x00\x18\x00\x19\x00\x2d\x00\x2e\x00\x16\x00\x21\x00\x41\x00\x2d\x00\x2e\x00\x21\x00\x18\x00\x19\x00\x06\x00\x3f\x00\x18\x00\x19\x00\x41\x00\x2d\x00\x2e\x00\x21\x00\x07\x00\x2d\x00\x2e\x00\x21\x00\x18\x00\x19\x00\x05\x00\x07\x00\x18\x00\x19\x00\x3f\x00\x2d\x00\x2e\x00\x21\x00\x09\x00\x2d\x00\x2e\x00\x21\x00\x18\x00\x19\x00\x04\x00\x19\x00\x18\x00\x19\x00\x3f\x00\x2d\x00\x2e\x00\x21\x00\x05\x00\x2d\x00\x2e\x00\x21\x00\x18\x00\x19\x00\x0a\x00\x3f\x00\x18\x00\x19\x00\x41\x00\x2d\x00\x2e\x00\x21\x00\x16\x00\x2d\x00\x2e\x00\x21\x00\x18\x00\x19\x00\x02\x00\x16\x00\x18\x00\x19\x00\x16\x00\x2d\x00\x2e\x00\x21\x00\x16\x00\x2d\x00\x2e\x00\x21\x00\x18\x00\x19\x00\x03\x00\x16\x00\x18\x00\x19\x00\x3f\x00\x2d\x00\x2e\x00\x21\x00\x01\x00\x2d\x00\x2e\x00\x21\x00\x18\x00\x19\x00\x41\x00\x05\x00\x18\x00\x19\x00\x41\x00\x2d\x00\x2e\x00\x21\x00\x41\x00\x2d\x00\x2e\x00\x21\x00\x18\x00\x19\x00\x41\x00\x07\x00\x18\x00\x19\x00\x41\x00\x2d\x00\x2e\x00\x21\x00\x41\x00\x2d\x00\x2e\x00\x21\x00\x18\x00\x19\x00\x05\x00\x04\x00\x18\x00\x19\x00\x3f\x00\x2d\x00\x2e\x00\x21\x00\x03\x00\x2d\x00\x2e\x00\x21\x00\x18\x00\x19\x00\x02\x00\x2e\x00\x18\x00\x19\x00\x16\x00\x2d\x00\x2e\x00\x21\x00\x3f\x00\x2d\x00\x2e\x00\x21\x00\x18\x00\x19\x00\x2e\x00\x08\x00\x18\x00\x19\x00\x16\x00\x2d\x00\x2e\x00\x21\x00\x3f\x00\x2d\x00\x2e\x00\x21\x00\x18\x00\x19\x00\x3f\x00\x02\x00\x18\x00\x19\x00\x16\x00\x2d\x00\x2e\x00\x21\x00\x16\x00\x2d\x00\x2e\x00\x21\x00\x18\x00\x19\x00\x16\x00\x16\x00\x16\x00\x02\x00\x16\x00\x2d\x00\x2e\x00\x21\x00\x3f\x00\x2d\x00\x2e\x00\x28\x00\x29\x00\x2a\x00\x3f\x00\x3f\x00\x2d\x00\x2e\x00\x3f\x00\x2d\x00\x2e\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x3f\x00\x3f\x00\x03\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x02\x00\x3f\x00\x2c\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x07\x00\x40\x00\x07\x00\x3f\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x3f\x00\x07\x00\x07\x00\x07\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x07\x00\x1a\x00\x1b\x00\x07\x00\x3f\x00\x07\x00\x43\x00\xff\xff\x1a\x00\x1b\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x69\x00\x47\x01\x65\x00\x72\x00\x49\x00\x6a\x00\xfa\x00\xa6\x00\x2d\x00\x6b\x00\x66\x00\x71\x01\x6c\x00\x6d\x00\xd5\x00\x4d\x00\x79\x01\xe7\x00\xab\x00\xe9\x00\xd6\x00\xea\x00\x67\x00\x09\x00\x4e\x00\x60\x01\x7f\x00\x28\x01\xe2\x00\xe3\x00\x7a\x01\x80\x00\xac\x00\x41\x01\x34\x01\xfb\x00\xfc\x00\x4f\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x7b\x01\x50\x00\x4a\x00\x4b\x00\x6d\x01\x48\x00\x49\x00\x2e\x00\xe1\x00\xe2\x00\xe3\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x11\x00\xa7\x00\x72\x01\x81\x00\x82\x00\x83\x00\x87\xff\x6a\x00\x87\xff\x84\x00\x20\x00\x13\x00\x85\x00\x6f\x01\x6c\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x86\x00\x87\x00\x1b\x00\xdf\x00\x4a\x00\x4b\x00\x62\x01\x6b\x00\x33\x01\x34\x01\x6c\x00\x6d\x00\x19\x01\x5a\x01\x6a\x00\x89\x00\x8a\x00\x8b\x00\x6b\x00\xe0\x00\x66\x00\x6c\x00\x6d\x00\x74\x00\x3b\x00\xfd\x00\x65\x00\x3c\x00\x09\x00\x48\x01\xff\x00\x00\x01\x67\x00\x09\x00\x66\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x67\x00\x09\x00\x5d\x01\xc0\x00\x09\x00\x5f\x01\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\x43\x00\x09\x00\x1b\x00\x09\x00\x93\x00\x94\x00\x95\x00\x96\x00\x97\x00\x98\x00\x2a\x00\x03\x00\x04\x00\x42\x01\x2b\x01\x65\x00\x05\x00\x06\x00\x3f\x01\x07\x00\x02\x00\x03\x00\x04\x00\x66\x00\xfb\x00\xfc\x00\x05\x00\x06\x00\x45\x01\x07\x00\xf1\x00\x64\x01\x79\x00\xfc\x00\x7a\x00\x67\x00\x09\x00\x65\x01\x79\x00\x14\x01\x7a\x00\xba\x00\x22\x01\xe5\x00\xe6\x00\xd6\x00\x5c\x01\x79\x00\xd8\x00\x7a\x00\x28\x00\x25\x01\x79\x00\xad\x00\x7a\x00\x08\x00\x09\x00\x7b\x00\x98\x00\x9a\x00\x41\x00\x7c\x00\x47\x00\x7b\x00\x3f\x00\x08\x00\x09\x00\x7c\x00\x7d\x00\x09\x00\xea\x00\x79\x00\x7b\x00\x7a\x00\x7d\x00\x09\x00\x7c\x00\x7b\x00\x57\x00\xeb\x00\x79\x00\x7c\x00\x7a\x00\x7d\x00\x09\x00\xb4\x00\x3a\x00\x3b\x00\x7d\x00\x09\x00\x3c\x00\x09\x00\x78\x00\x79\x00\x2b\x00\x7a\x00\x7b\x00\x3e\x00\xaf\x00\x79\x00\x7c\x00\x7a\x00\x1c\x00\x1e\x00\x21\x00\x7b\x00\x28\x00\x7d\x00\x09\x00\x7c\x00\x7d\x01\x29\x00\x7b\x01\x72\x01\x23\x01\xdc\x00\x7d\x00\x09\x00\x7b\x00\x74\x01\x24\x01\xdc\x00\x7c\x00\x75\x01\x7b\x00\xdd\x00\x65\x00\x76\x01\x7c\x00\x7d\x00\x09\x00\xdd\x00\x65\x00\x77\x01\x66\x00\x7d\x00\x09\x00\xf5\x00\xdc\x00\x6a\x01\x66\x00\x78\x01\x6b\x01\xf6\x00\xf7\x00\x6c\x01\x67\x00\x09\x00\xdd\x00\x65\x00\x6d\x01\x6f\x01\x67\x00\x09\x00\xf8\x00\x65\x00\x73\x01\x66\x00\x62\x01\x64\x01\xdb\x00\xdc\x00\x67\x01\x66\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x67\x00\x09\x00\xdd\x00\x65\x00\x68\x01\x69\x01\x67\x00\x09\x00\x57\x01\x93\x00\x94\x00\x66\x00\x5a\x01\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x41\x01\x11\x00\x12\x00\x58\x01\x67\x00\x09\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x13\x00\x59\x01\x3e\x00\x4d\x01\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x5c\x01\x1a\x00\x1b\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\x2b\x01\x5f\x01\x4b\x01\x4c\x01\x93\x00\x94\x00\x95\x00\x96\x00\x97\x00\x98\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\xa2\x00\x4f\x01\x4e\x01\x51\x01\x93\x00\x94\x00\x95\x00\x96\x00\x97\x00\x98\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\x50\x01\x1e\x01\x52\x01\x53\x01\x93\x00\x94\x00\x95\x00\x96\x00\x97\x00\x98\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\x3c\x01\x54\x01\x55\x01\x3d\x01\x93\x00\x94\x00\x95\x00\x96\x00\x97\x00\x98\x00\x2a\x01\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\x3e\x01\x56\x01\x3f\x01\x44\x01\x93\x00\x94\x00\x95\x00\x96\x00\x97\x00\x98\x00\x09\x01\x41\x01\x36\x01\x45\x01\x47\x01\x2e\x01\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\x30\x01\x31\x01\xfb\x00\x33\x01\x93\x00\x94\x00\x95\x00\x96\x00\x97\x00\x98\x00\x2f\x01\x36\x01\x37\x01\x19\x01\x38\x01\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\x16\x01\x39\x01\x3a\x01\x3b\x01\x93\x00\x94\x00\x95\x00\x96\x00\x97\x00\x98\x00\xda\x00\x17\x01\x18\x01\x1b\x01\x1d\x01\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\x1f\x01\x20\x01\x21\x01\x23\x01\x93\x00\x94\x00\x95\x00\x96\x00\x97\x00\x98\x00\x88\x00\x28\x01\x27\x01\x2d\x01\xee\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\xef\x00\xe5\x00\xf3\x00\xf4\x00\x93\x00\x94\x00\x95\x00\x96\x00\x97\x00\x98\x00\xbf\x00\xf5\x00\x03\x01\x02\x01\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\x06\x01\x0a\x01\x0c\x01\x0b\x01\x93\x00\x94\x00\x95\x00\x96\x00\x97\x00\x98\x00\xfd\x00\x65\x00\x0d\x01\x0e\x01\x49\x01\xff\x00\x00\x01\xfd\x00\x65\x00\x66\x00\x0f\x01\xfe\x00\xff\x00\x00\x01\xfd\x00\x65\x00\x66\x00\xd1\x00\x65\x00\x31\x01\x00\x01\x67\x00\x09\x00\x66\x00\x04\x01\xd3\x00\x66\x00\x10\x01\x67\x00\x09\x00\x11\x01\x12\x01\x13\x01\xd1\x00\x65\x00\x67\x00\x09\x00\x14\x01\x67\x00\x09\x00\x07\x01\xd3\x00\x66\x00\xd1\x00\x65\x00\xb1\x00\xb9\x00\xb3\x00\xd1\x00\x65\x00\xd2\x00\xd3\x00\x66\x00\xba\x00\x67\x00\x09\x00\x1b\x01\x66\x00\xef\x00\x65\x00\xb4\x00\xb6\x00\xf0\x00\x65\x00\x67\x00\x09\x00\xaf\x00\x66\x00\xb7\x00\x67\x00\x09\x00\x66\x00\x03\x01\x65\x00\xbc\x00\xbd\x00\xb1\x00\x65\x00\xb8\x00\x67\x00\x09\x00\x66\x00\xc0\x00\x67\x00\x09\x00\x66\x00\xbd\x00\x65\x00\xd8\x00\xda\x00\xc1\x00\x65\x00\xe1\x00\x67\x00\x09\x00\x66\x00\xe8\x00\x67\x00\x09\x00\x66\x00\xc2\x00\x65\x00\xed\x00\x9a\x00\xc3\x00\x65\x00\x9d\x00\x67\x00\x09\x00\x66\x00\xa0\x00\x67\x00\x09\x00\x66\x00\xc4\x00\x65\x00\xa1\x00\xa4\x00\xc5\x00\x65\x00\xa3\x00\x67\x00\x09\x00\x66\x00\xa8\x00\x67\x00\x09\x00\x66\x00\xc6\x00\x65\x00\xa5\x00\xa9\x00\xc7\x00\x65\x00\xaa\x00\x67\x00\x09\x00\x66\x00\xad\x00\x67\x00\x09\x00\x66\x00\xc8\x00\x65\x00\x59\x00\xaf\x00\xc9\x00\x65\x00\x5a\x00\x67\x00\x09\x00\x66\x00\x60\x00\x67\x00\x09\x00\x66\x00\xca\x00\x65\x00\x5b\x00\x61\x00\xcb\x00\x65\x00\x5c\x00\x67\x00\x09\x00\x66\x00\x5d\x00\x67\x00\x09\x00\x66\x00\xcc\x00\x65\x00\x5e\x00\x63\x00\xcd\x00\x65\x00\x5f\x00\x67\x00\x09\x00\x66\x00\x62\x00\x67\x00\x09\x00\x66\x00\xce\x00\x65\x00\x64\x00\x74\x00\xcf\x00\x65\x00\x76\x00\x67\x00\x09\x00\x66\x00\x78\x00\x67\x00\x09\x00\x66\x00\xd0\x00\x65\x00\x41\x00\x43\x00\xda\x00\x65\x00\x46\x00\x67\x00\x09\x00\x66\x00\x45\x00\x67\x00\x09\x00\x66\x00\x9b\x00\x65\x00\x43\x00\x47\x00\x9d\x00\x65\x00\x51\x00\x67\x00\x09\x00\x66\x00\x20\x00\x67\x00\x09\x00\x66\x00\x9e\x00\x65\x00\x57\x00\x2f\x00\x64\x00\x65\x00\x52\x00\x67\x00\x09\x00\x66\x00\x53\x00\x67\x00\x09\x00\x66\x00\x76\x00\x65\x00\x54\x00\x55\x00\x56\x00\x36\x00\x38\x00\x67\x00\x09\x00\x66\x00\x30\x00\x67\x00\x09\x00\x39\x00\x3a\x00\x3b\x00\x31\x00\x32\x00\x3c\x00\x09\x00\x33\x00\x67\x00\x09\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\x07\x01\x34\x00\x35\x00\x37\x00\x93\x00\x94\x00\x95\x00\x96\x00\x97\x00\x98\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\xaf\x00\x39\x00\x20\x00\x2e\x00\x93\x00\x94\x00\x95\x00\x96\x00\x97\x00\x98\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\x1e\x00\x21\x00\x23\x00\x20\x00\x93\x00\x94\x00\x95\x00\x96\x00\x97\x00\x98\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x00\x00\x00\x00\x20\x00\x24\x00\x25\x00\x26\x00\x93\x00\x94\x00\x00\x00\x00\x00\x00\x00\x00\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x27\x00\x93\x00\x94\x00\x28\x00\x20\x00\x1e\x00\xff\xff\x00\x00\x93\x00\x94\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (1, 149) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136),
	(137 , happyReduce_137),
	(138 , happyReduce_138),
	(139 , happyReduce_139),
	(140 , happyReduce_140),
	(141 , happyReduce_141),
	(142 , happyReduce_142),
	(143 , happyReduce_143),
	(144 , happyReduce_144),
	(145 , happyReduce_145),
	(146 , happyReduce_146),
	(147 , happyReduce_147),
	(148 , happyReduce_148),
	(149 , happyReduce_149)
	]

happy_n_terms = 68 :: Int
happy_n_nonterms = 47 :: Int

happyReduce_1 = happySpecReduce_0  0# happyReduction_1
happyReduction_1  =  happyIn4
		 (return ()
	)

happyReduce_2 = happySpecReduce_2  0# happyReduction_2
happyReduction_2 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (do happy_var_1; happy_var_2
	)}}

happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_5 = happySpecReduce_1  1# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_6 = happyReduce 8# 1# happyReduction_6
happyReduction_6 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (L _ (CmmT_Name	happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Name	happy_var_5)) -> 
	case happyOut9 happy_x_6 of { happy_var_6 -> 
	happyIn5
		 (do lits <- sequence happy_var_6;
		     staticClosure happy_var_3 happy_var_5 (map getLit lits)
	) `HappyStk` happyRest}}}

happyReduce_7 = happyReduce 5# 2# happyReduction_7
happyReduction_7 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (L _ (CmmT_String	happy_var_2)) -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	happyIn6
		 (do ss <- sequence happy_var_4;
		     code (emitData (section happy_var_2) (concat ss))
	) `HappyStk` happyRest}}

happyReduce_8 = happySpecReduce_0  3# happyReduction_8
happyReduction_8  =  happyIn7
		 ([]
	)

happyReduce_9 = happySpecReduce_2  3# happyReduction_9
happyReduction_9 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	happyIn7
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_10 = happySpecReduce_2  4# happyReduction_10
happyReduction_10 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name	happy_var_1)) -> 
	happyIn8
		 (return [CmmDataLabel (mkRtsDataLabelFS happy_var_1)]
	)}

happyReduce_11 = happySpecReduce_3  4# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_2 of { happy_var_2 -> 
	happyIn8
		 (do e <- happy_var_2;
			     return [CmmStaticLit (getLit e)]
	)}

happyReduce_12 = happySpecReduce_2  4# happyReduction_12
happyReduction_12 happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (return [CmmUninitialised
							(machRepByteWidth happy_var_1)]
	)}

happyReduce_13 = happyReduce 5# 4# happyReduction_13
happyReduction_13 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_4 of { (L _ (CmmT_String	happy_var_4)) -> 
	happyIn8
		 (return [mkString happy_var_4]
	) `HappyStk` happyRest}

happyReduce_14 = happyReduce 5# 4# happyReduction_14
happyReduction_14 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (L _ (CmmT_Int		happy_var_3)) -> 
	happyIn8
		 (return [CmmUninitialised 
							(fromIntegral happy_var_3)]
	) `HappyStk` happyRest}

happyReduce_15 = happyReduce 5# 4# happyReduction_15
happyReduction_15 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { (L _ (CmmT_Int		happy_var_3)) -> 
	happyIn8
		 (return [CmmUninitialised 
						(machRepByteWidth happy_var_1 * 
							fromIntegral happy_var_3)]
	) `HappyStk` happyRest}}

happyReduce_16 = happySpecReduce_3  4# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (L _ (CmmT_Int		happy_var_2)) -> 
	happyIn8
		 (return [CmmAlign (fromIntegral happy_var_2)]
	)}

happyReduce_17 = happyReduce 5# 4# happyReduction_17
happyReduction_17 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (L _ (CmmT_Name	happy_var_3)) -> 
	case happyOut9 happy_x_4 of { happy_var_4 -> 
	happyIn8
		 (do lits <- sequence happy_var_4;
		     return $ map CmmStaticLit $
		       mkStaticClosure (mkRtsInfoLabelFS happy_var_3) 
			 dontCareCCS (map getLit lits) [] [] []
	) `HappyStk` happyRest}}

happyReduce_18 = happySpecReduce_0  5# happyReduction_18
happyReduction_18  =  happyIn9
		 ([]
	)

happyReduce_19 = happySpecReduce_3  5# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_2 of { happy_var_2 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 (happy_var_2 : happy_var_3
	)}}

happyReduce_20 = happyReduce 7# 6# happyReduction_20
happyReduction_20 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_3 of { happy_var_3 -> 
	case happyOut47 happy_x_4 of { happy_var_4 -> 
	case happyOut12 happy_x_6 of { happy_var_6 -> 
	happyIn10
		 (do ((entry_ret_label, info, live, formals, gc_block, frame), stmts) <-
		       getCgStmtsEC' $ loopDecls $ do {
		         (entry_ret_label, info, live) <- happy_var_1;
		         formals <- sequence happy_var_2;
		         gc_block <- happy_var_3;
		         frame <- happy_var_4;
		         happy_var_6;
		         return (entry_ret_label, info, live, formals, gc_block, frame) }
		     blks <- code (cgStmtsToBlocks stmts)
		     code (emitInfoTableAndCode entry_ret_label (CmmInfo gc_block frame info) formals blks)
	) `HappyStk` happyRest}}}}}

happyReduce_21 = happySpecReduce_3  6# happyReduction_21
happyReduction_21 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 (do (entry_ret_label, info, live) <- happy_var_1;
		     formals <- sequence happy_var_2;
		     code (emitInfoTableAndCode entry_ret_label (CmmInfo Nothing Nothing info) formals [])
	)}}

happyReduce_22 = happyReduce 7# 6# happyReduction_22
happyReduction_22 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (L _ (CmmT_Name	happy_var_1)) -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_3 of { happy_var_3 -> 
	case happyOut47 happy_x_4 of { happy_var_4 -> 
	case happyOut12 happy_x_6 of { happy_var_6 -> 
	happyIn10
		 (do ((formals, gc_block, frame), stmts) <-
			getCgStmtsEC' $ loopDecls $ do {
		          formals <- sequence happy_var_2;
		          gc_block <- happy_var_3;
			  frame <- happy_var_4;
		          happy_var_6;
		          return (formals, gc_block, frame) }
                     blks <- code (cgStmtsToBlocks stmts)
		     code (emitProc (CmmInfo gc_block frame CmmNonInfoTable) (mkRtsCodeLabelFS happy_var_1) formals blks)
	) `HappyStk` happyRest}}}}}

happyReduce_23 = happyReduce 14# 7# happyReduction_23
happyReduction_23 (happy_x_14 `HappyStk`
	happy_x_13 `HappyStk`
	happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (L _ (CmmT_Name	happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Int		happy_var_5)) -> 
	case happyOutTok happy_x_7 of { (L _ (CmmT_Int		happy_var_7)) -> 
	case happyOutTok happy_x_9 of { (L _ (CmmT_Int		happy_var_9)) -> 
	case happyOutTok happy_x_11 of { (L _ (CmmT_String	happy_var_11)) -> 
	case happyOutTok happy_x_13 of { (L _ (CmmT_String	happy_var_13)) -> 
	happyIn11
		 (do prof <- profilingInfo happy_var_11 happy_var_13
		     return (mkRtsEntryLabelFS happy_var_3,
			CmmInfoTable prof (fromIntegral happy_var_9)
				     (ThunkInfo (fromIntegral happy_var_5, fromIntegral happy_var_7) NoC_SRT),
			[])
	) `HappyStk` happyRest}}}}}}

happyReduce_24 = happyReduce 16# 7# happyReduction_24
happyReduction_24 (happy_x_16 `HappyStk`
	happy_x_15 `HappyStk`
	happy_x_14 `HappyStk`
	happy_x_13 `HappyStk`
	happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (L _ (CmmT_Name	happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Int		happy_var_5)) -> 
	case happyOutTok happy_x_7 of { (L _ (CmmT_Int		happy_var_7)) -> 
	case happyOutTok happy_x_9 of { (L _ (CmmT_Int		happy_var_9)) -> 
	case happyOutTok happy_x_11 of { (L _ (CmmT_String	happy_var_11)) -> 
	case happyOutTok happy_x_13 of { (L _ (CmmT_String	happy_var_13)) -> 
	case happyOutTok happy_x_15 of { (L _ (CmmT_Int		happy_var_15)) -> 
	happyIn11
		 (do prof <- profilingInfo happy_var_11 happy_var_13
		     return (mkRtsEntryLabelFS happy_var_3,
			CmmInfoTable prof (fromIntegral happy_var_9)
				     (FunInfo (fromIntegral happy_var_5, fromIntegral happy_var_7) NoC_SRT (fromIntegral happy_var_15) 0
				      (ArgSpec 0)
				      zeroCLit),
			[])
	) `HappyStk` happyRest}}}}}}}

happyReduce_25 = happyReduce 18# 7# happyReduction_25
happyReduction_25 (happy_x_18 `HappyStk`
	happy_x_17 `HappyStk`
	happy_x_16 `HappyStk`
	happy_x_15 `HappyStk`
	happy_x_14 `HappyStk`
	happy_x_13 `HappyStk`
	happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (L _ (CmmT_Name	happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Int		happy_var_5)) -> 
	case happyOutTok happy_x_7 of { (L _ (CmmT_Int		happy_var_7)) -> 
	case happyOutTok happy_x_9 of { (L _ (CmmT_Int		happy_var_9)) -> 
	case happyOutTok happy_x_11 of { (L _ (CmmT_String	happy_var_11)) -> 
	case happyOutTok happy_x_13 of { (L _ (CmmT_String	happy_var_13)) -> 
	case happyOutTok happy_x_15 of { (L _ (CmmT_Int		happy_var_15)) -> 
	case happyOutTok happy_x_17 of { (L _ (CmmT_Int		happy_var_17)) -> 
	happyIn11
		 (do prof <- profilingInfo happy_var_11 happy_var_13
		     return (mkRtsEntryLabelFS happy_var_3,
			CmmInfoTable prof (fromIntegral happy_var_9)
				     (FunInfo (fromIntegral happy_var_5, fromIntegral happy_var_7) NoC_SRT (fromIntegral happy_var_15) (fromIntegral happy_var_17)
				      (ArgSpec 0)
				      zeroCLit),
			[])
	) `HappyStk` happyRest}}}}}}}}

happyReduce_26 = happyReduce 16# 7# happyReduction_26
happyReduction_26 (happy_x_16 `HappyStk`
	happy_x_15 `HappyStk`
	happy_x_14 `HappyStk`
	happy_x_13 `HappyStk`
	happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (L _ (CmmT_Name	happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Int		happy_var_5)) -> 
	case happyOutTok happy_x_7 of { (L _ (CmmT_Int		happy_var_7)) -> 
	case happyOutTok happy_x_9 of { (L _ (CmmT_Int		happy_var_9)) -> 
	case happyOutTok happy_x_11 of { (L _ (CmmT_Int		happy_var_11)) -> 
	case happyOutTok happy_x_13 of { (L _ (CmmT_String	happy_var_13)) -> 
	case happyOutTok happy_x_15 of { (L _ (CmmT_String	happy_var_15)) -> 
	happyIn11
		 (do prof <- profilingInfo happy_var_13 happy_var_15
		     -- If profiling is on, this string gets duplicated,
		     -- but that's the way the old code did it we can fix it some other time.
		     desc_lit <- code $ mkStringCLit happy_var_13
		     return (mkRtsEntryLabelFS happy_var_3,
			CmmInfoTable prof (fromIntegral happy_var_11)
				     (ConstrInfo (fromIntegral happy_var_5, fromIntegral happy_var_7) (fromIntegral happy_var_9) desc_lit),
			[])
	) `HappyStk` happyRest}}}}}}}

happyReduce_27 = happyReduce 12# 7# happyReduction_27
happyReduction_27 (happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (L _ (CmmT_Name	happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Int		happy_var_5)) -> 
	case happyOutTok happy_x_7 of { (L _ (CmmT_Int		happy_var_7)) -> 
	case happyOutTok happy_x_9 of { (L _ (CmmT_String	happy_var_9)) -> 
	case happyOutTok happy_x_11 of { (L _ (CmmT_String	happy_var_11)) -> 
	happyIn11
		 (do prof <- profilingInfo happy_var_9 happy_var_11
		     return (mkRtsEntryLabelFS happy_var_3,
			CmmInfoTable prof (fromIntegral happy_var_7)
				     (ThunkSelectorInfo (fromIntegral happy_var_5) NoC_SRT),
			[])
	) `HappyStk` happyRest}}}}}

happyReduce_28 = happyReduce 6# 7# happyReduction_28
happyReduction_28 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (L _ (CmmT_Name	happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Int		happy_var_5)) -> 
	happyIn11
		 (do let infoLabel = mkRtsInfoLabelFS happy_var_3
		     return (mkRtsRetLabelFS happy_var_3,
			CmmInfoTable (ProfilingInfo zeroCLit zeroCLit) (fromIntegral happy_var_5)
				     (ContInfo [] NoC_SRT),
			[])
	) `HappyStk` happyRest}}

happyReduce_29 = happyReduce 8# 7# happyReduction_29
happyReduction_29 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (L _ (CmmT_Name	happy_var_3)) -> 
	case happyOutTok happy_x_5 of { (L _ (CmmT_Int		happy_var_5)) -> 
	case happyOut44 happy_x_7 of { happy_var_7 -> 
	happyIn11
		 (do live <- sequence (map (liftM Just) happy_var_7)
		     return (mkRtsRetLabelFS happy_var_3,
			CmmInfoTable (ProfilingInfo zeroCLit zeroCLit) (fromIntegral happy_var_5)
			             (ContInfo live NoC_SRT),
			live)
	) `HappyStk` happyRest}}}

happyReduce_30 = happySpecReduce_0  8# happyReduction_30
happyReduction_30  =  happyIn12
		 (return ()
	)

happyReduce_31 = happySpecReduce_2  8# happyReduction_31
happyReduction_31 happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 (do happy_var_1; happy_var_2
	)}}

happyReduce_32 = happySpecReduce_2  8# happyReduction_32
happyReduction_32 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 (do happy_var_1; happy_var_2
	)}}

happyReduce_33 = happySpecReduce_3  9# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (mapM_ (newLocal defaultKind happy_var_1) happy_var_2
	)}}

happyReduce_34 = happyMonadReduce 4# 9# happyReduction_34
happyReduction_34 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { (L _ (CmmT_String	happy_var_1)) -> 
	case happyOut49 happy_x_2 of { happy_var_2 -> 
	case happyOut14 happy_x_3 of { happy_var_3 -> 
	( do k <- parseKind happy_var_1;
					      return $ mapM_ (newLocal k happy_var_2) happy_var_3)}}}
	) (\r -> happyReturn (happyIn13 r))

happyReduce_35 = happySpecReduce_3  9# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (mapM_ newImport happy_var_2
	)}

happyReduce_36 = happySpecReduce_3  9# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn13
		 (return ()
	)

happyReduce_37 = happySpecReduce_1  10# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name	happy_var_1)) -> 
	happyIn14
		 ([happy_var_1]
	)}

happyReduce_38 = happySpecReduce_3  10# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name	happy_var_1)) -> 
	case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_39 = happySpecReduce_1  11# happyReduction_39
happyReduction_39 happy_x_1
	 =  happyIn15
		 (nopEC
	)

happyReduce_40 = happySpecReduce_2  11# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name	happy_var_1)) -> 
	happyIn15
		 (do l <- newLabel happy_var_1; code (labelC l)
	)}

happyReduce_41 = happyReduce 4# 11# happyReduction_41
happyReduction_41 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (do reg <- happy_var_1; e <- happy_var_3; stmtEC (CmmAssign reg e)
	) `HappyStk` happyRest}}

happyReduce_42 = happyReduce 7# 11# happyReduction_42
happyReduction_42 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	case happyOut28 happy_x_6 of { happy_var_6 -> 
	happyIn15
		 (doStore happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_43 = happyMonadReduce 11# 11# happyReduction_43
happyReduction_43 (happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { (L _ (CmmT_String	happy_var_3)) -> 
	case happyOut28 happy_x_4 of { happy_var_4 -> 
	case happyOut32 happy_x_6 of { happy_var_6 -> 
	case happyOut19 happy_x_8 of { happy_var_8 -> 
	case happyOut20 happy_x_9 of { happy_var_9 -> 
	case happyOut16 happy_x_10 of { happy_var_10 -> 
	( foreignCall happy_var_3 happy_var_1 happy_var_4 happy_var_6 happy_var_9 happy_var_8 happy_var_10)}}}}}}}
	) (\r -> happyReturn (happyIn15 r))

happyReduce_44 = happyMonadReduce 10# 11# happyReduction_44
happyReduction_44 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_4 of { (L _ (CmmT_Name	happy_var_4)) -> 
	case happyOut32 happy_x_6 of { happy_var_6 -> 
	case happyOut19 happy_x_8 of { happy_var_8 -> 
	case happyOut20 happy_x_9 of { happy_var_9 -> 
	( primCall happy_var_1 happy_var_4 happy_var_6 happy_var_9 happy_var_8)}}}}}
	) (\r -> happyReturn (happyIn15 r))

happyReduce_45 = happyMonadReduce 5# 11# happyReduction_45
happyReduction_45 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { (L _ (CmmT_Name	happy_var_1)) -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	( stmtMacro happy_var_1 happy_var_3)}}
	) (\r -> happyReturn (happyIn15 r))

happyReduce_46 = happyReduce 7# 11# happyReduction_46
happyReduction_46 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut22 happy_x_2 of { happy_var_2 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	case happyOut23 happy_x_5 of { happy_var_5 -> 
	case happyOut26 happy_x_6 of { happy_var_6 -> 
	happyIn15
		 (doSwitch happy_var_2 happy_var_3 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}

happyReduce_47 = happySpecReduce_3  11# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (L _ (CmmT_Name	happy_var_2)) -> 
	happyIn15
		 (do l <- lookupLabel happy_var_2; stmtEC (CmmBranch l)
	)}

happyReduce_48 = happyReduce 4# 11# happyReduction_48
happyReduction_48 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (do e1 <- happy_var_2; e2 <- sequence happy_var_3; stmtEC (CmmJump e1 e2)
	) `HappyStk` happyRest}}

happyReduce_49 = happySpecReduce_3  11# happyReduction_49
happyReduction_49 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (do e <- sequence happy_var_2; stmtEC (CmmReturn e)
	)}

happyReduce_50 = happyReduce 6# 11# happyReduction_50
happyReduction_50 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut17 happy_x_2 of { happy_var_2 -> 
	case happyOut12 happy_x_4 of { happy_var_4 -> 
	case happyOut27 happy_x_6 of { happy_var_6 -> 
	happyIn15
		 (ifThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_51 = happySpecReduce_0  12# happyReduction_51
happyReduction_51  =  happyIn16
		 (CmmMayReturn
	)

happyReduce_52 = happySpecReduce_2  12# happyReduction_52
happyReduction_52 happy_x_2
	happy_x_1
	 =  happyIn16
		 (CmmNeverReturns
	)

happyReduce_53 = happySpecReduce_1  13# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (happy_var_1
	)}

happyReduce_54 = happySpecReduce_1  13# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (do e <- happy_var_1; return (BoolTest e)
	)}

happyReduce_55 = happySpecReduce_3  14# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	happyIn18
		 (do e1 <- happy_var_1; e2 <- happy_var_3; 
					  return (BoolAnd e1 e2)
	)}}

happyReduce_56 = happySpecReduce_3  14# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	happyIn18
		 (do e1 <- happy_var_1; e2 <- happy_var_3; 
					  return (BoolOr e1 e2)
	)}}

happyReduce_57 = happySpecReduce_2  14# happyReduction_57
happyReduction_57 happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (do e <- happy_var_2; return (BoolNot e)
	)}

happyReduce_58 = happySpecReduce_3  14# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (happy_var_2
	)}

happyReduce_59 = happySpecReduce_0  15# happyReduction_59
happyReduction_59  =  happyIn19
		 (CmmUnsafe
	)

happyReduce_60 = happyMonadReduce 1# 15# happyReduction_60
happyReduction_60 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { (L _ (CmmT_String	happy_var_1)) -> 
	( parseSafety happy_var_1)}
	) (\r -> happyReturn (happyIn19 r))

happyReduce_61 = happySpecReduce_0  16# happyReduction_61
happyReduction_61  =  happyIn20
		 (Nothing
	)

happyReduce_62 = happySpecReduce_2  16# happyReduction_62
happyReduction_62 happy_x_2
	happy_x_1
	 =  happyIn20
		 (Just []
	)

happyReduce_63 = happySpecReduce_3  16# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (Just happy_var_2
	)}

happyReduce_64 = happySpecReduce_1  17# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_GlobalReg   happy_var_1)) -> 
	happyIn21
		 ([happy_var_1]
	)}

happyReduce_65 = happySpecReduce_3  17# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_GlobalReg   happy_var_1)) -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_66 = happyReduce 5# 18# happyReduction_66
happyReduction_66 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (L _ (CmmT_Int		happy_var_2)) -> 
	case happyOutTok happy_x_4 of { (L _ (CmmT_Int		happy_var_4)) -> 
	happyIn22
		 (Just (fromIntegral happy_var_2, fromIntegral happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_67 = happySpecReduce_0  18# happyReduction_67
happyReduction_67  =  happyIn22
		 (Nothing
	)

happyReduce_68 = happySpecReduce_0  19# happyReduction_68
happyReduction_68  =  happyIn23
		 ([]
	)

happyReduce_69 = happySpecReduce_2  19# happyReduction_69
happyReduction_69 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn23
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_70 = happyReduce 6# 20# happyReduction_70
happyReduction_70 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut12 happy_x_5 of { happy_var_5 -> 
	happyIn24
		 ((happy_var_2, happy_var_5)
	) `HappyStk` happyRest}}

happyReduce_71 = happySpecReduce_1  21# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Int		happy_var_1)) -> 
	happyIn25
		 ([ fromIntegral happy_var_1 ]
	)}

happyReduce_72 = happySpecReduce_3  21# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Int		happy_var_1)) -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 (fromIntegral happy_var_1 : happy_var_3
	)}}

happyReduce_73 = happyReduce 5# 22# happyReduction_73
happyReduction_73 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_4 of { happy_var_4 -> 
	happyIn26
		 (Just happy_var_4
	) `HappyStk` happyRest}

happyReduce_74 = happySpecReduce_0  22# happyReduction_74
happyReduction_74  =  happyIn26
		 (Nothing
	)

happyReduce_75 = happySpecReduce_0  23# happyReduction_75
happyReduction_75  =  happyIn27
		 (nopEC
	)

happyReduce_76 = happyReduce 4# 23# happyReduction_76
happyReduction_76 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 (happy_var_3
	) `HappyStk` happyRest}

happyReduce_77 = happySpecReduce_3  24# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_U_Quot [happy_var_1,happy_var_3]
	)}}

happyReduce_78 = happySpecReduce_3  24# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_Mul [happy_var_1,happy_var_3]
	)}}

happyReduce_79 = happySpecReduce_3  24# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_U_Rem [happy_var_1,happy_var_3]
	)}}

happyReduce_80 = happySpecReduce_3  24# happyReduction_80
happyReduction_80 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_Sub [happy_var_1,happy_var_3]
	)}}

happyReduce_81 = happySpecReduce_3  24# happyReduction_81
happyReduction_81 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_Add [happy_var_1,happy_var_3]
	)}}

happyReduce_82 = happySpecReduce_3  24# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_U_Shr [happy_var_1,happy_var_3]
	)}}

happyReduce_83 = happySpecReduce_3  24# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_Shl [happy_var_1,happy_var_3]
	)}}

happyReduce_84 = happySpecReduce_3  24# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_And [happy_var_1,happy_var_3]
	)}}

happyReduce_85 = happySpecReduce_3  24# happyReduction_85
happyReduction_85 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_Xor [happy_var_1,happy_var_3]
	)}}

happyReduce_86 = happySpecReduce_3  24# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_Or [happy_var_1,happy_var_3]
	)}}

happyReduce_87 = happySpecReduce_3  24# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_U_Ge [happy_var_1,happy_var_3]
	)}}

happyReduce_88 = happySpecReduce_3  24# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_U_Gt [happy_var_1,happy_var_3]
	)}}

happyReduce_89 = happySpecReduce_3  24# happyReduction_89
happyReduction_89 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_U_Le [happy_var_1,happy_var_3]
	)}}

happyReduce_90 = happySpecReduce_3  24# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_U_Lt [happy_var_1,happy_var_3]
	)}}

happyReduce_91 = happySpecReduce_3  24# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_Ne [happy_var_1,happy_var_3]
	)}}

happyReduce_92 = happySpecReduce_3  24# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (mkMachOp MO_Eq [happy_var_1,happy_var_3]
	)}}

happyReduce_93 = happySpecReduce_2  24# happyReduction_93
happyReduction_93 happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_2 of { happy_var_2 -> 
	happyIn28
		 (mkMachOp MO_Not [happy_var_2]
	)}

happyReduce_94 = happySpecReduce_2  24# happyReduction_94
happyReduction_94 happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_2 of { happy_var_2 -> 
	happyIn28
		 (mkMachOp MO_S_Neg [happy_var_2]
	)}

happyReduce_95 = happyMonadReduce 5# 24# happyReduction_95
happyReduction_95 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { (L _ (CmmT_Name	happy_var_3)) -> 
	case happyOut29 happy_x_5 of { happy_var_5 -> 
	( do { mo <- nameToMachOp happy_var_3 ;
					        return (mkMachOp mo [happy_var_1,happy_var_5]) })}}}
	) (\r -> happyReturn (happyIn28 r))

happyReduce_96 = happySpecReduce_1  24# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (happy_var_1
	)}

happyReduce_97 = happySpecReduce_2  25# happyReduction_97
happyReduction_97 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Int		happy_var_1)) -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn29
		 (return (CmmLit (CmmInt happy_var_1 happy_var_2))
	)}}

happyReduce_98 = happySpecReduce_2  25# happyReduction_98
happyReduction_98 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Float	happy_var_1)) -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn29
		 (return (CmmLit (CmmFloat happy_var_1 happy_var_2))
	)}}

happyReduce_99 = happySpecReduce_1  25# happyReduction_99
happyReduction_99 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_String	happy_var_1)) -> 
	happyIn29
		 (do s <- code (mkStringCLit happy_var_1); 
				      return (CmmLit s)
	)}

happyReduce_100 = happySpecReduce_1  25# happyReduction_100
happyReduction_100 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (happy_var_1
	)}

happyReduce_101 = happyReduce 4# 25# happyReduction_101
happyReduction_101 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn29
		 (do e <- happy_var_3; return (CmmLoad e happy_var_1)
	) `HappyStk` happyRest}}

happyReduce_102 = happyMonadReduce 5# 25# happyReduction_102
happyReduction_102 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_2 of { (L _ (CmmT_Name	happy_var_2)) -> 
	case happyOut35 happy_x_4 of { happy_var_4 -> 
	( exprOp happy_var_2 happy_var_4)}}
	) (\r -> happyReturn (happyIn29 r))

happyReduce_103 = happySpecReduce_3  25# happyReduction_103
happyReduction_103 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_2 of { happy_var_2 -> 
	happyIn29
		 (happy_var_2
	)}

happyReduce_104 = happySpecReduce_0  26# happyReduction_104
happyReduction_104  =  happyIn30
		 (wordRep
	)

happyReduce_105 = happySpecReduce_2  26# happyReduction_105
happyReduction_105 happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_2 of { happy_var_2 -> 
	happyIn30
		 (happy_var_2
	)}

happyReduce_106 = happySpecReduce_0  27# happyReduction_106
happyReduction_106  =  happyIn31
		 ([]
	)

happyReduce_107 = happySpecReduce_3  27# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (happy_var_2
	)}

happyReduce_108 = happySpecReduce_0  28# happyReduction_108
happyReduction_108  =  happyIn32
		 ([]
	)

happyReduce_109 = happySpecReduce_1  28# happyReduction_109
happyReduction_109 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (happy_var_1
	)}

happyReduce_110 = happySpecReduce_1  29# happyReduction_110
happyReduction_110 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 ([happy_var_1]
	)}

happyReduce_111 = happySpecReduce_3  29# happyReduction_111
happyReduction_111 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_112 = happySpecReduce_1  30# happyReduction_112
happyReduction_112 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (do e <- happy_var_1; return (e, inferHint e)
	)}

happyReduce_113 = happyMonadReduce 2# 30# happyReduction_113
happyReduction_113 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (L _ (CmmT_String	happy_var_2)) -> 
	( do h <- parseHint happy_var_2;
					      return $ do
						e <- happy_var_1; return (e,h))}}
	) (\r -> happyReturn (happyIn34 r))

happyReduce_114 = happySpecReduce_0  31# happyReduction_114
happyReduction_114  =  happyIn35
		 ([]
	)

happyReduce_115 = happySpecReduce_1  31# happyReduction_115
happyReduction_115 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (happy_var_1
	)}

happyReduce_116 = happySpecReduce_1  32# happyReduction_116
happyReduction_116 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ([ happy_var_1 ]
	)}

happyReduce_117 = happySpecReduce_3  32# happyReduction_117
happyReduction_117 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_118 = happySpecReduce_1  33# happyReduction_118
happyReduction_118 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name	happy_var_1)) -> 
	happyIn37
		 (lookupName happy_var_1
	)}

happyReduce_119 = happySpecReduce_1  33# happyReduction_119
happyReduction_119 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_GlobalReg   happy_var_1)) -> 
	happyIn37
		 (return (CmmReg (CmmGlobal happy_var_1))
	)}

happyReduce_120 = happySpecReduce_0  34# happyReduction_120
happyReduction_120  =  happyIn38
		 ([]
	)

happyReduce_121 = happyReduce 4# 34# happyReduction_121
happyReduction_121 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_2 of { happy_var_2 -> 
	happyIn38
		 (happy_var_2
	) `HappyStk` happyRest}

happyReduce_122 = happySpecReduce_1  35# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 ([happy_var_1]
	)}

happyReduce_123 = happySpecReduce_2  35# happyReduction_123
happyReduction_123 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 ([happy_var_1]
	)}

happyReduce_124 = happySpecReduce_3  35# happyReduction_124
happyReduction_124 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_125 = happySpecReduce_1  36# happyReduction_125
happyReduction_125 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (do e <- happy_var_1; return (e, inferHint (CmmReg (CmmLocal e)))
	)}

happyReduce_126 = happyMonadReduce 2# 36# happyReduction_126
happyReduction_126 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { (L _ (CmmT_String	happy_var_1)) -> 
	case happyOut41 happy_x_2 of { happy_var_2 -> 
	( do h <- parseHint happy_var_1;
					      return $ do
						e <- happy_var_2; return (e,h))}}
	) (\r -> happyReturn (happyIn40 r))

happyReduce_127 = happySpecReduce_1  37# happyReduction_127
happyReduction_127 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name	happy_var_1)) -> 
	happyIn41
		 (do e <- lookupName happy_var_1;
				     return $
				       case e of 
					CmmReg (CmmLocal r) -> r
					other -> pprPanic "CmmParse:" (ftext happy_var_1 <> text " not a local register")
	)}

happyReduce_128 = happySpecReduce_1  38# happyReduction_128
happyReduction_128 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_Name	happy_var_1)) -> 
	happyIn42
		 (do e <- lookupName happy_var_1;
				     return $
				       case e of 
					CmmReg r -> r
					other -> pprPanic "CmmParse:" (ftext happy_var_1 <> text " not a register")
	)}

happyReduce_129 = happySpecReduce_1  38# happyReduction_129
happyReduction_129 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (CmmT_GlobalReg   happy_var_1)) -> 
	happyIn42
		 (return (CmmGlobal happy_var_1)
	)}

happyReduce_130 = happySpecReduce_0  39# happyReduction_130
happyReduction_130  =  happyIn43
		 ([]
	)

happyReduce_131 = happySpecReduce_3  39# happyReduction_131
happyReduction_131 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_2 of { happy_var_2 -> 
	happyIn43
		 (happy_var_2
	)}

happyReduce_132 = happySpecReduce_0  40# happyReduction_132
happyReduction_132  =  happyIn44
		 ([]
	)

happyReduce_133 = happySpecReduce_1  40# happyReduction_133
happyReduction_133 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (happy_var_1
	)}

happyReduce_134 = happySpecReduce_2  41# happyReduction_134
happyReduction_134 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 ([happy_var_1]
	)}

happyReduce_135 = happySpecReduce_1  41# happyReduction_135
happyReduction_135 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 ([happy_var_1]
	)}

happyReduce_136 = happySpecReduce_3  41# happyReduction_136
happyReduction_136 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_137 = happySpecReduce_2  42# happyReduction_137
happyReduction_137 happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (L _ (CmmT_Name	happy_var_2)) -> 
	happyIn46
		 (newLocal defaultKind happy_var_1 happy_var_2
	)}}

happyReduce_138 = happyMonadReduce 3# 42# happyReduction_138
happyReduction_138 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { (L _ (CmmT_String	happy_var_1)) -> 
	case happyOut49 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { (L _ (CmmT_Name	happy_var_3)) -> 
	( do k <- parseKind happy_var_1;
				     return $ newLocal k happy_var_2 happy_var_3)}}}
	) (\r -> happyReturn (happyIn46 r))

happyReduce_139 = happySpecReduce_0  43# happyReduction_139
happyReduction_139  =  happyIn47
		 (return Nothing
	)

happyReduce_140 = happyReduce 5# 43# happyReduction_140
happyReduction_140 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_2 of { happy_var_2 -> 
	case happyOut35 happy_x_4 of { happy_var_4 -> 
	happyIn47
		 (do { target <- happy_var_2;
					       args <- sequence happy_var_4;
					       return $ Just (UpdateFrame target args) }
	) `HappyStk` happyRest}}

happyReduce_141 = happySpecReduce_0  44# happyReduction_141
happyReduction_141  =  happyIn48
		 (return Nothing
	)

happyReduce_142 = happySpecReduce_2  44# happyReduction_142
happyReduction_142 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (L _ (CmmT_Name	happy_var_2)) -> 
	happyIn48
		 (do l <- lookupLabel happy_var_2; return (Just l)
	)}

happyReduce_143 = happySpecReduce_1  45# happyReduction_143
happyReduction_143 happy_x_1
	 =  happyIn49
		 (I8
	)

happyReduce_144 = happySpecReduce_1  45# happyReduction_144
happyReduction_144 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (happy_var_1
	)}

happyReduce_145 = happySpecReduce_1  46# happyReduction_145
happyReduction_145 happy_x_1
	 =  happyIn50
		 (I16
	)

happyReduce_146 = happySpecReduce_1  46# happyReduction_146
happyReduction_146 happy_x_1
	 =  happyIn50
		 (I32
	)

happyReduce_147 = happySpecReduce_1  46# happyReduction_147
happyReduction_147 happy_x_1
	 =  happyIn50
		 (I64
	)

happyReduce_148 = happySpecReduce_1  46# happyReduction_148
happyReduction_148 happy_x_1
	 =  happyIn50
		 (F32
	)

happyReduce_149 = happySpecReduce_1  46# happyReduction_149
happyReduction_149 happy_x_1
	 =  happyIn50
		 (F64
	)

happyNewToken action sts stk
	= cmmlex(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	L _ CmmT_EOF -> happyDoAction 67# tk action sts stk;
	L _ (CmmT_SpecChar ':') -> cont 1#;
	L _ (CmmT_SpecChar ';') -> cont 2#;
	L _ (CmmT_SpecChar '{') -> cont 3#;
	L _ (CmmT_SpecChar '}') -> cont 4#;
	L _ (CmmT_SpecChar '[') -> cont 5#;
	L _ (CmmT_SpecChar ']') -> cont 6#;
	L _ (CmmT_SpecChar '(') -> cont 7#;
	L _ (CmmT_SpecChar ')') -> cont 8#;
	L _ (CmmT_SpecChar '=') -> cont 9#;
	L _ (CmmT_SpecChar '`') -> cont 10#;
	L _ (CmmT_SpecChar '~') -> cont 11#;
	L _ (CmmT_SpecChar '/') -> cont 12#;
	L _ (CmmT_SpecChar '*') -> cont 13#;
	L _ (CmmT_SpecChar '%') -> cont 14#;
	L _ (CmmT_SpecChar '-') -> cont 15#;
	L _ (CmmT_SpecChar '+') -> cont 16#;
	L _ (CmmT_SpecChar '&') -> cont 17#;
	L _ (CmmT_SpecChar '^') -> cont 18#;
	L _ (CmmT_SpecChar '|') -> cont 19#;
	L _ (CmmT_SpecChar '>') -> cont 20#;
	L _ (CmmT_SpecChar '<') -> cont 21#;
	L _ (CmmT_SpecChar ',') -> cont 22#;
	L _ (CmmT_SpecChar '!') -> cont 23#;
	L _ (CmmT_DotDot) -> cont 24#;
	L _ (CmmT_DoubleColon) -> cont 25#;
	L _ (CmmT_Shr) -> cont 26#;
	L _ (CmmT_Shl) -> cont 27#;
	L _ (CmmT_Ge) -> cont 28#;
	L _ (CmmT_Le) -> cont 29#;
	L _ (CmmT_Eq) -> cont 30#;
	L _ (CmmT_Ne) -> cont 31#;
	L _ (CmmT_BoolAnd) -> cont 32#;
	L _ (CmmT_BoolOr) -> cont 33#;
	L _ (CmmT_CLOSURE) -> cont 34#;
	L _ (CmmT_INFO_TABLE) -> cont 35#;
	L _ (CmmT_INFO_TABLE_RET) -> cont 36#;
	L _ (CmmT_INFO_TABLE_FUN) -> cont 37#;
	L _ (CmmT_INFO_TABLE_CONSTR) -> cont 38#;
	L _ (CmmT_INFO_TABLE_SELECTOR) -> cont 39#;
	L _ (CmmT_else) -> cont 40#;
	L _ (CmmT_export) -> cont 41#;
	L _ (CmmT_section) -> cont 42#;
	L _ (CmmT_align) -> cont 43#;
	L _ (CmmT_goto) -> cont 44#;
	L _ (CmmT_if) -> cont 45#;
	L _ (CmmT_jump) -> cont 46#;
	L _ (CmmT_foreign) -> cont 47#;
	L _ (CmmT_never) -> cont 48#;
	L _ (CmmT_prim) -> cont 49#;
	L _ (CmmT_return) -> cont 50#;
	L _ (CmmT_returns) -> cont 51#;
	L _ (CmmT_import) -> cont 52#;
	L _ (CmmT_switch) -> cont 53#;
	L _ (CmmT_case) -> cont 54#;
	L _ (CmmT_default) -> cont 55#;
	L _ (CmmT_bits8) -> cont 56#;
	L _ (CmmT_bits16) -> cont 57#;
	L _ (CmmT_bits32) -> cont 58#;
	L _ (CmmT_bits64) -> cont 59#;
	L _ (CmmT_float32) -> cont 60#;
	L _ (CmmT_float64) -> cont 61#;
	L _ (CmmT_GlobalReg   happy_dollar_dollar) -> cont 62#;
	L _ (CmmT_Name	happy_dollar_dollar) -> cont 63#;
	L _ (CmmT_String	happy_dollar_dollar) -> cont 64#;
	L _ (CmmT_Int		happy_dollar_dollar) -> cont 65#;
	L _ (CmmT_Float	happy_dollar_dollar) -> cont 66#;
	_ -> happyError' tk
	})

happyError_ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (>>=)
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => Located CmmToken -> P a
happyError' tk = (\token -> happyError) tk

cmmParse = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq


section :: String -> Section
section "text"	 = Text
section "data" 	 = Data
section "rodata" = ReadOnlyData
section "relrodata" = RelocatableReadOnlyData
section "bss"	 = UninitialisedData
section s	 = OtherSection s

mkString :: String -> CmmStatic
mkString s = CmmString (map (fromIntegral.ord) s)

-- mkMachOp infers the type of the MachOp from the type of its first
-- argument.  We assume that this is correct: for MachOps that don't have
-- symmetrical args (e.g. shift ops), the first arg determines the type of
-- the op.
mkMachOp :: (MachRep -> MachOp) -> [ExtFCode CmmExpr] -> ExtFCode CmmExpr
mkMachOp fn args = do
  arg_exprs <- sequence args
  return (CmmMachOp (fn (cmmExprRep (head arg_exprs))) arg_exprs)

getLit :: CmmExpr -> CmmLit
getLit (CmmLit l) = l
getLit (CmmMachOp (MO_S_Neg _) [CmmLit (CmmInt i r)])  = CmmInt (negate i) r
getLit _ = panic "invalid literal" -- TODO messy failure

nameToMachOp :: FastString -> P (MachRep -> MachOp)
nameToMachOp name = 
  case lookupUFM machOps name of
	Nothing -> fail ("unknown primitive " ++ unpackFS name)
	Just m  -> return m

exprOp :: FastString -> [ExtFCode CmmExpr] -> P (ExtFCode CmmExpr)
exprOp name args_code =
  case lookupUFM exprMacros name of
     Just f  -> return $ do
        args <- sequence args_code
	return (f args)
     Nothing -> do
	mo <- nameToMachOp name
	return $ mkMachOp mo args_code

exprMacros :: UniqFM ([CmmExpr] -> CmmExpr)
exprMacros = listToUFM [
  ( FSLIT("ENTRY_CODE"),   \ [x] -> entryCode x ),
  ( FSLIT("INFO_PTR"),     \ [x] -> closureInfoPtr x ),
  ( FSLIT("STD_INFO"),     \ [x] -> infoTable x ),
  ( FSLIT("FUN_INFO"),     \ [x] -> funInfoTable x ),
  ( FSLIT("GET_ENTRY"),    \ [x] -> entryCode (closureInfoPtr x) ),
  ( FSLIT("GET_STD_INFO"), \ [x] -> infoTable (closureInfoPtr x) ),
  ( FSLIT("GET_FUN_INFO"), \ [x] -> funInfoTable (closureInfoPtr x) ),
  ( FSLIT("INFO_TYPE"),    \ [x] -> infoTableClosureType x ),
  ( FSLIT("INFO_PTRS"),    \ [x] -> infoTablePtrs x ),
  ( FSLIT("INFO_NPTRS"),   \ [x] -> infoTableNonPtrs x )
  ]

-- we understand a subset of C-- primitives:
machOps = listToUFM $
	map (\(x, y) -> (mkFastString x, y)) [
	( "add",	MO_Add ),
	( "sub",	MO_Sub ),
	( "eq",		MO_Eq ),
	( "ne",		MO_Ne ),
	( "mul",	MO_Mul ),
	( "neg",	MO_S_Neg ),
	( "quot",	MO_S_Quot ),
	( "rem",	MO_S_Rem ),
	( "divu",	MO_U_Quot ),
	( "modu",	MO_U_Rem ),

	( "ge",		MO_S_Ge ),
	( "le",		MO_S_Le ),
	( "gt",		MO_S_Gt ),
	( "lt",		MO_S_Lt ),

	( "geu",	MO_U_Ge ),
	( "leu",	MO_U_Le ),
	( "gtu",	MO_U_Gt ),
	( "ltu",	MO_U_Lt ),

	( "flt",	MO_S_Lt ),
	( "fle",	MO_S_Le ),
	( "feq",	MO_Eq ),
	( "fne",	MO_Ne ),
	( "fgt",	MO_S_Gt ),
	( "fge",	MO_S_Ge ),
	( "fneg",	MO_S_Neg ),

	( "and",	MO_And ),
	( "or",		MO_Or ),
	( "xor",	MO_Xor ),
	( "com",	MO_Not ),
	( "shl",	MO_Shl ),
	( "shrl",	MO_U_Shr ),
	( "shra",	MO_S_Shr ),

	( "lobits8",  flip MO_U_Conv I8  ),
	( "lobits16", flip MO_U_Conv I16 ),
	( "lobits32", flip MO_U_Conv I32 ),
	( "lobits64", flip MO_U_Conv I64 ),
	( "sx16",     flip MO_S_Conv I16 ),
	( "sx32",     flip MO_S_Conv I32 ),
	( "sx64",     flip MO_S_Conv I64 ),
	( "zx16",     flip MO_U_Conv I16 ),
	( "zx32",     flip MO_U_Conv I32 ),
	( "zx64",     flip MO_U_Conv I64 ),
	( "f2f32",    flip MO_S_Conv F32 ),  -- TODO; rounding mode
	( "f2f64",    flip MO_S_Conv F64 ),  -- TODO; rounding mode
	( "f2i8",     flip MO_S_Conv I8 ),
	( "f2i16",    flip MO_S_Conv I16 ),
	( "f2i32",    flip MO_S_Conv I32 ),
	( "f2i64",    flip MO_S_Conv I64 ),
	( "i2f32",    flip MO_S_Conv F32 ),
	( "i2f64",    flip MO_S_Conv F64 )
	]

callishMachOps = listToUFM $
	map (\(x, y) -> (mkFastString x, y)) [
        ( "write_barrier", MO_WriteBarrier )
        -- ToDo: the rest, maybe
    ]

parseSafety :: String -> P CmmSafety
parseSafety "safe"   = return (CmmSafe NoC_SRT)
parseSafety "unsafe" = return CmmUnsafe
parseSafety str      = fail ("unrecognised safety: " ++ str)

parseHint :: String -> P MachHint
parseHint "ptr"    = return PtrHint
parseHint "signed" = return SignedHint
parseHint "float"  = return FloatHint
parseHint str      = fail ("unrecognised hint: " ++ str)

parseKind :: String -> P Kind
parseKind "ptr"    = return KindPtr
parseKind str      = fail ("unrecognized kin: " ++ str)

defaultKind :: Kind
defaultKind = KindNonPtr

-- labels are always pointers, so we might as well infer the hint
inferHint :: CmmExpr -> MachHint
inferHint (CmmLit (CmmLabel _)) = PtrHint
inferHint (CmmReg (CmmGlobal g)) | isPtrGlobalReg g = PtrHint
inferHint _ = NoHint

isPtrGlobalReg Sp		= True
isPtrGlobalReg SpLim		= True
isPtrGlobalReg Hp		= True
isPtrGlobalReg HpLim		= True
isPtrGlobalReg CurrentTSO	= True
isPtrGlobalReg CurrentNursery	= True
isPtrGlobalReg _		= False

happyError :: P a
happyError = srcParseFail

-- -----------------------------------------------------------------------------
-- Statement-level macros

stmtMacro :: FastString -> [ExtFCode CmmExpr] -> P ExtCode
stmtMacro fun args_code = do
  case lookupUFM stmtMacros fun of
    Nothing -> fail ("unknown macro: " ++ unpackFS fun)
    Just fcode -> return $ do
	args <- sequence args_code
	code (fcode args)

stmtMacros :: UniqFM ([CmmExpr] -> Code)
stmtMacros = listToUFM [
  ( FSLIT("CCS_ALLOC"),		   \[words,ccs]  -> profAlloc words ccs ),
  ( FSLIT("CLOSE_NURSERY"),	   \[]  -> emitCloseNursery ),
  ( FSLIT("ENTER_CCS_PAP_CL"),     \[e] -> enterCostCentrePAP e ),
  ( FSLIT("ENTER_CCS_THUNK"),      \[e] -> enterCostCentreThunk e ),
  ( FSLIT("HP_CHK_GEN"),           \[words,liveness,reentry] -> 
                                      hpChkGen words liveness reentry ),
  ( FSLIT("HP_CHK_NP_ASSIGN_SP0"), \[e,f] -> hpChkNodePointsAssignSp0 e f ),
  ( FSLIT("LOAD_THREAD_STATE"),    \[] -> emitLoadThreadState ),
  ( FSLIT("LDV_ENTER"),            \[e] -> ldvEnter e ),
  ( FSLIT("LDV_RECORD_CREATE"),    \[e] -> ldvRecordCreate e ),
  ( FSLIT("OPEN_NURSERY"),	   \[]  -> emitOpenNursery ),
  ( FSLIT("PUSH_UPD_FRAME"),	   \[sp,e] -> emitPushUpdateFrame sp e ),
  ( FSLIT("SAVE_THREAD_STATE"),    \[] -> emitSaveThreadState ),
  ( FSLIT("SET_HDR"),		   \[ptr,info,ccs] -> 
					emitSetDynHdr ptr info ccs ),
  ( FSLIT("STK_CHK_GEN"),          \[words,liveness,reentry] -> 
                                      stkChkGen words liveness reentry ),
  ( FSLIT("STK_CHK_NP"),	   \[e] -> stkChkNodePoints e ),
  ( FSLIT("TICK_ALLOC_PRIM"), 	   \[hdr,goods,slop] -> 
					tickyAllocPrim hdr goods slop ),
  ( FSLIT("TICK_ALLOC_PAP"),       \[goods,slop] -> 
					tickyAllocPAP goods slop ),
  ( FSLIT("TICK_ALLOC_UP_THK"),    \[goods,slop] -> 
					tickyAllocThunk goods slop ),
  ( FSLIT("UPD_BH_UPDATABLE"),       \[] -> emitBlackHoleCode False ),
  ( FSLIT("UPD_BH_SINGLE_ENTRY"),    \[] -> emitBlackHoleCode True ),

  ( FSLIT("RET_P"),	\[a] ->       emitRetUT [(PtrArg,a)]),
  ( FSLIT("RET_N"),	\[a] ->       emitRetUT [(NonPtrArg,a)]),
  ( FSLIT("RET_PP"),	\[a,b] ->     emitRetUT [(PtrArg,a),(PtrArg,b)]),
  ( FSLIT("RET_NN"),	\[a,b] ->     emitRetUT [(NonPtrArg,a),(NonPtrArg,b)]),
  ( FSLIT("RET_NP"),	\[a,b] ->     emitRetUT [(NonPtrArg,a),(PtrArg,b)]),
  ( FSLIT("RET_PPP"),	\[a,b,c] ->   emitRetUT [(PtrArg,a),(PtrArg,b),(PtrArg,c)]),
  ( FSLIT("RET_NPP"),	\[a,b,c] ->   emitRetUT [(NonPtrArg,a),(PtrArg,b),(PtrArg,c)]),
  ( FSLIT("RET_NNP"),	\[a,b,c] ->   emitRetUT [(NonPtrArg,a),(NonPtrArg,b),(PtrArg,c)]),
  ( FSLIT("RET_NNNP"),	\[a,b,c,d] -> emitRetUT [(NonPtrArg,a),(NonPtrArg,b),(NonPtrArg,c),(PtrArg,d)]),
  ( FSLIT("RET_NPNP"),	\[a,b,c,d] -> emitRetUT [(NonPtrArg,a),(PtrArg,b),(NonPtrArg,c),(PtrArg,d)])

 ]

-- -----------------------------------------------------------------------------
-- Our extended FCode monad.

-- We add a mapping from names to CmmExpr, to support local variable names in
-- the concrete C-- code.  The unique supply of the underlying FCode monad
-- is used to grab a new unique for each local variable.

-- In C--, a local variable can be declared anywhere within a proc,
-- and it scopes from the beginning of the proc to the end.  Hence, we have
-- to collect declarations as we parse the proc, and feed the environment
-- back in circularly (to avoid a two-pass algorithm).

data Named = Var CmmExpr | Label BlockId
type Decls = [(FastString,Named)]
type Env   = UniqFM Named

newtype ExtFCode a = EC { unEC :: Env -> Decls -> FCode (Decls, a) }

type ExtCode = ExtFCode ()

returnExtFC a = EC $ \e s -> return (s, a)
thenExtFC (EC m) k = EC $ \e s -> do (s',r) <- m e s; unEC (k r) e s'

instance Monad ExtFCode where
  (>>=) = thenExtFC
  return = returnExtFC

-- This function takes the variable decarations and imports and makes 
-- an environment, which is looped back into the computation.  In this
-- way, we can have embedded declarations that scope over the whole
-- procedure, and imports that scope over the entire module.
-- Discards the local declaration contained within decl'
loopDecls :: ExtFCode a -> ExtFCode a
loopDecls (EC fcode) =
      EC $ \e globalDecls -> do
	(decls', a) <- fixC (\ ~(decls,a) -> fcode (addListToUFM e (decls ++ globalDecls)) globalDecls)
	return (globalDecls, a)

getEnv :: ExtFCode Env
getEnv = EC $ \e s -> return (s, e)

addVarDecl :: FastString -> CmmExpr -> ExtCode
addVarDecl var expr = EC $ \e s -> return ((var, Var expr):s, ())

addLabel :: FastString -> BlockId -> ExtCode
addLabel name block_id = EC $ \e s -> return ((name, Label block_id):s, ())

newLocal :: Kind -> MachRep -> FastString -> ExtFCode LocalReg
newLocal kind ty name = do
   u <- code newUnique
   let reg = LocalReg u ty kind
   addVarDecl name (CmmReg (CmmLocal reg))
   return reg

-- Creates a foreign label in the import. CLabel's labelDynamic
-- classifies these labels as dynamic, hence the code generator emits the
-- PIC code for them.
newImport :: FastString -> ExtFCode ()
newImport name
   = addVarDecl name (CmmLit (CmmLabel (mkForeignLabel name Nothing True)))

newLabel :: FastString -> ExtFCode BlockId
newLabel name = do
   u <- code newUnique
   addLabel name (BlockId u)
   return (BlockId u)

lookupLabel :: FastString -> ExtFCode BlockId
lookupLabel name = do
  env <- getEnv
  return $ 
     case lookupUFM env name of
	Just (Label l) -> l
	_other -> BlockId (newTagUnique (getUnique name) 'L')

-- Unknown names are treated as if they had been 'import'ed.
-- This saves us a lot of bother in the RTS sources, at the expense of
-- deferring some errors to link time.
lookupName :: FastString -> ExtFCode CmmExpr
lookupName name = do
  env <- getEnv
  return $ 
     case lookupUFM env name of
	Just (Var e) -> e
	_other -> CmmLit (CmmLabel (mkRtsCodeLabelFS name))

-- Lifting FCode computations into the ExtFCode monad:
code :: FCode a -> ExtFCode a
code fc = EC $ \e s -> do r <- fc; return (s, r)

code2 :: (FCode (Decls,b) -> FCode ((Decls,b),c))
	 -> ExtFCode b -> ExtFCode c
code2 f (EC ec) = EC $ \e s -> do ((s',b),c) <- f (ec e s); return (s',c)

nopEC = code nopC
stmtEC stmt = code (stmtC stmt)
stmtsEC stmts = code (stmtsC stmts)
getCgStmtsEC = code2 getCgStmts'
getCgStmtsEC' = code2 (\m -> getCgStmts' m >>= f)
  where f ((decl, b), c) = return ((decl, b), (b, c))

forkLabelledCodeEC ec = do
  stmts <- getCgStmtsEC ec
  code (forkCgStmts stmts)


profilingInfo desc_str ty_str = do
  lit1 <- if opt_SccProfilingOn 
		   then code $ mkStringCLit desc_str
		   else return (mkIntCLit 0)
  lit2 <- if opt_SccProfilingOn 
		   then code $ mkStringCLit ty_str
		   else return (mkIntCLit 0)
  return (ProfilingInfo lit1 lit2)


staticClosure :: FastString -> FastString -> [CmmLit] -> ExtCode
staticClosure cl_label info payload
  = code $ emitDataLits (mkRtsDataLabelFS cl_label) lits
  where  lits = mkStaticClosure (mkRtsInfoLabelFS info) dontCareCCS payload [] [] []

foreignCall
	:: String
	-> [ExtFCode (CmmFormal,MachHint)]
	-> ExtFCode CmmExpr
	-> [ExtFCode (CmmExpr,MachHint)]
	-> Maybe [GlobalReg]
        -> CmmSafety
        -> CmmReturnInfo
        -> P ExtCode
foreignCall conv_string results_code expr_code args_code vols safety ret
  = do  convention <- case conv_string of
          "C" -> return CCallConv
          "stdcall" -> return StdCallConv
          "C--" -> return CmmCallConv
          _ -> fail ("unknown calling convention: " ++ conv_string)
	return $ do
	  results <- sequence results_code
	  expr <- expr_code
	  args <- sequence args_code
	  --code (stmtC (CmmCall (CmmCallee expr convention) results args safety))
          case convention of
            -- Temporary hack so at least some functions are CmmSafe
            CmmCallConv -> code (stmtC (CmmCall (CmmCallee expr convention) results args safety ret))
            _ -> 
             let expr' = adjCallTarget convention expr args in
             case safety of
	      CmmUnsafe ->
                code (emitForeignCall' PlayRisky results 
                   (CmmCallee expr' convention) args vols NoC_SRT ret)
              CmmSafe srt ->
                code (emitForeignCall' (PlaySafe unused) results 
                   (CmmCallee expr' convention) args vols NoC_SRT ret) where
	        unused = panic "not used by emitForeignCall'"

adjCallTarget :: CCallConv -> CmmExpr -> [(CmmExpr,MachHint)] -> CmmExpr
#ifdef mingw32_TARGET_OS
-- On Windows, we have to add the '@N' suffix to the label when making
-- a call with the stdcall calling convention.
adjCallTarget StdCallConv (CmmLit (CmmLabel lbl)) args
  = CmmLit (CmmLabel (addLabelSize lbl (sum (map size args))))
  where size (e,_) = max wORD_SIZE (machRepByteWidth (cmmExprRep e))
                 -- c.f. CgForeignCall.emitForeignCall
#endif
adjCallTarget _ expr _
  = expr

primCall
	:: [ExtFCode (CmmFormal,MachHint)]
	-> FastString
	-> [ExtFCode (CmmExpr,MachHint)]
	-> Maybe [GlobalReg]
        -> CmmSafety
        -> P ExtCode
primCall results_code name args_code vols safety
  = case lookupUFM callishMachOps name of
	Nothing -> fail ("unknown primitive " ++ unpackFS name)
	Just p  -> return $ do
		results <- sequence results_code
		args <- sequence args_code
		case safety of
		  CmmUnsafe ->
		    code (emitForeignCall' PlayRisky results
		      (CmmPrim p) args vols NoC_SRT CmmMayReturn)
		  CmmSafe srt ->
		    code (emitForeignCall' (PlaySafe unused) results 
		      (CmmPrim p) args vols NoC_SRT CmmMayReturn) where
		    unused = panic "not used by emitForeignCall'"

doStore :: MachRep -> ExtFCode CmmExpr  -> ExtFCode CmmExpr -> ExtCode
doStore rep addr_code val_code
  = do addr <- addr_code
       val <- val_code
	-- if the specified store type does not match the type of the expr
	-- on the rhs, then we insert a coercion that will cause the type
	-- mismatch to be flagged by cmm-lint.  If we don't do this, then
	-- the store will happen at the wrong type, and the error will not
	-- be noticed.
       let coerce_val 
		| cmmExprRep val /= rep = CmmMachOp (MO_U_Conv rep rep) [val]
		| otherwise             = val
       stmtEC (CmmStore addr coerce_val)

-- Return an unboxed tuple.
emitRetUT :: [(CgRep,CmmExpr)] -> Code
emitRetUT args = do
  tickyUnboxedTupleReturn (length args)  -- TICK
  (sp, stmts) <- pushUnboxedTuple 0 args
  emitStmts stmts
  when (sp /= 0) $ stmtC (CmmAssign spReg (cmmRegOffW spReg (-sp)))
  stmtC (CmmJump (entryCode (CmmLoad (cmmRegOffW spReg sp) wordRep)) [])
  -- TODO (when using CPS): emitStmt (CmmReturn (map snd args))

-- -----------------------------------------------------------------------------
-- If-then-else and boolean expressions

data BoolExpr
  = BoolExpr `BoolAnd` BoolExpr
  | BoolExpr `BoolOr`  BoolExpr
  | BoolNot BoolExpr
  | BoolTest CmmExpr

-- ToDo: smart constructors which simplify the boolean expression.

ifThenElse cond then_part else_part = do
     then_id <- code newLabelC
     join_id <- code newLabelC
     c <- cond
     emitCond c then_id
     else_part
     stmtEC (CmmBranch join_id)
     code (labelC then_id)
     then_part
     -- fall through to join
     code (labelC join_id)

-- 'emitCond cond true_id'  emits code to test whether the cond is true,
-- branching to true_id if so, and falling through otherwise.
emitCond (BoolTest e) then_id = do
  stmtEC (CmmCondBranch e then_id)
emitCond (BoolNot (BoolTest (CmmMachOp op args))) then_id
  | Just op' <- maybeInvertComparison op
  = emitCond (BoolTest (CmmMachOp op' args)) then_id
emitCond (BoolNot e) then_id = do
  else_id <- code newLabelC
  emitCond e else_id
  stmtEC (CmmBranch then_id)
  code (labelC else_id)
emitCond (e1 `BoolOr` e2) then_id = do
  emitCond e1 then_id
  emitCond e2 then_id
emitCond (e1 `BoolAnd` e2) then_id = do
	-- we'd like to invert one of the conditionals here to avoid an
	-- extra branch instruction, but we can't use maybeInvertComparison
	-- here because we can't look too closely at the expression since
	-- we're in a loop.
  and_id <- code newLabelC
  else_id <- code newLabelC
  emitCond e1 and_id
  stmtEC (CmmBranch else_id)
  code (labelC and_id)
  emitCond e2 then_id
  code (labelC else_id)


-- -----------------------------------------------------------------------------
-- Table jumps

-- We use a simplified form of C-- switch statements for now.  A
-- switch statement always compiles to a table jump.  Each arm can
-- specify a list of values (not ranges), and there can be a single
-- default branch.  The range of the table is given either by the
-- optional range on the switch (eg. switch [0..7] {...}), or by
-- the minimum/maximum values from the branches.

doSwitch :: Maybe (Int,Int) -> ExtFCode CmmExpr -> [([Int],ExtCode)]
         -> Maybe ExtCode -> ExtCode
doSwitch mb_range scrut arms deflt
   = do 
	-- Compile code for the default branch
	dflt_entry <- 
		case deflt of
		  Nothing -> return Nothing
		  Just e  -> do b <- forkLabelledCodeEC e; return (Just b)

	-- Compile each case branch
	table_entries <- mapM emitArm arms

	-- Construct the table
	let
	    all_entries = concat table_entries
	    ixs = map fst all_entries
	    (min,max) 
		| Just (l,u) <- mb_range = (l,u)
		| otherwise              = (minimum ixs, maximum ixs)

	    entries = elems (accumArray (\_ a -> Just a) dflt_entry (min,max)
				all_entries)
	expr <- scrut
	-- ToDo: check for out of range and jump to default if necessary
        stmtEC (CmmSwitch expr entries)
   where
	emitArm :: ([Int],ExtCode) -> ExtFCode [(Int,BlockId)]
	emitArm (ints,code) = do
	   blockid <- forkLabelledCodeEC code
	   return [ (i,blockid) | i <- ints ]


-- -----------------------------------------------------------------------------
-- Putting it all together

-- The initial environment: we define some constants that the compiler
-- knows about here.
initEnv :: Env
initEnv = listToUFM [
  ( FSLIT("SIZEOF_StgHeader"), 
    Var (CmmLit (CmmInt (fromIntegral (fixedHdrSize * wORD_SIZE)) wordRep) )),
  ( FSLIT("SIZEOF_StgInfoTable"),
    Var (CmmLit (CmmInt (fromIntegral stdInfoTableSizeB) wordRep) ))
  ]

parseCmmFile :: DynFlags -> FilePath -> IO (Maybe Cmm)
parseCmmFile dflags filename = do
  showPass dflags "ParseCmm"
  buf <- hGetStringBuffer filename
  let
	init_loc = mkSrcLoc (mkFastString filename) 1 0
	init_state = (mkPState buf init_loc dflags) { lex_state = [0] }
		-- reset the lex_state: the Lexer monad leaves some stuff
		-- in there we don't want.
  case unP cmmParse init_state of
    PFailed span err -> do printError span err; return Nothing
    POk pst code -> do
	cmm <- initC dflags no_module (getCmm (unEC code initEnv [] >> return ()))
	let ms = getMessages pst
	printErrorsAndWarnings dflags ms
        when (errorsFound dflags ms) $ exitWith (ExitFailure 1)
        dumpIfSet_dyn dflags Opt_D_dump_cmm "Cmm" (pprCmms [cmm])
	return (Just cmm)
  where
	no_module = panic "parseCmmFile: no module"
{-# LINE 1 "GenericTemplate.hs" #-}
{-# LINE 1 "GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Int# Happy_IntList





{-# LINE 49 "GenericTemplate.hs" #-}

{-# LINE 59 "GenericTemplate.hs" #-}

{-# LINE 68 "GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st

{-# LINE 127 "GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ > 500
	narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	intToInt16# i
#else
	(i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
	i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
#else
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
#endif
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#





data HappyAddr = HappyA# Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             off    = indexShortOffAddr happyGotoOffsets st1
             off_i  = (off +# nt)
             new_state = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
