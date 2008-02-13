{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module ParserCore ( parseCore ) where

import IfaceSyn
import ForeignCall
import RdrHsSyn
import HsSyn
import RdrName
import OccName
import Type ( Kind,
              liftedTypeKindTyCon, openTypeKindTyCon, unliftedTypeKindTyCon,
              argTypeKindTyCon, ubxTupleKindTyCon, mkArrowKind, mkTyConApp
            )
import Name( Name, nameOccName, nameModule, mkExternalName )
import Module
import PackageConfig	( mainPackageId, stringToPackageId )
import ParserCoreUtils
import LexCore
import Literal
import SrcLoc
import TysPrim( wordPrimTyCon, intPrimTyCon, charPrimTyCon, 
		floatPrimTyCon, doublePrimTyCon, addrPrimTyCon )
import TyCon ( TyCon, tyConName )
import FastString
import Outputable
import Char
import Unique

#include "../HsVersions.h"
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
happyIn4 :: (HsExtCore RdrName) -> (HappyAbsSyn )
happyIn4 x = unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (HsExtCore RdrName)
happyOut4 x = unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (Module) -> (HappyAbsSyn )
happyIn5 x = unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (Module)
happyOut5 x = unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (Name) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (Name)
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (Name) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (Name)
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (Name) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (Name)
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ([String]) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ([String])
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ([String]) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ([String])
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([TyClDecl RdrName]) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ([TyClDecl RdrName])
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (TyClDecl RdrName) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (TyClDecl RdrName)
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (OccName -> [LConDecl RdrName]) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (OccName -> [LConDecl RdrName])
happyOut13 x = unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([LConDecl RdrName]) -> (HappyAbsSyn )
happyIn14 x = unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([LConDecl RdrName])
happyOut14 x = unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (LConDecl RdrName) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (LConDecl RdrName)
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ([LHsTyVarBndr RdrName]) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ([LHsTyVarBndr RdrName])
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: ([LHsType RdrName]) -> (HappyAbsSyn )
happyIn17 x = unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> ([LHsType RdrName])
happyOut17 x = unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: ([IfaceType]) -> (HappyAbsSyn )
happyIn18 x = unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> ([IfaceType])
happyOut18 x = unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (IfaceType) -> (HappyAbsSyn )
happyIn19 x = unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (IfaceType)
happyOut19 x = unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (IfaceType) -> (HappyAbsSyn )
happyIn20 x = unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (IfaceType)
happyOut20 x = unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (IfaceType) -> (HappyAbsSyn )
happyIn21 x = unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (IfaceType)
happyOut21 x = unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: ([IfaceBinding]) -> (HappyAbsSyn )
happyIn22 x = unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> ([IfaceBinding])
happyOut22 x = unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (IfaceBinding) -> (HappyAbsSyn )
happyIn23 x = unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (IfaceBinding)
happyOut23 x = unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ([(IfaceLetBndr, IfaceExpr)]) -> (HappyAbsSyn )
happyIn24 x = unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> ([(IfaceLetBndr, IfaceExpr)])
happyOut24 x = unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: ((IfaceLetBndr, IfaceExpr)) -> (HappyAbsSyn )
happyIn25 x = unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> ((IfaceLetBndr, IfaceExpr))
happyOut25 x = unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (IfaceBndr) -> (HappyAbsSyn )
happyIn26 x = unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (IfaceBndr)
happyOut26 x = unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ([IfaceBndr]) -> (HappyAbsSyn )
happyIn27 x = unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> ([IfaceBndr])
happyOut27 x = unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (IfaceIdBndr) -> (HappyAbsSyn )
happyIn28 x = unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (IfaceIdBndr)
happyOut28 x = unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (IfaceTvBndr) -> (HappyAbsSyn )
happyIn29 x = unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (IfaceTvBndr)
happyOut29 x = unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: ([IfaceTvBndr]) -> (HappyAbsSyn )
happyIn30 x = unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> ([IfaceTvBndr])
happyOut30 x = unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (IfaceKind) -> (HappyAbsSyn )
happyIn31 x = unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (IfaceKind)
happyOut31 x = unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (IfaceKind) -> (HappyAbsSyn )
happyIn32 x = unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (IfaceKind)
happyOut32 x = unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (IfaceExpr) -> (HappyAbsSyn )
happyIn33 x = unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (IfaceExpr)
happyOut33 x = unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (IfaceExpr) -> (HappyAbsSyn )
happyIn34 x = unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (IfaceExpr)
happyOut34 x = unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (IfaceExpr) -> (HappyAbsSyn )
happyIn35 x = unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (IfaceExpr)
happyOut35 x = unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: ([IfaceAlt]) -> (HappyAbsSyn )
happyIn36 x = unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> ([IfaceAlt])
happyOut36 x = unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (IfaceAlt) -> (HappyAbsSyn )
happyIn37 x = unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (IfaceAlt)
happyOut37 x = unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (Literal) -> (HappyAbsSyn )
happyIn38 x = unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (Literal)
happyOut38 x = unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (FastString) -> (HappyAbsSyn )
happyIn39 x = unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (FastString)
happyOut39 x = unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (String) -> (HappyAbsSyn )
happyIn40 x = unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (String)
happyOut40 x = unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (OccName) -> (HappyAbsSyn )
happyIn41 x = unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (OccName)
happyOut41 x = unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyInTok :: Token -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> Token
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xa2\x01\xa2\x01\x94\x01\x92\x01\x6e\x01\x96\x01\x90\x01\xf2\x00\x6e\x01\x8f\x01\x8f\x01\x29\x01\x93\x01\x29\x01\x00\x00\x00\x00\x8c\x01\x00\x00\x8e\x01\x91\x01\x21\x00\x00\x00\x00\x00\x8a\x01\x89\x01\x00\x00\x21\x00\x08\x00\xf2\x00\x29\x01\x8d\x01\x00\x00\x88\x01\x85\x01\x8b\x01\x82\x01\x08\x00\x00\x00\x86\x01\x87\x01\x00\x00\x00\x00\x27\x01\x27\x01\x84\x01\x83\x01\x27\x01\x18\x01\x08\x00\x7d\x01\x81\x01\x80\x01\x00\x00\x21\x00\x00\x00\x4f\x01\x7f\x01\x7e\x01\x00\x00\x00\x00\x00\x01\x00\x00\x08\x00\x7c\x01\x24\x01\x08\x00\x00\x00\x00\x00\x7b\x01\x2c\x01\x00\x00\x00\x00\x7a\x01\x0e\x01\x00\x00\x00\x00\x00\x00\x79\x01\x78\x01\x03\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x01\x00\x00\x00\x00\x00\x00\xf2\x00\x77\x01\xe9\x00\x76\x01\x72\x01\x01\x01\x73\x00\x73\x01\x74\x01\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x71\x01\x00\x00\x4f\x01\x00\x00\x00\x00\x4f\x01\x73\x00\x70\x01\x00\x00\x5f\x01\xad\x00\x6f\x01\x6d\x01\x6c\x01\x6b\x01\x6a\x01\x38\x00\x24\x01\x38\x00\x08\x00\x75\x01\x00\x00\x38\x00\x38\x00\x08\x00\x2a\x00\x5b\x01\x69\x01\x66\x01\x61\x01\x68\x01\x0e\x01\x00\x00\x60\x00\x00\x00\x0e\x01\x08\x00\x00\x00\x00\x00\x5e\x01\x00\x00\x00\x00\x00\x00\x00\x00\x24\x01\x67\x01\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x01\x00\x01\x00\x00\x00\x00\x00\x60\x01\x24\x01\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x65\x01\x64\x01\x63\x01\x62\x01\xf3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x55\x01\x00\x00\x00\x00\x00\x00\x00\x00\x5d\x01\x00\x00\x4b\x01\x47\x01\xf5\x00\x31\x01\x44\x01\x3e\x01\x2e\x01\x21\x01\x3f\x01\x07\x01\x25\x01\x24\x01\x24\x01\xf5\x00\x00\x00\xfc\x00\x24\x01\x00\x00\x24\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x0a\x01\x00\x00\xf8\x00\x00\x00\x35\x01\x00\x00\xf9\x00\x1f\x01\xf4\x00\xe4\x00\xe0\x00\x3a\x01\x00\x00\x38\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2d\x00\x00\x00\x00\x00\x00\x00\xcf\x00\x00\x00\x57\x00\xc7\x00\x05\x01\x36\x01\x00\x00\x00\x00\x9d\x00\xb9\x00\xa9\x00\x00\x00\xc5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\x00\xd0\x00\x00\x00\x00\x00\xcc\x00\x34\x01\xc1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\xef\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\x00\x00\x00\xb6\x00\x00\x00\x6d\x00\xa0\x00\x00\x00\x00\x00\x31\x00\x8b\x00\x00\x00\x00\x00\x00\x00\x9c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x77\x00\x00\x00\x00\x00\x00\x00\xf6\xff\x00\x00\x74\x00\x00\x00\x00\x00\x4b\x00\x54\x01\x00\x00\x00\x00\x00\x00\x9e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x83\x00\x00\x00\x00\x00\x81\x00\x51\x01\x00\x00\x00\x00\x50\x00\x3c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf3\x00\x44\x00\xeb\x00\x41\x00\x00\x00\x00\x00\xe3\x00\x72\x00\x3a\x00\x28\x01\xfa\xff\x00\x00\x00\x00\x00\x00\x00\x00\x9a\x00\x00\x00\x00\x00\x00\x00\x98\x00\x0a\x00\x00\x00\x00\x00\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe2\x00\xde\x00\xda\x00\xcd\x00\x00\x00\x00\x00\x00\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x00\x00\x63\x00\x4e\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x14\x00\x0d\x00\x5c\x00\x00\x00\x00\x00\x06\x00\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\x00\x00\x00\x00\xdb\xff\xf5\xff\x00\x00\x00\x00\xcc\xff\x00\x00\xcc\xff\xf4\xff\xfe\xff\x00\x00\xd8\xff\x00\x00\x00\x00\x00\x00\xaa\xff\xfd\xff\xf9\xff\x00\x00\xd4\xff\x00\x00\x00\x00\xdb\xff\xcc\xff\x00\x00\xce\xff\x00\x00\x00\x00\xf1\xff\x00\x00\x00\x00\xfb\xff\x00\x00\x00\x00\xcb\xff\xda\xff\xe7\xff\xe7\xff\xde\xff\x00\x00\xe7\xff\xcc\xff\x00\x00\xaa\xff\x00\x00\xd7\xff\xf8\xff\x00\x00\xd9\xff\x00\x00\x00\x00\x00\x00\xe4\xff\xe2\xff\xe7\xff\xe5\xff\x00\x00\xaa\xff\x00\x00\x00\x00\xe1\xff\xe0\xff\xef\xff\x00\x00\xf0\xff\xf2\xff\x00\x00\x00\x00\xc9\xff\xca\xff\xc8\xff\x00\x00\xee\xff\xea\xff\xa8\xff\xdd\xff\xc1\xff\xc2\xff\xbc\xff\xbb\xff\xd5\xff\xc0\xff\xc3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaa\xff\x00\x00\xe6\xff\x00\x00\xdf\xff\xfa\xff\xf7\xff\xa9\xff\xf9\xff\xd6\xff\x00\x00\xdc\xff\xe3\xff\x00\x00\xd1\xff\x00\x00\xd2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbe\xff\x00\x00\xe7\xff\x00\x00\x00\x00\xef\xff\x00\x00\x00\x00\xc6\xff\x00\x00\x00\x00\xcd\xff\x00\x00\xc7\xff\x00\x00\x00\x00\xf3\xff\xed\xff\xea\xff\xeb\xff\xec\xff\xe8\xff\xbd\xff\x00\x00\x00\x00\xb7\xff\xb6\xff\xb5\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbf\xff\xd3\xff\x00\x00\x00\x00\xd0\xff\xfc\xff\xf6\xff\xba\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb9\xff\xe9\xff\xc4\xff\xc5\xff\x00\x00\xac\xff\xab\xff\xad\xff\xae\xff\x00\x00\xcf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb8\xff\x00\x00\x00\x00\xb1\xff\x00\x00\xb3\xff\xb0\xff\xaf\xff\xb2\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x0f\x00\x04\x00\x0a\x00\x0b\x00\x03\x00\x04\x00\x02\x00\x13\x00\x04\x00\x15\x00\x04\x00\x03\x00\x04\x00\x02\x00\x0f\x00\x04\x00\x1f\x00\x10\x00\x11\x00\x05\x00\x02\x00\x0f\x00\x04\x00\x23\x00\x10\x00\x11\x00\x1d\x00\x1e\x00\x1f\x00\x25\x00\x1f\x00\x22\x00\x23\x00\x1d\x00\x1e\x00\x1f\x00\x23\x00\x1f\x00\x22\x00\x23\x00\x1d\x00\x1e\x00\x1f\x00\x23\x00\x0d\x00\x22\x00\x23\x00\x1d\x00\x1e\x00\x1f\x00\x14\x00\x15\x00\x22\x00\x23\x00\x02\x00\x0f\x00\x04\x00\x0a\x00\x0b\x00\x03\x00\x04\x00\x02\x00\x1f\x00\x04\x00\x15\x00\x23\x00\x03\x00\x04\x00\x02\x00\x0f\x00\x04\x00\x1f\x00\x10\x00\x11\x00\x18\x00\x02\x00\x0c\x00\x04\x00\x23\x00\x10\x00\x11\x00\x1d\x00\x1e\x00\x1f\x00\x25\x00\x1f\x00\x22\x00\x23\x00\x1d\x00\x1e\x00\x1f\x00\x23\x00\x02\x00\x22\x00\x23\x00\x1d\x00\x1e\x00\x1f\x00\x23\x00\x02\x00\x22\x00\x23\x00\x1d\x00\x1e\x00\x1f\x00\x14\x00\x15\x00\x22\x00\x23\x00\x02\x00\x10\x00\x04\x00\x02\x00\x23\x00\x04\x00\x03\x00\x02\x00\x17\x00\x04\x00\x02\x00\x23\x00\x04\x00\x20\x00\x21\x00\x22\x00\x0d\x00\x0e\x00\x0f\x00\x0f\x00\x20\x00\x21\x00\x22\x00\x05\x00\x06\x00\x05\x00\x06\x00\x1d\x00\x1e\x00\x1f\x00\x1d\x00\x1b\x00\x22\x00\x23\x00\x1d\x00\x22\x00\x23\x00\x1d\x00\x23\x00\x22\x00\x23\x00\x0c\x00\x22\x00\x23\x00\x03\x00\x04\x00\x03\x00\x04\x00\x03\x00\x04\x00\x03\x00\x04\x00\x03\x00\x04\x00\x24\x00\x1b\x00\x24\x00\x10\x00\x11\x00\x10\x00\x11\x00\x10\x00\x11\x00\x10\x00\x11\x00\x10\x00\x11\x00\x09\x00\x1b\x00\x1c\x00\x1b\x00\x1c\x00\x1b\x00\x1c\x00\x03\x00\x04\x00\x23\x00\x0f\x00\x23\x00\x05\x00\x23\x00\x23\x00\x23\x00\x03\x00\x23\x00\x03\x00\x04\x00\x10\x00\x11\x00\x03\x00\x04\x00\x03\x00\x04\x00\x1f\x00\x0e\x00\x0f\x00\x03\x00\x03\x00\x10\x00\x11\x00\x03\x00\x05\x00\x10\x00\x11\x00\x10\x00\x11\x00\x23\x00\x0e\x00\x0f\x00\x0f\x00\x03\x00\x0e\x00\x0f\x00\x03\x00\x03\x00\x23\x00\x03\x00\x23\x00\x03\x00\x03\x00\x03\x00\x23\x00\x0f\x00\x23\x00\x0e\x00\x0f\x00\x0f\x00\x03\x00\x23\x00\x23\x00\x0f\x00\x0f\x00\x23\x00\x05\x00\x06\x00\x03\x00\x05\x00\x0f\x00\x01\x00\x0f\x00\x07\x00\x08\x00\x23\x00\x05\x00\x0d\x00\x23\x00\x23\x00\x0f\x00\x0e\x00\x0f\x00\x23\x00\x23\x00\x06\x00\x1f\x00\x08\x00\x00\x00\x0a\x00\x0b\x00\x0c\x00\x23\x00\x0f\x00\x0f\x00\x1f\x00\x04\x00\x24\x00\x1f\x00\x19\x00\x23\x00\x12\x00\x13\x00\x16\x00\x15\x00\x1a\x00\x15\x00\x0f\x00\x1b\x00\x1f\x00\x1f\x00\x13\x00\x21\x00\x22\x00\x23\x00\x24\x00\x18\x00\x0f\x00\x23\x00\x0f\x00\x06\x00\x1d\x00\x08\x00\x1f\x00\x0a\x00\x0b\x00\x0c\x00\x12\x00\x13\x00\x0f\x00\x15\x00\x1b\x00\x0f\x00\x1f\x00\x0f\x00\x1f\x00\x19\x00\x0f\x00\x07\x00\x08\x00\x1a\x00\x13\x00\x0f\x00\x19\x00\x23\x00\x1f\x00\x18\x00\x20\x00\x1f\x00\x19\x00\x1f\x00\x1d\x00\x19\x00\x23\x00\x1b\x00\x19\x00\x1a\x00\x19\x00\x1a\x00\x19\x00\x1a\x00\x19\x00\x1a\x00\x19\x00\x12\x00\x23\x00\x11\x00\x23\x00\x0f\x00\x23\x00\x1e\x00\x23\x00\x09\x00\x23\x00\x21\x00\x22\x00\x23\x00\x24\x00\x16\x00\x17\x00\x18\x00\x16\x00\x17\x00\x18\x00\x16\x00\x17\x00\x18\x00\x10\x00\x1f\x00\x20\x00\x02\x00\x03\x00\x10\x00\x10\x00\x10\x00\x10\x00\x16\x00\x10\x00\x10\x00\x1b\x00\x19\x00\x20\x00\x07\x00\x17\x00\x1f\x00\x10\x00\x16\x00\x16\x00\x16\x00\x16\x00\x10\x00\xff\xff\x0f\x00\x1e\x00\x15\x00\x19\x00\x10\x00\x12\x00\xff\xff\x1c\x00\xff\xff\x10\x00\xff\xff\x15\x00\x15\x00\x12\x00\xff\xff\x23\x00\x1e\x00\x14\x00\x11\x00\x23\x00\x1c\x00\x20\x00\x16\x00\x19\x00\x1e\x00\x14\x00\x1e\x00\x14\x00\x11\x00\x01\x00\x16\x00\x20\x00\x1c\x00\x1f\x00\x15\x00\x20\x00\x1e\x00\x15\x00\xff\xff\xff\xff\x1f\x00\xff\xff\x20\x00\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x52\x00\x5f\x00\x53\x00\x8e\x00\x4e\x00\x2a\x00\x2b\x00\x52\x00\x7c\x00\x53\x00\x11\x00\x30\x00\x2a\x00\x2b\x00\x52\x00\x3f\x00\x53\x00\x61\x00\x2c\x00\xb4\x00\xa2\x00\x52\x00\x31\x00\x53\x00\x12\x00\x2c\x00\xad\x00\x54\x00\x55\x00\xcc\x00\x4f\x00\x40\x00\x57\x00\x58\x00\x54\x00\x55\x00\xc7\x00\x2e\x00\x32\x00\x57\x00\x58\x00\x54\x00\x55\x00\xca\x00\x2e\x00\x15\x00\x57\x00\x58\x00\x54\x00\x55\x00\xcb\x00\x69\x00\x33\x00\x57\x00\x58\x00\x52\x00\x21\x00\x53\x00\x4d\x00\x4e\x00\x2a\x00\x2b\x00\x52\x00\x16\x00\x53\x00\x19\x00\x12\x00\x2a\x00\x2b\x00\x52\x00\x3f\x00\x53\x00\x16\x00\x2c\x00\x90\x00\xb7\x00\x52\x00\xac\x00\x53\x00\x12\x00\x2c\x00\x95\x00\x54\x00\x55\x00\xa4\x00\x4f\x00\x40\x00\x57\x00\x58\x00\x54\x00\x55\x00\xab\x00\x2e\x00\xb9\x00\x57\x00\x58\x00\x54\x00\x55\x00\x97\x00\x2e\x00\xb9\x00\x57\x00\x58\x00\x54\x00\x55\x00\x73\x00\x32\x00\x33\x00\x57\x00\x58\x00\x52\x00\x65\x00\x53\x00\x52\x00\x9f\x00\x53\x00\x3a\x00\x52\x00\x8d\x00\x53\x00\x52\x00\x12\x00\x53\x00\xc9\x00\xbb\x00\xbc\x00\x91\x00\x92\x00\x3c\x00\x72\x00\xba\x00\xbb\x00\xbc\x00\xa2\x00\x65\x00\x34\x00\xa3\x00\x54\x00\x55\x00\x56\x00\xaf\x00\x73\x00\x57\x00\x58\x00\x7a\x00\x57\x00\x58\x00\x7d\x00\x3d\x00\x57\x00\x58\x00\x7f\x00\x57\x00\x58\x00\x2a\x00\x2b\x00\x2a\x00\x2b\x00\x2a\x00\x2b\x00\x2a\x00\x2b\x00\x2a\x00\x2b\x00\x66\x00\x48\x00\x66\x00\x2c\x00\x84\x00\x2c\x00\x89\x00\x2c\x00\x84\x00\x2c\x00\x6b\x00\x2c\x00\x51\x00\x23\x00\x85\x00\xae\x00\x85\x00\x86\x00\x85\x00\x86\x00\x2a\x00\x2b\x00\x2e\x00\x21\x00\x2e\x00\x25\x00\x2e\x00\x26\x00\x2e\x00\x3a\x00\x2e\x00\x2a\x00\x2b\x00\x2c\x00\x61\x00\x2a\x00\x2b\x00\x2a\x00\x2b\x00\x16\x00\x62\x00\x3c\x00\x3a\x00\x3a\x00\x2c\x00\x38\x00\x3a\x00\x34\x00\x2c\x00\x46\x00\x2c\x00\x2d\x00\x2e\x00\x3b\x00\x3c\x00\xa6\x00\x3a\x00\x42\x00\x3c\x00\x3a\x00\x3a\x00\x3d\x00\x0b\x00\x2e\x00\x3a\x00\x3a\x00\x0d\x00\x2e\x00\xa7\x00\x2e\x00\x43\x00\x3c\x00\xa8\x00\x3a\x00\x3d\x00\x3d\x00\xa9\x00\x93\x00\x3d\x00\x25\x00\x65\x00\x3a\x00\x14\x00\x5f\x00\x04\x00\x96\x00\x0e\x00\x08\x00\x3d\x00\x16\x00\x15\x00\x3d\x00\x3d\x00\x98\x00\xbe\x00\xbf\x00\x3d\x00\x3d\x00\x5a\x00\x61\x00\x5b\x00\x03\x00\x5c\x00\x5d\x00\x5e\x00\x3d\x00\x3f\x00\x5f\x00\x16\x00\x30\x00\x66\x00\xc0\x00\xc9\x00\x3d\x00\x29\x00\x10\x00\x81\x00\x11\x00\x60\x00\xc1\x00\x88\x00\x82\x00\x40\x00\x61\x00\x4b\x00\x75\x00\x76\x00\x77\x00\x78\x00\x4c\x00\x21\x00\x12\x00\x5f\x00\x5a\x00\x4d\x00\x5b\x00\x32\x00\x5c\x00\x5d\x00\x5e\x00\x0f\x00\x10\x00\x5f\x00\x11\x00\x7f\x00\x3f\x00\x16\x00\x21\x00\x61\x00\xc2\x00\x4a\x00\x07\x00\x08\x00\x60\x00\x4b\x00\x72\x00\x8f\x00\x12\x00\x61\x00\x4c\x00\x18\x00\x40\x00\xc3\x00\x16\x00\x4d\x00\xc7\x00\x1f\x00\x73\x00\x1d\x00\x39\x00\x1d\x00\x28\x00\x1d\x00\x1e\x00\x1d\x00\x22\x00\x9e\x00\xc5\x00\x1f\x00\xb9\x00\x1f\x00\x72\x00\x1f\x00\xc4\x00\x1f\x00\xb7\x00\x1f\x00\x75\x00\x76\x00\x77\x00\x78\x00\x6e\x00\xc5\x00\x70\x00\x6e\x00\xa1\x00\x70\x00\x6e\x00\x6f\x00\x70\x00\xb6\x00\x68\x00\x69\x00\x0a\x00\x0b\x00\xb1\x00\xb2\x00\xb3\x00\xb4\x00\xa6\x00\xab\x00\x8b\x00\x82\x00\x8c\x00\x51\x00\x95\x00\x8d\x00\x16\x00\x9e\x00\x9a\x00\x9b\x00\x9c\x00\x9d\x00\x6d\x00\x00\x00\x7c\x00\x8e\x00\x6e\x00\xa1\x00\x89\x00\x84\x00\x00\x00\x6b\x00\x00\x00\x65\x00\x00\x00\x22\x00\x38\x00\x37\x00\x00\x00\x79\x00\x83\x00\x41\x00\x45\x00\x7a\x00\x64\x00\x51\x00\x46\x00\x42\x00\x36\x00\x25\x00\x48\x00\x28\x00\x1b\x00\x03\x00\x1c\x00\x18\x00\x19\x00\x16\x00\x22\x00\x18\x00\x1d\x00\x07\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x18\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (1, 87) [
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
	(87 , happyReduce_87)
	]

happy_n_terms = 38 :: Int
happy_n_nonterms = 38 :: Int

happyReduce_1 = happyReduce 4# 0# happyReduction_1
happyReduction_1 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut5 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (HsExtCore happy_var_2 [] []
	) `HappyStk` happyRest}

happyReduce_2 = happySpecReduce_3  1# happyReduction_2
happyReduction_2 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn5
		 (undefined
	)

happyReduce_3 = happySpecReduce_3  2# happyReduction_3
happyReduction_3 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn6
		 (undefined
	)

happyReduce_4 = happySpecReduce_3  3# happyReduction_4
happyReduction_4 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn7
		 (undefined
	)

happyReduce_5 = happySpecReduce_3  4# happyReduction_5
happyReduction_5 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn8
		 (undefined
	)

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TKcname happy_var_1) -> 
	happyIn9
		 ([happy_var_1]
	)}

happyReduce_7 = happySpecReduce_3  5# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TKcname happy_var_1) -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 (happy_var_1:happy_var_3
	)}}

happyReduce_8 = happySpecReduce_1  6# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 ([happy_var_1]
	)}

happyReduce_9 = happySpecReduce_3  6# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TKcname happy_var_1) -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 (happy_var_1:happy_var_3
	)}}

happyReduce_10 = happySpecReduce_0  7# happyReduction_10
happyReduction_10  =  happyIn11
		 ([]
	)

happyReduce_11 = happySpecReduce_2  7# happyReduction_11
happyReduction_11 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 (happy_var_1:happy_var_2
	)}}

happyReduce_12 = happyReduce 8# 8# happyReduction_12
happyReduction_12 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	case happyOut14 happy_x_6 of { happy_var_6 -> 
	happyIn12
		 (mkTyData DataType ( noLoc []
				    , noLoc (ifaceExtRdrName happy_var_2)
				    , map toHsTvBndr happy_var_3
				    , Nothing
				    ) Nothing happy_var_6 Nothing
	) `HappyStk` happyRest}}}

happyReduce_13 = happyReduce 5# 8# happyReduction_13
happyReduction_13 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	happyIn12
		 (let tc_rdr = ifaceExtRdrName happy_var_2 in
                  mkTyData NewType ( noLoc []
				   , noLoc tc_rdr
				   , map toHsTvBndr happy_var_3
				   , Nothing
				   ) Nothing (happy_var_4 (rdrNameOcc tc_rdr)) Nothing
	) `HappyStk` happyRest}}}

happyReduce_14 = happySpecReduce_0  9# happyReduction_14
happyReduction_14  =  happyIn13
		 ((\ tc_occ -> [])
	)

happyReduce_15 = happySpecReduce_2  9# happyReduction_15
happyReduction_15 happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 ((\ tc_occ -> let { dc_name  = mkRdrUnqual (setOccNameSpace dataName tc_occ) ;
			                     con_info = PrefixCon [toHsType happy_var_2] }
			                in [noLoc $ ConDecl (noLoc dc_name) Explicit []
					   (noLoc []) con_info ResTyH98 Nothing])
	)}

happyReduce_16 = happySpecReduce_0  10# happyReduction_16
happyReduction_16  =  happyIn14
		 ([]
	)

happyReduce_17 = happySpecReduce_1  10# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 ([happy_var_1]
	)}

happyReduce_18 = happySpecReduce_3  10# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (happy_var_1:happy_var_3
	)}}

happyReduce_19 = happySpecReduce_3  11# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (noLoc $ ConDecl (noLoc (mkRdrUnqual happy_var_1)) Explicit happy_var_2 (noLoc []) (PrefixCon happy_var_3) ResTyH98 Nothing
	)}}}

happyReduce_20 = happySpecReduce_3  11# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (noLoc $ ConDecl (noLoc (mkRdrUnqual happy_var_1)) Explicit [] (noLoc []) (PrefixCon []) (undefined happy_var_3) Nothing
	)}}

happyReduce_21 = happySpecReduce_0  12# happyReduction_21
happyReduction_21  =  happyIn16
		 ([]
	)

happyReduce_22 = happySpecReduce_3  12# happyReduction_22
happyReduction_22 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (toHsTvBndr happy_var_2 : happy_var_3
	)}}

happyReduce_23 = happySpecReduce_1  13# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (map toHsType happy_var_1
	)}

happyReduce_24 = happySpecReduce_0  14# happyReduction_24
happyReduction_24  =  happyIn18
		 ([]
	)

happyReduce_25 = happySpecReduce_2  14# happyReduction_25
happyReduction_25 happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (happy_var_1:happy_var_2
	)}}

happyReduce_26 = happySpecReduce_1  15# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (IfaceTyVar happy_var_1
	)}

happyReduce_27 = happySpecReduce_1  15# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (IfaceTyConApp (IfaceTc happy_var_1) []
	)}

happyReduce_28 = happySpecReduce_3  15# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (happy_var_2
	)}

happyReduce_29 = happySpecReduce_2  16# happyReduction_29
happyReduction_29 happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (foldl IfaceAppTy (IfaceTyVar happy_var_1) happy_var_2
	)}}

happyReduce_30 = happySpecReduce_2  16# happyReduction_30
happyReduction_30 happy_x_2
	happy_x_1
	 =  happyIn20
		 (undefined
	)

happyReduce_31 = happySpecReduce_2  16# happyReduction_31
happyReduction_31 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (IfaceTyConApp (IfaceTc happy_var_1) happy_var_2
	)}}

happyReduce_32 = happySpecReduce_3  16# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (happy_var_2
	)}

happyReduce_33 = happySpecReduce_1  17# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (happy_var_1
	)}

happyReduce_34 = happySpecReduce_3  17# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (IfaceFunTy happy_var_1 happy_var_3
	)}}

happyReduce_35 = happyReduce 4# 17# happyReduction_35
happyReduction_35 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut30 happy_x_2 of { happy_var_2 -> 
	case happyOut21 happy_x_4 of { happy_var_4 -> 
	happyIn21
		 (foldr IfaceForAllTy happy_var_4 happy_var_2
	) `HappyStk` happyRest}}

happyReduce_36 = happySpecReduce_0  18# happyReduction_36
happyReduction_36  =  happyIn22
		 ([]
	)

happyReduce_37 = happySpecReduce_3  18# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_38 = happyReduce 4# 19# happyReduction_38
happyReduction_38 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (IfaceRec happy_var_3
	) `HappyStk` happyRest}

happyReduce_39 = happySpecReduce_1  19# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (let (b,r) = happy_var_1
				  in IfaceNonRec b r
	)}

happyReduce_40 = happySpecReduce_1  20# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ([happy_var_1]
	)}

happyReduce_41 = happySpecReduce_3  20# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (happy_var_1:happy_var_3
	)}}

happyReduce_42 = happyReduce 5# 21# happyReduction_42
happyReduction_42 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	case happyOut35 happy_x_5 of { happy_var_5 -> 
	happyIn25
		 ((IfLetBndr happy_var_1 happy_var_3 NoInfo, happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_43 = happySpecReduce_2  21# happyReduction_43
happyReduction_43 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (happy_var_2
	)}

happyReduce_44 = happySpecReduce_2  22# happyReduction_44
happyReduction_44 happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_2 of { happy_var_2 -> 
	happyIn26
		 (IfaceTvBndr happy_var_2
	)}

happyReduce_45 = happySpecReduce_1  22# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (IfaceIdBndr happy_var_1
	)}

happyReduce_46 = happySpecReduce_1  23# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 ([happy_var_1]
	)}

happyReduce_47 = happySpecReduce_2  23# happyReduction_47
happyReduction_47 happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_2 of { happy_var_2 -> 
	happyIn27
		 (happy_var_1:happy_var_2
	)}}

happyReduce_48 = happyReduce 5# 24# happyReduction_48
happyReduction_48 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_2 of { happy_var_2 -> 
	case happyOut21 happy_x_4 of { happy_var_4 -> 
	happyIn28
		 ((happy_var_2,happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_49 = happySpecReduce_1  25# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 ((happy_var_1, ifaceLiftedTypeKind)
	)}

happyReduce_50 = happyReduce 5# 25# happyReduction_50
happyReduction_50 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	happyIn29
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_51 = happySpecReduce_0  26# happyReduction_51
happyReduction_51  =  happyIn30
		 ([]
	)

happyReduce_52 = happySpecReduce_2  26# happyReduction_52
happyReduction_52 happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn30
		 (happy_var_1:happy_var_2
	)}}

happyReduce_53 = happySpecReduce_1  27# happyReduction_53
happyReduction_53 happy_x_1
	 =  happyIn31
		 (ifaceLiftedTypeKind
	)

happyReduce_54 = happySpecReduce_1  27# happyReduction_54
happyReduction_54 happy_x_1
	 =  happyIn31
		 (ifaceUnliftedTypeKind
	)

happyReduce_55 = happySpecReduce_1  27# happyReduction_55
happyReduction_55 happy_x_1
	 =  happyIn31
		 (ifaceOpenTypeKind
	)

happyReduce_56 = happySpecReduce_3  27# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (happy_var_2
	)}

happyReduce_57 = happySpecReduce_1  28# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (happy_var_1
	)}

happyReduce_58 = happySpecReduce_3  28# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (ifaceArrow happy_var_1 happy_var_3
	)}}

happyReduce_59 = happySpecReduce_3  28# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (ifaceEq happy_var_1 happy_var_3
	)}}

happyReduce_60 = happySpecReduce_1  29# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (IfaceLcl happy_var_1
	)}

happyReduce_61 = happySpecReduce_1  29# happyReduction_61
happyReduction_61 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (IfaceExt happy_var_1
	)}

happyReduce_62 = happySpecReduce_1  29# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (IfaceExt happy_var_1
	)}

happyReduce_63 = happySpecReduce_1  29# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (IfaceLit happy_var_1
	)}

happyReduce_64 = happySpecReduce_3  29# happyReduction_64
happyReduction_64 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn33
		 (happy_var_2
	)}

happyReduce_65 = happySpecReduce_2  30# happyReduction_65
happyReduction_65 happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	happyIn34
		 (IfaceApp happy_var_1 happy_var_2
	)}}

happyReduce_66 = happySpecReduce_3  30# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 (IfaceApp happy_var_1 (IfaceType happy_var_3)
	)}}

happyReduce_67 = happySpecReduce_1  30# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (happy_var_1
	)}

happyReduce_68 = happySpecReduce_1  31# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (happy_var_1
	)}

happyReduce_69 = happyReduce 4# 31# happyReduction_69
happyReduction_69 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut27 happy_x_2 of { happy_var_2 -> 
	case happyOut35 happy_x_4 of { happy_var_4 -> 
	happyIn35
		 (foldr IfaceLam happy_var_4 happy_var_2
	) `HappyStk` happyRest}}

happyReduce_70 = happyReduce 4# 31# happyReduction_70
happyReduction_70 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut35 happy_x_4 of { happy_var_4 -> 
	happyIn35
		 (IfaceLet happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_71 = happyReduce 10# 31# happyReduction_71
happyReduction_71 (happy_x_10 `HappyStk`
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
	 = case happyOut21 happy_x_3 of { happy_var_3 -> 
	case happyOut33 happy_x_5 of { happy_var_5 -> 
	case happyOut28 happy_x_7 of { happy_var_7 -> 
	case happyOut36 happy_x_9 of { happy_var_9 -> 
	happyIn35
		 (IfaceCase happy_var_5 (fst happy_var_7) happy_var_3 happy_var_9
	) `HappyStk` happyRest}}}}

happyReduce_72 = happySpecReduce_3  31# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_2 of { happy_var_2 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (IfaceCast happy_var_2 happy_var_3
	)}}

happyReduce_73 = happySpecReduce_3  31# happyReduction_73
happyReduction_73 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TKstring happy_var_2) -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (case happy_var_2 of
	       --"SCC"      -> IfaceNote (IfaceSCC "scc") happy_var_3
	       "InlineMe"   -> IfaceNote IfaceInlineMe happy_var_3
	)}}

happyReduce_74 = happySpecReduce_3  31# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TKstring happy_var_2) -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (IfaceFCall (ForeignCall.CCall 
                                                    (CCallSpec (StaticTarget (mkFastString happy_var_2)) 
                                                               CCallConv (PlaySafe False))) 
                                                 happy_var_3
	)}}

happyReduce_75 = happySpecReduce_1  32# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ([happy_var_1]
	)}

happyReduce_76 = happySpecReduce_3  32# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (happy_var_1:happy_var_3
	)}}

happyReduce_77 = happyReduce 4# 33# happyReduction_77
happyReduction_77 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_2 of { happy_var_2 -> 
	case happyOut35 happy_x_4 of { happy_var_4 -> 
	happyIn37
		 ((IfaceDataAlt happy_var_1, map ifaceBndrName happy_var_2, happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_78 = happySpecReduce_3  33# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 ((IfaceDataAlt happy_var_1, [], happy_var_3)
	)}}

happyReduce_79 = happySpecReduce_3  33# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 ((IfaceLitAlt happy_var_1, [], happy_var_3)
	)}}

happyReduce_80 = happySpecReduce_3  33# happyReduction_80
happyReduction_80 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 ((IfaceDefault, [], happy_var_3)
	)}

happyReduce_81 = happyReduce 5# 34# happyReduction_81
happyReduction_81 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TKinteger happy_var_2) -> 
	case happyOut19 happy_x_4 of { happy_var_4 -> 
	happyIn38
		 (convIntLit happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_82 = happyReduce 5# 34# happyReduction_82
happyReduction_82 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TKrational happy_var_2) -> 
	case happyOut19 happy_x_4 of { happy_var_4 -> 
	happyIn38
		 (convRatLit happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_83 = happyReduce 5# 34# happyReduction_83
happyReduction_83 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TKchar happy_var_2) -> 
	happyIn38
		 (MachChar happy_var_2
	) `HappyStk` happyRest}

happyReduce_84 = happyReduce 5# 34# happyReduction_84
happyReduction_84 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TKstring happy_var_2) -> 
	happyIn38
		 (MachStr (mkFastString happy_var_2)
	) `HappyStk` happyRest}

happyReduce_85 = happySpecReduce_1  35# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TKname happy_var_1) -> 
	happyIn39
		 (mkFastString happy_var_1
	)}

happyReduce_86 = happySpecReduce_1  36# happyReduction_86
happyReduction_86 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TKname happy_var_1) -> 
	happyIn40
		 (happy_var_1
	)}

happyReduce_87 = happySpecReduce_1  37# happyReduction_87
happyReduction_87 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TKcname happy_var_1) -> 
	happyIn41
		 (mkOccName dataName happy_var_1
	)}

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	TKEOF -> happyDoAction 37# tk action sts stk;
	TKmodule -> cont 1#;
	TKdata -> cont 2#;
	TKnewtype -> cont 3#;
	TKforall -> cont 4#;
	TKrec -> cont 5#;
	TKlet -> cont 6#;
	TKin -> cont 7#;
	TKcase -> cont 8#;
	TKof -> cont 9#;
	TKcast -> cont 10#;
	TKnote -> cont 11#;
	TKexternal -> cont 12#;
	TKlocal -> cont 13#;
	TKwild -> cont 14#;
	TKoparen -> cont 15#;
	TKcparen -> cont 16#;
	TKobrace -> cont 17#;
	TKcbrace -> cont 18#;
	TKhash -> cont 19#;
	TKeq -> cont 20#;
	TKcolon -> cont 21#;
	TKcoloncolon -> cont 22#;
	TKcoloneqcolon -> cont 23#;
	TKstar -> cont 24#;
	TKrarrow -> cont 25#;
	TKlambda -> cont 26#;
	TKat -> cont 27#;
	TKdot -> cont 28#;
	TKquestion -> cont 29#;
	TKsemicolon -> cont 30#;
	TKname happy_dollar_dollar -> cont 31#;
	TKcname happy_dollar_dollar -> cont 32#;
	TKinteger happy_dollar_dollar -> cont 33#;
	TKrational happy_dollar_dollar -> cont 34#;
	TKstring happy_dollar_dollar -> cont 35#;
	TKchar happy_dollar_dollar -> cont 36#;
	_ -> happyError' tk
	})

happyError_ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => Token -> P a
happyError' tk = (\token -> happyError) tk

parseCore = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq


ifaceKind kc = IfaceTyConApp kc []

ifaceBndrName (IfaceIdBndr (n,_)) = n
ifaceBndrName (IfaceTvBndr (n,_)) = n

convIntLit :: Integer -> IfaceType -> Literal
convIntLit i (IfaceTyConApp tc [])
  | tc `eqTc` intPrimTyCon  = MachInt  i  
  | tc `eqTc` wordPrimTyCon = MachWord i
  | tc `eqTc` charPrimTyCon = MachChar (chr (fromInteger i))
  | tc `eqTc` addrPrimTyCon && i == 0 = MachNullAddr
convIntLit i aty
  = pprPanic "Unknown integer literal type" (ppr aty)

convRatLit :: Rational -> IfaceType -> Literal
convRatLit r (IfaceTyConApp tc [])
  | tc `eqTc` floatPrimTyCon  = MachFloat  r
  | tc `eqTc` doublePrimTyCon = MachDouble r
convRatLit i aty
  = pprPanic "Unknown rational literal type" (ppr aty)

eqTc :: IfaceTyCon -> TyCon -> Bool   -- Ugh!
eqTc (IfaceTc name) tycon = name == tyConName tycon

-- Tiresomely, we have to generate both HsTypes (in type/class decls) 
-- and IfaceTypes (in Core expressions).  So we parse them as IfaceTypes,
-- and convert to HsTypes here.  But the IfaceTypes we can see here
-- are very limited (see the productions for 'ty', so the translation
-- isn't hard
toHsType :: IfaceType -> LHsType RdrName
toHsType (IfaceTyVar v)        		 = noLoc $ HsTyVar (mkRdrUnqual (mkTyVarOcc v))
toHsType (IfaceAppTy t1 t2)    		 = noLoc $ HsAppTy (toHsType t1) (toHsType t2)
toHsType (IfaceFunTy t1 t2)    		 = noLoc $ HsFunTy (toHsType t1) (toHsType t2)
toHsType (IfaceTyConApp (IfaceTc tc) ts) = foldl mkHsAppTy (noLoc $ HsTyVar (ifaceExtRdrName tc)) (map toHsType ts) 
toHsType (IfaceForAllTy tv t)            = add_forall (toHsTvBndr tv) (toHsType t)

-- We also need to convert IfaceKinds to Kinds (now that they are different).
-- Only a limited form of kind will be encountered... hopefully
toKind :: IfaceKind -> Kind
toKind (IfaceFunTy ifK1 ifK2)  = mkArrowKind (toKind ifK1) (toKind ifK2)
toKind (IfaceTyConApp ifKc []) = mkTyConApp (toKindTc ifKc) []
toKind other                   = pprPanic "toKind" (ppr other)

toKindTc :: IfaceTyCon -> TyCon
toKindTc IfaceLiftedTypeKindTc   = liftedTypeKindTyCon
toKindTc IfaceOpenTypeKindTc     = openTypeKindTyCon
toKindTc IfaceUnliftedTypeKindTc = unliftedTypeKindTyCon
toKindTc IfaceUbxTupleKindTc     = ubxTupleKindTyCon
toKindTc IfaceArgTypeKindTc      = argTypeKindTyCon
toKindTc other                   = pprPanic "toKindTc" (ppr other)

ifaceTcType ifTc = IfaceTyConApp ifTc []

ifaceLiftedTypeKind   = ifaceTcType IfaceLiftedTypeKindTc
ifaceOpenTypeKind     = ifaceTcType IfaceOpenTypeKindTc
ifaceUnliftedTypeKind = ifaceTcType IfaceUnliftedTypeKindTc

ifaceArrow ifT1 ifT2 = IfaceFunTy ifT1 ifT2

ifaceEq ifT1 ifT2 = IfacePredTy (IfaceEqPred ifT1 ifT2)

toHsTvBndr :: IfaceTvBndr -> LHsTyVarBndr RdrName
toHsTvBndr (tv,k) = noLoc $ KindedTyVar (mkRdrUnqual (mkTyVarOcc tv)) (toKind k)

ifaceExtRdrName :: Name -> RdrName
ifaceExtRdrName name = mkOrig (nameModule name) (nameOccName name)
ifaceExtRdrName other = pprPanic "ParserCore.ifaceExtRdrName" (ppr other)

add_forall tv (L _ (HsForAllTy exp tvs cxt t))
  = noLoc $ HsForAllTy exp (tv:tvs) cxt t
add_forall tv t
  = noLoc $ HsForAllTy Explicit [tv] (noLoc []) t
  
happyError :: P a 
happyError s l = failP (show l ++ ": Parse error\n") (take 100 s) l
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
