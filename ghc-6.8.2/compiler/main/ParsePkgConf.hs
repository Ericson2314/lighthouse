{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module ParsePkgConf( loadPackageConfig ) where

#include "HsVersions.h"

import PackageConfig
import Lexer
import DynFlags
import FastString
import StringBuffer
import ErrUtils  ( mkLocMessage )
import SrcLoc
import Outputable
import Panic     ( GhcException(..) )
import Control.Exception ( throwDyn )
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
happyIn4 :: ([ PackageConfig ]) -> (HappyAbsSyn )
happyIn4 x = unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> ([ PackageConfig ])
happyOut4 x = unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ([ PackageConfig ]) -> (HappyAbsSyn )
happyIn5 x = unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ([ PackageConfig ])
happyOut5 x = unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (PackageConfig) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (PackageConfig)
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (PackageConfig -> PackageConfig) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (PackageConfig -> PackageConfig)
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (PackageConfig -> PackageConfig) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (PackageConfig -> PackageConfig)
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (PackageIdentifier) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (PackageIdentifier)
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Version) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (Version)
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([PackageIdentifier]) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ([PackageIdentifier])
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ([PackageIdentifier]) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> ([PackageIdentifier])
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ([Int]) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ([Int])
happyOut13 x = unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([Int]) -> (HappyAbsSyn )
happyIn14 x = unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([Int])
happyOut14 x = unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ([String]) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ([String])
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ([String]) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ([String])
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyInTok :: Located Token -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> Located Token
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x4c\x00\x48\x00\x4a\x00\x45\x00\x0a\x00\x1c\x00\x00\x00\x00\x00\x47\x00\x46\x00\x00\x00\x44\x00\x00\x00\x16\x00\x00\x00\x3f\x00\x03\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x01\x00\x00\x00\x42\x00\x00\x00\x41\x00\x40\x00\x3e\x00\x00\x00\x36\x00\x3c\x00\x3a\x00\x00\x00\x00\x00\x38\x00\x39\x00\x35\x00\x00\x00\x00\x00\x37\x00\x34\x00\x33\x00\x32\x00\x31\x00\x30\x00\x2f\x00\x00\x00\x2e\x00\x2b\x00\x2d\x00\x09\x00\x2c\x00\x00\x00\x2a\x00\x26\x00\x25\x00\x20\x00\x00\x00\x00\x00\x29\x00\x27\x00\x0d\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x28\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\x00\x00\x21\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x04\x00\x00\x00\xfb\xff\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xfe\xff\x00\x00\x00\x00\xfd\xff\x00\x00\xfb\xff\x00\x00\xf9\xff\x00\x00\x00\x00\xfa\xff\x00\x00\xf8\xff\xf7\xff\xf2\xff\xf3\xff\x00\x00\xf5\xff\xf6\xff\x00\x00\xf4\xff\xee\xff\x00\x00\x00\x00\xe8\xff\x00\x00\xe6\xff\x00\x00\xe7\xff\xef\xff\x00\x00\x00\x00\x00\x00\xed\xff\xe5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xec\xff\xea\xff\x00\x00\x00\x00\x00\x00\xeb\xff\xe9\xff\x00\x00\x00\x00\x00\x00\xf0\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x05\x00\x01\x00\x05\x00\x08\x00\x07\x00\x03\x00\x0c\x00\x0c\x00\x0b\x00\x09\x00\x08\x00\x09\x00\x04\x00\x04\x00\x0b\x00\x04\x00\x04\x00\x08\x00\x0a\x00\x08\x00\x09\x00\x09\x00\x05\x00\x02\x00\x0a\x00\x08\x00\x05\x00\x03\x00\x04\x00\x01\x00\x02\x00\x04\x00\x05\x00\x0a\x00\x09\x00\x06\x00\x04\x00\x02\x00\x0c\x00\x00\x00\x02\x00\x0a\x00\x06\x00\x03\x00\x07\x00\x03\x00\x05\x00\x04\x00\x01\x00\x05\x00\x02\x00\x06\x00\xff\xff\x07\x00\x01\x00\xff\xff\x06\x00\x08\x00\x07\x00\x05\x00\xff\xff\x09\x00\x06\x00\x08\x00\x05\x00\x04\x00\x09\x00\x04\x00\x06\x00\x05\x00\xff\xff\x01\x00\x07\x00\x07\x00\x03\x00\x08\x00\x07\x00\x04\x00\x03\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x1c\x00\x1b\x00\x14\x00\x1d\x00\x15\x00\x18\x00\x1e\x00\x1e\x00\x16\x00\x1c\x00\x19\x00\x1a\x00\x38\x00\x08\x00\x3f\x00\x20\x00\x20\x00\x09\x00\x39\x00\x21\x00\x22\x00\x22\x00\x1c\x00\x12\x00\x3d\x00\x28\x00\x13\x00\x0d\x00\x0e\x00\x05\x00\x06\x00\x0b\x00\x0c\x00\x36\x00\x34\x00\x2e\x00\x13\x00\x0c\x00\x29\x00\x03\x00\x42\x00\x39\x00\x3f\x00\x41\x00\x3b\x00\x36\x00\x3c\x00\x3d\x00\x31\x00\x3a\x00\x32\x00\x34\x00\x00\x00\x33\x00\x1b\x00\x00\x00\x2e\x00\x30\x00\x2d\x00\x2c\x00\x00\x00\x2b\x00\x28\x00\x21\x00\x23\x00\x24\x00\x22\x00\x25\x00\x11\x00\x26\x00\x00\x00\x0a\x00\x27\x00\x10\x00\x03\x00\x09\x00\x10\x00\x08\x00\x05\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (1, 26) [
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
	(26 , happyReduce_26)
	]

happy_n_terms = 12 :: Int
happy_n_nonterms = 13 :: Int

happyReduce_1 = happySpecReduce_2  0# happyReduction_1
happyReduction_1 happy_x_2
	happy_x_1
	 =  happyIn4
		 ([]
	)

happyReduce_2 = happySpecReduce_3  0# happyReduction_2
happyReduction_2 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (reverse happy_var_2
	)}

happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ([ happy_var_1 ]
	)}

happyReduce_4 = happySpecReduce_3  1# happyReduction_4
happyReduction_4 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut6 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_5 = happyReduce 4# 2# happyReduction_5
happyReduction_5 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn6
		 (happy_var_3 defaultPackageConfig
	) `HappyStk` happyRest}

happyReduce_6 = happySpecReduce_1  3# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn7
		 (\p -> happy_var_1 p
	)}

happyReduce_7 = happySpecReduce_3  3# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (\p -> happy_var_1 (happy_var_3 p)
	)}}

happyReduce_8 = happyMonadReduce 3# 4# happyReduction_8
happyReduction_8 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { (L _ (ITvarid    happy_var_1)) -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	( case unpackFS happy_var_1 of
		        "package"     -> return (\p -> p{package = happy_var_3})
			_other        -> happyError)}}
	) (\r -> happyReturn (happyIn8 r))

happyReduce_9 = happySpecReduce_3  4# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn8
		 (id
	)

happyReduce_10 = happyMonadReduce 3# 4# happyReduction_10
happyReduction_10 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { (L _ (ITvarid    happy_var_1)) -> 
	case happyOutTok happy_x_3 of { (L _ (ITconid    happy_var_3)) -> 
	( case unpackFS happy_var_1 of {
		   	"exposed" -> 
			   case unpackFS happy_var_3 of {
				"True"  -> return (\p -> p{exposed=True});
				"False" -> return (\p -> p{exposed=False});
				_       -> happyError };
		   	"license" -> return id; -- not interested
		   	_         -> happyError })}}
	) (\r -> happyReturn (happyIn8 r))

happyReduce_11 = happyReduce 4# 4# happyReduction_11
happyReduction_11 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyIn8
		 (id
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  4# happyReduction_12
happyReduction_12 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (ITvarid    happy_var_1)) -> 
	case happyOut15 happy_x_3 of { happy_var_3 -> 
	happyIn8
		 (\p -> case unpackFS happy_var_1 of
		        "exposedModules"    -> p{exposedModules    = happy_var_3}
		        "hiddenModules"     -> p{hiddenModules     = happy_var_3}
		        "importDirs"        -> p{importDirs        = happy_var_3}
		        "libraryDirs"       -> p{libraryDirs       = happy_var_3}
		        "hsLibraries"       -> p{hsLibraries       = happy_var_3}
		        "extraLibraries"    -> p{extraLibraries    = happy_var_3}
		        "extraGHCiLibraries"-> p{extraGHCiLibraries= happy_var_3}
		        "includeDirs"       -> p{includeDirs       = happy_var_3}
		        "includes"          -> p{includes          = happy_var_3}
		        "hugsOptions"       -> p{hugsOptions       = happy_var_3}
		        "ccOptions"         -> p{ccOptions         = happy_var_3}
		        "ldOptions"         -> p{ldOptions         = happy_var_3}
		        "frameworkDirs"     -> p{frameworkDirs     = happy_var_3}
		        "frameworks"        -> p{frameworks        = happy_var_3}
		        "haddockInterfaces" -> p{haddockInterfaces = happy_var_3}
		        "haddockHTMLs"      -> p{haddockHTMLs      = happy_var_3}
		        "depends"     	    -> p{depends = []}
				-- empty list only, non-empty handled below
			other -> p
	)}}

happyReduce_13 = happyMonadReduce 3# 4# happyReduction_13
happyReduction_13 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { (L _ (ITvarid    happy_var_1)) -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	( case unpackFS happy_var_1 of
		        "depends"     -> return (\p -> p{depends = happy_var_3})
			_other        -> happyError)}}
	) (\r -> happyReturn (happyIn8 r))

happyReduce_14 = happyReduce 10# 5# happyReduction_14
happyReduction_14 (happy_x_10 `HappyStk`
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
	 = case happyOutTok happy_x_5 of { (L _ (ITstring   happy_var_5)) -> 
	case happyOut10 happy_x_9 of { happy_var_9 -> 
	happyIn9
		 (PackageIdentifier{ pkgName = unpackFS happy_var_5, 
					     pkgVersion = happy_var_9 }
	) `HappyStk` happyRest}}

happyReduce_15 = happyReduce 10# 6# happyReduction_15
happyReduction_15 (happy_x_10 `HappyStk`
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
	 = case happyOut13 happy_x_5 of { happy_var_5 -> 
	case happyOut15 happy_x_9 of { happy_var_9 -> 
	happyIn10
		 (Version{ versionBranch=happy_var_5, versionTags=happy_var_9 }
	) `HappyStk` happyRest}}

happyReduce_16 = happySpecReduce_3  7# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 (happy_var_2
	)}

happyReduce_17 = happySpecReduce_1  8# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 ([ happy_var_1 ]
	)}

happyReduce_18 = happySpecReduce_3  8# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_19 = happySpecReduce_2  9# happyReduction_19
happyReduction_19 happy_x_2
	happy_x_1
	 =  happyIn13
		 ([]
	)

happyReduce_20 = happySpecReduce_3  9# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (happy_var_2
	)}

happyReduce_21 = happySpecReduce_1  10# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (ITinteger  happy_var_1)) -> 
	happyIn14
		 ([ fromIntegral happy_var_1 ]
	)}

happyReduce_22 = happySpecReduce_3  10# happyReduction_22
happyReduction_22 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (ITinteger  happy_var_1)) -> 
	case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (fromIntegral happy_var_1 : happy_var_3
	)}}

happyReduce_23 = happySpecReduce_2  11# happyReduction_23
happyReduction_23 happy_x_2
	happy_x_1
	 =  happyIn15
		 ([]
	)

happyReduce_24 = happySpecReduce_3  11# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (happy_var_2
	)}

happyReduce_25 = happySpecReduce_1  12# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (ITstring   happy_var_1)) -> 
	happyIn16
		 ([ unpackFS happy_var_1 ]
	)}

happyReduce_26 = happySpecReduce_3  12# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L _ (ITstring   happy_var_1)) -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (unpackFS happy_var_1 : happy_var_3
	)}}

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	L _ ITeof -> happyDoAction 11# tk action sts stk;
	L _ ITocurly -> cont 1#;
	L _ ITccurly -> cont 2#;
	L _ ITobrack -> cont 3#;
	L _ ITcbrack -> cont 4#;
	L _ ITcomma -> cont 5#;
	L _ ITequal -> cont 6#;
	L _ (ITvarid    happy_dollar_dollar) -> cont 7#;
	L _ (ITconid    happy_dollar_dollar) -> cont 8#;
	L _ (ITstring   happy_dollar_dollar) -> cont 9#;
	L _ (ITinteger  happy_dollar_dollar) -> cont 10#;
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
happyError' :: () => Located Token -> P a
happyError' tk = (\token -> happyError) tk

parse = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq


happyError :: P a
happyError = srcParseFail

loadPackageConfig :: FilePath -> IO [PackageConfig]
loadPackageConfig conf_filename = do
   buf <- hGetStringBuffer conf_filename
   let loc  = mkSrcLoc (mkFastString conf_filename) 1 0
   case unP parse (mkPState buf loc defaultDynFlags) of
	PFailed span err -> 
           throwDyn (InstallationError (showSDoc (mkLocMessage span err)))

	POk _ pkg_details -> do
	    return pkg_details
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
