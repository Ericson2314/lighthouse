{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex.Base.RegexLike
-- Copyright   :  (c) Chris Kuklewicz 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org, textregexlazy@personal.mightyreason.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTC+FD)
--
-- Classes and instances for Regex matching.
--
-- 
-- All the classes are declared here, and some common type aliases, and
-- the MatchResult data type.
-- 
-- The only instances here are for Extract String and Extract ByteString.
-- There are no data values.  The 'RegexContext' instances are in
-- "Text.Regex.Base.Context", except for ones which run afoul of a
-- repeated variable (RegexContext regex a a), which are defined in each
-- modules' String and ByteString modules.
-----------------------------------------------------------------------------

module Text.Regex.Base.RegexLike (
  -- ** Type aliases
  MatchOffset,
  MatchLength,
  MatchArray,
  MatchText,
  -- ** Data types
  MatchResult(..),
  -- ** Classes
  RegexOptions(..),
  RegexMaker(..),
  RegexLike(..),
  RegexContext(..),
  Extract(..),
  ) where

import Data.Array(Array,(!))
import Data.Maybe(isJust)
import Data.ByteString(ByteString)
import qualified Data.ByteString as B (take,drop,empty)

-- | 0 based index from start of source, or (-1) for unused
type MatchOffset = Int
-- | non-negative length of a match
type MatchLength = Int
-- | 0 based array, with 0th index indicating the full match.  If the
-- full match location is not available, represent as (0,0).
type MatchArray = Array Int (MatchOffset,MatchLength)
type MatchText source = Array Int (source,(MatchOffset,MatchLength))


-- | This is the same as the type from JRegex.
data MatchResult a = MR {
    mrBefore :: a,
    mrMatch  :: a,
    mrAfter  :: a,
    mrSubList :: [a],
    mrSubs   :: Array Int a
}

----------------
-- | Rather than carry them around spearately, the options for how to
-- execute a regex are kept as part of the regex.  There are two types
-- of options.  Those that can only be specified at compilation time
-- and never changed are CompOpt.  Those that can be changed later and
-- affect how matching is performed are ExecOpt.  The actually types
-- for these depend on the backend.
class RegexOptions regex compOpt execOpt 
  | regex->compOpt execOpt, compOpt->regex execOpt, execOpt->regex compOpt where
  blankCompOpt :: compOpt    -- ^ no options set at all in the backend
  blankExecOpt :: execOpt    -- ^ no options set at all in the backend
  defaultCompOpt :: compOpt  -- ^ reasonable options (extended,caseSensitive,multiline regex)
  defaultExecOpt :: execOpt  -- ^ reasonable options (extended,caseSensitive,multiline regex)
  setExecOpts :: execOpt -> regex -> regex
  -- ^ forget old flags and use new ones
  getExecOpts :: regex -> execOpt
  -- ^ retrieve the current flags

----------------
-- | RegexMaker captures the creation of the compiled regular
-- expression from a source type and an option type.  The 'makeRegex'
-- function has a default implementation that depends on makeRegexOpts
-- and used 'defaultCompOpt' and 'defaultExecOpt'.
class (RegexOptions regex compOpt execOpt) => RegexMaker regex compOpt execOpt source 
  | regex -> compOpt execOpt, compOpt -> regex execOpt, execOpt -> regex compOpt where
  -- | make using the defaultCompOpt and defaultExecOpt
  makeRegex :: source -> regex
  -- | Specify your own options
  makeRegexOpts :: compOpt -> execOpt -> source -> regex

  makeRegex = makeRegexOpts defaultCompOpt defaultExecOpt

----------------
-- | RegexLike is parametrized on a regular expression type and a
-- source type to run the matching on.
--
-- There are default implementations: matchTest and matchOnceText
-- using matchOnce; matchCount and matchAllText using
-- matchAll. matchOnce uses matchOnceText and matchAll uses
-- matchAllText. So a minimal complete instance need to provide
-- (matchOnce or matchOnceText) and (matchAll or matchAllText).
class (Extract source)=> RegexLike regex source where
  matchAll   :: regex -> source-> [MatchArray]
  -- | This can return an array of (offset,length) index pairs for the
  -- match and captured substrings.
  matchOnce  :: regex -> source-> Maybe MatchArray
  matchCount :: regex -> source-> Int
  matchTest  :: regex -> source-> Bool
  matchAllText  :: regex -> source-> [MatchText source]
  -- | This can return a tuple of three items: the source before the
  -- match, an array of the match and captured substrings (with their
  -- indices), and the source after the match.
  matchOnceText :: regex -> source-> Maybe (source,MatchText source,source)

  matchAll regex source = map (fmap snd) (matchAllText regex source)
  matchOnce regex source = fmap (\(_,mt,_) -> fmap snd mt) (matchOnceText regex source)
  matchTest regex source = isJust (matchOnce regex source)
  matchCount regex source = length (matchAll regex source)
  matchOnceText regex source = 
    fmap (\ma -> let (o,l) = ma!0
                 in (before o source
                    ,fmap (\ol -> (extract ol source,ol)) ma
                    ,after (o+l) source))
         (matchOnce regex source)
  matchAllText regex source =
    map (fmap (\ol -> (extract ol source,ol)))
        (matchAll regex source)

----------------
-- | RegexContext is the polymorphic interface to do matching
class (RegexLike regex source) => RegexContext regex source target where
  match :: regex -> source -> target
  matchM :: (Monad m) => regex -> source -> m target

----------------
-- | Extract allows for indexing operations on String or ByteString.
class Extract source where
  -- | before is a renamed "take"
  before :: Int -> source -> source
  -- | after is a renamed "drop"
  after :: Int -> source -> source
  -- | For when there is no match, this can construct an empty data value
  empty :: source
  -- | extract takes an offset and length and has a default
  -- implementation of @extract (off,len) source = before len (after
  -- off source)@
  extract :: (Int,Int) -> source -> source
  extract (off,len) source = before len (after off source)

instance Extract String where
  before =  take; after = drop; empty = []

instance Extract ByteString where
  before = B.take; after = B.drop; empty = B.empty
