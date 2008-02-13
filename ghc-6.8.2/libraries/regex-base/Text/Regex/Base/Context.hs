{-# OPTIONS_GHC -fglasgow-exts #-}
{-|

Module      :  Text.Regex.Base.Context
Copyright   :  (c) Chris Kuklewicz 2006
License     :  BSD-style (see the file LICENSE)

Maintainer  :  libraries@haskell.org, textregexlazy@personal.mightyreason.com
Stability   :  experimental
Portability :  non-portable (MPTC+FD)

This is a module of instances of 'RegexContext'.  Nothing else is
exported.   These work for all the front ends and backends
interchangably.  These instances are important because they provide
the different results that can be gotten from a match or matchM
operation (often via the @=~@ and @=~~@ operators).  The name is Context
because they are context dependent: use them in a context that expects
an Int and you get a count of matches, use them in a Bool context and
get True if there is a match, etc.

@ 
type MatchArray = Array Int (MatchOffset, MatchLength)
type MatchText source = Array Int (source, (MatchOffset, MatchLength))
@

These are for the first match:

(@ RegexLike a b => RegexContext a b Bool @) :
  Whether there is any match or not

(@ RegexLike a b => RegexContext a b () @) :
  Useful using @=~~@ in a monad, since failure to match is via 'fail'

(@ RegexLike a b => RegexContext a b (MatchOffset,MatchLength) @) :
  This returns the initial index and length of the whole match.
  Starting at (-1) indicates failure to match.

(@ RegexLike a b => RegexContext a b b @) :
  This returns the text of the whole match.
  It will return 'empty' from 'Extract' if there is no match.
  These are defined in other modules, but documented here for convenience.

(@ RegexLike a b => RegexContext a b MatchArray @) :
  Where each sub-part the match starts and its length.
  Starting at (-1) indicates unused.

(@ RegexLike a b => RegexContext a b (Array Int b) @) :
  The text of each sub-part of the match.
  Unused matches are 'empty' (defined via 'Extract')

(@ RegexLike a b => RegexContext a b (b, MatchText b, b) @) :
  The text before the match, the details of the match, and the text after the match

(@ RegexLike a b => RegexContext a b (b, b, b) @) :
  The text before the match, the text of the match, the text after the match

(@ RegexLike a b => RegexContext a b (b, b, b, [b]) @) :
  The text before the match, the text of the match, the text after the match, and the text of the 1st and higher sub-parts of the match.  This is the same return value as used in Text.Regex.

(@ RegexLike a b => RegexContext a b (MatchResult b) @) :
  The 'MatchResult' structure for the match.

These instances are for all the matches (non-overlapping):

(@ RegexLike a b => RegexContext a b Int @) :
  The number of matches

These instances similar to the single match ones above, but in a List:

(@ RegexLike a b => RegexContext a b [(MatchOffset, MatchLength)] @) :

(@ RegexLike a b => RegexContext a b [b] @) :
  As a policy, I chose [b] to be the list of whole text of each match.

(@ RegexLike a b => RegexContext a b [MatchArray] @) :

(@ RegexLike a b => RegexContext a b [Array Int b] @) :

(@ RegexLike a b => RegexContext a b [MatchText b] @) :

(@ RegexLike a b => RegexContext a b [[b]] @) :
  This is the list of the list of the text of the sub-part of each match.
  Unused matches are 'empty' (defined via 'Extract')

-}

module Text.Regex.Base.Context() where

import Data.Array(Array,(!),elems,listArray)
import Data.Maybe(maybe)
import Text.Regex.Base.RegexLike(RegexLike(..),RegexContext(..)
  ,MatchResult(..),Extract(empty),MatchOffset,MatchLength,MatchArray,MatchText)

{-
-- Get the ByteString type for mood/doom
import Data.ByteString(ByteString)
-- Get the Regex types for the mood/doom workaround
import qualified Text.Regex.Lib.WrapPosix as R1(Regex)
import qualified Text.Regex.Lib.WrapPCRE as R2(Regex)
import qualified Text.Regex.Lib.WrapLazy as R3(Regex)
import qualified Text.Regex.Lib.WrapDFAEngine as R4(Regex)
-- Get the RegexLike instances
import Text.Regex.Lib.StringPosix()
import Text.Regex.Lib.StringPCRE()
import Text.Regex.Lib.StringLazy()
import Text.Regex.Lib.StringDFAEngine()
import Text.Regex.Lib.ByteStringPosix()
import Text.Regex.Lib.ByteStringPCRE()
import Text.Regex.Lib.ByteStringLazy()
import Text.Regex.Lib.ByteStringDFAEngine()
-}
{-

mood :: (RegexLike a b) => a -> b -> b
{-# INLINE mood #-}
mood r s = case matchOnceText r s of
    Nothing -> empty
    Just (_,ma,_) -> fst (ma!0)

doom :: (RegexLike a b,Monad m) => a -> b -> m b
{-# INLINE doom #-}
doom =  actOn (\(_,ma,_)->fst (ma!0))

{- These run afoul of various restrictions if I say
   "instance RegexContext a b b where"
   so I am listing these cases explicitly
-}

instance RegexContext R1.Regex String String where match = mood; matchM = doom
instance RegexContext R2.Regex String String where match = mood; matchM = doom
instance RegexContext R3.Regex String String where match = mood; matchM = doom
instance RegexContext R4.Regex String String where match = mood; matchM = doom
instance RegexContext R1.Regex ByteString ByteString where match = mood; matchM = doom
instance RegexContext R2.Regex ByteString ByteString where match = mood; matchM = doom
instance RegexContext R3.Regex ByteString ByteString where match = mood; matchM = doom
instance RegexContext R4.Regex ByteString ByteString where match = mood; matchM = doom
-}


nullArray :: Array Int a
{-# INLINE nullArray #-}
nullArray = listArray (1,0) []

nullFail :: (RegexContext regex source [target],Monad m) => regex -> source -> m [target]
{-# INLINE nullFail #-}
nullFail r s = case match r s of
                 [] -> regexFailed
                 xs -> return xs

regexFailed :: (Monad m) => m b
{-# INLINE regexFailed #-}
regexFailed =  fail $ "regex failed to match"

actOn :: (RegexLike r s,Monad m) => ((s,MatchText s,s)->t) -> r -> s -> m t
{-# INLINE actOn #-}
actOn f r s = case matchOnceText r s of
    Nothing -> regexFailed
    Just preMApost -> return (f preMApost)

-- ** Instances based on matchTest ()

instance (RegexLike a b) => RegexContext a b Bool where 
  match = matchTest
  matchM r s = case match r s of
                 False -> regexFailed
                 True -> return True

instance (RegexLike a b) => RegexContext a b () where
  match _ _ = ()
  matchM r s = case matchTest r s of
                 False -> regexFailed
                 True -> return ()

-- ** Instance based on matchCount

instance (RegexLike a b) => RegexContext a b Int where
  match = matchCount
  matchM r s = case match r s of
                 0 -> regexFailed
                 x -> return x

-- ** Instances based on matchOnce,matchOnceText

instance (RegexLike a b) => RegexContext a b (MatchOffset,MatchLength) where 
  match r s = maybe (-1,0) (!0) (matchOnce r s)
  matchM r s = maybe regexFailed (return.(!0)) (matchOnce r s)

#if __GLASGOW_HASKELL__
-- overlaps with instance (RegexLike a b) => RegexContext a b (Array Int b)
instance (RegexLike a b) => RegexContext a b MatchArray where 
  match r s = maybe nullArray id (matchOnce r s)
  matchM r s = maybe regexFailed return (matchOnce r s)
#endif

instance (RegexLike a b) => RegexContext a b (b,MatchText b,b) where 
  match r s = maybe (s,nullArray,empty) id (matchOnceText r s)
  matchM r s = maybe regexFailed return (matchOnceText r s)

instance (RegexLike a b) => RegexContext a b (Array Int b) where 
  match r s = maybe nullArray id (matchM r s)
  matchM = actOn (\(_,ma,_) -> fmap fst ma)

instance (RegexLike a b) => RegexContext a b (b,b,b) where 
  match r s = maybe (s,empty,empty) id (matchM r s)
  matchM = actOn (\(pre,ma,post) -> let ((whole,_):_) = elems ma
                                    in (pre,whole,post))

instance (RegexLike a b) => RegexContext a b (b,b,b,[b]) where 
  match r s = maybe (s,empty,empty,[]) id (matchM r s)
  matchM = actOn (\(pre,ma,post) -> let ((whole,_):subs) = elems ma
                                    in (pre,whole,post,map fst subs))

instance (RegexLike a b) => RegexContext a b (MatchResult b) where 
  match r s = maybe (MR {mrBefore = s,mrMatch = empty,mrAfter = empty
                        ,mrSubs = nullArray,mrSubList = []}) id (matchM r s)
  matchM = actOn (\(pre,ma,post) -> 
     let ((whole,_):subs) = elems ma
     in MR { mrBefore = pre
           , mrMatch = whole
           , mrAfter = post
           , mrSubs = fmap fst ma
           , mrSubList = map fst subs })

-- ** Instances based on matchAll,matchAllText

#if __GLASGOW_HASKELL__
-- overlaps with instance (RegexLike a b) => RegexContext a b [Array Int b]
instance (RegexLike a b) => RegexContext a b [MatchArray] where
  match = matchAll
  matchM = nullFail
#endif

instance (RegexLike a b) => RegexContext a b [MatchText b] where
  match = matchAllText
  matchM = nullFail
           
#if __GLASGOW_HASKELL__
-- overlaps with instance (RegexLike a b) => RegexContext a b [b]
instance (RegexLike a b) => RegexContext a b [(MatchOffset,MatchLength)] where
  match r s = [ ma!0 | ma <- matchAll r s ]
  matchM = nullFail
#endif

instance (RegexLike a b) => RegexContext a b [b] where
  match r s = [ fst (ma!0) | ma <- matchAllText r s ]
  matchM = nullFail

instance (RegexLike a b) => RegexContext a b [Array Int b] where
  match r s = [ fmap fst ma | ma <- matchAllText r s ]
  matchM = nullFail

instance (RegexLike a b) => RegexContext a b [[b]] where
  match r s = [ map fst (elems ma) | ma <- matchAllText r s ]
  matchM = nullFail
