{-# OPTIONS_GHC  -fglasgow-exts -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex.Posix.Wrap
-- Copyright   :  (c) Chris Kuklewicz 2006 derived from (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org, textregexlazy@personal.mightyreason.com
-- Stability   :  experimental
-- Portability :  non-portable (regex-base needs MPTC+FD)
--
-- WrapPosix.hsc exports a wrapped version of the ffi imports.  To
-- increase type safety, the flags are newtype'd.  The other important
-- export is a 'Regex' type that is specific to the Posix library
-- backend.  The flags are documented in "Text.Regex.Posix".  The
-- 'defaultCompOpt' is @(compExtended .|. compNewline)@.
--
-- The 'Regex', 'CompOption', and 'ExecOption' types and their 'RegexOptions'
-- instance is declared.  The '=~' and '=~~' convenience functions are
-- defined.
--
-- The exported symbols are the same whether HAVE_REGEX_H is defined, but
-- when it is not defined then @getVersion == Nothing@ and all other
-- exported values will call error or fail.
--
-- This module will fail or error only if allocation fails or a nullPtr
-- is passed in.
--
-----------------------------------------------------------------------------

module Text.Regex.Posix.Wrap(
  -- ** High-level API
  Regex,
  RegOffset,
  (=~),
  (=~~),

  -- ** Low-level API
  WrapError,
  wrapCompile,
  wrapTest,
  wrapMatch,
  wrapMatchAll,
  wrapCount,

  -- ** Miscellaneous
  unusedRegOffset,

  -- ** Compilation options
  CompOption(CompOption),
  compBlank,
  compExtended,   -- use extended regex syntax
  compIgnoreCase, -- ignore case when matching
  compNoSub,      -- no substring matching needed
  compNewline,    -- '.' doesn't match newline

  -- ** Execution options
  ExecOption(ExecOption),
  execBlank,
  execNotBOL,     -- not at begining of line
  execNotEOL,     -- not at end of line

  -- ** Return codes
  ReturnCode(ReturnCode),
  retBadbr,
  retBadpat,
  retBadrpt,
  retEcollate,
  retEctype,
  retEescape,
  retEsubreg,
  retEbrack,
  retEparen,
  retEbrace,
  retErange,
  retEspace
  ) where

#ifndef __NHC__
#include "HsRegexPosixConfig.h"
#else
#define HAVE_REGEX_H 1
#define HAVE_REGCOMP 1
#endif

#include <sys/types.h>

#if HAVE_REGEX_H && HAVE_REGCOMP
#include "regex.h"
#else
#include "regex/regex.h"

-- CFILES stuff is Hugs only
{-# CFILES cbits/reallocf.c #-}
{-# CFILES cbits/regcomp.c #-}
{-# CFILES cbits/regerror.c #-}
{-# CFILES cbits/regexec.c #-}
{-# CFILES cbits/regfree.c #-}
#endif

import Control.Monad(mapM)
import Data.Array(Array,listArray)
import Data.Bits(Bits(..))
import Data.Int -- need whatever #regoff_t type will be
import Foreign(Ptr, FunPtr, nullPtr, mallocForeignPtrBytes,
               addForeignPtrFinalizer, Storable(peekByteOff), allocaArray,
               allocaBytes, withForeignPtr,ForeignPtr,plusPtr)
import Foreign.C(CSize,CInt)
import Foreign.C.String(peekCAString, CString)
import Text.Regex.Base.RegexLike(RegexOptions(..),RegexMaker(..),RegexContext(..),MatchArray)

type CRegex = ()   -- dummy regex_t used below to read out nsub value

-- | RegOffset is "typedef int regoff_t" on Linux and ultimately "typedef
-- long long __int64_t" on Max OS X.  So rather than saying
-- 2,147,483,647 is all the length you need, I'll take the larger:
-- 9,223,372,036,854,775,807 should be enough bytes for anyone, no
-- need for Integer. The alternative is to compile to different sizes
-- in a platform dependent manner with "type RegOffset = (#type
-- regoff_t)", which I do not want to do.
--
-- There is also a special value 'unusedRegOffset' :: 'RegOffset' which is
-- (-1) and as a starting index means that the subgroup capture was
-- unused.  Otherwise the RegOffset indicates a character boundary that
-- is before the character at that index offset, with the first
-- character at index offset 0. So starting at 1 and ending at 2 means
-- to take only the second character.
type RegOffset = Int64

-- | A bitmapped 'CInt' containing options for compilation of regular
-- expressions.  Option values (and their man 3 regcomp names) are
--
--  * 'compBlank' which is a completely zero value for all the flags.
--    This is also the 'blankCompOpt' value.
--
--  * 'compExtended' (REG_EXTENDED) which can be set to use extended instead
--    of basic regular expressions.
--    This is set in the 'defaultCompOpt' value.
--
--  * 'compNewline' (REG_NEWLINE) turns on newline sensitivity: The dot (.)
--    and inverted set @[^ ]@ never match newline, and ^ and $ anchors do
--    match after and before newlines.
--    This is set in the 'defaultCompOpt' value.
--
--  * 'compIgnoreCase' (REG_ICASE) which can be set to match ignoring upper
--    and lower distinctions.
--
--  * 'compNoSub' (REG_NOSUB) which turns off all information from matching
--    except whether a match exists.
#ifdef __GLASGOW_HASKELL__
newtype CompOption = CompOption CInt deriving (Eq,Show,Num,Bits)
#else
newtype CompOption = CompOption CInt deriving (Eq,Show)

instance Num CompOption where
	CompOption x + CompOption y = CompOption (x + y)
	CompOption x - CompOption y = CompOption (x - y)
	CompOption x * CompOption y = CompOption (x * y)
	abs (CompOption x) = CompOption (abs x)
	signum (CompOption x) = CompOption (signum x)
	fromInteger n = CompOption (fromInteger n)

instance Bits CompOption where
	CompOption x .&. CompOption y = CompOption (x .&. y)
	CompOption x .|. CompOption y = CompOption (x .|. y)
	CompOption x `xor` CompOption y = CompOption (x `xor` y)
	complement (CompOption x) = CompOption (complement x)
	shift (CompOption x) n = CompOption (shift x n)
	rotate (CompOption x) n = CompOption (rotate x n)
	bitSize (CompOption x) = bitSize x
	isSigned (CompOption x) = isSigned x
#endif

-- | A bitmapped 'CInt' containing options for execution of compiled
-- regular expressions.  Option values (and their man 3 regexec names) are
--
--  * 'execBlank' which is a complete zero value for all the flags.  This is
--    the blankExecOpt value.
--
--  * 'execNotBOL' (REG_NOTBOL) can be set to prevent ^ from matching at the
--    start of the input.
--
--  * 'execNotEOL' (REG_NOTEOL) can be set to prevent $ from matching at the
--    end of the input (before the terminating NUL).
#ifdef __GLASGOW_HASKELL__
newtype ExecOption = ExecOption CInt deriving (Eq,Show,Num,Bits)
#else
newtype ExecOption = ExecOption CInt deriving (Eq,Show)

instance Num ExecOption where
	ExecOption x + ExecOption y = ExecOption (x + y)
	ExecOption x - ExecOption y = ExecOption (x - y)
	ExecOption x * ExecOption y = ExecOption (x * y)
	abs (ExecOption x) = ExecOption (abs x)
	signum (ExecOption x) = ExecOption (signum x)
	fromInteger n = ExecOption (fromInteger n)

instance Bits ExecOption where
	ExecOption x .&. ExecOption y = ExecOption (x .&. y)
	ExecOption x .|. ExecOption y = ExecOption (x .|. y)
	ExecOption x `xor` ExecOption y = ExecOption (x `xor` y)
	complement (ExecOption x) = ExecOption (complement x)
	shift (ExecOption x) n = ExecOption (shift x n)
	rotate (ExecOption x) n = ExecOption (rotate x n)
	bitSize (ExecOption x) = bitSize x
	isSigned (ExecOption x) = isSigned x
#endif

-- | ReturnCode is an enumerated 'CInt', corresponding to the error codes
-- from @man 3 regex@:
--
-- * 'retBadbr' (@REG_BADBR@) invalid repetition count(s) in @{ }@
--
-- * 'retBadpat' (@REG_BADPAT@) invalid regular expression
--
-- * 'retBadrpt' (@REG_BADRPT@) @?@, @*@, or @+@ operand invalid
--
-- * 'retEcollate' (@REG_ECOLLATE@) invalid collating element
--
-- * 'retEctype' (@REG_ECTYPE@) invalid character class
--
-- * 'retEescape' (@REG_EESCAPE@) @\\@ applied to unescapable character
--
-- * 'retEsubreg' (@REG_ESUBREG@) invalid backreference number
--
-- * 'retEbrack' (@REG_EBRACK@) brackets @[ ]@ not balanced
--
-- * 'retEparen' (@REG_EPAREN@) parentheses @( )@ not balanced
--
-- * 'retEbrace' (@REG_EBRACE@) braces @{ }@ not balanced
--
-- * 'retErange' (@REG_ERANGE@) invalid character range in @[ ]@
--
-- * 'retEspace' (@REG_ESPACE@) ran out of memory
--
-- * 'retNoMatch' (@REG_NOMATCH@) The regexec() function failed to match
--
newtype ReturnCode = ReturnCode CInt deriving (Eq,Show)

-- | A compiled regular expression.
data Regex = Regex (ForeignPtr CRegex) CompOption ExecOption

-- | A completely zero value for all the flags.
-- This is also the 'blankCompOpt' value.
compBlank :: CompOption
compBlank = CompOption 0

-- | A completely zero value for all the flags.
-- This is also the 'blankExecOpt' value.
execBlank :: ExecOption
execBlank = ExecOption 0

unusedRegOffset :: RegOffset
unusedRegOffset = (-1)

-- | The return code will be retOk when it is the Haskell wrapper and
-- not the underlying library generating the error message.
type WrapError = (ReturnCode,String)

wrapCompile :: CompOption -- ^ Flags (bitmapped)
            -> ExecOption -- ^ Flags (bitmapped)
            -> CString -- ^ The regular expression to compile (ASCII only, no null bytes)
            -> IO (Either WrapError Regex) -- ^ Returns: the compiled regular expression

wrapTest :: Regex -> CString
         -> IO (Either WrapError Bool)

-- | wrapMatch returns offsets for the begin and end of each capture.
-- Unused captures have offsets of unusedRegOffset which is (-1)
wrapMatch :: Regex -> CString
          -> IO (Either WrapError (Maybe [(RegOffset,RegOffset)]))

-- | wrapMatchAll returns the offset and length of each capture.
-- Unused captures have an offset of unusedRegOffset which is (-1) and
-- length of 0.
wrapMatchAll :: Regex -> CString
             -> IO (Either WrapError [MatchArray])

wrapCount :: Regex -> CString
          -> IO (Either WrapError Int)

(=~)  :: (RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target)
      => source1 -> source -> target
(=~~) :: (RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target,Monad m)
      => source1 -> source -> m target

instance RegexOptions Regex CompOption ExecOption where
  blankCompOpt = compBlank
  blankExecOpt = execBlank
  defaultCompOpt = compExtended .|. compNewline
  defaultExecOpt = execBlank
  setExecOpts e' (Regex r c _) = Regex r c e'
  getExecOpts (Regex _ _ e) = e

-- (=~) ::(RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target) => source1 -> source -> target
(=~) x r = let make :: RegexMaker Regex CompOption ExecOption a => a -> Regex
               make = makeRegex
           in match (make r) x

-- (=~~) ::(RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target,Monad m) => source1 -> source -> m target
(=~~) x r = let make :: RegexMaker Regex CompOption ExecOption a => a -> Regex
                make = makeRegex
            in matchM (make r) x

type CRegMatch = () -- dummy regmatch_t used below to read out so and eo values

-- -----------------------------------------------------------------------------
-- The POSIX regex C interface

#if __GLASGOW_HASKELL__ || __HUGS__

foreign import ccall unsafe "regcomp"
  c_regcomp :: Ptr CRegex -> CString -> CompOption -> IO ReturnCode

foreign import ccall unsafe "&regfree"
  c_regfree :: FunPtr (Ptr CRegex -> IO ())

foreign import ccall unsafe "regexec"
  c_regexec :: Ptr CRegex -> CString -> CSize
            -> Ptr CRegMatch -> ExecOption -> IO ReturnCode

foreign import ccall unsafe "regerror"
  c_regerror :: ReturnCode -> Ptr CRegex
             -> CString -> CSize -> IO CSize

#elif HAVE_REGEX_H && HAVE_REGCOMP

foreign import ccall unsafe "regex.h regcomp"
  c_regcomp :: Ptr CRegex -> CString -> CompOption -> IO ReturnCode

foreign import ccall unsafe "regex.h &regfree"
  c_regfree :: FunPtr (Ptr CRegex -> IO ())

foreign import ccall unsafe "regex.h regexec"
  c_regexec :: Ptr CRegex -> CString -> CSize
            -> Ptr CRegMatch -> ExecOption -> IO ReturnCode

foreign import ccall unsafe "regex.h regerror"
  c_regerror :: ReturnCode -> Ptr CRegex
             -> CString -> CSize -> IO CSize

#else

foreign import ccall unsafe "regex/regex.h regcomp"
  c_regcomp :: Ptr CRegex -> CString -> CompOption -> IO ReturnCode

foreign import ccall unsafe "regex/regex.h &regfree"
  c_regfree :: FunPtr (Ptr CRegex -> IO ())

foreign import ccall unsafe "regex/regex.h regexec"
  c_regexec :: Ptr CRegex -> CString -> CSize
            -> Ptr CRegMatch -> ExecOption -> IO ReturnCode

foreign import ccall unsafe "regex/regex.h regerror"
  c_regerror :: ReturnCode -> Ptr CRegex
             -> CString -> CSize -> IO CSize
#endif

retOk :: ReturnCode
retOk = ReturnCode 0

-- Flags for regexec
#enum ExecOption,ExecOption, \
  execNotBOL = REG_NOTBOL, \
  execNotEOL = REG_NOTEOL

-- Flags for regcomp
#enum CompOption,CompOption, \
  compExtended = REG_EXTENDED, \
  compIgnoreCase = REG_ICASE, \
  compNoSub = REG_NOSUB, \
  compNewline = REG_NEWLINE

-- Return values from regexec (REG_NOMATCH, REG_ESPACE,...)
-- Error codes from regcomp (not REG_NOMATCH)
-- Though calling retNoMatch an error is rather missing the point...
#enum ReturnCode,ReturnCode, \
  retNoMatch = REG_NOMATCH, \
  retBadbr = REG_BADBR, \
  retBadpat = REG_BADPAT, \
  retBadrpt = REG_BADRPT, \
  retEcollate = REG_ECOLLATE, \
  retEctype = REG_ECTYPE, \
  retEescape = REG_EESCAPE, \
  retEsubreg = REG_ESUBREG, \
  retEbrack = REG_EBRACK, \
  retEparen = REG_EPAREN, \
  retEbrace = REG_EBRACE, \
  retErange = REG_ERANGE, \
  retEspace = REG_ESPACE
----
-- error helpers

nullTest :: Ptr a -> String -> IO (Either WrapError b) -> IO (Either WrapError b)
{-# INLINE nullTest #-}
nullTest ptr msg io = do
  if nullPtr == ptr
    then return (Left (retOk,"Ptr parameter was nullPtr in Text.Regex.TRE.Wrap."++msg))
    else io

{-
wrapRC :: ReturnCode -> IO (Either WrapError b)
{-# INLINE wrapRC #-}
wrapRC r = return (Left (r,"Error in Text.Regex.Posix.Wrap: "++show r))
-}
wrapError :: ReturnCode -> Ptr CRegex -> IO (Either WrapError b)
wrapError errCode regex_ptr = do
  -- Call once to compute the error message buffer size
  errBufSize <- c_regerror errCode regex_ptr nullPtr 0
  -- Allocate a temporary buffer to hold the error message
  allocaArray (fromIntegral errBufSize) $ \errBuf -> do
   nullTest errBuf "wrapError errBuf" $ do
    c_regerror errCode regex_ptr errBuf errBufSize
    msg <- peekCAString errBuf :: IO String
    return (Left (errCode, msg))

----------
wrapCompile flags e pattern = do
 nullTest pattern "wrapCompile pattern" $ do
  regex_fptr <- mallocForeignPtrBytes (#const sizeof(regex_t))
  withForeignPtr regex_fptr $ \regex_ptr -> do
    if nullPtr == regex_ptr
      then return (Left (retOk,"Text.Regex.Posix.Wrap.wrapCompile could not malloc"))
      else do
        errCode <- c_regcomp regex_ptr pattern flags
        addForeignPtrFinalizer c_regfree regex_fptr
        if (errCode == retOk)
          then return . Right $ Regex regex_fptr flags e
          else wrapError errCode regex_ptr

---------
wrapTest (Regex regex_fptr _ flags) cstr = do
 nullTest cstr "wrapTest" $ do
  withForeignPtr regex_fptr $ \regex_ptr -> do
    r <- c_regexec regex_ptr cstr 0 nullPtr flags
    if r == retOk
      then return (Right True)
      else if r == retNoMatch
              then return (Right False)
              else wrapError r regex_ptr

---------
wrapMatch regex@(Regex regex_fptr compileOptions flags) cstr = do
 nullTest cstr "wrapMatch cstr" $ do
  if (0 /= compNoSub .&. compileOptions)
    then do
      r <- wrapTest regex cstr
      case r of
        Right True -> return (Right (Just [])) -- Source of much "wtf?" crap
        Right False -> return (Right Nothing)
        Left err -> return (Left err)
    else do
      withForeignPtr regex_fptr $ \regex_ptr -> do
        nsub <- (#peek regex_t, re_nsub) regex_ptr :: IO CSize
        let nsub_int,nsub_bytes :: Int
            nsub_int = fromIntegral nsub
            nsub_bytes = ((1 + nsub_int) * (#const sizeof(regmatch_t)))
        -- add one because index zero covers the whole match
        allocaBytes nsub_bytes $ \p_match -> do
         nullTest p_match "wrapMatch allocaBytes" $ do
          doMatch regex_ptr cstr nsub p_match flags

-- Very very thin wrapper
-- Requires, but does not check, that nsub>=0
-- Cannot return (Right (Just []))
doMatch :: Ptr CRegex -> CString -> CSize -> Ptr CRegMatch -> ExecOption
        -> IO (Either WrapError (Maybe [(RegOffset,RegOffset)]))
{-# INLINE doMatch #-}
doMatch regex_ptr cstr nsub p_match flags = do
  r <- c_regexec regex_ptr cstr (1 + nsub) p_match flags
  if r == retOk
    then do
       regions <- mapM getOffsets . take (1+fromIntegral nsub)
                  . iterate (`plusPtr` (#const sizeof(regmatch_t))) $ p_match
       return (Right (Just regions)) -- regions will not be []
    else if r == retNoMatch
       then return (Right Nothing)
       else wrapError r regex_ptr
  where
    getOffsets :: Ptr CRegMatch -> IO (RegOffset,RegOffset)
    {-# INLINE getOffsets #-}
    getOffsets pmatch' = do
      start <- (#peek regmatch_t, rm_so) pmatch' :: IO (#type regoff_t)
      end   <- (#peek regmatch_t, rm_eo) pmatch' :: IO (#type regoff_t)
      return (fromIntegral start,fromIntegral end)

wrapMatchAll regex@(Regex regex_fptr compileOptions flags) cstr = do
 nullTest cstr "wrapMatchAll cstr" $ do
  if (0 /= compNoSub .&. compileOptions)
    then do
      r <- wrapTest regex cstr
      case r of
        Right True -> return (Right [(toMA 0 [])]) -- Source of much "wtf?" crap
        Right False -> return (Right [])
        Left err -> return (Left err)
    else do
      withForeignPtr regex_fptr $ \regex_ptr -> do
        nsub <- (#peek regex_t, re_nsub) regex_ptr :: IO CSize
        let nsub_int,nsub_bytes :: Int
            nsub_int = fromIntegral nsub
            nsub_bytes = ((1 + nsub_int) * (#const sizeof(regmatch_t)))
        -- add one because index zero covers the whole match
        allocaBytes nsub_bytes $ \p_match -> do
         nullTest cstr "wrapMatchAll p_match" $ do
          let flags' = (execNotBOL .|. flags)
              at pos = doMatch regex_ptr (plusPtr cstr pos) nsub p_match flags'
              loop acc old (s,e) | old `seq` s == e = return (Right (acc []))
                                 | otherwise = do
                let pos = old + fromIntegral e
                result <- at pos
                case result of
                  Right Nothing -> return (Right (acc []))
                  Right (Just parts@(whole:_)) -> let ma = toMA pos parts
                                                  in loop (acc.(ma:)) pos whole
                  Left err -> return (Left err)
                  Right (Just []) -> return (Right (acc [(toMA pos [])])) -- should never happen
          result <- doMatch regex_ptr cstr nsub p_match flags
          case result of
            Right Nothing -> return (Right [])
            Right (Just parts@(whole:_)) -> let ma = toMA 0 parts
                                            in loop (ma:) 0 whole
            Left err -> return (Left err)
            Right (Just []) -> return (Right [(toMA 0 [])]) -- should never happen
  where
    toMA :: Int -> [(RegOffset,RegOffset)] -> Array Int (Int,Int)
    toMA pos [] = listArray (0,0) [(pos,0)] -- wtf?
    toMA pos parts = listArray (0,pred (length parts))
      . map (\(s,e)-> if s>=0 then (pos+fromIntegral s, fromIntegral (e-s)) else (-1,0))
      $ parts

---------
wrapCount regex@(Regex regex_fptr compileOptions flags) cstr = do
 nullTest cstr "wrapCount cstr" $ do
  if (0 /= compNoSub .&. compileOptions)
    then do
      r <- wrapTest regex cstr
      case r of
        Right True -> return (Right 1)
        Right False -> return (Right 0)
        Left err -> return (Left err)
    else do
      withForeignPtr regex_fptr $ \regex_ptr -> do
        let nsub_bytes = (#size regmatch_t)
        allocaBytes nsub_bytes $ \p_match -> do
         nullTest cstr "wrapCount p_match" $ do
          let flags' = (execNotBOL .|. flags)
              at pos = doMatch regex_ptr (plusPtr cstr pos) 0 p_match flags'
              loop acc old (s,e) | acc `seq` old `seq` False = undefined
                                 | s == e = return (Right acc)
                                 | otherwise = do
                let pos = old + fromIntegral e
                result <- at pos
                case result of
                  Right Nothing -> return (Right acc)
                  Right (Just (whole:_)) -> loop (succ acc) pos whole
                  Left err -> return (Left err)
                  Right (Just []) -> return (Right acc) -- should never happen
          result <- doMatch regex_ptr cstr 0 p_match flags
          case result of
            Right Nothing -> return (Right 0)
            Right (Just (whole:_)) -> loop 1 0 whole
            Left err -> return (Left err)
            Right (Just []) -> return (Right 0) -- should never happen

{-

-- This is the slower 0.66 version of the code (91s instead of 79s on 10^6 bytes)

wrapMatchAll regex cstr = do
  let regex' = setExecOpts (execNotBOL .|. (getExecOpts regex)) regex
      at pos = wrapMatch regex' (plusPtr cstr pos)
      loop old (s,e) | s == e = return []
                     | otherwise = do
        let pos = old + fromIntegral e
        result <- at pos
        case unwrap result of
          Nothing -> return []
          Just [] -> return ((toMA pos []):[]) -- wtf?
          Just parts@(whole:_) -> do rest <- loop pos whole
                                     return ((toMA pos parts) : rest)
  result <- wrapMatch regex cstr
  case unwrap result of
    Nothing -> return []
    Just [] -> return ((toMA 0 []):[]) -- wtf?
    Just parts@(whole:_) -> do rest <- loop 0 whole
                               return ((toMA 0 parts) : rest)
---------
-- This was also changed to match wrapMatchAll after 0.66
wrapCount regex cstr = do
  let regex' = setExecOpts (execNotBOL .|. (getExecOpts regex)) regex
      at pos = wrapMatch regex' (plusPtr cstr pos)
      loop acc old (s,e) | acc `seq` old `seq` False = undefined
                         | s == e = return acc
                         | otherwise = do
        let pos = old + fromIntegral e
        result <- at pos
        case unwrap result of
          Nothing -> return acc
          Just [] -> return (succ acc) -- wtf?
          Just (whole:_) -> loop (succ acc) pos whole
  result <- wrapMatch regex cstr
  case unwrap result of
    Nothing -> return 0
    Just [] -> return 1 -- wtf?
    Just (whole:_) -> loop 1 0 whole
-}
