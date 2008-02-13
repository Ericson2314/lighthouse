{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-----------------------------------------------------------------------------
--
-- Parsing the top of a Haskell source file to get its module name,
-- imports and options.
--
-- (c) Simon Marlow 2005
-- (c) Lemmih 2006
--
-----------------------------------------------------------------------------

module HeaderInfo ( getImports
                  , getOptionsFromFile, getOptions
                  , optionsErrorMsgs ) where

#include "HsVersions.h"

import Parser		( parseHeader )
import Lexer
import FastString
import HsSyn		( ImportDecl(..), HsModule(..) )
import Module		( ModuleName, moduleName )
import PrelNames        ( gHC_PRIM, mAIN_NAME )
import StringBuffer	( StringBuffer(..), hGetStringBuffer, hGetStringBufferBlock
                        , appendStringBuffers )
import Config
import SrcLoc
import DynFlags
import ErrUtils
import Util
import Outputable
import Pretty           ()
import Panic
import Maybes
import Bag		( emptyBag, listToBag )

import Control.Exception
import Control.Monad
import System.Exit
import System.IO
import Data.List

#if __GLASGOW_HASKELL__ >= 601
import System.IO		( openBinaryFile )
#else
import IOExts                   ( openFileEx, IOModeEx(..) )
#endif

#if __GLASGOW_HASKELL__ < 601
openBinaryFile fp mode = openFileEx fp (BinaryMode mode)
#endif

getImports :: DynFlags -> StringBuffer -> FilePath -> FilePath
    -> IO ([Located ModuleName], [Located ModuleName], Located ModuleName)
getImports dflags buf filename source_filename = do
  let loc  = mkSrcLoc (mkFastString filename) 1 0
  case unP parseHeader (mkPState buf loc dflags) of
	PFailed span err -> parseError span err
	POk pst rdr_module -> do
          let ms = getMessages pst
          printErrorsAndWarnings dflags ms
          when (errorsFound dflags ms) $ exitWith (ExitFailure 1)
	  case rdr_module of
	    L _ (HsModule mb_mod _ imps _ _ _ _ _) ->
	      let
                main_loc = mkSrcLoc (mkFastString source_filename) 1 0
		mod = mb_mod `orElse` L (srcLocSpan main_loc) mAIN_NAME
	        (src_idecls, ord_idecls) = partition isSourceIdecl (map unLoc imps)
		source_imps   = map getImpMod src_idecls	
		ordinary_imps = filter ((/= moduleName gHC_PRIM) . unLoc) 
					(map getImpMod ord_idecls)
		     -- GHC.Prim doesn't exist physically, so don't go looking for it.
	      in
	      return (source_imps, ordinary_imps, mod)
  
parseError span err = throwDyn $ mkPlainErrMsg span err

isSourceIdecl (ImportDecl _ s _ _ _) = s

getImpMod (ImportDecl located_mod _ _ _ _) = located_mod

--------------------------------------------------------------
-- Get options
--------------------------------------------------------------


getOptionsFromFile :: FilePath            -- input file
                   -> IO [Located String] -- options, if any
getOptionsFromFile filename
    = Control.Exception.bracket
	      (openBinaryFile filename ReadMode)
              (hClose)
              (\handle ->
                   do buf <- hGetStringBufferBlock handle blockSize
                      loop handle buf)
    where blockSize = 1024
          loop handle buf
              | len buf == 0 = return []
              | otherwise
              = case getOptions' buf filename of
                  (Nothing, opts) -> return opts
                  (Just buf', opts) -> do nextBlock <- hGetStringBufferBlock handle blockSize
                                          newBuf <- appendStringBuffers buf' nextBlock
                                          if len newBuf == len buf
                                             then return opts
                                             else do opts' <- loop handle newBuf
                                                     return (opts++opts')

getOptions :: StringBuffer -> FilePath -> [Located String]
getOptions buf filename
    = case getOptions' buf filename of
        (_,opts) -> opts

-- The token parser is written manually because Happy can't
-- return a partial result when it encounters a lexer error.
-- We want to extract options before the buffer is passed through
-- CPP, so we can't use the same trick as 'getImports'.
getOptions' :: StringBuffer         -- Input buffer
            -> FilePath             -- Source file. Used for msgs only.
            -> ( Maybe StringBuffer -- Just => we can use more input
               , [Located String]   -- Options.
               )
getOptions' buf filename
    = parseToks (lexAll (pragState buf loc))
    where loc  = mkSrcLoc (mkFastString filename) 1 0

          getToken (buf,L _loc tok) = tok
          getLoc (buf,L loc _tok) = loc
          getBuf (buf,_tok) = buf
          combine opts (flag, opts') = (flag, opts++opts')
          add opt (flag, opts) = (flag, opt:opts)

          parseToks (open:close:xs)
              | IToptions_prag str <- getToken open
              , ITclose_prag       <- getToken close
              = map (L (getLoc open)) (words str) `combine`
                parseToks xs
          parseToks (open:close:xs)
              | ITinclude_prag str <- getToken open
              , ITclose_prag       <- getToken close
              = map (L (getLoc open)) ["-#include",removeSpaces str] `combine`
                parseToks xs
          parseToks (open:xs)
              | ITlanguage_prag <- getToken open
              = parseLanguage xs
          -- The last token before EOF could have been truncated.
          -- We ignore it to be on the safe side.
          parseToks [tok,eof]
              | ITeof <- getToken eof
              = (Just (getBuf tok),[])
          parseToks (eof:_)
              | ITeof <- getToken eof
              = (Just (getBuf eof),[])
          parseToks _ = (Nothing,[])
          parseLanguage ((_buf,L loc (ITconid fs)):rest)
              = checkExtension (L loc fs) `add`
                case rest of
                  (_,L loc ITcomma):more -> parseLanguage more
                  (_,L loc ITclose_prag):more -> parseToks more
                  (_,L loc _):_ -> languagePragParseError loc
          parseLanguage (tok:_)
              = languagePragParseError (getLoc tok)
          lexToken t = return t
          lexAll state = case unP (lexer lexToken) state of
                           POk state' t@(L _ ITeof) -> [(buffer state,t)]
                           POk state' t -> (buffer state,t):lexAll state'
                           _ -> [(buffer state,L (last_loc state) ITeof)]

checkExtension :: Located FastString -> Located String
checkExtension (L l ext)
-- Checks if a given extension is valid, and if so returns
-- its corresponding flag. Otherwise it throws an exception.
 =  let ext' = unpackFS ext in
    if ext' `elem` supportedLanguages
       || ext' `elem` (map ("No"++) supportedLanguages)
    then L l ("-X"++ext')
    else unsupportedExtnError l ext'

languagePragParseError loc =
  pgmError (showSDoc (mkLocMessage loc (
                text "cannot parse LANGUAGE pragma")))

unsupportedExtnError loc unsup =
  pgmError (showSDoc (mkLocMessage loc (
                text "unsupported extension: " <>
                text unsup)))


optionsErrorMsgs :: [String] -> [Located String] -> FilePath -> Messages
optionsErrorMsgs unhandled_flags flags_lines filename
  = (emptyBag, listToBag (map mkMsg unhandled_flags_lines))
  where	unhandled_flags_lines = [ L l f | f <- unhandled_flags, 
					  L l f' <- flags_lines, f == f' ]
        mkMsg (L flagSpan flag) = 
            ErrUtils.mkPlainErrMsg flagSpan $
                    text "unknown flag in  {-# OPTIONS #-} pragma:" <+> text flag

