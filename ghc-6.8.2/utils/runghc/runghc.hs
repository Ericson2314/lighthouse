{-# OPTIONS -cpp -fffi #-}
#if __GLASGOW_HASKELL__ < 603
#include "config.h"
#else
#include "ghcconfig.h"
#endif
-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2004
--
-- runghc program, for invoking from a #! line in a script.  For example:
--
--   script.lhs:
--      #! /usr/bin/runghc
--      > main = putStrLn "hello!"
--
-- runghc accepts one flag:
--
--      -f <path>    specify the path
--
-- -----------------------------------------------------------------------------

module Main (main) where

import System.Environment
import System.IO
import Data.List
import System.Exit
import Data.Char

#ifdef USING_COMPAT
import Compat.RawSystem ( rawSystem )
import Compat.Directory ( findExecutable )
#else
import System.Cmd       ( rawSystem )
import System.Directory ( findExecutable )
#endif

main :: IO ()
main = do
    args <- getArgs
    case getGhcLoc args of
        (Just ghc, args') -> doIt ghc args'
        (Nothing, args') -> do
            mb_ghc <- findExecutable "ghc"
            case mb_ghc of
                Nothing  -> dieProg ("cannot find ghc")
                Just ghc -> doIt ghc args'

getGhcLoc :: [String] -> (Maybe FilePath, [String])
getGhcLoc ("-f" : ghc : args) = (Just ghc, args)
getGhcLoc (('-' : 'f' : ghc) : args) = (Just ghc, args)
-- If you need the first GHC flag to be a -f flag then you can pass --
-- first
getGhcLoc ("--" : args) = (Nothing, args)
getGhcLoc args = (Nothing, args)

doIt :: String -> [String] -> IO ()
doIt ghc args = do
    let (ghc_args, rest) = getGhcArgs args
    case rest of
        [] -> dieProg usage
        filename : prog_args -> do
            let expr = "System.Environment.withProgName " ++ show filename ++
                       " (System.Environment.withArgs " ++ show prog_args ++
                       " (GHC.TopHandler.runIOFastExit" ++
                       " (Main.main Prelude.>> Prelude.return ())))"
            res <- rawSystem ghc (["-ignore-dot-ghci"] ++ ghc_args ++
                                  [ "-e", expr, filename])
               -- runIOFastExit: makes exceptions raised by Main.main
               -- behave in the same way as for a compiled program.
               -- The "fast exit" part just calls exit() directly
               -- instead of doing an orderly runtime shutdown,
               -- otherwise the main GHCi thread will complain about
               -- being interrupted.
               --
               -- Why (main >> return ()) rather than just main?  Because
               -- otherwise GHCi by default tries to evaluate the result
               -- of the IO in order to show it (see #1200).
            exitWith res

getGhcArgs :: [String] -> ([String], [String])
getGhcArgs args = case break pastArgs args of
                      (xs, "--":ys) -> (xs, ys)
                      (xs, ys)      -> (xs, ys)

pastArgs :: String -> Bool
-- You can use -- to mark the end of the flags, in caes you need to use
-- a file called -foo.hs for some reason. You almost certainly shouldn't,
-- though.
pastArgs "--" = True
pastArgs ('-':_) = False
pastArgs _       = True

dieProg :: String -> IO a
dieProg msg = do
    p <- getProgName
    hPutStrLn stderr (p ++ ": " ++ msg)
    exitWith (ExitFailure 1)

usage :: String
usage = "syntax: runghc [-f GHC-PATH | --] [GHC-ARGS] [--] FILE ARG..."

