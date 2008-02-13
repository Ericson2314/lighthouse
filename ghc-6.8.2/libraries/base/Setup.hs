{-
We need to do some ugly hacks here as base mix of portable and
unportable stuff, as well as home to some GHC magic.
-}

module Main (main) where

import Control.Monad
import Data.List
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Version
import System.Cmd
import System.FilePath

data When = Always | NewHaddock
    deriving (Eq, Show)

main :: IO ()
main = do let hooks = defaultUserHooks {
                  buildHook = build_primitive_sources
                            $ filter_modules_hook Always
                            $ buildHook defaultUserHooks,
                  makefileHook = build_primitive_sources
                               $ filter_modules_hook Always
                               $ makefileHook defaultUserHooks,
                  haddockHook = filter_modules_hook NewHaddock
                              $ haddockHook defaultUserHooks,
                  instHook = filter_modules_hook Always
                           $ instHook defaultUserHooks }
          defaultMainWithHooks hooks

type Hook a = PackageDescription -> LocalBuildInfo -> UserHooks -> a -> IO ()

build_primitive_sources :: Hook a -> Hook a
build_primitive_sources f pd lbi uhs x
 = do when (compilerFlavor (compiler lbi) == GHC) $ do
          let genprimopcode = joinPath ["..", "..", "utils",
                                        "genprimopcode", "genprimopcode"]
              primops = joinPath ["..", "..", "compiler", "prelude",
                                  "primops.txt"]
              primhs = joinPath ["GHC", "Prim.hs"]
              primopwrappers = joinPath ["GHC", "PrimopWrappers.hs"]
          maybeExit $ system (genprimopcode ++ " --make-haskell-source < "
                           ++ primops ++ " > " ++ primhs)
          maybeExit $ system (genprimopcode ++ " --make-haskell-wrappers < "
                           ++ primops ++ " > " ++ primopwrappers)
      f pd lbi uhs x

filter_modules_hook :: When -> Hook a -> Hook a
filter_modules_hook w f pd lbi uhs x
 = let newHaddock = case lookupProgram haddockProgram (withPrograms lbi) of
                    Just haddockProg ->
                        case programVersion haddockProg of
                        Just v -> v >= Version [2,0] []
                        Nothing -> False
                    Nothing -> False
       lib = case library pd of
                 Just l -> l
                 Nothing -> error "Expected a library"
       lib' = let ems = filter ("GHC.Prim" /=) (exposedModules lib)
              in lib { exposedModules = ems }
       lib'' = if (w == Always) || ((w == NewHaddock) && newHaddock)
               then lib'
               else lib
       pd' = pd { library = Just lib'' }
   in f pd' lbi uhs x

