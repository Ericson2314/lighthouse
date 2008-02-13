module Main (main) where

import Graphics.UI.ObjectIO

main :: IO ()
main = startIO NDI () (openDialog undefined hello) []
    where hello	= Dialog "" (TextControl "Hello world!" []) [WindowClose (noLS closeProcess)]