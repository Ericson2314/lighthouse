#!/usr/bin/env runhaskell

> -- I usually compile this with "ghc --make -o setup Setup.hs"

> module Main (main) where
>
> import Distribution.Simple
>
> main :: IO ()
> main = defaultMainWithHooks defaultUserHooks

