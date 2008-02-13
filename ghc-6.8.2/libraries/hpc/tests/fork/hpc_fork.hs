module Main where

import System.Posix.Process

main = do
  pid1 <- forkProcess $ do print "Hello"
  pid2 <- forkProcess $ do print "World"
  print ()

