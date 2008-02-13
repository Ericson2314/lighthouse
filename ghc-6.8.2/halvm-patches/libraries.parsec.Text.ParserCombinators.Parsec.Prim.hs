*** ghc-pristine/libraries/parsec/Text/ParserCombinators/Parsec/Prim.hs	2007-01-05 10:42:25.000000000 -0800
--- ghc-xen/libraries/parsec/Text/ParserCombinators/Parsec/Prim.hs	2007-01-10 15:21:38.000000000 -0800
***************
*** 18,24 ****
  
                     -- basic types
                     , Parser, GenParser
!                    , runParser, parse, parseFromFile, parseTest
                     
                     -- primitive parsers:
                     -- instance Functor Parser     : fmap
--- 18,27 ----
  
                     -- basic types
                     , Parser, GenParser
!                    , runParser, parse
! #ifndef xen_HOST_OS
!                    , parseFromFile, parseTest
! #endif
                     
                     -- primitive parsers:
                     -- instance Functor Parser     : fmap
***************
*** 141,146 ****
--- 144,150 ----
  -----------------------------------------------------------
  -- run a parser
  -----------------------------------------------------------
+ #ifndef xen_HOST_OS
  parseFromFile :: Parser a -> SourceName -> IO (Either ParseError a)
  parseFromFile p fname
      = do{ input <- readFile fname
***************
*** 154,160 ****
                        ; print err
                        }
          Right x  -> print x
! 
  
  parse :: GenParser tok () a -> SourceName -> [tok] -> Either ParseError a
  parse p name input
--- 158,164 ----
                        ; print err
                        }
          Right x  -> print x
! #endif
  
  parse :: GenParser tok () a -> SourceName -> [tok] -> Either ParseError a
  parse p name input
