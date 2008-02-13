*** ghc-pristine/libraries/bytestring/Data/ByteString/Lazy/Char8.hs	2007-11-01 19:58:53.000000000 -0700
--- ghc-xen/libraries/bytestring/Data/ByteString/Lazy/Char8.hs	2007-11-14 18:17:46.000000000 -0800
***************
*** 155,160 ****
--- 155,162 ----
          readInt,
          readInteger,
  
+ #ifndef xen_HOST_OS
+ 
          -- * I\/O with 'ByteString's
  
          -- ** Standard input and output
***************
*** 177,183 ****
  --      hGetN,                  -- :: Int -> Handle -> Int64 -> IO ByteString
  --      hGetContentsN,          -- :: Int -> Handle -> IO ByteString
  --      hGetNonBlockingN,       -- :: Int -> Handle -> IO ByteString
! 
          -- undocumented deprecated things:
          join                    -- :: ByteString -> [ByteString] -> ByteString
  
--- 179,185 ----
  --      hGetN,                  -- :: Int -> Handle -> Int64 -> IO ByteString
  --      hGetContentsN,          -- :: Int -> Handle -> IO ByteString
  --      hGetNonBlockingN,       -- :: Int -> Handle -> IO ByteString
! #endif
          -- undocumented deprecated things:
          join                    -- :: ByteString -> [ByteString] -> ByteString
  
***************
*** 188,196 ****
          (ByteString, fromChunks, toChunks
          ,empty,null,length,tail,init,append,reverse,transpose,cycle
          ,concat,take,drop,splitAt,intercalate,isPrefixOf,group,inits,tails,copy
          ,hGetContents, hGet, hPut, getContents
          ,hGetNonBlocking
!         ,putStr, putStrLn, interact)
  
  -- Functions we need to wrap.
  import qualified Data.ByteString.Lazy as L
--- 190,201 ----
          (ByteString, fromChunks, toChunks
          ,empty,null,length,tail,init,append,reverse,transpose,cycle
          ,concat,take,drop,splitAt,intercalate,isPrefixOf,group,inits,tails,copy
+ #ifndef xen_HOST_OS
          ,hGetContents, hGet, hPut, getContents
          ,hGetNonBlocking
!         ,putStr, putStrLn, interact
! #endif
!         )
  
  -- Functions we need to wrap.
  import qualified Data.ByteString.Lazy as L
***************
*** 208,217 ****
          (reverse,head,tail,last,init,null,length,map,lines,foldl,foldr,unlines
          ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,elem,filter
          ,unwords,words,maximum,minimum,all,concatMap,scanl,scanl1,foldl1,foldr1
!         ,readFile,writeFile,appendFile,replicate,getContents,getLine,putStr,putStrLn
!         ,zip,zipWith,unzip,notElem,repeat,iterate,interact,cycle)
  
  import System.IO            (hClose,openFile,IOMode(..))
  #ifndef __NHC__
  import Control.Exception    (bracket)
  #else
--- 213,226 ----
          (reverse,head,tail,last,init,null,length,map,lines,foldl,foldr,unlines
          ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,elem,filter
          ,unwords,words,maximum,minimum,all,concatMap,scanl,scanl1,foldl1,foldr1
! #ifndef xen_HOST_OS
!         ,readFile,writeFile,appendFile,getContents,getLine,putStr,putStrLn,interact
! #endif
!         ,replicate,zip,zipWith,unzip,notElem,repeat,iterate,cycle)
  
+ #ifndef xen_HOST_OS
  import System.IO            (hClose,openFile,IOMode(..))
+ #endif
  #ifndef __NHC__
  import Control.Exception    (bracket)
  #else
***************
*** 775,780 ****
--- 784,790 ----
            end n c cs = let c' = chunk c cs
                          in c' `seq` (n, c')
  
+ #ifndef xen_HOST_OS
  -- | Read an entire file /lazily/ into a 'ByteString'. Use 'text mode'
  -- on Windows to interpret newlines
  readFile :: FilePath -> IO ByteString
***************
*** 789,795 ****
  appendFile :: FilePath -> ByteString -> IO ()
  appendFile f txt = bracket (openFile f AppendMode) hClose
      (\hdl -> hPut hdl txt)
! 
  
  -- ---------------------------------------------------------------------
  -- Internal utilities
--- 799,805 ----
  appendFile :: FilePath -> ByteString -> IO ()
  appendFile f txt = bracket (openFile f AppendMode) hClose
      (\hdl -> hPut hdl txt)
! #endif
  
  -- ---------------------------------------------------------------------
  -- Internal utilities
