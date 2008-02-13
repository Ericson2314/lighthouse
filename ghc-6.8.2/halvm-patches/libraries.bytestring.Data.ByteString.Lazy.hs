*** ghc-pristine/libraries/bytestring/Data/ByteString/Lazy.hs	2007-11-01 19:58:53.000000000 -0700
--- ghc-xen/libraries/bytestring/Data/ByteString/Lazy.hs	2007-11-14 18:17:04.000000000 -0800
***************
*** 173,178 ****
--- 173,179 ----
          copy,                   -- :: ByteString -> ByteString
  --        defrag,                -- :: ByteString -> ByteString
  
+ #ifndef xen_HOST_OS
          -- * I\/O with 'ByteString's
  
          -- ** Standard input and output
***************
*** 196,202 ****
  --      hGetN,                  -- :: Int -> Handle -> Int -> IO ByteString
  --      hGetContentsN,          -- :: Int -> Handle -> IO ByteString
  --      hGetNonBlockingN,       -- :: Int -> Handle -> IO ByteString
! 
          -- undocumented deprecated things:
          join                    -- :: ByteString -> [ByteString] -> ByteString
  
--- 197,203 ----
  --      hGetN,                  -- :: Int -> Handle -> Int -> IO ByteString
  --      hGetContentsN,          -- :: Int -> Handle -> IO ByteString
  --      hGetNonBlockingN,       -- :: Int -> Handle -> IO ByteString
! #endif
          -- undocumented deprecated things:
          join                    -- :: ByteString -> [ByteString] -> ByteString
  
***************
*** 207,214 ****
      (reverse,head,tail,last,init,null,length,map,lines,foldl,foldr,unlines
      ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,elem,filter,maximum
      ,minimum,all,concatMap,foldl1,foldr1,scanl, scanl1, scanr, scanr1
!     ,repeat, cycle, interact, iterate,readFile,writeFile,appendFile,replicate
!     ,getContents,getLine,putStr,putStrLn ,zip,zipWith,unzip,notElem)
  
  import qualified Data.List              as L  -- L for list/lazy
  import qualified Data.ByteString        as S  -- S for strict (hmm...)
--- 208,219 ----
      (reverse,head,tail,last,init,null,length,map,lines,foldl,foldr,unlines
      ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,elem,filter,maximum
      ,minimum,all,concatMap,foldl1,foldr1,scanl, scanl1, scanr, scanr1
!     ,repeat, cycle, replicate, iterate
! #ifndef xen_HOST_OS
!     ,readFile,writeFile,appendFile,interact
!     ,getContents,getLine,putStr,putStrLn
! #endif
!     ,zip,zipWith,unzip,notElem)
  
  import qualified Data.List              as L  -- L for list/lazy
  import qualified Data.ByteString        as S  -- S for strict (hmm...)
***************
*** 221,228 ****
--- 226,235 ----
  
  import Data.Word                (Word8)
  import Data.Int                 (Int64)
+ #ifndef xen_HOST_OS
  import System.IO                (Handle,stdin,stdout,openBinaryFile,IOMode(..)
                                  ,hClose,hWaitForInput,hIsEOF)
+ #endif
  import System.IO.Unsafe
  #ifndef __NHC__
  import Control.Exception        (bracket)
***************
*** 1133,1138 ****
--- 1140,1147 ----
  -- TODO defrag func that concatenates block together that are below a threshold
  -- defrag :: ByteString -> ByteString
  
+ #ifndef xen_HOST_OS
+ 
  -- ---------------------------------------------------------------------
  -- Lazy ByteString IO
  
***************
*** 1251,1256 ****
--- 1260,1267 ----
  interact :: (ByteString -> ByteString) -> IO ()
  interact transformer = putStr . transformer =<< getContents
  
+ #endif
+ 
  -- ---------------------------------------------------------------------
  -- Internal utilities
  
