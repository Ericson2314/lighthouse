*** ghc-pristine/libraries/bytestring/Data/ByteString/Char8.hs	2007-11-01 19:58:53.000000000 -0700
--- ghc-xen/libraries/bytestring/Data/ByteString/Char8.hs	2007-11-14 18:11:35.000000000 -0800
***************
*** 181,186 ****
--- 181,187 ----
          useAsCString,           -- :: ByteString -> (CString    -> IO a) -> IO a
          useAsCStringLen,        -- :: ByteString -> (CStringLen -> IO a) -> IO a
  
+ #ifndef xen_HOST_OS
          -- * I\/O with 'ByteString's
  
          -- ** Standard input and output
***************
*** 204,210 ****
          hPut,                   -- :: Handle -> ByteString -> IO ()
          hPutStr,                -- :: Handle -> ByteString -> IO ()
          hPutStrLn,              -- :: Handle -> ByteString -> IO ()
! 
          -- undocumented deprecated things:
          join                    -- :: ByteString -> [ByteString] -> ByteString
  
--- 205,211 ----
          hPut,                   -- :: Handle -> ByteString -> IO ()
          hPutStr,                -- :: Handle -> ByteString -> IO ()
          hPutStrLn,              -- :: Handle -> ByteString -> IO ()
! #endif
          -- undocumented deprecated things:
          join                    -- :: ByteString -> [ByteString] -> ByteString
  
***************
*** 217,225 ****
                                  ,dropWhile,span,break,elem,filter,unwords
                                  ,words,maximum,minimum,all,concatMap
                                  ,scanl,scanl1,scanr,scanr1
-                                 ,appendFile,readFile,writeFile
                                  ,foldl1,foldr1,replicate
                                  ,getContents,getLine,putStr,putStrLn,interact
                                  ,zip,zipWith,unzip,notElem)
  
  import qualified Data.ByteString as B
--- 218,228 ----
                                  ,dropWhile,span,break,elem,filter,unwords
                                  ,words,maximum,minimum,all,concatMap
                                  ,scanl,scanl1,scanr,scanr1
                                  ,foldl1,foldr1,replicate
+ #ifndef xen_HOST_OS
+                                 ,appendFile,readFile,writeFile
                                  ,getContents,getLine,putStr,putStrLn,interact
+ #endif
                                  ,zip,zipWith,unzip,notElem)
  
  import qualified Data.ByteString as B
***************
*** 232,241 ****
                         ,concat,take,drop,splitAt,intercalate
                         ,sort,isPrefixOf,isSuffixOf,isInfixOf,isSubstringOf
                         ,findSubstring,findSubstrings,copy,group
! 
                         ,getLine, getContents, putStr, putStrLn, interact
                         ,hGetContents, hGet, hPut, hPutStr, hPutStrLn
                         ,hGetLine, hGetNonBlocking
                         ,packCString,packCStringLen
                         ,useAsCString,useAsCStringLen
                         )
--- 235,245 ----
                         ,concat,take,drop,splitAt,intercalate
                         ,sort,isPrefixOf,isSuffixOf,isInfixOf,isSubstringOf
                         ,findSubstring,findSubstrings,copy,group
! #ifndef xen_HOST_OS
                         ,getLine, getContents, putStr, putStrLn, interact
                         ,hGetContents, hGet, hPut, hPutStr, hPutStrLn
                         ,hGetLine, hGetNonBlocking
+ #endif
                         ,packCString,packCStringLen
                         ,useAsCString,useAsCStringLen
                         )
***************
*** 249,256 ****
  
  import Data.Char    ( isSpace )
  import qualified Data.List as List (intersperse)
! 
  import System.IO                (openFile,hClose,hFileSize,IOMode(..))
  #ifndef __NHC__
  import Control.Exception        (bracket)
  #else
--- 253,261 ----
  
  import Data.Char    ( isSpace )
  import qualified Data.List as List (intersperse)
! #ifndef xen_HOST_OS
  import System.IO                (openFile,hClose,hFileSize,IOMode(..))
+ #endif
  #ifndef __NHC__
  import Control.Exception        (bracket)
  #else
***************
*** 969,974 ****
--- 974,980 ----
            combine2 b (n:m:ns) = let t = m*b + n in t `seq` (t : combine2 b ns)
            combine2 _ ns       = ns
  
+ #ifndef xen_HOST_OS
  -- | Read an entire file strictly into a 'ByteString'.  This is far more
  -- efficient than reading the characters into a 'String' and then using
  -- 'pack'.  It also may be more efficient than opening the file and
***************
*** 986,989 ****
  appendFile :: FilePath -> ByteString -> IO ()
  appendFile f txt = bracket (openFile f AppendMode) hClose
      (\h -> hPut h txt)
! 
--- 992,995 ----
  appendFile :: FilePath -> ByteString -> IO ()
  appendFile f txt = bracket (openFile f AppendMode) hClose
      (\h -> hPut h txt)
! #endif
