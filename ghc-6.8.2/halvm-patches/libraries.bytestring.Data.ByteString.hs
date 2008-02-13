*** ghc-pristine/libraries/bytestring/Data/ByteString.hs	2007-11-01 19:58:53.000000000 -0700
--- ghc-xen/libraries/bytestring/Data/ByteString.hs	2007-11-14 18:46:56.000000000 -0800
***************
*** 174,179 ****
--- 174,180 ----
          useAsCString,           -- :: ByteString -> (CString    -> IO a) -> IO a
          useAsCStringLen,        -- :: ByteString -> (CStringLen -> IO a) -> IO a
  
+ #ifndef xen_HOST_OS
          -- * I\/O with 'ByteString's
  
          -- ** Standard input and output
***************
*** 197,203 ****
          hPut,                   -- :: Handle -> ByteString -> IO ()
          hPutStr,                -- :: Handle -> ByteString -> IO ()
          hPutStrLn,              -- :: Handle -> ByteString -> IO ()
! 
          -- undocumented deprecated things:
          join                    -- :: ByteString -> [ByteString] -> ByteString
  
--- 198,204 ----
          hPut,                   -- :: Handle -> ByteString -> IO ()
          hPutStr,                -- :: Handle -> ByteString -> IO ()
          hPutStrLn,              -- :: Handle -> ByteString -> IO ()
! #endif
          -- undocumented deprecated things:
          join                    -- :: ByteString -> [ByteString] -> ByteString
  
***************
*** 209,217 ****
                                  ,concat,any,take,drop,splitAt,takeWhile
                                  ,dropWhile,span,break,elem,filter,maximum
                                  ,minimum,all,concatMap,foldl1,foldr1
!                                 ,scanl,scanl1,scanr,scanr1
!                                 ,readFile,writeFile,appendFile,replicate
                                  ,getContents,getLine,putStr,putStrLn,interact
                                  ,zip,zipWith,unzip,notElem)
  
  import Data.ByteString.Internal
--- 210,220 ----
                                  ,concat,any,take,drop,splitAt,takeWhile
                                  ,dropWhile,span,break,elem,filter,maximum
                                  ,minimum,all,concatMap,foldl1,foldr1
!                                 ,scanl,scanl1,scanr,scanr1,replicate
! #ifndef xen_HOST_OS
!                                 ,readFile,writeFile,appendFile
                                  ,getContents,getLine,putStr,putStrLn,interact
+ #endif
                                  ,zip,zipWith,unzip,notElem)
  
  import Data.ByteString.Internal
***************
*** 243,251 ****
--- 246,256 ----
  import Foreign.Storable         (Storable(..))
  
  -- hGetBuf and hPutBuf not available in yhc or nhc
+ #ifndef xen_HOST_OS
  import System.IO                (stdin,stdout,hClose,hFileSize
                                  ,hGetBuf,hPutBuf,openBinaryFile
                                  ,Handle,IOMode(..))
+ #endif
  
  import Data.Monoid              (Monoid, mempty, mappend, mconcat)
  
***************
*** 257,266 ****
--- 262,275 ----
  
  #if defined(__GLASGOW_HASKELL__)
  
+ #ifndef xen_HOST_OS
  import System.IO                (hGetBufNonBlocking)
+ #endif
  import System.IO.Error          (isEOFError)
  
+ #ifndef xen_HOST_OS
  import GHC.Handle
+ #endif
  import GHC.Prim                 (Word#, (+#), writeWord8OffAddr#)
  import GHC.Base                 (build)
  import GHC.Word hiding (Word8)
***************
*** 1708,1713 ****
--- 1717,1723 ----
  
  -- ---------------------------------------------------------------------
  -- line IO
+ #ifndef xen_HOST_OS
  
  -- | Read a line from stdin.
  getLine :: IO ByteString
***************
*** 1909,1914 ****
--- 1919,1925 ----
  appendFile f txt = bracket (openBinaryFile f AppendMode) hClose
      (\h -> hPut h txt)
  
+ #endif
  {-
  --
  -- Disable until we can move it into a portable .hsc file
