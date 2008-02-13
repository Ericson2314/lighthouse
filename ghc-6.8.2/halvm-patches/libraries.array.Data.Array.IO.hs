*** ghc-pristine/libraries/array/Data/Array/IO.hs	2007-11-01 19:58:24.000000000 -0700
--- ghc-xen/libraries/array/Data/Array/IO.hs	2007-11-14 17:57:35.000000000 -0800
***************
*** 24,32 ****
--- 24,34 ----
     -- * Overloaded mutable array interface
     module Data.Array.MArray,
  
+ #ifndef xen_HOST_OS
     -- * Doing I\/O with @IOUArray@s
     hGetArray,		-- :: Handle -> IOUArray Int Word8 -> Int -> IO Int
     hPutArray,		-- :: Handle -> IOUArray Int Word8 -> Int -> IO ()
+ #endif
   ) where
  
  import Prelude
***************
*** 44,50 ****
--- 46,54 ----
  
  import GHC.Arr
  import GHC.IOBase
+ #ifndef xen_HOST_OS
  import GHC.Handle
+ #endif
  #else
  import Data.Char
  import System.IO
***************
*** 117,122 ****
--- 121,127 ----
  -- ---------------------------------------------------------------------------
  -- hGetArray
  
+ #ifndef xen_HOST_OS
  -- | Reads a number of 'Word8's from the specified 'Handle' directly
  -- into an array.
  hGetArray
***************
*** 208,213 ****
--- 213,219 ----
  				    bufRPtr=0, bufWPtr=count, bufSize=count }
  		    flushWriteBuffer fd stream this_buf
  		    return ()
+ #endif
  
  -- ---------------------------------------------------------------------------
  -- Internal Utils
***************
*** 217,228 ****
--- 223,236 ----
  foreign import ccall unsafe "__hscore_memcpy_src_off"
     memcpy_ba_baoff :: RawBuffer -> RawBuffer -> CInt -> CSize -> IO (Ptr ())
  
+ #ifndef xen_HOST_OS
  illegalBufferSize :: Handle -> String -> Int -> IO a
  illegalBufferSize handle fn sz = 
  	ioException (IOError (Just handle)
  			    InvalidArgument  fn
  			    ("illegal buffer size " ++ showsPrec 9 (sz::Int) [])
  			    Nothing)
+ #endif
  
  #else /* !__GLASGOW_HASKELL__ */
  hGetArray :: Handle -> IOUArray Int Word8 -> Int -> IO Int
