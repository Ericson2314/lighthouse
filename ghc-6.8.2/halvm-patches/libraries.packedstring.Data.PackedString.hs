*** ghc-pristine/libraries/packedstring/Data/PackedString.hs	2007-12-10 10:19:35.000000000 -0800
--- ghc-xen/libraries/packedstring/Data/PackedString.hs	2008-01-02 14:27:14.000000000 -0800
***************
*** 30,36 ****
  	packString,  -- :: String -> PackedString
  	unpackPS,    -- :: PackedString -> String
  
! #ifndef __NHC__
  	-- * I\/O with @PackedString@s	
  	hPutPS,      -- :: Handle -> PackedString -> IO ()
  	hGetPS,      -- :: Handle -> Int -> IO PackedString
--- 30,36 ----
  	packString,  -- :: String -> PackedString
  	unpackPS,    -- :: PackedString -> String
  
! #if !defined(__NHC__) && !defined(xen_HOST_OS)
  	-- * I\/O with @PackedString@s	
  	hPutPS,      -- :: Handle -> PackedString -> IO ()
  	hGetPS,      -- :: Handle -> Int -> IO PackedString
***************
*** 318,323 ****
--- 318,324 ----
  substrPS :: PackedString -> Int -> Int -> PackedString
  substrPS (PS ps) begin end = packString [ ps ! i | i <- [begin..end] ]
  
+ #ifndef xen_HOST_OS
  -- -----------------------------------------------------------------------------
  -- hPutPS
  
***************
*** 348,353 ****
--- 349,355 ----
    l <- hGetArray h arr i
    chars <- mapM (\i -> readArray arr i >>= return.chr.fromIntegral) [0..l-1]
    return (packNChars l chars)
+ #endif
  
  #else	/* __NHC__ */
  
