*** ghc-pristine/libraries/base/Prelude.hs	2007-01-05 10:42:21.000000000 -0800
--- ghc-xen/libraries/base/Prelude.hs	2007-01-10 14:06:07.000000000 -0800
***************
*** 123,128 ****
--- 123,129 ----
      
      -- * Basic Input and output
      IO,
+ #ifndef xen_HOST_OS
      -- ** Simple I\/O operations
      -- All I/O functions defined here are character oriented.  The
      -- treatment of the newline character will vary on different systems.
***************
*** 138,143 ****
--- 139,145 ----
      -- *** Files
      FilePath,
      readFile, writeFile, appendFile, readIO, readLn,
+ #endif
      -- ** Exception handling in the I\/O monad
      IOError, ioError, userError, catch
  
