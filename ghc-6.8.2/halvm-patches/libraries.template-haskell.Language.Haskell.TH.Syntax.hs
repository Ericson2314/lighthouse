*** ghc-pristine/libraries/template-haskell/Language/Haskell/TH/Syntax.hs	2007-11-01 20:00:08.000000000 -0700
--- ghc-xen/libraries/template-haskell/Language/Haskell/TH/Syntax.hs	2007-11-05 17:20:43.000000000 -0800
***************
*** 1,4 ****
! {-# OPTIONS_GHC -fglasgow-exts #-}
  	-- Need GlaExts for the nested forall in defn of Q,
  	-- and the deriving Data, Typeable
  {-# OPTIONS_GHC -w #-}
--- 1,4 ----
! {-# OPTIONS_GHC -fglasgow-exts -cpp #-}
  	-- Need GlaExts for the nested forall in defn of Q,
  	-- and the deriving Data, Typeable
  {-# OPTIONS_GHC -w #-}
***************
*** 59,65 ****
--- 59,67 ----
  import Data.IORef
  import GHC.IOBase	( unsafePerformIO )
  import Control.Monad (liftM)
+ #ifndef xen_HOST_OS
  import System.IO	( hPutStrLn, stderr )
+ #endif
  import Data.Char        ( isAlpha )
  
  -----------------------------------------------------
***************
*** 102,109 ****
--- 104,116 ----
                   ; writeIORef counter (n+1)
                   ; return (mkNameU s n) }
  
+ #ifdef xen_HOST_OS
+   qReport True  msg = fail ("Template Haskell error: " ++ msg)
+   qReport False msg = fail ("Template Haskell error: " ++ msg)
+ #else
    qReport True  msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)
    qReport False msg = hPutStrLn stderr ("Template Haskell error: " ++ msg)
+ #endif
  
    qReify v       = badIO "reify"
    qCurrentModule = badIO "currentModule"
