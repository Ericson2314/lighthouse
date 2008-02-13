*** ghc-pristine/utils/genprimopcode/Main.hs	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/utils/genprimopcode/Main.hs	2007-11-05 18:04:08.000000000 -0800
***************
*** 480,486 ****
--- 480,488 ----
  ppType (TyApp "RealWorld"   []) = "realWorldTy"
  ppType (TyApp "ThreadId#"   []) = "threadIdPrimTy"
  ppType (TyApp "ForeignObj#" []) = "foreignObjPrimTy"
+ #ifdef ALLOW_INTERPRETER
  ppType (TyApp "BCO#"        []) = "bcoPrimTy"
+ #endif
  ppType (TyApp "()"          []) = "unitTy" 	-- unitTy is TysWiredIn's name for ()
  
  ppType (TyVar "a")               = "alphaTy"
