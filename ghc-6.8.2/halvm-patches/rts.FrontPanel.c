*** ghc-pristine/rts/FrontPanel.c	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/rts/FrontPanel.c	2007-11-05 17:27:25.000000000 -0800
***************
*** 620,626 ****
--- 620,628 ----
  		    switch (info->type) {
  
  		    case CONSTR:
+ #ifdef ALLOW_INTERPRETER
  		    case BCO:
+ #endif // ALLOW_INTERPRETER
  			if (((StgClosure *)p)->header.info == &stg_DEAD_WEAK_info) {
  			    size = sizeofW(StgWeak);
  			    type = Other;
