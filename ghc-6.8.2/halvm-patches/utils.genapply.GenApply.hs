*** ghc-pristine/utils/genapply/GenApply.hs	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/utils/genapply/GenApply.hs	2007-11-05 18:03:38.000000000 -0800
***************
*** 511,516 ****
--- 511,517 ----
          text "switch [INVALID_OBJECT .. N_CLOSURE_TYPES] (TO_W_(%INFO_TYPE(%STD_INFO(info)))) {",
  	nest 4 (vcat [
  
+ #ifdef ALLOW_INTERPRETER
  --    if fast == 1:
  --        print "    bco_lbl:"
  --    else:
***************
*** 523,528 ****
--- 524,530 ----
  		args all_args_size fun_info_label {- tag stmt -}False
  	 ]),
  	text "}",
+ #endif
  
  --    if fast == 1:
  --        print "    fun_lbl:"
