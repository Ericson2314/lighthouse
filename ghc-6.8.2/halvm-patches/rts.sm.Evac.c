*** ghc-pristine/rts/sm/Evac.c	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/rts/sm/Evac.c	2007-11-05 17:35:33.000000000 -0800
***************
*** 513,520 ****
--- 513,522 ----
    case CONSTR:
      return copy_tag(q,sizeW_fromITBL(info),stp,tag);
  
+ #ifdef ALLOW_INTERPRETER
    case BCO:
      return copy(q,bco_sizeW((StgBCO *)q),stp);
+ #endif
  
    case CAF_BLACKHOLE:
    case SE_CAF_BLACKHOLE:
***************
*** 576,582 ****
--- 578,586 ----
      q = ((StgInd*)q)->indirectee;
      goto loop;
  
+ #ifdef ALLOW_INTERPRETER
    case RET_BCO:
+ #endif
    case RET_SMALL:
    case RET_BIG:
    case RET_DYN:
