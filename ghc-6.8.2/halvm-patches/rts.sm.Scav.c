*** ghc-pristine/rts/sm/Scav.c	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/rts/sm/Scav.c	2007-11-05 17:37:15.000000000 -0800
***************
*** 212,221 ****
--- 212,223 ----
  	scavenge_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
  	p += size;
  	break;
+ #ifdef ALLOW_INTERPRETER
      case ARG_BCO:
  	scavenge_large_bitmap((StgPtr)payload, BCO_BITMAP(fun), size);
  	p += size;
  	break;
+ #endif
      default:
  	bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
      small_bitmap:
***************
*** 406,411 ****
--- 408,414 ----
  	break;
      }
  
+ #ifdef ALLOW_INTERPRETER
      case BCO: {
  	StgBCO *bco = (StgBCO *)p;
  	bco->instrs = (StgArrWords *)evacuate((StgClosure *)bco->instrs);
***************
*** 414,419 ****
--- 417,423 ----
  	p += bco_sizeW(bco);
  	break;
      }
+ #endif
  
      case IND_PERM:
        if (stp->gen->no != 0) {
***************
*** 786,791 ****
--- 790,796 ----
  	    break;
  	}
  
+ #ifdef ALLOW_INTERPRETER
  	case BCO: {
  	    StgBCO *bco = (StgBCO *)p;
  	    bco->instrs = (StgArrWords *)evacuate((StgClosure *)bco->instrs);
***************
*** 793,798 ****
--- 798,804 ----
  	    bco->ptrs = (StgMutArrPtrs *)evacuate((StgClosure *)bco->ptrs);
  	    break;
  	}
+ #endif 
  
  	case IND_PERM:
  	    // don't need to do anything here: the only possible case
***************
*** 1661,1666 ****
--- 1667,1673 ----
  	    scavenge_srt((StgClosure **)GET_SRT(info), info->i.srt_bitmap);
  	continue;
  
+ #ifdef ALLOW_INTERPRETER
      case RET_BCO: {
  	StgBCO *bco;
  	nat size;
***************
*** 1674,1679 ****
--- 1681,1687 ----
  	p += size;
  	continue;
      }
+ #endif
  
        // large bitmap (> 32 entries, or > 64 on a 64-bit machine) 
      case RET_BIG:
