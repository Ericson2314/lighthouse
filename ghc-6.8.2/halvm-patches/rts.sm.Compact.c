*** ghc-pristine/rts/sm/Compact.c	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/rts/sm/Compact.c	2007-11-05 17:38:00.000000000 -0800
***************
*** 348,353 ****
--- 348,354 ----
  	    }
  	    continue;
  
+ #ifdef ALLOW_INTERPRETER
  	case RET_BCO: {
  	    StgBCO *bco;
  	    nat size;
***************
*** 361,366 ****
--- 362,368 ----
  	    p += size;
  	    continue;
  	}
+ #endif // ALLOW_INTERPRETER
  
  	    // large bitmap (> 32 entries, or 64 on a 64-bit machine) 
  	case RET_BIG:
***************
*** 411,420 ****
--- 413,424 ----
  	thread_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size);
  	p += size;
  	break;
+ #ifdef ALLOW_INTERPRETER
      case ARG_BCO:
  	thread_large_bitmap((StgPtr)payload, BCO_BITMAP(fun), size);
  	p += size;
  	break;
+ #endif // ALLOW_INTERPRETER
      default:
  	bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
      small_bitmap:
***************
*** 591,596 ****
--- 595,601 ----
  	thread(&((StgClosure *)p)->payload[1]);
  	return p + sizeofW(StgHeader) + 2;
  	
+ #ifdef ALLOW_INTERPRETER
      case BCO: {
  	StgBCO *bco = (StgBCO *)p;
  	thread_(&bco->instrs);
***************
*** 598,603 ****
--- 603,609 ----
  	thread_(&bco->ptrs);
  	return p + bco_sizeW(bco);
      }
+ #endif // ALLOW_INTERPRETER
  
      case THUNK:
      {
