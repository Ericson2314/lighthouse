*** ghc-pristine/includes/Storage.h	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/includes/Storage.h	2007-11-05 16:53:10.000000000 -0800
***************
*** 359,366 ****
--- 359,368 ----
  INLINE_HEADER StgWord tso_sizeW ( StgTSO *tso )
  { return TSO_STRUCT_SIZEW + tso->stack_size; }
  
+ #ifdef ALLOW_INTERPRETER
  INLINE_HEADER StgWord bco_sizeW ( StgBCO *bco )
  { return bco->size; }
+ #endif // ALLOW_INTERPRETER
  
  INLINE_HEADER nat
  closure_sizeW_ (StgClosure *p, StgInfoTable *info)
***************
*** 409,416 ****
--- 411,420 ----
  	return mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
      case TSO:
  	return tso_sizeW((StgTSO *)p);
+ #ifdef ALLOW_INTERPRETER
      case BCO:
  	return bco_sizeW((StgBCO *)p);
+ #endif
      case TVAR_WATCH_QUEUE:
          return sizeofW(StgTVarWatchQueue);
      case TVAR:
***************
*** 460,467 ****
--- 464,473 ----
      case RET_BIG:
  	return 1 + GET_LARGE_BITMAP(&info->i)->size;
  
+ #ifdef ALLOW_INTERPRETER
      case RET_BCO:
  	return 2 + BCO_BITMAP_SIZE((StgBCO *)((P_)frame)[1]);
+ #endif
  
      default:
  	return 1 + BITMAP_SIZE(info->i.layout.bitmap);
