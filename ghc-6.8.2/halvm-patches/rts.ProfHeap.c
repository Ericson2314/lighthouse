*** ghc-pristine/rts/ProfHeap.c	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/rts/ProfHeap.c	2007-11-12 16:59:01.000000000 -0800
***************
*** 837,843 ****
  
  	if (count == 0) continue;
  
! #if !defined(PROFILING)
  	switch (RtsFlags.ProfFlags.doHeapProfile) {
  	case HEAP_BY_CLOSURE_TYPE:
  	    fprintf(hp_file, "%s", (char *)ctr->identity);
--- 837,843 ----
  
  	if (count == 0) continue;
  
! #if defined(PROFILING)
  	switch (RtsFlags.ProfFlags.doHeapProfile) {
  	case HEAP_BY_CLOSURE_TYPE:
  	    fprintf(hp_file, "%s", (char *)ctr->identity);
***************
*** 969,978 ****
--- 969,980 ----
  		size = BLACKHOLE_sizeW();
  		break;
  
+ #ifdef ALLOW_INTERPRETER
  	    case BCO:
  		prim = rtsTrue;
  		size = bco_sizeW((StgBCO *)p);
  		break;
+ #endif
  
  	    case MVAR:
  	    case WEAK:
