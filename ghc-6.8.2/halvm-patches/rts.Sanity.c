*** ghc-pristine/rts/Sanity.c	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/rts/Sanity.c	2007-11-05 17:58:48.000000000 -0800
***************
*** 16,22 ****
  #include "PosixSource.h"
  #include "Rts.h"
  
! #ifdef DEBUG                                                   /* whole file */
  
  #include "RtsFlags.h"
  #include "RtsUtils.h"
--- 16,22 ----
  #include "PosixSource.h"
  #include "Rts.h"
  
! #if defined(DEBUG) || defined(PERFORM_SANITY_CHECKS)            /* whole file */
  
  #include "RtsFlags.h"
  #include "RtsUtils.h"
***************
*** 145,150 ****
--- 145,151 ----
  			 BITMAP_BITS(info->i.layout.bitmap), size);
  	return 1 + size;
  
+ #ifdef ALLOW_INTERPRETER
      case RET_BCO: {
  	StgBCO *bco;
  	nat size;
***************
*** 153,158 ****
--- 154,160 ----
  	checkLargeBitmap((StgPtr)c + 2, BCO_BITMAP(bco), size);
  	return 2 + size;
      }
+ #endif // ALLOW_INTERPRETER
  
      case RET_BIG: // large bitmap (> 32 entries)
  	size = GET_LARGE_BITMAP(&info->i)->size;
***************
*** 841,848 ****
        break;
  
      default:
!       barf("checkStaticObjetcs: strange closure %p (%s)", 
! 	   p, info_type(p));
      }
    }
  }
--- 843,849 ----
        break;
  
      default:
!       barf("checkStaticObjects: strange closure %p", p);
      }
    }
  }
***************
*** 878,884 ****
  
      default:
        barf("checkBQ: strange closure %d in blocking queue for closure %p (%s)\n", 
! 	   get_itbl(bqe)->type, closure, info_type(closure));
      }
    } while (!end);
  }
--- 879,885 ----
  
      default:
        barf("checkBQ: strange closure %d in blocking queue for closure %p (%s)\n", 
! 	   get_itbl(bqe)->type, closure, "");
      }
    } while (!end);
  }
***************
*** 902,908 ****
      
      default:
        barf("checkBQ: strange closure %d in blocking queue for closure %p (%s)\n", 
! 	   get_itbl(bqe)->type, closure, info_type(closure));
      }
    } while (!end);
  }
--- 903,909 ----
      
      default:
        barf("checkBQ: strange closure %d in blocking queue for closure %p (%s)\n", 
! 	   get_itbl(bqe)->type, closure, "");
      }
    } while (!end);
  }
