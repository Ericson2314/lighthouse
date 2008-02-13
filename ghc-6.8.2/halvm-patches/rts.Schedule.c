*** ghc-pristine/rts/Schedule.c	2007-11-01 19:55:14.000000000 -0700
--- ghc-xen/rts/Schedule.c	2007-11-05 17:59:10.000000000 -0800
***************
*** 171,177 ****
  rtsBool emitSchedule = rtsTrue;
  #endif
  
! #if !defined(mingw32_HOST_OS)
  #define FORKPROCESS_PRIMOP_SUPPORTED
  #endif
  
--- 171,178 ----
  rtsBool emitSchedule = rtsTrue;
  #endif
  
! #if !defined(mingw32_HOST_OS) && !defined(xen_HOST_OS)
! #error "FOO!"
  #define FORKPROCESS_PRIMOP_SUPPORTED
  #endif
  
***************
*** 623,633 ****
  	ret = r->rRet;
  	break;
      }
!     
      case ThreadInterpret:
  	cap = interpretBCO(cap);
  	ret = cap->r.rRet;
  	break;
  	
      default:
  	barf("schedule: invalid what_next field");
--- 624,636 ----
  	ret = r->rRet;
  	break;
      }
!    
! #ifdef ALLOW_INTERPRETER
      case ThreadInterpret:
  	cap = interpretBCO(cap);
  	ret = cap->r.rRet;
  	break;
+ #endif
  	
      default:
  	barf("schedule: invalid what_next field");
***************
*** 731,737 ****
  schedulePreLoop(void)
  {
  #if defined(GRAN) 
-     /* set up first event to get things going */
      /* ToDo: assign costs for system setup and init MainTSO ! */
      new_event(CurrentProc, CurrentProc, CurrentTime[CurrentProc],
  	      ContinueThread, 
--- 734,739 ----
***************
*** 865,871 ****
  /* ----------------------------------------------------------------------------
   * Start any pending signal handlers
   * ------------------------------------------------------------------------- */
- 
  #if defined(RTS_USER_SIGNALS) && !defined(THREADED_RTS)
  static void
  scheduleStartSignalHandlers(Capability *cap)
--- 867,872 ----
***************
*** 1011,1017 ****
  	}
  #endif
  
! #if !defined(THREADED_RTS)
  	/* Probably a real deadlock.  Send the current main thread the
  	 * Deadlock exception.
  	 */
--- 1012,1018 ----
  	}
  #endif
  
! #if !defined(THREADED_RTS) && !defined(xen_HOST_OS)
  	/* Probably a real deadlock.  Send the current main thread the
  	 * Deadlock exception.
  	 */
