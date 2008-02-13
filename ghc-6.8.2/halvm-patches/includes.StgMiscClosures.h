*** ghc-pristine/includes/StgMiscClosures.h	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/includes/StgMiscClosures.h	2007-11-05 16:50:20.000000000 -0800
***************
*** 94,100 ****
--- 94,102 ----
  #if defined(PAR)
  RTS_INFO(stg_FETCH_ME_BQ_info);
  #endif
+ #ifdef ALLOW_INTERPRETER
  RTS_FUN_INFO(stg_BCO_info);
+ #endif // ALLOW_INTERPRETER
  RTS_INFO(stg_EVACUATED_info);
  RTS_INFO(stg_WEAK_info);
  RTS_INFO(stg_DEAD_WEAK_info);
***************
*** 151,157 ****
--- 153,161 ----
  #if defined(PAR)
  RTS_ENTRY(stg_FETCH_ME_BQ_entry);
  #endif
+ #ifdef ALLOW_INTERPRETER
  RTS_ENTRY(stg_BCO_entry);
+ #endif // ALLOW_INTERPRETER
  RTS_ENTRY(stg_EVACUATED_entry);
  RTS_ENTRY(stg_WEAK_entry);
  RTS_ENTRY(stg_DEAD_WEAK_entry);
***************
*** 579,586 ****
--- 583,592 ----
  RTS_FUN(finalizzeWeakzh_fast);
  RTS_FUN(deRefWeakzh_fast);
  
+ #ifdef ALLOW_INTERPRETER
  RTS_FUN(newBCOzh_fast);
  RTS_FUN(mkApUpd0zh_fast);
+ #endif // ALLOW_INTERPRETER
  
  RTS_FUN(retryzh_fast);
  RTS_FUN(catchRetryzh_fast);
