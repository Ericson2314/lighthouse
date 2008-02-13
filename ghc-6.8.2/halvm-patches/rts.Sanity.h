*** ghc-pristine/rts/Sanity.h	2007-04-25 10:10:40.000000000 -0700
--- ghc-xen/rts/Sanity.h	2007-09-06 12:56:24.000000000 -0700
***************
*** 8,14 ****
  
  #ifndef SANITY_H
  
! #ifdef DEBUG
  
  # if defined(PAR)
  # define PVM_PE_MASK    0xfffc0000
--- 8,14 ----
  
  #ifndef SANITY_H
  
! #if defined(DEBUG) || defined(PERFORM_SANITY_CHECKS)
  
  # if defined(PAR)
  # define PVM_PE_MASK    0xfffc0000
