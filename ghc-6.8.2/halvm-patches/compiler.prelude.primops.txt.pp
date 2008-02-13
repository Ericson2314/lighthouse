*** ghc-pristine/compiler/prelude/primops.txt.pp	2007-11-01 19:55:12.000000000 -0700
--- ghc-xen/compiler/prelude/primops.txt.pp	2007-11-05 16:42:41.000000000 -0800
***************
*** 1663,1676 ****
  section "Bytecode operations" 
  	{Support for the bytecode interpreter and linker.}
  ------------------------------------------------------------------------
! 
  primtype BCO#
     {Primitive bytecode type.}
! 
  primop   AddrToHValueOp "addrToHValue#" GenPrimOp
     Addr# -> (# a #)
     {Convert an {\tt Addr\#} to a followable type.}
  
  primop   MkApUpd0_Op "mkApUpd0#" GenPrimOp
     BCO# -> (# a #)
     with
--- 1663,1677 ----
  section "Bytecode operations" 
  	{Support for the bytecode interpreter and linker.}
  ------------------------------------------------------------------------
! #ifdef ALLOW_INTERPRETER
  primtype BCO#
     {Primitive bytecode type.}
! #endif
  primop   AddrToHValueOp "addrToHValue#" GenPrimOp
     Addr# -> (# a #)
     {Convert an {\tt Addr\#} to a followable type.}
  
+ #ifdef ALLOW_INTERPRETER
  primop   MkApUpd0_Op "mkApUpd0#" GenPrimOp
     BCO# -> (# a #)
     with
***************
*** 1681,1686 ****
--- 1682,1688 ----
     with
     has_side_effects = True
     out_of_line      = True
+ #endif
  
  primop  UnpackClosureOp "unpackClosure#" GenPrimOp
     a -> (# Addr#, Array# b, ByteArr# #)
