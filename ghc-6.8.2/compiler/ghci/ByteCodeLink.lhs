%
% (c) The University of Glasgow 2000-2006
%
ByteCodeLink: Bytecode assembler and linker

\begin{code}
{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module ByteCodeLink ( 
	HValue, 
	ClosureEnv, emptyClosureEnv, extendClosureEnv,
	linkBCO, lookupStaticPtr, lookupName
       ,lookupIE
  ) where

#include "HsVersions.h"

import ByteCodeItbls
import ByteCodeAsm
import ObjLink

import Name
import NameEnv
import OccName
import PrimOp
import Module
import PackageConfig
import FastString
import Panic
import Outputable

-- Standard libraries
import GHC.Word		( Word(..) )

import Data.Array.Base
import GHC.Arr		( STArray(..) )

import Control.Exception ( throwDyn )
import Control.Monad	( zipWithM )
import Control.Monad.ST ( stToIO )

import GHC.Exts
import GHC.Arr		( Array(..) )
import GHC.IOBase	( IO(..) )
import GHC.Ptr		( Ptr(..), castPtr )
import GHC.Base		( writeArray#, RealWorld, Int(..), Word# )  
\end{code}


%************************************************************************
%*									*
\subsection{Linking interpretables into something we can run}
%*									*
%************************************************************************

\begin{code}
type ClosureEnv = NameEnv (Name, HValue)
newtype HValue = HValue Any

emptyClosureEnv = emptyNameEnv

extendClosureEnv :: ClosureEnv -> [(Name,HValue)] -> ClosureEnv
extendClosureEnv cl_env pairs
  = extendNameEnvList cl_env [ (n, (n,v)) | (n,v) <- pairs]
\end{code}


%************************************************************************
%*									*
\subsection{Linking interpretables into something we can run}
%*									*
%************************************************************************

\begin{code}
{- 
data BCO# = BCO# ByteArray# 		-- instrs   :: Array Word16#
                 ByteArray# 		-- literals :: Array Word32#
                 PtrArray# 		-- ptrs     :: Array HValue
                 ByteArray#		-- itbls    :: Array Addr#
-}

linkBCO :: ItblEnv -> ClosureEnv -> UnlinkedBCO -> IO HValue
linkBCO ie ce ul_bco
   = do BCO bco# <- linkBCO' ie ce ul_bco
	-- SDM: Why do we need mkApUpd0 here?  I *think* it's because
	-- otherwise top-level interpreted CAFs don't get updated 
 	-- after evaluation.   A top-level BCO will evaluate itself and
	-- return its value when entered, but it won't update itself.
	-- Wrapping the BCO in an AP_UPD thunk will take care of the
	-- update for us.
	--
	-- Update: the above is true, but now we also have extra invariants:
	--   (a) An AP thunk *must* point directly to a BCO
	--   (b) A zero-arity BCO *must* be wrapped in an AP thunk
	--   (c) An AP is always fully saturated, so we *can't* wrap
	--       non-zero arity BCOs in an AP thunk.
	-- 
	if (unlinkedBCOArity ul_bco > 0) 
	   then return (unsafeCoerce# bco#)
	   else case mkApUpd0# bco# of { (# final_bco #) -> return final_bco }


linkBCO' :: ItblEnv -> ClosureEnv -> UnlinkedBCO -> IO BCO
linkBCO' ie ce (UnlinkedBCO nm arity insns_barr bitmap literalsSS ptrsSS)
   -- Raises an IO exception on failure
   = do let literals = ssElts literalsSS
	    ptrs     = ssElts ptrsSS

        linked_literals <- mapM (lookupLiteral ie) literals

        let n_literals = sizeSS literalsSS
            n_ptrs     = sizeSS ptrsSS

	ptrs_arr <- mkPtrsArray ie ce n_ptrs ptrs

        let 
            ptrs_parr = case ptrs_arr of Array _lo _hi _n parr -> parr

            literals_arr = listArray (0, n_literals-1) linked_literals
                           :: UArray Int Word
            literals_barr = case literals_arr of UArray _lo _hi _n barr -> barr

	    (I# arity#)  = arity

        newBCO insns_barr literals_barr ptrs_parr arity# bitmap


-- we recursively link any sub-BCOs while making the ptrs array
mkPtrsArray :: ItblEnv -> ClosureEnv -> Int -> [BCOPtr] -> IO (Array Int HValue)
mkPtrsArray ie ce n_ptrs ptrs = do
  marr <- newArray_ (0, n_ptrs-1)
  let 
    fill (BCOPtrName n)     i = do
	ptr <- lookupName ce n
	unsafeWrite marr i ptr
    fill (BCOPtrPrimOp op)  i = do
 	ptr <- lookupPrimOp op
	unsafeWrite marr i ptr
    fill (BCOPtrBCO ul_bco) i = do
	BCO bco# <- linkBCO' ie ce ul_bco
	writeArrayBCO marr i bco#
    fill (BCOPtrBreakInfo brkInfo) i =                    
        unsafeWrite marr i (unsafeCoerce# brkInfo)
    fill (BCOPtrArray brkArray) i =                    
        unsafeWrite marr i (unsafeCoerce# brkArray)
  zipWithM fill ptrs [0..]
  unsafeFreeze marr

newtype IOArray i e = IOArray (STArray RealWorld i e)

instance MArray IOArray e IO where
    getBounds (IOArray marr) = stToIO $ getBounds marr
    getNumElements (IOArray marr) = stToIO $ getNumElements marr
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOArray marr)
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOArray marr)
    unsafeRead (IOArray marr) i = stToIO (unsafeRead marr i)
    unsafeWrite (IOArray marr) i e = stToIO (unsafeWrite marr i e)

-- XXX HACK: we should really have a new writeArray# primop that takes a BCO#.
writeArrayBCO :: IOArray Int a -> Int -> BCO# -> IO ()
writeArrayBCO (IOArray (STArray _ _ _ marr#)) (I# i#) bco# = IO $ \s# ->
  case (unsafeCoerce# writeArray#) marr# i# bco# s# of { s# ->
  (# s#, () #) }

{-
writeArrayMBA :: IOArray Int a -> Int -> MutableByteArray# a -> IO ()
writeArrayMBA (IOArray (STArray _ _ marr#)) (I# i#) mba# = IO $ \s# ->
  case (unsafeCoerce# writeArray#) marr# i# bco# s# of { s# ->
  (# s#, () #) }
-}

data BCO = BCO BCO#

newBCO :: ByteArray# -> ByteArray# -> Array# a -> Int# -> ByteArray# -> IO BCO
newBCO instrs lits ptrs arity bitmap
   = IO $ \s -> case newBCO# instrs lits ptrs arity bitmap s of 
		  (# s1, bco #) -> (# s1, BCO bco #)


lookupLiteral :: ItblEnv -> BCONPtr -> IO Word
lookupLiteral ie (BCONPtrWord lit) = return lit
lookupLiteral ie (BCONPtrLbl  sym) = do Ptr a# <- lookupStaticPtr sym
			                return (W# (int2Word# (addr2Int# a#)))
lookupLiteral ie (BCONPtrItbl nm)  = do Ptr a# <- lookupIE ie nm
			                return (W# (int2Word# (addr2Int# a#)))

lookupStaticPtr :: FastString -> IO (Ptr ())
lookupStaticPtr addr_of_label_string 
   = do let label_to_find = unpackFS addr_of_label_string
        m <- lookupSymbol label_to_find 
        case m of
           Just ptr -> return ptr
           Nothing  -> linkFail "ByteCodeLink: can't find label" 
                                label_to_find

lookupPrimOp :: PrimOp -> IO HValue
lookupPrimOp primop
   = do let sym_to_find = primopToCLabel primop "closure"
        m <- lookupSymbol sym_to_find
        case m of
           Just (Ptr addr) -> case addrToHValue# addr of
                                 (# hval #) -> return hval
           Nothing -> linkFail "ByteCodeLink.lookupCE(primop)" sym_to_find

lookupName :: ClosureEnv -> Name -> IO HValue
lookupName ce nm
   = case lookupNameEnv ce nm of
        Just (_,aa) -> return aa
        Nothing 
           -> ASSERT2(isExternalName nm, ppr nm)
	      do let sym_to_find = nameToCLabel nm "closure"
                 m <- lookupSymbol sym_to_find
                 case m of
                    Just (Ptr addr) -> case addrToHValue# addr of
                                          (# hval #) -> return hval
                    Nothing         -> linkFail "ByteCodeLink.lookupCE" sym_to_find

lookupIE :: ItblEnv -> Name -> IO (Ptr a)
lookupIE ie con_nm 
   = case lookupNameEnv ie con_nm of
        Just (_, a) -> return (castPtr (itblCode a))
        Nothing
           -> do -- try looking up in the object files.
                 let sym_to_find1 = nameToCLabel con_nm "con_info"
                 m <- lookupSymbol sym_to_find1
                 case m of
                    Just addr -> return addr
                    Nothing 
                       -> do -- perhaps a nullary constructor?
                             let sym_to_find2 = nameToCLabel con_nm "static_info"
                             n <- lookupSymbol sym_to_find2
                             case n of
                                Just addr -> return addr
                                Nothing   -> linkFail "ByteCodeLink.lookupIE" 
                                                (sym_to_find1 ++ " or " ++ sym_to_find2)

linkFail :: String -> String -> IO a
linkFail who what
   = throwDyn (ProgramError $
        unlines [ ""
	        , "During interactive linking, GHCi couldn't find the following symbol:"
		, ' ' : ' ' : what 
		, "This may be due to you not asking GHCi to load extra object files,"
		, "archives or DLLs needed by your current session.  Restart GHCi, specifying"
		, "the missing library using the -L/path/to/object/dir and -lmissinglibname"
		, "flags, or simply by naming the relevant files on the GHCi command line."
		, "Alternatively, this link failure might indicate a bug in GHCi."
		, "If you suspect the latter, please send a bug report to:"
		, "  glasgow-haskell-bugs@haskell.org"
		])

-- HACKS!!!  ToDo: cleaner
nameToCLabel :: Name -> String{-suffix-} -> String
nameToCLabel n suffix
   = if pkgid /= mainPackageId
        then package_part ++ '_': qual_name
        else qual_name
  where
        pkgid = modulePackageId mod
        mod = nameModule n
        package_part = unpackFS (zEncodeFS (packageIdFS (modulePackageId mod)))
        module_part  = unpackFS (zEncodeFS (moduleNameFS (moduleName mod)))
        occ_part     = unpackFS (zEncodeFS (occNameFS (nameOccName n)))
        qual_name = module_part ++ '_':occ_part ++ '_':suffix


primopToCLabel :: PrimOp -> String{-suffix-} -> String
primopToCLabel primop suffix
   = let str = "base_GHCziPrimopWrappers_" ++ unpackFS (zEncodeFS (occNameFS (primOpOcc primop))) ++ '_':suffix
     in --trace ("primopToCLabel: " ++ str)
        str
\end{code}

