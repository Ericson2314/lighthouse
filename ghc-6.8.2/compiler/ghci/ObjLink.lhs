%
% (c) The University of Glasgow, 2000-2006
%

-- ---------------------------------------------------------------------------
-- 	The dynamic linker for object code (.o .so .dll files)
-- ---------------------------------------------------------------------------

Primarily, this module consists of an interface to the C-land dynamic linker.

\begin{code}
{-# OPTIONS -#include "Linker.h" #-}

{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module ObjLink ( 
   initObjLinker,	 -- :: IO ()
   loadDLL,		 -- :: String -> IO (Maybe String)
   loadObj,     	 -- :: String -> IO ()
   unloadObj,   	 -- :: String -> IO ()
   insertSymbol,         -- :: String -> String -> Ptr a -> IO ()
   lookupSymbol,	 -- :: String -> IO (Maybe (Ptr a))
   resolveObjs  	 -- :: IO SuccessFlag
  )  where

import Panic		( panic )
import BasicTypes	( SuccessFlag, successIf )
import Config		( cLeadingUnderscore )
import Outputable

import Control.Monad    ( when )
import Foreign.C
import Foreign		( nullPtr )
import GHC.Exts         ( Ptr(..), unsafeCoerce# )



-- ---------------------------------------------------------------------------
-- RTS Linker Interface
-- ---------------------------------------------------------------------------

insertSymbol :: String -> String -> Ptr a -> IO ()
insertSymbol obj_name key symbol
    = let str = prefixUnderscore key
      in withCString obj_name $ \c_obj_name ->
         withCString str $ \c_str ->
          c_insertSymbol c_obj_name c_str symbol

lookupSymbol :: String -> IO (Maybe (Ptr a))
lookupSymbol str_in = do
   let str = prefixUnderscore str_in
   withCString str $ \c_str -> do
     addr <- c_lookupSymbol c_str
     if addr == nullPtr
	then return Nothing
	else return (Just addr)

prefixUnderscore :: String -> String
prefixUnderscore
 | cLeadingUnderscore == "YES" = ('_':)
 | otherwise                   = id

loadDLL :: String -> IO (Maybe String)
-- Nothing      => success
-- Just err_msg => failure
loadDLL str = do
  maybe_errmsg <- withCString str $ \dll -> c_addDLL dll
  if maybe_errmsg == nullPtr
	then return Nothing
	else do str <- peekCString maybe_errmsg
		return (Just str)

loadObj :: String -> IO ()
loadObj str = do
   withCString str $ \c_str -> do
     r <- c_loadObj c_str
     when (r == 0) (panic "loadObj: failed")

unloadObj :: String -> IO ()
unloadObj str =
   withCString str $ \c_str -> do
     r <- c_unloadObj c_str
     when (r == 0) (panic "unloadObj: failed")

resolveObjs :: IO SuccessFlag
resolveObjs = do
   r <- c_resolveObjs
   return (successIf (r /= 0))

-- ---------------------------------------------------------------------------
-- Foreign declarations to RTS entry points which does the real work;
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "addDLL"	   c_addDLL :: CString -> IO CString
foreign import ccall unsafe "initLinker"   initObjLinker :: IO ()
foreign import ccall unsafe "insertSymbol" c_insertSymbol :: CString -> CString -> Ptr a -> IO ()
foreign import ccall unsafe "lookupSymbol" c_lookupSymbol :: CString -> IO (Ptr a)
foreign import ccall unsafe "loadObj"      c_loadObj :: CString -> IO Int
foreign import ccall unsafe "unloadObj"    c_unloadObj :: CString -> IO Int
foreign import ccall unsafe "resolveObjs"  c_resolveObjs :: IO Int
\end{code}
