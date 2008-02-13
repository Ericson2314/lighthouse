{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.Readline
-- Copyright   :  (c) unknown
-- License     :  GPL (depends on libreadline, which is GPL)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires libreadline)
--
-- A Haskell binding to the GNU readline library.  The GNU Readline
-- library provides a set of functions for use by applications that
-- allow users to edit command lines as they are typed in.  By
-- default, the line editing commands are similar to those of
-- emacs.  A vi-style line editing interface is also available.
--
-- An example of a typical use of readline with history functionality
-- is illustrated in the following read, eval, print loop:
--
-- @
-- readEvalPrintLoop :: IO ()
-- readEvalPrintLoop = do
--   maybeLine <- readline \"% \"
--   case maybeLine of 
--    Nothing     -> return () -- EOF \/ control-d
--    Just \"exit\" -> return ()
--    Just line -> do addHistory line
--                    putStrLn $ \"The user input: \" ++ (show line)
--                    readEvalPrintLoop
-- @
--

-----------------------------------------------------------------------------

#include "HsReadline.h"
#include "HsReadlineConfig.h"
module System.Console.Readline (
    --------------------------------------------------------------------
    -- Basic Behavior.
    
    readline,   -- :: String -> IO (Maybe String)
    addHistory, -- :: String -> IO ()
    
    --------------------------------------------------------------------
    -- Readline Variables.
    
    getLineBuffer,        -- :: IO String
#if HAVE_READLINE_4
    setLineBuffer,        -- :: String -> IO ()
#endif
    
    -- Functions involving point positions are meaningful only when string
    -- conversion between Haskell and C preserves the length.
    getPoint,             -- :: IO Int
    setPoint,             -- :: Int -> IO ()
    getEnd,               -- :: IO Int
    setEnd,               -- :: Int -> IO ()
    getMark,              -- :: IO Int
    setMark,              -- :: Int -> IO ()
    
    setDone,              -- :: Bool -> IO ()
    setPendingInput,      -- :: Char -> IO ()
#if HAVE_READLINE_4
    setEraseEmptyLine,    -- :: Bool -> IO ()
#endif
    getPrompt,            -- :: IO String
#if HAVE_READLINE_4
    setAlreadyPrompted,   -- :: Bool -> IO ()
#endif
    getLibraryVersion,    -- :: IO String
    getTerminalName,      -- :: IO String
    setReadlineName,      -- :: String -> IO ()
    getInStream,          -- :: IO Handle
    getOutStream,         -- :: IO Handle
    setStartupHook,       -- :: Maybe (IO ()) -> IO ()
#if HAVE_READLINE_4
    setPreInputHook,      -- :: Maybe (IO ()) -> IO ()
#endif
    setEventHook,         -- :: Maybe (IO ()) -> IO ()
    -- rl_getc_function wrapper is not provided because it uses FILE *
    -- and it would be too expensive to convert FILE * to Handle
    -- for each character.
    setRedisplayFunction, -- :: Maybe (IO ()) -> IO ()
    -- Nothing means the original: rl_redisplay.
    
    --------------------------------------------------------------------
    -- Selecting a Keymap.
    
    -- Keymaps are not garbage collected. They must be explicitly freed
    -- using freeKeymap.
    
    Keymap,             -- data Keymap
    newBareKeymap,      -- :: IO Keymap
    copyKeymap,         -- :: Keymap -> IO Keymap
    newKeymap,          -- :: IO Keymap
    freeKeymap,         -- :: Keymap -> IO ()
    getKeymap,          -- :: IO Keymap
    setKeymap,          -- :: Keymap -> IO ()
    getKeymapByName,    -- :: String -> IO Keymap
    getKeymapName,      -- :: Keymap -> IO (Maybe String)
    getExecutingKeymap, -- :: IO Keymap
    getBindingKeymap,   -- :: IO Keymap
    
    --------------------------------------------------------------------
    -- Binding Keys.
    
    Callback,           -- type Callback = Int -> Char -> IO Int
    addDefun,           -- :: String -> Callback -> Maybe Char -> IO ()
    bindKey,            -- :: Char -> Callback -> IO ()
    bindKeyInMap,       -- :: Char -> Callback -> Keymap -> IO ()
    unbindKey,          -- :: Char -> IO ()
    unbindKeyInMap,     -- :: Char -> Keymap -> IO ()
    -- rl_unbind_function_in_map is not provided because Haskell functions
    -- have no identity.
    unbindCommandInMap, -- :: String -> Keymap -> IO ()
    Entry(..),          -- data Entry
                        --     = Function Callback
                        --     | Macro    String
                        --     | Keymap   Keymap
    genericBind,        -- :: String -> Entry -> Keymap -> IO ()
    parseAndBind,       -- :: String -> IO ()
    readInitFile,       -- :: String -> IO ()
    
    --------------------------------------------------------------------
    -- Associating Function Names and Bindings.
    
    namedFunction,    -- :: String -> IO (Maybe Callback)
    functionOfKeyseq, -- :: String -> Maybe Keymap -> IO Entry
    -- rl_invoking_keyseqs and rl_invoking_keyseqs_in_map are not provided
    -- because Haskell functions have no identity.
    functionDumper,   -- :: Bool -> IO ()
    listFunmapNames,  -- :: IO ()
#if HAVE_READLINE_4
    funmapNames,      -- :: IO [String]
#endif
    
    --------------------------------------------------------------------
    -- Allowing Undoing.
    
    beginUndoGroup, endUndoGroup, -- :: IO ()
    UndoCode(..),   -- data UndoCode
                    --     = UndoDelete
                    --     | UndoInsert
                    --     | UndoBegin
                    --     | UndoEnd
    addUndo,        -- :: UndoCode -> Int -> Int -> String -> IO ()
    freeUndoList,   -- :: IO ()
    doUndo,         -- :: IO Bool
    modifying,      -- :: Int -> Int -> IO ()
    
    --------------------------------------------------------------------
    -- Redisplay.
    
    redisplay,                      -- :: IO ()
    forcedUpdateDisplay,            -- :: IO ()
    onNewLine,                      -- :: IO ()
#if HAVE_READLINE_4
    onNewLineWithPrompt,            -- :: IO ()
#endif
    resetLineState,                 -- :: IO ()
    message,                        -- :: String -> IO ()
    clearMessage,                   -- :: IO ()
#if HAVE_READLINE_4
    savePrompt,                     -- :: IO ()
    restorePrompt,                  -- :: IO ()
#endif

    --------------------------------------------------------------------
    -- Modifying Text.
    
    insertText, -- :: String -> IO ()
    deleteText, -- :: Int -> Int -> IO ()
    copyText,   -- :: Int -> Int -> IO String
    killText,   -- :: Int -> Int -> IO ()
    
    --------------------------------------------------------------------
    -- Utility functions.
    
    readKey,          -- :: IO Char
    stuffChar,        -- :: Char -> IO Bool
    initialize,       -- :: IO ()
    resetTerminal,    -- :: Maybe String -> IO ()
    ding,             -- :: IO Bool
#if HAVE_READLINE_4
    displayMatchList, -- :: [String] -> IO ()
#endif
    
    --------------------------------------------------------------------
    -- Alternate Interface.
    
    callbackHandlerInstall, -- :: String -> (String -> IO ()) -> IO (IO ())
    -- Returns the cleanup action.
    callbackReadChar,       -- :: IO ()
    
    --------------------------------------------------------------------
    -- Readline Signal Handling.
    
#if HAVE_READLINE_4
    setCatchSignals,    -- :: Bool -> IO ()
    getCatchSignals,    -- :: IO Bool
    setCatchSigwinch,   -- :: Bool -> IO ()
    getCatchSigwinch,   -- :: IO Bool
    cleanupAfterSignal, -- :: IO ()
    freeLineState,      -- :: IO ()
    resetAfterSignal,   -- :: IO ()
    resizeTerminal,     -- :: IO ()
#endif
    setSignals,         -- :: IO ()
    clearSignals,       -- :: IO ()
    
    --------------------------------------------------------------------
    -- Completion functions.
    
    completeInternal,                 -- :: Char -> IO ()
    complete,                         -- :: Int -> Char -> IO Int
    possibleCompletions,              -- :: Int -> Char -> IO Int
    insertCompletions,                -- :: Int -> Char -> IO Int
    -- readline uses functions that are called multiple times and
    -- return an entry at a time, maintaining their state at which
    -- point they are. This is silly in a functional language so here
    -- we work with functions String -> IO [String].
    completionMatches,
        -- :: String -> (String -> IO [String]) -> IO (Maybe (String, [String]))
    filenameCompletionFunction,       -- :: String -> IO [String]
    usernameCompletionFunction,       -- :: String -> IO [String]
    setCompletionEntryFunction,
        -- :: Maybe (String -> IO [String]) -> IO ()
    setAttemptedCompletionFunction,
        -- :: Maybe (String -> Int -> Int -> IO (Maybe (String, [String]))) -> IO ()
    setFilenameQuotingFunction,
        -- :: Maybe (String -> Bool -> Ptr CChar -> IO String) -> IO ()
    quoteFilename,
        -- :: String -> Bool -> Ptr CChar -> IO String
    setFilenameDequotingFunction,
        -- :: Maybe (String -> Maybe Char -> IO String) -> IO ()
    setCharIsQuotedP,
        -- :: Maybe (String -> Int -> IO Bool) -> IO ()
    getCompletionQueryItems,          -- :: IO Int
    setCompletionQueryItems,          -- :: Int -> IO ()
    getBasicWordBreakCharacters,      -- :: IO String
    setBasicWordBreakCharacters,      -- :: String -> IO ()
    getBasicQuoteCharacters,          -- :: IO String
    setBasicQuoteCharacters,          -- :: String -> IO ()
    getCompleterWordBreakCharacters,  -- :: IO String
    setCompleterWordBreakCharacters,  -- :: String -> IO ()
    getCompleterQuoteCharacters,      -- :: IO String
    setCompleterQuoteCharacters,      -- :: String -> IO ()
    getFilenameQuoteCharacters,       -- :: IO String
    setFilenameQuoteCharacters,       -- :: String -> IO ()
    getSpecialPrefixes,               -- :: IO String
    setSpecialPrefixes,               -- :: String -> IO ()
    getCompletionAppendCharacter,     -- :: IO (Maybe Char)
    setCompletionAppendCharacter,     -- :: Maybe Char -> IO ()
    setIgnoreCompletionDuplicates,    -- :: Bool -> IO ()
    getIgnoreCompletionDuplicates,    -- :: IO Bool
    setFilenameCompletionDesired,     -- :: Bool -> IO ()
    getFilenameCompletionDesired,     -- :: IO Bool
    setFilenameQuotingDesired,        -- :: Bool -> IO ()
    getFilenameQuotingDesired,        -- :: IO Bool
    setInhibitCompletion,             -- :: Bool -> IO ()
    getInhibitCompletion,             -- :: IO Bool
    setAttemptedCompletionOver,       -- :: Bool -> IO ()
    getAttemptedCompletionOver,       -- :: IO Bool
    setIgnoreSomeCompletionsFunction,
        -- :: Maybe ([String] -> IO [String]) -> IO ()
        -- The function may not make the list longer!
    setDirectoryCompletionHook
        -- :: Maybe (String -> IO String) -> IO ()
#if HAVE_READLINE_5
    ,
    setCompletionWordBreakHook
        -- :: Maybe (IO (Maybe String)) -> IO ()
#endif
#if HAVE_READLINE_4
    ,
    setCompletionDisplayMatchesHook
        -- :: Maybe ([String] -> IO ()) -> IO ()
#endif
    )
    
    where

------------------------------------------------------------------------

import Control.Monad	( liftM, when, unless )
import Data.Char	( chr, ord )
import Data.Maybe	( fromMaybe )
import System.IO	( Handle )
import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef	( newIORef, readIORef, writeIORef )
import Foreign.Ptr	( Ptr, nullPtr, castPtr, castFunPtrToPtr,
			  FunPtr, nullFunPtr, freeHaskellFunPtr )
import Foreign.Storable	( Storable(..) )
import Foreign.Marshal.Utils ( maybePeek, maybeWith, withMany )
import Foreign.Marshal.Alloc ( alloca, free )
import Foreign.Marshal.Array ( mallocArray, peekArray0, pokeArray0, withArray0 )
import Foreign.C.Types	( CInt, CChar, CFile )
import Foreign.C.String	( newCString, peekCString, withCString,
			  castCharToCChar, castCCharToChar )
import GHC.Handle	( fdToHandle )

{-# CFILES HsReadline_cbits.c #-}

------------------------------------------------------------------------
-- Basic Behavior.

-- | readline is similar to 'System.IO.getLine', but with rich edit
-- functionality and history capability.  readline will read a line
-- from the terminal and return it, using /prompt/ as a prompt.  If
-- prompt is the empty string, no prompt is issued.  The line returned
-- has the final newline removed, so only the text of the line
-- remains.  A blank line returns the empty string.  If EOF is
-- encountered while reading a line, and the line is empty, Nothing is
-- returned.  If an EOF is read with a non-empty line, it is treated
-- as a newline.

readline :: String-- ^prompt
	 -> IO (Maybe String) -- ^returns the line the user input, or Nothing if EOF is encountered.
readline prompt = do
    ptr <- withCString prompt readlineC
    flip maybePeek ptr $ \ptr' -> do
        line <- peekCString ptr'
        free ptr'
        return line
foreign import ccall "readline" readlineC :: Ptr CChar -> IO (Ptr CChar)


-- |Add this command to the history.  This allows users to search backward
-- through history with C-r and step through with up and down arrows, among
-- other things.
addHistory :: String -> IO ()
addHistory line = withCString line add_history
foreign import ccall unsafe add_history :: Ptr CChar -> IO ()

------------------------------------------------------------------------
-- Readline Variables.

getLineBuffer :: IO String
getLineBuffer = peek rl_line_buffer >>= peekCString

#if HAVE_READLINE_4
setLineBuffer :: String -> IO ()
setLineBuffer line = do
    -- TODO: Fix the next line when text conversions are available!
    let lineC = map castCharToCChar line
    rl_extend_line_buffer (fromIntegral (length lineC))
    ptr <- peek rl_line_buffer
    pokeArray0 0 (castPtr ptr) lineC
#endif

foreign import ccall "&"
  rl_line_buffer :: Ptr (Ptr CChar)
#if HAVE_READLINE_4
-- The readline docs claim that rl_extend_line_buffer returns CInt,
-- but the header and source both say that it returns void.
foreign import ccall unsafe rl_extend_line_buffer :: CInt -> IO ()
#endif

-- Functions involving point positions are meaningful only when string
-- conversion between Haskell and C preserves the length.

getPoint :: IO Int
getPoint = liftM fromIntegral (peek rl_point)

setPoint :: Int -> IO ()
setPoint p = poke rl_point (fromIntegral p)

foreign import ccall "&" rl_point :: Ptr CInt

getEnd :: IO Int
getEnd = liftM fromIntegral (peek rl_end)

setEnd :: Int -> IO ()
setEnd p = poke rl_end (fromIntegral p)

foreign import ccall "&" rl_end :: Ptr CInt

getMark :: IO Int
getMark = liftM fromIntegral (peek rl_mark)

setMark :: Int -> IO ()
setMark p = poke rl_mark (fromIntegral p)

foreign import ccall "&" rl_mark :: Ptr CInt

setDone :: Bool -> IO ()
setDone done = poke rl_done (if done then 1 else 0)
foreign import ccall "&" rl_done :: Ptr CInt

setPendingInput :: Char -> IO ()
setPendingInput key = poke rl_pending_input (fromIntegral (ord key))
foreign import ccall "&" rl_pending_input :: Ptr CInt

#if HAVE_READLINE_4
setEraseEmptyLine :: Bool -> IO ()
setEraseEmptyLine erase = poke rl_erase_empty_line (if erase then 1 else 0)
foreign import ccall "&" rl_erase_empty_line :: Ptr CInt
#endif

getPrompt :: IO String
getPrompt = peek rl_prompt >>= peekCString
foreign import ccall "&" rl_prompt :: Ptr (Ptr CChar)

#if HAVE_READLINE_4
setAlreadyPrompted :: Bool -> IO ()
setAlreadyPrompted pr = poke rl_already_prompted (if pr then 1 else 0)
foreign import ccall "&" rl_already_prompted :: Ptr CInt
#endif

getLibraryVersion :: IO String
getLibraryVersion = peek rl_library_version >>= peekCString
foreign import ccall "&" rl_library_version :: Ptr (Ptr CChar)

getTerminalName :: IO String
getTerminalName = peek rl_terminal_name >>= peekCString
foreign import ccall "&" rl_terminal_name :: Ptr (Ptr CChar)

setReadlineName :: String -> IO ()
setReadlineName name = newCString name >>= poke rl_readline_name
    -- The memory for name will never be freed. Otherwise we would
    -- have to recognize the original value which is a static string
    -- literal. This function is usually called only once anyway.
foreign import ccall "&" rl_readline_name :: Ptr (Ptr CChar)

getInStream :: IO Handle
getInStream = peek rl_instream >>= hs_fileno >>= fdToHandle . fromIntegral
foreign import ccall "&" rl_instream :: Ptr (Ptr CFile)

getOutStream :: IO Handle
getOutStream = peek rl_outstream >>= hs_fileno >>= fdToHandle . fromIntegral
foreign import ccall "&" rl_outstream :: Ptr (Ptr CFile)

foreign import ccall unsafe "__hscore_hs_fileno"
  hs_fileno :: Ptr CFile -> IO CInt

setStartupHook :: Maybe (IO ()) -> IO ()
setStartupHook hook = setFunPtr rl_startup_hook hook exportHookInt
foreign import ccall "&" rl_startup_hook :: Ptr (FunPtr (IO CInt))

#if HAVE_READLINE_4
setPreInputHook :: Maybe (IO ()) -> IO ()
setPreInputHook hook = setFunPtr rl_pre_input_hook hook exportHookInt
foreign import ccall "&" rl_pre_input_hook :: Ptr (FunPtr (IO CInt))
#endif

setEventHook :: Maybe (IO ()) -> IO ()
setEventHook hook = setFunPtr rl_event_hook hook exportHookInt
foreign import ccall "&" rl_event_hook :: Ptr (FunPtr (IO CInt))

-- rl_getc_function wrapper is not provided because it uses FILE *
-- and it would be too expensive to convert FILE * to Handle
-- for each character.

setRedisplayFunction :: Maybe (IO ()) -> IO ()
-- Nothing means the original: rl_redisplay.
setRedisplayFunction fun = do
    oldPtr <- peek rl_redisplay_function
    when (oldPtr /= nullFunPtr && oldPtr /= rl_redisplay) $
        freeHaskellFunPtr oldPtr
    newPtr <- case fun of
        Nothing -> return rl_redisplay
        Just f  -> exportHookVoid f
    poke rl_redisplay_function newPtr
foreign import ccall "&" rl_redisplay_function :: Ptr (FunPtr (IO ()))
foreign import ccall "&" rl_redisplay :: FunPtr (IO ())
-- rl_redisplay_function can never be NULL.

exportHookInt :: IO () -> IO (FunPtr (IO CInt))
exportHookInt hook = exportHookIntC (hook >> return 0)
foreign import ccall "wrapper"
  exportHookIntC :: IO CInt -> IO (FunPtr (IO CInt))

foreign import ccall "wrapper"
  exportHookVoid :: IO () -> IO (FunPtr (IO ()))

setFunPtr_freeIf :: (FunPtr a -> Bool)
                 -> Ptr (FunPtr a)
                 -> Maybe b
                 -> (b -> IO (FunPtr a))
                 -> IO ()
setFunPtr_freeIf pred variable newFun makeNewFun = do
    oldPtr <- peek variable
    when (pred oldPtr) $ freeHaskellFunPtr oldPtr
    newPtr <- case newFun of
        Nothing -> return nullFunPtr
        Just f  -> makeNewFun f
    poke variable newPtr

setFunPtr :: Ptr (FunPtr a)
          -> Maybe b
          -> (b -> IO (FunPtr a))
          -> IO ()
setFunPtr = setFunPtr_freeIf (/= nullFunPtr)

------------------------------------------------------------------------
-- Selecting a Keymap.

-- Keymaps are not garbage collected. They must be explicitly freed
-- using freeKeymap.

data KeymapTag = KeymapTag
newtype Keymap = MkKeymap (Ptr KeymapTag)

foreign import ccall unsafe "rl_make_bare_keymap" newBareKeymap :: IO Keymap

foreign import ccall unsafe "rl_copy_keymap" copyKeymap :: Keymap -> IO Keymap

foreign import ccall unsafe "rl_make_keymap" newKeymap :: IO Keymap

freeKeymap :: Keymap -> IO ()
freeKeymap k@(MkKeymap km) = do
    rl_discard_keymap k
    free km

foreign import ccall unsafe "rl_discard_keymap" 
  rl_discard_keymap :: Keymap -> IO ()

foreign import ccall unsafe "rl_get_keymap"
  getKeymap :: IO Keymap

foreign import ccall unsafe "rl_set_keymap"
  setKeymap :: Keymap -> IO ()

getKeymapByName :: String -> IO Keymap
getKeymapByName name = withCString name rl_get_keymap_by_name
foreign import ccall unsafe 
  rl_get_keymap_by_name :: Ptr CChar -> IO Keymap

getKeymapName :: Keymap -> IO (Maybe String)
getKeymapName km = do
    ptr <- rl_get_keymap_name km
    maybePeek peekCString ptr

foreign import ccall unsafe "rl_get_keymap_name"
  rl_get_keymap_name :: Keymap -> IO (Ptr CChar)

getExecutingKeymap :: IO Keymap
getExecutingKeymap = liftM MkKeymap $ peek rl_executing_keymap
foreign import ccall "&" rl_executing_keymap :: Ptr (Ptr KeymapTag)

getBindingKeymap :: IO Keymap
getBindingKeymap = liftM MkKeymap $ peek rl_binding_keymap
foreign import ccall "&" rl_binding_keymap :: Ptr (Ptr KeymapTag)

------------------------------------------------------------------------
-- Binding Keys.

type Callback = Int -> Char -> IO Int
type CallbackC = CInt -> CInt -> IO CInt

addDefun :: String -> Callback -> Maybe Char -> IO ()
addDefun name cb key = do
    namePtr <- newCString name
    -- rl_add_defun does *not* make a copy of the function name.
    cbPtr <- exportCallback cb
    -- The memory will never be freed. But readline does not provide
    -- removing defuns anyway.
    rl_add_defun namePtr cbPtr (maybe (-1) (fromIntegral . ord) key)
    return ()
foreign import ccall unsafe "rl_add_defun"
    rl_add_defun :: Ptr CChar -> FunPtr CallbackC -> CInt -> IO CInt

bindKey :: Char -> Callback -> IO ()
bindKey key cb = do
    cbPtr <- exportCallback cb
    -- The memory will never be freed. We should provide a way to
    -- free it, but it's complicated because of multiple keymaps.
    -- It should probably be done explicitly.
    rl_bind_key (fromIntegral (ord key)) cbPtr
    return ()
foreign import ccall unsafe "rl_bind_key"
  rl_bind_key :: CInt -> FunPtr CallbackC -> IO CInt

bindKeyInMap :: Char -> Callback -> Keymap -> IO ()
bindKeyInMap key cb km = do
    cbPtr <- exportCallback cb
    rl_bind_key_in_map (fromIntegral (ord key)) cbPtr km
    return ()
foreign import ccall unsafe "rl_bind_key_in_map"
    rl_bind_key_in_map :: CInt -> FunPtr CallbackC -> Keymap -> IO CInt

unbindKey :: Char -> IO ()
unbindKey key = do
    rl_unbind_key (fromIntegral (ord key))
    return ()
foreign import ccall unsafe rl_unbind_key :: CInt -> IO CInt

unbindKeyInMap :: Char -> Keymap -> IO ()
unbindKeyInMap key km = do
    rl_unbind_key_in_map (fromIntegral (ord key)) km
    return ()
foreign import ccall unsafe "rl_unbind_key_in_map"
  rl_unbind_key_in_map :: CInt -> Keymap -> IO CInt

-- rl_unbind_function_in_map is not provided because Haskell functions
-- have no identity.

unbindCommandInMap :: String -> Keymap -> IO ()
unbindCommandInMap comm km = do
    withCString comm $ \commPtr -> rl_unbind_command_in_map commPtr km
    return ()
foreign import ccall unsafe "rl_unbind_command_in_map"
  rl_unbind_command_in_map :: Ptr CChar -> Keymap -> IO CInt

data Entry
    = Function Callback
    | Macro String
    | Keymap Keymap

genericBind :: String -> Entry -> Keymap -> IO ()
genericBind keys (Function cb) km = do
    cbPtr <- exportCallback cb
    genericBind' (#const ISFUNC) keys (castFunPtrToPtr cbPtr) km
genericBind keys (Macro s) km =
    withCString s $ \ptr -> genericBind' (#const ISMACR) keys ptr km
genericBind keys (Keymap (MkKeymap km')) km =
    genericBind' (#const ISKMAP) keys (castPtr km') km

genericBind' :: CInt -> String -> Ptr CChar -> Keymap -> IO ()
genericBind' typ keys dat km = do
    withCString keys $ \keysPtr -> rl_generic_bind typ keysPtr dat km
    return ()
foreign import ccall unsafe "rl_generic_bind"
    rl_generic_bind :: CInt -> Ptr CChar -> Ptr CChar -> Keymap -> IO CInt

parseAndBind :: String -> IO ()
parseAndBind s = do
    ok <- withCString s rl_parse_and_bind
    unless (ok == 0) $ ioError (userError "Parse error")
foreign import ccall unsafe "rl_parse_and_bind"
  rl_parse_and_bind :: Ptr CChar -> IO CInt

readInitFile :: String -> IO ()
readInitFile name = do
    ok <- withCString name rl_read_init_file
    unless (ok == 0) $ ioError (userError "Can't read file")
foreign import ccall unsafe "rl_read_init_file"
  rl_read_init_file :: Ptr CChar -> IO CInt

------------------------------------------------------------------------
-- Associating Function Names and Bindings.

namedFunction :: String -> IO (Maybe Callback)
namedFunction name = do
    ptr <- withCString name rl_named_function
    return $ if ptr == nullFunPtr then Nothing else Just (importCallback ptr)
foreign import ccall unsafe "rl_named_function"
  rl_named_function :: Ptr CChar -> IO (FunPtr CallbackC)

functionOfKeyseq :: String -> Maybe Keymap -> IO Entry
functionOfKeyseq keys km =
    withCString keys $ \keysPtr -> alloca $ \typPtr -> do
        dat <- rl_function_of_keyseq keysPtr (fromMaybe (MkKeymap nullPtr) km) typPtr
        typ <- peek typPtr
        case typ of
            (#const ISFUNC) ->
                return (Function (importCallback dat))
            (#const ISMACR) ->
                liftM Macro (peekCString (castFunPtrToPtr dat))
            (#const ISKMAP) ->
                return (Keymap (MkKeymap (castFunPtrToPtr dat)))
            _ -> error "functionOfKeyseq: unknown type"
foreign import ccall unsafe "rl_function_of_keyseq"
  rl_function_of_keyseq :: Ptr CChar -> Keymap -> Ptr CInt -> IO (FunPtr CallbackC)

-- rl_invoking_keyseqs and rl_invoking_keyseqs_in_map are not provided
-- because Haskell functions have no identity.

functionDumper :: Bool -> IO ()
functionDumper readable = rl_function_dumper (if readable then 1 else 0)
foreign import ccall unsafe "rl_function_dumper"
  rl_function_dumper :: CInt -> IO ()

foreign import ccall unsafe "rl_list_funmap_names" listFunmapNames :: IO ()

#if HAVE_READLINE_4
funmapNames :: IO [String]
funmapNames = do
    namesPtr <- rl_funmap_names
    namePtrs <- peekArray0 nullPtr namesPtr
    free namesPtr
    mapM peekCString namePtrs
foreign import ccall unsafe "rl_funmap_names"
  rl_funmap_names :: IO (Ptr (Ptr CChar))
#endif

exportCallback :: Callback -> IO (FunPtr CallbackC)
exportCallback cb =
    exportCallbackC $ \n key ->
        liftM fromIntegral (cb (fromIntegral n) (chr (fromIntegral key)))
foreign import ccall "wrapper" 
  exportCallbackC :: CallbackC -> IO (FunPtr CallbackC)

importCallback :: FunPtr CallbackC -> Callback
importCallback ptr n key =
    liftM fromIntegral $
        importCallbackC ptr (fromIntegral n) (fromIntegral (ord key))
foreign import ccall "dynamic"
  importCallbackC :: FunPtr CallbackC -> CallbackC

------------------------------------------------------------------------
-- Allowing Undoing.

beginUndoGroup :: IO ()
beginUndoGroup = do rl_begin_undo_group; return ()
foreign import ccall unsafe "rl_begin_undo_group"
  rl_begin_undo_group :: IO CInt

endUndoGroup :: IO ()
endUndoGroup = do rl_end_undo_group; return ()
foreign import ccall unsafe "rl_end_undo_group"
  rl_end_undo_group :: IO CInt

data UndoCode = UndoDelete | UndoInsert | UndoBegin | UndoEnd

addUndo :: UndoCode -> Int -> Int -> String -> IO ()
addUndo uc start end text =
    withCString text $ \textPtr ->
        rl_add_undo uc' (fromIntegral start) (fromIntegral end) textPtr
    where
    uc' = case uc of
        UndoDelete -> #const UNDO_DELETE
        UndoInsert -> #const UNDO_INSERT
        UndoBegin  -> #const UNDO_BEGIN
        UndoEnd    -> #const UNDO_END
foreign import ccall unsafe 
  rl_add_undo :: CInt -> CInt -> CInt -> Ptr CChar -> IO ()

#if HAVE_READLINE_4_2
foreign import ccall unsafe "rl_free_undo_list" freeUndoList :: IO ()
#else
foreign import ccall unsafe "free_undo_list" freeUndoList :: IO ()
#endif

doUndo :: IO Bool
doUndo = liftM (/= 0) rl_do_undo
foreign import ccall unsafe "rl_do_undo"
  rl_do_undo :: IO CInt

modifying :: Int -> Int -> IO ()
modifying start end = do
    rl_modifying (fromIntegral start) (fromIntegral end)
    return ()
foreign import ccall unsafe "rl_modifying"
  rl_modifying :: CInt -> CInt -> IO CInt

------------------------------------------------------------------------
-- Redisplay.

foreign import ccall unsafe "rl_redisplay" redisplay :: IO ()

forcedUpdateDisplay :: IO ()
forcedUpdateDisplay = do rl_forced_update_display; return ()
foreign import ccall unsafe "rl_forced_update_display"
  rl_forced_update_display :: IO CInt

onNewLine :: IO ()
onNewLine = do rl_on_new_line; return ()
foreign import ccall unsafe "rl_on_new_line"
  rl_on_new_line :: IO CInt

#if HAVE_READLINE_4
onNewLineWithPrompt :: IO ()
onNewLineWithPrompt = do rl_on_new_line_with_prompt; return ()
foreign import ccall unsafe "rl_on_new_line_with_prompt"
  rl_on_new_line_with_prompt :: IO CInt
#endif

resetLineState :: IO ()
resetLineState = do rl_reset_line_state; return ()
foreign import ccall unsafe "rl_reset_line_state"
  rl_reset_line_state :: IO CInt

message :: String -> IO ()
message s = withCString s hs_rl_message
foreign import ccall unsafe "hs_rl_message"
  hs_rl_message :: Ptr CChar -> IO ()

clearMessage :: IO ()
clearMessage = do rl_clear_message; return ()
foreign import ccall unsafe "rl_clear_message"
  rl_clear_message :: IO CInt

#if HAVE_READLINE_4
foreign import ccall unsafe "rl_save_prompt" savePrompt :: IO ()

foreign import ccall unsafe "rl_restore_prompt" restorePrompt :: IO ()
#endif

------------------------------------------------------------------------
-- Modifying Text.

insertText :: String -> IO ()
insertText s = do withCString s rl_insert_text; return ()
foreign import ccall unsafe "rl_insert_text"
  rl_insert_text :: Ptr CChar -> IO CInt

deleteText :: Int -> Int -> IO ()
deleteText start end = do
    rl_delete_text (fromIntegral start) (fromIntegral end)
    return ()
foreign import ccall unsafe "rl_delete_text"
  rl_delete_text :: CInt -> CInt -> IO CInt

copyText :: Int -> Int -> IO String
copyText start end = do
    ptr <- rl_copy_text (fromIntegral start) (fromIntegral end)
    text <- peekCString ptr
    free ptr
    return text
foreign import ccall unsafe "rl_copy_text"
  rl_copy_text :: CInt -> CInt -> IO (Ptr CChar)

killText :: Int -> Int -> IO ()
killText start end = do
    rl_kill_text (fromIntegral start) (fromIntegral end)
    return ()
foreign import ccall unsafe "rl_kill_text"
  rl_kill_text :: CInt -> CInt -> IO CInt

------------------------------------------------------------------------
-- Utility functions.

readKey :: IO Char
readKey = liftM (chr . fromIntegral) rl_read_key
foreign import ccall unsafe "rl_read_key"
  rl_read_key :: IO CInt

stuffChar :: Char -> IO Bool
stuffChar key = liftM (/= 0) (rl_stuff_char (fromIntegral (ord key)))
foreign import ccall unsafe "rl_stuff_char"
  rl_stuff_char :: CInt -> IO CInt

initialize :: IO ()
initialize = do rl_initialize; return ()
foreign import ccall unsafe "rl_initialize"
  rl_initialize :: IO CInt

resetTerminal :: Maybe String -> IO ()
resetTerminal name = do
    maybeWith withCString name rl_reset_terminal
    return ()
foreign import ccall unsafe "rl_reset_terminal"
  rl_reset_terminal :: Ptr CChar -> IO CInt

ding :: IO Bool
ding = liftM (== 0) rl_ding
#if HAVE_READLINE_4_2
foreign import ccall unsafe "rl_ding" rl_ding :: IO CInt
#else
foreign import ccall unsafe "ding" rl_ding :: IO CInt
#endif

#if HAVE_READLINE_4
displayMatchList :: [String] -> IO ()
displayMatchList matches =
    withMany withCString matches $ \matchPtrs ->
        withArray0 nullPtr (nullPtr:matchPtrs) $ \matchesPtr ->
            rl_display_match_list
                matchesPtr
                (fromIntegral (length matches))
                (fromIntegral (maximum (map length matches)))
foreign import ccall unsafe "rl_display_match_list"
  rl_display_match_list :: Ptr (Ptr CChar) -> CInt -> CInt -> IO ()
#endif

------------------------------------------------------------------------
-- Alternate Interface.

type Handler = Ptr CChar -> IO ()

callbackHandlerInstall :: String -> (String -> IO ()) -> IO (IO ())
callbackHandlerInstall prompt lhandler = do
    lhandlerPtr <- exportHandler $ \linePtr -> peekCString linePtr >>= lhandler
    withCString prompt $ \promptPtr -> do
        rl_callback_handler_install promptPtr lhandlerPtr
    return (do rl_callback_handler_remove; freeHaskellFunPtr lhandlerPtr)
foreign import ccall "wrapper"
  exportHandler :: Handler -> IO (FunPtr Handler)
foreign import ccall unsafe "rl_callback_handler_install"
  rl_callback_handler_install :: Ptr CChar -> FunPtr Handler -> IO ()
foreign import ccall unsafe "rl_callback_handler_remove"
  rl_callback_handler_remove :: IO ()

foreign import ccall "rl_callback_read_char" 
  callbackReadChar :: IO ()

------------------------------------------------------------------------
-- Readline Signal Handling.

#if HAVE_READLINE_4
setCatchSignals :: Bool -> IO ()
setCatchSignals cat = poke rl_catch_signals (if cat then 1 else 0)

getCatchSignals :: IO Bool
getCatchSignals = liftM (/= 0) (peek rl_catch_signals)

foreign import ccall "&" rl_catch_signals :: Ptr CInt

setCatchSigwinch :: Bool -> IO ()
setCatchSigwinch cat = poke rl_catch_sigwinch (if cat then 1 else 0)

getCatchSigwinch :: IO Bool
getCatchSigwinch = liftM (/= 0) (peek rl_catch_sigwinch)

foreign import ccall "&" rl_catch_sigwinch :: Ptr CInt

foreign import ccall unsafe "rl_cleanup_after_signal" cleanupAfterSignal :: IO ()

foreign import ccall unsafe "rl_free_line_state" freeLineState :: IO ()

foreign import ccall unsafe "rl_reset_after_signal" resetAfterSignal :: IO ()

foreign import ccall unsafe "rl_resize_terminal" resizeTerminal :: IO ()
#endif

setSignals :: IO ()
setSignals = do rl_set_signals; return ()
foreign import ccall unsafe "rl_set_signals"
  rl_set_signals :: IO CInt

clearSignals :: IO ()
clearSignals = do rl_clear_signals; return ()
foreign import ccall unsafe "rl_clear_signals"
  rl_clear_signals :: IO CInt

------------------------------------------------------------------------
-- Completion functions.

completeInternal :: Char -> IO ()
completeInternal what = do
    rl_complete_internal (fromIntegral (ord what))
    return ()
foreign import ccall "rl_complete_internal"
  rl_complete_internal :: CInt -> IO CInt

complete :: Int -> Char -> IO Int
complete n key =
    liftM fromIntegral $
        rl_complete (fromIntegral n) (fromIntegral (ord key))
foreign import ccall "rl_complete"
  rl_complete :: CInt -> CInt -> IO CInt

possibleCompletions :: Int -> Char -> IO Int
possibleCompletions n key =
    liftM fromIntegral $
        rl_possible_completions (fromIntegral n) (fromIntegral (ord key))
foreign import ccall "rl_possible_completions"
  rl_possible_completions :: CInt -> CInt -> IO CInt

insertCompletions :: Int -> Char -> IO Int
insertCompletions n key =
    liftM fromIntegral $
        rl_insert_completions (fromIntegral n) (fromIntegral (ord key))
foreign import ccall "rl_insert_completions"
  rl_insert_completions :: CInt -> CInt -> IO CInt

type Generator = Ptr CChar -> CInt -> IO (Ptr CChar)

singleToWhole :: Generator -> String -> IO [String]
singleToWhole f text =
    withCString text $ \textPtr -> let
        loop n = do
            ptr <- f textPtr n
            if ptr == nullPtr
                then return []
                else do
                    str <- peekCString ptr
                    free ptr
                    rest <- loop (n+1)
                    return (str:rest)
        in loop 0

wholeToSingle :: (String -> IO [String]) -> IO Generator
wholeToSingle f = do
    ref <- newIORef []
    return $ \textPtr state -> do
        when (state == 0) $ peekCString textPtr >>= f >>= writeIORef ref
        next <- readIORef ref
        case next of
            []   -> return nullPtr
            x:xs -> do
                writeIORef ref xs
                newCString x

completionMatches
    :: String -> (String -> IO [String]) -> IO (Maybe (String, [String]))
completionMatches text entry =
    withCString text $ \textPtr -> do
        entryPtr <- wholeToSingle entry >>= exportGenerator
        matchesPtr <- rl_completion_matches textPtr entryPtr
        freeHaskellFunPtr entryPtr
        if matchesPtr == nullPtr then return Nothing else do
            matchPtrs <- peekArray0 nullPtr matchesPtr
            (text':matches) <- mapM peekCString matchPtrs
            mapM_ free matchPtrs
            free matchesPtr
            return (Just (text', matches))
#if HAVE_READLINE_4_2
foreign import ccall "rl_completion_matches"
    rl_completion_matches :: Ptr CChar -> FunPtr Generator -> IO (Ptr (Ptr CChar))
#else
foreign import ccall "completion_matches"
    rl_completion_matches :: Ptr CChar -> FunPtr Generator -> IO (Ptr (Ptr CChar))
#endif

filenameCompletionFunction :: String -> IO [String]
filenameCompletionFunction = singleToWhole rl_filename_completion_function
#if HAVE_READLINE_4_2
foreign import ccall unsafe "rl_filename_completion_function"
  rl_filename_completion_function :: Generator
#else
foreign import ccall unsafe "filename_completion_function"
    rl_filename_completion_function :: Generator
#endif

usernameCompletionFunction :: String -> IO [String]
usernameCompletionFunction = singleToWhole rl_username_completion_function
#if HAVE_READLINE_4_2
foreign import ccall unsafe "rl_username_completion_function"
  rl_username_completion_function :: Generator
#else
foreign import ccall unsafe "username_completion_function"
    rl_username_completion_function :: Generator
#endif

setCompletionEntryFunction :: Maybe (String -> IO [String]) -> IO ()
setCompletionEntryFunction fun =
    setFunPtr rl_completion_entry_function fun $ \f ->
        wholeToSingle f >>= exportGenerator
foreign import ccall "&" rl_completion_entry_function :: Ptr (FunPtr Generator)

foreign import ccall "wrapper"
    exportGenerator :: Generator -> IO (FunPtr Generator)

type Completer = Ptr CChar -> CInt -> CInt -> IO (Ptr (Ptr CChar))

setAttemptedCompletionFunction
    :: Maybe (String -> Int -> Int -> IO (Maybe (String, [String]))) -> IO ()
setAttemptedCompletionFunction fun =
    setFunPtr rl_attempted_completion_function fun $ \f ->
        exportCompleter $ \textPtr start end -> do
            text <- peekCString textPtr
            found <- f text (fromIntegral start) (fromIntegral end)
            case found of
                Nothing -> return nullPtr
                Just (text', matches) -> do
                    matchPtrs <- mapM newCString (text':matches)
                    matchesPtr <- mallocArray (length matchPtrs + 1)
                    pokeArray0 nullPtr matchesPtr matchPtrs
                    return matchesPtr

foreign import ccall "&"   rl_attempted_completion_function :: Ptr (FunPtr Completer)
foreign import ccall "wrapper"
    exportCompleter :: Completer -> IO (FunPtr Completer)

#if HAVE_READLINE_5
type StringFunc = IO (Ptr CChar)

foreign import ccall "&" rl_completion_word_break_hook 
    :: Ptr (FunPtr StringFunc)

foreign import ccall "wrapper" 
    exportStringFunc :: StringFunc -> IO (FunPtr StringFunc)

setCompletionWordBreakHook
    :: Maybe (IO (Maybe String)) -> IO ()
setCompletionWordBreakHook fun = 
    setFunPtr rl_completion_word_break_hook fun $ \f ->
        exportStringFunc $ do
            wordBreaks <- f
            case wordBreaks of
                Nothing -> return nullPtr
                Just wordBreaksString -> newCString wordBreaksString

#endif

type Quoter = Ptr CChar -> CInt -> Ptr CChar -> IO (Ptr CChar)

setFilenameQuotingFunction
    :: Maybe (String -> Bool -> Ptr CChar -> IO String) -> IO ()
setFilenameQuotingFunction fun =
    setFunPtr_freeIf
        (\oldPtr -> oldPtr /= nullFunPtr && oldPtr /= rl_quote_filename)
        rl_filename_quoting_function fun $ \f ->
        exportQuoter $ \textPtr typ qp -> do
            text <- peekCString textPtr
            s <- f text (typ == (#const MULT_MATCH)) qp
            newCString s
foreign import ccall "&"  rl_filename_quoting_function :: Ptr (FunPtr Quoter)
foreign import ccall "wrapper"
  exportQuoter :: Quoter -> IO (FunPtr Quoter)

-- We must not freeHaskellFunPtr the original value of the
-- rl_filename_quoting_function variable, because it's a native C
-- function. But this value, rl_quote_filename, is a static function,
-- not exported by readline. So we read it from the variable at the
-- beginning and store it in a Haskell's global variable. We also
-- export its Haskell translation to be able to restore its behavior
-- by setFilenameQuotingFunction.

{-# NOINLINE rl_quote_filename #-}
rl_quote_filename :: FunPtr Quoter
rl_quote_filename = unsafePerformIO $ peek rl_filename_quoting_function

quoteFilename :: String -> Bool -> Ptr CChar -> IO String
quoteFilename text typ qp = do
    ptr <- withCString text $ \textPtr ->
        importQuoter rl_quote_filename
            textPtr
            (if typ then (#const SINGLE_MATCH) else (#const MULT_MATCH))
            qp
    s <- peekCString ptr
    free ptr
    return s
foreign import ccall "dynamic" importQuoter :: FunPtr Quoter -> Quoter

type Dequoter = Ptr CChar -> CInt -> IO (Ptr CChar)

setFilenameDequotingFunction :: Maybe (String -> Maybe Char -> IO String) -> IO ()
setFilenameDequotingFunction fun =
    setFunPtr rl_filename_dequoting_function fun $ \f ->
        exportDequoter $ \textPtr qc -> do
            text <- peekCString textPtr
            s <- f text (if qc==0 then Nothing else Just (chr (fromIntegral qc)))
            newCString s

foreign import ccall "&"rl_filename_dequoting_function :: Ptr (FunPtr Dequoter)

foreign import ccall "wrapper" 
  exportDequoter :: Dequoter -> IO (FunPtr Dequoter)

type IsQuoted = Ptr CChar -> CInt -> IO CInt

setCharIsQuotedP :: Maybe (String -> Int -> IO Bool) -> IO ()
setCharIsQuotedP fun =
    setFunPtr rl_char_is_quoted_p fun $ \f ->
        exportIsQuoted $ \textPtr index -> do
            text <- peekCString textPtr
            quoted <- f text (fromIntegral index)
            return (if quoted then 1 else 0)
foreign import ccall "&" rl_char_is_quoted_p :: Ptr (FunPtr IsQuoted)

foreign import ccall "wrapper"
  exportIsQuoted :: IsQuoted -> IO (FunPtr IsQuoted)

getCompletionQueryItems :: IO Int
getCompletionQueryItems =
    liftM fromIntegral (peek rl_completion_query_items)

setCompletionQueryItems :: Int -> IO ()
setCompletionQueryItems items =
    poke rl_completion_query_items (fromIntegral items)

foreign import ccall "&" rl_completion_query_items :: Ptr CInt

getBasicWordBreakCharacters :: IO String
getBasicWordBreakCharacters = getCharacters rl_basic_word_break_characters

setBasicWordBreakCharacters :: String -> IO ()
setBasicWordBreakCharacters =
    setCharacters_freeIf
        (/= orig_rl_basic_word_break_characters)
        rl_basic_word_break_characters

foreign import ccall "&" rl_basic_word_break_characters :: Ptr (Ptr CChar)

-- Similarly to rl_quote_filename, we must be able to detect the
-- original pointer to a static char array.

{-# NOINLINE orig_rl_basic_word_break_characters #-}
orig_rl_basic_word_break_characters :: Ptr CChar
orig_rl_basic_word_break_characters = unsafePerformIO $
    peek rl_basic_word_break_characters

getBasicQuoteCharacters :: IO String
getBasicQuoteCharacters = getCharacters rl_basic_quote_characters

setBasicQuoteCharacters :: String -> IO ()
setBasicQuoteCharacters =
    setCharacters_freeIf
        (/= orig_rl_basic_quote_characters)
        rl_basic_quote_characters

foreign import ccall "&" rl_basic_quote_characters :: Ptr (Ptr CChar)

{-# NOINLINE orig_rl_basic_quote_characters #-}
orig_rl_basic_quote_characters :: Ptr CChar
orig_rl_basic_quote_characters = unsafePerformIO $
    peek rl_basic_quote_characters

getCompleterWordBreakCharacters :: IO String
getCompleterWordBreakCharacters = getCharacters rl_completer_word_break_characters

setCompleterWordBreakCharacters :: String -> IO ()
setCompleterWordBreakCharacters =
    setCharacters_freeIf
        (\oldPtr -> oldPtr /= nullPtr &&
                    oldPtr /= orig_rl_basic_word_break_characters)
        rl_completer_word_break_characters

foreign import ccall "&" rl_completer_word_break_characters :: Ptr (Ptr CChar)

getCompleterQuoteCharacters :: IO String
getCompleterQuoteCharacters = getCharacters rl_completer_quote_characters

setCompleterQuoteCharacters :: String -> IO ()
setCompleterQuoteCharacters cs = do
    oldPtr <- peek rl_completer_quote_characters
    when (oldPtr /= nullPtr) $ free oldPtr
    -- I think that rl_completer_quote_characters should never be empty
    -- but can be NULL.
    newPtr <- if null cs
        then return nullPtr
        else do
            ptr <- mallocArray (length cs + 1)
            pokeArray0 0 ptr (map castCharToCChar cs)
            return ptr
    poke rl_completer_quote_characters newPtr

foreign import ccall "&" rl_completer_quote_characters :: Ptr (Ptr CChar)

getFilenameQuoteCharacters :: IO String
getFilenameQuoteCharacters = getCharacters rl_filename_quote_characters

setFilenameQuoteCharacters :: String -> IO ()
setFilenameQuoteCharacters = setCharacters rl_filename_quote_characters

foreign import ccall "&" rl_filename_quote_characters :: Ptr (Ptr CChar)

getSpecialPrefixes :: IO String
getSpecialPrefixes = getCharacters rl_special_prefixes

setSpecialPrefixes :: String -> IO ()
setSpecialPrefixes = setCharacters rl_special_prefixes

foreign import ccall "&" rl_special_prefixes :: Ptr (Ptr CChar)

getCompletionAppendCharacter :: IO (Maybe Char)
getCompletionAppendCharacter = do
    ch <- peek rl_completion_append_character
    return $ if ch == 0 then Nothing else Just (chr (fromIntegral ch))

setCompletionAppendCharacter :: Maybe Char -> IO ()
setCompletionAppendCharacter ch =
    poke rl_completion_append_character (maybe 0 (fromIntegral . ord) ch)

foreign import ccall "&" rl_completion_append_character :: Ptr CInt

setIgnoreCompletionDuplicates :: Bool -> IO ()
setIgnoreCompletionDuplicates ign =
    poke rl_ignore_completion_duplicates (if ign then 1 else 0)

getIgnoreCompletionDuplicates :: IO Bool
getIgnoreCompletionDuplicates =
    liftM (/= 0) (peek rl_ignore_completion_duplicates)

foreign import ccall "&" rl_ignore_completion_duplicates :: Ptr CInt

setFilenameCompletionDesired :: Bool -> IO ()
setFilenameCompletionDesired comp =
    poke rl_filename_completion_desired (if comp then 1 else 0)

getFilenameCompletionDesired :: IO Bool
getFilenameCompletionDesired =
    liftM (/= 0) (peek rl_filename_completion_desired)

foreign import ccall "&" rl_filename_completion_desired :: Ptr CInt

setFilenameQuotingDesired :: Bool -> IO ()
setFilenameQuotingDesired quot =
    poke rl_filename_quoting_desired (if quot then 1 else 0)

getFilenameQuotingDesired :: IO Bool
getFilenameQuotingDesired =
    liftM (/= 0) (peek rl_filename_quoting_desired)

foreign import ccall "&" rl_filename_quoting_desired :: Ptr CInt

setInhibitCompletion :: Bool -> IO ()
setInhibitCompletion inh = poke rl_inhibit_completion (if inh then 1 else 0)

getInhibitCompletion :: IO Bool
getInhibitCompletion = liftM (/= 0) (peek rl_inhibit_completion)

foreign import ccall "&" rl_attempted_completion_over :: Ptr CInt

getAttemptedCompletionOver :: IO Bool
getAttemptedCompletionOver = 
    liftM (/=0) (peek rl_attempted_completion_over)

setAttemptedCompletionOver :: Bool -> IO ()
setAttemptedCompletionOver over = 
    poke rl_attempted_completion_over (if over then 1 else 0)

foreign import ccall "&" rl_inhibit_completion :: Ptr CInt

type Ignorer = Ptr (Ptr CChar) -> IO CInt

setIgnoreSomeCompletionsFunction :: Maybe ([String] -> IO [String]) -> IO ()
-- The function may not make the list longer!
setIgnoreSomeCompletionsFunction fun =
    setFunPtr rl_ignore_some_completions_function fun $ \f ->
        exportIgnorer $ \matchesPtr -> do
            matchPtrs <- peekArray0 nullPtr matchesPtr
            matches <- mapM peekCString matchPtrs
            mapM_ free matchPtrs
            f matches >>= mapM newCString >>= pokeArray0 nullPtr matchesPtr
            return 0
foreign import ccall "&" rl_ignore_some_completions_function :: Ptr (FunPtr Ignorer)

foreign import ccall "wrapper"
  exportIgnorer :: Ignorer -> IO (FunPtr Ignorer)

type DirCompleter = Ptr (Ptr CChar) -> IO CInt

setDirectoryCompletionHook :: Maybe (String -> IO String) -> IO ()
setDirectoryCompletionHook fun =
    setFunPtr rl_directory_completion_hook fun $ \f ->
        exportDirCompleter $ \dirPtrPtr -> do
            oldDirPtr <- peek dirPtrPtr
            oldDir <- peekCString oldDirPtr
            free oldDirPtr
            newDirPtr <- f oldDir >>= newCString
            poke dirPtrPtr newDirPtr
            return 0
foreign import ccall "&" rl_directory_completion_hook :: Ptr (FunPtr DirCompleter)
foreign import ccall "wrapper"
    exportDirCompleter :: DirCompleter -> IO (FunPtr DirCompleter)

#if HAVE_READLINE_4
type Displayer = Ptr (Ptr CChar) -> CInt -> CInt -> IO ()

setCompletionDisplayMatchesHook :: Maybe ([String] -> IO ()) -> IO ()
setCompletionDisplayMatchesHook fun =
    setFunPtr rl_completion_display_matches_hook fun $ \f ->
        exportDisplayHook $ \matchesPtr _ _ ->
            peekArray0 nullPtr matchesPtr >>= mapM peekCString >>= f
foreign import ccall "&" rl_completion_display_matches_hook :: Ptr (FunPtr Displayer)
foreign import ccall "wrapper"
    exportDisplayHook :: Displayer -> IO (FunPtr Displayer)
#endif

setCharacters_freeIf :: (Ptr CChar -> Bool) -> Ptr (Ptr CChar) -> String -> IO ()
setCharacters_freeIf pred variable chars = do
    oldPtr <- peek variable
    when (pred oldPtr) $ free oldPtr
    newPtr <- mallocArray (length chars + 1)
    pokeArray0 0 newPtr (map castCharToCChar chars)
    poke variable newPtr

setCharacters :: Ptr (Ptr CChar) -> String -> IO ()
setCharacters = setCharacters_freeIf (/= nullPtr)

getCharacters :: Ptr (Ptr CChar) -> IO String
getCharacters variable = do
    ptr <- peek variable
    if ptr == nullPtr then return "" else do
        cs <- peekArray0 0 ptr
        return (map castCCharToChar cs)
