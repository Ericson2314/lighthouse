{-# OPTIONS_GHC -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.SimpleLineEditor
-- Copyright   :  (c) 2000,2003, Malcolm Wallace
-- License     :  GPL (if it depends on readline, which is GPL)
--                BSD (otherwise)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (unix-specific at the moment)
--
-- A simple line editor, using the GNU readline library if available,
-- or a small emulation otherwise.
--
-----------------------------------------------------------------------------

module System.Console.SimpleLineEditor
  ( initialise		--	:: IO ()
  , restore		--	:: IO ()
  , getLineEdited	--	:: String -> IO (Maybe String)
  , delChars		--	:: String -> IO ()
  ) where

import System.IO (stdin, stdout, BufferMode(..), hSetBuffering)
import Control.Monad (when)
import Data.Char (isSpace)
import Data.Maybe (isJust, fromJust)
#if USE_READLINE
import System.Console.Readline
#else
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Cmd (system)
import System.IO (hGetChar)
import System.IO.Unsafe (unsafePerformIO)
#endif

-- | Set up the environment so that the terminal passes characters directly
--   into the Haskell program, for immediate interpretation by the line editor.
initialise :: IO ()
initialise = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  NoBuffering
#if USE_READLINE
    initialize
#else
    -- The following call is probably non-portable.  Better suggestions?
    -- Note, we turn OFF terminal echoing of input characters
    system("stty -icanon min 1 -echo")
    return ()
#endif

-- | Restore the environment so that the terminal is usable in normal
--   mode once again.
restore :: IO ()
restore = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin  LineBuffering
#if ! USE_READLINE
    -- The following call is probably non-portable.  Better suggestions?
    -- We assume the terminal should echo input characters after restoration
    system("stty icanon echo")
    return ()
#endif

-- | Remove the given string from immediately behind (to the left of) the
--   current cursor position.
delChars :: String -> IO ()
delChars []     = return ()
delChars (_:xs) = do putStr "\BS \BS"
                     delChars xs

-- | 'getLineEdited p' uses the string @p@ as a prompt, and returns a line
--   of input from the user.  The user can edit the line in-place before
--   completion, using common readline-like command keys.  (The real readline
--   is used when available, or otherwise a simplified emulation.)

#if USE_READLINE

getLineEdited :: String -> IO (Maybe String)
getLineEdited prompt = do
  ms <- readline prompt
  case ms of 
    Nothing -> return ms
    Just s  -> when (not (all isSpace s)) (addHistory s) >> return ms

#else

-- nasty imperative state holds the command history
history :: IORef [String]
history = unsafePerformIO (newIORef [])

getLineEdited :: String -> IO (Maybe String)
getLineEdited prompt = do
    putStr prompt
    previous <- readIORef history
    ms <- gl "" 0 ([],previous)
    case ms of 
      Nothing -> return ms
      Just s  -> do when (not (all isSpace s))
                         (writeIORef history (reverse s: previous))
                    return ms
  where
    gl s 0 hist = do	-- s is accumulated line (in reverse)
			-- 0 is cursor position FROM THE END of the string
      cmd <- lineCmd
      case cmd of
        Char c   -> putChar c >> gl (c:s) 0 hist
        Accept   -> return (Just (reverse s))
        Cancel   -> return Nothing
        Delete L -> if null s then gl s 0 hist
                    else delChars "_" >> gl (tail s) 0 hist
        Delete Begin -> delChars s >> gl "" 0 hist
        Move L   -> if not (null s) then putStr ("\BS") >> gl s 1 hist
                    else gl s 0 hist
        History  -> case hist of
                      (_fut, [])    -> gl s 0 hist
                      (fut, p:past) -> do delChars s
                                          putStr (reverse p)
                                          gl p 0 (s:fut, past)
        Future   -> case hist of
                      ([], _past)   -> gl s 0 hist
                      (f:fut, past) -> do delChars s
                                          putStr (reverse f)
                                          gl f 0 (fut, s:past)
        _        -> gl s 0 hist

    gl s n hist = do	-- s is accumulated line, n(/=0) is cursor position
      cmd <- lineCmd
      case cmd of
        Char c   -> do putStr (c: reverse (take n s))
                       putStr (replicate n '\BS')
                       gl (take n s ++ c: drop n s) n hist
        Accept   -> return (Just (reverse s))
        Cancel   -> return Nothing
        Move R   -> do let n1 = n-1
                       putStr (reverse (take n s)++" ")
                       putStr (replicate n '\BS')
                       gl s n1 hist
        Delete R -> do let n1 = n-1
                       putStr (reverse (take n1 s) ++ " ")
                       putStr (replicate (n1+1) '\BS')
                       gl (take n1 s ++ drop n s) n1 hist
        Move L   -> do let n1 = n+1
                       if n1 <= length s then do
                           putStr ('\BS':reverse (take n1 s))
                           putStr (replicate n1 '\BS')
                           gl s n1 hist
                         else do
                           putStr (reverse s++" ")
                           putStr (replicate n1 '\BS')
                           gl s n hist
        Delete L -> do let n1 = n+1
                       if n1 <= length s then do
                           putStr ('\BS':reverse (take n s)++" ")
                           putStr (replicate n1 '\BS')
                           gl (take n s ++ drop n1 s) n hist
                         else do
                           putStr (reverse s++" ")
                           putStr (replicate n1 '\BS')
                           gl s n hist
        History  -> case hist of
                      (_fut, [])    -> gl s n hist
                      (fut, p:past) -> do putStr (replicate n ' ')
                                          delChars s
                                          putStr (reverse p)
                                          gl p 0 (s:fut, past)
        Future   -> case hist of
                      ([], _past)   -> gl s n hist
                      (f:fut, past) -> do putStr (replicate n ' ')
                                          delChars s
                                          putStr (reverse f)
                                          gl f 0 (fut, s:past)
        _        -> gl s n hist


-- Define a mini-command language, to separate the lexing of input
-- commands from their interpretation.  Note there is room for expansion
-- here, e.g. commands include word-at-a-time movement, but we don't
-- currently have a key binding for that.
data LineCmd = Char Char | Move Cursor | Delete Cursor
             | Accept | Cancel | History | Future | NoOp
data Cursor  = L | R | Begin | End -- not implemented yet: | WordL | WordR

-- This little lexer for keystrokes does a reasonable job, but there
-- are plenty of problems.  E.g. the backspace key might generate a
-- ^H character and not display it, which results in a mismatched cursor
-- position.  Behaviour is highly dependent on terminal settings I imagine.
lineCmd :: IO LineCmd
lineCmd = do
    c1 <- hGetChar stdin
    case c1 of
      '\n'   -> putChar '\n' >> return Accept
      '\^K'  -> putChar '\n' >> return Cancel
      '\DEL' -> return (Delete L)
      '\BS'  -> return (Delete L)
      '\^L'  -> return (Move R)
      '\^[' -> do
        c2 <- hGetChar stdin
        case c2 of
          'k' -> return History
          'j' -> return Future
          '[' -> do
              c3 <- hGetChar stdin
              case c3 of
                'D' -> return (Move L)
                'C' -> return (Move R)
                'A' -> return History
                'B' -> return Future
                '3' -> do c <- hGetChar stdin
                          case c of
                            '~' -> return (Delete R)
                            _   -> return NoOp
                '4' -> do c <- hGetChar stdin
                          case c of
                            '~' -> return (Move End)
                            _   -> return NoOp
                '1' -> do c <- hGetChar stdin
                          case c of
                            '~' -> return (Move Begin)
                            _   -> return NoOp
                _   -> return NoOp
          'O' -> do
              c3 <- hGetChar stdin
              case c3 of
                'D' -> return (Move L)
                'C' -> return (Move R)
                'A' -> return History
                'B' -> return Future
                _   -> return NoOp
          _   -> return NoOp
      _ -> return (Char c1)

#endif /* USE_READLINE */

{-
-- | A simple interactive test for the line-editing functionality.

-- (This illustrates the necessary use of 'initialise' and 'restore'
--  as brackets around the editing loop.)
testIt :: IO ()
testIt = initialise >> loop >> restore
  where loop = do l <- getLineEdited "prompt> "
                  when (isJust l) (putStrLn (fromJust l))
                  when (l/=Just "quit") loop
-}
