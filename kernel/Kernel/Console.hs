module Kernel.Console
    ( Console
    , putString, putStringLn
    , Kernel.Console.putChar, putChar'
    , clearScreen
    , moveCursorBackward
    , clearEOL
    ) where

{-P:
import Prelude hiding (putChar)
-}
import H.Monad(H)
import H.Concurrency
import Control.Monad ( when )

import Kernel.Types.Console

defaultAttrs :: VideoAttributes
defaultAttrs = 0x17

putString :: Console -> String -> H ()
putString (Console lock con) str =
    withLock lock $ writeList2Chan (consoleChan con) $ map putc str

putc '\n' = NewLine
putc c = PutChar defaultAttrs c

putStringLn :: Console -> String -> H ()
putStringLn (Console lock con) str =
    withLock lock $ do writeList2Chan (consoleChan con) $ map putc str
	               writeChan (consoleChan con) NewLine

putChar :: Console -> Char -> H ()
putChar (Console lock con) char =
    withLock lock $ writeChan (consoleChan con) $ putc char

putChar' :: Console -> VideoAttributes -> Char -> H ()
putChar' (Console lock con) attrs char =
    withLock lock $ writeChan (consoleChan con) $ PutChar attrs char

clearScreen :: Console -> H ()
clearScreen (Console lock con) =
    withLock lock $ writeChan (consoleChan con) $ ClearScreen

moveCursorBackward :: Console -> Int -> H ()
moveCursorBackward (Console lock con) count =
    withLock lock $ writeChan (consoleChan con) $ MoveCursorBackward count

clearEOL :: Console -> H ()
clearEOL (Console lock con) =
    withLock lock $ writeChan (consoleChan con) ClearEOL

isValidPosition :: ConsoleData -> Row -> Col -> H Bool
isValidPosition con row col =
    let height = consoleHeight con
	width = consoleWidth con
     in return (row < 0 || col < 0 || row >= height || col >= width)

checkPosition :: ConsoleData -> Row -> Col -> H ()
checkPosition console row col =
    do valid <- isValidPosition console row col
       when (not valid) $ fail "Invalid position"
