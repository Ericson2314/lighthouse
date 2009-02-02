module Kernel.Types.Console where

import H.Concurrency(Chan)
import Data.Word ( Word8 )
import Control.Concurrent.Lock

type VideoAttributes = Word8

type Row = Int
type Col = Int

data ConsoleCommand
    = NewLine
    | CarriageReturn
    | ClearEOL
    | PutChar VideoAttributes Char
    | MoveCursorBackward Int
    | ClearScreen

data ConsoleData = ConsoleData
    { consoleChan :: Chan ConsoleCommand
    , consoleHeight :: Int
    , consoleWidth :: Int
    }

data Console = Console Lock ConsoleData
