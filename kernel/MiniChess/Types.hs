{-
 - Mini-Chess in Haskell
 - Copyright (C) 2007 Kenny Graunke <kennyg@cs.pdx.edu>
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation (version 2 of the License) This program
 - is distributed in the hope that it will be useful, but WITHOUT ANY
 - WARRANTY; without even the implied warranty of MERCHANTABILITY or
 - FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 - for more details. You should have received a copy of the GNU General
 - Public License along with this program; if not, write to the Free
 - Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 - 02111-1307, USA.
 -}

module MiniChess.Types where

import Data.Array
import Data.Char (toLower)

-----------------------------------------------------------------------------
-- Data Definitions 
-----------------------------------------------------------------------------

data Color = White | Black deriving (Show, Eq)
data PieceType = K | Q | B | N | R | P deriving (Show, Read, Eq, Ord, Enum, Ix)
data BoardPiece = Empty | BP PieceType Color deriving Eq

-- The board.  Each square on the board is referenced by (i, j) coordinate
-- pairs; the board is simply an immutable array indexed by such pairs.
type Square = (Int, Int)
newtype Board = Board (Array Square BoardPiece)

-- Game state is the turn number, who's about to move, and the current board.
data GameState = GameState Int Color Board
data EndCondition = Victory Color | Draw | InProgress deriving (Show, Eq)

-- Moves are simply a pair of squares: from/source square -> to/dest. square.
data Move = Move Square Square deriving Eq

type Player = GameState -> IO Move

type Score = Int

-- Never, -ever- use minBound in negamax: -minBound is still negative!
infinity :: Int
infinity = maxBound - 1

turnLimit :: Int
turnLimit = 40
-----------------------------------------------------------------------------
-- Type Class Instances: Show and Read
-----------------------------------------------------------------------------

instance Show Move where
  show (Move (i,j) (i',j')) = ["abcde" !! j]  ++ show (6-i) ++ "-"
                           ++ ["abcde" !! j'] ++ show (6-i')

instance Read Move where
  readsPrec _ (a:b:'-':d:e:cs) | a >= 'a' && a <= 'e' && d >= 'a' && d <= 'e' && b >= '0' && b <= '6' && e >= '0' && e <= '6' = [(Move (6-(fromEnum b - fromEnum '0'), fromEnum a - fromEnum 'a') (6-(fromEnum e - fromEnum '0'), fromEnum d - fromEnum 'a'), cs)]

instance Show BoardPiece where
  show Empty = "."
  show (BP p White) = show p               -- White pieces are 'K', 'Q', and so on.
  show (BP p Black) = map toLower (show p) -- Black pieces are 'k', 'q', and so on.

instance Read BoardPiece where
  readsPrec _ ('.':cs) = [(Empty, cs)]
  readsPrec _ ('K':cs) = [(BP K White, cs)]
  readsPrec _ ('k':cs) = [(BP K Black, cs)]
  readsPrec _ ('Q':cs) = [(BP Q White, cs)]
  readsPrec _ ('q':cs) = [(BP Q Black, cs)]
  readsPrec _ ('B':cs) = [(BP B White, cs)]
  readsPrec _ ('b':cs) = [(BP B Black, cs)]
  readsPrec _ ('N':cs) = [(BP N White, cs)]
  readsPrec _ ('n':cs) = [(BP N Black, cs)]
  readsPrec _ ('R':cs) = [(BP R White, cs)]
  readsPrec _ ('r':cs) = [(BP R Black, cs)]
  readsPrec _ ('P':cs) = [(BP P White, cs)]
  readsPrec _ ('p':cs) = [(BP P Black, cs)]

instance Show Board where
  show (Board a) = f 0 (assocs a) ""
    where f :: Int -> [((Int,Int), BoardPiece)] -> String -> String
          f _ [] acc = acc
          f l xss@(((i,j), m):xs) acc | l < i = f (l + 1) xss (acc ++ "\n")
          f l ((_, cp):xs) acc = f l xs (acc ++ show cp)

instance Read Board where
  readsPrec _ s = [(Board (listArray ((0,0), (5,4)) (map rd $ concat rows)), unlines rest)]
    where (rows, rest) = splitAt 6 . lines $ s
          rd c = read [c]

instance Show GameState where
  show (GameState move White b) = show move ++ " W\n" ++ show b
  show (GameState move Black b) = show move ++ " B\n" ++ show b

instance Read GameState where
  readsPrec _ s = [(GameState turns color board, rest)]
    where turns :: Int
          turns = (read . fst . head . lex) s
          color = if (fst . head . lex . snd . head . lex) s == "W"
                     then White
                     else Black
          board :: Board
          board = read (unlines (drop 1 (lines s)))
          rest = unlines (drop 7 (lines s))

-----------------------------------------------------------------------------
-- Helpful Functions                    
-----------------------------------------------------------------------------
notColor :: Color -> Color
notColor White = Black
notColor Black = White

-- Since Squares don't officially bound their values to (0..5, 0..4), this
-- function checks to make sure a move is really valid.  Convenient because
-- one can generate all moves and then discard those which would be off the
-- board.
isBoardIndex :: Square -> Bool
isBoardIndex = inRange ((0,0), (5,4))

