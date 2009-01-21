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

module MiniChess.Core where

import MiniChess.Types
import Data.Array
import Data.List (find)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
-----------------------------------------------------------------------------
-- Move Generator
-----------------------------------------------------------------------------

moveGen :: GameState -> [Move]
moveGen (GameState turn c (Board b)) = concat [move sp | sp <- movingPieces]
  where isMovingColor :: BoardPiece -> Bool
        isMovingColor Empty = False
        isMovingColor (BP p pc) = pc == c

        -- movingPieces is a list of all the player on move's pieces.
        movingPieces :: [(Square, PieceType)]
        movingPieces = map (\(x, (BP p _)) -> (x, p))
                           (filter (isMovingColor . snd) (assocs b))

        -- followUnblocked returns a list of all possible squares one can reach
        -- in a straight line from a starting square, stopping when blocked by
        -- a piece of the moving player's color.  Includes moves which would
        -- capture an opposing piece.  Second argument is row/column deltas,
        -- allowing one to check vertical, horizontal, or diagonal moves.
        followUnblocked :: Square -> (Int, Int) -> [Square] -> [Square]
        followUnblocked (i,j) (di,dj) acc | not (isBoardIndex (i,j)) || isMovingColor (b!(i,j)) = acc
                                          | Empty == (b!(i,j)) = followUnblocked (i+di,j+dj) (di,dj) ((i,j):acc)
                                          | otherwise = (i,j):acc -- capture move

        -- move packages up generated moves, real work is done by dest.
        move :: (Square, PieceType) -> [Move]
        move (s,p) = [Move s d | d <- dest (s,p)]

        -- dest generates a list of all possible destination squares, following
        -- the movement rules for the piece starting in the given square.
        -- All destinations are guaranteed to be valid; includes capture moves.
        dest :: (Square, PieceType) -> [Square]
        -- Kings can move or attack 1 space in any direction.
        dest ((i,j), K) = [d | d <- [(i-1,j),(i+1,j),(i,j-1),(i,j+1),(i-1,j-1),(i+1,j-1), (i-1,j+1),(i+1,j+1)], isBoardIndex d, not (isMovingColor (b!d))]
        -- Queens, rooks, and bishops can move in straight lines...
        dest ((i,j), Q) = dest ((i,j), R) ++ dest ((i,j), B)
        dest ((i,j), B) = followUnblocked (i-1,j-1) (-1, -1)
                        $ followUnblocked (i-1,j+1) (-1,  1)
                        $ followUnblocked (i+1,j-1) ( 1, -1)
                        $ followUnblocked (i+1,j+1) ( 1,  1)
                        $ []
        dest ((i,j), R) = followUnblocked (i-1,j) (-1, 0)
                        $ followUnblocked (i+1,j) ( 1, 0)
                        $ followUnblocked (i,j-1) ( 0,-1)
                        $ followUnblocked (i,j+1) ( 0, 1)
                        $ []
        -- Knights can move in L-shaped patterns, jumping over other pieces.
        dest ((i,j), N) = [d | d <- [(i-2,j-1),(i-2,j+1),(i+2,j-1),(i+2,j+1),(i-1,j-2),(i+1,j-2),(i-1,j+2),(i+1,j+2)], isBoardIndex d && not (isMovingColor (b!d))]
        -- Pawns are a bit special---they can move up/down 1 space (based on
        -- color) only if the space is empty, but they must attack diagonally.
        dest ((i,j), P) = [d | d <- [(i+dirn,j)], isBoardIndex d, (b!d) == Empty] ++ [d | d <- [(i+dirn,j-1),(i+dirn,j+1)], isBoardIndex d, (b!d) /= Empty, not (isMovingColor (b!d))]
          where dirn = if c == White then -1 else 1

-- Handles pawn promotion
pieceAfterMove :: BoardPiece -> Move -> BoardPiece
pieceAfterMove p (Move _ (i',j')) = case (p, i') of
    (BP P White, 0) -> BP Q White -- pawn promotion
    (BP P Black, 5) -> BP Q Black
    (x, _)              -> x       -- normal pieces just move

-- executeBoardMove transforms the board, given a valid move.  Doesn't check
-- the validity of the move...it should be supplied by moveGen.
executeBoardMove :: Board -> Move -> Board
executeBoardMove (Board b) m@(Move ij i'j') = Board (b//[(ij, Empty), (i'j', pieceAfterMove (b!ij) m)])

executeMove :: GameState -> Move -> GameState
executeMove (GameState turns c b) move = GameState (if c == Black
                                                      then turns + 1
                                                      else turns)
                                                   (notColor c)
                                                   (executeBoardMove b move)

executeScoredMove :: (GameState, Score) -> Move -> (GameState, Score)
executeScoredMove (g, s) m = (executeMove g m, partialEval (g, s) m)

-- gameOver returns the current game status - draw, victory, or in progress.
-- A game ends after 40 moves, or if the current player has no king.
gameOver :: GameState -> EndCondition
gameOver g@(GameState t c (Board b)) =
  case find king (elems b) of
    Nothing -> Victory (notColor c)
    Just _  -> if t > turnLimit then Draw else InProgress
  where king (BP K c') = c == c'
        king  _        = False

-- [mc-6] Mini-chess state evaluator.
-- http://wiki.cs.pdx.edu/cs542/mini-chess/eval.html

pieceVal :: PieceType -> Score
pieceVal K = 100000 -- worth more than anything
pieceVal Q = 900
pieceVal R = 500
pieceVal N = 400
pieceVal B = 300
pieceVal P = 100

stateValue :: GameState -> Score -- [not normalized]
stateValue g@(GameState turns c (Board b)) = totalScore
  where coloredPieceVal :: BoardPiece -> Score
        coloredPieceVal (BP p pc) = if pc == c then pieceVal p else -pieceVal p
        coloredPieceVal Empty = 0
        totalScore = case gameOver g of
                          Victory c' -> (if c == c' then 1 else -1) * (infinity - turns)
                          _          -> sum (map coloredPieceVal (elems b))

partialEval :: (GameState, Score) -> Move -> Score
partialEval (GameState turns c (Board b), s) m@(Move ij i'j') = -totalScore -- flipped since side changes
  where boardPieceVal (BP p _) = pieceVal p
        boardPieceVal Empty = 0

        captureScore = boardPieceVal (b!i'j')

        promotionScore = boardPieceVal (pieceAfterMove (b!ij) m) - boardPieceVal (b!ij)

        totalScore = case b!i'j' of
                          (BP K _) -> infinity - (turns + 1)
                          _        -> s + captureScore + promotionScore

negamax :: (GameState, Score) -> Int -> Score -> Score -> Score
negamax (g@(GameState turns _ _),v) d a b | d == 0 || v < -999999 || v > 999999 || turns > turnLimit = v
                                          | otherwise = f (moveGen g) a
  where f :: [Move] -> Score -> Score
        f [] a = a
        f (m:ms) a | a' >= b = a'
                   | otherwise = f ms a'
            where a' = max a (if d == 1
                                 then -partialEval (g,v) m
                                 else -negamax (executeScoredMove (g,v) m) (d-1) (-b) (-a))

pickMove :: Int -> GameState -> (Move, Score)
pickMove d g = f ms a m
  where (m:ms) = moveGen g
        a = -negamax (executeScoredMove (g, stateValue g) m) d (-infinity) infinity
        f :: [Move] -> Score -> Move -> (Move, Score)
        f [] a bestM = (bestM, a)
        f (m:ms) a bestM = f ms a' m'
            where a' = max a (-negamax (executeScoredMove (g, stateValue g) m) d (-infinity) (-a))
                  m' = if a' > a then m else bestM

-- Iterative Deepening Players - the ones you actually want to play
iterativePlayer :: Player
iterativePlayer g =
  do moveVar <- newMVar $ head (moveGen g) -- some dumb move, just so we have one at any time
     id <- forkIO $ iterated moveVar
     milliSleep 5000
     m <- takeMVar moveVar
     killThread id
     return m
  where milliSleep = threadDelay . (*) 1000
        color = (\(GameState _ c _) -> c) g
        selectAtDepth :: MVar Move -> Int -> IO ()
        selectAtDepth moveVar d = do let (m, v) = pickMove d g
                                     --putStrLn ("Depth " ++ show d ++ " suggests " ++ show m ++ " with expected value " ++ show v)
                                     swapMVar moveVar m
                                     return ()
        iterated moveVar = mapM_ (selectAtDepth moveVar) [1..15]

singleDepthPlayer :: Int -> Player
singleDepthPlayer d g = do let (m,v) = pickMove d g
                           --putStrLn ("Depth " ++ show d ++ " suggests " ++ show m ++ " with expected value " ++ show v)
                           return m

-- Manual move input (for human players)
{-
humanPlayer :: Player
humanPlayer g = do let moves = moveGen g
                   print moves
                   l <- getLine
                   let m = read l
                   if elem m (moveGen g)
                      then return m
                      else do putStrLn "Invalid move"
                              humanPlayer g
-}

-- Helper function.  Plays a game starting from some initial state, given a
-- function to select a move.
playGameFrom :: (String -> IO ()) -> (Color -> Player) -> GameState -> IO ()
playGameFrom putStr _ g | over /= InProgress = -- the case for game over
  do print g
     print over
  where over = gameOver g
        print x = putStr (show x) >> putStr "\n"
playGameFrom putStr player g@(GameState _ c (Board b)) = -- game still going
  do print g
     putChar '\n'
     m <- player c g
     putStrLn (movemsg m)
     putChar '\n'
     playGameFrom putStr player (executeMove g m)
  where movemsg m@(Move ij i'j') =
           show c ++ " moving " ++ show (b!ij) ++ " " ++ show m
                  ++ case b!i'j' of
                       Empty -> ""
                       p -> " capturing " ++ show p
        putChar  x = putStr [x]
        putStrLn x = putStr x >> putStr "\n"
        print      = putStrLn . show
