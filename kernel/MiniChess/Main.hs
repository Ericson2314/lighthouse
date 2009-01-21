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

module MiniChess.Main where

import MiniChess.Types
import MiniChess.Core

start :: GameState
start = read "1 W\nkqbnr\nppppp\n.....\n.....\nPPPPP\nRNBQK"

main putStr = do let whitePlayer = singleDepthPlayer 5
                 let blackPlayer = singleDepthPlayer 6
                 --let whitePlayer = humanPlayer
                 --let whitePlayer = iterativePlayer
                 --let blackPlayer = iterativePlayer
                 playGameFrom putStr (\c -> if c == White then whitePlayer else blackPlayer) start

