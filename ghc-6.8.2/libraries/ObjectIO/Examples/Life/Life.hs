module Life where

import Graphics.UI.ObjectIO
import Control.Monad(when)

type Generation	= [[LifeCell]]
type CellSize	= Int
type ClickPoint	= Point2

data LifeCell
   = LifeCell
	{ cx	:: !Int
	, cy	:: !Int
	, age	:: !Int
	}

colours :: [Colour]
colours = [red,magenta,green,yellow,cyan,blue]

ageToColour :: Int -> Colour
ageToColour age
	| age<=0	= colours !! 0
	| age>=5	= colours !! 5
	| otherwise	= colours !! age

makeGeneration :: Generation
makeGeneration = []

makeLifeCell :: ClickPoint -> CellSize -> LifeCell
makeLifeCell p size = LifeCell{cx=clickPointToCell (x p) size,cy=clickPointToCell (y p) size,age=0}
	where
		clickPointToCell :: Int -> Int -> Int
		clickPointToCell x size
			| x<0		= x `div` size
			| otherwise	= x `div` size+1

newLifeCell :: Int -> Int -> LifeCell
newLifeCell x y = LifeCell{cx=x,cy=y,age=0}


--	Rendering of LifeCells.

drawCells :: (LifeCell -> Draw ()) -> Generation -> Draw ()
drawCells f gen = mapM_ f (concat gen)

drawCell :: CellSize -> LifeCell -> Draw ()
drawCell size (LifeCell {cx=cx,cy=cy,age=age}) = do
	setPenColour (ageToColour age)
	fill (Rectangle {corner1=Point2 px py,corner2=Point2 (px+size) (py+size)})
	when (size > 2) (undraw (Rectangle {corner1=Point2 (px-1) (py-1),corner2=Point2 (px+size) (py+size)}))
	where
		px	= cx*size
		py	= cy*size

eraseCell :: CellSize -> LifeCell -> Draw ()
eraseCell size (LifeCell {cx=cx,cy=cy}) =
	unfill (Rectangle {corner1=Point2 px py,corner2=Point2 (px+size) (py+size)})
	where
		px	= cx*size
		py	= cy*size


{-	Insert a LifeCell to a Generation. 
	In a Generation LifeCells are ordered by increasing x-coordinate first, and by increasing y-coordinate second.
-}
insertCell :: LifeCell -> Generation -> Generation
insertCell c1@(LifeCell{cx=x1}) gen@(cs@(LifeCell{cx=x2,cy=y2}:x2ys) : cs_xs)
	| x2<x1			= cs : insertCell c1 cs_xs
	| x2==x1		= insertCelly c1 cs : cs_xs
	| otherwise		= [c1]:gen
	where
		insertCelly :: LifeCell -> [LifeCell] -> [LifeCell]
		insertCelly c1@(LifeCell {cy=y1}) g@(c2@(LifeCell {cx=x2,cy=y2}):x2ys)
			| y2<y1		= c2	: insertCelly c1 x2ys
			| y2==y1	= c1	: x2ys
			| otherwise	= c1	: g
		insertCelly c1 _ = [c1]
insertCell c1 [] = [[c1]]

{-	Remove a LifeCell from a Generation.
-}
removeCell :: LifeCell -> Generation -> Generation
removeCell c1@(LifeCell {cx=x1,cy=y1}) gen@(cs@((LifeCell {cx=x2,cy=y2}):x2ys):cs_xs)
	| x2<x1		= cs:removeCell c1 cs_xs
	| x2>x1		= gen
	| otherwise	=
		let cs1 = removeCelly c1 cs
		in if null cs1 then cs_xs
		   else cs1 : cs_xs
	where
		removeCelly :: LifeCell -> [LifeCell] -> [LifeCell]
		removeCelly c1@(LifeCell {cy=y1}) cs@(c2@(LifeCell {cx=x2,cy=y2}):x2ys)
			| y2<y1		= c2 : removeCelly c1 x2ys
			| y2==y1	= x2ys
			| otherwise	= cs
		removeCelly _ _ = []
removeCell c ([]:cs_xs) = removeCell c cs_xs
removeCell c _ = []

{-	Calculate the new Generation (first tuple result) and the Generation of LifeCells that die (second tuple result).
-}
lifeGame :: Generation -> (Generation,Generation)
lifeGame gen =
	let 
		(next,_,die) = nextGen gen gen
		next1 = celebrateSurvival next gen
	in 
		(next1,die)
	where
		nextGen :: Generation -> Generation -> (Generation,Generation,Generation)
		nextGen ((c@(LifeCell{cx=cx,cy=cy}):cs_x):cs_xs) gen
			| neighbours34 (neighbours c gen) = (insertCell c gennext1,new,diednext)
			| otherwise			  = (gennext1,new,insertCell c diednext)
			where
				(gennext,newbornsnext,diednext)	= nextGen (cs_x:cs_xs) gen1
				(gennext1,new)			= newBorns c newbornsnext gennext gen
				gen1				= shiftGeneration (cs_x:cs_xs) gen

				neighbours34 [_,_,_]	=  True
				neighbours34 [_,_,_,_] 	=  True
				neighbours34 _		=  False

				newBorns :: LifeCell -> Generation -> Generation -> Generation -> (Generation,Generation)
				newBorns c newbornsnext gennext gen =
					newBorns1 (newBornNeighbours c gen) newbornsnext gennext gen
					where
						newBorns1 (c@(LifeCell {cx=x1,cy=y1}):cs) newbornsnext gennext gen
							| neighbours3 (neighbours c gen) = (insertCell c gennext1,insertCell c newbornsnext1)
							| otherwise			 = next_genANDnewborns
							where
								(gennext1,newbornsnext1)	= next_genANDnewborns
								next_genANDnewborns		= newBorns1 cs newbornsnext gennext gen

								neighbours3 :: [LifeCell] -> Bool
								neighbours3 [_,_,_]	= True
								neighbours3 _ 		= False
						newBorns1 [] newbornsnext gennext _ = (gennext,newbornsnext)

						--	newBornNeighbours c gen -> dead neighbours of c in gen in decreasing order.

						newBornNeighbours :: LifeCell -> Generation -> [LifeCell]
						newBornNeighbours (LifeCell {cx=cx,cy=cy}) gen =
							newBornNeighbours1 (cx-1) (cx+1) (cy-1) gen []
							where
								newBornNeighbours1:: Int -> Int -> Int -> Generation -> [LifeCell] -> [LifeCell]
								newBornNeighbours1 x xn y (cs@((LifeCell {cx=x2}):_):cs_xs) newborns
									| x>xn		= newborns
									| x2<x		= newBornNeighbours1 x xn y cs_xs newborns
									| x2==x		= newBornNeighbours2 x y (y+2) cs (newBornNeighbours1 (x+1) xn y cs_xs newborns)
									| otherwise	= (newLifeCell x y:newLifeCell x (y+1):newLifeCell x (y+2):newBornNeighbours1 (x+1) xn y cs_xs newborns)
								newBornNeighbours1 x xn y [] newborns
									| x>xn		= newborns
									| otherwise	= (newLifeCell x y:newLifeCell x (y+1):newLifeCell x (y+2):newBornNeighbours1 (x+1) xn y [] newborns)

								newBornNeighbours2:: Int -> Int -> Int -> [LifeCell] -> [LifeCell] -> [LifeCell]
								newBornNeighbours2 x y yn (c@(LifeCell {cx=x2,cy=y2}):cs) cs_xs
									| y>yn		= cs_xs
									| y2<y		= newBornNeighbours2 x y yn cs cs_xs
									| y2==y		= newBornNeighbours2 x (y+1) yn cs cs_xs
									| otherwise	= (newLifeCell x y:newBornNeighbours2 x (y+1) yn cs cs_xs)
								newBornNeighbours2 x y yn [] cs_xs
									| y>yn		= cs_xs
									| otherwise	= newLifeCell x y:newBornNeighbours2 x (y+1) yn [] cs_xs

				shiftGeneration :: Generation -> Generation -> Generation
				shiftGeneration (   (c@(LifeCell {cx=cx,cy=cy}):_):_) gen = shiftGeneration1 c{cx=cx-2,cy=cy-2} gen
				shiftGeneration ([]:(c@(LifeCell {cx=cx,cy=cy}):_):_) gen = shiftGeneration1 c{cx=cx-2,cy=cy-2} gen
				shiftGeneration partial_gen           gen = gen

				shiftGeneration1:: LifeCell -> Generation -> Generation
				shiftGeneration1 c@(LifeCell {cx=x1,cy=y1}) gen@((c2@(LifeCell {cx=x2,cy=y2}):cs_x):cs_xs)
					| x2<x1			= shiftGeneration1 c cs_xs
					| x2==x1 && y2<y1	= shiftGeneration1 c (cs_x:cs_xs)
					| otherwise		= gen
				shiftGeneration1 c ([]:cs_xs) = shiftGeneration1 c cs_xs
				shiftGeneration1 c _ = []

				--	neighbours c gen -> neighbours of c in gen in decreasing order.

				neighbours :: LifeCell -> Generation -> [LifeCell]
				neighbours (LifeCell {cx=x,cy=y}) gen =
					neighbours1 (x-1) (x+1) (y-1) gen []
					where
						neighbours1:: Int -> Int -> Int -> Generation -> [LifeCell] -> [LifeCell]
						neighbours1 x xn y (cs@((LifeCell {cx=x2,cy=y2}):_):cs_xs) neighbours
							| x2<x		= neighbours1 x xn y cs_xs neighbours
							| x2<=xn	= neighbours2 y (y+2) cs (neighbours1 (x+1) xn y cs_xs neighbours)
							| otherwise	= neighbours
						neighbours1 _ _ _ [] neighbours = neighbours

						neighbours2:: Int -> Int -> [LifeCell] -> [LifeCell] -> [LifeCell]
						neighbours2 y yn (c@(LifeCell {cx=x2,cy=y2}):cs) cs_xs
							| y2<y		= neighbours2 y yn cs cs_xs
							| y2<=yn	= (c:neighbours2 (y+1) yn cs cs_xs)
							| otherwise	= cs_xs
						neighbours2 _ _ [] cs_xs = cs_xs

		nextGen ([]:cs_xs) gen = nextGen cs_xs gen
		nextGen _ _ = ([],[],[])
	
		celebrateSurvival :: Generation -> Generation -> Generation
		celebrateSurvival new old = map (map (celebrate old)) new
			where
				celebrate :: Generation -> LifeCell -> LifeCell
				celebrate old newcell
					| found		= newcell{age=age+1}
					| otherwise	= newcell{age=age}
					where
						(found,age)	= getCellAge newcell old

				getCellAge :: LifeCell -> Generation -> (Bool,Int)
				getCellAge c1@(LifeCell {cx=x1}) (xs@((LifeCell {cx=x2}):_):xss)
					| x1<x2		= (False,0)
					| x1>x2		= getCellAge  c1 xss
					| otherwise	= getCellAge' c1 xs
				getCellAge _ _ = (False,0)

				getCellAge' :: LifeCell -> [LifeCell] -> (Bool,Int)
				getCellAge' c1@(LifeCell {cy=y1}) (LifeCell{cy=y2,age=age}:xs)
					| y1<y2		= (False,0)
					| y1>y2		= getCellAge' c1 xs
					| otherwise	= (True,age)
				getCellAge' _ _ = (False,0)
