module WormShow where

import	Graphics.UI.ObjectIO
import	WormState

--	The drawing constants.
wormBackGroundColour	= RGB {r=maxRGB,g=maxRGB,b=maxRGB*3 `div` 4}
wormFontSize		= 12 :: Int
pointsPos		= Point2{x=72, y=15}
lifesPos		= Point2{x=255,y= 5}
levelPos		= Point2{x=465,y=15}
cornerX			= 15 :: Int
cornerY			= 23 :: Int
segSize			= 4  :: Int
cellSize		= 10 :: Int

--	Draw the game.
drawGame :: Level -> Food -> Points -> Worm -> Lives -> Draw ()
drawGame (Level {level=level,obstacles=obstacles}) food points worm lives = do
	unfill	(Rectangle
			{ corner1=Point2{x=cornerX-8,y=0}
			, corner2=Point2{x=cornerX+sizeX*cellSize+16,y=cornerY+sizeY*cellSize+16}
			})
	drawBorders
	drawObstacles	obstacles
	drawPoints	points
	drawWorm	worm
	drawFood	food
	drawLevel	level
	drawLives	lives	
	where
		drawObstacles :: [Obstacle] -> Draw ()
		drawObstacles [] = return ()
		drawObstacles obstacles = do
			setPenColour (RGB {r=maxRGB `div` 2,g=maxRGB `div` 2,b=0})
			mapM_ drawObstacle obstacles
			setPenColour black			
			where
				drawObstacle :: Obstacle -> Draw ()
				drawObstacle (Rectangle (Point2 ltx lty) (Point2 rbx rby)) =
					fill (Rectangle (Point2 lx ty) (Point2 rx by))
					where
						lx = cornerX+cellSize*ltx-2
						ty = cornerY+cellSize*lty-2
						rx = cornerX+cellSize*rbx+2
						by = cornerY+cellSize*rby+2

		drawPoints :: Points -> Draw ()
		drawPoints points = do
			setPenColour magenta
			drawAt pointsPos{x=(x pointsPos) - 57} "Points: "
			setPenColour	black
			drawNewPoints	points

		drawWorm :: Worm -> Draw ()
		drawWorm [] = return ()
		drawWorm (head:rest) = do
			mapM_ (drawSegment red) rest
			drawSegment green head
			setPenColour black			

		drawLevel :: Int -> Draw ()
		drawLevel level = do
			setPenColour magenta
			drawAt levelPos{x=x-50} "Level: "
			setPenColour black
			unfill (Rectangle (Point2 (x-1) (y-12)) (Point2 (x+100) (y+4)))
			drawAt levelPos (show level)			
			where
				Point2{x=x,y=y}	= levelPos

		drawLives :: Lives -> Draw ()
		drawLives lives
			| lives /= 0 = drawLittleWorms lives
			| otherwise  = do
				setPenColour	magenta
				drawAt (Point2 {x=lx-63,y=ly+10}) "No more worms!"
				setPenColour	black
			where
				Point2{x=lx,y=ly} = lifesPos

				drawLittleWorms :: Lives -> Draw ()
				drawLittleWorms lives
					| lives>0 = do
						drawLittleWorm   lives
						drawLittleWorms (lives-1)
					| otherwise = do
						setPenColour	magenta
						drawAt (Point2 {x=lx-63,y=ly+10}) "Worms:"
						setPenColour	black						
					where
						Point2{x=lx,y=ly} = lifesPos

						drawLittleWorm :: Int -> Draw ()
						drawLittleWorm n = do
							setPenSize	5
							setPenColour	red
							setPenPos	(Point2{x=x,y=y})
							drawLineTo	(Point2{x=x+9,y=y})
							setPenColour	green
							drawLineTo	(Point2{x=x+10,y=y})
							setPenSize	1
							setPenColour	black
							where
								x	   	  = lx+20*((n-1) `div` 2)
								y		  = ly+ 7*((n-1) `mod` 2)
								Point2{x=lx,y=ly} = lifesPos

drawBorders :: Draw ()
drawBorders = do
	setPenColour	black
	setPenSize	3
	draw (Rectangle 
		{ corner1=Point2{x=cornerX-3,y=cornerY-3}
	  	, corner2=Point2{x=cornerX+sizeX*cellSize+11,y=cornerY+sizeY*cellSize+11}
		})
	setPenSize 1

drawSegment :: Colour -> Segment -> Draw ()
drawSegment color (Point2{x=x,y=y}) = do
	setPenColour color
	fillAt (Point2 {x=cornerX+cellSize*x,y=cornerY+cellSize*y}) (Oval {oval_rx=segSize,oval_ry=segSize})

eraseSegment :: Segment -> Draw ()
eraseSegment segment = drawSegment wormBackGroundColour segment

drawFood :: Food -> Draw ()
drawFood (Food{pos=pos}) = do
	setPenColour	magenta
	fillAt (Point2 {x=x1,y=y1}) (Box {box_w=6,box_h=6})
	setPenColour	black
	where
		x1	= cornerX+cellSize*(x pos)-3
		y1	= cornerY+cellSize*(y pos)-3

eraseFood :: Food -> Draw ()
eraseFood (Food{pos=pos}) =
	unfillAt (Point2 {x=x1,y=y1}) (Box {box_w=6,box_h=6})
	where
		x1	= cornerX+cellSize*(x pos)-3
		y1	= cornerY+cellSize*(y pos)-3

drawNewPoints :: Points -> Draw ()
drawNewPoints points = do
	unfill (Rectangle (Point2 (x-1) (y-12)) (Point2 (x+100) (y+4)))
	drawAt pointsPos (show points)
	where
		Point2{x=x,y=y} = pointsPos

--	Show a step of the worm.
drawStep :: Bool -> Food -> Food -> Points -> Segment -> Segment -> Segment -> Draw ()
drawStep scored oldfood newfood points oldh head tail
	| not scored = drawMove oldh head tail
	| otherwise  = do
		eraseFood	oldfood
		drawFood	newfood
		drawNewPoints	points
		drawMove	oldh head tail
	where
		drawMove :: Segment -> Segment -> Segment -> Draw ()
		drawMove oldh head (Point2 {x=0,y=0}) = do
			drawSegment  red oldh
			drawSegment  green head
			setPenColour black
			
		drawMove oldh head tail = do
			drawSegment	red oldh
			drawSegment	green head
			drawSegment	wormBackGroundColour tail
			setPenColour	black			


--	Close the Playfield between two levels.
drawAnimation :: Int -> Int -> Draw ()
drawAnimation 40 1 = do
	setPenColour	white
	drawBorders
	setPenColour	black
	
drawAnimation n step
	| step<0 = do
		setPenSize 3
		unfill	(Rectangle (Point2 l b) (Point2 x y))
		unfill	(Rectangle (Point2 r t) (Point2 x y))
		draw    (Rectangle (Point2 l t) (Point2 r b))

	| otherwise = do
		setPenSize 3
		unfill	(Rectangle (Point2 l b) (Point2 x     (y-3)))
		unfill	(Rectangle (Point2 r t) (Point2 (x-3)     y))
		draw	(Rectangle (Point2 l t) (Point2 r         b))
	where
		l	= cornerX-3
		t	= cornerY-3
		r	= l+w*n
		b	= t+h*n 
		x	= r-step*w
		y	= b-step*h 
		w	= (48+sizeX*cellSize) `div` 40
		h	= (48+sizeY*cellSize) `div` 40
