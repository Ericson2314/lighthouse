module Main where

--	**************************************************************************************************
--
--	This program shows the Towers of Hanoi algorithm graphically.
--
--	The program has been written in Haskell and uses the Port of Clean Object I/O library to Haskell
--	
--	**************************************************************************************************

import Graphics.UI.ObjectIO

type Tower = [Int]
type Moves = [Int]
data Towers
   = Towers
	{ moves	 :: !Moves
	, tower1 :: !Tower
	, tower2 :: !Tower
	, tower3 :: !Tower
	}
data TowerPos
   = TowerPos
	{ pos	:: Int
	, tower	:: Tower
	}

viewDomain = Rectangle{corner1=Point2{x=50,y=0},corner2=Point2{x=480,y=180}}
speed1 = ticksPerSecond `div` 2
speed2 = ticksPerSecond `div` 3
speed3 = ticksPerSecond `div` 6
speed4 = ticksPerSecond `div` 12
speed5 = 0

minDisks = 2
maxDisks = 10
xOffs	 = maxDisks * 10 + 1

--	Starting the program

main :: IO ()
main = openIds 5 >>= startHanoi

startHanoi :: [Id] -> IO ()
startHanoi [runID,haltID,contID,timerID,windowID] =
	startIO SDI
		(initTowers 0)				-- The initial local  process state
		initialise				-- The initialisation action
		[ProcessClose closeProcess]		-- Only default process attributes
	where
		initialise ps = do
			ps <- openWindow undefined window ps
			ps <- openMenu   undefined menu   ps
			ps <- openTimer  undefined timer  ps
			return ps

		menu	= Menu "&Hanoi"
				(   SubMenu "&Run (nr disks)"
					( RadioMenu
					    [("&"++show i,Nothing,Nothing,noLS (run i)) | i<-[minDisks..maxDisks]]
					    1	[]
					)
					[ MenuId runID
					]
				:+: MenuItem "Halt"
					[ MenuId haltID
					, MenuShortKey	  '.'
					, MenuSelectState Unable
					, MenuFunction	  (noLS halt)
					]
				:+: MenuItem "Continue"
					[ MenuId contID
					, MenuShortKey	','
					, MenuSelectState Unable
					, MenuFunction (noLS continue)
					]
				:+: SubMenu "&Speed" 
					( RadioMenu
						[ ("V&ery Slow", Nothing, Just 'A', noLS (setSpeed speed1))
						, ("&Slow"     , Nothing, Just 'S', noLS (setSpeed speed2))
						, ("&Normal"   , Nothing, Just 'D', noLS (setSpeed speed3))
						, ("&Fast"     , Nothing, Just 'F', noLS (setSpeed speed4))
						, ("&Very Fast", Nothing, Just 'G', noLS (setSpeed speed5))
						] 3 []
					) []
				:+: MenuSeparator []
				:+: MenuItem "&Quit"
					[ MenuShortKey 'q'
					, MenuFunction (noLS closeProcess)
					]
				) []

		window	= Window "Hanoi" NilLS
				[ WindowId		windowID
				, WindowViewDomain	viewDomain
				, WindowLook		True (look (initTowers 0))
				]

		timer	= Timer speed3 NilLS
				[ TimerId		timerID
				, TimerSelectState	Unable
				, TimerFunction		(noLS1 stepHanoi)
				]

		-- The function for the Run command.
		run :: Int -> Towers -> GUI Towers Towers
		run nr_disks _ = do
			disableMenuElements [runID,contID]
			enableMenuElements [haltID]
			enableTimer timerID
			let towers = initTowers nr_disks
			setWindowLook windowID True (True,look towers)
			return towers

		-- The function for the Halt command.
		halt :: Towers -> GUI Towers Towers
		halt towers = do
			enableMenuElements  [runID,contID]
			disableMenuElements [haltID]
			disableTimer timerID
			return towers

		-- The function for the Continue command.
		continue :: Towers -> GUI Towers Towers
		continue towers = do
			disableMenuElements [runID,contID]
			enableMenuElements [haltID]
			enableTimer timerID
			return towers

		-- Set the speed of a (possibly running) Hanoi simulation.
		setSpeed :: Int -> Towers -> GUI Towers Towers
		setSpeed speed towers = do
			setTimerInterval timerID speed
			return towers

		-- The timer function: take a move from the list of all moves and show it in the window.
		stepHanoi :: NrOfIntervals -> Towers -> GUI Towers Towers
		stepHanoi _ towers@(Towers {moves=[]}) = do
			enableMenuElements  [runID]
			disableMenuElements [haltID]
			disableTimer 	    timerID
			return towers
		stepHanoi _ towers = do
			drawInWindow windowID drawf
			setWindowLook windowID False (True,look towers1)
			return towers1
			where
				(drawf,towers1)	= changeTowers towers

				changeTowers :: Towers -> (Draw (),Towers)
				changeTowers towers@(Towers {moves=(1:2:moves),tower1=(f1:r1),tower2=t2}) =
					(drawMove 1 2 f1 (length r1) (length t2),towers{moves=moves,tower1=r1,tower2=(f1:t2)})
				changeTowers towers@(Towers {moves=(1:3:moves),tower1=(f1:r1),tower3=t3}) =
					(drawMove 1 3 f1 (length r1) (length t3),towers{moves=moves,tower1=r1,tower3=(f1:t3)})
				changeTowers towers@(Towers {moves=(2:1:moves),tower2=(f2:r2),tower1=t1}) =
					(drawMove 2 1 f2 (length r2) (length t1),towers{moves=moves,tower2=r2,tower1=(f2:t1)})
				changeTowers towers@(Towers {moves=(2:3:moves),tower2=(f2:r2),tower3=t3}) =
					(drawMove 2 3 f2 (length r2) (length t3),towers{moves=moves,tower2=r2,tower3=(f2:t3)})
				changeTowers towers@(Towers {moves=(3:1:moves),tower3=(f3:r3),tower1=t1}) =
					(drawMove 3 1 f3 (length r3) (length t1),towers{moves=moves,tower3=r3,tower1=(f3:t1)})
				changeTowers towers@(Towers {moves=(3:2:moves),tower3=(f3:r3),tower2=t2}) =
					(drawMove 3 2 f3 (length r3) (length t2),towers{moves=moves,tower3=r3,tower2=(f3:t2)})

				drawMove :: Int -> Int -> Int -> Int -> Int -> Draw ()
				drawMove start end disk lenfr lento = do
					eraseDisk (Rectangle{corner1=(Point2{x=fx-w,y=fy}),corner2=(Point2{x=fx+w,y=fy+10})})
					drawDisk  (Rectangle{corner1=(Point2{x=tx-w,y=ty}),corner2=(Point2{x=tx+w,y=ty+10})})
					where
						tx	= end  *xOffs
						ty	= 10+10*(maxDisks-lento) 
						fx	= start*xOffs
						fy	= 10+10*(maxDisks-lenfr) 
						w	= disk *5


-- The initial Towers value, given the number of disks

initTowers :: Int -> Towers
initTowers nr_disks =
	Towers
	  { moves	= hanoi nr_disks 1 2 3
	  , tower1	= [1..nr_disks]
	  , tower2	= []
	  , tower3	= []
	  }
	where
		hanoi :: Int -> Int -> Int -> Int -> Moves	-- The function that calculates the list of disk moves
		hanoi n start end via
			| n==0		= []
			| otherwise	= hanoi m start via end ++ (start:end:hanoi m via end start)
			where
				m			= n-1

--	The update function: erase the window and redraw the towers

look :: Towers -> SelectState -> UpdateState -> Draw ()
look (Towers {tower1=tower1,tower2=tower2,tower3=tower3}) _ (UpdateState {newFrame=newFrame}) = do
	unfill newFrame
	draw (TowerPos{pos=1,tower=tower1})
	draw (TowerPos{pos=2,tower=tower2})
	draw (TowerPos{pos=3,tower=tower3})

instance Drawables TowerPos where
	draw (TowerPos{pos=pos,tower=tower}) = do
		drawTower pos (maxDisks-length tower) tower
		drawName  pos
		where
			drawTower :: Int -> Int -> Tower -> Draw ()
			drawTower nr i (f:r) = do
				drawDisk (Rectangle {corner1=Point2{x=x-w,y=y},corner2=Point2{x=x+w,y=y+10}})
				drawTower nr (i+1) r
				where
					x	= nr*xOffs
					w	= f *5
					y	= 20+10*i
			drawTower _ _ [] = return ()

			drawName :: Int -> Draw ()
			drawName nr
				| nr==1	= setPenPos (Point2{x=  xOffs-14,y=y}) >> draw "from"
				| nr==2	= setPenPos (Point2{x=2*xOffs-6, y=y}) >> draw "to"
				| nr==3	= setPenPos (Point2{x=3*xOffs-9, y=y}) >> draw "via"
				where
					y		= 35+10*maxDisks
	
	drawAt _ tower = draw tower
	
	undraw (TowerPos{pos=pos,tower=tower}) =
		unfill (Rectangle {corner1=Point2{x=x-w `div` 2,y=0},corner2=Point2{x=x+w `div` 2,y=35+10*maxDisks}})
		where
			x	= pos*xOffs
			w	= 10*maxDisks	
	
	undrawAt _ tower = undraw tower

eraseDisk :: Rectangle -> Draw ()
eraseDisk rectangle = unfill rectangle

drawDisk :: Rectangle -> Draw ()
drawDisk rectangle = do
	setPenColour	(RGB {r=maxRGB `div` 2,g=maxRGB*3 `div` 5,b=maxRGB*7 `div` 10})
	fill rectangle
	setPenColour black
	draw rectangle
