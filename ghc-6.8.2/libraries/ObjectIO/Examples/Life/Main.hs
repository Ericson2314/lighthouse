module Main where

--	**************************************************************************************************
--
--	This is the LifeGame program.
--
--	The program has been written in Clean 1.3.2 and uses the Clean Standard Object I/O library 1.2
--	
--	**************************************************************************************************

import Graphics.UI.ObjectIO
import Life
import Help
import Data.Char(ord,chr)

data Life = Life
   	{ gen  :: Generation
	, size :: CellSize
	}
	
initialLife = Life
	{ gen  = makeGeneration
	, size = startCellSize
	}

main :: IO ()
main = openIds 6 >>= startLife

startLife :: [Id] -> IO ()
startLife [eraseID,playID,haltID,stepID,windowID,timerID] =
	startIO SDI initialLife initialise [ProcessClose closeProcess]
	where
	-- initialise creates the gui of the life.
	initialise ps = do		
		ps <- openTimer undefined timer    ps
		ps <- openMenu  undefined file     ps
		ps <- openMenu  undefined options  ps
		ps <- openMenu  undefined commands ps
		size <- getProcessWindowSize
		ps <- openWindow undefined (window size) ps
		return ps
	
	-- window defines the window that displays the universe and its inhabitants.
	window size =
		Window "Life" NilLS
			[ WindowId		windowID
			, WindowClose		(noLS closeProcess)
			, WindowMouse		onlyMouseDown Able (noLS1 track)
			, WindowViewDomain	(getViewDomain startCellSize)
			, WindowViewSize	size
			, WindowOrigin		zero
			, WindowHScroll 	(hscroll startCellSize)
			, WindowVScroll		(vscroll startCellSize)
			, WindowLook		True (look initialLife)
			, WindowPen		[PenBack black]
			]
		where
			hscroll dh viewframe (SliderState{sliderThumb=sliderThumb}) move
				= case move of
					SliderIncSmall -> sliderThumb+dh
					SliderDecSmall -> sliderThumb-dh
					SliderIncLarge -> sliderThumb+(w (rectangleSize viewframe))*9 `div` 10
					SliderDecLarge -> sliderThumb-(w (rectangleSize viewframe))*9 `div` 10
					SliderThumb x  -> x
			vscroll dv viewframe (SliderState{sliderThumb=sliderThumb}) move
				= case move of
					SliderIncSmall -> sliderThumb+dv
					SliderDecSmall -> sliderThumb-dv
					SliderIncLarge -> sliderThumb+(h (rectangleSize viewframe))*9 `div` 10
					SliderDecLarge -> sliderThumb-(h (rectangleSize viewframe))*9 `div` 10
					SliderThumb y  -> y
	
	-- timer defines the timer that calculates subsequent life generations.
	timer	= Timer 0 NilLS
		[ TimerId		timerID
		, TimerSelectState	Unable
		, TimerFunction		(noLS1 (\_->step))
		]

	-- file defines the "File" menu, containing only the quit command to terminate the program.
	file	= Menu "&File"
			(	MenuItem "&About LifeGameExample..."
					[ MenuFunction (noLS (showAbout "Life" "LifeHelp")) ]
			:+:	MenuSeparator		[]
			:+:	MenuItem "&Quit"	[MenuShortKey 'q',MenuFunction (noLS closeProcess)]
			)	[]

	-- options defines the "Options" menu to set the size of the displayed cells.
	options	= Menu "&Options"
			( SubMenu "Cell Size"				
		  		( RadioMenu
		  			[(title (2^i),Nothing,Just (char i),noLS (newsize (2^i))) | i<-[0..4]] 4 []
		  		) []
			) []
		where
			title size	= show size ++ " * " ++ show size
			char  i		= chr (ord '1'+i)
	
	-- commands defines the "Commands" menu to run and halt the computations of life generations.
	commands = Menu "&Commands"
			(	MenuItem "&Erase Cells"	[MenuId eraseID,MenuShortKey 'e',MenuFunction (noLS erase)]
		  	:+:	MenuItem "&Play"	[MenuId playID, MenuShortKey 'p',MenuFunction (noLS play)]
			:+:	MenuItem "&Halt"	[MenuId haltID, MenuShortKey 'h',MenuFunction (noLS halt), MenuSelectState Unable]
			:+:	MenuItem "&Step"	[MenuId stepID, MenuShortKey 's',MenuFunction (noLS step)]
			)	[]
	
	-- play starts the computation of successive generations given the current set of life cells.
	play :: Life -> GUI Life Life
	play life = do
		disableWindowMouse  windowID
		disableMenuElements [eraseID,playID,stepID]
		enableMenuElements  [haltID]
		enableTimer timerID
		return life
	
	-- halt stops the computation of successive generations, but does not change the current generation. 
	halt :: Life -> GUI Life Life
	halt life = do
		enableWindowMouse	windowID
		disableMenuElements	[haltID]
		enableMenuElements	[eraseID,playID,stepID]
		disableTimer		timerID
		return life
	
	-- step calculates the next generation and displays it.
	step :: Life -> GUI Life Life
	step life@(Life {gen=gen,size=size}) = do		
		drawInWindow windowID render
		setWindowLook windowID False (True,look life)
		return life{gen=next}
		where
			(next,died)	= lifeGame gen
			render		= drawCells (drawCell size) next >> (drawCells (eraseCell size) died)
	
	-- erase sets the current generation to empty and clears the window.
	erase :: Life -> GUI Life Life
	erase life = do
		let life1 = life{gen=makeGeneration}
		setWindowLook windowID True (True,look life1)
		return life1
	
	-- newsize changes the size in which life cells are rendered and redraws the window.
	newsize :: Int -> Life -> GUI Life Life
	newsize newSize life@(Life {size=oldSize}) = do
		let life1 = life{size=newSize}		
		viewframe <- getWindowViewFrame windowID		
  		let oldOrigin = corner1 viewframe
		let newOrigin = Point2 {x=(x oldOrigin) `div` oldSize*newSize,y=(y oldOrigin) `div` oldSize*newSize}		
		setWindowLook windowID False (True,look life1{gen=makeGeneration})		
		setWindowViewDomain windowID (getViewDomain newSize)		
		moveWindowViewFrame windowID (toVector newOrigin-toVector oldOrigin)		
		setWindowLook windowID True (True,look life1)		
		return life1
	
	-- The window look:
	look :: Life -> SelectState -> UpdateState -> Draw ()
	look (Life{gen=gen,size=size}) _ (UpdateState {newFrame=newFrame}) = do
		unfill newFrame
		drawCells (drawCell size) gen		
	
	-- The window mouse accepts only MouseDown user actions:
	onlyMouseDown :: MouseState -> Bool
	onlyMouseDown (MouseDown _ _ _) = True
	onlyMouseDown (MouseDrag _ _)	= True
	onlyMouseDown _			= False
	
	-- The window mouse action places and removes alive cells:
	track :: MouseState -> Life -> GUI Life Life
	track mouse life@(Life {gen=gen,size=size})
		| commandDown modifiers = do
			drawInWindow windowID (eraseCell size cell)
			setWindowLook windowID False (True,look life)
			return life{gen=removeCell cell gen}
		| otherwise = do
			drawInWindow windowID (drawCell size cell)
			setWindowLook windowID False (True,look life)
			return life{gen=insertCell cell gen}
		where
			(pos,modifiers)	= case mouse of
				(MouseDown pos mods _) -> (pos,mods)
				(MouseDrag pos mods)   -> (pos,mods)
			cell = makeLifeCell pos size
	
	-- Given the size in which to render life cells, getViewDomain calculates the corresponding ViewDomain:
	getViewDomain :: CellSize -> ViewDomain
	getViewDomain size =		
		Rectangle (Point2 (size*left) (size*top)) (Point2 (size*right) (size*bottom))
		where
			Rectangle (Point2 left top) (Point2 right bottom) = universe


--	Program constants.

universe	= Rectangle (Point2 (-1000) (-1000)) (Point2 1000 1000)
startCellSize	= 8
