module Main where

--	**************************************************************************************************
--
--	The famous Unix game 'worm' (or 'snake').
--
--	The program has been written in Clean 1.3.2 and uses the Clean Standard Object I/O library 1.2
--	
--	**************************************************************************************************

import Prelude hiding (Either(..))
import Graphics.UI.ObjectIO
import WormShow
import WormState
import Help
import HighScore
import Notice
import System.Random


--	GUI constants.
helpFile	= "WormHelp"
hiScoresFile	= "wormhi"
nrOfHiScores	= 8


--	Start of the program.
main :: IO ()
main = do
	hiscores <- readHiScores hiScoresFile
	ids <- openIds 8
	startWorm hiscores ids

startWorm :: HiScores -> [Id] -> IO ()
startWorm best ids = startIO SDI (initState best) (initialise ids) []

initialise :: [Id] -> State -> GUI State State
initialise ids state@(State {best=best}) = do
	state <- openWindow undefined window      state
	state <- openMenu   undefined filemenu    state
	state <- openMenu   undefined optionsmenu state
	state <- openTimer  undefined timer       state
	state <- initFoodSupply (mkStdGen 0)      state
	state <- initWindowPicture 		  state
	return state
	where
		[fileID,playID,haltID,quitID,levelID,contID,windowID,timerID] = ids

		initFoodSupply :: StdGen ->  State -> GUI State State
		initFoodSupply seed state@(State {worm=worm,gamelevel=gamelevel}) = do
			let (food,foods) = newFood worm gamelevel (randoms seed)
			return state{food=food,foodsupply=foods}

		initWindowPicture :: State -> GUI State State
		initWindowPicture state = do
			drawInWindow windowID setPenFontSize
			return state
			where
				setPenFontSize :: Draw ()
				setPenFontSize = do
					font <- getPenFont					
					setPenFont font{fontSize=wormFontSize}

		filemenu = Menu "File" 
			(	MenuItem "Play"			[MenuId playID,MenuShortKey 'r',MenuFunction (noLS play)]
			:+:	MenuItem "Halt"			[MenuId haltID,MenuShortKey '.',MenuFunction (noLS halt),MenuSelectState Unable]
			:+:	MenuSeparator			[]
			:+: 	MenuItem "About Worm..."	[MenuFunction (noLS (showAbout "Worm" helpFile))]
			:+:	MenuItem "Help"			[MenuFunction (noLS (showHelp helpFile))]
			:+:	MenuSeparator			[]
			:+:	MenuItem "Quit"			[MenuId quitID,MenuShortKey 'q',MenuFunction (noLS quit)]
			)	[MenuId fileID]
		optionsmenu = Menu "Options"
			( 	RadioMenu
					[ ("Slow"  ,Nothing,Just '1',noLS (setSpeed easySpeed)  )
					, ("Medium",Nothing,Just '2',noLS (setSpeed mediumSpeed))
					, ("Fast"  ,Nothing,Just '3',noLS (setSpeed hardSpeed)  )
					] 1 []
			:+: 	MenuSeparator []
			:+:	MenuItem "High Scores" [MenuShortKey 'h',MenuFunction (noLS showBest)]
			)
			[	MenuId	levelID
			]
		window	= Window "Worm" NilLS
			[ WindowId			windowID
			, WindowClose			(noLS quit)
			, WindowKeyboard		keyFilter Unable (noLS1 makeTurn)
			, WindowPen			[PenBack wormBackGroundColour]
			, WindowViewDomain		zero{corner2=Point2{x=488,y=303}}
			, WindowLook			True (updateWindow (initState best))
			]
		timer	= Timer easySpeed NilLS [TimerId timerID, TimerSelectState Unable, TimerFunction (noLS1 oneStep)]

		--	The update function for the playfield window.
		updateWindow :: State -> SelectState -> UpdateState -> Draw ()
		updateWindow (State {gamelevel=gamelevel,food=food,points=points,worm=worm,lives=lives}) _ (UpdateState {updArea=updArea}) = do
			mapM_ unfill updArea
			drawGame gamelevel food points worm lives

		--	The function for the Play command.
		play :: State -> GUI State State
		play state = do
			disableMenus		[levelID]
			disableMenuElements	[playID,quitID]
			enableMenuElements	[haltID]
			setTimerInterval	timerID (speed (gamelevel state))
			enableWindowKeyboard	windowID
			enableTimer		timerID
			drawInWindow		windowID (drawGame initlevel newfood initpoints initworm initlives)
			setWindowCursor		windowID HiddenCursor
			return initstate
			where
				initlevel		= initLevel (fix (gamelevel state))
				initworm		= newWorm initlevel
				(newfood,foods1)	= newFood initworm initlevel (foodsupply state)
				initpoints		= 0
				initlives		= nrOfWorms
				initstate		= state
						{ gamelevel	= initlevel
						, food		= newfood
						, foodsupply	= foods1
						, grow		= 0
						, points	= initpoints
						, dir		= rightKey
						, worm		= initworm
						, lives		= initlives
						}

		--	The functions for the Halt/Continue command(s).
		halt :: State -> GUI State State
		halt state = do
			setWindowCursor		windowID StandardCursor
			disableWindowKeyboard	windowID
			disableTimer		timerID
			enableMenuElements	[quitID]
			closeMenuElements	fileID [haltID]
			openMenuElements fileID 1 undefined continue
			return state
			where
				continue = MenuItem "Continue" [MenuId contID, MenuShortKey '.', MenuFunction (noLS contf)]

				contf :: State -> GUI State State
				contf state = do
					enableWindowKeyboard	windowID
					enableTimer		timerID
					setWindowCursor		windowID HiddenCursor
					disableMenuElements	[quitID]
					closeMenuElements	fileID [contID]
					openMenuElements	fileID 1 undefined hlt
					return	state
					where
						hlt = MenuItem "Halt" [MenuId haltID, MenuShortKey '.', MenuFunction (noLS halt)]

		--	The function for the Quit command: stop the program and write the high scores to file.
		quit :: State -> GUI State State
		quit state@(State {best=best}) = do
			state <- closeProcess state
			liftIO (writeHiScores hiScoresFile best)
			return state

		--	Set a new speed (called when one of the Options commands is chosen).
		setSpeed :: Int -> State -> GUI State State
		setSpeed fix state =
			return state{gamelevel=(gamelevel state){fix=fix,speed=fix}}

		--	Show the high scores.
		showBest :: State -> GUI State State
		showBest state@(State {best=best}) = showHiScores "Worm High Scores:" best state

		--	The MakeTurn function is called when an arrow key is pressed.
		keyFilter :: KeyboardState -> Bool
		keyFilter (SpecialKey key (KeyDown _) _) = key `elem` [downKey,leftKey,rightKey,upKey]
		keyFilter _ = False

		makeTurn :: KeyboardState -> State -> GUI State State
		makeTurn (SpecialKey key _ _) state@(State {dir=dir})
			| (dir==upKey   || dir==downKey)  && (key==leftKey || key==rightKey)	= oneStep 1 state{dir=key}
			| (dir==leftKey || dir==rightKey) && (key==upKey   || key==downKey )	= oneStep 1 state{dir=key}
			| otherwise = return state

		--	The function for the Timer device: do one step of the worm game.
		oneStep :: NrOfIntervals -> State -> GUI State State
		oneStep _ state@(State {gamelevel=gamelevel,food=food,foodsupply=foodsupply,grow=grow,points=points,dir=dir,worm=worm,best=best,lives=lives})
			| newlevel/=curlevel	= switchLevel gamelevel foodsupply points2 points best lives state
			| otherwise		=
				let
					state1 = state{food=food1,foodsupply=foods1,grow=grow1,points=points2,worm=worm1}
				in
					if collide
					then nextLife state1
					else do
						drawInWindow windowID (drawStep scored food food1 points2 (head worm) hd tl)
						return state1
			where
				(hd,tl,worm1)		= stepWorm dir grow worm
				scored			= hd==pos food
				collide			= collision gamelevel worm hd
				(food1,foods1)		= if scored then (newFood worm1 gamelevel foodsupply) else (food,foodsupply)
				grow1			= if scored then (grow+((value food)*3)`div`2) else (max 0 (grow-1))
				points1			= if scored then points+(value food)*(length worm1) `div` 2 else points
				points2			= if collide then max 0 (points1-100) else points1
				curlevel		= points  `div` pointsPerLevel
				newlevel		= points2 `div` pointsPerLevel

				collision :: Level -> Worm -> Segment -> Bool
				collision level worm head
					=  (not (inRectangle head (Rectangle {corner1=Point2{x=1,y=1},corner2=Point2{x=sizeX,y=sizeY}})))
					|| (any (inRectangle head) (obstacles level))
					|| (head `elem` worm)
					where
						inRectangle :: Point2 -> Obstacle -> Bool
						inRectangle (Point2 x y) (Rectangle (Point2 lx ty) (Point2 rx by)) =
							x>=lx && x<=rx && y>=ty && y<=by

				stepWorm :: SpecialKey -> Grow -> Worm -> (Segment,Segment,Worm)
				stepWorm dir 0 worm = (hd,tl,(hd:worm1))
					where
						(tl,worm1) = getAndRemoveLast worm
						hd	   = newHead dir (head worm)

						getAndRemoveLast :: [x] -> (x,[x])
						getAndRemoveLast [x]    = (x,[])
						getAndRemoveLast (x:xs) = (x1,x:xs1)
							where 
								(x1,xs1) = getAndRemoveLast xs
				stepWorm dir _ worm = (hd,zero,hd:worm)
					where
						hd = newHead dir (head worm)

				newHead :: SpecialKey -> Segment -> Segment
				newHead key segment@(Point2 x y)
					| key==upKey	= segment{y=y-1}
					| key==downKey	= segment{y=y+1}
					| key==leftKey	= segment{x=x-1}
					| key==rightKey	= segment{x=x+1}
					| otherwise	= error ("newHead applied to unknown SpecialKey: "++show key)

				switchLevel :: Level -> [Food] -> Points -> Points -> HiScores -> Lives -> State -> GUI State State
				switchLevel curlevel foods newPoints oldPoints high lives state = do
					id <- openId
					nextLevelAnimation id newstate
					where	
						newlevel 		= (if newPoints>oldPoints then increaseLevel else decreaseLevel) curlevel
						initworm 		= newWorm newlevel
						(newfood,foods1)	= newFood initworm newlevel foods
						newstate		= State
							{ gamelevel	= newlevel
							, food		= newfood
							, foodsupply	= foods1
							, grow		= 0
							, points	= newPoints
							, dir		= rightKey
							, worm		= initworm
							, best		= high
							, lives		= if newPoints>oldPoints then lives+1 else lives-1
							}

						nextLevelAnimation :: Id -> State -> GUI State State
						nextLevelAnimation id state = do
							disableWindowKeyboard windowID
							disableTimer	      timerID					
							state <- openTimer (nrAnimationSteps,-1) (Timer (ticksPerSecond `div` 30) NilLS
													[ TimerId	id
													, TimerFunction	betweenLevels
													]) state
							return state
							where
								nrAnimationSteps= 40

								betweenLevels :: NrOfIntervals -> GUIFun (Int,Int) State
								betweenLevels _ ((animationStep,step), state@(State{gamelevel=gamelevel,food=food,points=points,worm=worm,lives=lives}))
									| animationStep<=1 = return ((2,1),state)
									| animationStep<=nrAnimationSteps = do
										drawInWindow windowID (drawAnimation animationStep step)
										return ((animationStep+step,step),state)
									| otherwise = do
										drawInWindow windowID (drawGame gamelevel food points worm lives)
										enableTimer  timerID
										closeTimer   id
										enableWindowKeyboard	windowID
										return ((animationStep,step),state)

				nextLife :: State -> GUI State State
				nextLife state@(State {gamelevel=gamelevel,foodsupply=foodsupply,points=points,best=best,worm=worm,lives=lives})
					| lives>0 =						
						let
							(newfood,foods1)= newFood newworm gamelevel foodsupply
							newworm			= newWorm gamelevel

							deadWormAlert :: Id -> Worm -> State -> GUI State State
							deadWormAlert id worm state = do
								disableTimer		timerID
								disableWindowKeyboard	windowID
								state <- openTimer worm (Timer (ticksPerSecond `div` 30) NilLS [TimerId id,TimerFunction deadWorm]) state
								return state
								where
									deadWorm :: NrOfIntervals -> GUIFun Worm State
									deadWorm _ (segment:rest,state) = do
										drawInWindow windowID (eraseSegment segment)
										return (rest,state)
									deadWorm _ (segments,state@(State {gamelevel=gamelevel,food=food,points=points,worm=worm,lives=lives})) = do
										drawInWindow windowID (drawGame gamelevel food points worm lives)
										enableTimer  timerID
										closeTimer   id
										enableWindowKeyboard windowID
										return (segments,state)
						in do
							id <- openId
							deadWormAlert id worm state{ food = newfood
										   , foodsupply = foods1
										   , grow       = 0
										   , dir        = rightKey
										   , worm       = newworm
										   , lives      = lives-1
			   							   }
					| otherwise =
						let
							dialog [overId,okId,editId]
								= Dialog "Game Over"
									(	TextControl	  "Game Over with a new high score!"	[ControlPos (Left,zero)]
									:+:	TextControl	  "Your name:"							[ControlPos (Left,zero)]
									:+:	EditControl   "" (PixelWidth (hmm 45.0)) 1			[ControlId	editId]
									:+:	ButtonControl "OK"									[ControlPos (Center,zero),ControlFunction (noLS overOK)]
									)
									[	WindowId		overId
									,	WindowOk		okId
									,	WindowItemSpace	(hmm 6.0) (vmm 6.0)
									]
								where
									overOK :: State -> GUI State State
									overOK state = do										
										(_, mb_name) <- getControlText editId
										state <- (case mb_name of
												Nothing -> error "OK button could not retrieved."
												Just name -> addscore name state)
										closeWindow overId state
										where
											addscore :: String -> State -> GUI State State
											addscore name state@(State {points=points,best=curBest})
												| null name = return state
												| otherwise = do
													let newBest = addScore nrOfHiScores (HiScore{name=name,score=points}) curBest
													return state{best=newBest}
						in do
							enableMenus [levelID]
							enableMenuElements [playID,quitID]
							disableMenuElements [haltID]
							disableTimer	timerID
							disableWindowKeyboard	windowID
							setWindowCursor	 windowID StandardCursor
							(if (itsAHighScore nrOfHiScores points best)
							 then do
								ids <- openIds 3
								openModalDialog undefined (dialog ids) state
								return state
							 else return state)
					| otherwise = do
						openModalDialog undefined (Notice ["Game Over, no high score."] (NoticeButton "OK" return) []) state
						return state
