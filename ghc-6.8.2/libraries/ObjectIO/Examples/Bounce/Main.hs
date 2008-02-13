module Main where


--	**************************************************************************************************
--
--	A program that creates two interactive processes that bounce balls in an open-ended barrel.
--
--	The program has been written in Clean 1.3.2 and uses the Clean Standard Object I/O library 1.2
--	
--	**************************************************************************************************

import Graphics.UI.ObjectIO
import BounceTypes
import BounceDraw

data Message						-- The message type:
   = BallsArrive [Ball]					-- balls that have crossed process border
   | BounceOpened					-- the other bounce process has been created
   | QuitBounce						-- quit the bounce process
data Bounce						-- The local program state:
   = Bounce
   	{ talkTo :: RId Message 			-- who to play with
	, barrel :: Barrel				-- the shape of the barrel
	, balls	 :: [Ball]				-- the balls in the barrel
	}

--	Create the initial interactive process:

main :: IO ()
main = do
	rIdA <- openRId
	rIdB <- openRId
	wIdA <- openId
	wIdB <- openId
	tIdA <- openId
	tIdB <- openId
	startProcesses
		[ bounce wIdB tIdB rIdB rIdA "Bounce B" RightTop rightBarrelSetUp
		, bounce wIdA tIdA rIdA rIdB "Bounce A" LeftTop  leftBarrelSetUp
		]

bounce :: Id -> Id -> RId Message -> RId Message -> Title -> ItemLoc -> (Barrel,[Ball]) -> Process
bounce wId tId me you name itemLoc (barrel,balls) =
	Process SDI initLocal initIO [ProcessClose quit]
	where
		barrelDomain	= bDomain barrel
		barrelSize	= rectangleSize barrelDomain
		maxSize		= maxFixedWindowSize
		windowSize	= Size{w=min (w barrelSize) (w maxSize `div` 2),h=min (h barrelSize) (h maxSize `div` 2)}
		splitWalls	= splitWallsInBarrel barrel
		initLocal	= Bounce{talkTo=you,barrel=barrel,balls=balls}

		initIO ps = do
			ps <- openWindow   undefined window   ps
			ps <- openMenu     undefined menu     ps
			ps <- openTimer    undefined timer    ps
			ps <- openReceiver undefined receiver ps
			return ps

	--	window defines the window that displays the barrel and the current balls.
		window	= Window name NilLS
				[ WindowId		wId
				, WindowLook		False (updateBalls initLocal)
				, WindowViewSize	windowSize
				, WindowPos		(itemLoc,zero)
				]

		updateBalls :: Bounce -> SelectState -> UpdateState -> Draw ()
		updateBalls (Bounce {balls=balls,barrel=barrel}) _ (UpdateState{oldFrame=oldFrame,newFrame=newFrame,updArea=updArea}) = do
			drawBarrel area scale barrel			
			mapM_ (drawBall scale (corner1 domain)) balls
			where
				domain		= bDomain barrel
				windowSize	= rectangleSize newFrame
				barrelSize	= rectangleSize domain
				scale		= scaleSize windowSize barrelSize
				area		= if oldFrame==newFrame then updArea else [newFrame]

	--	menu defines the bounce menu. It contains only the quit command to terminate the application.
		menu	= Menu name
				(	MenuItem "About Bounce..."	[MenuFunction (noLS bounceHelp)]
				:+:	MenuSeparator			[]
				:+:	MenuItem "Quit"			[MenuFunction (noLS quit),MenuShortKey 'q']
				)	[]

		quit :: Bounce -> GUI Bounce Bounce
		quit bounce@(Bounce {talkTo=talkTo}) = do
			asyncSend talkTo QuitBounce
			closeProcess bounce

	--	timer defines the timer that will calculate the movements of the current balls as often as possible.
		timer	= Timer 0 NilLS 
				[ TimerId			tId
				, TimerFunction	(noLS1 (bounceBalls splitWalls))
				]
			where
				bounceBalls :: ([SingleWall],[SingleWall]) -> NrOfIntervals -> Bounce -> GUI Bounce Bounce
				bounceBalls splitWalls _ bounce@(Bounce {talkTo=talkTo,balls=balls,barrel=barrel}) = do
					windowSize <- getWindowViewSize wId
					let scale = scaleSize windowSize barrelSize
					let eraseOld = map (eraseBall scale base) balls
					let drawNew  = map (drawBall  scale base) ins
					drawInWindow wId (sequence_ (eraseOld++drawNew))
					let bounce1 = bounce{balls=ins}
					setWindowLook wId False (False,updateBalls bounce1)
					(if null outs then return () else asyncSend talkTo (BallsArrive outs))
					return bounce1
					where
						nextBallPos	= nextBallPositions splitWalls balls
						ballsMoved	= map moveBall nextBallPos
						domain		= bDomain barrel
						base		= corner1 domain
						barrelSize	= rectangleSize domain
						(ins,outs)	= splitBallsInBarrel domain ballsMoved

	--	receiver defines the receiver that will receive new balls and termination requests.
		receiver :: Receiver Message ls Bounce
		receiver = Receiver me (receive splitWalls) []
			where
				receive :: ([SingleWall],[SingleWall]) -> Message -> GUIFun ls Bounce
				receive (horizontal,vertical) (BallsArrive newBalls) (ls, bounce@(Bounce {balls=oldBalls})) = do
					let newBalls1 = map correctBall newBalls
					return (ls, bounce{balls=newBalls1++oldBalls})
					where
						correctBall :: Ball -> Ball
						correctBall ball@(Ball {bCenter=center,bSpeed=speed}) =
							let 
								ball1 = ball{bCenter=movePoint (-speed) center}
								ball2 = checkVerticalWalls   vertical   ball1
								ball3 = checkHorizontalWalls horizontal ball2
								ball4 = moveBall ball3
							in
								ball4
				receive _ BounceOpened (ls, bounce) = do
					enableTimer tId
					return (ls, bounce)
				receive _ QuitBounce   (ls, bounce) = do
					bounce <- closeProcess bounce
					return (ls, bounce)

	--	bounceHelp opens a dialog that tells something about this application.
		bounceHelp :: Bounce -> GUI Bounce Bounce
		bounceHelp bounce = do
			okId <- openId
			openModalDialog undefined (dDef okId) bounce
			return bounce
			where
				dDef okId = Dialog "About bounce"
					(	TextControl   "This is a Clean program"
							[ControlPos center]
					:+:	ButtonControl "Ok"	[ControlId okId
									,ControlPos center
									,ControlFunction (noLS close)
									]
					)
					[	WindowOk okId
					]
				center	= (Center,zero)

				close :: Bounce -> GUI Bounce Bounce
				close bounce = do
					Just id <- getActiveWindow
					closeWindow id bounce


--	Determine which balls are inside and which are outside the barrel:

splitBallsInBarrel :: ViewDomain -> [Ball] -> ([Ball],[Ball])
splitBallsInBarrel domain balls =
	foldr (ballInOrOut domain) ([],[]) balls
	where
		ballInOrOut :: ViewDomain -> Ball -> ([Ball],[Ball]) -> ([Ball],[Ball])
		ballInOrOut (Rectangle{corner1=Point2{x=left,y=top},corner2=Point2{x=right,y=bottom}}) ball@(Ball {bCenter=center}) (ins,outs)
			| between (x center) left right && between (y center) top bottom = (ball:ins,outs)
			| otherwise = (ins,ball:outs)

nextBallPositions :: ([SingleWall],[SingleWall]) -> [Ball] -> [Ball]
nextBallPositions (horizontal,vertical) balls
	= map (checkHorizontalWalls horizontal) (
	  map (checkVerticalWalls   vertical)   (
	  computeNextBallPositions [] balls))
	where
		computeNextBallPositions :: [Ball] -> [Ball] -> [Ball]
		computeNextBallPositions ballsDone (ball:balls) =
			computeNextBallPositions (ballDone:newBallsDone) newBalls
			where
				(newBallsDone,newBalls,ballDone) = checkBallCollisions ballsDone balls ball

				checkBallCollisions :: [Ball] -> [Ball] -> Ball -> ([Ball],[Ball],Ball)
				checkBallCollisions balls1 balls2 ball =
					(newBalls1,newBalls2,ball2)
					where
						(newBalls1,ball1) = checkBallCollision balls1 ball
						(newBalls2,ball2) = checkBallCollision balls2 ball1

						checkBallCollision :: [Ball] -> Ball -> ([Ball],Ball)
						checkBallCollision (ball2@(Ball {bCenter=center2,bRadius=radius2,bSpeed=step2}):others)
											ball1@(Ball {bCenter=center1,bRadius=radius1,bSpeed=step1})
							| dist (bCenter (moveBall ball1)) center2 <= fromIntegral (radius1+radius2) =
								let 
									(others1,ball11)	= checkBallCollision others ball1{bSpeed=step2}
								in 
									(ball2{bSpeed=step1}:others1,ball11)
							| otherwise =
								let
									(others1,ball11)	= checkBallCollision others ball1
								in
									(ball2:others1,ball11)
						checkBallCollision others ball = (others,ball)
		computeNextBallPositions ballsDone [] = ballsDone

checkHorizontalWalls :: [SingleWall] -> Ball -> Ball
checkHorizontalWalls (((a,b),interior):walls) ball@(Ball {bCenter=center,bRadius=radius,bSpeed=speed})
	| interior /= startInterior 	= checkHorizontalWalls walls ball
	| not collision			= checkHorizontalWalls walls ball1
	| otherwise			= ball1
	where
		c		= bCenter (moveBall ball)
		speed1		= if collision then speed{vy=(0-vy speed)} else speed
		collision	=    (between (x c) (x a-radius) (x b+radius))
				  && (signum (vy speed) /= interior)
				  && (if posSign then y a+signRadius>=y c else y a+signRadius<=y c)
		signRadius	= interior*radius
		posSign		= interior>0
		startInterior	= signum (y center-y a)
		ball1		= ball{bSpeed=speed1}
checkHorizontalWalls [] ball = ball

checkVerticalWalls :: [SingleWall] -> Ball -> Ball
checkVerticalWalls (((a,b),interior):walls) ball@(Ball {bCenter=center,bRadius=radius,bSpeed=speed})
	| interior /= startInterior	= checkVerticalWalls walls ball
	| not collision			= checkVerticalWalls walls ball1
	| otherwise			= ball1
	where
		c		= bCenter (moveBall ball)
		speed1		= if collision then speed{vx=0-vx speed} else speed
		collision	=    (between (y c) (y a-radius) (y b+radius))
				  && (signum (vx speed) /= interior)
				  && (if posSign then x a+signRadius>=x c else x a+signRadius<=x c)
		signRadius	= interior*radius
		posSign		= interior>0
		startInterior	= signum (x center-x a)
		ball1		= ball{bSpeed=speed1}
checkVerticalWalls [] ball = ball

moveBall :: Ball -> Ball
moveBall ball@(Ball {bCenter=center,bSpeed=speed}) =
	ball{bCenter=movePoint speed center}
