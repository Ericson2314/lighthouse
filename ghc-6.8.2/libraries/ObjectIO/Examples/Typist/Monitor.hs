module Monitor where


--	**************************************************************************************************
--
--	This module defines a process creation function that tracks the number of typed keys in a typing
--	session. 
--	This module has been written in Haskell and uses the Port of Clean Standard Object I/O library 1.2
--
--	**************************************************************************************************


import Graphics.UI.ObjectIO
import Control.Monad(unless)


data Count
   = Count
   	{ oks	:: Int		-- nr of OK keys
	, bads	:: Int		-- nr of correction keys
	}
data Monitor
   = Monitor
	{ count		:: Count	-- current  registration of keys
	, counts	:: [Count]	-- previous registration of keys
	, time		:: Int		-- current tracking time since start (in seconds)
	, tracking	:: Bool		-- monitor is currently tracking
	}
data MonitorMessage			-- The message type of the monitor process
	= BeginSession			-- Begin a typist session
	| KeyHit Char			-- Register key stroke
	| EndSession			-- End a typist session
	| Quit				-- Close the monitor process


openMonitor :: ItemPos -> RId MonitorMessage -> GUI ps ()
openMonitor pos monitorId = do
	metrics <- liftIO (doScreenDraw (getFontMetrics dialogFont))
	ids <- openIds 2
	openProcesses (monitorProcess pos dialogFont metrics monitorId ids)

monitorProcess :: ItemPos -> Font -> FontMetrics -> RId MonitorMessage -> [Id] -> Process
monitorProcess pos font metrics monitorId [wId,tId] =
	Process SDI initLocal initIO []
	where
		initcount	= Count{oks=0,bads=0}
		initcounts	= []
		initLocal	= Monitor{count=initcount,counts=initcounts,time=0,tracking=False}
	
	--	initIO initialises the monitor process.
		initIO ps = do
			ps <- openWindow undefined window ps
			ps <- openTimer  undefined timer  ps
			ps <- openReceiver False receiver ps
			return ps
	
	--	window is the single document of the monitor process.
		window	= Window "Monitor" NilLS
				[ WindowId		wId
				, WindowPos		pos
				, WindowViewDomain	pDomain
				, WindowLook		True (monitorlook initLocal)
				, WindowInit		(noLS (\ps -> drawInWindow wId (setPenFont font) >> return ps))
				]

		pDomain	= Rectangle{corner1=zero,corner2=Point2{x=windowWidth,y=windowHeight}}
	
	--	monitorlook defines the look of the monitor window. 
		monitorlook :: Monitor -> SelectState -> UpdateState -> Draw ()
		monitorlook (Monitor{counts=counts,tracking=tracking}) _ _ = do
			drawBackground
			drawSecondsLine
			drawKeyHitsLine
			mapM_ drawKeyHitColumn (zip [0..] counts)
			unless tracking (drawTotalAndAverage font metrics counts)

	--	The timer gathers per second the number of good and bad key hits.
	--	The monitor window is updated by drawing only the new diagram bars.
		timer	= Timer ticksPerSecond NilLS
				[ TimerId		tId
				, TimerSelectState	Unable
				, TimerFunction		(noLS1 (showKeyHits False))
				]

		showKeyHits :: Bool -> NrOfIntervals -> Monitor -> GUI Monitor Monitor
		showKeyHits final dt monitor@(Monitor {count=count,counts=counts,time=time}) = do
			drawInWindow wId (mapM_ drawKeyHitColumn (zip [time..] newcounts))
			let monitor1 = monitor{count=initcount,counts=counts++newcounts,time=time+dt}
			setWindowLook wId final (True,monitorlook monitor1)
			return monitor1
			where
				missedcounts 	= if dt>1 then (replicate (dt-1) (Count{oks=0,bads=0})) else []
				newcounts	= count:missedcounts

	--	The receiver is the interface of the monitor process to the typist process.
		receiver :: Receiver MonitorMessage Bool Monitor
		receiver = Receiver monitorId receive []

		receive :: MonitorMessage -> (Bool,Monitor) -> GUI Monitor (Bool,Monitor)

	--	Starting a tracking session enables the timer and clears all previous tracking information.
		receive BeginSession (_,monitor) = do
			let monitor1 = initLocal{tracking=True}
			enableTimer  tId
			setWindowLook wId True (True,monitorlook monitor1)
			return (False,monitor1)

	--	For each key hit, only administrate whether it is a good or bad key hit.
		receive (KeyHit char) (_,monitor) =
			return (True,incCount char monitor)
			where
				incCount :: Char -> Monitor -> Monitor
				incCount c monitor@(Monitor {count=count})
					| c=='\b'	= monitor{count=count{bads=bads count+1}}
					| otherwise	= monitor{count=count{oks =oks  count+1}}

	--	Ending a session disables the timer and presents the number and average of key hits. 
		receive EndSession (firstkeyreceived,monitor@(Monitor {time=time})) = do
			let monitor1 = monitor{tracking=False}
			showKeyHits True (60-time) monitor1
			disableTimer tId
			return (firstkeyreceived,monitor1)

	--	Quit closes the monitor process.
		receive Quit (firstkeyreceived,monitor) = do
			monitor <- closeProcess monitor
			return (firstkeyreceived,monitor)

--	The drawing functions:

drawBackground :: Draw ()
drawBackground = unfill (Rectangle {corner1=zero,corner2=Point2 windowWidth windowHeight})

drawSecondsLine :: Draw ()
drawSecondsLine = do
	setPenPos (Point2 graphX graphY)
	draw (Vector2 {vx=graphWidth,vy=0})
	mapM_ drawSecond [0,10..maxNrOfSeconds]
	where
		drawSecond :: Int -> Draw ()
		drawSecond i = do
			drawAt (Point2 x graphY) (Vector2 {vx=0,vy=axisMarkSize})
			drawAt (Point2 x (graphY+secondsOffset)) (show i)
			where
				x = graphX+i*secondsWidth

drawKeyHitsLine :: Draw ()
drawKeyHitsLine = do
	setPenPos (Point2 graphX graphY)
	draw (Vector2 {vx=0,vy=0-graphHeight})
	mapM_ drawKeyHit [0,2..maxNrKeyHits]
	where
		drawKeyHit :: Int -> Draw ()
		drawKeyHit i = do
			drawAt (Point2 (graphX-axisMarkSize) y) (Vector2 {vx=axisMarkSize,vy=0})
			drawAt (Point2 x y) (show i)
			where
				x	= graphX-keyHitsOffset
				y	= graphY-i*keyHitHeight

drawTotalAndAverage :: Font -> FontMetrics -> [Count] -> Draw ()
drawTotalAndAverage font metrics counts
	| null counts	= return ()
	| otherwise	= totalAndAverage font metrics counts
	where
		totalAndAverage :: Font -> FontMetrics -> [Count] -> Draw ()
		totalAndAverage font metrics@(FontMetrics {fMaxWidth=fMaxWidth}) counts = do
			totalW   <- getFontStringWidth font totalT
			averageW <- getFontStringWidth font averageT
			sumW 	 <- getFontStringWidth font sumT
			drawAverage height
			setPenPos (Point2 graphX summaryY)
			draw totalT
			movePenPos (Vector2 {vx=0-totalW,vy=lineHeight})
			draw averageT
			setPenPos (Point2 {x=graphX+(max totalW averageW)+fMaxWidth,y=summaryY})
			draw sumT
			movePenPos (Vector2 {vx=0-sumW,vy=lineHeight})
			draw (show (round average))
			return ()
			where
				lineHeight	= fontLineHeight metrics
				summaryY	= windowHeight-summaryMargin
				seconds		= length counts
				total		= foldr (+) 0 (map (\c -> oks c - bads c) counts)
				average		= (fromIntegral total)/(fromIntegral seconds)
				height		= round (average*(fromIntegral keyHitHeight))
				sumT		= show total
				averageT	= "Average:"
				totalT		= "Total:"

				drawAverage :: Int -> Draw ()
				drawAverage height
					| height<=0	= return ()
					| otherwise	= accXorPicture (drawLine (Point2 graphX y) (Point2 (graphX+graphWidth) y))
					where
						y = graphY-height

drawKeyHitColumn :: (Int,Count) -> Draw ()
drawKeyHitColumn (i,(Count {oks=oks,bads=bads})) = do
	fill (Rectangle {corner1=Point2{x=leftX,y=yOk-1},corner2=Point2{x=rightX,y=graphY}})
	setPenColour	 red
	fill (Rectangle {corner1=Point2{x=leftX,y=yBad}, corner2=Point2{x=rightX,y=graphY}})
	drawSeparation (Point2{x=leftX,y=yBad}) (Point2{x=rightX,y=yBad}) bads
	setPenColour	 black
	return ()
	where
		yBad		= graphY-bads*keyHitHeight
		yOk		= graphY-hits*keyHitHeight
		leftX		= graphX+i *  secondsWidth
		rightX		= leftX +     secondsWidth
		hits		= oks+bads

		drawSeparation :: Point2 -> Point2 -> Int -> Draw ()
		drawSeparation a b badHits
			| badHits==0	= return ()
			| otherwise	= setPenColour white >> drawLine a b


--	Application constants:

keyHitHeight	= 10
keyHitsOffset   = 20
maxNrKeyHits	= 10
secondsWidth	= 4
secondsOffset   = 15
maxNrOfSeconds	= 60
summaryMargin	= 20
	
topMargin	= 20
leftMargin	= 60		-- note: LeftMargin  >KeyHitsOffset
rightMargin	= 30
bottomMargin	= 60		-- note: BottomMargin>SecondsOffset+SummaryMargin
	
graphWidth	= maxNrOfSeconds*secondsWidth
graphHeight	= maxNrKeyHits  *keyHitHeight
graphX		= leftMargin
graphY		= topMargin +graphHeight
axisMarkSize	= 3
windowWidth	= leftMargin+rightMargin +graphWidth
windowHeight	= topMargin +bottomMargin+graphHeight
