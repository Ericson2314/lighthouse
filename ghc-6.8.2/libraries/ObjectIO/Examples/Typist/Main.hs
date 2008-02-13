module Main where


--	**************************************************************************************************
--
--	This program creates two interactive processes:
--	-	One process presents a simple text window in which text can be typed (this module).
--	-	The other process keeps track of the number of typed keys per minute (monitor module).
--	Communication is done by means of message passing.
--	In a future distributed version the two processes can be run on different processors.
--
--	The program has been written in Haskell and uses the Port of Clean Standard Object I/O library 1.2
--	
--	**************************************************************************************************


import Graphics.UI.ObjectIO
import Monitor


nrOfLines		= 10
nrOfMaxCharsPerLine	= 30

type Typist = Bool

main :: IO ()
main = do
	metrics <- doScreenDraw (getFontMetrics dialogFont)
	rId <- openRId
	ids <- openIds 5
	startProcesses (typistProcess dialogFont metrics rId ids)

typistProcess :: Font -> FontMetrics -> RId MonitorMessage -> [Id] -> Process
typistProcess font metrics rId [wId,editId,mId,runId,tId] =
	Process SDI False initIO [ProcessClose quit]
	where
	--	initIO initialises the typist process.
		initIO typist = do
			typist <- openWindow undefined window typist
			typist <- openMenu   undefined menu   typist
			typist <- openTimer  undefined timer  typist
			Just pos <- getWindowPos wId
			let monitorpos = (LeftTop,pos{vy=vy pos+h wSize})
			openMonitor monitorpos rId
			return typist

	--	window is the single document of the typist process. 
	--	Keyboard information is sent to the monitor process.
		window	= Window "Typist window" 
				( EditControl "" (PixelWidth (w wSize-2*fMaxWidth metrics)) nrOfLines
					[ ControlKeyboard	keyFilter Able (noLS1 sendKeys)
					, ControlId		editId
					, ControlSelectState	Unable
					, ControlPos		(Center,zero)
					, ControlTip		"Type your text in here"
					, ControlResize		(\_ _ newWindowSize->newWindowSize)
					]
				)
				[	WindowId	wId
				,	WindowViewSize	wSize
				]
			where
		--		Filter only non-repeating character keys to the monitor process.
				keyFilter (CharKey _ (KeyDown repeat)) 		= not repeat
				keyFilter (SpecialKey key (KeyDown repeat) _) 	= key `elem` [backSpaceKey,deleteKey] && not repeat
				keyFilter _					= False

		--		Key messages start with BeginSession so that monitoring will start after the first key hit.
		--		Only after the first key hit the timer stopwatch is activated.
				sendKeys :: KeyboardState -> Typist -> GUI Typist Typist
				sendKeys keyboard typist
					| typist = do
						asyncSend rId (KeyHit char)
						return typist
					| otherwise = do
						asyncSend rId BeginSession
						enableTimer tId
						asyncSend rId (KeyHit char)
						return True
					where
						char = case (fromJust (getKeyboardStateKey keyboard)) of
							IsCharKey    c -> c
							IsSpecialKey c -> '\b'

		wSize = Size
			{ w = (nrOfMaxCharsPerLine+2)*fMaxWidth metrics
			, h = (nrOfLines+2)*(fontLineHeight metrics)
			}

	--	menu defines the commands of the typist process. 
		menu = Menu "File"
			(	MenuItem "Run"  [MenuShortKey 'r', MenuFunction (noLS run), MenuId runId]
			:+:	MenuSeparator	[]
			:+:	MenuItem "Quit" [MenuShortKey 'q', MenuFunction (noLS quit)]
			)
			[	MenuId	mId
			]
			where
		--		run starts a session by enabling the edit control that receives the keyboard input.
				run :: Typist -> GUI Typist Typist
				run typist = do
					setControlText editId ""
					enableControl  editId
					disableMenuElements [runId]
					return False

	--	timer will end a typing session after 60 seconds. The timer is enabled by sendKeys.	
		timer = Timer (60*ticksPerSecond) NilLS
			[ TimerId		tId
			, TimerSelectState	Unable
			, TimerFunction		(noLS1 endOfSession)
			]
			where
		--		The monitor process is notified of the end of the session by receiving the EndSession message.
				endOfSession :: NrOfIntervals -> Typist -> GUI Typist Typist
				endOfSession _ typist = do
					disableTimer tId
					disableControl editId
					enableMenuElements [runId]
					asyncSend rId EndSession
					return typist

	--	quit closes boths processes. 
		quit :: Typist -> GUI Typist Typist
		quit typist = do
			asyncSend rId Quit
			typist <- closeProcess typist
			return typist
