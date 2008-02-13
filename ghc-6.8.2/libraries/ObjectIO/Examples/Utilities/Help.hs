module Help(showAbout, showHelp) where

--	**************************************************************************************************
--
--	General utility for handling information about the application and present help.
--
--	This module has been written in Haskell and uses the Object I/O library 1.2
--	
--	**************************************************************************************************

import	Graphics.UI.ObjectIO

type InfoDef = (Size,[InfoLine])

type InfoLine = (InfoFontDef,Int,Int,String)

data InfoFontDef
	= InfoFont Font	Centered
	| NoFont	Centered
	
type Centered = Bool

data Fonts
   = Fonts
   	{ normal	:: Font
	, large		:: Font
	, bold		:: Font
	, large_bold	:: Font
	}
	
type Heights = (Int,Int)

infoFontName1	= fontName sansSerifFont
infoFontName2	= fontName serifFont
normalSize1	= 9
normalSize2	= 12
largeSize1	= 12
largeSize2	= 14
margin		= 8
aboutBegin	= "\\About"
aboutEnd	= "\\EndAbout"
helpBegin	= "\\Help"
helpEnd		= "\\EndHelp"
about		= False
help		= True


{-	showAbout opens a window:
	-	it has the title of the application name (String argument 1),
	-	it displays the about information of the application (found in the helpfile, name argument 2),
	-	it has an Ok button that closes this window, 
	-	it has a Help button that displays the help information (see showHelp).
-}
showAbout :: String -> String -> ps -> GUI ps ps
showAbout appName helpFile ps = do
	okId <- openId
	(size,infoText)	<- readInfo about infoFonts aboutBegin aboutEnd helpFile
	let about = Dialog ("About "++appName)
		(   CustomControl size (look (normal infoFonts) infoText) 
				[ ControlPos		(Center,zero)]
		:+: ButtonControl "Ok"
				[ ControlId		okId
				, ControlFunction	(noLS closeActiveWindow)
				, ControlPos		(Center,zero)
				]
		:+: ButtonControl "Help"
				[ ControlFunction	(noLS (\ps -> closeActiveWindow ps >>= showHelp helpFile))]
		) [ WindowOk okId ]
	ps <- openDialog undefined about ps
	return ps


{- showHelp opens a SDI process that displays the help information found in the helpfile. -}
showHelp :: String -> ps -> GUI ps ps
showHelp helpfile ps = do	
	(size,infoText) <- readInfo help infoFonts helpBegin helpEnd helpfile
	let window = Window "Help" NilLS
		[ WindowViewSize	size
		, WindowLook		True (look (normal infoFonts) infoText)
		, WindowHScroll		hscroll
		, WindowVScroll		vscroll
		, WindowClose		(noLS closeProcess)
		, WindowViewDomain	(zero{corner2=Point2{x=w size,y=h size}})
		]
	openProcesses (Process SDI () (openWindow undefined window) [ProcessClose closeProcess])
	return ps
	where
		hscroll curViewFrame (SliderState{sliderThumb=sliderThumb}) move
			= case move of
				SliderIncSmall -> sliderThumb+10
				SliderDecSmall -> sliderThumb-10
				SliderIncLarge -> sliderThumb+(w (rectangleSize curViewFrame))*4 `div` 5
				SliderDecLarge -> sliderThumb-(w (rectangleSize curViewFrame))*4 `div` 5
				SliderThumb x  -> x

		vscroll curViewFrame (SliderState{sliderThumb=sliderThumb}) move
			= case move of
				SliderIncSmall -> sliderThumb+10
				SliderDecSmall -> sliderThumb-10
				SliderIncLarge -> sliderThumb+(h (rectangleSize curViewFrame))*4 `div` 5
				SliderDecLarge -> sliderThumb-(h (rectangleSize curViewFrame))*4 `div` 5
				SliderThumb x  -> x

look :: Font -> [InfoLine] -> SelectState -> UpdateState -> Draw ()
look font lines _ (UpdateState{updArea=updArea}) = do
	setPenFont font
	mapM_ (\r -> unfill r >> drawInfo font ((y (corner1 r))-1) ((y (corner2 r))+40) lines) updArea


--	Try to open a prefered set of fonts to display the help and about information:

infoFonts :: Fonts
infoFonts =
      Fonts 
	{ normal    = sansSerifFont{fontSize=normalSize1}
	, large     = sansSerifFont{fontSize=largeSize1 }
	, bold      = sansSerifFont{fontSize=normalSize1,fontIsBold=True}
	, large_bold= sansSerifFont{fontSize=largeSize1, fontIsBold=True}
	}	


-- Determine the line height and leading of a given font:

getFontHeightAndAscent :: Fonts -> GUI ps ((Int,Int),(Int,Int))
getFontHeightAndAscent fonts = do
	(normalM,largeM) <- liftIO (doScreenDraw (getMetrics fonts))
	return ((fontLineHeight normalM,fAscent normalM), (fontLineHeight largeM,fAscent largeM))
	where
		getMetrics :: Fonts -> Draw (FontMetrics,FontMetrics)
		getMetrics (Fonts {normal=normal,large=large}) = do
			normalM <- getFontMetrics normal
			largeM  <- getFontMetrics large
			return (normalM,largeM)


--	Reading and pre-processing of the file containing the about- and help-info.

readInfo :: Bool -> Fonts -> String -> String -> String -> GUI ps (Size,[InfoLine])
readInfo help fonts begin end fileName = do
	metrics <- getFontHeightAndAscent fonts
	content	<- liftIO (fmap lines (readFile fileName))
	let (found,info,content1) = readInfoFile begin end content
	processInfoStrings fonts metrics 
		(if not found
		 then [if help then (errpref ++ "does not contain help information.") else "\\DThis is a Haskell program."]
		 else info)
	where
		errpref	= "The help file \'"++fileName++"\' "

		processInfoStrings :: Fonts -> ((Int,Int),(Int,Int)) -> [String] -> GUI ps InfoDef
		processInfoStrings fonts ((normalHeight,normalAscent),(largeHeight,largeAscent)) lines = do
			(size,lines) <- addFontToInfoLines fonts (normalHeight,largeHeight) 0 (margin+largeAscent) lines
			let width = margin+w size+margin
			infoLines <- mapM (centerInfoLine (normal fonts) width) lines
			return (Size {w=width,h=h size+margin-largeAscent},infoLines)
			where
				addFontToInfoLines :: Fonts -> Heights -> Int -> Int -> [String] -> GUI ps InfoDef
				addFontToInfoLines fonts heights maxx maxy (line:rest) = do
					(font,wid,hgt,line) <- parseInfoLine fonts heights line
					(size,rest) <- addFontToInfoLines fonts heights (max maxx wid) (maxy+hgt) rest
					return (size,((font,margin,maxy,line):rest))
					where
					{-	parseInfoLine determines the font that should be used to draw the line.
						If line == '\{L,b,B,c,C,d,D}'++line1 then a special font is used, otherwise the default font is used.
						parseInfoLine also calculates the width and height of the line.
					-}
						parseInfoLine :: Fonts -> Heights -> String -> GUI ps (InfoFontDef,Int,Int,String)
						parseInfoLine fonts@(Fonts {normal=normal,large=large,bold=bold,large_bold=large_bold}) heights@(nhgt,lhgt) line				
							| linelen<2 || head line /= '\\' = do
								width <- liftIO (doScreenDraw (getFontStringWidth normal line))
								return (NoFont False,width,nhgt,line)
							| otherwise =
								let 
									(infofont,font,height) = case line !! 1 of
										'L' -> (InfoFont large      False, large,      lhgt)
										'b' -> (InfoFont bold       False, bold,       nhgt)
										'B' -> (InfoFont large_bold False, large_bold, lhgt)
										'c' -> (NoFont   True,             normal,     nhgt)
										'C' -> (InfoFont large      True , large,      lhgt)
										'd' -> (InfoFont bold       True , bold,       nhgt)
										'D' -> (InfoFont large_bold True , large_bold, lhgt)
										_   -> (NoFont   False,            normal,     nhgt)
									lineText = tail (tail line)
								in do
									width <- liftIO (doScreenDraw (getFontStringWidth font lineText))
									return (infofont,width,height,lineText)
							where
								linelen	= length line
				addFontToInfoLines _ _ maxx maxy [] = return (Size {w=maxx,h=maxy},[])

				centerInfoLine :: Font -> Int -> InfoLine -> GUI ps InfoLine
				centerInfoLine nft maxx info@(NoFont centered,x,y,line)
					| not centered = return info
					| otherwise    = do
						width <- liftIO (doScreenDraw (getFontStringWidth nft line))
						return (NoFont centered,(maxx-width) `div` 2,y,line)
				centerInfoLine nft maxx info@(InfoFont font centered,x,y,line)
					| not centered = return info
					| otherwise    = do
						width <- liftIO (doScreenDraw (getFontStringWidth font line))
						return (InfoFont font centered,(maxx-width) `div` 2,y,line)

		readInfoFile :: String -> String -> [String] -> (Bool,[String],[String])
		readInfoFile begin end content =
			let (begin_found,content1) = findInfoBegin begin content
			in if not begin_found then (False,[],content1)
			   else let (lines,content2) = readInfoUntil end content1
				in (True,lines,content2)
			where
				findInfoBegin :: String -> [String] -> (Bool,[String])
				findInfoBegin begin [] 		 = (False, [])
				findInfoBegin begin (line:lines)
					| isPrefixOf begin line	= (True,lines)
					| otherwise		= findInfoBegin begin lines

				readInfoUntil :: String -> [String] -> ([String],[String])
				readInfoUntil end [] = ([],[])
				readInfoUntil end (line:lines)			
					| isPrefixOf end line = ([],lines)
					| otherwise =
						let (lines1,content) = readInfoUntil end lines
						in (line:lines1,content)

				isPrefixOf :: String -> String -> Bool
				isPrefixOf prefix string = prefix == take (length prefix) string


--	The drawing of the about/help info.

drawInfo :: Font -> Int -> Int -> [InfoLine] -> Draw ()
drawInfo defaultfont top bot ((InfoFont font c,x,y,line):rest)
	| y>bot		= return ()
	| y<top		= drawInfo defaultfont top bot rest
	| otherwise 	= do
		setPenFont font
		drawAt (Point2 {x=x,y=y}) line
		setPenFont defaultfont
		drawInfo defaultfont top bot rest
drawInfo defaultfont top bot ((NoFont c,x,y,line):rest)
	| y>bot		= return ()
	| y<top		= drawInfo defaultfont top bot rest
	| otherwise	= do
		drawAt (Point2 {x=x,y=y}) line
		drawInfo defaultfont top bot rest
drawInfo _ _ _ _ = return ()
