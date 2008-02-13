module Main where


--	**************************************************************************************************
--
--	A simple slide game that uses bitmaps to show nice pictures.
--	On Macintosh: 
--		* Make sure the application has sufficient 'Extra memory' (Application options)
--		* Select a PICT file;
--	On Windows(9x/NT):
--		* Make sure the application has sufficient 'Heap' (Application options)
--		* Select a BMP file.
--
--	The program has been written in Haskell and uses the Port of Clean Standard Object I/O library 1.2.2
--	
--	**************************************************************************************************


import Graphics.UI.ObjectIO
import Graphics.UI.ObjectIO.CommonDef
import System.Random
import Data.List(partition)
import Control.Monad(when)


{-	openSlideGame first attempts to read in the bitmap.
	If successfull, openSlideGame then checks whether the given bitmap has proper dimensions.
	If this is the case then a window is opened that will contain the slide game.
	The initial positions of the slides are determined by shuffling them nr_shuffle times randomly. 
	The local state of the window keeps track of the current position of the hole.
-}

type WindowState = Coord		-- The current position of the hole
	
data Coord
  =  Coord
  	{ col	  :: !Int		-- The zero based column number
	, row	  :: !Int		-- The zero based row    number
	} deriving Eq

main :: IO ()
main = startIO SDI () init [ProcessClose closeProcess]
	where
		init ps = do
			(maybeFile, ps) <- selectInputFile ps
			case maybeFile of
				Nothing   -> return ()
				Just file -> do
					maybeBitmap <- liftIO (openBitmap file)
					case maybeBitmap of
						Nothing -> return ()
						Just bitmap ->
	  						let
	  							bitmapsize = getBitmapSize bitmap
	  							blocksize = Size{w=(w bitmapsize) `div` 4,h=(h bitmapsize) `div` 4}
	  							
	  							seed = mkStdGen 0
								(okCoords,hole)	  = initLast [Coord{col=col,row=row} | row<-[0..3],col<-[0..3]]
								(_,coords1,hole1) = iteraten nr_shuffle shuffle (seed,zip okCoords okCoords,hole)
	  						in
	  							if not (ok_blocksize blocksize) then return ps
								else do
									windowId <- openId
									allcids  <- openIds 15
									allr2ids <- openR2Ids 15
									openSlideWindow hole1 bitmap blocksize windowId allcids allr2ids coords1 ps

		nr_shuffle = 200
		
		openSlideWindow hole bitmap blocksize windowId allcids allr2ids coords =
			openWindow hole (window bitmap blocksize windowId allcids allr2ids coords)

		ok_blocksize (Size {w=w,h=h}) =
			isBetween w minBlockWidth maxBlockWidth && isBetween h minBlockHeight maxBlockHeight
			where
				minBlockWidth  = 20
				minBlockHeight = 20
				maxBlockWidth  = maxWinWidth  `div` 4
				maxBlockHeight = maxWinHeight `div` 4
				
				Size{w=maxWinWidth,h=maxWinHeight} = maxFixedWindowSize

		shuffle :: (StdGen,[(Coord,Coord)],Coord) -> (StdGen,[(Coord,Coord)],Coord)
		shuffle (seed,coords,hole) =
			let 
				(candidates,others)	= partition (\(okCoord,coord)->distCoord coord hole==1) coords
			  	(random_nr,seed1)	= random seed
			  	(before,(okCandidate,candidate):after) = splitAt (random_nr `mod` (length candidates)) candidates
			in
				(seed1,before++((okCandidate,hole):after)++others,candidate)


{-	window defines the Window that shows the slide game.
	It contains a list of slide controls.
	Closing the window will terminate the program.
-}
window :: Bitmap -> Size -> Id -> [Id] -> [SlideR2Id] -> [(Coord,Coord)] -> Window (ListLS SlideControl) WindowState ps
window bitmap blocksize windowId allcids allr2ids coords
	= Window "SlideGame"
		(	ListLS (map (slideControl bitmap blocksize windowId allr2ids) coord_ids)
		)
		[	WindowClose 	(noLS closeProcess)
		,	WindowId	windowId
		,	WindowItemSpace 0 0
		,	WindowViewSize	Size{w=4*(w blocksize),h=4*(h blocksize)}
		,	WindowPen	[PenBack grey]
		]
	where
		coord_ids = zip3 coords allcids allr2ids


{-	slideControl defines one slide control of the slide game.
	A slide control consists of two components:
	*	A custom button control:
			This control shows a part of the bitmap image.
			Selecting this control will swap places with the current hole iff it is adjacent to the 
			hole.
			It checks whether all slide controls are at their desired locations, and if so disables 
			the window.
			Note that disabling the window will disable all slide controls. The look of a slide control 
			is such that in disabled state it will not frame its bitmap part, so the complete bitmap 
			will be displayed.
	*	A receiver control:
			This control handles external requests that inform whether the slide control is at its 
			desired position.
-}

type SlideState = Coord				-- The local state of a slide control
type SlideMsgIn					-- The ingoing messages of the slide control
	= ()					-- Inform whether the control is currently at its desired location
type SlideMsgOut 				-- The outgoing messages of the slide control
	= Bool					-- True iff the control is currently at its desired location
type SlideR2Id = R2Id SlideMsgIn SlideMsgOut	-- Shorthand for the receiver id of a slide control
	
type SlideControl				-- Shorthand for the slide control constructor type
	= AddLS (TupLS CustomButtonControl (Receiver2 SlideMsgIn SlideMsgOut))

slideControl :: Bitmap -> Size -> Id -> [SlideR2Id] -> ((Coord,Coord),Id,SlideR2Id) -> SlideControl WindowState ps
slideControl bitmap size windowId allr2ids ((okCoord,initCoord),cid,r2id) =
	AddLS initCoord (custombutton :+: receiver2)
	where
		others = filter (==r2id) allr2ids

		custombutton = CustomButtonControl size slideLook
					[ ControlPos	  (LeftTop,(offset initCoord))
					, ControlFunction slideMove
					, ControlId	  cid
					]
		slideLook select UpdateState{newFrame=newFrame} = do
			drawAt (Point2 {x=0-(col okCoord)*(w size),y=0-(row okCoord)*(h size)}) bitmap
			when (enabled select) (draw newFrame)

		offset (Coord{col=col,row=row}) = Vector2{vx=(w size)*col,vy=(h size)*row}

		slideMove :: ((SlideState,WindowState),ps) -> GUI ps ((SlideState,WindowState),ps)
		slideMove ((curCoord,curHole),ps)
			| distCoord curCoord curHole /= 1 = return ((curCoord,curHole),ps)
			| otherwise = do			
				setControlPos windowId [(cid,(LeftTop,(offset curHole)))]
				(if curHole /= okCoord then return ((curHole,curCoord), ps)
				 else do
					(others_ok,ps) <- stateMapM areYouOk others ps
--					when (all others_ok) (disableWindow windowId)
					return ((curHole,curCoord), ps))

		areYouOk :: SlideR2Id -> ps -> GUI ps (Bool,ps)
		areYouOk r2id ps = syncSend2 r2id () ps

		receiver2 = Receiver2 r2id receive2 []

		receive2 :: SlideMsgIn -> ((SlideState,WindowState),ps) -> GUI ps (SlideMsgOut,((SlideState,WindowState),ps))
		receive2 () ((curCoord,curHole),ps) = return (okCoord==curCoord,((curCoord,curHole),ps))

--	The distance between two Coords:
distCoord :: Coord -> Coord -> Int
distCoord coord1 coord2
	= abs (col coord1-col coord2) + abs (row coord1-row coord2)

instance Zero Coord where
	zero = Coord{col=0,row=0}

iteraten :: Int -> (a -> a) -> a -> a
iteraten 0 f x = x
iteraten n f x = iteraten (n-1) f (f x)
