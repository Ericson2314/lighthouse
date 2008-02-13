module WormState where

import	Graphics.UI.ObjectIO
import	HighScore
import  Random


--	The worm data types.

data State
   = State
	{ gamelevel	:: Level
	, food		:: Food
	, foodsupply	:: [Food]
	, grow		:: Grow
	, points	:: Points
	, dir		:: SpecialKey
	, worm		:: Worm
	, best		:: HiScores
	, lives		:: Lives
	}
data Level
   = Level
	{ fix		:: Int
	, speed		:: Int
	, level		:: Int
	, obstacles	:: [Obstacle]
	}
data Food
   = Food
   	{ value		:: Int
	, pos		:: Point2
	}
type Grow		= Int
type Obstacle 		= Rectangle
type Segment		= Point2
type Worm		= [Segment]
type Points		= Int
type Lives		= Int

sizeX			= 45 :: Int
sizeY			= 26 :: Int

nrOfWorms		= 4 :: Int
nrOfLevels		= 8 :: Int
pointsPerLevel		= 500 :: Int
startLevel		= 0 :: Int

easySpeed		= ticksPerSecond `div` 6
mediumSpeed		= ticksPerSecond `div` 9
hardSpeed		= ticksPerSecond `div` 18
accelation		= ticksPerSecond `div` 18


--	Initial State.
initState :: HiScores -> State
initState best =
	State
	  { gamelevel	= initlevel
	  , food	= initfood
	  , foodsupply	= []
	  , grow	= 0
	  , points	= 0
	  , dir		= rightKey
	  , worm	= initworm
	  , best	= best
	  , lives	= nrOfWorms
	  }
	where
		initfood	= Food{value=0,pos=Point2{x=(-10),y=(-10)}}
		initlevel	= initLevel easySpeed
		initworm	= newWorm initlevel


--	Make a new initial worm.
newWorm :: Level -> Worm
newWorm Level{level=level} =
	[Point2{x=x,y=y} | x<-[5,4..1]]
	where
		y = startHeights !! (level `mod` nrOfLevels)

startHeights :: [Int]
startHeights = [13,5,13,13,13,1,1,14]


--	Construct the next level.
initLevel :: Int -> Level
initLevel fix =
	Level {fix=fix,speed=fix,level=startLevel,obstacles=sampleObstacles!!startLevel}

decreaseLevel :: Level -> Level
decreaseLevel curlevel@(Level {speed=speed,level=level}) =
	let
		newLevel = level-1
	  	newSpeed = if level `mod` nrOfLevels==0 && level/=0 then speed+accelation else speed
	in
		curlevel
		  { fix		= newSpeed
		  , speed	= newSpeed
		  , level	= newLevel
		  , obstacles	= sampleObstacles !! (newLevel `mod` nrOfLevels)
	  	  }

increaseLevel :: Level -> Level
increaseLevel curlevel@(Level {speed=speed,level=level}) =
	let
		newLevel = level+1
		newSpeed = if level `mod` nrOfLevels==0 && level/=0 then speed-accelation else speed
	in
		curlevel
		  { fix		= newSpeed
		  , speed	= newSpeed
		  , level	= newLevel
		  , obstacles	= sampleObstacles !! (newLevel `mod` nrOfLevels)
	  	  }

sampleObstacles :: [[Obstacle]]
sampleObstacles =
		[ []
	    	, [Rectangle (Point2 12 11) (Point2 34 16)]
	    	, [Rectangle (Point2 12  1) (Point2 34  3), Rectangle (Point2 12 24) (Point2 34 26)]
		, [Rectangle (Point2  7  7) (Point2 38  9), Rectangle (Point2  7 17) (Point2 38 19)]
		, [Rectangle (Point2  1  1) (Point2 18 10), Rectangle (Point2 28 17) (Point2 45 26)]
		, [Rectangle (Point2 14  3) (Point2 15 24), Rectangle (Point2 30  3) (Point2 31 24)]
		, [Rectangle (Point2  3 13) (Point2 43 14), Rectangle (Point2 22  3) (Point2 24 24)]
		, [Rectangle (Point2  3  3) (Point2 20 12), Rectangle (Point2 26 15) (Point2 43 24)]
		]

--	Generate a food supply.
instance Random Food where  
	random seed = (Food{value=value,pos=pos}, seed3)
		where
			(random1,seed1)	= random seed
			(random2,seed2)	= random seed1
			(random3,seed3)	= random seed2
			foodx		= (incMod random2 (sizeX-2))+1
			foody		= (incMod random3 (sizeY-2))+1
			pos		= Point2{x=foodx,y=foody}
			value		= incMod random1 9

			incMod a b	= (a `mod` b)+1

	randomR _ seed = random seed

--	Think of some new random food.
newFood :: Worm -> Level -> [Food] -> (Food, [Food])
newFood worm level@(Level {obstacles=obstacles}) (food@(Food{pos=pos}):foods)
	| pos `elem` worm || any (inRectangle pos) obstacles = newFood worm level foods
	| otherwise					     = (food, foods)
	where
		inRectangle :: Point2 -> Obstacle -> Bool
		inRectangle (Point2{x=x,y=y}) (Rectangle (Point2 lx ty) (Point2 rx by)) =
			x>=lx && x<=rx && y>=ty && y<=by
