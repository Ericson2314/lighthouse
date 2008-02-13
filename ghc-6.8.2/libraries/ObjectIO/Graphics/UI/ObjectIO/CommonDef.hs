-----------------------------------------------------------------------------
-- |
-- Module      :  CommonDef
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- CommonDef defines common types for the Object I\/O library and access-rules.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.CommonDef		
		(
		-- * Common definitions
		
		-- ** Type declarations
                  St, Cond, UCond,
                  
                -- ** Function declarations
		  toSt, setBetween, isBetween, minmax
		, fst3snd3, fst3thd3, snd3thd3, eqfst2id, eqfst3id,
		  
		-- * Bound type
		  Bound(..),
		  
		-- ** Operations
		  zeroBound, decBound, incBound,
		  
		-- * Calculation rules on Point2s, Sizes, Rectangles and Vector2s:
		  Graphics.UI.ObjectIO.OS.Types.Rect(..),
		  
		-- ** Function declarations
		  addPointSize
		, rectangleToRect, rectToRectangle
		, isEmptyRect, isEmptyRectangle
		, pointInRect, pointInRectangle
		, posSizeToRect, posSizeToRectangle
		, sizeToRect, sizeToRectangle, rectSize
		, disjointRects, intersectRects, subtractRects,
		  
		-- ** Class declarations
		  AddVector(..), SubVector(..), ToTuple(..), FromTuple(..)
		, ToTuple4(..), FromTuple4(..),		

		-- * List operations		
		  isSingleton, initLast, split, condMap, uspan
		, filterMap, stateMap, ucontains, cselect, access, accessList
		, remove, uremove, creplace, ucreplace, replaceOrAppend, ureplaceOrAppend
		, removeCheck, removeSpecialChars, disjointLists, noDuplicates, unzip4
		, stateMapM, foldrM, sequenceMap, ssequence, gather,

		-- * Error generation
		  dumpFatalError, dummy,
		  
                -- * A visible module
                  module Graphics.UI.ObjectIO.StdIOCommon
                ) where


import Graphics.UI.ObjectIO.OS.Types (Rect(..))
import Graphics.UI.ObjectIO.StdIOCommon

-- | State transformer
type St s a = s -> (a,s)

-- | Simple predicate
type	Cond  x = x -> Bool

-- | Predicate which returns its updated argument
type	UCond x = x -> (Bool,x)

-- | This function is convenient for lifting a function to a 'St' transformer.
toSt :: (x -> y) -> St x y
toSt acc x = (acc x,x)

setBetween :: Int -> Int -> Int -> Int
setBetween x low up
	| x<=low     = low
	| x>=up      = up
	| otherwise  = x

isBetween :: Int -> Int -> Int -> Bool
isBetween x low up
	| x<low      = False
	| otherwise  = x<=up

minmax :: Int -> Int -> (Int,Int)
minmax a b
	| a<=b       = (a,b)
	| otherwise  = (b,a)


{-	Calculation rules on Point2s, Sizes, and Vector2s:
-}
addPointSize :: Size -> Point2 -> Point2
addPointSize size point = Point2 {x=x point+w size,y=y point+h size}


instance Zero Rect where
	zero = Rect {rleft=0,rtop=0,rright=0,rbottom=0}

class AddVector a where
	addVector :: Vector2 -> a -> a		-- add the vector argument to the second argument

instance AddVector Point2 where
	addVector v p
		= Point2 {x=x p+vx v,y=y p+vy v}
instance AddVector Rect where
	addVector v r
		= Rect {rleft=rleft r+vx v,rtop=rtop r+vy v,rright=rright r+vx v,rbottom=rbottom r+vy v}
instance AddVector Rectangle where
	addVector v r
		= Rectangle {corner1=addVector v (corner1 r),corner2=addVector v (corner2 r)}


class SubVector a where
	subVector :: Vector2 -> a -> a		-- subtract the vector argument from the second argument

instance SubVector Point2 where
	subVector v p
		= Point2 {x=x p-vx v,y=y p-vy v}
instance SubVector Rect where
	subVector v r
		= Rect {rleft=rleft r-vx v,rtop=rtop r-vy v,rright=rright r-vx v,rbottom=rbottom r-vy v}
instance SubVector Rectangle where
	subVector v r
		= Rectangle {corner1=subVector v (corner1 r),corner2=subVector v (corner2 r)}

rectangleToRect :: Rectangle -> Rect
rectangleToRect r
	| x_less_x' && y_less_y' = Rect {rleft=a,  rtop=b,  rright=a', rbottom=b'}
	| x_less_x'              = Rect {rleft=a,  rtop=b', rright=a', rbottom=b }
	| y_less_y'              = Rect {rleft=a', rtop=b,  rright=a,  rbottom=b'}
	| otherwise              = Rect {rleft=a', rtop=b', rright=a,  rbottom=b }
	where
		c1               = corner1 r
		c2               = corner2 r
		a                = x c1
		b                = y c1
		a'               = x c2
		b'               = y c2
		x_less_x'        = a<=a'
		y_less_y'        = b<=b'

rectToRectangle :: Rect -> Rectangle
rectToRectangle r
	= Rectangle {corner1=Point2 {x=rleft r,y=rtop r},corner2=Point2 {x=rright r,y=rbottom r}}

isEmptyRect :: Rect -> Bool
isEmptyRect r
	= rleft r==rright r || rtop r==rbottom r

isEmptyRectangle :: Rectangle -> Bool
isEmptyRectangle r
	= x (corner1 r) == x (corner2 r) || y (corner1 r) == y (corner2 r)

pointInRect :: Point2 -> Rect -> Bool
pointInRect p r
	= isBetween (x p) (rleft r) (rright r) && isBetween (y p) (rtop r) (rbottom r)

pointInRectangle :: Point2 -> Rectangle -> Bool
pointInRectangle point rectangle
	= pointInRect point (rectangleToRect rectangle)

posSizeToRect :: Point2 -> Size -> Rect
posSizeToRect point size
	= let
		(left,right) = minmax (x point) (x point+w size)
		(top,bottom) = minmax (y point) (y point+h size)
	in	Rect {rleft=left,rtop=top, rright=right,rbottom=bottom}

posSizeToRectangle :: Point2 -> Size -> Rectangle
posSizeToRectangle pos size
	= Rectangle {corner1=pos,corner2=Point2 {x=x pos+w size,y=y pos+h size}}

sizeToRect :: Size -> Rect
sizeToRect size
	= posSizeToRect zero size

sizeToRectangle :: Size -> Rectangle
sizeToRectangle size
	= zero {corner2=Point2 {x=w size,y=h size}}

disjointRects :: Rect -> Rect -> Bool
disjointRects rect1 rect2
	= isEmptyRect rect1 || 
	  isEmptyRect rect2 || 
	  rleft rect1>=rright rect2 || rbottom rect1<=rtop rect2 || rright rect1<=rleft rect2 || rtop rect1>=rbottom rect2

intersectRects :: Rect -> Rect -> Rect
intersectRects rect1 rect2
	| disjointRects rect1 rect2	= zero
	| otherwise			= Rect	{ rleft  = max (rleft   rect1) (rleft   rect2)
						, rtop   = max (rtop    rect1) (rtop    rect2)
						, rright = min (rright  rect1) (rright  rect2)
						, rbottom= min (rbottom rect1) (rbottom rect2)
						}

subtractRects :: Rect -> Rect -> [Rect]
subtractRects rect1 rect2
	= let
	--	subtractFittingRect r1 r2 subtracts r2 from r1 assuming that r2 fits inside r1
		subtractFittingRect :: Rect -> Rect -> [Rect]
		subtractFittingRect rect1 rect2
			= let
				l1 = rleft rect1;	t1 = rtop rect1;	r1 = rright rect1;	b1 = rbottom rect1
				l2 = rleft rect2;	t2 = rtop rect2;	r2 = rright rect2;	b2 = rbottom rect2
			in	filter (not . isEmptyRect) (map fromTuple4 [(l1,t1,r1,t2),(l1,t2,l2,b2),(r2,t2,r1,b2),(l1,b2,r1,b1)])
	in	subtractFittingRect rect1 (intersectRects rect1 rect2)


rectSize :: Rect -> Size
rectSize r
	= Size {w=abs (rright r-rleft r),h=abs (rbottom r-rtop r)}


{-	Conversion of Size, Point2, and Vector2 to tuples (toTuple) and from tuples (fromTuple):
-}
class ToTuple a where
	toTuple :: a -> (Int,Int)
class FromTuple a where
	fromTuple :: (Int,Int) -> a

instance ToTuple Size where
	toTuple size = (w size,h size)
instance ToTuple Point2 where
	toTuple point = (x point,y point)
instance ToTuple Vector2 where
	toTuple v = (vx v,vy v)

instance FromTuple Size where
	fromTuple (w,h) = Size {w=w,h=h}
instance FromTuple Point2 where
	fromTuple (x,y) = Point2 {x=x,y=y}
instance FromTuple Vector2 where
	fromTuple (vx,vy) = Vector2 {vx=vx,vy=vy}

{-	Conversion of Rect, and Rectangle to 4-tuples (toTuple4) and from 4-tuples (fromTuple4):
-}
class ToTuple4 a where
	toTuple4 :: a -> (Int,Int,Int,Int)
class FromTuple4 a where
	fromTuple4 :: (Int,Int,Int,Int) -> a

instance ToTuple4 Rect where
	toTuple4 r = (rleft r,rtop r,rright r,rbottom r)
instance ToTuple4 Rectangle where
	toTuple4 r = toTuple4 (rectangleToRect r)
instance FromTuple4 Rect where
	fromTuple4 r = rectangleToRect (fromTuple4 r)
instance FromTuple4 Rectangle where
	fromTuple4 (l,t,r,b) = Rectangle {corner1=Point2 {x=l,y=t},corner2=Point2 {x=r,y=b}}


-- | Error generation rule.
-- Evaluation causes termination with the message:
--	Fatal error in rule \<rule\> \[module\] \<message\> .

dumpFatalError :: String -> String -> String -> x
dumpFatalError rule moduleName msg
	= error ("Fatal error in rule " ++ rule ++ " [" ++ moduleName ++ "]: " ++ msg ++ ".\n")


-- | Universal dummy value.
-- Evaluation causes termination with the message:
-- 	Fatal error: dummy evaluated! \<message\> .

dummy :: String -> x
dummy msg = error ("Fatal error: dummy evaluated! " ++ msg ++ ".\n")

{-	Bound data type:
-}
data	Bound
	= Finite Int		-- Fix a finite positive bound of N
	| Infinite		-- No bound

instance Eq Bound where
	(==) (Finite i) (Finite j) = i==j || i<=0 && j<=0
	(==) Infinite   Infinite   = True
	(==) _          _          = False

zeroBound:: Bound -> Bool
zeroBound (Finite i) = i<=0
zeroBound _          = False

decBound :: Bound -> Bound
decBound (Finite i)
	| i<=0       = Finite 0
	| otherwise  = Finite (i-1)
decBound bound
	= bound

incBound :: Bound -> Bound
incBound (Finite i)
	| i<=0       = Finite 1
	| otherwise  = Finite (i+1)
incBound bound
	= bound


{-	List operations:
-}

isSingleton :: [x] -> Bool
isSingleton [x] = True
isSingleton _   = False

initLast :: [x] -> ([x],x)
initLast [x]
	= ([],x)
initLast (x:xs)
	= let (init,last) = initLast xs
	  in  (x:init,last)

split :: Int -> [x] -> ([x],[x])
split _ []
	= ([],[])
split n xs
	| n<=0
		= ([],xs)
	| otherwise
		= let	(x:xs1) = xs
			(ys,zs)  = split (n-1) xs1
		  in	(x:ys,zs)

condMap :: Cond x -> IdFun x -> [x] -> (Bool,[x])
condMap c f (x:xs)
	| c x           = (True,f x:xs1)
	| otherwise     = (b,x:xs1)
	where
		(b,xs1) = condMap c f xs
condMap _ _ _
	= (False, [])

uspan :: (UCond a) -> [a] -> ([a],[a])	-- Same as span (StdList), but preserving uniqueness
uspan c (x:xs)
	| keep
		= let	(ys,zs) = uspan c xs
	  	  in	(x1:ys,zs)
	| otherwise
		= ([],x1:xs)
	where
		(keep,x1) = c x
uspan _ _
	= ([],[])

filterMap :: (x -> (Bool,y)) -> [x] -> [y]
filterMap f (x:xs)
	| keep           = y:ys
	| otherwise      = ys
	where
		(keep,y) = f x
		ys       = filterMap f xs
filterMap _ _
	= []

stateMap :: (x -> s -> (y,s)) -> [x] -> s -> ([y],s)
stateMap f (x:xs) s
	= (y:ys,s2)
	where
		(y, s1) = f x s
		(ys,s2) = stateMap f xs s1
stateMap _ _ s
	= ([],s)

ucontains :: (UCond x) -> [x] -> (Bool,[x])
ucontains c (x:xs)
	| cond
		= (True,x1:xs)
	| otherwise
		= let (b,xs1) = ucontains c xs
		  in  (b,x1:xs1)
	where
		(cond,x1) = c x
ucontains _ _
	= (False,[])

cselect :: (Cond x) -> x -> [x] -> (Bool, x)
cselect c n (x:xs)
	| c x		= (True,x)
	| otherwise	= cselect c n xs
cselect _ n _
	= (False,n)

access :: (St x (Bool,y)) -> y -> [x] -> (Bool,y,[x])
access acc n (x:xs)
	| cond
	  	= (True,y,x1:xs)
	| otherwise
		= let (b,y1,xs1) = access acc n xs
		  in  (b,y1,x1:xs1)
	where
		((cond,y),x1) = acc x
access _ n _
	= (False,n,[])

accessList :: (St x y) -> [x] -> ([y],[x])
accessList acc (x:xs)
	= (y:ys, x1:xs1)
	where
		(y, x1)  = acc x
		(ys,xs1) = accessList acc xs
accessList _ _
	= ([],[])

remove :: (Cond x) -> x -> [x] -> (Bool,x,[x])
remove c n (x:xs)
	| c x
		= (True,x,xs)
	| otherwise
		= let	(b,y,xs1) = remove c n xs
		  in	(b,y,x:xs1)
remove _ n _
	= (False,n,[])

uremove :: (UCond x) -> x -> [x] -> (Bool,x,[x])
uremove c n (x:xs)
	| cond
		= (True,x1,xs)
	| otherwise
		= let (b,y,xs1) = uremove c n xs
		  in  (b,y,x1:xs1)
	where
		(cond,x1) = c x
uremove _ n _
	= (False,n,[])

creplace :: (Cond x) -> x -> [x] -> (Bool,[x])
creplace c y (x:xs)
	| c x
		= (True,y:xs)
	| otherwise
		= let	(b,xs1) = creplace c y xs
		  in	(b,x:xs1)
creplace _ _ _
	= (False,[])

ucreplace :: (UCond x) -> x -> [x] -> (Bool,[x])
ucreplace c y (x:xs)
	| cond
		= (True,y:xs)
	| otherwise
		= let (b,xs1) = ucreplace c y xs
		  in  (b,x1:xs1)
	where
		(cond,x1) = c x
ucreplace _ _ _
	= (False,[])

replaceOrAppend :: (Cond x) -> x -> [x] -> [x]
replaceOrAppend c y (x:xs)
	| c x
		= y:xs
	| otherwise
		= x:replaceOrAppend c y xs
replaceOrAppend _ y _
	= [y]

ureplaceOrAppend :: (UCond x) -> x -> [x] -> [x]
ureplaceOrAppend c y (x:xs)
	| cond
		= y:xs
	| otherwise
		= x1:ureplaceOrAppend c y xs
	where
		(cond,x1) = c x
ureplaceOrAppend _ y _
	= [y]

removeCheck :: (Eq x) => x -> [x] -> (Bool, [x])
removeCheck y (x:xs)
	| y==x
		= (True,xs)
	| otherwise
		= let	(b,xs1) = removeCheck y xs
		  in	(b,x:xs1)
removeCheck _ _
	= (False,[])

removeSpecialChars :: [Char] -> String -> String
removeSpecialChars sc (c1:cs1@(c2:cs2))
	| c1 `elem` sc = c2:removeSpecialChars sc cs2
	| otherwise    = c1:removeSpecialChars sc cs1
removeSpecialChars sc [c]
	| c `elem` sc  = []
	| otherwise    = [c]
removeSpecialChars _ _
	= []

disjointLists :: (Eq x) => [x] -> [x] -> Bool
disjointLists xs ys
	| null xs || null ys = True
	| shorter xs ys      = disjointLists' xs ys
	| otherwise          = disjointLists' ys xs
	where
		shorter :: [x] -> [x] -> Bool
		shorter []     _      = True
		shorter (_:xs) (_:ys) = shorter xs ys
		shorter _      _      = False
		
		disjointLists' :: (Eq x) => [x] -> [x] -> Bool
		disjointLists' (x:xs) ys = not (x `elem` ys) && disjointLists' xs ys
		disjointLists' _      _  = True

noDuplicates :: (Eq x) => [x] -> Bool
noDuplicates (x:xs) = not (x `elem` xs) && noDuplicates xs
noDuplicates _      = True

unzip4 :: [(a,b,c,d)]	-> ([a],[b],[c],[d])
unzip4 xs = foldr (\(a,b,c,d) ~(as,bs,cs,ds) -> (a:as,b:bs,c:cs,d:ds)) ([],[],[],[]) xs


--	A number of useful monadic combinators:

stateMapM :: (Monad m) => (x -> s -> m (y,s)) -> [x] -> s -> m ([y],s)	-- the monadic version of stateMap
stateMapM f xs s
	= ssequence (map f xs) s

foldrM :: (Monad m) => (x -> s -> m s) -> s -> [x] -> m s		-- the monadic version of stateMap2
foldrM _ s []     = return s
foldrM f s (x:xs) = f x s >>= (\s -> foldrM f s xs)


sequenceMap :: (Monad m) => (x -> m a) -> [x] -> m [a]			-- frequently occurring combination
sequenceMap f xs = sequence (map f xs)

ssequence :: (Monad m) => [s -> m (a,s)] -> s -> m ([a],s)		-- state version of sequence
ssequence (m:ms) s
	= do {
		(a, s1) <- m s;
		(as,s2) <- ssequence ms s1;
		return (a:as,s2)
	  }
ssequence [] s
	= return ([],s)
	
	
--	gather collects all first elements that belong to the same second element.

gather :: Eq b => [(a,b)] -> [([a],b)]
gather ((d,x):xs) = ((d:ds),x) : gather xs'
	where
		(ds,xs') = gatherElements x xs

		gatherElements :: Eq b => b -> [(a,b)] -> ([a],[(a,b)])
		gatherElements x ((d,x'):xs)
			| x==x'	    = (d:ds,xs')
			| otherwise = (ds,(d,x'):xs')
			where
				(ds,xs')	= gatherElements x xs
		gatherElements _ [] = ([],[])
gather [] = []


fst3snd3 :: (a,b,c) -> (a,b)
fst3snd3 (t1,t2,_ ) = (t1,t2)

fst3thd3 :: (a,b,c) -> (a,c)
fst3thd3 (t1,_ ,t3) = (t1,t3)

snd3thd3 :: (a,b,c) -> (b,c)
snd3thd3 (_ ,t2,t3) = (t2,t3)

eqfst2id :: Eq a => a -> (a,b) -> Bool
eqfst2id id1 (id2,_)	= id1==id2

eqfst3id :: Eq a => a -> (a,b,c) -> Bool
eqfst3id id1 (id2,_,_)	= id1==id2
