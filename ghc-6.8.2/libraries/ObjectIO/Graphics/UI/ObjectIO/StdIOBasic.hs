-----------------------------------------------------------------------------
-- |
-- Module      :  StdIOBasic
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdIOBasic defines basic types and access functions for the I\/O library.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdIOBasic (module Graphics.UI.ObjectIO.StdIOBasic) where



class Zero a where
	zero   :: a
class One a where
	one    :: a
class Toggle a where		-- This used to be ~ in Clean, but in Haskell ~ is reserved.
	toggle :: a -> a


instance Toggle Bool where
	toggle x = not x

{-	General type constructors for composing context-independent data structures.
-}
infixr 9 :^:
data	Tup	t1 t2			= t1 :^: t2


{-	General type constructors for composing context-dependent data structures.
-}
infixr 9 :~:
data	TupSt	t1 t2		ps	= (t1 ps) :~: (t2 ps)
data	ListCS	t		ps	= ListCS [t ps]
data	NilCS			ps	= NilCS


{-	General type constructors for composing local and context-dependent 
	data structures.
-}
infixr 9 :+:
data	TupLS	t1 t2	ls	ps	= (t1 ls ps) :+: (t2 ls ps)
data	ListLS	t	ls	ps	= ListLS [t ls ps]
data	NilLS		ls	ps	= NilLS
data	NewLS	t	ls	ps	= forall new . NewLS new (t  new     ps)
data	AddLS	t	ls	ps	= forall add . AddLS add (t (add,ls) ps)


type	Index = Int
type	Title = String


data	Vector2 = Vector2 {vx :: !Int, vy :: !Int}
	deriving (Eq,Show)

instance Zero Vector2 where
	zero          = Vector2 {vx=0,vy=0}
instance Num Vector2 where
	(+)    v1 v2  = Vector2 {vx=vx v1+vx v2,vy=vy v1+vy v2}
	(-)    v1 v2  = Vector2 {vx=vx v1-vx v2,vy=vy v1-vy v2}
	negate v      = Vector2 {vx=0-vx v,vy=0-vy v}

{-	(*), abs, signum, fromInteger are undefined!!
	Application generates runtime error.
-}
	(*) _ _       = error "Undefined application of Vector2 instance of (*) Num"
	abs _         = error "Undefined application of Vector2 instance of abs Num"
	signum _      = error "Undefined application of Vector2 instance of signum Num"
	fromInteger   = error "Undefined application of Vector2 instance of fromInteger Num"


class ToVector x where
	toVector :: x -> Vector2

data	Size = Size {w :: !Int, h :: !Int}
	deriving (Eq,Show)

instance Zero Size where		-- {w=0,h=0}
	zero = Size {w=0,h=0}
instance ToVector Size where		-- {w,h}->{vx=w,vy=h}
	toVector s = Vector2 {vx=w s,vy=h s}


data	Point2    = Point2 {x :: !Int, y :: !Int}
	deriving (Eq,Show)
data	Rectangle = Rectangle {corner1 :: !Point2, corner2 :: !Point2}
	deriving (Eq,Show)

instance Zero Point2 where
	zero          = Point2 {x=0,y=0}
instance Num Point2 where
	(+)    p1 p2  = Point2 {x=x p1+x p2,y=y p1+y p2}
	(-)    p1 p2  = Point2 {x=x p1-x p2,y=y p1-y p2}
	negate p      = Point2 {x=0-x p,y=0-y p}

{-	(*), abs, signum, fromInteger are undefined!!
	Application generates runtime error.
-}
	(*) _ _       = error "Undefined application of Point2 instance of (*) Num"
	abs _         = error "Undefined application of Point2 instance of abs Num"
	signum _      = error "Undefined application of Point2 instance of signum Num"
	fromInteger _ = error "Undefined application of Point2 instance of fromInteger Num"
instance ToVector Point2 where
	toVector p    = Vector2 {vx=x p,vy=y p}

instance Zero Rectangle where
	zero = Rectangle {corner1=zero, corner2=zero}

rectangleSize :: Rectangle -> Size		-- {w=abs (@1.corner1-@1.corner2).x,
						--  h=abs (@1.corner1-@1.corner2).y}
rectangleSize r
	= let	p1 = corner1 r
		p2 = corner2 r
	  in	Size {w=abs (x p2-x p1),h=abs (y p2-y p1)}

movePoint :: Vector2 -> Point2 -> Point2	-- {vx,vy} {x,y} -> {vx+x,vy+y}
movePoint v p = Point2 {x=vx v+x p,y=vy v+y p}


type	IdFun st = st -> st


-- | IOMonad class is a simply way to call IO actions from IO-like monads.
-- There are instances for 'IO', 'Draw' and 'GUI'.
class Monad m => IOMonad m where
	liftIO :: IO a -> m a
	
instance IOMonad IO where
	liftIO f = f
