module BounceTypes where


import	Graphics.UI.ObjectIO

data Barrel
   = Barrel
   	{ bBase		:: Point2		-- the base point of the barrel
	, bWalls	:: [Wall]		-- the walls of the barrel
	, bDomain	:: BarrelDomain		-- the enclosing rectangular area of the barrel
	}
type Wall =
   	( Vector2				-- the displacement of the wall (a la Polygon)
	, Interior				-- the sign at what side the wall faces the interior
	)
type SingleWall =
	( Line					-- the exact pixel position of the wall
	, Interior
	)
type Line		= (Point2,Point2)
type BarrelDomain	= Rectangle
type Interior		= Int
type Scale		= (Double,Double)	-- (horizontal scale, vertical scale)
type Radius		= Int

data Ball
   = Ball
   	{ bCenter	:: Point2		-- the center of the ball
	, bRadius	:: Radius		-- the radius of the ball
	, bSpeed	:: Vector2		-- the direction and speed of the ball
	, bColour	:: Colour		-- the colour of the ball
	}

leftBarrelSetUp :: (Barrel,[Ball])
leftBarrelSetUp =
	( Barrel
		{ bBase	 = Point2{x=600,y=100}
		, bWalls = [ (Vector2 {vx = -400,vy = 0  },   1)
			   , (Vector2 {vx = 0,   vy = -99},  -1)
			   , (Vector2 {vx = -199,vy = 0  },   1)
			   , (Vector2 {vx = 0,   vy = 299},   1)
			   , (Vector2 {vx = 199, vy = 0  },  -1)
			   , (Vector2 {vx = 0,   vy = -100}, -1)
			   , (Vector2 {vx = 400, vy = 0  },  -1)
			   ]
		,	bDomain	= Rectangle{corner1=Point2 {x=(-10),y=(-10)},corner2=Point2 {x=600,y=310}}
		}
			,	[	Ball {bCenter=Point2 {x=30,y=150},bRadius=15,bSpeed=Vector2 {vx=10,vy=3},   bColour=red   }
				,	Ball {bCenter=Point2 {x=60,y=150},bRadius=10,bSpeed=Vector2 {vx=5, vy=(-9)},bColour=yellow}
				]
		)

rightBarrelSetUp :: (Barrel,[Ball])
rightBarrelSetUp
	=	( (Barrel 
			{ bBase	= Point2{x=600,y=100}
			, bWalls	= [	(Vector2 {vx = 400, vy = 0   }, 1)
					  ,	(Vector2 {vx = 0,   vy = -99 }, 1)
					  ,	(Vector2 {vx = 200, vy = 0   }, 1)
					  ,	(Vector2 {vx = 0,   vy = 299 },-1)
					  ,	(Vector2 {vx = -200,vy = 0   },-1)
					  ,	(Vector2 {vx = 0,   vy = -100}, 1)
					  ,	(Vector2 {vx = -400,vy = 0   },-1)
					  ]
			, bDomain	= Rectangle {corner1=Point2 {x=600,y=(-10)},corner2=Point2 {x=1210,y=310}}
			})
		,	[ Ball {bCenter=Point2{x=750,y=150},bRadius=8,bSpeed=Vector2 {vx=6,vy=(-9)},bColour=magenta}
			, Ball {bCenter=Point2{x=800,y=140},bRadius=9,bSpeed=Vector2 {vx=(-2),vy=3},bColour=blue   }
			]
		)

barrelToPolygon :: Scale -> Barrel -> (Point2,Polygon)
barrelToPolygon scale (Barrel {bBase=base,bWalls=walls,bDomain=domain}) =
	(scalebase scale base domain,Polygon{polygon_shape=map (scalewall scale) walls})
	where
		scalebase :: Scale -> Point2 -> BarrelDomain -> Point2
		scalebase (kx,ky) base (Rectangle {corner1=corner1}) =
			Point2 {x=round (kx*(fromIntegral (x offset))),y=round (ky*(fromIntegral (y offset)))}
			where
				offset	= base-corner1

		scalewall :: Scale -> Wall -> Vector2
		scalewall (kx,ky) (Vector2 {vx=vx,vy=vy},_) =
			Vector2 {vx=round (kx*(fromIntegral vx)),vy=round (ky*(fromIntegral vy))}

splitWallsInBarrel :: Barrel -> ([SingleWall],[SingleWall])
splitWallsInBarrel (Barrel{bBase=base,bWalls=walls}) = (horizontal,vertical)
	where
		(_,horizontal,vertical) = foldl splitwall (base,[],[]) walls

		splitwall :: (Point2,[SingleWall],[SingleWall]) -> Wall -> (Point2,[SingleWall],[SingleWall])
		splitwall (base,horizontal,vertical) wall@(v,interior)
			| vx v ==0	= (base1,horizontal,wall1:vertical)
			| otherwise	= (base1,wall1:horizontal,vertical)
			where
				base1		= movePoint v base
				wall1		= (orientLine (base,base1),interior)

				orientLine :: Line -> Line
				orientLine (a@(Point2 {x=aX,y=aY}),b@(Point2 {x=bX,y=bY}))
					| aX==bX 	= (a{y=min aY bY},b{y=max aY bY})
					| otherwise	= (a{x=min aX bX},b{x=max aX bX})


--	Common functions:

between :: Ord a => a -> a -> a -> Bool
between x low high = x>=low && x<=high

dist :: Point2 -> Point2 -> Double
dist (Point2{x=x1,y=y1}) (Point2 {x=x2,y=y2}) =
	sqrt (dX*dX+dY*dY)
	where
		dX	= fromIntegral (x2-x1)
		dY	= fromIntegral (y2-y1)

scaleSize :: Size -> Size -> Scale
scaleSize size1 size2 =
	((fromIntegral (w size1))/(fromIntegral (w size2)),(fromIntegral (h size1))/(fromIntegral (h size2)))
