module BounceDraw where

import Graphics.UI.ObjectIO
import BounceTypes


drawBarrel :: UpdateArea -> Scale -> Barrel -> Draw ()
drawBarrel updArea scale barrel = do
	setPenColour grey
	mapM_ fill updArea
	unfillAt base polygon	
	where
		(base,polygon)	= barrelToPolygon scale barrel

drawBall :: Scale -> Point2 -> Ball -> Draw ()
drawBall (kx,ky) base (Ball{bCenter=center,bRadius=radius,bColour=colour}) = do
	setPenColour	colour
	fillAt		center1 oval
	setPenColour	black
	drawAt 		center1 oval
	where
		k	= min kx ky
		r	= (max (round (k*(fromIntegral radius))) 2)-1
		offset	= center-base
		center1	= Point2{x=round (kx*(fromIntegral (x offset))),y=round (ky*(fromIntegral (y offset)))}
		oval	= Oval{oval_rx=r,oval_ry=r}

eraseBall :: Scale -> Point2 -> Ball -> Draw ()
eraseBall (kx,ky) base (Ball {bCenter=center,bRadius=radius}) = do
	unfillAt center1 oval
	undrawAt center1 oval
	where
		r	= (max (round (k*(fromIntegral radius))) 2)-1
		k	= min kx ky
		offset	= center-base
		center1	= Point2 {x=round (kx*(fromIntegral (x offset))),y=round (ky*(fromIntegral (y offset)))}
		oval	= Oval{oval_rx=r,oval_ry=r}
