module GraphViz ( GraphAccessors(..), GraphViz(..) ) where

import Graphics.UI.ObjectIO
import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.StdWindowAttribute
import Control.Monad(when)

data GraphAccessors v
   = GraphAccessors
	{ getNodeName	:: v -> String
       	, isParent 	:: v -> v -> Bool
       	, hasParents 	:: v -> Bool
       	}
       	
data Node v
   = Node
       	{ value    :: v
       	, nodePos  :: !Point2
       	, nodeSize :: !Size
       	} deriving Show
       
type Graph v = [Node v]

dx = 15
dy = 15
br = 3

data GraphViz v ls ps = GraphViz String (GraphAccessors v) [v] [WindowAttribute ls ps]

instance Windows (GraphViz v) where	
	openWindow ls (GraphViz title accessors graph attrs) ps = do
		windowID <- if hasId then return (getWindowIdAtt idAttr) else openId
		(size,graph) <- liftIO (doScreenDraw (layoutGraph accessors graph))
		openWindow (graph,Nothing,ls) (window windowID graph size) ps
		where
			window windowID graph size = 
				Window title NilLS
					[ WindowId    windowID
					, WindowClose closeFn
					, WindowLook  True (look True graph)
					, WindowPen   [PenBack grey]
					, WindowMouse onlyDrag Able (track windowID)
					, WindowViewDomain (sizeToRectangle size)
					, WindowViewSize size
					, WindowHScroll hscroll
					, WindowVScroll	vscroll
					]

			(hasId,idAttr,attrs1) = remove isWindowId    undefined attrs
			(_,closeAttr, attrs2) = remove isWindowClose (WindowClose (noLS closeActiveWindow)) attrs1
			closeFn ((graph,grabState,ls),ps) = do
				(ls,ps) <- getWindowCloseFun closeAttr (ls,ps)
				return ((graph,grabState,ls),ps)

			-- scrolling
			hscroll viewframe (SliderState{sliderThumb=sliderThumb}) move
				= case move of
					SliderIncSmall -> sliderThumb+5
					SliderDecSmall -> sliderThumb-5
					SliderIncLarge -> sliderThumb+(w (rectangleSize viewframe))*9 `div` 10
					SliderDecLarge -> sliderThumb-(w (rectangleSize viewframe))*9 `div` 10
					SliderThumb x  -> x
			vscroll viewframe (SliderState{sliderThumb=sliderThumb}) move
				= case move of
					SliderIncSmall -> sliderThumb+5
					SliderDecSmall -> sliderThumb-5
					SliderIncLarge -> sliderThumb+(h (rectangleSize viewframe))*9 `div` 10
					SliderDecLarge -> sliderThumb-(h (rectangleSize viewframe))*9 `div` 10
					SliderThumb y  -> y

			-- Drawing
			look drawNet graph _ (UpdateState {newFrame=newFrame}) = do
				unfill newFrame
				when drawNet (drawNetwork accessors graph)
				mapM_ (drawNode accessors) graph

			-- Mouse handling			
			onlyDrag :: MouseState -> Bool
			onlyDrag (MouseDown _ _ _) = True
			onlyDrag (MouseDrag _ _)   = True
			onlyDrag (MouseUp   _ _)   = True
			onlyDrag _		   = False

			startNodeTracking graph pos
				| found     = Just (n:graph1,Vector2 (x2-x1) (y2-y1))
				| otherwise = Nothing
				where
					(found,n,graph1) = remove (pointInRectangle pos . nodeBoundRectangle) undefined graph
					Point2 x1 y1 = nodePos n
					Point2 x2 y2 = pos				

			dragNode (n:ns) (Just v) pos = n{nodePos=subVector v pos}:ns
			dragNode graph  _        pos = graph

			track windowID (MouseDown pos mods _) ((graph,grabState,ls),ps) =
				case startNodeTracking graph pos of
					Just (graph,v) -> do
						setWindowLook windowID True (True,look False graph)
						return ((graph,Just v,ls), ps)
					Nothing -> return ((graph,grabState,ls),ps)
			track windowID (MouseDrag pos mods) ((graph,grabState,ls),ps) = do
				let graph1 = dragNode graph grabState pos
				drawInWindow windowID (undrawNode (head graph) >> mapM_ (drawNode accessors) graph1)
				setWindowLook windowID False (True,look False graph1)
				return ((graph1,grabState,ls),ps)
			track windowID (MouseUp	  pos mods) ((graph,grabState,ls),ps) = do
				let graph1 = dragNode graph grabState pos
				setWindowViewDomain windowID (sizeToRectangle (calcGraphSize zero graph1))
				drawInWindow windowID (undrawNode (head graph) >> mapM_ (drawNode accessors) graph1)
				setWindowLook windowID True  (True,look True  graph1)
				return ((graph1,Nothing,ls),ps)
				
layoutGraph :: GraphAccessors v -> [v] -> Draw (Size,Graph v)
layoutGraph a values = do
	metrics <- getPenFontMetrics
	let height = fontLineHeight metrics+2*br
	let graph  = map mkNode values
	(width,pos,rs,ts) <- layoutNodes (not . hasParents a) height 0 (Point2 dx dy) [] graph
	(width,rs) <- loop height width pos rs ts
	return (Size (width+dx) (y pos+dy),rs)
	where
		mkNode value = Node {value=value, nodePos=zero, nodeSize=zero}
		
		loop vh width pos rs []     = return (width,rs)
		loop vh width pos rs (t:ts) = do
			(width,pos,rs,ts) <- layoutNode vh width t pos rs ts
			loop vh width pos{y=(y pos)+dy} rs ts

		layoutNode vh width t pos@(Point2 x y) rs ts = do
			w <- getPenFontStringWidth (getNodeName a (value t))
			(width,pos2@(Point2 x' y'),rs,ts) <- layoutNodes (isParent a (value t)) vh width (pos{x=x+w+2*br+dx}) rs ts
			let y2 = max ((y'+y-vh) `div` 2) y
			let width' = max width (x+w+2*br)
			return (width',pos{y=(max (y2+vh) y')}, (t{nodePos=Point2{x=x,y=y2}, nodeSize=(Size (w+2*br) vh)}:rs),ts)

		layoutNodes f vh width pos rs [] = do
			return (width,pos{y=(y pos)-dy},rs,[])
		layoutNodes f vh width pos rs (t:ts)
			| f (value t) = do
				(width,pos,rs,ts) <- layoutNode vh width t pos rs ts
				layoutNodes f vh width pos{y=(y pos)+dy} rs ts
			| otherwise = do
				(width,pos,rs,ts) <- layoutNodes f vh width pos rs ts
				return (width,pos,rs,t:ts)


nodeBoundRectangle n = posSizeToRectangle (nodePos n) (nodeSize n)

drawNode a n = do
	c <- getPenColour
	let rect = nodeBoundRectangle n
	setPenColour white
	fill rect
	setPenColour black
	draw rect
	metrics <- getPenFontMetrics
	let nx = x (nodePos n) + br
	let ny = y (nodePos n) + br + (fAscent metrics) + (fLeading metrics)
	drawAt (Point2 nx ny) (getNodeName a (value n))
	setPenColour c

undrawNode n = unfill (nodeBoundRectangle n)
	


centerPos (Node {nodePos=(Point2 x y), nodeSize=(Size w h)}) = Point2 (x+w `div` 2) (y+h `div` 2)

drawNetwork a graph = loop graph
	where
		loop [] = return ()
		loop (n:ns) = do
			drawArcs n graph
			loop ns
			
		drawArcs _ [] = return ()
		drawArcs n1 (n2:ns)
			| isParent a (value n1) (value n2) = do
				drawLine (centerPos n1) (centerPos n2)
				drawArcs n1 ns
			| otherwise = drawArcs n1 ns
			
undrawNetwork a graph = loop graph
	where
		loop [] = return ()
		loop (n:ns) = do
			undrawArcs n graph
			loop ns
	
		undrawArcs _ [] = return ()
		undrawArcs n1 (n2:ns)
			| isParent a (value n1) (value n2) = do
				undrawLine (centerPos n1) (centerPos n2)
				undrawArcs n1 ns
			| otherwise = undrawArcs n1 ns
			

calcGraphSize size [] = size
calcGraphSize (Size w h) (n@(Node {nodePos=p, nodeSize=(Size w' h')}):ns) = 
	calcGraphSize (Size (max (x p+w'+dx) w) (max (y p+h'+dy) h)) ns
