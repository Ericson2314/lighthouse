-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  KeyFocus
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Layout contains the basic layout calculation functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Layout
		( LayoutItem(..), Root(..), Relative
              	, getLayoutItem, layoutItems
              	) where


import Prelude hiding (Either(..))	-- Added to hide Left and Right
import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.Window.Handle(LayoutInfo(..), Origin)


layoutFatalError :: String -> String -> x
layoutFatalError rule message
	= dumpFatalError rule "Layout" message


--	The data types used for calculating the layout:
data	LayoutItem
	= LayoutItem
		{ liId        :: !Id		-- The Id      of the item
		, liItemPos   :: ItemPos	-- The ItemPos of the item
		, liItemSize  :: !Size		-- The Size    of the item
		}
data	Root
	= Root
		{ rootItem    :: !LayoutItem	-- The original item that has been laid out
		, rootPos     :: !Vector2	-- The exact location of the item relative to current origin
		, rootTree    :: ![Relative]	-- The dependent items
		}
data	Relative
	= Relative
		{ relativeItem :: !LayoutItem	-- The original item that has been laid out
		, relativePos  :: !Vector2	-- The exact location of the item relative to current origin
		}

--	Basic operations on LayoutItems, Roots, and Relatives:
identifyLayoutItem :: Id -> LayoutItem -> Bool
identifyLayoutItem id item
	= id==liId item

identifyRoot :: Id -> Root -> Bool
identifyRoot id root
	= identifyLayoutItem id (rootItem root)

identifyRelative :: Id -> Relative -> Bool
identifyRelative id relative
	= identifyLayoutItem id (relativeItem relative)

{-	removeRoot id removes that Root from the [Root] that either:
		* can be identified by id, or
		* contains a Relative that can be identified by id
-}
removeRoot :: Id -> [Root] -> (Bool,Root,[Root])
removeRoot id (item:items)
	| identifyRoot id item
		= (True,item,items)
	| any (identifyRelative id) (rootTree item)
		= (True,item,items)
	| otherwise
		= let (found,root,items1) = removeRoot id items
		  in  (found,root,item:items1)
removeRoot _ items
	= (False,undefined,items)


{-	getLayoutItemPosSize id retrieves the position and size of:
		* the Root argument in case id identifies the root, or
		* the Relative that can be identified by id
-}
getLayoutItemPosSize :: Id -> Root -> (Bool,Vector2,Size)
getLayoutItemPosSize id item
	| identifyRoot id item
		= (True,rootPos item,liItemSize $ rootItem $ item)
	| not found
		= (False,zero,zero)
	| otherwise
		= (True,relativePos relative,liItemSize $ relativeItem $ relative)
	where
		(found,relative) = cselect (identifyRelative id) undefined (rootTree item)


{-	shiftRelative v shifts the position of the Relative argument by v.
-}
shiftRelative :: Vector2 -> Relative -> Relative
shiftRelative v item
	= item {relativePos=relativePos item+v}

{-	shiftRoot v shifts the position of the Root argument and its Relatives by v.
-}
shiftRoot :: Vector2 -> Root -> Root
shiftRoot offset item
	= item {rootPos=rootPos item+offset,rootTree=map (shiftRelative offset) (rootTree item)}

{-	getRootBoundingBox calculates the smallest enclosing rectangle of the Root
	argument and its Relatives.
-}
getRootBoundingBox :: Root -> Rect
getRootBoundingBox (Root {rootPos=rootPos,rootItem=rootItem,rootTree=rootTree})
	= getRelativeBoundingBox rootTree (posSizeToRect (Point2 {x=vx rootPos,y=vy rootPos}) (liItemSize rootItem))
	where
		getRelativeBoundingBox :: [Relative] -> Rect -> Rect
		getRelativeBoundingBox (item:items) boundBox
			= getRelativeBoundingBox items (mergeBoundingBox boundBox (posSizeToRect (Point2 {x=vx v,y=vy v}) (liItemSize $ relativeItem $ item)))
			where
				v = relativePos item
				
				mergeBoundingBox :: Rect -> Rect -> Rect
				mergeBoundingBox (Rect {rleft=lR,rtop=tR,rright=rR,rbottom=bR}) (Rect {rleft=lB,rtop=tB,rright=rB,rbottom=bB})
					= Rect {rleft=min lR lB,rtop=min tR tB,rright=max rR rB,rbottom=max bR bB}
		getRelativeBoundingBox _ boundBox
			= boundBox

{-	getLayoutItem id roots
		retrieves the position (Vector2) and size (Size) of the item identified by id.
		In case the item is a relative, it is removed from the [Root].
		In case the item is a root, it is removed only if it has an empty layout tree.
		The LayoutInfo classifies the ItemPos of the layout root of the retrieved item.
	getLayoutItem returns a runtime error in case no item could be identified.
-}
getLayoutItem :: Id -> [Root] -> (LayoutInfo,Vector2,Size,[Root])
getLayoutItem id items@(root:roots)
	| identifyRoot id root
		= if   null depends
		  then (layoutInfo,corner,size,roots)
		  else (layoutInfo,corner,size,items)
	| inTree
		= (layoutInfo,rPos,rSize,root {rootTree=depends1}:roots)
	| otherwise
		= let (layoutInfo1,pos,size1,roots1) = getLayoutItem id roots
		  in  (layoutInfo1,pos,size1,root:roots1)
	where
		corner                       = rootPos root
		size                         = liItemSize $ rootItem $ root
		depends                      = rootTree root
		(inTree,rPos,rSize,depends1) = getRelativeItem id depends
		layoutInfo                   = case liItemPos $ rootItem $ root of
		                                    (Fix,_)           -> LayoutFix
		                               --   (_,OffsetFun i f) -> LayoutFun i f
		                                    _                 -> LayoutFrame
		
		getRelativeItem :: Id -> [Relative] -> (Bool,Vector2,Size,[Relative])
		getRelativeItem id (item:items)
			| identifyRelative id item
				= (True,relativePos item,liItemSize $ relativeItem $ item,items)
			| otherwise
				= let (found,pos,size,items1) = getRelativeItem id items
				  in  (found,pos,size,item:items1)
		getRelativeItem _ items
			= (False,zero,zero,items)
getLayoutItem id _
	= layoutFatalError "getLayoutItem" "Unknown Id"


{-	layoutItems is the actual layout algorithm.
	It calculates the precise position (in pixels) of each LayoutItem.
	The position is calculated from a zero origin.
	Assumptions:
	-	All LayoutItems have a layout element ItemPos.
	-	All relative references to previous elements have been identified (so LeftOfPrev --> LeftOf id and so on).
-}
layoutItems :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Size -> Size -> [(ViewDomain,Point2)] -> [LayoutItem] -> (Size,[Root])
layoutItems hMargins@(lMargin,rMargin) vMargins@(tMargin,bMargin) itemSpaces reqSize minSize orientations layoutItems
	= (finalSize,roots1)
	where
		reqSize1     = if reqSize/=zero then Size {w=w reqSize-lMargin-rMargin,h=h reqSize-tMargin-bMargin} else reqSize
		layoutItems1 = sortLayoutItems layoutItems
		(_,roots)    = foldr (calcRootPosition itemSpaces orientations) (0,[]) layoutItems1
		size         = calcAreaSize orientations roots reqSize1 minSize
		roots1       = foldr (calcFramePosition hMargins vMargins orientations size) [] roots
		finalSize    = Size {w=lMargin+w size+rMargin,h=tMargin+h size+bMargin}


{-	sortLayoutItems sorts the list of item positions such that relatively laynout items 
	are placed immediately(!!) behind their target items. They are not placed in the rootTree
	of the target item. This is done by calcRootPosition. 
	sortLayoutItems failures:
	-	a cyclic dependency has been located: the Ids are printed and computation stops
	-	unknown references have been located: the Ids are printed and computation stops
-}
sortLayoutItems :: [LayoutItem] -> [LayoutItem]
sortLayoutItems layoutItems
	= sortLayoutItems' [] (lineItems++relItems)
	where
		(lineItems,relItems) = divide (\item->isLine (liItemPos item)) layoutItems
		
		sortLayoutItems' :: [LayoutItem] -> [LayoutItem] -> [LayoutItem]
		sortLayoutItems' done [] = done
		sortLayoutItems' done (item1:todo)
			| not isrelative
				= sortLayoutItems' (item1:done) todo
			| otherwise
				= let (done1,chain,todo1) = getItemPosChain id2 done [item1] todo
				  in  (sortLayoutItems' (insertItemPosChain chain done1) todo1)
			where
				pos1             = liItemPos item1
				(isrelative,id2) = isRelative pos1
				
				getItemPosChain :: Id -> [LayoutItem] -> [LayoutItem] -> [LayoutItem] -> ([LayoutItem],[LayoutItem],[LayoutItem])
				getItemPosChain nextId done chain todo
					| in_chain
						= layoutFatalError "calculating layout" "cyclic dependency between Ids"
					| in_done
						= (done,chain,todo)
					| not in_todo
						= layoutFatalError "calculating layout" "reference to unknown Id"
					| not isrelative
						= (done,next:chain,todo1)
					| otherwise
						= getItemPosChain id2 done (next:chain) todo1
					where
						in_chain             = any (identifyLayoutItem nextId) chain
						in_done              = any (identifyLayoutItem nextId) done
						(in_todo,next,todo1) = remove (identifyLayoutItem nextId) undefined todo
						nextPos              = liItemPos next
						(isrelative,id2)     = isRelative nextPos
				
				insertItemPosChain :: [LayoutItem] -> [LayoutItem] -> [LayoutItem]
				insertItemPosChain chain@(final:_) done
					| not isrelative
						= chain'++done
					| otherwise
						= insertchain id chain' done
					where
						(isrelative,id) = isRelative (liItemPos final)
						chain'          = reverse chain
						
						insertchain :: Id -> [LayoutItem] -> [LayoutItem] -> [LayoutItem]
						insertchain id chain (item:items)
							| identifyLayoutItem id item
								= chain++(item:items)
							| otherwise
								= item:(insertchain id chain items)
						insertchain _ chain _
							= chain
				insertItemPosChain _ done	-- this alternative will actually never be reached
					= done


{-	Calculate the positions of line oriented items and the space they occupy. 
	Place relatively placed items in the root tree of the item referred to.
	Items that are positioned at a fixed spot (Fix pos) are laid out relative to the given origin.
	Assumptions:
	-	All relative layout positions refer to existing elements which must occur in the done list.
	
	Note:	Renter = Right or Center,
		Corner = LeftTop, RightTop, LeftBottom or RightBottom
-}
calcRootPosition :: (Int,Int) -> [(ViewDomain,Origin)] -> LayoutItem -> (Int,[Root]) -> (Int,[Root])
calcRootPosition itemSpaces orientations item1 sDone@(sizeY,done)
	| isfix
		= let	(_,origin) = head orientations
			itemoffset = itemPosOffset fixpos orientations
			pos        = itemoffset-toVector origin
			item1'     = Root {rootItem=item1,rootPos=pos,rootTree=[]}
		  in	(sizeY, item1':done)
	| isrelative && exists
		= let	(sizeY',item2') = if   isRelativeX pos1
			                  then calcXPosition itemSpaces orientations item1 id2 sizeY item2
			                  else calcYPosition itemSpaces orientations item1 id2 sizeY item2
		  in	(sizeY',item2':done1)
	| isrelative
		= layoutFatalError "calculating layout" "reference to unknown Id (not caught by sortLayoutItems)"
	| isCorner pos1
		= let	item1' = Root {rootItem=item1,rootPos=zero,rootTree=[]}
		  in	(sizeY, item1':done)
	| otherwise
		= let	height       = h $ liItemSize $ item1
			itemoffset   = itemPosOffset (snd pos1) orientations
			yOffset      = vy itemoffset
			yOffset1     = if sizeY==0 then yOffset else snd itemSpaces+yOffset
		  in	(max sizeY (sizeY+yOffset1+height), (Root {rootItem=item1,rootPos=zero {vy=sizeY+yOffset1},rootTree=[]}):done)
	where
		pos1                 = liItemPos item1
		(isfix,fixpos)       = isFix pos1
		(isrelative,id2)     = isRelative pos1
		(exists,item2,done1) = removeRoot id2 done
		
	{-	calcXPosition calculates the position of item1 which is horizontally relative to the item identified by id2.
		This item is either item2 or occurs in the layout tree of item2.
		item1 is placed as a Relative in the layout tree of item2.
	-}
		calcXPosition :: (Int,Int) -> [(ViewDomain,Origin)] -> LayoutItem -> Id -> Int -> Root -> (Int,Root)
		calcXPosition itemSpaces orientations item1 id2 sizeY item2@(Root {rootItem=root2,rootTree=tree2})
			| not ok
				= layoutFatalError "calcXPosition" "dependent item could not be found in rootTree"
			| otherwise
				= ( if isCorner pos2 || isfix2 then sizeY else max (t+h size1) sizeY
				  , item2 {rootTree=depend:tree2}
				  )
			where
				pos1               = liItemPos  item1
				size1              = liItemSize item1
				pos2               = liItemPos  root2
				(isfix2,_)         = isFix pos2
				l                  = if   isLeftOf pos1
				                     then vx corner2-w size1-fst itemSpaces+vx offset
				                     else vx corner2+w size2+fst itemSpaces+vx offset
				t                  = vy corner2+vy offset
				offset             = itemPosOffset (snd pos1) orientations
				(ok,corner2,size2) = getLayoutItemPosSize id2 item2
				depend             = Relative {relativeItem=item1,relativePos=Vector2 {vx=l,vy=t}}
		
	{-	calcYPosition calculates the position of item1 which is vertically relative to the item identified by id2.
		This item is either item2 or occurs in the layout tree of item2.
		item1 is placed as a Relative in the layout tree of item2.
	-}
		calcYPosition :: (Int,Int) -> [(ViewDomain,Origin)] -> LayoutItem -> Id -> Int -> Root -> (Int,Root)
		calcYPosition itemSpaces orientations item1 id2 sizeY item2@(Root {rootItem=root2,rootTree=tree2})
			| not ok
				= layoutFatalError "calcXPosition" "dependent item could not be found in rootTree"
			| otherwise
				= ( if isCorner pos2 || isfix2 then sizeY else max (t+h size1) sizeY
				  , item2 {rootTree=depend:tree2}
				  )
			where
				pos1               = liItemPos  item1
				size1              = liItemSize item1
				pos2               = liItemPos  root2
				(isfix2,_)         = isFix pos2
				l                  = vx corner2+vx offset
				t                  = if   isBelow pos1
				                     then vy corner2+h size2+snd itemSpaces+vy offset
				                     else vy corner2-h size1-snd itemSpaces+vy offset
				offset             = itemPosOffset (snd pos1) orientations
				(ok,corner2,size2) = getLayoutItemPosSize id2 item2
				depend             = Relative {relativeItem=item1,relativePos=Vector2 {vx=l,vy=t}}


{-	In case no requested size is given (requested size==zero), calculate the actual 
	width and height of the overall area. The overall area is the smallest enclosing 
	rectangle of the line and fix layout items, provided it fits the corner oriented items.
	In case of a requested size, yield this size.
-}
calcAreaSize :: [(ViewDomain,Origin)] -> [Root] -> Size -> Size -> Size
calcAreaSize orientations roots reqSize minimumSize
	| reqSize/=zero
		= stretchSize minimumSize reqSize
	| otherwise
		= foldr (fitRootInArea origin orientations) minimumSize roots
	where
		origin = snd (head orientations)
		
		stretchSize :: Size -> Size -> Size
		stretchSize size1 size2 = Size {w=max (w size1) (w size2), h=max (h size1) (h size2)}
		
	--	fitRootInArea stretches the Size argument such that the bounding box of the Root argument fits. 
		fitRootInArea :: Origin -> [(ViewDomain,Origin)] -> Root -> Size -> Size
		fitRootInArea origin orientations root frameSize
			= stretchSize frameSize (Size {w=reqX,h=reqY})
			where
				corner       = rootPos root
				size         = liItemSize $ rootItem $ root
				(loc,offset) = liItemPos  $ rootItem $ root
				v            = itemPosOffset offset orientations
				itemBoundBox = getRootBoundingBox root
				(reqX,reqY)  = delimit loc itemBoundBox
				
				delimit :: ItemLoc -> Rect -> (Int,Int)
				
				delimit Fix (Rect {rright=rright,rbottom=rbottom})
					| r'<=0 || b'<=0
						= (0,0)
					| otherwise
						= (r',b')
					where
						r' = rright -x origin
						b' = rbottom-y origin
				
				delimit LeftTop (Rect {rright=rright,rbottom=rbottom})
					= (rright-vx lefttop,rbottom-vy lefttop)
					where
						lefttop = corner-v
				
				delimit RightTop (Rect {rleft=rleft,rbottom=rbottom})
					= (vx righttop-rleft,rbottom-vy righttop)
					where
						righttop = corner+zero {vx=w size}-v
				
				delimit LeftBottom (Rect {rtop=rtop,rright=rright})
					= (rright-vx leftbottom,vy leftbottom-rtop)
					where
						leftbottom = corner+zero {vy=h size}-v
				
				delimit RightBottom (Rect {rleft=rleft,rtop=rtop})
					= (vx rightbottom-rleft,vy rightbottom-rtop)
					where
						rightbottom = corner+toVector size-v
				
				delimit Left (Rect {rright=rright,rbottom=rbottom})
					= (rright-left,rbottom)
					where
						left = vx corner-vx v
				
				delimit Center (Rect {rleft=rleft,rright=rright,rbottom=rbottom})
					= (rright-rleft,rbottom)
				
				delimit Right (Rect {rleft=rleft,rbottom=rbottom})
					= (right-rleft,rbottom)
					where
						right = vx corner+w size-vy v


{-	calcFramePosition calculates the layout of all frame aligned items. In addition it adds the margin offsets
	to each item. 
-}
calcFramePosition :: (Int,Int) -> (Int,Int) -> [(ViewDomain,Origin)] -> Size -> Root -> [Root] -> [Root]
calcFramePosition hMargins@(lMargin,_) vMargins@(tMargin,_) orientations sizeArea@(Size {w=width,h=height}) item done
	| isRenter pos || isCorner pos
		= let	sizeItem  = liItemSize $ rootItem $ item
			widthLeft = width-w sizeItem
			v         = if   isCorner pos
			            then cornerShift  orientations pos sizeItem sizeArea
			            else Vector2 {vx=lineShift orientations pos widthLeft,vy=0}
			shift     = Vector2 {vx=vx v+lMargin,vy=vy v+tMargin}
			item'     = shiftRoot shift item
		  in	item':done
	| otherwise
		= (shiftRoot (Vector2 {vx=lMargin,vy=tMargin}) item):done
	where
		pos = liItemPos $ rootItem $ item
		
		lineShift :: [(ViewDomain,Origin)] -> ItemPos -> Int -> Int
		lineShift orientations (Center,offset) space
			= round ((fromIntegral space)/2.0) + vx (itemPosOffset offset orientations)
		lineShift orientations (_,offset) space
			= space + vx (itemPosOffset offset orientations)
		
		cornerShift :: [(ViewDomain,Origin)] -> ItemPos -> Size -> Size -> Vector2
		cornerShift orientations (LeftTop,offset) _ _
			= itemPosOffset offset orientations
		cornerShift orientations (RightTop,offset) (Size {w=wItem}) (Size {w=w})
			= v {vx=w-wItem+vx v}
			where
				v = itemPosOffset offset orientations
		cornerShift orientations (LeftBottom,offset) (Size {h=hItem}) (Size {h=h})
			= v {vy=h-hItem+vy v}
			where
				v = itemPosOffset offset orientations
		cornerShift orientations (RightBottom,offset) (Size {w=wItem,h=hItem}) (Size {w=w,h=h})
			= Vector2 {vx=w-wItem+vx v,vy=h-hItem+vy v}
			where
				v = itemPosOffset offset orientations


--	itemPosOffset calculates the actual offset vector of the given ItemOffset value.

itemPosOffset :: ItemOffset -> [(ViewDomain,Origin)] -> Vector2
{-
itemPosOffset NoOffset _
	= zero
-}
itemPosOffset {-(OffsetVector v)-}v _
	= v
{-
itemPosOffset (OffsetFun i f) orientations
	| isBetween i 1 (length orientations)
		= f (orientations!!(i-1))
	| otherwise
		= layoutFatalError "calculating OffsetFun" ("illegal ParentIndex value: "++show i)
-}


--	ItemPos predicates.

isFix :: ItemPos -> (Bool,ItemOffset)
isFix (Fix,offset) = (True, offset)
isFix _            = (False,zero) --NoOffset

isLine :: ItemPos -> Bool
isLine (Left,  _) = True
isLine (Center,_) = True
isLine (Right, _) = True
isLine _          = False

isRelative :: ItemPos -> (Bool,Id)
isRelative (LeftOf  id,_) = (True,id)
isRelative (RightTo id,_) = (True,id)
isRelative (Above   id,_) = (True,id)
isRelative (Below   id,_) = (True,id)
isRelative _              = (False,undefined)

isRelativeX :: ItemPos -> Bool
isRelativeX (LeftOf  _,_) = True
isRelativeX (RightTo _,_) = True
isRelativeX _             = False

isRenter :: ItemPos -> Bool
isRenter (Center,_) = True
isRenter (Right, _) = True
isRenter _          = False

isCorner :: ItemPos -> Bool
isCorner (LeftTop,     _) = True
isCorner (RightTop,    _) = True
isCorner (LeftBottom,  _) = True
isCorner (RightBottom, _) = True
isCorner _                = False

isLeftOf :: ItemPos -> Bool
isLeftOf (LeftOf _,_) = True
isLeftOf _            = False

isBelow  :: ItemPos -> Bool
isBelow  (Below _,_) = True
isBelow  _           = False


--	Auxiliary functions:

divide :: Cond x -> [x] -> ([x],[x])	-- divide cond xs = (filter cond xs,filter (not o cond) xs)
divide f (x:xs)
	| f x       = (x:yes,no)
	| otherwise = (yes,x:no)
	where
		(yes,no) = divide f xs
divide _ _
	= ([],[])
