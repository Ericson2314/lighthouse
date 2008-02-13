-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Window.Validate
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Window.Validate contains window validation functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Window.Validate
		( validateWindowId
		, validateWindow
		, exactWindowSize
		, exactWindowPos
		, validateViewDomain
		) where




import Prelude hiding (Either(..))	-- Either = Left | Right must be hidden
import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Control.Layout
import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.StdId
import Graphics.UI.ObjectIO.StdWindowAttribute
import Graphics.UI.ObjectIO.Window.Access
import Graphics.UI.ObjectIO.Window.Handle
import Graphics.UI.ObjectIO.KeyFocus(newFocusItems)
import Graphics.UI.ObjectIO.StdSystem(maxScrollWindowSize)
import Graphics.UI.ObjectIO.StdPicture(unfill, Draw(..))
import Graphics.UI.ObjectIO.OS.System
import Graphics.UI.ObjectIO.OS.Window
import Graphics.UI.ObjectIO.OS.Picture(PenAttribute(..), setPenAttribute, defaultPen)
import Foreign.Ptr(nullPtr)
import qualified Data.Map as Map
import Data.List(find)


windowValidateFatalError :: String -> String -> x
windowValidateFatalError function message
	= dumpFatalError function "windowvalidate" message


{-	validateWindowId checks whether the Id of the window/dialogue has already been bound.
	If so, Nothing is returned; otherwise a proper Id value for the window/dialogue is returned.
	The Id is not bound.
-}
validateWindowId :: Maybe Id -> GUI ps (Maybe Id)
validateWindowId Nothing
	= do {
		wId <- openId;
		return (Just wId)
	  }
validateWindowId (Just id)
	= do {
		idtable <- ioStGetIdTable;
		if   Map.member id idtable
		then return Nothing
		else return (Just id)
	  }


{-	Validate the given window.
-}
validateWindow :: OSWindowMetrics -> OSDInfo -> WindowHandle ls ps -> WindowHandles ps
               -> IO (Index,Point2,Size,Vector2,WindowHandle ls ps)

validateWindow wMetrics _ wH@(WindowHandle {whMode=mode,whKind=IsDialog,whItemNrs=whItemNrs,whItems=whItemsX,whAtts=atts}) windows
	= do {
		(derSize,items) <- layoutControls wMetrics hMargins vMargins spaces reqSize minSize [(domain,zero)] whItemsX;
		let (itemNrs,   items1) = genWElementItemNrs whItemNrs items
		    focusItems 		= getWElementKeyFocusIds True items1
		    derSize1            = determineRequestedSize derSize sizeAtt
		    domain1             = sizeToRectangle derSize1
		    okSize              = exactWindowSize wMetrics domain1 derSize1 False False IsDialog
		in do {			
			okPos <- exactWindowPos wMetrics okSize pos IsDialog mode windows;
			return ( index
			       , okPos
			       , okSize
			       , zero
			       , wH { whItemNrs   = itemNrs
			            , whKeyFocus  = newFocusItems focusItems
			            , whItems     = items1
			            , whSelect    = True
			            , whAtts      = atts4
			            , whDefaultId = defid
			            , whCancelId  = canid
			            , whSize      = okSize
			            }			       
			       )
		   }
	  }
	where
		atts1                 = filter isValidDialogAttribute         atts
		(index,atts2)	      = validateWindowIndex mode              atts1 windows
		(pos,  atts3) 	      = validateWindowPos   mode              atts2 windows
		sizeAtt               = attrSize                              atts3	-- Retrieve Window(View/Outer)Size (identical for Dialogs)
		(hMargins,vMargins)   = attrMargins         IsDialog wMetrics atts3
		spaces                = getWindowItemSpaces IsDialog wMetrics atts3
		defid       	      = getOkId                               atts3 whItemsX
		canid       	      = getCancelId                           atts3 whItemsX
		atts4       	      = validateWindowInitActive              atts3 whItemsX		
		reqSize               = determineRequestedSize zero sizeAtt
		(minWidth,minHeight)  = osMinWindowSize
		minSize               = Size {w=minWidth,h=minHeight}
		domain                = sizeToRectangle reqSize		

validateWindow wMetrics osdInfo wH@(WindowHandle {whKind=IsWindow,whItemNrs=whItemNrs,whItems=whItems,whAtts=atts}) windows
	= let
		atts1                  = filter isValidWindowAttribute atts
		mode                   = Modeless
		(domain,atts2)         = validateWindowDomain          atts1
		(maybe_hScroll,atts3)  = validateWindowHScroll         atts2
		(maybe_vScroll,atts4)  = validateWindowVScroll         atts3
		(sysLook,look, atts5)  = validateWindowLook            atts4
		atts6		       = validateCaretPos domain atts5
	  in
	  do {
		(reqSize,atts7)       <- validateWindowSize wMetrics domain isMDI True (isJust maybe_hScroll,isJust maybe_vScroll) atts6;
		let
			(index,atts8)            = validateWindowIndex mode  atts7 windows
			(pos,  atts9)   	 = validateWindowPos   mode  atts8 windows
			(penAtts,atts10)         = attrPen                       atts9
			(hMargins,vMargins)      = attrMargins IsWindow wMetrics atts10
			spaces                   = getWindowItemSpaces IsWindow wMetrics atts10
			isAble                   = attrSelectState                       atts10
			defid         		 = getOkId                               atts10 whItems
			canid         		 = getCancelId                           atts10 whItems
			atts11         		 = validateWindowInitActive              atts10 whItems
			pen                      = foldr setPenAttribute defaultPen (reverse penAtts)
		in
		do {
			(derSize,items)         <- layoutControls wMetrics hMargins vMargins spaces reqSize minSize [(domain,corner1 domain)] whItems;
			let
				(itemNrs,items1)    = genWElementItemNrs whItemNrs items
				focusItems 	    = getWElementKeyFocusIds True items1
				(origin,atts12)     = validateOrigin derSize domain atts11
				okSize              = exactWindowSize wMetrics domain derSize (isJust maybe_hScroll) (isJust maybe_vScroll) IsWindow
			in
			do {
				okPos <- exactWindowPos wMetrics okSize pos IsWindow mode windows;
				let
					(hScroll,vScroll) = validScrollInfos wMetrics okSize maybe_hScroll maybe_vScroll
				in return ( index
	                                  , okPos
	                                  , okSize
	                                  , toVector (origin-corner1 domain)
	                                  , wH { whItemNrs    = itemNrs
	                                       , whKeyFocus   = newFocusItems focusItems
	                                       , whWindowInfo = WindowInfo
	                                                           { windowDomain  = rectangleToRect domain
	                                                           , windowOrigin  = corner1 domain
	                                                           , windowHScroll = hScroll
	                                                           , windowVScroll = vScroll
	                                                           , windowLook    = LookInfo {lookFun=look,lookPen=pen,lookSysUpdate=sysLook}
	                                                           , windowClip    = ClipState {clipRgn=nullPtr,clipOk=False}
	                                                           }
	                                       , whItems      = items1
	                                       , whSelect     = isAble
	                                       , whAtts       = atts12
	                                       , whDefaultId  = defid
	                                       , whCancelId   = canid
	                                       , whSize       = okSize
	                                       }
	                                  )
			}
		}
	  }
	where
		minSize = fromTuple osMinWindowSize
		isMDI   = getOSDInfoDocumentInterface osdInfo == MDI
		
		validScrollInfos :: OSWindowMetrics -> Size -> Maybe ScrollFunction -> Maybe ScrollFunction
		                 -> (Maybe ScrollInfo,Maybe ScrollInfo)
		validScrollInfos wMetrics wSize maybe_hScroll maybe_vScroll
			= (fmap (scrollInfo hScrollRect) maybe_hScroll,fmap (scrollInfo vScrollRect) maybe_vScroll)
			  where
				  windowRect   = sizeToRect wSize
				  hasScrolls   = (isJust maybe_hScroll,isJust maybe_vScroll)
				  hScrollRect  = getWindowHScrollRect wMetrics hasScrolls windowRect
				  vScrollRect  = getWindowVScrollRect wMetrics hasScrolls windowRect

				  scrollInfo :: Rect -> ScrollFunction -> ScrollInfo
				  scrollInfo r@(Rect {rleft=rleft,rtop=rtop}) scrollFun
					  = ScrollInfo
					    { scrollFunction = scrollFun
					    , scrollItemPos  = Point2 {x=rleft,y=rtop}
					    , scrollItemSize = rectSize r
					    , scrollItemPtr  = osNoWindowPtr
					    }



determineRequestedSize :: Size -> Maybe Size -> Size
determineRequestedSize size Nothing  = size
determineRequestedSize _ (Just size) = size


{-	validateWindowIndex validates the WindowIndex attribute. 
	The return Index is the validated Index. 
	The return WindowAttribute list does not contain a WindowIndex attribute.
-}
validateWindowIndex :: WindowMode -> [WindowAttribute ls ps] -> WindowHandles ps -> (Index,[WindowAttribute ls ps])
validateWindowIndex mode atts windows@(WindowHandles {whsWindows=whsWindows}) = (okIndex,atts')
	where
		(modal,modeless)      	= span isModalWindow whsWindows
		nrModals      	       	= length modal
		nrModeless 	       	= length modeless
		(_,indexAtt,atts')	= remove isWindowIndex (WindowIndex 0) atts
		index			= getWindowIndexAtt indexAtt
		okIndex                	= if   mode==Modal
		                          then 0						-- Open modal windows frontmost
		                          else setBetween index nrModals (nrModals+nrModeless)	-- Open modeless windows behind the modal windows


{-	validateWindowPos validates the WindowPos attribute.
	If no WindowPos attribute is given then Nothing is returned.
	If the WindowPos is relative, it is verified that the window relates to an existing window.
	If this is not the case, then Nothing is returned.
	The resulting attribute list does not contain the WindowPos attribute anymore.
-}
validateWindowPos :: WindowMode -> [WindowAttribute ls ps] -> WindowHandles ps
                      -> (Maybe ItemPos,[WindowAttribute ls ps])
validateWindowPos mode atts windows
	| not hasPosAtt
		= (Nothing,atts')
	| not isRelative
		= (Just itemPos,atts')
	| otherwise
		= (if hasWindowHandlesWindow (toWID relativeTo) windows then Just itemPos else Nothing,atts')
	where
		(hasPosAtt,posAtt,atts') = remove isWindowPos undefined atts
		itemPos			 = getWindowPosAtt posAtt
		(isRelative,relativeTo)  = isRelativeItemPos itemPos


{-	The result ({corner1=A,corner2=B},_) of validateWindowDomain is such that A<B (point A lies to 
	the left of and above point B). If either A.x==B.x or A.y==B.y then the ViewDomain is illegal and 
	the computation is aborted. 
	The default ViewDomain is maximal and positive, i.e.:
		{viewDomainRange & corner1=zero}.
-}
{-	Used only for Window  -}
validateWindowDomain :: [WindowAttribute ls ps] -> (ViewDomain,[WindowAttribute ls ps])
validateWindowDomain atts
	| not hasDomain
		= (viewDomainRange {corner1=zero},atts1)
	| isEmptyRectangle domain
		= windowValidateFatalError "validateWindowDomain" "Window has illegal ViewDomain argument"
	| otherwise
		= (validateViewDomain domain,atts1)
	where
		(hasDomain,domainAtt,atts1) = remove isWindowViewDomain undefined atts
		domain                      = getWindowViewDomainAtt domainAtt

validateViewDomain :: ViewDomain -> ViewDomain
validateViewDomain domain
	= Rectangle
	     { corner1 = Point2
	                    { x = setBetween dl rl rr
	                    , y = setBetween dt rt rb
	                    }
	     , corner2 = Point2
	                    { x = setBetween dr rl rr
	                    , y = setBetween db rt rb
	                    }
	     }
	where
		(Rect {rleft=dl,rtop=dt,rright=dr,rbottom=db}) = rectangleToRect domain
		(Rect {rleft=rl,rtop=rt,rright=rr,rbottom=rb}) = rectangleToRect viewDomainRange


validateCaretPos :: ViewDomain -> [WindowAttribute ls ps] -> [WindowAttribute ls ps]
validateCaretPos domain atts
	| not hasCaret = atts
	| otherwise    = (WindowCaret (Point2 cx' cy') size) : atts1
	where
		(hasCaret,caretAtt,atts1)  = remove isWindowCaret undefined atts
		(Point2 cx cy, size@(Size cw ch)) = getWindowCaretAtt caretAtt
		(Rect {rleft=dl,rtop=dt,rright=dr,rbottom=db}) = rectangleToRect domain
		cx' = setBetween cx dl (dr-cw)
		cy' = setBetween cy dt (db-ch)


{-	validateWindowSize wMetrics viewDomain isMDI isResizable (hasHScroll,hasVScroll) atts
		takes care that the Window(View/Outer)Size attribute fits on the current screen.
		The Boolean  isMDI should be True iff the window belongs to a MDI process.
		The Boolean  isResizable should be True iff the window is resizable. 
		The Booleans hasHScroll hasVScroll should be True iff the window has the WindowHScroll, WindowVScroll
		attribute set respectively. 
		In addition, the WindowOuterSize attribute is mapped to WindowViewSize attribute.
-}
{-	Used only for Window -}
validateWindowSize :: OSWindowMetrics -> ViewDomain -> Bool -> Bool -> (Bool,Bool) -> [WindowAttribute ls ps]
                                                                          -> IO (Size,[WindowAttribute ls ps])
validateWindowSize wMetrics domain isMDI isResizable hasScrolls atts
	| not hasSize = 
		let domainSize = rectangleSize domain
		    pictSize   = Size {w=min (w domainSize) (w maxSize),h=min (h domainSize) (h maxSize)}
		in return (pictSize,(WindowViewSize pictSize):atts)
	| isWindowViewSize sizeAtt =
		let size       = getWindowViewSizeAtt sizeAtt
		    size1      = Size {w=setBetween (w size) (fst minSize) (w maxSize),h=setBetween (h size) (snd minSize) (h maxSize)}
		in return (size1,snd (creplace isWindowViewSize (WindowViewSize size1) atts))
	| otherwise = do
		(dw,dh)   <- osStripOuterSize isMDI isResizable
		let	outerSize   = getWindowOuterSizeAtt sizeAtt
			(w',h')     = (w outerSize-dw,h outerSize-dh)
			visScrolls  = osScrollbarsAreVisible wMetrics (rectangleToRect domain) (w',h') hasScrolls
			viewSize    = rectSize (getWindowContentRect wMetrics visScrolls (sizeToRect (Size {w=w',h=h'})))
			(_,_,atts1) = remove isWindowOuterSize undefined atts
			(_,_,atts2) = remove isWindowViewSize  undefined atts1
		return (viewSize,(WindowViewSize viewSize):atts)		
	where
		(hasSize,sizeAtt) = cselect (\att->isWindowViewSize att || isWindowOuterSize att) undefined atts
		minSize           = osMinWindowSize
		maxSize           = maxScrollWindowSize



{-	validateOrigin takes care that the WindowOrigin attribute is a point in the rectangle
	formed by the left top of the (validated!) ViewDomain, and the width and height of the 
	(validated!) derived size.
-}
{-	Used only for Window -}
validateOrigin :: Size -> ViewDomain -> [WindowAttribute ls ps] -> (Point2,[WindowAttribute ls ps])
validateOrigin (Size {w=w,h=h}) domain@(Rectangle {corner1=Point2 {x=l,y=t},corner2=Point2 {x=r,y=b}}) atts
	= (Point2 {x=setBetween (x origin) l (max l (r-w)),y=setBetween (y origin) t (max t (b-h))},atts1)
	where
		(_,domainAtt,atts1) = remove isWindowOrigin (WindowOrigin (corner1 domain)) atts
		origin              = getWindowOriginAtt domainAtt


{-	validateWindow(H/V)Scroll removes the Window(H/V)Scroll attribute from the attribute list. 
-}
{-	Used only for Window  -}
validateWindowHScroll :: [WindowAttribute ls ps] -> (Maybe ScrollFunction,[WindowAttribute ls ps])
validateWindowHScroll atts
	| found     = (Just (getWindowHScrollFun scrollAtt),atts1)
	| otherwise = (Nothing,atts1)
	where
			(found,scrollAtt,atts1) = remove isWindowHScroll undefined atts

{-	Used only for Window -}
validateWindowVScroll :: [WindowAttribute ls ps] -> (Maybe ScrollFunction,[WindowAttribute ls ps])
validateWindowVScroll atts
	| found     = (Just (getWindowVScrollFun scrollAtt),atts1)
	| otherwise = (Nothing,atts1)
	where
		(found,scrollAtt,atts1) = remove isWindowVScroll undefined atts



{-	validateWindowLook takes care that the optional WindowLook attribute is removed from the attribute list.
	If no attribute was present, then a default look is provided that paints the window with the background colour
	using standard window update mechanism.
-}
--	Used only for Window
validateWindowLook :: [WindowAttribute ls ps] -> (Bool,Look,[WindowAttribute ls ps])
validateWindowLook atts
	= (sysLook,lookFun,atts1)
	where
		(_,lookAtt,atts1) = remove isWindowLook (WindowLook True defaultlook) atts
		(sysLook,lookFun) = getWindowLookAtt lookAtt
		
		defaultlook :: SelectState -> UpdateState -> Draw ()
		defaultlook _ updState = mapM_ unfill (updArea updState)



--	Retrieve (View/Outer)Size, Margins, ItemSpaces, SelectState, and PenAttributes from the attribute list.

attrSize :: [WindowAttribute ls ps] -> Maybe Size
attrSize atts
	| not hasSize          = Nothing
	| isWindowViewSize att = Just (getWindowViewSizeAtt  att)
	| otherwise            = Just (getWindowOuterSizeAtt att)
	where	
		(hasSize,att)  = cselect (\att->isWindowViewSize att || isWindowOuterSize att) undefined atts

attrMargins :: WindowKind -> OSWindowMetrics -> [WindowAttribute ls ps] -> ((Int,Int),(Int,Int))
attrMargins wKind wMetrics atts
	= (getWindowHMargins wKind wMetrics atts,getWindowVMargins wKind wMetrics atts)

attrSelectState :: [WindowAttribute ls ps] -> Bool
attrSelectState atts
	= enabled (getWindowSelectStateAtt (snd (cselect isWindowSelectState (WindowSelectState Able) atts)))

attrPen :: [WindowAttribute ls ps] -> ([PenAttribute],[WindowAttribute ls ps])
attrPen atts = (getWindowPenAtt penAtt,atts1)
	where (_,penAtt,atts1) = remove isWindowPen (WindowPen []) atts


{-	get(Ok/Cancel)Id select the Id of the Window(Ok/Cancel) attribute, and checks
	whether this Id corresponds with a (Custom)ButtonControl.
-}

getOkId :: [WindowAttribute ls ps] -> [WElementHandle ls ps] -> Maybe Id
getOkId atts itemHs
	| not hasid 				= Nothing
	| isOkOrCancelControlId id itemHs 	= Just id
	| otherwise 				= Nothing
	where
	  (hasid,idAtt) = cselect isWindowOk undefined atts
	  id            = getWindowOkAtt idAtt

getCancelId :: [WindowAttribute ls ps] -> [WElementHandle ls ps] -> Maybe Id
getCancelId atts itemHs
	| not hasid 			  = Nothing
	| isOkOrCancelControlId id itemHs = Just id
	| otherwise 			  = Nothing
	where
	  (hasid,idAtt) = cselect isWindowCancel undefined atts
	  id            = getWindowCancelAtt idAtt
	  

{-	Used only by getOkId and getCancelId. -}
isOkOrCancelControlId :: Id -> [WElementHandle ls ps] -> Bool
isOkOrCancelControlId id itemHs =
	case getControlKind id itemHs of
	  Just kind -> kind==IsButtonControl || kind==IsCustomButtonControl
	  Nothing   -> False

{-	validateWindowInitActive checks if the WindowInitActive attribute corresponds with an existing control.
	If this is not the case, the attribute is removed from the attribute list.
-}

validateWindowInitActive :: [WindowAttribute ls ps] -> [WElementHandle ls ps]
                        		-> [WindowAttribute ls ps]
validateWindowInitActive atts itemHs
	| not hasAtt     = atts1
	| isNothing kind = atts1
	| otherwise      = atts
	where
	  (hasAtt,att,atts1) = remove isWindowInitActive undefined atts
	  kind     	     = getControlKind (getWindowInitActiveAtt att) itemHs


{-	getControlKind id itemHs
		returns (Just ControlKind) of the control in the item list. 
		If no such control could be found then Nothing is returned.
-}
{-	Used only by isOkOrCancelControlId and validateWindowInitActive. -}
getControlKind :: Id -> [WElementHandle ls ps] -> Maybe ControlKind
getControlKind id (itemH:itemHs) =
	case getControlKind' id itemH of
	   mk@(Just _) -> mk
	   Nothing     -> getControlKind id itemHs	           
	where
		getControlKind' :: Id -> WElementHandle ls ps -> Maybe ControlKind
		getControlKind' id (WExtendLSHandle addLS itemHs) = getControlKind id itemHs
		getControlKind' id (WChangeLSHandle newLS itemHs) = getControlKind id itemHs
		getControlKind' id (WListLSHandle itemHs) = getControlKind id itemHs
		getControlKind' id itemH@(WItemHandle {wItemId=wItemId,wItemKind=wItemKind,wItems=itemHs})
			| wItemId == Just id 	= Just wItemKind
			| otherwise 		= getControlKind id itemHs
getControlKind _ _ = Nothing



{-	exactWindowSize determines the exact size of a window.
	The size is extended to fit in sliderbars if requested (argument 4 and 5).
-}
exactWindowSize :: OSWindowMetrics -> ViewDomain -> Size -> Bool -> Bool -> WindowKind -> Size
exactWindowSize wMetrics domain wSize@(Size {w=w,h=h}) hasHScroll hasVScroll wKind
	| wKind==IsDialog          = wSize
	| visHScroll && visVScroll = Size {w=w',h=h'}
	| visHScroll               = wSize {h=h'}
	| visVScroll               = wSize {w=w'}
	| otherwise                = wSize
	where
		visHScroll         = hasHScroll && osScrollbarIsVisible (minmax (x $ corner1 domain) (x $ corner2 domain)) w
		visVScroll         = hasVScroll && osScrollbarIsVisible (minmax (y $ corner1 domain) (y $ corner2 domain)) h
		w'                 = w+osmVSliderWidth  wMetrics
		h'                 = h+osmHSliderHeight wMetrics


{-	exactWindowPos determines the exact position of a window.
	The size argument must be the exact size as calculated by exactWindowSize of the window.
	The ItemPos argument must be the validated(!) ItemPos attribute of the window.
-}
exactWindowPos :: OSWindowMetrics -> Size -> Maybe ItemPos -> WindowKind -> WindowMode -> WindowHandles ps -> IO Point2
exactWindowPos wMetrics exactSize maybePos wKind wMode windows
	| wKind==IsDialog && wMode==Modal
		= do {
			screenRect <- osScreenRect;
			let
				screenSize = rectSize screenRect
				l          = rleft screenRect + round ((fromIntegral (w screenSize - w exactSize))/2.0)
				t          = rtop  screenRect + round ((fromIntegral (h screenSize - h exactSize))/3.0)
				pos        = Point2 {x=setBetween l (rleft screenRect) (rright screenRect),y=setBetween t (rtop screenRect) (rbottom screenRect)}
			in return pos
		  }
	| isNothing maybePos = return zero
	| otherwise
		= do {
			pos  <- getItemPosPosition wMetrics exactSize (fromJust maybePos) windows;
			pos1 <- setWindowInsideScreen pos exactSize;
			return pos1
		  }
	where
	{-	getItemPosPosition calculates the exact position of the given window. 
		getItemPosPosition does not check whether this position will place the window off screen.
	-}
		getItemPosPosition :: OSWindowMetrics -> Size -> ItemPos -> WindowHandles ps -> IO Point2
		getItemPosPosition wMetrics size itemPos windows@(WindowHandles {whsWindows=wsHs})
			| isRelative
				= do {
					rect <- osScreenRect;
					let	unidentifyWindow :: WID -> WindowStateHandle ps -> Bool
						unidentifyWindow wid wsH
							= let ids = getWindowStateHandleWIDS wsH
							  in  not (identifyWIDS wid ids)
						
						screenDomain        = rectToRectangle rect
						screenOrigin        = Point2 {x=rleft rect,y=rtop rect}
						(wptr,wsH1)  = case find (unidentifyWindow (toWID relativeTo)) wsHs of
						                          Nothing -> windowValidateFatalError "getItemPosPosition" "target window could not be found"
						                          Just (wsH@(WindowStateHandle wids wlsH)) -> (wPtr wids,wsH)
						relativeSize 	    = getWindowStateHandleSize wsH1
					in
					do {
						(relativeX,relativeY) <- osGetWindowPos wptr;
						let
							(relativeW,relativeH) = toTuple relativeSize
							(exactW,exactH)       = (w size,h size)
							v                     = itemPosOffset (snd itemPos) screenDomain screenOrigin
							(vx',vy')             = (vx v,vy v)
							pos                   = case (fst itemPos) of
							                            (LeftOf  _) -> Point2 {x=relativeX+vx'-exactW,   y=relativeY+vy'}
							                            (RightTo _) -> Point2 {x=relativeX+vx'+relativeW,y=relativeY+vy'}
							                            (Above   _) -> Point2 {x=relativeX+vx',          y=relativeY+vy'-exactH}
							                            (Below   _) -> Point2 {x=relativeX+vx',          y=relativeY+vy'+relativeH}
							                            other       -> windowValidateFatalError "getItemPosPosition" "unexpected ItemLoc alternative"
						in return pos
					}
				  }
			| isAbsolute
				= do {
					rect <- osScreenRect;
					let	screenDomain        = rectToRectangle rect
						screenOrigin        = Point2 {x=rleft rect,y=rtop rect}
					in return (movePoint (itemPosOffset offset screenDomain screenOrigin) zero)
				  }
			| isCornerItemPos itemPos
				= do {
					rect <- osScreenRect;
					let	screenDomain        = rectToRectangle rect
						screenOrigin        = Point2 {x=rleft rect,y=rtop rect}
						(exactW,exactH)     = toTuple size
						v                   = itemPosOffset (snd itemPos) screenDomain screenOrigin
						(vx',vy')           = (vx v,vy v)
						pos                 = case (fst itemPos) of
						                           LeftTop     -> Point2 {x=rleft  rect + vx',        y=rtop    rect + vy'}
						                           RightTop    -> Point2 {x=rright rect + vx'-exactW, y=rtop    rect + vy'}
						                           LeftBottom  -> Point2 {x=rleft  rect + vx',        y=rbottom rect + vy'-exactH}
						                           RightBottom -> Point2 {x=rright rect + vx'-exactW, y=rbottom rect + vy'-exactH}
					in return pos
				  }
			| otherwise
				= return zero
			where
				(isRelative,relativeTo) = isRelativeItemPos itemPos
				(isAbsolute,offset)     = isAbsoluteItemPos itemPos
		
	{-	setWindowInsideScreen makes sure that a window at the given position and given size will be on screen.
	-}
		setWindowInsideScreen :: Point2 -> Size -> IO Point2
		setWindowInsideScreen pos@(Point2 {x=x,y=y}) size@(Size {w=w,h=h})
			= do {
				screenRect <- osScreenRect;
				let
					(Size {w=screenW,h=screenH})
					        = rectSize screenRect
					(x',y') = (setBetween x (rleft screenRect) (rright screenRect-w),setBetween y (rtop screenRect) (rbottom screenRect-h))
					pos1    = if   w<=screenW && h<=screenH then Point2 {x=x',y=y'}		-- window fits entirely on screen
					          else if w<=screenW            then Point2 {x=x',y=0 }		-- window is to high
					          else if h<=screenH            then Point2 {x=0, y=y'}		-- window is to wide
					                                        else zero			-- window doesn't fit anyway
				in return pos1
			  }


--	itemPosOffset calculates the actual offset vector of the given ItemOffset value.

itemPosOffset :: ItemOffset -> ViewDomain -> Point2 -> Vector2
{-
itemPosOffset NoOffset _ _
	= zero
-}
itemPosOffset {-(OffsetVector v)-}v _ _
	= v
{-
itemPosOffset (OffsetFun i f) domain origin
	| i==1       = f (domain,origin)
	| otherwise  = windowValidateFatalError "calculating OffsetFun" ("illegal ParentIndex value: "++show i)
-}


--	Predicates on ItemPos:
isRelativeItemPos :: ItemPos -> (Bool,Id)
isRelativeItemPos (LeftOf  id,_) = (True, id)
isRelativeItemPos (RightTo id,_) = (True, id)
isRelativeItemPos (Above   id,_) = (True, id)
isRelativeItemPos (Below   id,_) = (True, id)
isRelativeItemPos _              = (False,undefined)

isAbsoluteItemPos :: ItemPos -> (Bool,ItemOffset)
isAbsoluteItemPos (Fix,offset) = (True, offset)
isAbsoluteItemPos _            = (False,undefined)

isCornerItemPos :: ItemPos -> Bool
isCornerItemPos (LeftTop,_)     = True
isCornerItemPos (RightTop,_)    = True
isCornerItemPos (LeftBottom,_)  = True
isCornerItemPos (RightBottom,_) = True
isCornerItemPos _               = False
