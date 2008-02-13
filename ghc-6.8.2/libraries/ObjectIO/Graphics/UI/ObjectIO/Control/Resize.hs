-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Resize
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Control.Resize(resizeControls) where



import	Graphics.UI.ObjectIO.CommonDef as C
import	Graphics.UI.ObjectIO.StdControlAttribute(isControlResize, getControlResizeFun,
			    			 isControlMinimumSize, getControlMinimumSizeAtt,
			    			 isControlPos, getControlPosAtt, isControlViewSize)
import	Graphics.UI.ObjectIO.Control.Layout(layoutControls)
import	Graphics.UI.ObjectIO.Control.Relayout(relayoutControls)
import	Graphics.UI.ObjectIO.Window.Handle
import	Graphics.UI.ObjectIO.Window.Access(getWItemCompoundInfo, getWItemEditInfo, getWItemSliderInfo, 
		     			   getCompoundContentRect,
		     			   getWindowHMargins, getWindowVMargins, getWindowItemSpaces)
import	Graphics.UI.ObjectIO.Window.ClipState(forceValidWindowClipState, invalidateCompoundClipState)
import	Graphics.UI.ObjectIO.Window.Draw(drawWindowLook)
import	Graphics.UI.ObjectIO.Window.Update(updateWindowBackgrounds)
import	Graphics.UI.ObjectIO.OS.System(OSWindowMetrics(..))
import	Graphics.UI.ObjectIO.OS.Types(Rect, OSWindowPtr)
import	Graphics.UI.ObjectIO.OS.Window(osMinWindowSize, osMinCompoundSize, osScrollbarsAreVisible)

{-	resizeControls proceeds as follows:
	-	Apply to every control its ControlResizeFunction if applicable.
	-	If some controls have changed size or have been layout relative to the window view frame:
		-	Calculate the new layout.
		-	Reposition and resize the appropriate controls.
-}
resizeControls :: OSWindowMetrics -> Bool -> Bool -> WIDS -> Origin -> Size -> Size -> WindowHandle ls ps -> IO (WindowHandle ls ps)
resizeControls wMetrics isActive updateAll wids@(WIDS {wPtr=wPtr}) oldOrigin oldWSize newWSize wH@(WindowHandle {whWindowInfo=windowInfo,whItems=oldItems,whAtts=whAtts,whDefaultId=whDefaultId}) =
	let (layoutChanged,newItems) = calcNewControlsSize wMetrics originShifted oldWSize newWSize oldItems
	in
	   if not layoutChanged && (w newWSize) <= (w oldWSize) && (h newWSize) <= (h oldWSize)
	   then do	   	   
	   	   wH <- forceValidWindowClipState wMetrics True wPtr wH{whItems=newItems}
		   (if lookSysUpd && not updateAll then return wH
		    else drawWindowLook wMetrics wPtr (return ()) UpdateState{oldFrame=oldFrame,newFrame=newFrame,updArea=[newFrame]} wH)
	   else do
		   (_,newItems) <- layoutControls wMetrics hMargins vMargins spaces newWSize minSize [(domain,newOrigin)] newItems		  
		   wH <- forceValidWindowClipState wMetrics True wPtr wH{whItems=newItems}
		   updRgn <- relayoutControls wMetrics (whSelect wH) (whShow wH) (sizeToRect oldWSize) (sizeToRect newWSize) zero zero wPtr whDefaultId oldItems (whItems wH)
		   wH <- updateWindowBackgrounds wMetrics updRgn wids wH
		   let Point2{x=x,y=y} = oldOrigin
		   let (oldw,oldh) = toTuple oldWSize
		   let (neww,newh) = toTuple newWSize
		   let updArea = if (lookSysUpd && not originShifted && (neww>oldw || newh>oldh) && isActive && not updateAll)
		   	 	 then (	 (if (neww>oldw) then [Rectangle {corner1=oldOrigin{x=x+oldw},corner2=Point2{x=x+neww,y=y+min oldh newh}}] else [])
		  		      ++ (if (newh>oldh) then [Rectangle {corner1=oldOrigin{y=y+oldh},corner2=Point2{x=x+neww,y=y+newh}}]          else [])
		  		      )
		  		 else [newFrame]
		   let updState	= UpdateState{oldFrame=oldFrame,newFrame=newFrame,updArea=updArea}
		   drawWindowLook wMetrics wPtr (return ()) updState wH
	where
		originShifted			= oldOrigin /= newOrigin		
		(newOrigin,domainRect,lookInfo) = (windowOrigin windowInfo,windowDomain windowInfo,windowLook windowInfo)
		domain				= rectToRectangle domainRect
		lookSysUpd			= lookSysUpdate lookInfo
		(defMinW,   defMinH)		= osMinWindowSize		
		hMargins			= getWindowHMargins   (whKind wH) wMetrics whAtts
		vMargins			= getWindowVMargins   (whKind wH) wMetrics whAtts
		spaces				= getWindowItemSpaces (whKind wH) wMetrics whAtts
		minSize				= Size{w=defMinW,h=defMinH}
		oldFrame			= posSizeToRectangle oldOrigin oldWSize
		newFrame			= posSizeToRectangle newOrigin newWSize


{-	calcNewControlsSize applies to a Control its ControlResizeFunction if it has one.
	The Boolean result holds iff some Control has changed its size or may cause change of layout.
-}
calcNewControlsSize :: OSWindowMetrics -> Bool -> Size -> Size -> [WElementHandle ls ps]  -> (Bool,[WElementHandle ls ps])
calcNewControlsSize wMetrics originShifted oldWSize newWSize [] = (False,[])
calcNewControlsSize wMetrics originShifted oldWSize newWSize (itemH:itemHs) =
   let 
	(layoutChanged1,itemH1)	 = calcNewControlSize  wMetrics originShifted oldWSize newWSize itemH
	(layoutChanged2,itemHs1) = calcNewControlsSize wMetrics originShifted oldWSize newWSize itemHs
   in
	(layoutChanged1 || layoutChanged2,itemH1:itemHs1)
   where
	calcNewControlSize :: OSWindowMetrics -> Bool -> Size -> Size -> WElementHandle ls ps -> (Bool,WElementHandle ls ps)
	calcNewControlSize wMetrics originShifted oldWSize newWSize itemH@(WItemHandle {wItemAtts=atts})
	    | not resizable = (isViewFrameSensitive || isOriginSensitive,itemH)
	    | otherwise =
		let (layoutChanged,itemH1) = calcNewWItemSize wMetrics originShifted (\oldCSize -> resizeF oldCSize oldWSize newWSize) itemH
		in (isViewFrameSensitive || isOriginSensitive || layoutChanged,itemH1)
	    where		
		(resizable,resizeAtt)	= cselect isControlResize undefined atts
		(hasPos,posAtt)		= cselect isControlPos undefined atts
		itemPos			= getControlPosAtt posAtt
		resizeF			= getControlResizeFun resizeAtt
		isViewFrameSensitive	= if hasPos
					  then	(case (fst itemPos) of
							LeftBottom	-> True
							RightTop	-> True
							RightBottom	-> True
							Center		-> True
							C.Right		-> True
							Fix		-> originShifted
							_		-> False
						)
					  else False
		isOriginSensitive	= False
		-- should be:
		{-			if hasPos
					  then  (case (snd itemPos) of
							OffsetFun _ _	-> True
							_		-> False
						)
					  else False -}
		
		calcNewWItemSize :: OSWindowMetrics -> Bool -> (IdFun Size) -> WElementHandle ls ps -> (Bool,WElementHandle ls ps)
		
{-	PA: the current size argument of the resize function must be the outer size, not the view size.
		Similar: the result size is also the outer size. 
-}		calcNewWItemSize wMetrics originShifted resizeF itemH@(WItemHandle {wItemKind=IsCompoundControl}) =
			let 
				visOldScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple oldSize) hasScrolls
			  	oldFrameSize	= rectSize (getCompoundContentRect wMetrics visOldScrolls (sizeToRect oldSize))
			  	newSize1	= resizeF oldSize
			  	newSize		= Size {w=max (w minSize) (w newSize1),h=max (h minSize) (h newSize1)}
			  	visNewScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple newSize) hasScrolls
			  	newFrameSize	= rectSize (getCompoundContentRect wMetrics visNewScrolls (sizeToRect newSize))
			in				
				if newFrameSize==oldFrameSize then (False,itemH)
				else
					let 
						newOrigin	= calcNewOrigin origin domainRect newFrameSize
						newInfo		= WCompoundInfo info{compoundOrigin=newOrigin}
						(_,itemHs)	= calcNewControlsSize wMetrics (originShifted || newOrigin/=origin) oldFrameSize newFrameSize (wItems itemH)
						itemH1		= itemH{wItemSize=newSize,wItemAtts=replaceSizeAtt newFrameSize atts,wItemInfo=newInfo,wItems=itemHs}
						itemH2		= invalidateCompoundClipState itemH1
					in (True,itemH2)
			where
				atts			= wItemAtts itemH
				oldSize			= wItemSize itemH
				info			= getWItemCompoundInfo (wItemInfo itemH)
				origin			= compoundOrigin info
				domainRect		= compoundDomain info
				hasScrolls		= (isJust (compoundHScroll info),isJust (compoundVScroll info))
				(defMinW,defMinH)	= osMinCompoundSize
				minSize			= getControlMinimumSizeAtt (snd (cselect isControlMinimumSize (ControlMinimumSize (Size{w=defMinW,h=defMinH})) atts))
			
				calcNewOrigin :: Point2 -> Rect -> Size -> Point2	-- This code also appears at windowdevice: windowStateSizeAction
				calcNewOrigin (Point2 {x=x,y=y}) (Rect {rleft=rleft,rtop=rtop,rright=rright,rbottom=rbottom}) (Size{w=w,h=h})
					= Point2{x=x',y=y'}
					where
						x'	= if x+w > rright  then (max (rright -w) rleft) else x
						y'	= if y+h > rbottom then (max (rbottom-h) rtop ) else y
		
		calcNewWItemSize _ originShifted resizeF itemH@(WItemHandle {wItemKind=IsCustomControl}) =			
			(newSize1/=oldSize,itemH{wItemSize=newSize1,wItemAtts=replaceSizeAtt newSize1 (wItemAtts itemH)})
			where
				oldSize		= wItemSize itemH
				newSize		= resizeF oldSize
				newSize1	= Size{w=max 0 (w newSize),h=max 0 (h newSize)}
		
		calcNewWItemSize _ originShifted resizeF itemH@(WItemHandle {wItemKind=IsCustomButtonControl}) =
			(newSize1/=oldSize,itemH{wItemSize=newSize1,wItemAtts=replaceSizeAtt newSize1 (wItemAtts itemH)})
			where
				oldSize		= wItemSize itemH
				newSize		= resizeF oldSize
				newSize1	= Size{w=max 0 (w newSize),h=max 0 (h newSize)}
		
		calcNewWItemSize wMetrics originShifted resizeF itemH@(WItemHandle {wItemKind=IsEditControl}) =			
			(newSize1/=oldSize,itemH{wItemSize=newSize1,wItemInfo=editInfo,wItemAtts=replaceSizeAtt newSize1 (wItemAtts itemH)})
			where
				oldSize		= wItemSize itemH
				newSize		= resizeF oldSize
				info		= getWItemEditInfo (wItemInfo itemH)
				lineHeight	= osmHeight wMetrics
				nrLines1	= max 1 ((h newSize) `div` lineHeight)
				newSize1	= Size {w=max 0 (w newSize),h=nrLines1*lineHeight}
				editInfo	= WEditInfo info{editInfoWidth=w newSize1,editInfoNrLines=nrLines1}
		
		calcNewWItemSize wMetrics originShifted resizeF itemH@(WItemHandle {wItemKind=IsLayoutControl}) =
			let 
				newSize1 = resizeF oldSize
			  	newSize	 = Size {w=max (w minSize) (w newSize1),h=max (h minSize) (h newSize1)}
			in
				if newSize==oldSize then (False,itemH)
				else 
					let (_,itemHs) = calcNewControlsSize wMetrics originShifted oldSize newSize (wItems itemH)
					in (True,itemH{wItemSize=newSize,wItemAtts=replaceSizeAtt newSize atts,wItems=itemHs})
			where
				atts	= wItemAtts itemH
				oldSize	= wItemSize itemH
				minSize	= getControlMinimumSizeAtt (snd (cselect isControlMinimumSize (ControlMinimumSize zero) atts))
		
		calcNewWItemSize wMetrics originShifted resizeF itemH@(WItemHandle {wItemKind=IsSliderControl}) =			
			(newSize1/=oldSize,itemH{wItemSize=newSize1,wItemInfo=sliderInfo,wItemAtts=replaceSizeAtt newSize1 (wItemAtts itemH)})
			where
				oldSize		= wItemSize itemH
				newSize		= resizeF oldSize
				info		= getWItemSliderInfo (wItemInfo itemH)
				horizontal	= sliderInfoDir info==Horizontal
				newSize1	= if horizontal	
						  then Size{w=max (w newSize) 0,h=osmHSliderHeight wMetrics}
						  else Size{w=osmVSliderWidth wMetrics,h=max (h newSize) 0}
				sSize		= (if horizontal then w else h) newSize1
				sliderInfo	= WSliderInfo info{sliderInfoLength=sSize}
		
		calcNewWItemSize _ _ _ itemH = (False,itemH)
	
	calcNewControlSize wMetrics originShifted oldWSize newWSize (WListLSHandle itemHs) =
		let (layoutChanged,itemHs1) = calcNewControlsSize wMetrics originShifted oldWSize newWSize itemHs
		in (layoutChanged,WListLSHandle itemHs1)
	
	calcNewControlSize wMetrics originShifted oldWSize newWSize (WExtendLSHandle exLS itemHs) =
		let (layoutChanged,itemHs1) = calcNewControlsSize wMetrics originShifted oldWSize newWSize itemHs
		in (layoutChanged,WExtendLSHandle exLS itemHs1)
	
	calcNewControlSize wMetrics originShifted oldWSize newWSize (WChangeLSHandle chLS itemHs) =
		let (layoutChanged,itemHs1) = calcNewControlsSize wMetrics originShifted oldWSize newWSize itemHs
		in (layoutChanged,WChangeLSHandle chLS itemHs1)
	
	replaceSizeAtt :: Size -> [ControlAttribute ls ps] -> [ControlAttribute ls ps]
	replaceSizeAtt size atts	
		| replaced  = atts1
		| otherwise = atts++[sizeAtt]
		where
			(replaced,atts1) = creplace isControlViewSize sizeAtt atts
			sizeAtt		 = ControlViewSize size
