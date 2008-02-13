-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Access
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Control.Access contains all read functions on controls.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Control.Access
		( getcontrolslayouts, getcontrolsviewsizes
		, getcontrolsoutersizes, getcontrolsselects
	      	, getcontrolsshowstates, getcontrolstexts
	      	, getcontrolsnrlines, getcontrolslooks
	      	, getcontrolsminsizes, getcontrolsresizes
	      	, getcontrolitems, getcontrolsselections
	      	, getcontrolsmarks
	      	, getslidersdirections, getslidersstates
	      	, getcontrolsframes, getcontrolsdomains
	      	, getscrollfunctions
	      	) where		     


import Graphics.UI.ObjectIO.Id
import Graphics.UI.ObjectIO.StdIOCommon as C
import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.StdControlAttribute
import Graphics.UI.ObjectIO.Window.Access
import Graphics.UI.ObjectIO.OS.Window(OSWindowMetrics, osGetEditControlText, osScrollbarsAreVisible, osGetCompoundContentRect)


{-	Higher order access on [WElementHandle ls ps].
	Although statemapWElementHandles has (... WElementHandle ls ps ...) function arguments,
	these will only be applied to WItemHandle alternatives.
-}

type MapFunction ps x = forall ls . WElementHandle ls ps -> x -> IO (WElementHandle ls ps, x)

statemapWElementHandles :: MapFunction ps x -> [Id] -> [WElementHandle ls ps] -> x -> IO ([Id], [WElementHandle ls ps], x)
statemapWElementHandles f ids [] s = return (ids, [], s)
statemapWElementHandles f ids (itemH:itemHs) s
	| null ids      = return (ids, (itemH:itemHs), s)
	| otherwise   = do
		(ids, itemH,  s) <- statemapWElementHandle  f ids itemH  s
		(ids, itemHs, s) <- statemapWElementHandles f ids itemHs s
		return (ids, itemH:itemHs, s)
	where
		statemapWElementHandle :: MapFunction ps x -> [Id] -> WElementHandle ls ps -> x -> IO ([Id], WElementHandle ls ps, x)
		statemapWElementHandle f ids (WListLSHandle itemHs) s = do
			(ids, itemHs, s) <- statemapWElementHandles f ids itemHs s
			return (ids, WListLSHandle itemHs, s)
		statemapWElementHandle f ids (WChangeLSHandle chLS itemHs) s = do
			(ids, itemHs, s) <- statemapWElementHandles f ids itemHs s
			return (ids, WChangeLSHandle chLS itemHs, s)
		statemapWElementHandle f ids (WExtendLSHandle exLS itemHs) s = do
			(ids, itemHs, s) <- statemapWElementHandles f ids itemHs s
			return (ids, WExtendLSHandle exLS itemHs, s)
		statemapWElementHandle f ids itemH s = do
			(ids, itemH, s) <- statemapWElementHandle' f ids itemH s
			(if isRecursiveControl (wItemKind itemH)
			 then do
				(ids, itemHs, s) <- statemapWElementHandles f ids (wItems itemH) s
				return (ids, itemH{wItems=itemHs}, s)
			 else return (ids, itemH, s))
			where
				statemapWElementHandle' :: MapFunction ps x -> [Id] -> WElementHandle ls ps -> x -> IO ([Id], WElementHandle ls ps, x)
				statemapWElementHandle' f ids itemH@(WItemHandle {wItemId=wItemId}) s
					| isNothing wItemId = return (ids, itemH, s)
					| not hadId			= return (ids, itemH, s)
					| otherwise			= do
						(itemH, s) <- f itemH s
						return (ids1, itemH, s)
					where				
						itemId			= fromJust wItemId
						(hadId,ids1)	= removeCheck itemId ids


--	Access operations on WElementHandle.

getcontrolslayouts :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,(Maybe ItemPos,Vector2))] -> IO (WindowHandle ls ps, [(Id,Bool,(Maybe ItemPos,Vector2))])
getcontrolslayouts ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) layouts = do
	(ids, itemHs, layouts) <- statemapWElementHandles getlayouts ids itemHs layouts
	return (wH{whItems=itemHs}, layouts)
	where
		getlayouts :: WElementHandle ls ps -> [(Id,Bool,(Maybe ItemPos,Vector2))] -> IO (WElementHandle ls ps, [(Id,Bool,(Maybe ItemPos,Vector2))])
		getlayouts itemH@(WItemHandle {wItemAtts=atts,wItemPos=wItemPos}) layouts =
			return (itemH, layouts1)
			where
				itemPos			= if hasAtt then Just (getControlPosAtt posAtt) else Nothing
				(hasAtt,posAtt)	= cselect isControlPos (ControlPos (C.Left,zero)) atts
				layout			= (itemId,True,(itemPos,toVector wItemPos))
				(_,layouts1)	= creplace (eqfst3id itemId) layout layouts
				itemId          = fromJust (wItemId itemH)

getcontrolsviewsizes :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Size)] -> IO (WindowHandle ls ps , [(Id,Bool,Size)])
getcontrolsviewsizes ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) sizes = do
		(ids, itemHs, sizes) <- statemapWElementHandles (getsizes wMetrics) ids itemHs sizes
		return (wH{whItems=itemHs}, sizes)
		where
			getsizes :: OSWindowMetrics -> WElementHandle ls ps -> [(Id,Bool,Size)] -> IO (WElementHandle ls ps, [(Id,Bool,Size)])
			getsizes wMetrics itemH@(WItemHandle {wItemKind=wItemKind}) sizes =
				return (itemH, sizes1)
				where
					itemSize		= wItemSize itemH
					info			= getWItemCompoundInfo (wItemInfo itemH)
					(domainRect,hasScrolls)
									= (compoundDomain info,(isJust (compoundHScroll info),isJust (compoundVScroll info)))
					visScrolls		= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
					size			= if (wItemKind/=IsCompoundControl)
 									  then itemSize
									  else rectSize (getCompoundContentRect wMetrics visScrolls (sizeToRect itemSize))
					(_,sizes1)		= creplace (eqfst3id itemId) (itemId,True,size) sizes
					itemId          = fromJust (wItemId itemH)

getcontrolsoutersizes :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Size)] -> IO (WindowHandle ls ps , [(Id,Bool,Size)])
getcontrolsoutersizes ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) sizes = do
	(ids, itemHs, sizes) <- statemapWElementHandles (getsizes wMetrics) ids itemHs sizes
	return (wH{whItems=itemHs}, sizes)
	where
		getsizes :: OSWindowMetrics -> WElementHandle ls ps -> [(Id,Bool,Size)] -> IO (WElementHandle ls ps, [(Id,Bool,Size)])
		getsizes wMetrics itemH@(WItemHandle {}) sizes =
			return (itemH, sizes1)
			where
				size			= wItemSize itemH
				(_,sizes1)		= creplace (eqfst3id itemId) (itemId,True,size) sizes
				itemId          = fromJust (wItemId itemH)

getcontrolsselects :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,SelectState)] -> IO (WindowHandle ls ps , [(Id,Bool,SelectState)])
getcontrolsselects ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) selects = do
	(ids, itemHs, selects) <- statemapWElementHandles getselects ids itemHs selects
	return (wH{whItems=itemHs}, selects)
	where
		getselects :: WElementHandle ls ps -> [(Id,Bool,SelectState)] -> IO (WElementHandle ls ps, [(Id,Bool,SelectState)])
		getselects itemH@(WItemHandle {wItemSelect=wItemSelect}) selects =
			return (itemH, selects1)
			where
				selectstate		= if wItemSelect then Able else Unable
				select			= (itemId,True,selectstate)
				(_,selects1)	= creplace (eqfst3id itemId) select selects
				itemId          = fromJust (wItemId itemH)
				

getcontrolsshowstates :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Bool)] -> IO (WindowHandle ls ps, [(Id,Bool,Bool)])
getcontrolsshowstates ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) shows = do
	(ids, itemHs, shows) <- statemapWElementHandles getshowstates ids itemHs shows
	return (wH{whItems=itemHs}, shows)
	where
		getshowstates :: WElementHandle ls ps -> [(Id,Bool,Bool)] -> IO (WElementHandle ls ps, [(Id,Bool,Bool)])
		getshowstates itemH@(WItemHandle {wItemShow=wItemShow}) shows =
			return (itemH, shows1)
			where
				show			= (itemId,True,wItemShow)
				(_,shows1)		= creplace (eqfst3id itemId) show shows
				itemId          = fromJust (wItemId itemH)

getcontrolstexts :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Maybe String)] -> IO (WindowHandle ls ps, [(Id,Bool,Maybe String)])
getcontrolstexts ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) texts = do
	(ids, itemHs, texts) <- statemapWElementHandles gettext ids itemHs texts
	return (wH{whItems=itemHs}, texts)
	where
		gettext :: WElementHandle ls ps -> [(Id,Bool,Maybe String)] -> IO (WElementHandle ls ps, [(Id,Bool,Maybe String)])
		gettext itemH@(WItemHandle {}) texts =			
			case wItemKind itemH of
					IsPopUpControl  -> do
							(textline, popUpInfo) <- getPopUpText (getWItemPopUpInfo  info)
							return (itemH{wItemInfo=WPopUpInfo popUpInfo}, snd (creplace (eqfst3id itemId) (itemId,True,Just textline) texts))
					IsTextControl  -> do
							let textline = textInfoText   (getWItemTextInfo   info)
							return (itemH, snd (creplace (eqfst3id itemId) (itemId,True,Just textline) texts))
					IsEditControl  -> do
							textline <- osGetEditControlText (wPtr wids) (wItemPtr itemH)
							return (itemH{wItemInfo=WEditInfo ((getWItemEditInfo info){editInfoText=textline})}, snd (creplace (eqfst3id itemId) (itemId,True,Just textline) texts))
					IsButtonControl -> do
							let textline = buttonInfoText (getWItemButtonInfo info)
							return (itemH, snd (creplace (eqfst3id itemId) (itemId,True,Just textline) texts))
					_ -> return (itemH, texts)
			where
				info                 = wItemInfo itemH
				itemId               = fromJust (wItemId itemH)
				
				getPopUpText :: PopUpInfo ls ps -> IO (String, PopUpInfo ls ps)
				getPopUpText popUpInfo@(PopUpInfo {popUpInfoEdit=popUpInfoEdit,popUpInfoItems=popUpInfoItems}) =
					case popUpInfoEdit of
						Nothing -> 
							if null popUpInfoItems
							then return ("", popUpInfo)
							else return (fst (popUpInfoItems!!(max 0 (popUpInfoIndex popUpInfo - 1))), popUpInfo)
						Just info -> do
							content <- osGetEditControlText (wItemPtr itemH) (popUpEditPtr info)
							return (content, popUpInfo{popUpInfoEdit=Just (info{popUpEditText=content})})


getcontrolsnrlines :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Maybe Int)] -> IO (WindowHandle ls ps,[(Id,Bool,Maybe Int)])
getcontrolsnrlines ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) nrlines = do
	(ids, itemHs, nrlines) <- statemapWElementHandles getnrlines ids itemHs nrlines
	return (wH{whItems=itemHs}, nrlines)
	where
		getnrlines :: WElementHandle ls ps -> [(Id,Bool,Maybe Int)] -> IO (WElementHandle ls ps,[(Id,Bool,Maybe Int)])
		getnrlines itemH@(WItemHandle {wItemKind=itemKind}) nrlines
			| itemKind /= IsEditControl = return (itemH,nrlines)
			| otherwise		    = return (itemH,nrlines1)
			where
				info	= getWItemEditInfo (wItemInfo itemH)
				itemId  = fromJust (wItemId itemH)
				nrline	= (itemId,True,Just (editInfoNrLines info))
				(_,nrlines1) = creplace (eqfst3id itemId) nrline nrlines


getcontrolslooks :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Maybe (Bool,Look))] -> IO (WindowHandle ls ps, [(Id,Bool,Maybe (Bool,Look))])
getcontrolslooks ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) looks = do
	(ids, itemHs, looks) <- statemapWElementHandles getlooks ids itemHs looks
	return (wH{whItems=itemHs}, looks)
	where
		getlooks :: WElementHandle ls ps -> [(Id,Bool,Maybe (Bool,Look))] -> IO (WElementHandle ls ps, [(Id,Bool,Maybe (Bool,Look))])
		getlooks itemH@(WItemHandle {wItemKind=itemKind,wItemInfo=info}) looks
			| not haslook	= return (itemH, looks )
			| otherwise	= return (itemH, looks1)
			where
				itemId			= fromJust (wItemId itemH)
				(haslook,lookInfo)	= getLookInfo itemKind info
				look 			= Just (lookSysUpdate lookInfo,lookFun lookInfo)
				(_,looks1)		= creplace (eqfst3id itemId) (itemId,True,look) looks

				getLookInfo :: ControlKind -> WItemInfo ls ps -> (Bool,LookInfo)
				getLookInfo IsCustomButtonControl info =
					(True,cButtonInfoLook (getWItemCustomButtonInfo info))
				getLookInfo IsCustomControl info =
					(True,customInfoLook (getWItemCustomInfo info))
				getLookInfo IsCompoundControl info =
					(True,compoundLook (compoundLookInfo (getWItemCompoundInfo info)))
				getLookInfo _ _ = (False,undefined)


getcontrolsminsizes :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Maybe Size)] -> IO (WindowHandle ls ps,[(Id,Bool,Maybe Size)])
getcontrolsminsizes ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) sizes = do
	(ids, itemHs, sizes) <- statemapWElementHandles getsizes ids itemHs sizes
	return (wH{whItems=itemHs}, sizes)
	where
		getsizes :: WElementHandle ls ps -> [(Id,Bool,Maybe Size)] -> IO (WElementHandle ls ps, [(Id,Bool,Maybe Size)])
		getsizes itemH@(WItemHandle {wItemAtts=atts,wItems=itemHs}) sizes = return (itemH, sizes1)
			where
				itemId		     = fromJust (wItemId itemH)
				(has_minsize,minatt) = cselect isControlMinimumSize (dummy "getcontrolsminsizes") atts
				size		     = (itemId,True,if has_minsize then (Just (getControlMinimumSizeAtt minatt)) else Nothing)
				(_,sizes1)	     = creplace (eqfst3id itemId) size sizes


getcontrolsresizes :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Maybe ControlResizeFunction)] -> IO (WindowHandle ls ps,[(Id,Bool,Maybe ControlResizeFunction)])
getcontrolsresizes ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) resizes = do
	(ids, itemHs, resizes) <- statemapWElementHandles getresizes ids itemHs resizes
	return (wH{whItems=itemHs}, resizes)
	where
		getresizes :: WElementHandle ls ps -> [(Id,Bool,Maybe ControlResizeFunction)] -> IO (WElementHandle ls ps, [(Id,Bool,Maybe ControlResizeFunction)])
		getresizes itemH@(WItemHandle {wItemAtts=atts,wItems=itemHs}) resizes
			| not hasResize		= return (itemH, resizes )
			| otherwise		= return (itemH, resizes1)
			where
				itemId			= fromJust (wItemId itemH)
				resize			= (itemId,True,Just (getControlResizeFun resizeAtt))
				(_,resizes1)    	= creplace (eqfst3id itemId) resize resizes
				(hasResize,resizeAtt)	= cselect isControlResize (dummy "getresizes") atts


getcontrolitems :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Maybe [String])] -> IO (WindowHandle ls ps, [(Id,Bool,Maybe [String])])
getcontrolitems ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) titles = do
	(ids, itemHs, titles) <- statemapWElementHandles gettitle ids itemHs titles
	return (wH{whItems=itemHs}, titles)
	where
		gettitle :: WElementHandle ls ps -> [(Id,Bool,Maybe [String])] -> IO (WElementHandle ls ps, [(Id,Bool,Maybe [String])])
		gettitle itemH@(WItemHandle {wItemKind=itemKind,wItemInfo=info}) titles			
			| Just title <- mb_title = 
				let (_,titles1)	= creplace (eqfst3id itemId) title titles
				in return (itemH, titles1)
			| otherwise		 = return (itemH, titles)
			where
				fst3 (a,b,c)	= a
				fst4 (a,b,c,d)	= a
				itemId		= fromJust (wItemId itemH)
				mb_title 	= case itemKind of
					IsPopUpControl	-> Just (itemId,True,Just (map fst  (popUpInfoItems (getWItemPopUpInfo info))))
					IsListBoxControl-> Just (itemId,True,Just (map fst3 (listBoxInfoItems (getWItemListBoxInfo info))))
					IsRadioControl	-> Just (itemId,True,Just [fst3 (radioItem item) | item<-radioItems (getWItemRadioInfo info)])
					IsCheckControl	-> Just (itemId,True,Just [fst4 (checkItem item) | item<-checkItems (getWItemCheckInfo info)])
					_		-> Nothing
					
getcontrolsselections :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Maybe Index)] -> IO (WindowHandle ls ps, [(Id,Bool,Maybe Index)])
getcontrolsselections ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) marks = do
	(ids, itemHs, marks) <- statemapWElementHandles getselection ids itemHs marks
	return (wH{whItems=itemHs}, marks)
	where
		getselection :: WElementHandle ls ps -> [(Id,Bool,Maybe Index)] -> IO (WElementHandle ls ps, [(Id,Bool,Maybe Index)])
		getselection itemH@(WItemHandle {wItemKind=itemKind,wItemInfo=info}) indices
			| Just index <- mb_index =
				let (_,indices1) = creplace (eqfst3id itemId) index indices
				in return (itemH,indices1)
			| otherwise 	  = return (itemH,indices)
			where
				itemId		= fromJust (wItemId itemH)
				mb_index = case itemKind of
					IsRadioControl  -> Just (itemId,True,Just (radioIndex (getWItemRadioInfo info)))
					IsPopUpControl  -> Just (itemId,True,Just (popUpInfoIndex (getWItemPopUpInfo info)))
					IsListBoxControl-> Just (itemId,True,Just (findSelIndex 1 (listBoxInfoItems (getWItemListBoxInfo info))))
					_	        -> Nothing
					
				findSelIndex n [] = 0
				findSelIndex n ((title,mark,f):items)
					| marked mark = n
					| otherwise   = findSelIndex (n+1) items
				
				
getcontrolsmarks :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Maybe [Index])] -> IO (WindowHandle ls ps, [(Id,Bool,Maybe [Index])])
getcontrolsmarks ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) marks = do
	(ids, itemHs, marks) <- statemapWElementHandles getmarks ids itemHs marks
	return (wH{whItems=itemHs}, marks)
	where
		getmarks :: WElementHandle ls ps -> [(Id,Bool,Maybe [Index])] -> IO (WElementHandle ls ps, [(Id,Bool,Maybe [Index])])
		getmarks itemH@(WItemHandle {wItemKind=IsCheckControl,wItemInfo=info}) marks = return (itemH,marks1)
			where
				itemId		= fromJust (wItemId itemH)
				indices		= getMarkIndices 1 (checkItems (getWItemCheckInfo info))
				mark		= (itemId,True,Just indices)
				(_,marks1)	= creplace (eqfst3id itemId) mark marks

				getMarkIndices :: Index -> [CheckItemInfo ls ps] -> [Index]
				getMarkIndices index (CheckItemInfo {checkItem=(_,_,mark,_)}:items) =
					let
						indexs	= getMarkIndices (index+1) items
					in
						if marked mark then index:indexs
						else indexs
				getMarkIndices _ _ = []
		getmarks itemH@(WItemHandle {wItemKind=IsListBoxControl,wItemInfo=info}) marks = return (itemH,marks1)
			where
				itemId		= fromJust (wItemId itemH)
				indices		= getIndices 1 (listBoxInfoItems (getWItemListBoxInfo info))
				mark		= (itemId,True,Just indices)
				(_,marks1)	= creplace (eqfst3id itemId) mark marks

				getIndices :: Index -> [ListBoxControlItem ls ps] -> [Index]
				getIndices index ((_,mark,_):items) =
					let
						indices	= getIndices (index+1) items
					in
						if marked mark then index:indices
						else indices
				getIndices _ _ = []
		getmarks itemH marks = return (itemH,marks)


getslidersdirections :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Maybe Direction)] -> IO (WindowHandle ls ps,[(Id,Bool,Maybe Direction)])
getslidersdirections ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) sliders = do
	(ids, itemHs, sliders) <- statemapWElementHandles getdirections ids itemHs sliders
	return (wH{whItems=itemHs}, sliders)
	where
		getdirections :: WElementHandle ls ps -> [(Id,Bool,Maybe Direction)] -> IO (WElementHandle ls ps, [(Id,Bool,Maybe Direction)])
		getdirections itemH@(WItemHandle {wItemKind=itemKind,wItemInfo=info}) sliders
			| itemKind /= IsSliderControl	= return (itemH,sliders )
			| otherwise			= return (itemH,sliders1)
			where
				itemId		= fromJust (wItemId itemH)
				slider		= (itemId,True,Just (sliderInfoDir (getWItemSliderInfo info)))
				(_,sliders1)	= creplace (eqfst3id itemId) slider sliders
		

getslidersstates :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Maybe SliderState)] -> IO (WindowHandle ls ps,[(Id,Bool,Maybe SliderState)])
getslidersstates ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) states = do
	(ids, itemHs, states) <- statemapWElementHandles getstates ids itemHs states
	return (wH{whItems=itemHs}, states)
	where
		getstates :: WElementHandle ls ps -> [(Id,Bool,Maybe SliderState)] -> IO (WElementHandle ls ps, [(Id,Bool,Maybe SliderState)])
		getstates itemH@(WItemHandle {wItemKind=itemKind,wItemInfo=info}) states
			| itemKind /= IsSliderControl	= return (itemH,states )
			| otherwise			= return (itemH,states1)
			where
				itemId		= fromJust (wItemId itemH)
				state		= (itemId,True,Just (sliderInfoState (getWItemSliderInfo info)))
				(_,states1)	= creplace (eqfst3id itemId) state states


getcontrolsframes :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Maybe ViewFrame)] -> IO (WindowHandle ls ps,[(Id,Bool,Maybe ViewFrame)])
getcontrolsframes ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) frames = do
	(ids, itemHs, frames) <- statemapWElementHandles getframes ids itemHs frames
	return (wH{whItems=itemHs}, frames)
	where
		getframes :: WElementHandle ls ps -> [(Id,Bool,Maybe ViewFrame)] -> IO (WElementHandle ls ps, [(Id,Bool,Maybe ViewFrame)])
		getframes itemH@(WItemHandle {wItemKind=itemKind,wItemSize=itemSize,wItemInfo=wItemInfo}) frames
			| itemKind /= IsCompoundControl	= return (itemH,frames )
			| otherwise			= return (itemH,frames1)
			where
				itemId		= fromJust (wItemId itemH)
				info		= getWItemCompoundInfo wItemInfo
				(origin,domainRect,hasScrolls) = (compoundOrigin info,compoundDomain info,(isJust (compoundHScroll info),isJust (compoundVScroll info)))
				visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
				itemRect	= osGetCompoundContentRect wMetrics visScrolls (posSizeToRect origin itemSize)
				frame		= (itemId,True,Just (rectToRectangle itemRect))
				(_,frames1)	= creplace (eqfst3id itemId) frame frames
		

getcontrolsdomains :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Maybe ViewDomain)] -> IO (WindowHandle ls ps,[(Id,Bool,Maybe ViewDomain)])
getcontrolsdomains ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) domains = do
	(ids, itemHs, domains) <- statemapWElementHandles getdomains ids itemHs domains
	return (wH{whItems=itemHs}, domains)
	where
		getdomains :: WElementHandle ls ps -> [(Id,Bool,Maybe ViewDomain)] -> IO (WElementHandle ls ps, [(Id,Bool,Maybe ViewDomain)])
		getdomains itemH@(WItemHandle {wItemKind=itemKind,wItemInfo=info}) domains
			| itemKind /= IsCompoundControl	= return (itemH,domains )
			| otherwise			= return (itemH,domains1)
			where
				itemId		= fromJust (wItemId itemH)
				domain		= (itemId,True,Just (rectToRectangle (compoundDomain (getWItemCompoundInfo info))))
				(_,domains1) 	= creplace (eqfst3id itemId) domain domains


getscrollfunctions :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> [(Id,Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))] -> IO (WindowHandle ls ps,[(Id,Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))])
getscrollfunctions ids wMetrics wids wH@(WindowHandle {whItems=itemHs}) funcs = do
	(ids, itemHs, funcs) <- statemapWElementHandles getfuncs ids itemHs funcs
	return (wH{whItems=itemHs}, funcs)
	where
		getfuncs :: WElementHandle ls ps -> [(Id,Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))] -> IO (WElementHandle ls ps, [(Id,Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))])
		getfuncs itemH@(WItemHandle {wItemKind=itemKind,wItemInfo=wItemInfo}) funcs
			| itemKind /= IsCompoundControl = return (itemH,funcs )
			| otherwise			= return (itemH,funcs1)
			where
				itemId		= fromJust (wItemId itemH)
				info		= getWItemCompoundInfo wItemInfo
				hScroll		= compoundHScroll info
				vScroll		= compoundVScroll info
				func		= (itemId,True,Just ((Horizontal,fmap scrollFunction hScroll)
								    ,(Vertical,  fmap scrollFunction vScroll)
								    ))
				(_,funcs1)	= creplace (eqfst3id itemId) func funcs