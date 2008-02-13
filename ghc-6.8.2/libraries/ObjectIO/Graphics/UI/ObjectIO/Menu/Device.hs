-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Menu.Device
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Menu.Device(menuFunctions) where


import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.StdId
import	Graphics.UI.ObjectIO.StdGUI
import	Graphics.UI.ObjectIO.Process.IOState
import	Graphics.UI.ObjectIO.Menu.Create
import	Graphics.UI.ObjectIO.Menu.Handle
import	Graphics.UI.ObjectIO.Menu.DefAccess
import	Graphics.UI.ObjectIO.Menu.Access(menuStateHandleGetMenuId)
import	Graphics.UI.ObjectIO.StdMenuAttribute(isMenuFunction, getMenuFun, isMenuModsFunction, getMenuModsFun)
import  Graphics.UI.ObjectIO.Receiver.Handle
import	Graphics.UI.ObjectIO.StdProcessAttribute(getProcessToolbarAtt, isProcessToolbar)
import	Graphics.UI.ObjectIO.OS.Menu
import	Graphics.UI.ObjectIO.OS.Types(osNoWindowPtr, OSMenu)
import	Graphics.UI.ObjectIO.OS.MenuEvent
import	Graphics.UI.ObjectIO.Id(IdParent(..))
import	Control.Monad(when, unless)
import  System.IO(fixIO)
import  Data.IORef(readIORef)
import  qualified Data.Map as Map (lookup)


menuDeviceFatalError :: String -> String -> x
menuDeviceFatalError rule error = dumpFatalError rule "MenuDevice" error

menuFunctions :: DeviceFunctions ps
menuFunctions = DeviceFunctions
	  {	dDevice	= MenuDevice
	  ,	dShow	= menuShow
	  ,	dHide	= menuHide
	  ,	dEvent	= menuEvent
	  ,	dDoIO	= menuIO
	  ,	dOpen	= menuOpen
	  ,	dClose	= menuClose
	  }

menuShow :: ps -> GUI ps ps
menuShow ps = do	
	osdinfo <- accIOEnv ioStGetOSDInfo
	(let 
		maybeMenuBar = getOSDInfoOSMenuBar osdinfo
		osMenuBar				= fromJust maybeMenuBar
	 in 
		if isNothing maybeMenuBar
	 	then menuDeviceFatalError "menuFunctions.dShow" "OSMenuBar could not be retrieved from OSDInfo"
	 	else do	
			liftIO (osMenuBarSet osMenuBar)		  	
		  	appIOEnv (ioStSetOSDInfo (setOSDInfoOSMenuBar osMenuBar osdinfo)))
	return ps

menuClose :: ps -> GUI ps ps
menuClose ps = do
	osdinfo <- accIOEnv ioStGetOSDInfo
	(case getOSDInfoOSMenuBar osdinfo of
		Nothing -> menuDeviceFatalError "MenuFunctions.dClose" "OSMenuBar could not be retrieved from OSDInfo"
		Just osMenuBar -> do
			(found,menus) <- accIOEnv (ioStGetDevice MenuDevice)
			unless found (menuDeviceFatalError "MenuFunctions.dClose" "could not retrieve MenuSystemState from IOSt")				
			let mHs	= menuSystemStateGetMenuHandles menus
			liftIO (disposeMenuHandles mHs osMenuBar)
			ioid <- accIOEnv ioStGetIOId			
			it <- ioStGetIdTable
			let it1 = foldr (disposeIds ioid) it (mMenus mHs)
			ioStSetIdTable it1
			appIOEnv (ioStRemoveDevice MenuDevice)
			appIOEnv (ioStRemoveDeviceFunctions MenuDevice)
			return ps)
	where
		disposeIds :: SystemId -> MenuStateHandle ps -> IdTable -> IdTable
		disposeIds ioid (MenuStateHandle (MenuLSHandle {mlsHandle=mH})) is =
			foldr (disposeMenuIds ioid) is (mItems mH)
			
menuHide :: ps -> GUI ps ps
menuHide ps = do	
	(found,menus) <- accIOEnv (ioStGetDevice MenuDevice)	
	when found (liftIO (osMenuBarClear) >> appIOEnv (ioStSetDevice menus))
	return ps


{-	Opening menus:
	Note:	all interactive processes have atleast the AppleMenu to identify the 
			process, and is used for checking whether the process is active
			(see IOStIsActive further down this module).
			If the process is a subprocess, then the ioguishare of its IOSt
			contains the list to the Mac toolbox menu system.
-}
menuOpen :: ps -> GUI ps ps
menuOpen ps = do
	hasMenu <- accIOEnv (ioStHasDevice MenuDevice)
	(if hasMenu then return ps
	 else do
		di <- accIOEnv ioStGetDocumentInterface
		popUpId <- getPopUpId di
		let bound = case di of
			NDI -> Finite 0
			SDI -> Infinite
			MDI -> Infinite
		let mHs	= MenuHandles
			{ mMenus	= []
			, mKeys		= []			
			, mEnabled	= systemAble
			, mNrMenuBound	= bound
			, mPopUpId	= popUpId					
			}
		appIOEnv (ioStSetDevice (MenuSystemState mHs))
		appIOEnv (ioStSetDeviceFunctions menuFunctions)
		return ps)
	where
		getPopUpId :: DocumentInterface -> GUI ps (Maybe Id)
		getPopUpId NDI = return Nothing
		getPopUpId _ = fmap Just openId


menuIO :: DeviceEvent -> ps -> GUI ps ps
menuIO deviceEvent ps = do
	ok <- accIOEnv (ioStHasDevice MenuDevice)
	unless ok (menuDeviceFatalError "menuFunctions.dDoIO" "could not retrieve MenuSystemState from IOSt")
	toGUI (menuIO deviceEvent ps)
	where
		menuIO :: DeviceEvent -> ps -> IOSt ps -> IO (ps,IOSt ps)
		menuIO (ReceiverEvent rId) ps ioState =
			let
				(_,mDevice) = ioStGetDevice MenuDevice ioState
				menus   = menuSystemStateGetMenuHandles mDevice
			in
				menuMsgIO rId menus ps ioState
			where		
				menuMsgIO :: Id -> MenuHandles ps -> ps -> IOSt ps -> IO (ps, IOSt ps)
				menuMsgIO rId menus@(MenuHandles {mMenus=mHs}) ps ioState = do
					iocontext <- readIORef (ioStGetContext ioState)
					let Just idParent = Map.lookup rId (ioContextGetIdTable iocontext)
					menusMsgIO (\mHs ioState -> (ioStSetDevice (MenuSystemState menus{mMenus=mHs})) ioState) (idpId idParent) rId mHs ps ioState
					where
						menusMsgIO :: ([MenuStateHandle ps] -> IOSt ps -> IOSt ps) ->
							      Id -> Id -> [MenuStateHandle ps] -> ps -> IOSt ps -> IO (ps, IOSt ps)
						menusMsgIO build menuId rId [] ps ioState =
							menuDeviceFatalError "menuIO (ReceiverEvent _) _" "menu could not be found"
						menusMsgIO build menuId rId (msH:msHs) ps ioState =
							if menuStateHandleGetMenuId msH == menuId
							then menuStateMsgIO (\msH ioState -> build (msH:msHs) ioState) rId msH ps ioState
							else menusMsgIO (\msHs ioState -> build (msH:msHs) ioState) menuId rId msHs ps ioState
								

		menuIO (MenuTraceEvent info) ps ioState =
			let
				(_,mDevice) = ioStGetDevice MenuDevice ioState
				menus = menuSystemStateGetMenuHandles mDevice
			in
				menuTraceIO info menus ps ioState
			where
				menuTraceIO :: MenuTraceInfo -> MenuHandles ps -> ps -> IOSt ps -> IO (ps, IOSt ps)
				menuTraceIO info@(MenuTraceInfo {mtId=mtId}) menus@(MenuHandles {mMenus=mHs}) ps ioState = do
					menusTraceIO (\mHs ioState -> (ioStSetDevice (MenuSystemState menus{mMenus=mHs})) ioState) mtId info mHs ps ioState
					where
						menusTraceIO :: ([MenuStateHandle ps] -> IOSt ps -> IOSt ps) ->
								Id -> MenuTraceInfo -> [MenuStateHandle ps] -> ps -> IOSt ps -> IO (ps, IOSt ps)
						menusTraceIO build menuId info [] ps ioState =
							menuDeviceFatalError "menuIO (MenuTraceEvent _) _" "menu could not be found"
						menusTraceIO build menuId info (msH:msHs) ps ioState =
							if menuStateHandleGetMenuId msH == menuId
							then menuStateTraceIO (\msH ioState -> build (msH:msHs) ioState) info msH ps ioState
							else menusTraceIO (\msHs ioState -> build (msH:msHs) ioState) menuId info msHs ps ioState
								

		menuIO (ToolbarSelection tbsItemNr) ps ioState =
			let 
				atts     	    = ioStGetProcessAttributes ioState
				(hasToolbarAtt,att) = cselect isProcessToolbar undefined atts
				toolbarItems	    = getProcessToolbarAtt att
			in
				if not hasToolbarAtt
				then return (ps,ioState)
				else fromGUI (getToolbarFunction tbsItemNr toolbarItems ps) ioState					
			where
				getToolbarFunction :: Int -> [ToolbarItem ps] -> ps -> GUI ps ps
				getToolbarFunction _ [] =
					menuDeviceFatalError "menuIO (ToolbarSelection _)" "toolbar index out of range"
				getToolbarFunction i (ToolbarSeparator:items) = getToolbarFunction i items
				getToolbarFunction i ((ToolbarItem _ _ f):items)
					| i==1 	= f
					| otherwise		= getToolbarFunction (i-1) items
		menuIO _ _ _
			= menuDeviceFatalError "menuIO" "unexpected DeviceEvent"



{-	Apply the Menu(Mods)Function of a selected menu item. -}

menuStateTraceIO :: (MenuStateHandle ps -> IOSt ps -> IOSt ps) -> 
		    MenuTraceInfo -> MenuStateHandle ps -> ps -> IOSt ps -> IO (ps, IOSt ps)
menuStateTraceIO build info@(MenuTraceInfo {mtParents=mtParents}) (MenuStateHandle (MenuLSHandle {mlsState=ls,mlsHandle=mH})) ps ioState = do
	r <- subMenusTraceIO (\itemHs st ioState -> build (MenuStateHandle (MenuLSHandle {mlsState=fst st,mlsHandle=mH{mItems=itemHs}})) ioState) (mHandle mH) mtParents (mItems mH) (ls,ps) ioState
	let ((_,ps1),ioState) = r
	return (ps1,ioState)
	where
		--	subMenusTraceIO finds the final submenu that contains the selected menu item and then applies its Menu(Mods)Function.
		subMenusTraceIO :: ([MenuElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IOSt ps) -> 
				   OSMenu -> [Int] -> [MenuElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IO ((ls,ps), IOSt ps)
		subMenusTraceIO build mHandle [] itemHs ls_ps ioState = do
			r <- menuElementsTraceIO build (mtItemNr info) mHandle 0 itemHs ls_ps ioState
			let (_,ls_ps1,ioState) = r
			return (ls_ps1,ioState)
		subMenusTraceIO build mHandle (subIndex:subIndices) itemHs ls_ps ioState = do
			r <- subMenuTraceIO build subIndex mHandle subIndices 0 itemHs ls_ps ioState
			let (_,ls_ps1,ioState) = r
			return (ls_ps1,ioState)
			where
				subMenuTraceIO :: ([MenuElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IOSt ps) ->
						  Int -> OSMenu -> [Int] -> Int -> [MenuElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IO (Int,(ls,ps),IOSt ps)
				subMenuTraceIO build parentIndex mHandle parentsIndex zIndex (itemH:itemHs) ls_ps ioState = do
					r <- subMenuTraceIO' (\itemH st ioState -> build (itemH:itemHs) st ioState) parentIndex mHandle parentsIndex zIndex itemH ls_ps ioState
					let (zIndex1,ls_ps1,ioState) = r
					(if parentIndex<zIndex1 then return r
					 else subMenuTraceIO (\itemHs st ioState -> build (itemH:itemHs) st ioState) parentIndex mHandle parentsIndex zIndex1 itemHs ls_ps1 ioState)
					where
						subMenuTraceIO' :: (MenuElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
								   Int -> OSMenu -> [Int] -> Int -> MenuElementHandle ls ps -> (ls,ps) -> IOSt ps -> IO (Int,(ls,ps),IOSt ps)
						subMenuTraceIO' build parentIndex mHandle parentsIndex zIndex subH@(SubMenuHandle {mSubHandle=mSubHandle, mSubItems=itemHs}) ls_ps ioState
							| parentIndex /= zIndex = return (zIndex+1,ls_ps,ioState)
							| otherwise = do
								r <- subMenusTraceIO (\itemHs st ioState -> build subH{mSubItems=itemHs} st ioState) mSubHandle parentsIndex itemHs ls_ps ioState
								let (ls_ps1,ioState) = r
								return (zIndex+1,ls_ps1,ioState)
						subMenuTraceIO' build parentIndex mHandle parentsIndex zIndex radioH@(RadioMenuHandle {mRadioItems=itemHs}) ls_ps ioState =
							return (zIndex+length itemHs,ls_ps,ioState)
						subMenuTraceIO' build parentIndex mHandle parentsIndex zIndex itemH@(MenuItemHandle {}) ls_ps ioState =
							return (zIndex+1,ls_ps,ioState)
						subMenuTraceIO' build parentIndex mHandle parentsIndex zIndex (MenuListLSHandle itemHs) ls_ps ioState =
							subMenuTraceIO (\itemHs st ioState -> build (MenuListLSHandle itemHs) st ioState) parentIndex mHandle parentsIndex zIndex itemHs ls_ps ioState
						subMenuTraceIO' build parentIndex mHandle parentsIndex zIndex (MenuExtendLSHandle exLS itemHs) (ls,ps) ioState = do
							r <- subMenuTraceIO (\itemHs st ioState -> build (MenuExtendLSHandle (fst (fst st)) itemHs) (snd (fst st),snd st) ioState) parentIndex mHandle parentsIndex zIndex itemHs ((exLS,ls),ps) ioState
							let (zIndex1,((_,ls1),ps1),ioState) = r
							return (zIndex,(ls1,ps1),ioState)
						subMenuTraceIO' build parentIndex mHandle parentsIndex zIndex (MenuChangeLSHandle chLS itemHs) (ls,ps) ioState = do
							r <- subMenuTraceIO (\itemHs st ioState -> build (MenuChangeLSHandle (fst st) itemHs) (ls,snd st) ioState) parentIndex mHandle parentsIndex zIndex itemHs (chLS,ps) ioState
							let (zIndex1,(_,ps1),ioState) = r
							return (zIndex,(ls,ps1),ioState)
						subMenuTraceIO' _ _ _ _ zIndex itemH ls_ps ioState =
							return (zIndex,ls_ps,ioState)
				subMenuTraceIO _ _ _ _ zIndex itemHs ls_ps ioState =
					return (zIndex,ls_ps,ioState)
		
		--	menuElementsTraceIO applies the Menu(Mods)Function of the menu item at index itemIndex to the context state.
		menuElementsTraceIO :: ([MenuElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IOSt ps) ->
				       Int -> OSMenu -> Int -> [MenuElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IO (Int,(ls,ps),IOSt ps)
		menuElementsTraceIO build itemIndex mHandle zIndex (itemH:itemHs) ls_ps ioState = do
			r <- menuElementTraceIO (\itemH st ioState -> build (itemH:itemHs) st ioState) itemIndex mHandle zIndex itemH ls_ps ioState
			let (zIndex1,ls_ps1,ioState) = r
			(if itemIndex<zIndex1 then return r
			 else menuElementsTraceIO (\itemHs st ioState -> build (itemH:itemHs) st ioState) itemIndex mHandle zIndex1 itemHs ls_ps1 ioState)
			where
				menuElementTraceIO :: (MenuElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
						      Int -> OSMenu -> Int -> MenuElementHandle ls ps -> (ls,ps) -> IOSt ps -> IO (Int, (ls,ps), IOSt ps)
				menuElementTraceIO build itemIndex mHandle zIndex itemH@(MenuItemHandle {mItemAtts=mItemAtts}) (ls,ps) ioState
					| itemIndex /= zIndex || not hasFun	= 
						return (zIndex+1,(ls,ps),ioState)
					| otherwise = do
						r <- fixIO (\st -> fromGUI (f (ls,ps)) (build itemH (fst st) ioState))
						let ((ls1,ps1), ioState) = r
						return (zIndex+1,(ls1,ps1),ioState)
					where
						(hasFun,fAtt) = cselect isEitherFun undefined mItemAtts
						f	= if isMenuFunction fAtt
							  then getMenuFun fAtt
							  else getMenuModsFun fAtt (mtModifiers info)
						isEitherFun f = isMenuFunction f || isMenuModsFunction f
				menuElementTraceIO build itemIndex mHandle zIndex radioH@(RadioMenuHandle {mRadioItems=itemHs}) ls_ps ioState =
					let nrRadios = length itemHs
					in
						if itemIndex>=zIndex+nrRadios then
							-- Selected item is not one of these radio items
							return (zIndex+nrRadios,ls_ps,ioState)
						else
						    let 
							curIndex	= mRadioIndex radioH
							newIndex	= mtItemNr info - zIndex + 1
							curH 		= mOSMenuItem (itemHs!!(curIndex-1))
							newH 		= mOSMenuItem (itemHs!!(newIndex-1))
						    in do
							  osMenuItemCheck False mHandle curH
							  osMenuItemCheck True  mHandle newH
							  r <- menuElementsTraceIO (\itemHs st ioState -> build radioH{mRadioItems=itemHs, mRadioIndex=newIndex} st ioState) itemIndex mHandle zIndex itemHs ls_ps ioState
							  let (_,ls_ps,ioState) = r
							  return (zIndex+length itemHs,ls_ps,ioState)
								
				menuElementTraceIO build itemIndex mHandle zIndex itemH@(SubMenuHandle {}) ls_ps ioState =
					return (zIndex+1,ls_ps,ioState)
					
				menuElementTraceIO build itemIndex mHandle zIndex (MenuListLSHandle itemHs) ls_ps ioState =
					menuElementsTraceIO (\itemHs st ioState -> build (MenuListLSHandle itemHs) st ioState) itemIndex mHandle zIndex itemHs ls_ps ioState
					
				menuElementTraceIO build itemIndex mHandle zIndex (MenuExtendLSHandle exLS itemHs) (ls,ps) ioState = do
					r <- menuElementsTraceIO (\itemHs st ioState -> build (MenuExtendLSHandle (fst (fst st)) itemHs) (snd (fst st),snd st) ioState) itemIndex mHandle zIndex itemHs ((exLS,ls),ps) ioState
					let (zIndex,((_,ls),ps),ioState) = r
					return (zIndex,(ls,ps),ioState)
				menuElementTraceIO build itemIndex mHandle zIndex (MenuChangeLSHandle chLS itemHs) (ls,ps) ioState = do
					r <- menuElementsTraceIO (\itemHs st ioState -> build (MenuChangeLSHandle (fst st) itemHs) (ls,snd st) ioState) itemIndex mHandle zIndex itemHs (chLS,ps) ioState
					let (zIndex,(_,ps),ioState) = r
					return (zIndex,(ls,ps),ioState)
				menuElementTraceIO _ _ _ zIndex itemH ls_ps ioState =
					return (zIndex,ls_ps,ioState)
		menuElementsTraceIO _ _ _ zIndex [] ls_ps ioState = 
			return (zIndex,ls_ps,ioState)


{-	menuStateMsgIO handles all message events. -}

menuStateMsgIO :: (MenuStateHandle ps -> IOSt ps -> IOSt ps) ->
		  Id -> MenuStateHandle ps -> ps -> IOSt ps -> IO (ps,IOSt ps)
menuStateMsgIO build rId msH@(MenuStateHandle mlsH@(MenuLSHandle {mlsState=ls,mlsHandle=mH})) ps ioState = do
    r <- menuMsgIO (\mH st ioState -> build (MenuStateHandle mlsH{mlsState=fst st,mlsHandle=mH}) ioState) rId mH (ls,ps) ioState
    let ((_,ps1),ioState) = r
    return (ps1,ioState)
    where
	-- menuMsgIO handles the first asynchronous message in the message queue of the indicated receiver menu element.
	menuMsgIO :: (MenuHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
		       Id -> MenuHandle ls ps -> (ls,ps) -> IOSt ps -> IO ((ls,ps), IOSt ps)
	menuMsgIO build rId mH@(MenuHandle {mItems=itemHs}) ls_ps ioState = do
	    r <- elementsMsgIO (\itemHs st ioState -> build mH{mItems=itemHs} st ioState) rId itemHs ls_ps ioState
	    let (_,ls_ps,ioState) = r
	    return (ls_ps,ioState)
	    where
		elementsMsgIO :: ([MenuElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IOSt ps) ->
				   Id -> [MenuElementHandle ls ps] -> (ls,ps) -> IOSt ps -> IO (Bool,(ls,ps),IOSt ps)
		elementsMsgIO build rId (itemH:itemHs) ls_ps ioState = do
			r <- elementMsgIO (\itemH st ioState -> build (itemH:itemHs) st ioState) rId itemH ls_ps ioState
			let (done,ls_ps,ioState) = r
			(if done then return r
			 else elementsMsgIO (\itemHs st ioState -> build (itemH:itemHs) st ioState) rId itemHs ls_ps ioState)
			where
				elementMsgIO :: (MenuElementHandle ls ps -> (ls,ps) -> IOSt ps -> IOSt ps) ->
						  Id -> MenuElementHandle ls ps -> (ls,ps) -> IOSt ps -> IO (Bool,(ls,ps),IOSt ps)
				elementMsgIO build id mrH@(MenuReceiverHandle rH@(ReceiverHandle {rFun=rFun,rId=rId}) atts) ls_ps ioState
					| id /= rId = return (False,ls_ps,ioState)
					| otherwise = do
						r <- fixIO (\st -> fromGUI (rFun ls_ps) (build mrH (fst st) ioState))
						let (ls_ps, ioState) = r
						return (True,ls_ps,ioState)

				elementMsgIO build rId subH@(SubMenuHandle {mSubItems=itemHs}) ls_ps ioState = do
					elementsMsgIO (\itemHs st ioState -> build subH{mSubItems=itemHs} st ioState) rId itemHs ls_ps ioState
					
				elementMsgIO build rId (MenuListLSHandle itemHs) ls_ps ioState = do
					elementsMsgIO (\itemHs st ioState -> build (MenuListLSHandle itemHs) st ioState) rId itemHs ls_ps ioState

				elementMsgIO build rId (MenuExtendLSHandle exLS itemHs) (ls,ps) ioState = do
					r <- elementsMsgIO (\itemHs st ioState -> build (MenuExtendLSHandle (fst (fst st)) itemHs) (snd (fst st),snd st) ioState) rId itemHs ((exLS,ls),ps) ioState
					let (done,((_,ls),ps),ioState) = r
					return (done,(ls,ps),ioState)
				elementMsgIO build rId (MenuChangeLSHandle chLS itemHs) (ls,ps) ioState = do
					r <- elementsMsgIO (\itemHs st ioState -> build (MenuChangeLSHandle (fst st) itemHs) (ls,snd st) ioState) rId itemHs (chLS,ps) ioState
					let (done,(_,ps),ioState) = r
					return (done,(ls,ps),ioState)
				elementMsgIO build _ itemH ls_ps ioState =
					return (False,ls_ps,ioState)
		elementsMsgIO _ _ [] ls_ps ioState =
			return (False,ls_ps,ioState)
