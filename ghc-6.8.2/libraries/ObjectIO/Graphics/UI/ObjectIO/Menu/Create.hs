-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Menu.Create
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Menu.Create
		( openMenu', createPopUpMenu, extendMenu
		, disposeMenuItemHandle, disposeMenuIds
		, disposeSubMenuHandles
		, disposeMenuItemHandle, disposeMenuHandles
		, closePopUpMenu
		, systemAble, systemUnable
		) where


import  Graphics.UI.ObjectIO.Id
import  Graphics.UI.ObjectIO.StdMenuDef
import	Graphics.UI.ObjectIO.StdMenuElementClass
import	Graphics.UI.ObjectIO.StdMenuAttribute(getMenuInitFun, isMenuInit)
import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.Process.IOState
import	Graphics.UI.ObjectIO.Window.SDISize
import	Graphics.UI.ObjectIO.Device.SystemState
import	Graphics.UI.ObjectIO.Menu.Access
import	Graphics.UI.ObjectIO.Menu.DefAccess
import	Graphics.UI.ObjectIO.Menu.Handle
import  Graphics.UI.ObjectIO.Receiver.Handle(ReceiverHandle(..))
import  Graphics.UI.ObjectIO.OS.MenuEvent(menuHandlesGetMenuStateHandles)
import	Graphics.UI.ObjectIO.OS.Menu
import	Graphics.UI.ObjectIO.OS.Types
import  Control.Monad(when, unless)
import  qualified Data.Map as Map


menuCreateFatalError :: String -> String -> x
menuCreateFatalError rule error
	= dumpFatalError rule "MenuCreate" error
	

{-	Creating menus:
		Because in a SDI process menus might be added to the process window, the ViewFrame of the
		process window can change size.
		In that case, the layout of the controls should be recalculated, and the window updated.
	openMenu' assumes that the Id argument has been verified and that the MenuDevice exists.
-}
openMenu' :: MenuElements m => Id -> ls -> (Menu m ls ps) -> ps -> GUI ps ps
openMenu' menuId ls mDef ps = do
    osdInfo <- accIOEnv (ioStGetOSDInfo)
    let maybeOSMenuBar = getOSDInfoOSMenuBar osdInfo
    when (isNothing maybeOSMenuBar) (menuCreateFatalError "openMenu (Menu)" "could not retrieve OSMenuBar from IOSt") 	-- This condition should never hold
    let osMenuBar = fromJust maybeOSMenuBar
    idtable <- ioStGetIdTable
    (found,mDevice) <- accIOEnv (ioStGetDevice MenuDevice)
    let mHs = menuSystemStateGetMenuHandles mDevice
    when (any (isMenuWithThisId menuId) (mMenus mHs)) (menuCreateFatalError "openMenu (Menu)" "inconsistency detected between IdTable and ReceiverTable")        
    let (menus,mHs1) = menuHandlesGetMenuStateHandles mHs
    let nrmenus = length menus
    let index = maybe nrmenus (\index -> setBetween index 0 nrmenus) (menuDefGetIndex mDef)
    ioid <- accIOEnv ioStGetIOId
    (sdiSize1,sdiPtr) <- getSDIWindowSize
    (ok,mH,mHs2,idtable) <- createMenu index ioid menuId mDef mHs1 idtable osMenuBar
    ioStSetIdTable idtable
    (if not ok then do
	appIOEnv (ioStSetDevice (MenuSystemState mHs2))
	throwGUI ErrorIdsInUse
     else do
	let (before,after)	= splitAt index menus
	let msH = MenuStateHandle (MenuLSHandle {mlsState=ls,mlsHandle=mH})
	let mHs3 = mHs2{mMenus=before++msH:after}
	liftIO (drawMenuBar osMenuBar)
	appIOEnv (ioStSetDevice (MenuSystemState mHs3))
	checkSDISize sdiPtr sdiSize1
	getMenuDefInit mDef ps)
    where	
	checkSDISize :: OSWindowPtr -> Size -> GUI ps ()
	checkSDISize sdiPtr sdiSize1 = do
		(sdiSize2,_) <- getSDIWindowSize
		when (sdiSize1 /= sdiSize2) (resizeSDIWindow sdiPtr sdiSize1 sdiSize2)
	
	getMenuDefInit :: Menu m ls ps -> ps -> GUI ps ps
	getMenuDefInit (Menu _ _ atts) = getMenuInitFun (snd (cselect isMenuInit (MenuInit return) atts))

	createMenu :: MenuElements m => Int -> SystemId -> Id -> Menu m ls ps -> MenuHandles ps -> IdTable -> OSMenuBar -> GUI ps (Bool,MenuHandle ls ps, MenuHandles ps, IdTable)
	createMenu index ioId menuId mDef mHs@(MenuHandles {mKeys=keys}) it osMenuBar = do
		ms <- menuElementToHandles (menuDefGetElements mDef)
		let itemHs = map menuElementStateToMenuElementHandle ms
		(case menuIdsAreConsistent ioId menuId itemHs it of
			Just it -> do
				(menu,mH) <- liftIO (newMenuHandle mDef index menuId osMenuBar)
				(_,itemHs,keys)	<- liftIO (createMenuElements osMenuBar menu 1 itemHs keys)				
				let it1 = Map.insert menuId (IdParent{idpIOId=ioId,idpDevice=MenuDevice,idpId=menuId}) it
				return (True,mH{mItems=itemHs},mHs{mKeys=keys},it1)
			Nothing	     -> return (False,undefined,mHs,it))
		
			

isMenuWithThisId :: Id -> MenuStateHandle ps -> Bool
isMenuWithThisId id msH = id == menuStateHandleGetMenuId msH

{-	creating pop up menus.
	It is assumed that MenuHandles contains no pop up menu in mMenus and that mPopUpId contains an Id.
-}
createPopUpMenu :: PopUpMenuElements m => SystemId -> ls -> PopUpMenu m ls ps -> MenuHandles ps -> IdTable -> OSMenuBar -> GUI ps (Bool,MenuHandles ps,IdTable)
createPopUpMenu ioId ls (PopUpMenu items) mHs@(MenuHandles {mMenus=mMenus, mKeys=keys, mPopUpId=mPopUpId}) it osMenuBar = do
    ms <- popUpMenuElementToHandles items
    let itemHs = map menuElementStateToMenuElementHandle ms
    let menuId = fromJust mPopUpId
    (case menuIdsAreConsistent ioId menuId itemHs it of
       Just it -> do
  	    menu <- liftIO (osCreatePopUpMenu)
	    (_,itemHs,keys) <- liftIO (createMenuElements osMenuBar menu 1 itemHs keys)
	    let mH = MenuHandle
			{ mHandle	= menu
			, mMenuId	= menuId
			, mTitle	= ""
			, mSelect	= True
			, mItems	= map validatePopUpMenuFunction itemHs
			}
	    let msH = MenuStateHandle (MenuLSHandle {mlsState=ls,mlsHandle=mH})
	    let mHs1 = mHs{mMenus=msH:mMenus, mKeys=keys, mPopUpId=Nothing}
	    return (True,mHs1,it)
       Nothing 	-> return (False,mHs,it))
    where
{-	validatePopUpMenuFunction takes care that all Menu(Mods)Function arguments of the elements
	apply closePopUpMenu after their own action.
-}
	validatePopUpMenuFunction :: MenuElementHandle ls ps -> MenuElementHandle ls ps
	validatePopUpMenuFunction itemH@(MenuItemHandle {mItemAtts=mItemAtts}) =
	    itemH{mItemAtts=map validateMenuFunction mItemAtts}
	    where
		validateMenuFunction :: MenuAttribute ls ps -> MenuAttribute ls ps
		validateMenuFunction (MenuFunction f) = MenuFunction (f' f)
		    where
			f' :: GUIFun ls ps -> GUIFun ls ps
			f' f ls_ps = do
				ls_ps1 <- f ls_ps
				closePopUpMenu'
				return ls_ps1
		validateMenuFunction (MenuModsFunction f) = MenuModsFunction (f' f)
		    where
			f' :: ModifiersFunction ls ps -> ModifiersFunction ls ps
			f' f modifiers ls_ps = do
				ls_ps1 <- f modifiers ls_ps
				closePopUpMenu'
				return ls_ps1
		validateMenuFunction att = att
	validatePopUpMenuFunction (MenuReceiverHandle _ _) =
		menuCreateFatalError "validatePopUpMenuFunction" "Receiver(2) should not be an element of PopUpMenus"
	validatePopUpMenuFunction (SubMenuHandle {}) =
		menuCreateFatalError "validatePopUpMenuFunction" "SubMenu should not be an element of PopUpMenus"
	validatePopUpMenuFunction itemH@(RadioMenuHandle {mRadioItems=itemHs}) =
		itemH{mRadioItems=map validatePopUpMenuFunction itemHs}
	validatePopUpMenuFunction itemH@(MenuSeparatorHandle {}) = itemH
	validatePopUpMenuFunction (MenuListLSHandle itemHs) =
		MenuListLSHandle (map validatePopUpMenuFunction itemHs)
	validatePopUpMenuFunction (MenuExtendLSHandle exLS itemHs) =
		MenuExtendLSHandle exLS (map validatePopUpMenuFunction itemHs)
	validatePopUpMenuFunction (MenuChangeLSHandle chLS itemHs) =
		MenuChangeLSHandle chLS (map validatePopUpMenuFunction itemHs)
	
{-	closePopUpMenu' takes care that the internal administration of the menus is restored again to
	no open pop up menu. It is assumed that all resources have been freed.
-}
	closePopUpMenu' :: GUI ps ()
	closePopUpMenu' = do
		(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
		unless found (menuCreateFatalError "closePopUpMenu" "could not retrieve MenuSystemState")
		let mHs = menuSystemStateGetMenuHandles mDevice
		let mHs1 = closePopUpMenu mHs
		appIOEnv (ioStSetDevice (MenuSystemState mHs1))


{-	Creating menu elements: retrieving toolbox handles and ids for elements, and building the menu gui. -}

createMenuElements :: OSMenuBar -> OSMenu -> Int -> [MenuElementHandle ls ps] -> [Char] -> IO (Int,[MenuElementHandle ls ps],[Char])
createMenuElements osMenubar menu iNr []             keys = return (iNr,[],keys)
createMenuElements osMenubar menu iNr (itemH:itemHs) keys = do
    (iNr,itemH, keys)	<- createMenuElement  osMenubar menu iNr itemH  keys
    (iNr,itemHs,keys)	<- createMenuElements osMenubar menu iNr itemHs keys
    return (iNr,itemH:itemHs,keys)
    where
	createMenuElement :: OSMenuBar -> OSMenu -> Int -> MenuElementHandle ls ps -> [Char] -> IO (Int, MenuElementHandle ls ps, [Char])
	createMenuElement osMenubar menu iNr itemH@(SubMenuHandle {mSubItems=itemHs}) keys = do
		itemH <- newSubMenuHandle itemH iNr menu		
		(_,itemHs,keys)	<- createMenuElements osMenubar (mSubHandle itemH) 1 itemHs keys
		return (iNr+1,itemH{mSubItems=itemHs},keys)
	createMenuElement osMenubar menu iNr itemH@(RadioMenuHandle {mRadioItems=itemHs}) keys = do
		(iNr,itemHs,keys) <- createMenuElements osMenubar menu iNr itemHs keys
		return (iNr,itemH{mRadioItems=itemHs},keys)
	createMenuElement osMenubar menu iNr itemH@(MenuItemHandle {}) keys = do
		let (itemH1,keys1) = checkShortcutKey itemH keys
		osMenuItem <- insertMenu osMenubar menu iNr itemH1
		return (iNr+1,itemH1{mOSMenuItem=osMenuItem},keys1)
	createMenuElement osMenubar menu iNr itemH@(MenuSeparatorHandle {}) keys = do
		osMenuItem <- osMenuSeparatorInsert menu iNr
		return (iNr+1,itemH{mOSMenuSeparator=osMenuItem},keys)
	createMenuElement _ _ iNr itemH@(MenuReceiverHandle _ _) keys = do
		return (iNr,itemH,keys)
	createMenuElement osMenubar menu iNr (MenuListLSHandle itemHs) keys = do
		(iNr,itemHs,keys) <- createMenuElements osMenubar menu iNr itemHs keys
		return (iNr,MenuListLSHandle itemHs,keys)
	createMenuElement osMenubar menu iNr (MenuExtendLSHandle exLS itemHs) keys = do
		(iNr,itemHs,keys) <- createMenuElements osMenubar menu iNr itemHs keys
		return (iNr,MenuExtendLSHandle exLS itemHs,keys)
	createMenuElement osmenubar menu iNr (MenuChangeLSHandle chLS itemHs) keys = do
		(iNr,itemHs,keys) <- createMenuElements osMenubar menu iNr itemHs keys
		return (iNr,MenuChangeLSHandle chLS itemHs,keys)	


{-	Extend an existing menu with new menu elements. -}

extendMenu :: OSMenuBar -> OSMenu -> Int -> [MenuElementHandle ls ps] -> [MenuElementHandle ls ps] -> [Char] -> IO ([MenuElementHandle ls ps],[Char])
extendMenu osMenubar menu iNr [] items keys = return (items,keys)
extendMenu osMenubar menu iNr (itemH:itemHs) items keys = do    
    (itemH, keys) <- extendMenu' osMenubar menu iNr itemH        keys
    (itemHs,keys) <- extendMenu  osMenubar menu iNr itemHs items keys
    return (itemH:itemHs,keys)
    where
	extendMenu' :: OSMenuBar -> OSMenu -> Int -> MenuElementHandle ls ps -> [Char] -> IO (MenuElementHandle ls ps, [Char])
	extendMenu' osMenubar menu iNr itemH@(SubMenuHandle {mSubHandle=handle, mSubItems=itemHs}) keys = do
		itemH <- newSubMenuHandle itemH iNr menu
		(_,itemHs,keys)	<- createMenuElements osMenubar handle 1 itemHs keys
		return (itemH{mSubItems=itemHs},keys)
	extendMenu' osMenubar menu iNr itemH@(RadioMenuHandle {mRadioItems=itemHs}) keys = do
		(_,itemHs,keys) <- createMenuElements osMenubar menu iNr itemHs keys
		return (itemH{mRadioItems=itemHs},keys)
	extendMenu' osmenubar menu iNr itemH@(MenuItemHandle {}) keys = do
		let (itemH1,keys1) = checkShortcutKey itemH keys
		osMenuItem <- insertMenu osMenubar menu iNr itemH1
		return (itemH1{mOSMenuItem=osMenuItem},keys1)
	extendMenu' osMenubar menu iNr itemH@(MenuSeparatorHandle {}) keys = do
		osMenuItem <- osMenuSeparatorInsert menu iNr
		return (itemH{mOSMenuSeparator=osMenuItem},keys)
	extendMenu' _ _ _ itemH@(MenuReceiverHandle _ _) keys =
		return (itemH,keys)
	extendMenu' osMenubar menu iNr (MenuListLSHandle itemHs) keys = do
		(itemHs,keys) <- extendMenu osMenubar menu iNr itemHs [] keys
		return (MenuListLSHandle itemHs,keys)
	extendMenu' osMenubar menu iNr (MenuExtendLSHandle exLS itemHs) keys = do
		(itemHs,keys) <- extendMenu osMenubar menu iNr itemHs [] keys
		return (MenuExtendLSHandle exLS itemHs,keys)
	extendMenu' osMenubar menu iNr (MenuChangeLSHandle chLS itemHs) keys = do
		(itemHs,keys) <- extendMenu osMenubar menu iNr itemHs [] keys
		return (MenuChangeLSHandle chLS itemHs,keys)

insertMenu :: OSMenuBar -> OSMenu -> Int -> MenuElementHandle ls ps -> IO OSMenuItem
insertMenu osMenubar menu iNr (MenuItemHandle {mItemKey=mItemKey,mItemTitle=mItemTitle,mItemSelect=mItemSelect,mItemMark=mItemMark,mItemAtts=mItemAtts}) = do
	osMenuItemInsert osMenubar menu iNr mItemTitle mItemSelect mItemMark shortcut
	where
		shortcut = case mItemKey of
			(Just key)	-> key
			_		-> '\0'

checkShortcutKey :: MenuElementHandle ls ps -> [Char] -> (MenuElementHandle ls ps,[Char])
checkShortcutKey mItemH cs = case mItemKey mItemH of
	Just c 	-> if c `elem` cs then (mItemH{mItemKey=Nothing},cs)
		   else (mItemH,c:cs)
	Nothing	-> (mItemH,cs)

--	Creation and manipulation of Menu(Element)Handles:

systemAble	= True
systemUnable	= False

--	Initialisation and Allocation:

newMenuHandle :: Menu m ls ps -> Int -> Id -> OSMenuBar -> IO (OSMenu,MenuHandle ls ps)
newMenuHandle mDef index menuId menuBar =
	let
		select = menuDefGetSelectState mDef
		title  = menuDefGetTitle mDef
	in do
		let able = enabled select
		menu <- osMenuInsert menuBar index title able
		let mH = MenuHandle
			{ mHandle   = menu
			, mMenuId   = menuId
			, mTitle    = title
			, mSelect   = able
			, mItems    = []
			}
		return (menu,mH)
	
newSubMenuHandle :: MenuElementHandle ls ps -> Int -> OSMenu -> IO (MenuElementHandle ls ps)
newSubMenuHandle mH@(SubMenuHandle {mSubTitle=mSubTitle,mSubSelect=able}) index menu = do	
	osH <- osSubMenuInsert menu index mSubTitle able
	return mH{mSubHandle=osH}

closePopUpMenu :: MenuHandles ps -> MenuHandles ps
closePopUpMenu mHs@(MenuHandles {mMenus=mMenus,mPopUpId=mPopUpId})
	| isJust mPopUpId = mHs
	| otherwise =
		let (msH:msHs) = mMenus		    
		in mHs{mMenus=msHs,mPopUpId=Just (menuStateHandleGetMenuId msH)}

disposeMenuItemHandle :: OSMenu -> Int -> MenuElementHandle ls ps ->  ([Char],IdTable) -> IO ([Char],IdTable)
disposeMenuItemHandle menu iNr (MenuItemHandle {mItemKey=mItemKey,mItemId=mItemId,mOSMenuItem=mOSMenuItem}) (keys,it) = do
	osMenuRemoveItem mOSMenuItem menu
	return (keys',it')
	where
		keys'	= case mItemKey of
			Just key -> key:keys
			Nothing  -> keys		
		it'	= case mItemId of
			Just id -> Map.delete (fromJust mItemId) it
			Nothing	-> it


disposeSubMenuHandles :: OSWindowPtr -> MenuElementHandle ls ps -> [Char] -> IO [Char]
disposeSubMenuHandles framePtr (MenuItemHandle {mItemKey=mItemKey,mOSMenuItem=mOSMenuItem}) keys =
	case mItemKey of
		Just key -> do
			osRemoveMenuShortKey framePtr mOSMenuItem
			return (filter ((==) key) keys)
		Nothing -> return keys
disposeSubMenuHandles framePtr (SubMenuHandle     {mSubItems=items}) keys = foldrM (disposeSubMenuHandles framePtr) keys items
disposeSubMenuHandles framePtr (RadioMenuHandle {mRadioItems=items}) keys = foldrM (disposeSubMenuHandles framePtr) keys items
disposeSubMenuHandles framePtr (MenuListLSHandle	      items) keys = foldrM (disposeSubMenuHandles framePtr) keys items
disposeSubMenuHandles framePtr (MenuExtendLSHandle _          items) keys = foldrM (disposeSubMenuHandles framePtr) keys items
disposeSubMenuHandles framePtr (MenuChangeLSHandle _          items) keys = foldrM (disposeSubMenuHandles framePtr) keys items
disposeSubMenuHandles _        _                                     keys = return keys

disposeMenuIds :: SystemId -> MenuElementHandle ls ps -> IdTable -> IdTable
disposeMenuIds pid (MenuItemHandle {mItemId=mItemId}) it
	| isNothing mItemId	= it
	| otherwise		= Map.delete (fromJust mItemId) it
disposeMenuIds pid (MenuReceiverHandle (ReceiverHandle {rId=rId}) _) it =
	Map.delete rId it
disposeMenuIds pid (SubMenuHandle {mSubItems=mSubItems}) it =
	foldr (disposeMenuIds pid) it mSubItems
disposeMenuIds pid (RadioMenuHandle {mRadioId=mRadioId,mRadioItems=mRadioItems}) it =
	let it1 = foldr (disposeMenuIds pid) it mRadioItems
	in maybe it1 (flip Map.delete it1) mRadioId
disposeMenuIds pid (MenuSeparatorHandle {mSepId=mSepId}) it =
	maybe it (flip Map.delete it) mSepId
disposeMenuIds pid (MenuListLSHandle mListItems) ts =
	foldr (disposeMenuIds pid) ts mListItems
disposeMenuIds pid (MenuExtendLSHandle _ mExtendItems) ts =
	foldr (disposeMenuIds pid) ts mExtendItems
disposeMenuIds pid (MenuChangeLSHandle _ mChangeItems) ts =
	foldr (disposeMenuIds pid) ts mChangeItems

disposeMenuHandles :: MenuHandles ps -> OSMenuBar -> IO ()
disposeMenuHandles menus@(MenuHandles {mMenus=mMenus}) osMenuBar =
	mapM_ dispose mMenus
	where
		dispose :: MenuStateHandle ps -> IO ()
		dispose mH = osMenuRemove (menuStateHandleGetHandle mH) osMenuBar
