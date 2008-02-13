-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Menu.Internal
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The actual implementation of most of the StdMenu functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Menu.Internal
		( enableMenus, disableMenus, setSelectMenus
		, closeMenuElements, closeMenuIndexElements, closeMenu
		, changeMenuSystemState, accessMenuSystemState
		, setMenuTitle
		) where


import	Graphics.UI.ObjectIO.Device.SystemState(menuSystemStateGetMenuHandles)
import	Graphics.UI.ObjectIO.Process.IOState
import	Graphics.UI.ObjectIO.Menu.Access
import	Graphics.UI.ObjectIO.Menu.Handle
import	Graphics.UI.ObjectIO.Menu.Items
import	Graphics.UI.ObjectIO.Window.SDISize
import  Graphics.UI.ObjectIO.CommonDef(dumpFatalError, foldrM, removeCheck, remove)
import  Graphics.UI.ObjectIO.Menu.Create(disposeMenuIds, disposeSubMenuHandles)
import  Graphics.UI.ObjectIO.Id(Id(..))
import  Graphics.UI.ObjectIO.StdIOCommon
import	Graphics.UI.ObjectIO.OS.Menu
import	Graphics.UI.ObjectIO.OS.MenuEvent(menuHandlesGetMenuStateHandles)
import	Graphics.UI.ObjectIO.OS.Types(osNoWindowPtr)
import  Control.Monad(when)
import  qualified Data.Map as Map

type DeltaMenuSystem    ps = OSMenuBar -> MenuHandles ps -> IO (MenuHandles ps)
type AccessMenuSystem x ps = OSMenuBar -> MenuHandles ps -> IO (x, MenuHandles ps)

menuInternalFatalError :: String -> String -> x
menuInternalFatalError function error
	= dumpFatalError function "menuinternal" error


--	General rules to access MenuHandles:

changeMenuSystemState :: Bool -> DeltaMenuSystem ps -> GUI ps ()
changeMenuSystemState redrawMenus f = do
	(found,mDevice) <- accIOEnv (ioStGetDevice MenuDevice)
	(if not found then return ()
	 else do
	        osdinfo <- accIOEnv ioStGetOSDInfo
	        (case getOSDInfoOSMenuBar osdinfo of
	        	Nothing -> menuInternalFatalError "changeMenuSystemState" "could not retrieve OSMenuBar from OSDInfo"
			Just osMenuBar -> do
				newMenus <- liftIO (f osMenuBar (menuSystemStateGetMenuHandles mDevice))
				liftIO (when redrawMenus (drawMenuBar osMenuBar))
				appIOEnv (ioStSetDevice (MenuSystemState newMenus))))
	

accessMenuSystemState :: Bool -> AccessMenuSystem x ps -> GUI ps (Maybe x)
accessMenuSystemState redrawMenus f = do
	(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
	(if not found then return Nothing
	 else do
			osdinfo <- accIOEnv ioStGetOSDInfo
			(case getOSDInfoOSMenuBar osdinfo of
				Nothing -> menuInternalFatalError "accessMenuSystemState" "could not retrieve OSMenuBar from OSDInfo"
				Just osMenuBar -> do
					(x,newMenus) <- liftIO (f osMenuBar (menuSystemStateGetMenuHandles mDevice))
					liftIO (when redrawMenus (drawMenuBar osMenuBar))
					appIOEnv (ioStSetDevice (MenuSystemState newMenus))
					return (Just x)))


{-	Closing a menu.
	Because in a SDI process menus might reside in the process window, the ViewFrame of the
	process window can change size.
	In that case, the layout of the controls should be recalculated, and the window updated.
-}

closeMenu :: Id -> GUI ps ()
closeMenu id = do
	osdInfo <- accIOEnv ioStGetOSDInfo
	(if getOSDInfoDocumentInterface osdInfo==NDI
	 then return ()
	 else
			case getOSDInfoOSMenuBar osdInfo of
				Nothing -> menuInternalFatalError "closeMenu" "could not retrieve OSMenuBar from OSDInfo"
				Just osMenuBar  -> do
						(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
						(if not found then return ()
						 else
							let
								mHs	= menuSystemStateGetMenuHandles mDevice
								(menus,mHs1) = menuHandlesGetMenuStateHandles mHs
								(found,mH,menus1)	= remove (isMenuWithThisId id) undefined menus
							in 
								if not found then do
									appIOEnv (ioStSetDevice (MenuSystemState mHs{mMenus=menus1}))
								else do
										(sdiSize1,sdiPtr) <- getSDIWindowSize
										keys <- liftIO (closeSubMenus osdInfo mH (mKeys mHs))
										it <- ioStGetIdTable
										ioid <- accIOEnv ioStGetIOId
										ioStSetIdTable (closeMenuIds ioid mH (Map.delete id it))
										liftIO (osMenuRemove (menuStateHandleGetHandle mH) osMenuBar >>
											drawMenuBar osMenuBar)
										let mHs1 = mHs{mMenus=menus1,mKeys=keys}
										appIOEnv (ioStSetDevice (MenuSystemState mHs1))
										(sdiSize2,_) <- getSDIWindowSize
										when (sdiSize1/=sdiSize2) (resizeSDIWindow sdiPtr sdiSize1 sdiSize2)))
	where
		isMenuWithThisId :: Id -> MenuStateHandle ps -> Bool
		isMenuWithThisId id msH = (id == menuStateHandleGetMenuId msH)
			

closeSubMenus :: OSDInfo -> MenuStateHandle ps -> [Char] -> IO [Char]
closeSubMenus osdInfo (MenuStateHandle (MenuLSHandle {mlsHandle=mH})) keys =
	foldrM (disposeSubMenuHandles framePtr) keys (mItems mH)
	where
		framePtr	= case (getOSDInfoOSInfo osdInfo) of
			Just info -> osFrame info
			_         -> osNoWindowPtr

closeMenuIds :: SystemId -> MenuStateHandle ps -> IdTable -> IdTable
closeMenuIds pid (MenuStateHandle (MenuLSHandle {mlsHandle=mH})) it =
	foldr (disposeMenuIds pid) it (mItems mH)


--	Enabling and Disabling of Menus:

enableMenus  :: [Id] -> GUI ps ()
enableMenus  ids = changeMenuSystemState True (setSelectMenus ids Able)

disableMenus :: [Id] -> GUI ps ()
disableMenus ids = changeMenuSystemState True (setSelectMenus ids Unable)

setSelectMenus :: [Id] -> SelectState -> OSMenuBar -> MenuHandles ps -> IO (MenuHandles ps)
setSelectMenus ids select osMenuBar menus@(MenuHandles {mEnabled=mEnabled,mMenus=mMenus}) = do
	(_,msHs) <- setSelectMenuHandles select osMenuBar mEnabled ids mMenus
	return (menus{mMenus=msHs})
	where	
		setSelectMenuHandles :: SelectState -> OSMenuBar -> Bool -> [Id] -> [MenuStateHandle ps] -> IO ([Id],[MenuStateHandle ps])
		setSelectMenuHandles select osMenuBar systemAble ids [] =
			return (ids,[])
		setSelectMenuHandles select osMenuBar systemAble ids (msH:msHs)
			| null ids = return (ids,msHs)
			| otherwise = do				
				(ids,msH)  <- setSelectMenuHandle  select osMenuBar systemAble ids msH
				(ids,msHs) <- setSelectMenuHandles select osMenuBar systemAble ids msHs
				return (ids,msH:msHs)
			where
				setSelectMenuHandle :: SelectState -> OSMenuBar -> Bool -> [Id] -> MenuStateHandle ps -> IO ([Id], MenuStateHandle ps)
				setSelectMenuHandle select osMenuBar systemAble ids msH@(MenuStateHandle mlsH@(MenuLSHandle {mlsHandle=mH@(MenuHandle {mMenuId=mMenuId,mHandle=mHandle})})) =
					let 
						(containsId,ids1) = removeCheck mMenuId ids
						msH1 = MenuStateHandle mlsH{mlsHandle=mH{mSelect=enabled select}}
					in
						if not containsId then return (ids1,msH)
						else
							if not systemAble then return (ids1,msH1)
							else do								
								(if enabled select then osEnableMenu else osDisableMenu) mHandle osMenuBar
								return (ids1,msH1)

--	Removing menu elements from (sub/radio)menus:

closeMenuElements :: Id -> [Id] -> GUI ps ()
closeMenuElements mId ids = do
	pid <- accIOEnv ioStGetIOId
	it <- ioStGetIdTable
	osdInfo <- accIOEnv ioStGetOSDInfo
	result <- accessMenuSystemState True (removeMenusItems osdInfo mId ids pid it)
	maybe (return ()) ioStSetIdTable result


--	Removing menu elements from (sub/radio)menus by index (counting from 1):

closeMenuIndexElements :: Bool -> SystemId -> (Id,Maybe Id) -> [Index] -> GUI ps ()
closeMenuIndexElements fromRadioMenu pid loc indices = do
	it <- ioStGetIdTable
	osdInfo <- accIOEnv ioStGetOSDInfo
	result <- accessMenuSystemState True (removeMenusIndexItems osdInfo fromRadioMenu loc indices pid it)
	maybe (return ()) ioStSetIdTable result


--	Set & Get the title of a menu.

setMenuTitle :: Id -> Title -> GUI ps ()
setMenuTitle id title = 
	changeMenuSystemState True (setOSMenuTitle id title)
	where
		setOSMenuTitle :: Id -> Title -> OSMenuBar -> MenuHandles ps -> IO (MenuHandles ps)
		setOSMenuTitle id title osMenuBar menus@(MenuHandles {mMenus=msHs}) = do
			msHs <- setOSMenusTitle id title osMenuBar msHs
			return (menus{mMenus=msHs})
			where
				setOSMenusTitle :: Id -> Title -> OSMenuBar -> [MenuStateHandle ps] -> IO [MenuStateHandle ps]
				setOSMenusTitle id title osMenuBar (msH:msHs)
					| id == menuStateHandleGetMenuId msH = do					
						osChangeMenuTitle osMenuBar (menuStateHandleGetHandle msH) title
						return (menuStateHandleSetTitle title msH:msHs)
					| otherwise = do
						msHs <- setOSMenusTitle id title osMenuBar msHs
						return (msH:msHs)
				setOSMenusTitle _ _ _ msHs = return msHs
