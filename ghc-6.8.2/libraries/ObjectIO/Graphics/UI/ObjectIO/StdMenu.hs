-----------------------------------------------------------------------------
-- |
-- Module      :  StdMenu
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdMenu defines functions on menus.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdMenu
		(
		-- * Opening a menu.
		  Menus(..),
		  
		-- * Closing a menu.
	      	  closeMenu,
	      	  
	      	-- * Enabling and disabling of the MenuSystem
	      	  enableMenuSystem
	      	, disableMenuSystem,
	      	
	      	-- * Enabling and disabling of Menus
	      	  enableMenus
	      	, disableMenus,
	      	
	      	-- * Get the 'SelectState' of a menu
	      	  getMenuSelectState,
	      	  
	      	-- * Adding menu elements to (sub\/radio)menus
	      	  openMenuElements
	      	, openSubMenuElements
	      	, openRadioMenuItems,
	      	
	      	-- * Removing menu elements from (sub\/radio)menus
	      	  closeMenuElements
	      	, closeMenuIndexElements
	      	, closeSubMenuIndexElements
	      	, closeRadioMenuIndexElements,
	      	
	      	-- * Determine the Ids of all menus.
	      	  getMenus,
	      	  
	      	-- * Determine the index position of a menu.
	      	  getMenuPos,
	      	  
	      	-- * Set and get the title of a menu.
	      	  setMenuTitle
	      	, getMenuTitle,
	      	
	      	-- * A visible module
	      	  module Graphics.UI.ObjectIO.StdMenuDef) where



import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.Process.IOState
import	Graphics.UI.ObjectIO.Id
import	Graphics.UI.ObjectIO.StdId(openId)
import	Graphics.UI.ObjectIO.Menu.Create
import	Graphics.UI.ObjectIO.Menu.Device
import qualified Graphics.UI.ObjectIO.Menu.Internal as I
import	Graphics.UI.ObjectIO.Menu.Handle
import	Graphics.UI.ObjectIO.Menu.Items
import  Graphics.UI.ObjectIO.StdMenuElementClass
import  Graphics.UI.ObjectIO.StdMenuDef
import	Graphics.UI.ObjectIO.Device.SystemState(windowSystemStateGetWindowHandles)
import	Graphics.UI.ObjectIO.Menu.Access(menuStateHandleGetMenuId, menuStateHandleGetSelect, menuStateHandleGetTitle, menuStateHandleGetHandle)
import	Graphics.UI.ObjectIO.Menu.DefAccess(menuDefGetMenuId)
import	Graphics.UI.ObjectIO.Device.SystemState(menuSystemStateGetMenuHandles)
import	Graphics.UI.ObjectIO.Window.Access(getWindowHandlesActiveModalDialog)
import  Graphics.UI.ObjectIO.OS.Menu(osTrackPopUpMenu, osEnableMenu, osDisableMenu)
import	Graphics.UI.ObjectIO.OS.MenuEvent(menuHandlesGetMenuStateHandles)
import  Control.Monad(unless)
import  qualified Data.Map as Map

stdMenuFatalError :: String -> String -> x
stdMenuFatalError function error =
	dumpFatalError function "StdMenu" error


--	General rules to access MenuHandles:


accessMenuHandles :: Id -> (MenuStateHandle ps -> x) -> GUI ps (Maybe x)
accessMenuHandles id f = do
	(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
	(if not found then return Nothing
	 else do return (accessmenuhandles id f (mMenus (menuSystemStateGetMenuHandles mDevice))))
	where
		accessmenuhandles :: Id -> (MenuStateHandle ps -> x) -> [MenuStateHandle ps] -> Maybe x
		accessmenuhandles id f [] = Nothing
		accessmenuhandles id f (mH:mHs)
			| id==menuStateHandleGetMenuId mH = Just (f mH)
			| otherwise = accessmenuhandles id f mHs


--	Opening a menu for an interactive process.

class Menus mdef where
	openMenu :: ls -> mdef ls ps -> ps -> GUI ps ps

instance MenuElements m => Menus (Menu m) where	
	openMenu ls mDef ps = do
		ps <- dOpen menuFunctions ps
		isZero <- checkZeroMenuBound
		(if isZero then throwGUI ErrorViolateDI
		 else do
			optMenuId <- validateMenuId (menuDefGetMenuId mDef)
			(case optMenuId of
				Just menuId -> openMenu' menuId ls mDef ps
				_ 	    -> throwGUI ErrorIdsInUse))
		where
			checkZeroMenuBound :: GUI ps Bool
			checkZeroMenuBound = do
				(found,mDevice) <- accIOEnv (ioStGetDevice MenuDevice)
				(if not found
				 then stdMenuFatalError "openMenu (Menu)" "could not retrieve MenuSystemState from IOSt"
				 else return (zeroBound (mNrMenuBound (menuSystemStateGetMenuHandles mDevice))))

			validateMenuId :: Maybe Id -> GUI ps (Maybe Id)
			validateMenuId Nothing = do
				mId <- openId
				return (Just mId)
			validateMenuId (Just id) = do
				idtable <- ioStGetIdTable
				return (if Map.member id idtable then Nothing else Just id)

instance PopUpMenuElements m => Menus (PopUpMenu m) where
	openMenu ls mDef ps = do
		osdInfo <- accIOEnv ioStGetOSDInfo
		(if getOSDInfoDocumentInterface osdInfo==NDI
		 then throwGUI ErrorViolateDI
		 else case getOSDInfoOSMenuBar osdInfo of
			Nothing -> stdMenuFatalError "openMenu (PopUpMen)" "OSMenuBar could not be retrieved from OSDInfo"
			Just osMenuBar -> do
				ps <- dOpen menuFunctions ps
				(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
				(if not found then throwGUI ErrorUnknownObject
				 else let 
					 mHs  = menuSystemStateGetMenuHandles mDevice
		  			 mHs1 = closePopUpMenu mHs
		  		      in do
		  		     		it <- ioStGetIdTable						
						ioid <- accIOEnv ioStGetIOId
						(ok,mHs2,it) <- createPopUpMenu ioid ls mDef mHs1 it osMenuBar
						ioStSetIdTable it
						appIOEnv (ioStSetDevice (MenuSystemState mHs2))		
						(if ok then handlePopUpMenu ps
						 else throwGUI ErrorIdsInUse)))
		where
		--	handlePopUpMenu opens the pop up menu.
			handlePopUpMenu :: ps -> GUI ps ps
			handlePopUpMenu ps = do
				osdInfo <- accIOEnv ioStGetOSDInfo
				let framePtr = case getOSDInfoOSInfo osdInfo of
					Just info -> osFrame info
					Nothing   -> stdMenuFatalError "openMenu (PopUpMenu)" "incorrect OSDInfo retrieved"
				(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
				unless found (stdMenuFatalError "openMenu (PopUpMenu)" "could not retrieve MenuSystemState from IOSt")
				let mHs	= menuSystemStateGetMenuHandles mDevice
				let ((popUpMenu:menus),mHs1) = menuHandlesGetMenuStateHandles mHs				
				let popUpId = menuStateHandleGetMenuId popUpMenu
				let mPtr = menuStateHandleGetHandle popUpMenu
				ok <- liftIO (osTrackPopUpMenu mPtr framePtr)
				(if not ok
				 then do
					appIOEnv (ioStSetDevice (MenuSystemState mHs1{mMenus=menus,mPopUpId=Just popUpId}))
					throwGUI (OtherError "PopUpMenu tracking error")
				 else do
					appIOEnv (ioStSetDevice (MenuSystemState mHs1{mMenus=popUpMenu:menus}))
					return ps)



--	Closing a menu.

-- | The function closes the specified menu.
closeMenu :: Id -> GUI ps ()
closeMenu id = I.closeMenu id

checkIdParent :: IdParent -> SystemId -> Bool
checkIdParent parent ioId = idpDevice parent==MenuDevice && idpIOId parent==ioId


--	Enabling and Disabling of the MenuSystem:

-- | The function enables interaction with menu device.
-- Enabling of the menu device doesn\'t affect the select
-- state of the individual menus.
enableMenuSystem :: GUI ps ()
enableMenuSystem = do
	isModal <- hasModalDialog
	di <- accIOEnv ioStGetDocumentInterface
	(if isModal || di==NDI then return ()
	 else I.changeMenuSystemState True (enableMenuSystem' di))
	where
		hasModalDialog :: GUI ps Bool
		hasModalDialog = do
			(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
			let windows	= windowSystemStateGetWindowHandles wDevice
			return (found && isJust (getWindowHandlesActiveModalDialog windows))

		enableMenuSystem' :: DocumentInterface -> OSMenuBar -> MenuHandles ps -> IO (MenuHandles ps)
		enableMenuSystem' di osMenuBar menus@(MenuHandles {mEnabled=mEnabled,mMenus=mMenus})
			| mEnabled = return menus
			| otherwise = do
				enableMenus mMenus osMenuBar
				return (menus{mEnabled=systemAble})
			where
				enableMenus :: [MenuStateHandle ps] -> OSMenuBar -> IO ()
				enableMenus [] osMenuBar = return ()
				enableMenus ((MenuStateHandle (MenuLSHandle {mlsHandle=m})):ms) osMenuBar = do
					osEnableMenu (mHandle m) osMenuBar
					enableMenus ms osMenuBar
						

-- | The function disables interaction with menu device.
-- Disabling of the menu device doesn\'t affect the select
-- state of the individual menus.
disableMenuSystem :: GUI ps ()
disableMenuSystem = do	
	di <- accIOEnv ioStGetDocumentInterface
	(if di==NDI then return ()
	 else I.changeMenuSystemState True (disableMenuSystem' di))
	where
		disableMenuSystem' :: DocumentInterface -> OSMenuBar -> MenuHandles ps -> IO (MenuHandles ps)
		disableMenuSystem' di osMenuBar menus@(MenuHandles {mEnabled=mEnabled,mMenus=mMenus})
			| mEnabled = return menus
			| otherwise = do
				disableMenus mMenus osMenuBar
				return (menus{mEnabled=systemUnable})
			where
				disableMenus :: [MenuStateHandle ps] -> OSMenuBar -> IO ()
				disableMenus [] osMenuBar = return ()
				disableMenus ((MenuStateHandle (MenuLSHandle {mlsHandle=m})):ms) osMenuBar = do
					osDisableMenu (mHandle m) osMenuBar
					disableMenus ms osMenuBar

--	Enabling and Disabling of Menus:

-- | The function enables the menus with the specified Ids.
-- When the menu and the corresponding menu device are both enabled
-- then the user can use the menu.
enableMenus :: [Id] -> GUI ps ()
enableMenus []  = return ()
enableMenus ids = I.enableMenus ids

-- | The function disables the menus with the specified Ids.
-- When the menu or the corresponding menu device are disabled
-- then the user cann\'t use the menu.
disableMenus :: [Id] -> GUI ps ()
disableMenus []  = return ()
disableMenus ids = I.disableMenus ids


--	Get the SelectState of a menu: 

getMenuSelectState :: Id -> GUI ps (Maybe SelectState)
getMenuSelectState id = do
	optSelect <- accessMenuHandles id menuStateHandleGetSelect
	return (fmap (\sel -> if sel then Able else Unable) optSelect)


{-	Adding menu elements to (sub/radio)menus:
		Items in a (sub/radio)menu are positioned starting from 1 and increasing by 1.
		Open with a position less than 1 adds the new elements in front
		Open with a position higher than the number of items adds the new elements to
		the end.
		Open an item on a position adds the item AFTER the item on that position.
-}

-- | dynamically creates additional elements to the specified menu.
openMenuElements :: MenuElements m => Id -> Index -> ls -> m ls ps -> GUI ps ()
openMenuElements mId pos ls new = do
	it   <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	(case Map.lookup mId it of
		Just parent | checkIdParent parent ioId && idpId parent == mId -> do
			(found,mDevice) <- accIOEnv (ioStGetDevice MenuDevice)
			unless found (throwGUI ErrorUnknownObject)
			osdInfo <- accIOEnv ioStGetOSDInfo
			(case getOSDInfoOSMenuBar osdInfo of
				Nothing -> stdMenuFatalError "openMenuElements" "OSMenuBar could not be retrieved from OSDInfo"
				Just osMenuBar -> do
					let menus = menuSystemStateGetMenuHandles mDevice
					(it,menus) <- addMenusItems (mId,Nothing) (max 0 pos) ls new ioId it menus osMenuBar					
					appIOEnv (ioStSetDevice (MenuSystemState menus))
					ioStSetIdTable it					
					return ())
	  	_ -> throwGUI ErrorUnknownObject)
	

-- | dynamically creates additional elements to the specified sub menu (here the Id is an Id of the sub menu).	
openSubMenuElements :: MenuElements m => Id -> Index -> ls -> m ls ps -> GUI ps ()
openSubMenuElements sId pos ls new = do
	it   <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	(case Map.lookup sId it of
		Just parent | checkIdParent parent ioId -> do
			(found,mDevice) <- accIOEnv (ioStGetDevice MenuDevice)
			unless found (throwGUI ErrorUnknownObject)
			osdInfo <- accIOEnv ioStGetOSDInfo
			(case getOSDInfoOSMenuBar osdInfo of
				Nothing -> stdMenuFatalError "openSubMenuElements" "OSMenuBar could not be retrieved from OSDInfo"
				Just osMenuBar -> do					
					let menus = menuSystemStateGetMenuHandles mDevice					
					(it,menus) <- addMenusItems (idpId parent,Just sId) (max 0 pos) ls new ioId it menus osMenuBar
					appIOEnv (ioStSetDevice (MenuSystemState menus))
					ioStSetIdTable it					
					return ())
		_ -> throwGUI ErrorUnknownObject)
	

-- | dynamically creates additional radio menu elements.
openRadioMenuItems :: Id -> Index -> [MenuRadioItem ps ps] -> GUI ps ()
openRadioMenuItems rId pos radioItems = do
	idtable <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	(case Map.lookup rId idtable of
		Just parent | checkIdParent parent ioId ->
			if null radioItems then return ()
			else
				let radioIds = filterMap (\(_,maybeId,_,_)->(isJust maybeId,fromJust maybeId)) radioItems
				in if not (okMembersIdTable radioIds idtable)
				   then throwGUI ErrorIdsInUse
				   else do
					   let mId	= idpId parent
					   I.changeMenuSystemState True (addMenuRadioItems (mId,rId) (max 0 pos) radioItems)
					   let  insMap id tbl = Map.insert id IdParent{idpIOId=ioId,idpDevice=MenuDevice,idpId=mId} tbl
					        idtable1 = foldr insMap idtable radioIds
					   ioStSetIdTable idtable1
		_ 	-> throwGUI ErrorUnknownObject)

--	Removing menu elements from (sub/radio)menus:

-- | closes the elements with specified Ids from the specified menu.
closeMenuElements :: Id -> [Id] -> GUI ps ()
closeMenuElements mId []  = return ()
closeMenuElements mId ids = I.closeMenuElements mId ids

--	Removing menu elements from (sub/radio)menus by index (counting from 1):

-- | closes the elements with specified indexes from the specified menu.
closeMenuIndexElements :: Id -> [Index] -> GUI ps ()
closeMenuIndexElements mId indices = do
	idtable <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	(case Map.lookup mId idtable of
		Just parent | checkIdParent parent ioId ->
			I.closeMenuIndexElements False ioId (mId,Nothing) indices
		_ -> return ())

-- | closes the elements with specified indexes from the specified sub menu.
closeSubMenuIndexElements :: Id -> [Index] -> GUI ps ()
closeSubMenuIndexElements sId indices = do
	idtable <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	(case Map.lookup sId idtable of
		Just parent | checkIdParent parent ioId ->
			I.closeMenuIndexElements False ioId (idpId parent,Just sId) indices
		_ -> return ())


-- | closes the radio menu elements with specified indexes from the specified menu.
closeRadioMenuIndexElements :: Id -> [Index] -> GUI ps ()
closeRadioMenuIndexElements rId indices = do
	idtable <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	(case Map.lookup rId idtable of
		Just parent | checkIdParent parent ioId ->
			I.closeMenuIndexElements True ioId (idpId parent,Just rId) indices
		_ -> return ())

--	Determine the Ids of all menus.

-- | returns the list of ids of all existing menus for the current process.
getMenus :: GUI ps [Id]
getMenus = do
	(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
	(if not found then return []
	 else let mHs	= menuSystemStateGetMenuHandles mDevice
	      in return (map menuStateHandleGetMenuId (mMenus mHs)))


--	Determine the index position of a menu.

-- | returns the menu item index from the item Id.
getMenuPos :: Id -> GUI ps (Maybe Index)
getMenuPos id = do
	(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
	(if not found then return Nothing
	 else let mHs	= menuSystemStateGetMenuHandles mDevice
	      in return (getMenuIndex id 0 (mMenus mHs)))
	where
		getMenuIndex :: Id -> Int -> [MenuStateHandle ps] -> Maybe Int
		getMenuIndex id index (mH:mHs)
			| id==menuStateHandleGetMenuId mH = Just index
			| otherwise = getMenuIndex id (index+1) mHs
		getmenuindex _ _ _ = Nothing


--	Set & Get the title of a menu.

-- | sets the menu title.
setMenuTitle :: Id -> Title -> GUI ps ()
setMenuTitle id title = I.setMenuTitle id title

-- | returns the menu title.
getMenuTitle :: Id -> GUI ps (Maybe Title)
getMenuTitle id = accessMenuHandles id menuStateHandleGetTitle
