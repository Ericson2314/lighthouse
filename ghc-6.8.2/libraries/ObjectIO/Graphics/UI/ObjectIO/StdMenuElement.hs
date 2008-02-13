-----------------------------------------------------------------------------
-- |
-- Module      :  StdMenuElement
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdMenuElement specifies all functions on menu elements.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdMenuElement
		( 
		-- * Enabling and disabling of menu elements
		  enableMenuElements, disableMenuElements
		, getMenuElementSelectStates, getMenuElementSelectState,
		-- * Marking and unmarking of MenuItems only
		  markMenuItems, unmarkMenuItems
		, getMenuElementMarkStates, getMenuElementMarkState, 
		-- * Changing the title of menu elements:
		  setMenuElementTitles, getMenuElementTitles
		, getMenuElementTitle,
		-- * Item selection
		  selectRadioMenuItem, selectRadioMenuIndexItem
		, getSelectedRadioMenuItems, getSelectedRadioMenuItem,
		-- * Short keys
		  getMenuElementShortKeys
		, getMenuElementShortKey
		) where


import	Graphics.UI.ObjectIO.Id
import	Graphics.UI.ObjectIO.CommonDef
import	Graphics.UI.ObjectIO.Process.IOState
import	Graphics.UI.ObjectIO.Menu.Access(menuStateHandleGetMenuId)
import	Graphics.UI.ObjectIO.Menu.Handle
import	Graphics.UI.ObjectIO.Device.SystemState(menuSystemStateGetMenuHandles)
import	Graphics.UI.ObjectIO.OS.Menu(drawMenuBar, osEnableMenuItem, osDisableMenuItem, osChangeMenuItemTitle, osMenuItemCheck)
import	Graphics.UI.ObjectIO.OS.Types(OSMenu)
import  Control.Monad(when)
import  Data.Char(toUpper)
import  qualified Data.Map as Map


stdMenuElementFatalError :: String -> String -> x
stdMenuElementFatalError function error = dumpFatalError function "StdMenuElement" error


--	The function isOkMenuElementId can be used to filter out the proper IdParent records.

isOkMenuElementId :: SystemId -> (x,Maybe IdParent) -> (Bool,(x,Id))
isOkMenuElementId ioId (x,Just (IdParent {idpIOId=idpIOId,idpDevice=idpDevice,idpId=idpId})) =
	(ioId==idpIOId && idpDevice==MenuDevice,(x,idpId))
isOkMenuElementId _ _ = (False,undefined)


--	Enabling and Disabling of menu elements:

enableMenuElements :: [Id] -> GUI ps ()
enableMenuElements ids = do
	it <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	let ids_mIds  = filterMap (isOkMenuElementId ioId) (map (\id -> (id,Map.lookup id it)) ids)
	let ids_mIds1 = gather ids_mIds
	(if null ids_mIds1 then return ()
	 else sequence [setMenu mId recurseAll setItemAbility (map (\id->(id,True)) ids) | (ids,mId)<-ids_mIds1] >> return ())

disableMenuElements :: [Id] -> GUI ps ()
disableMenuElements ids = do
	it <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	let ids_mIds  = filterMap (isOkMenuElementId ioId) (map (\id -> (id,Map.lookup id it)) ids)
	let ids_mIds1 = gather ids_mIds
	(if null ids_mIds1 then return ()
	 else sequence [setMenu mId recurseAll setItemAbility (map (\id->(id,False)) ids) | (ids,mId)<-ids_mIds1] >> return ())


setItemAbility :: Bool -> OSMenu -> Int -> MenuElementHandle ls ps -> IO (MenuElementHandle ls ps)

setItemAbility enabled menu itemNr itemH@(SubMenuHandle {mSubHandle=mSubHandle})
	| enabled	= osEnableMenuItem  menu mSubHandle >> return submenu
	| otherwise	= osDisableMenuItem menu mSubHandle >> return submenu
	where
		submenu		= itemH{mSubSelect=enabled}

setItemAbility enabled menu itemNr itemH@(RadioMenuHandle {mRadioItems=mRadioItems})
	| enabled	= enableAbleItems menu mRadioItems itemNr >> return radiomenu
	| otherwise	= disableAllItems menu mRadioItems itemNr >> return radiomenu
	where
		radiomenu = itemH{mRadioSelect=enabled}

		enableAbleItems :: OSMenu -> [MenuElementHandle ls ps] -> Int -> IO ()
		enableAbleItems menu (itemH:itemHs) itemNr = do
			enableAbleItems menu itemHs (itemNr+1)
			when (mItemSelect itemH) (osEnableMenuItem menu (mOSMenuItem itemH))
		enableAbleItems _ [] _ = return ()

		disableAllItems :: OSMenu -> [MenuElementHandle ls ps] -> Int -> IO ()
		disableAllItems menu itemHs itemNr =
			mapM_ (\itemH -> osDisableMenuItem menu (mOSMenuItem itemH)) itemHs

setItemAbility enabled menu itemNr itemH@(MenuItemHandle {mOSMenuItem=mOSMenuItem})
	| enabled	= osEnableMenuItem  menu mOSMenuItem >> return menuitem
	| otherwise	= osDisableMenuItem menu mOSMenuItem >> return menuitem
	where
		menuitem = itemH{mItemSelect=enabled}

setItemAbility _ _ _ itemH = return itemH


--	Marking and Unmarking of MenuItems only:

markMenuItems :: [Id] -> GUI ps ()
markMenuItems ids = do
	it <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	let ids_mIds = filterMap (isOkMenuElementId ioId) (map (\id -> (id,Map.lookup id it)) ids)
	let ids_mIds1 = gather ids_mIds
	(if null ids_mIds1 then return ()
	 else sequence [setMenu mId recurseSubMenuOnly setItemMark (map (\id->(id,True)) ids) | (ids,mId)<-ids_mIds1] >> return ())

unmarkMenuItems :: [Id] -> GUI ps ()
unmarkMenuItems ids = do
	it <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	let ids_mIds  = filterMap (isOkMenuElementId ioId) (map (\id -> (id,Map.lookup id it)) ids)
	let ids_mIds1 = gather ids_mIds
	(if null ids_mIds1 then return ()
	 else sequence [setMenu mId recurseSubMenuOnly setItemMark (map (\id->(id,False)) ids) | (ids,mId)<-ids_mIds1] >> return ())

setItemMark :: Bool -> OSMenu -> Int -> MenuElementHandle ls ps -> IO (MenuElementHandle ls ps)
setItemMark marked menu itemNr itemH@(MenuItemHandle {}) = do
	osMenuItemCheck marked menu (mOSMenuItem itemH)
	return (itemH{mItemMark=marked})
setItemMark _ _ _ itemH = return itemH


--	Changing the Title of menu elements:

setMenuElementTitles :: [(Id,Title)] -> GUI ps ()
setMenuElementTitles id_titles = do
	it <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	let id_titles_mIds  = filterMap (isOkMenuElementId ioId) (map (\id_t@(id,_) -> (id_t,Map.lookup id it)) id_titles)
	let id_titles_mIds1 = gather id_titles_mIds
	(if null id_titles_mIds1 then return ()
	 else sequence [setMenu mId recurseAll setItemTitle id_titles | (id_titles,mId)<-id_titles_mIds1] >> return ())
	where
		setItemTitle :: Title -> OSMenu -> Int -> MenuElementHandle ls ps -> IO (MenuElementHandle ls ps)
		setItemTitle title menu itemNr itemH@(SubMenuHandle {}) = do
			osChangeMenuItemTitle menu (mSubHandle itemH) title
			return (itemH{mSubTitle=title})
		setItemTitle title menu itemNr itemH@(MenuItemHandle {}) = do
			osChangeMenuItemTitle menu (mOSMenuItem itemH) title
			return (itemH{mItemTitle=title})
		setItemTitle _ _ _ itemH = return itemH


--	Changing the selected menu item of a RadioMenu by its Id:

selectRadioMenuItem :: Id -> Id -> GUI ps ()
selectRadioMenuItem id itemId = do
	it <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	let maybeParent	= Map.lookup id it
	(if not (fst (isOkMenuElementId ioId (id,maybeParent)))
	 then return ()
	 else setMenu (idpId (fromJust maybeParent)) recurseAll selectradiomenuitem [(id,itemId)])
	where
		selectradiomenuitem :: Id -> OSMenu -> Int -> MenuElementHandle ls ps -> IO (MenuElementHandle ls ps)
		selectradiomenuitem itemId menu itemNr radioH@(RadioMenuHandle {mRadioIndex=now,mRadioItems=itemHs})
			| null itemHs = return radioH
			| otherwise =
				let new	= getRadioMenuItemIndex itemId itemHs 1
				in if new == 0 then return radioH
				   else do
					(itemHs,_) <- stateMapM (setmark menu new) itemHs 1
					return (radioH{mRadioIndex=new,mRadioItems=itemHs})
			where
				getRadioMenuItemIndex :: Id -> [MenuElementHandle ls ps] -> Index -> Index
				getRadioMenuItemIndex id (MenuItemHandle {mItemId=mItemId}:itemHs) index
					| mItemId /= Just id = getRadioMenuItemIndex id itemHs (index+1)
					| otherwise	     = index
				getRadioMenuItemIndex _ [] _ = 0
		selectradiomenuitem _ _ _ itemH = return itemH

setmark :: OSMenu -> Index -> MenuElementHandle ls ps -> Int -> IO (MenuElementHandle ls ps,Int)
setmark menu new itemH@(MenuItemHandle {mOSMenuItem=mOSMenuItem}) index = do
	osMenuItemCheck marked menu mOSMenuItem
	return (itemH{mItemMark=marked},index+1)
	where
		marked	= index==new


--	Changing the selected menu item of a RadioMenu by its index:

selectRadioMenuIndexItem :: Id -> Index -> GUI ps ()
selectRadioMenuIndexItem id index = do
	it <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	let maybeParent	= Map.lookup id it
	(if not (fst (isOkMenuElementId ioId (id,maybeParent)))
	 then return ()
	 else setMenu (idpId (fromJust maybeParent)) recurseAll selectradiomenuindexitem [(id,index)])
	where
		selectradiomenuindexitem :: Index -> OSMenu -> Int -> MenuElementHandle ls ps -> IO (MenuElementHandle ls ps)
		selectradiomenuindexitem new menu itemNr radioH@(RadioMenuHandle {mRadioIndex=now,mRadioItems=itemHs})
			| null itemHs = return radioH
			| otherwise =
				let new	= setBetween new 1 (length itemHs)
				in if new==now then return radioH
				   else do
					(items,_) <- stateMapM (setmark menu new) itemHs 1
					return (radioH{mRadioIndex=new,mRadioItems=itemHs})
		selectradiomenuindexitem _ _ _ itemH = return itemH


--	Now do it!

type ItemChange x = (Id, x)
type Recurse = (Bool, Bool)
type DeltaItemHandle  ps x = forall ls . x -> OSMenu -> Int -> MenuElementHandle ls ps -> IO (MenuElementHandle ls ps)
type AccessItemHandle ps x = forall ls . MenuElementHandle ls ps -> x -> x

recurseAll		= (True, True )
recurseSubMenuOnly	= (True, False)
recurseRadioOnly	= (False,True )
recurseNone		= (False,False)

setMenu :: Id -> Recurse -> DeltaItemHandle ps x -> [ItemChange x] -> GUI ps ()
setMenu menuId recurse f changes = do
	ok <- accIOEnv (ioStHasDevice MenuDevice)
	(if not ok then return ()
	 else do
		osdinfo <- accIOEnv ioStGetOSDInfo
	  	(case getOSDInfoOSMenuBar osdinfo of
	  		Nothing -> stdMenuElementFatalError "setMenu" "OSMenuBar could not be retrieved from OSDInfo"
	  		Just osMenuBar -> do
	  			(found,menus) <- accIOEnv (ioStGetDevice MenuDevice)
				(if not found
				 then stdMenuElementFatalError "setMenu" "MenuSystemState could not be retrieved from IOSt"
				 else let
					  mHs = menuSystemStateGetMenuHandles menus
	  				  (found,msH) = cselect (eqMenuLSHandleId menuId) undefined (mMenus mHs)
	  			      in
					  if not found then return ()
					  else do
		  				msH <- liftIO (changeMenuItems f changes msH)
						liftIO (drawMenuBar osMenuBar)
		  				let (_,msHs) = creplace (eqMenuLSHandleId menuId) msH (mMenus mHs)
						appIOEnv (ioStSetDevice (MenuSystemState mHs{mMenus=msHs})))))
	where
		changeMenuItems :: DeltaItemHandle ps x -> [ItemChange x] -> MenuStateHandle ps -> IO (MenuStateHandle ps)
		changeMenuItems change changes (MenuStateHandle mlsH@(MenuLSHandle {mlsHandle=mH@(MenuHandle {mItems=itemHs})})) = do
			(_,itemHs,_) <- changeMenuItems' (mHandle mH) change changes itemHs 1
			return (MenuStateHandle mlsH{mlsHandle=mH{mItems=itemHs}})
			where
				changeMenuItems' :: OSMenu -> DeltaItemHandle ps x -> [ItemChange x] -> [MenuElementHandle ls ps] -> Int -> IO ([ItemChange x],[MenuElementHandle ls ps],Int)
				changeMenuItems' menu change changes []     itemNr = return (changes,[],itemNr)
				changeMenuItems' menu change changes (h:hs) itemNr
					| null changes = return (changes,(h:hs),itemNr)
					| otherwise    = do
						(changes,h, itemNr) <- changeMenuItem'  menu change changes h  itemNr
						(changes,hs,itemNr) <- changeMenuItems' menu change changes hs itemNr
						return (changes,(h:hs),itemNr)
					where
						changeMenuItem' :: OSMenu -> DeltaItemHandle ps x -> [ItemChange x] -> MenuElementHandle ls ps -> Int -> IO ([ItemChange x],MenuElementHandle ls ps,Int)
						changeMenuItem' menu change changes itemH@(MenuItemHandle {mItemId=mItemId}) itemNr
							| not anItem = return (changes,itemH,itemNr+1)
							| otherwise  = do
								itemH <- change x menu itemNr itemH
								return (changes1,itemH,itemNr+1)
							where
								(anItem,(_,x),changes1)	= case mItemId of
									Nothing	-> (False,(undefined,undefined),changes)
									Just id	-> remove (eqfst2id id) (id,undefined) changes

						changeMenuItem' menu change changes subH@(SubMenuHandle {mSubHandle=mSubHandle,mSubMenuId=mSubMenuId,mSubItems=mSubItems}) itemNr
							| not anItem && not (fst recurse) = return (changes,subH,itemNr+1)
							| otherwise = do
								(changes2,subItems,_) <- changeMenuItems' mSubHandle change changes1 mSubItems 1
								let subH1 = subH{mSubItems=subItems}
								(if not anItem then return (changes2,subH1,itemNr+1)
								 else do
									subH2 <- change x menu itemNr subH1
									return (changes2,subH2,itemNr+1))
							where
								(anItem,(_,x),changes1)	= case mSubMenuId of
									Nothing	-> (False,(undefined,undefined),changes)
									Just id	-> remove (eqfst2id id) (id,undefined) changes

						changeMenuItem' menu change changes radioH@(RadioMenuHandle {mRadioId=mRadioId,mRadioItems=mRadioItems}) itemNr
							| not anItem && not (snd recurse) = return (changes,radioH,itemNr+nrItems)
							| otherwise = do
								(changes2,items,_) <- changeMenuItems' menu change changes1 mRadioItems itemNr
								let radioH1 = radioH{mRadioItems=items}
								(if not anItem then return (changes2,radioH1,itemNr+nrItems)
								 else do
									radioH2 <- change x menu itemNr radioH1
									return (changes2,radioH2,itemNr+nrItems))
							where
								(anItem,(_,x),changes1)	= case mRadioId of
									Nothing	-> (False,(undefined,undefined),changes)
									Just id	-> remove (eqfst2id id) (id,undefined) changes
								nrItems	= length mRadioItems

						changeMenuItem' menu change changes (MenuListLSHandle items) itemNr = do
							(changes,items,itemNr) <- changeMenuItems' menu change changes items itemNr
							return (changes,MenuListLSHandle items,itemNr)

						changeMenuItem' menu change changes (MenuExtendLSHandle exLS items) itemNr = do
							(changes,items,itemNr) <- changeMenuItems' menu change changes items itemNr
							return (changes,MenuExtendLSHandle exLS items,itemNr)

						changeMenuItem' menu change changes (MenuChangeLSHandle chLS items) itemNr = do
							(changes,items,itemNr) <- changeMenuItems' menu change changes items itemNr
							return (changes,MenuChangeLSHandle chLS items,itemNr)

						changeMenuItem' _ _ changes h itemNr = return (changes,h,itemNr+1)


--	Read access operations on MState:

getMenu :: Id -> Recurse -> Cond x -> AccessItemHandle ps x -> x -> GUI ps x
getMenu menuId recurse cond f s = do
	ok <- accIOEnv (ioStHasDevice MenuDevice)
	(if not ok then throwGUI ErrorUnknownObject
	 else do
		(found,menus) <- accIOEnv (ioStGetDevice MenuDevice)
		(if not found
		 then stdMenuElementFatalError "getMenu" "MenuSystemState could not be retrieved from IOSt"
		 else let
			  mHs = menuSystemStateGetMenuHandles menus
			  (found,msH) = cselect (eqMenuLSHandleId menuId) undefined (mMenus mHs)
		      in
			  if not found then throwGUI ErrorUnknownObject
			  else return (statemapMenuStateHandle cond f msH s)))
	where
		statemapMenuStateHandle :: Cond x -> AccessItemHandle ps x -> MenuStateHandle ps -> x -> x
		statemapMenuStateHandle cond f (MenuStateHandle mlsH@(MenuLSHandle {mlsHandle=mH@(MenuHandle {mItems=itemHs})})) s =
			statemapMenuElementHandles cond f itemHs s
			where
				statemapMenuElementHandles :: Cond x -> AccessItemHandle ps x -> [MenuElementHandle ls ps] -> x -> x
				statemapMenuElementHandles cond f [] s = s
				statemapMenuElementHandles cond f (itemH:itemHs) s
					| cond s    = s
					| otherwise = statemapMenuElementHandles cond f itemHs (statemapMenuElementHandle cond f itemH s)
					where
						statemapMenuElementHandle :: Cond x -> AccessItemHandle ps x -> MenuElementHandle ls ps -> x -> x
						statemapMenuElementHandle cond f itemH@(MenuListLSHandle        itemHs) s = statemapMenuElementHandles cond f itemHs s
						statemapMenuElementHandle cond f itemH@(MenuExtendLSHandle exLS itemHs) s = statemapMenuElementHandles cond f itemHs s
						statemapMenuElementHandle cond f itemH@(MenuChangeLSHandle chLS itemHs) s = statemapMenuElementHandles cond f itemHs s
						statemapMenuElementHandle cond f itemH@(SubMenuHandle{mSubItems=itemHs}) s = 
							statemapMenuElementHandles cond f itemHs (if fst recurse then f itemH s else s)
						statemapMenuElementHandle cond f itemH@(RadioMenuHandle{mRadioItems=itemHs}) s =
							statemapMenuElementHandles cond f itemHs (if snd recurse then f itemH s else s)
						statemapMenuElementHandle cond f itemH			    	        s = f itemH s

eqMenuLSHandleId :: Id -> MenuStateHandle ps -> Bool
eqMenuLSHandleId id msH = id==menuStateHandleGetMenuId msH


getSelectedRadioMenuItems :: Id -> [Id] -> GUI ps [(Index,Maybe Id)]
getSelectedRadioMenuItems menuId ids = do
	r <- getMenu menuId recurseRadioOnly (null . fst) getradioitemselect (ids,map (\id->(id,0,Nothing)) ids)
	return (map snd3thd3 (snd r))
	where
		getradioitemselect :: MenuElementHandle ls ps -> ([Id],[(Id,Index,Maybe Id)]) -> ([Id],[(Id,Index,Maybe Id)])
		getradioitemselect itemH@(RadioMenuHandle {mRadioItems=itemHs,mRadioIndex=index}) (ids,selects)
			| not hadId	= (ids,selects)
			| otherwise	= (ids1,snd (creplace (eqfst3id id) (id,index,selectid) selects))
			where
				selectid = if index == 0 then Nothing else mItemId (itemHs !! (index-1))

				(hadId,id,ids1)	= case mRadioId itemH of
					Nothing	-> (False,undefined,ids)
					Just id	-> remove ((==) id) id ids

		getradioitemselect _ ids_selects = ids_selects

getSelectedRadioMenuItem :: Id -> Id -> GUI ps (Index,Maybe Id)
getSelectedRadioMenuItem menuId id = fmap head (getSelectedRadioMenuItems menuId [id])

getMenuElementSelectStates :: Id -> [Id] -> GUI ps [(Bool,SelectState)]
getMenuElementSelectStates menuId ids = do
	r <- getMenu menuId recurseAll (null . fst) getmenuitemselect (ids,map (\id->(id,False,Able)) ids)
	return (map snd3thd3 (snd r))
	where
		getmenuitemselect :: MenuElementHandle ls ps -> ([Id],[(Id,Bool,SelectState)]) -> ([Id],[(Id,Bool,SelectState)])
		getmenuitemselect itemH@(SubMenuHandle {}) (ids,selects)
			| not hadId	= (ids,selects)
			| otherwise	= (ids1,snd (creplace (eqfst3id id) (id,True,if mSubSelect itemH   then Able else Unable) selects))
			where
				(hadId,id,ids1)	= case mSubMenuId itemH of
					Nothing	-> (False,undefined,ids)
					Just id	-> remove ((==) id) id ids

		getmenuitemselect itemH@(RadioMenuHandle {}) (ids,selects)			  
			| not hadId	= (ids,selects)
			| otherwise	= (ids1,snd (creplace (eqfst3id id) (id,True,if mRadioSelect itemH then Able else Unable) selects))
			where
				(hadId,id,ids1)	= case mRadioId itemH of
					Nothing	-> (False,undefined,ids)
					Just id	-> remove ((==) id) id ids

		getmenuitemselect itemH@(MenuItemHandle {}) (ids,selects)
			| not hadId	= (ids,selects)
			| otherwise	= (ids1,snd (creplace (eqfst3id id) (id,True,if mItemSelect itemH  then Able else Unable) selects))
			where
				(hadId,id,ids1)	= case mItemId itemH of
					Nothing	-> (False,undefined,ids)
					Just id	-> remove ((==) id) id ids

		getmenuitemselect _ ids_selects = ids_selects

getMenuElementSelectState :: Id -> Id -> GUI ps (Bool,SelectState)
getMenuElementSelectState menuId id = fmap head (getMenuElementSelectStates menuId [id])


getMenuElementMarkStates :: Id -> [Id] -> GUI ps [(Bool,MarkState)]
getMenuElementMarkStates menuId ids = do
	r <- getMenu menuId recurseNone (null . fst) getmenuitemmark (ids,map (\id->(id,False,NoMark)) ids)
	return (map snd3thd3 (snd r))
	where
		getmenuitemmark :: MenuElementHandle ls ps -> ([Id],[(Id,Bool,MarkState)]) -> ([Id],[(Id,Bool,MarkState)])
		getmenuitemmark itemH@(MenuItemHandle {}) (ids,marks)			
			| not hadId	= (ids,marks)
			| otherwise	= (ids1,snd (creplace (eqfst3id id) (id,True,if mItemMark itemH then Mark else NoMark) marks))
			where
				(hadId,id,ids1)	= case mItemId itemH of
					Nothing	-> (False,undefined,ids)
					Just id	-> remove ((==) id) id ids
		getmenuitemmark _ ids_marks = ids_marks

getMenuElementMarkState :: Id -> Id -> GUI ps (Bool,MarkState)
getMenuElementMarkState menuId id = fmap head (getMenuElementMarkStates menuId [id])


getMenuElementTitles :: Id -> [Id] -> GUI ps [(Bool,Maybe String)]
getMenuElementTitles menuId ids = do
	r <- getMenu menuId recurseAll (null . fst) getmenuitemtitle (ids, map (\id->(id,False,Nothing)) ids)
	return (map snd3thd3 (snd r))
	where
		getmenuitemtitle :: MenuElementHandle ls ps -> ([Id],[(Id,Bool,Maybe String)]) -> ([Id],[(Id,Bool,Maybe String)])
		getmenuitemtitle itemH@(SubMenuHandle {}) (ids,titles)
			| not hadId	= (ids,titles)
			| otherwise	= (ids1,snd (creplace (eqfst3id id) (id,True,Just (mSubTitle itemH)) titles))
			where
				(hadId,id,ids1)	= case mSubMenuId itemH of
					Nothing	-> (False,undefined,ids)
					Just id	-> remove ((==) id) id ids

		getmenuitemtitle itemH@(RadioMenuHandle {}) (ids,titles)						  
			| not hadId	= (ids,titles)
			| otherwise	= (ids1,snd (creplace (eqfst3id id) (id,True,Nothing) titles))
			where
				(hadId,id,ids1)	= case mRadioId itemH of
					Nothing	-> (False,undefined,ids)
					Just id	-> remove ((==) id) id ids

		getmenuitemtitle itemH@(MenuItemHandle {}) (ids,titles)			
			| not hadId	= (ids,titles)
			| otherwise	= (ids1,snd (creplace (eqfst3id id) (id,True,Just (mItemTitle itemH)) titles))
			where
				(hadId,id,ids1)	= case mItemId itemH of
					Nothing	-> (False,undefined,ids)
					Just id	-> remove ((==) id) id ids

		getmenuitemtitle _ ids_titles = ids_titles

getMenuElementTitle :: Id -> Id -> GUI ps (Bool,Maybe String)
getMenuElementTitle menuId id = fmap head (getMenuElementTitles menuId [id])

-- | returns the shortcut keys associated with the corresponding menu elements
getMenuElementShortKeys :: Id -> [Id] -> GUI ps [(Bool,Maybe Char)]
getMenuElementShortKeys menuId ids = do
	r <- getMenu menuId recurseNone (null . fst) getmenuitemshortkey (ids, map (\id->(id,False,Nothing)) ids)
	return (map snd3thd3 (snd r))
	where
		getmenuitemshortkey :: MenuElementHandle ls ps -> ([Id],[(Id,Bool,Maybe Char)]) -> ([Id],[(Id,Bool,Maybe Char)])
		getmenuitemshortkey itemH@(MenuItemHandle {}) (ids,keys)
			| not hadId	= (ids,keys)
			| otherwise	= (ids1,snd (creplace (eqfst3id id) (id,True,mItemKey itemH) keys))
			where
				(hadId,id,ids1)	= case mItemId itemH of
					Nothing	-> (False,undefined,ids)
					Just id	-> remove ((==) id) id ids

		getmenuitemshortkey _ ids_keys = ids_keys

-- | returns the shortcut key associated with the menu element
getMenuElementShortKey :: Id -> Id -> GUI ps (Bool,Maybe Char)
getMenuElementShortKey menuId id = fmap head (getMenuElementShortKeys menuId [id])
