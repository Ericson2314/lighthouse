-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Validate
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Menu.Access where


import	Graphics.UI.ObjectIO.Menu.Handle
import  Graphics.UI.ObjectIO.StdIOBasic(Title)
import  Graphics.UI.ObjectIO.StdId(Id)
import	Graphics.UI.ObjectIO.OS.Menu
import	Graphics.UI.ObjectIO.OS.Types(OSMenu)


menuStateHandleGetHandle :: MenuStateHandle ps -> OSMenu
menuStateHandleGetHandle (MenuStateHandle (MenuLSHandle {mlsHandle=MenuHandle {mHandle=mHandle}})) = mHandle

menuStateHandleGetMenuId :: MenuStateHandle ps -> Id
menuStateHandleGetMenuId (MenuStateHandle (MenuLSHandle {mlsHandle=MenuHandle {mMenuId=mMenuId}})) = mMenuId

menuStateHandleGetTitle :: MenuStateHandle ps -> Title
menuStateHandleGetTitle (MenuStateHandle (MenuLSHandle {mlsHandle=MenuHandle {mTitle=mTitle}})) = mTitle

menuStateHandleGetSelect :: MenuStateHandle ps -> Bool
menuStateHandleGetSelect (MenuStateHandle (MenuLSHandle {mlsHandle=MenuHandle {mSelect=mSelect}})) = mSelect


menuStateHandleSetHandle :: OSMenu -> MenuStateHandle ps -> MenuStateHandle ps
menuStateHandleSetHandle menu (MenuStateHandle mlsH@(MenuLSHandle {mlsHandle=mH})) = MenuStateHandle mlsH{mlsHandle=mH{mHandle=menu}}

menuStateHandleSetTitle :: Title -> MenuStateHandle ps -> MenuStateHandle ps
menuStateHandleSetTitle title (MenuStateHandle mlsH@(MenuLSHandle {mlsHandle=mH})) = MenuStateHandle mlsH{mlsHandle=mH{mTitle=title}}

menuStateHandleSetSelect :: Bool -> MenuStateHandle ps -> MenuStateHandle ps
menuStateHandleSetSelect select (MenuStateHandle mlsH@(MenuLSHandle {mlsHandle=mH})) = MenuStateHandle mlsH{mlsHandle=mH{mSelect=select}}
