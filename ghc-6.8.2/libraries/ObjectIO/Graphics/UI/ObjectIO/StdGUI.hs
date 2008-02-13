{-# OPTIONS_GHC -cpp #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  StdGUI
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
--  This is a new module that provides similar functionality as StdPSt in the Clean
--  Object I\/O library.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdGUI (
        noLS, noLS1, 
        GUIFun, GUI, ProcessAttribute(..), ModifiersFunction, KeyboardFunction,
        MouseFunction, SliderAction, ScrollFunction,
        ProcessWindowResizeFunction, ProcessOpenFilesFunction, ToolbarItem(..)) where

#ifndef __HADDOCK__
import {-# SOURCE #-} Graphics.UI.ObjectIO.Process.IOState(GUI, noLS, noLS1)
#endif
import Graphics.UI.ObjectIO.StdIOCommon(Modifiers(..),KeyboardState(..),SliderState(..),SliderMove(..),MouseState(..),ViewFrame, ItemPos, Size(..))
import Graphics.UI.ObjectIO.StdBitmap(Bitmap)

type    GUIFun ls ps
 =  (ls,ps) -> GUI ps (ls,ps)
 
type    GUI2Fun ps
 =  ps -> GUI ps ps


{-  Process attributes.             -}

data    ProcessAttribute ps                  -- Default:
    = ProcessActivate   (ps -> GUI ps ps)        -- No action on activate
    | ProcessDeactivate (ps -> GUI ps ps)        -- No action on deactivate
    | ProcessClose      (ps -> GUI ps ps)        -- Process is closed
--  Attributes for (M/S)DI process only:
    | ProcessOpenFiles  (ProcessOpenFilesFunction ps)    -- Request to open files
    | ProcessWindowPos  ItemPos              -- Platform dependent
    | ProcessWindowSize Size                 -- Platform dependent
    | ProcessWindowResize   (ProcessWindowResizeFunction ps) -- Platform dependent
    | ProcessToolbar    [ToolbarItem ps]         -- Process has no toolbar
 -- Attributes for MDI processes only:
    | ProcessNoWindowMenu                    -- Process has WindowMenu

type    ProcessWindowResizeFunction ps 
     =  Size                        -- Old ProcessWindow size
     -> Size                        -- New ProcessWindow size
     -> ps -> GUI ps ps
type    ProcessOpenFilesFunction ps
     =  [String]                    -- The file names to open
     -> ps -> GUI ps ps

data    ToolbarItem ps
    = ToolbarItem Bitmap (Maybe String) (ps -> GUI ps ps)
    | ToolbarSeparator



{-  Frequently used function types.         -}

type    ModifiersFunction ls ps = Modifiers     -> GUIFun ls ps
type    KeyboardFunction  ls ps = KeyboardState -> GUIFun ls ps
type    MouseFunction     ls ps = MouseState    -> GUIFun ls ps
type    SliderAction      ls ps = SliderMove    -> GUIFun ls ps


{-  Scrolling function.             -}

type    ScrollFunction =
        ViewFrame   ->              -- Current  view
        SliderState ->              -- Current  state of scrollbar
        SliderMove  ->              -- Action of the user
        Int         
