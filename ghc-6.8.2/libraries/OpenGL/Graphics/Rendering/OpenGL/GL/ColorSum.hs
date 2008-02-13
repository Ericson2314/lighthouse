--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ColorSum
-- Copyright   :  (c) Sven Panne 2002-2006
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.9 (Color Sum) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.ColorSum (
   colorSum
) where

import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapColorSum), makeCapability )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( Capability )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar )

--------------------------------------------------------------------------------

colorSum :: StateVar Capability
colorSum = makeCapability CapColorSum
