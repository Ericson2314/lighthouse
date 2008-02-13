-----------------------------------------------------------------------------
-- |
-- Module      :  StdProcessDef
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- StdProcessDef contains the types to define interactive processes.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.StdProcessDef
	( 
	-- * Defintions
	  module Graphics.UI.ObjectIO.StdProcessDef,
	-- * A visible modules
	  module Graphics.UI.ObjectIO.StdIOCommon
	, module Graphics.UI.ObjectIO.StdGUI
	) where


import Graphics.UI.ObjectIO.StdGUI
import Graphics.UI.ObjectIO.StdIOCommon


-- | Standard process definition type
data	Process
	= forall ps . Process
			DocumentInterface	-- The process document interface
			ps			-- The process private state
			(ProcessInit ps)	-- The process initialisation
			[ProcessAttribute ps]	-- The process attributes
type	ProcessInit ps
	= ps -> GUI ps ps
