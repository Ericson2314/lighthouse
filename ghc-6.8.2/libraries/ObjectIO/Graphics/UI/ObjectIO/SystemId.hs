-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  SystemId
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- SystemId defines the basic identification values to identify GUI objects.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.SystemId where


data	SystemId = SystemId [Int] Int


worldSystemId :: Int -> SystemId	-- This systemid is always associated with the World
worldSystemId nrCreated = SystemId [0] nrCreated

worldChildId :: Int -> SystemId		-- This systemid is used for creating systemid from World
worldChildId nrCreated = SystemId [nrCreated,0] 0

initSystemId :: SystemId		-- This systemid is always associated with the initial IOState
initSystemId = SystemId [1] 0

nullSystemId :: SystemId		-- Dummy systemid
nullSystemId = SystemId [] 0

incrSystemId :: SystemId -> (SystemId,SystemId)
incrSystemId id@(SystemId idpath nrCreated)
	= (SystemId idpath idmax,SystemId (idmax:idpath) 0)
	where
		idmax = nrCreated+1

instance Eq SystemId where
	(==) (SystemId idpath1 _) (SystemId idpath2 _) = idpath1 == idpath2

instance Ord SystemId where
	(<) (SystemId idpath1 _) (SystemId idpath2 _)
		= lessidpath ids1 ids2
		where
			(ids1,ids2) = removecommonprefix idpath1 idpath2
			
			removecommonprefix xxs@(x:xs) yys@(y:ys)
				| x==y      = removecommonprefix xs ys
				| otherwise = (xxs,yys)
			removecommonprefix xs ys
				= (xs,ys)
			
			lessidpath (x:xs) (y:ys) = x<y
			lessidpath []     (_:_)  = True
			lessidpath _      _      = False
