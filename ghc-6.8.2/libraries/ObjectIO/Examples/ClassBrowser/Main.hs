module Main where


import Graphics.UI.ObjectIO
import GraphViz

data ClassInfo
   = ClassInfo
	{ ident    :: Int
       	, name     :: String
       	, parents  :: [Int]
       	}

gaccessors = 
	GraphAccessors
		{ getNodeName	= name
       		, isParent 	= (\n1 n2 -> (ident n1) `elem` (parents n2))
       		, hasParents 	= not . null . parents
       		}

main :: IO ()
main = startIO SDI () init [ProcessClose closeProcess]
	where
		init = openWindow undefined (GraphViz "Classes" gaccessors test [])

			
test = 	[ ClassInfo 1  "Eq"   		[]
	, ClassInfo 2  "Bounded" 	[]
	, ClassInfo 3  "Enum" 		[]
	, ClassInfo 4  "Read" 		[]
	, ClassInfo 5  "Show" 		[]
	, ClassInfo 6  "Ord"  		[1]
	, ClassInfo 7  "Real" 		[6,8]
	, ClassInfo 8  "Num"  		[1,5]
	, ClassInfo 9  "Ix"   		[6]
	, ClassInfo 10 "Integral" 	[3,7]
	, ClassInfo 11 "Fractional" 	[8]
	, ClassInfo 12 "RealFrac" 	[7,11]
	, ClassInfo 13 "Floating" 	[11]
	, ClassInfo 14 "RealFloat" 	[12,13]
	, ClassInfo 15 "Functor" 	[]
	, ClassInfo 16 "Monad" 		[]
	]