module Main where


--	**************************************************************************************************
--
--	A program in which two text files can be compared char by char.
--
--	The program has been written in Haskell and uses the Port of Clean's Standard Object I/O library 1.2
--	
--	**************************************************************************************************

import Prelude hiding (Either(..))
import Graphics.UI.ObjectIO


data Local
   = Local
   	{ name1	:: String
	, name2	:: String
	}

noFilesSelected = Local {name1="", name2=""}


main :: IO ()
main = startIO NDI noFilesSelected initIO []

initIO :: Local -> GUI Local Local
initIO ps = do
	showid <- openId
	openDialog undefined (dialog showid) ps
	where
		dialog showid =
			Dialog "Compare"
				(	ButtonControl "&Compare..."
						[	ControlFunction (noLS compare)
						,	ControlTip	"Compare two files"
						]
				:+:	ButtonControl "Compare &again"
						[	ControlFunction (noLS again)
						,	ControlTip	"Compare the files again"
						]
				:+:	ButtonControl "&Quit"
						[	ControlFunction (noLS closeProcess)
						,	ControlTip	"Quit compare"
						]
				)	[]
			where
				compare :: Local -> GUI Local Local
				compare ps = do
					(maybeFirstFile,ps)  <- selectInputFile ps
					(if isNothing maybeFirstFile
					 then return noFilesSelected
					 else do
						(maybeSecondFile,ps) <- selectInputFile ps
						(if isNothing maybeSecondFile
						 then return noFilesSelected
						 else showdifference (Local{name1=fromJust maybeFirstFile,name2=fromJust maybeSecondFile})))

				again :: Local -> GUI Local Local
				again ps@(Local{name1=name1,name2=name2})
					| name1=="" || name2=="" = compare ps
					| otherwise		 = showdifference ps

				showdifference :: Local -> GUI Local Local
				showdifference ps@(Local{name1=name1,name2=name2}) = do
					ps <- closeWindow showid ps
					content1 <- liftIO (readFile name1)
					content2 <- liftIO (readFile name2)
					let maybeDifference = compareLists 1 (lines content1) (lines content2)
					(if isNothing maybeDifference
					 then openDialog undefined dialogOk ps
					 else openDialog undefined (dialogDiff (fromJust maybeDifference)) ps)
					where
						dialogOk = Dialog "Result"
							( ListLS
								[	TextControl ("Files are equal") []								
								]
							)
							[ WindowId	showid
							, WindowClose	(noLS (closeWindow showid))
							]
							
						dialogDiff (i,line1,line2)		= Dialog "Difference found"
							( ListLS
								[	TextControl ("Difference at line "++show i) []
								,	TextControl line1 [ControlPos (Left,zero)]
								,	TextControl line2 [ControlPos (Left,zero)]
								]
							)
							[ WindowId	showid
							, WindowClose	(noLS (closeWindow showid))
							]

compareLists :: Int -> [String] -> [String] -> Maybe (Int,String,String)
compareLists n     []     [] = Nothing
compareLists n     [] (y:ys) = Just (n,"",y)
compareLists n (x:xs)     [] = Just (n,x,"")
compareLists n (x:xs) (y:ys)
	| x == y = compareLists (n+1) xs ys
	| otherwise = Just (n,x,y)
