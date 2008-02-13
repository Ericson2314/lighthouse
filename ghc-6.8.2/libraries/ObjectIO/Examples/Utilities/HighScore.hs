module HighScore where


{-	General utility for reading/writing high scores to Files and displaying current high scores.
	This module uses the Object I/O library, version 1.0.2.
-}


import	Graphics.UI.ObjectIO
import	qualified Graphics.UI.ObjectIO.StdIOCommon as SC

type HiScores = [HiScore]
data HiScore
   = HiScore
   	{ name	:: !String
	, score	:: !Int
	}
	deriving (Show,Read)

--	Read in the high scores:
readHiScores :: FilePath -> IO HiScores
readHiScores fname = do
	content <- fmap lines (readFile fname)
	return (map read content)

--	Write the high scores:
writeHiScores :: FilePath -> HiScores -> IO ()
writeHiScores fname highs = do
	let content = map show highs
	writeFile fname (unlines content)

-- 	Determine whether, given the number of high scores, a given score is actually a new high score:
itsAHighScore :: Int -> Int -> HiScores -> Bool
itsAHighScore nrOfHiScores score' hiscores
	| score'==0			= False
	| length hiscores<nrOfHiScores	= True
	| otherwise			= any (\hiscore -> score' > score hiscore) hiscores
	

--	Add a HiScore to the current list of high scores:
addScore :: Int -> HiScore -> HiScores -> HiScores
addScore nrOfHighScores hi hiscores =
	take nrOfHighScores (addscore hi hiscores)
	where
		addscore :: HiScore -> HiScores -> HiScores
		addscore hi' hiscores@(hi:his)
			| score hi > score hi'  = hi  : addscore hi' his
			| otherwise		= hi' : hiscores
		addscore hi' []			= [hi']


--	Display high scores in a modal dialog to the user:
showHiScores :: String -> HiScores -> ps -> GUI ps ps
showHiScores header highs ps = do
	okId <- openId
	wId  <- openId
	openModalDialog undefined (dialog wId okId) ps
	return ps
	where
		dialog id okId
			= Dialog "High Scores"
				(	TextControl header	[ControlPos (Center,zero)]
				:+:	text
				:+:	ButtonControl "OK"	[ControlId okId,ControlPos (Center,zero),ControlFunction (noLS (closeWindow id))]
				)
				[	WindowId	id
				,	WindowOk	okId
				]
		text = if null highs
		       then (ListLS [TextControl "No high scores available." [ControlPos (SC.Left,zero)]])
		       else (ListLS [TextControl (show hi++". "++take 13 name++" "++show score) [ControlPos (SC.Left,zero)]
							| (hi,HiScore{name=name,score=score}) <- zip [1..] highs
				    ])
