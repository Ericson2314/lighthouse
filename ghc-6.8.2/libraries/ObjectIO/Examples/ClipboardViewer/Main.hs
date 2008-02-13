module Main where

--	**************************************************************************************************
--
--	A program that can show and set the current content of the clipboard.
--
--	The program has been written in Haskell and uses the Port of Clean's Object I/O library 1.2
--	
--	**************************************************************************************************

import Prelude hiding (Either(..))
import Graphics.UI.ObjectIO					-- Import all standard gui library modules

main :: IO ()							-- The main rule
main = do
	ids <- openIds 3					-- Create 3 Id values
	startIO NDI () (initialise ids) []

initialise [viewid,showid, setid] = openDialog () clipview 	-- Open the clipview dialog
	where
		clipview
			= Dialog				-- The clipview window is a Dialog
				"Clipboard Viewer"		-- The title of the clipview window
				(				-- The controls of the clipview window:
					showclip		--    content display and refresh button
				:+:				--    +
					setclip			--    edit display and set button
				:+:				--    +
					quit			--    quit button
				)
				[	WindowId viewid		-- Id of the clipview window
				]

		showclip								-- The content display:
			=   EditControl "" width nrlines				--    an EditControl to display strings,
					[	ControlSelectState Unable		--    the user can't type text,
					,	ControlId          showid		--    its Id so the program can set text
					,	ControlPos         (Left,zero)		--    its position (new line & left)
					]
			:+: ButtonControl "Show"					--    button to get the clipboard content,
					[	ControlFunction    (noLS show)		--    performed by the function show
					,	ControlTip         "Display clipboard content"
					]

		setclip									-- The edit display:
			=   EditControl "" width nrlines				--    an EditControl,
					[	ControlId          setid		--    its Id so the program can get text,
					,	ControlPos         (Left,zero)		--    its position (new line & left)
					,	ControlTip         "Type text for clipboard"
					]
			:+: ButtonControl "Set"						--    a button to set clipboard content,
					[	ControlFunction    (noLS set)		--    performed by the function set
					,	ControlTip         "Move text to clipboard"
					]

		quit=   ButtonControl "Quit"						-- The quit button:
					[	ControlFunction    (noLS closeProcess)	--    simply closes the entire process
					,	ControlPos         (Center,zero)	--    its position (new line & center)
					,	ControlTip         "Quit clipboardview"
					]

		width	= PixelWidth (hmm 100.0)
		nrlines	= 5

		show ps = do								-- The Show function:
			content <- getClipboard						--	retrieve new clipboard content
			let text = getString content					--	as a String
			setControlText showid text					--	and display it
			return ps

		set ps = do								-- The Set function:
			(_,Just text) <- getControlText setid				--	get the current edit control content
			setClipboard [toClipboard text]					--	store it in the clipboard
			return ps

		getString (clip:clips) =
			case fromClipboard clip of
				Nothing   -> getString clips
				Just item -> item
		getString [] = ""
