module Main (main) where

--	**************************************************************************************************
--
--	This program creates two interactive processes that communicate via message passing.
--	In a future distributed version this program can be used as a graphical talk application.
--
--	This program is the converted Clean 1.3.2 to Haskell 98 + Object I/O 1.2 version.
--	**************************************************************************************************

import Graphics.UI.ObjectIO

--	The message type of talk processes:
data	Message
 =	NewLine String	-- Transmit a line of text
 |	Quit		-- Request termination

--	main creates two talk processes A and B that communicate by means of message passing.
main :: IO ()
main	= do	a     <- openRId
		b     <- openRId
		talkA <- talk "A" a b
		talkB <- talk "B" b a
		startProcesses [talkA,talkB]

{-	talk name me you
	defines a talk process named name, to which messages can be sent of type Message
	via me, and that sends messages of type Message to a receiver you.
-}
talk :: String -> RId Message -> RId Message -> IO Process
talk name me you
	= do	outId <- openId
		inId  <- openId
		let {	infield    = EditControl "" (ContentWidth "mmmmmmmmmm") 5
			                 [ ControlId          inId
			                 , ControlKeyboard    inputfilter Able (noLS1 (input inId you))
			                 ];
			outfield   = EditControl "" (ContentWidth "mmmmmmmmmm") 5
			                 [ ControlId          outId
			                 , ControlPos         (Below inId,zero)
			                 , ControlSelectState Unable
			                 ];
			talkdialog = Dialog ("Talk "++name) (infield:+:outfield)
			                 [ WindowClose (noLS (quit you))
			                 ];
			receiver   = Receiver me (noLS1 (receive outId)) [];
			process    = Process
			                 NDI
			                 ()
			                 (initialise talkdialog receiver)
			                 [ProcessClose (quit you)]
		} in return process
	where
		initialise ddef rdef ps
			= do	ps1 <- openDialog   undefined ddef ps
				ps2 <- openReceiver undefined rdef ps1
				return ps2

{-	input handles keyboard input in the input EditControl: 
	for every KeyDown keyboard input that has been accepted by the input EditControl, input sends the 
	current content text of the input EditControl to the other talk process with (NewLine text).
-}
inputfilter :: KeyboardState -> Bool
inputfilter keystate
	= getKeyboardStateKeyState keystate /= KeyUp

input :: Id -> RId Message -> KeyboardState -> ps -> GUI ps ps
input inId you _ ps = do
	(_, Just text) <- getControlText inId
	asyncSend you (NewLine text)
	return ps

{-	The message passing protocol of a talk process.
	On receipt of:
	(1) NewLine text: set the new text to the output control field of the talk dialog.
	(2) Quit:         this is always the last message of the other talk process when termination is 
	                  requested. The process should terminate itself.
-}
receive :: Id -> Message -> ps -> GUI ps ps
receive outId (NewLine text) ps
	= setControlText outId text >> return ps
receive _ Quit ps
	= closeProcess ps

{-	The quit command first sends the Quit message to the other talk process and then quits itself.
-}	
quit :: RId Message -> ps -> GUI ps ps
quit you ps = asyncSend you Quit >> closeProcess ps
