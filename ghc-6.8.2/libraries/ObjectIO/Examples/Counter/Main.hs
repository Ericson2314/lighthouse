module Main (main) where

import Prelude hiding (Either(..))
import Graphics.UI.ObjectIO

main
	= startIO NDI () init []
	where		
		init ps = do
			displayId <- openId
			openDialog 1 (dialog displayId) ps
		
		dialog displayId =
			Dialog "Counter"
					(EditControl "" displaywidth displayheight 
						[ ControlTip "Counter Value"
						, ControlPos (Center,zero)
						, ControlId displayId
						, ControlSelectState Unable
						]
					 :+: LayoutControl (
					     ButtonControl "&-"
						[ ControlFunction (upd (\x -> x - 1))
						, ControlTip "Decrement counter value"
						]
					 :+: ButtonControl "&+"
						[ ControlFunction (upd (\x -> x + 1))
						, ControlTip "Increment counter value"
						]
					 ) [ControlPos (Center, zero)])
				[ WindowClose (noLS closeProcess)
				, WindowInit  (initDialog displayId)
				]
			where
				upd f (value, ps) = do
				   let new_value = f value
				   setControlText displayId (show new_value)
				   return (new_value, ps)
				   
				initDialog displayId (ls,ps) = do
				   setControlText displayId (show ls)
				   return (ls,ps)
				
			
		displaywidth	= PixelWidth 100
		displayheight	= 1

