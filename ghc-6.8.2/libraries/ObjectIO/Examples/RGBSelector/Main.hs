module Main where


--	**************************************************************************************************
--
--	This program creates a windows that allows a user to create a RGB colour.
--
--	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
--	
--	**************************************************************************************************


import Graphics.UI.ObjectIO
import Prelude hiding (Left)


blackRGB = RGB   0   0   0
whiteRGB = RGB 255 255 255

main :: IO ()
main = do
	rgbid <- openR2Id
	ids <- openIds 7
	startColourPicker rgbid ids
	where
		startColourPicker rgbid ids = startIO SDI () initialise [ProcessClose closeProcess]
			where
				initialise ps = do
					rgbsize <- controlSize (colourPickControl rgbid ids initrgb Nothing) True Nothing Nothing Nothing
					let wdef = Window "Pick a colour" (colourPickControl rgbid ids initrgb Nothing)
							[ WindowViewSize   rgbsize
							, WindowPen        [PenBack lightGrey]
							]
					let mdef = Menu "PickRGB"
							(   MenuItem "MinRGB" [ MenuFunction (noLS (set rgbid blackRGB))
									      , MenuShortKey 'n'
									      ]
							:+: MenuItem "MaxRGB" [	MenuFunction (noLS (set rgbid whiteRGB))
									      ,	MenuShortKey 'm'
									      ]
							:+: MenuSeparator     []
							:+: MenuItem "Quit"   [	MenuFunction (noLS closeProcess)
									      ,	MenuShortKey 'q'
									      ]
							) []
					ps <- openWindow undefined wdef ps
					ps <- openMenu   undefined mdef ps
					return ps
					where
						initrgb	= RGB {r=maxRGB,g=maxRGB,b=maxRGB}

						set rid rgb ps = fmap snd (syncSend2 rid (InSet rgb) ps)



--	The definition of the text-slider component:

type RGBPickControl ls ps = TupLS SliderControl TextControl ls ps

rgbPickControl :: Colour -> (String,Id,Id) -> Id -> (Colour->Int) -> (Int->Colour->Colour) -> Maybe ItemPos -> RGBPickControl Colour ps
rgbPickControl rgb (text,sid,tid) did get set maybePos =
	SliderControl Horizontal length sliderstate slideraction (ControlId sid:controlPos)
	  :+: TextControl   (colourText text (get rgb))	[ControlId tid]
	where
		controlPos = case maybePos of
			Just pos	-> [ControlPos pos]
			_		-> []
		length		= PixelWidth (maxRGB-minRGB+1)
		sliderstate	= SliderState{sliderMin=minRGB, sliderMax=maxRGB, sliderThumb=get rgb}

		slideraction :: SliderMove -> GUIFun Colour ps
		slideraction move (rgb,ps) = do
			setSliderThumb sid y
			setControlText tid (colourText text y)
			setColourBox did newrgb
			return  (newrgb, ps)
			where
				y = case move of
					SliderIncSmall	-> min (get rgb+1 ) maxRGB
					SliderDecSmall	-> max (get rgb-1 ) minRGB
					SliderIncLarge	-> min (get rgb+10) maxRGB
					SliderDecLarge	-> max (get rgb-10) minRGB
					SliderThumb x	-> x
				newrgb	= set y rgb
	
colourText :: String -> Int -> String
colourText text x = text++" "++show x


--	The definition of a colour box:

type ColourBoxControl ls ps = CustomControl ls ps

colourBoxControl :: Colour -> Id -> Maybe ItemPos -> ColourBoxControl ls pst
colourBoxControl rgb did maybePos =
	CustomControl (Size{w=40,h=40}) (colourBoxLook rgb)
		((ControlId did) : (case maybePos of (Just pos) -> [ControlPos pos];_->[]))


colourBoxLook :: Colour -> SelectState -> UpdateState -> Draw ()
colourBoxLook colour _ (UpdateState {newFrame=newFrame}) = do
	setPenColour colour
	fill newFrame
	setPenColour black
	draw newFrame

setColourBox :: Id -> Colour -> GUI ps ()
setColourBox id rgb = setControlLook id True (True,colourBoxLook rgb)



--	The definition of the RGB access control:

data In	 =	InGet		| InSet Colour
data Out =	OutGet Colour	| OutSet

type RGBId = R2Id In Out

type ColourPickAccess ps = Receiver2 In Out Colour ps

colourPickAccess :: RGBId -> [(String,Id,Id)] -> Id -> ColourPickAccess ps
colourPickAccess rid rgbpicks did = Receiver2 rid accessRGB []
	where
		accessRGB :: In -> (Colour,ps) -> GUI ps (Out,(Colour,ps))
		accessRGB InGet (rgb,ps) =
			return (OutGet rgb,(rgb,ps))
		accessRGB (InSet rgb@(RGB r g b)) (_,ps) = do
			setColourBox    did rgb
			setSliderThumbs (map (\(y,(_,sid,_))->(sid,y)) settings)
			setControlTexts (map (\(y,(text,_,tid))->(tid,colourText text y)) settings)
			return (OutSet,(rgb,ps))
			where
				settings= zip [r,g,b] rgbpicks



--	The definition of the assembled colour picking control:

type ColourPickControl ls ps
	= NewLS ( LayoutControl
			(TupLS (LayoutControl (ListLS (TupLS SliderControl TextControl))) (TupLS CustomControl (Receiver2 In Out)))
		) ls ps

colourPickControl :: RGBId -> [Id] -> Colour -> Maybe ItemPos -> ColourPickControl ls ps
colourPickControl rgbid ids initrgb maybePos =
	NewLS initrgb
	  	(LayoutControl
			(	LayoutControl
				(   ListLS
					[ rgbPickControl initrgb rpicks did r (\x rgb->rgb{r=x}) left
					, rgbPickControl initrgb gpicks did g (\x rgb->rgb{g=x}) left
					, rgbPickControl initrgb bpicks did b (\x rgb->rgb{b=x}) left
					]) [ControlHMargin 0 0,ControlVMargin 0 0]
				:+: colourBoxControl initrgb did Nothing
				:+: colourPickAccess rgbid [rpicks,gpicks,bpicks] did
			) (case maybePos of Just pos -> [ControlPos pos]; _->[])
		)
	where	
		[rid,rtid,gid,gtid,bid,btid,did] = ids
		(rtext,gtext,btext)		 = ("Red","Green","Blue")
		(rpicks,gpicks,bpicks)		 = ((rtext,rid,rtid),(gtext,gid,gtid),(btext,bid,btid))
		left				 = Just (Left,zero)
