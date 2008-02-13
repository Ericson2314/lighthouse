-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  OS.Font
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- OS.Font defines all font related functions and data types.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.Font
		( Font(..)
		, osFontGetImp, osCreateFont		
		, osFontNames, osFontSizes
		, osGetFontCharWidths, osGetFontStringWidths
		, osGetFontMetrics, osGetPicFontMetrics
		, defaultFont, dialogFont, serifFont
		, sansSerifFont, smallFont, nonProportionalFont, symbolFont 
		) where



import Graphics.UI.ObjectIO.CommonDef(dumpFatalError, isBetween, minmax)
import Graphics.UI.ObjectIO.OS.Cutil_12
import Graphics.UI.ObjectIO.OS.ClCCall_12
import Graphics.UI.ObjectIO.OS.ClCrossCall_12
import Graphics.UI.ObjectIO.OS.Types
import System.IO.Unsafe(unsafePerformIO)


data	Font 
	= Font
		{ fontName        :: !String	-- Name of the font
		, fontSize        :: !Int	-- Size of the font
		, fontIsBold      :: !Bool
		, fontIsUnderline :: !Bool
		, fontIsItalic    :: !Bool
		, fontIsStrikeout :: !Bool
		} deriving Eq

iStrikeOut, iUnderline, iItalic, iBold :: Int
iStrikeOut	= 8
iUnderline	= 4
iItalic		= 2
iBold		= 1

{-# NOINLINE defaultFont #-}
defaultFont = unsafePerformIO osDefaultFont
    where
	osDefaultFont :: IO Font
	osDefaultFont = do
	    a1 <- malloc
	    a2 <- malloc
	    a3 <- malloc
	    winDialogFontDef a1 a2 a3
	    fname <- fpeek a1 >>= peekCString
	    styles<- fpeek a2
	    size  <- fpeek a3
	    return (osCreateFont fname styles size)
foreign import ccall "cpicture_121.h WinDefaultFontDef" winDefaultFontDef :: Ptr CString -> Ptr Int -> Ptr Int -> IO ()

{-# NOINLINE dialogFont #-}
dialogFont = unsafePerformIO osDialogFont
    where
        osDialogFont :: IO Font
        osDialogFont = do
 	    a1 <- malloc
	    a2 <- malloc
	    a3 <- malloc
	    winDialogFontDef a1 a2 a3
	    fname <- fpeek a1 >>= peekCString
	    styles<- fpeek a2
	    size  <- fpeek a3
	    return (osCreateFont fname styles size)
foreign import ccall "cpicture_121.h WinDialogFontDef" winDialogFontDef :: Ptr CString -> Ptr Int -> Ptr Int -> IO ()

{-# NOINLINE serifFont #-}
serifFont = unsafePerformIO osSerifFont
    where
        osSerifFont :: IO Font
        osSerifFont = do
	    a1 <- malloc
	    a2 <- malloc
	    a3 <- malloc
	    winSerifFontDef a1 a2 a3
	    fname <- fpeek a1 >>= peekCString
	    styles<- fpeek a2
	    size  <- fpeek a3
	    return (osCreateFont fname styles size)
foreign import ccall "cpicture_121.h WinSerifFontDef" winSerifFontDef :: Ptr CString -> Ptr Int -> Ptr Int -> IO ()

{-# NOINLINE sansSerifFont #-}
sansSerifFont = unsafePerformIO osSansSerifFont
    where
        osSansSerifFont :: IO Font
        osSansSerifFont = do
	    a1 <- malloc
	    a2 <- malloc
	    a3 <- malloc
	    winSansSerifFontDef a1 a2 a3
	    fname <- fpeek a1 >>= peekCString
	    styles<- fpeek a2
	    size  <- fpeek a3
	    return (osCreateFont fname styles size)
foreign import ccall "cpicture_121.h WinSansSerifFontDef" winSansSerifFontDef :: Ptr CString -> Ptr Int -> Ptr Int -> IO ()

{-# NOINLINE smallFont #-}
smallFont = unsafePerformIO osSmallFont
    where
        osSmallFont :: IO Font
        osSmallFont = do
	    a1 <- malloc
	    a2 <- malloc
	    a3 <- malloc
	    winSmallFontDef a1 a2 a3
	    fname <- fpeek a1 >>= peekCString
	    styles<- fpeek a2
	    size  <- fpeek a3
	    return (osCreateFont fname styles size)
foreign import ccall "cpicture_121.h WinSmallFontDef" winSmallFontDef :: Ptr CString -> Ptr Int -> Ptr Int -> IO ()

{-# NOINLINE nonProportionalFont #-}
nonProportionalFont = unsafePerformIO osNonProportionalFont
    where
        osNonProportionalFont :: IO Font
        osNonProportionalFont = do
  	    a1 <- malloc
	    a2 <- malloc
	    a3 <- malloc
	    winNonProportionalFontDef a1 a2 a3
	    fname <- fpeek a1 >>= peekCString
	    styles<- fpeek a2
	    size  <- fpeek a3
	    return (osCreateFont fname styles size)
foreign import ccall "cpicture_121.h WinNonProportionalFontDef" winNonProportionalFontDef :: Ptr CString -> Ptr Int -> Ptr Int -> IO ()

{-# NOINLINE symbolFont #-}
symbolFont = unsafePerformIO osSymbolFont
    where
        osSymbolFont :: IO Font
        osSymbolFont = do
	    a1 <- malloc
	    a2 <- malloc
	    a3 <- malloc
	    winSymbolFontDef a1 a2 a3
	    fname <- fpeek a1 >>= peekCString
	    styles<- fpeek a2
	    size  <- fpeek a3
	    return (osCreateFont fname styles size)
foreign import ccall "cpicture_121.h WinSymbolFontDef" winSymbolFontDef :: Ptr CString -> Ptr Int -> Ptr Int -> IO ()

osFontGetImp :: Font -> (String, Int, Int)
osFontGetImp font = 
	( fontName font
	, (if fontIsBold      font then iBold      else 0) .|.
	  (if fontIsItalic    font then iItalic    else 0) .|.
	  (if fontIsUnderline font then iUnderline else 0) .|.
	  (if fontIsStrikeout font then iStrikeOut else 0)
	, fontSize font
	)

osCreateFont fname styles size = 
	Font
	  { fontName        = fname
	  , fontSize        = size
	  , fontIsBold      = styles .&. iBold      /= 0
	  , fontIsItalic    = styles .&. iItalic    /= 0
	  , fontIsUnderline = styles .&. iUnderline /= 0
	  , fontIsStrikeout = styles .&. iStrikeOut /= 0
	  }

osFontNames :: IO [String]
osFontNames
	= do {
		(_,unsortednames) <- issueCleanRequest fontnamesCallback (rq0Cci ccRqGETFONTNAMES) [];
		return (sortAndRemoveDuplicates unsortednames)
	  }
	where
		fontnamesCallback :: CrossCallInfo -> [String] -> IO (CrossCallInfo,[String])
		fontnamesCallback cci names
			= do {
				newname <- peekCString (int2addr (p1 cci));
				return (return0Cci,newname:names)
			  }

sortAndRemoveDuplicates :: (Ord a) => [a] -> [a]
sortAndRemoveDuplicates (e:es)
	= insert e (sortAndRemoveDuplicates es)
	where
		insert :: (Ord a) => a -> [a] -> [a]
		insert a list@(b:x)
			| a<b       = a:list
			| a>b       = b:(insert a x)
			| otherwise = list
		insert a _ = [a]
sortAndRemoveDuplicates _ = []


osFontSizes :: Int -> Int -> String -> IO [Int]
osFontSizes between1 between2 fname
	= do {
		textptr           <- newCString fname;
		(_,unsortedsizes) <- issueCleanRequest fontSizesCallback (rq1Cci ccRqGETFONTSIZES (addr2int textptr)) [];
		return (sortAndRemoveDuplicates unsortedsizes)
	  }
	where
		(low,high)         = minmax between1 between2
		
		fontSizesCallback :: CrossCallInfo -> [Int] -> IO (CrossCallInfo,[Int])
		fontSizesCallback (CrossCallInfo {p1=size,p2=0}) sizes
			= return (return0Cci,newsizes)
			where
				newsizes = if   isBetween size low high
				           then size:sizes
				           else sizes
		fontSizesCallback _ _
			= return (return0Cci,[low..high])

fn Nothing  = nullPtr
fn (Just x) = x

osGetFontCharWidths :: Maybe OSPictContext -> [Char] -> Font -> IO [Int]
osGetFontCharWidths maybeHdc chars font = do
	let (fname,fstyles,fsize) = osFontGetImp font
	s1 <- newCString fname
	r <- sequence [winGetCharWidth c s1 fstyles fsize (fn maybeHdc) | c <- chars]
	free s1
	return r
foreign import ccall "cpicture_121.h WinGetCharWidth" winGetCharWidth :: Char -> CString -> Int -> Int -> OSPictContext -> IO Int

osGetFontStringWidths :: Maybe OSPictContext -> [String] -> Font -> IO [Int]
osGetFontStringWidths maybeHdc strings font = do
	let (fname,fstyles,fsize) = osFontGetImp font
	s1 <- newCString fname
	r <- sequence [withCString s (\s -> winGetStringWidth s s1 fstyles fsize (fn maybeHdc)) | s <- strings]
	free s1
	return r
foreign import ccall "cpicture_121.h WinGetStringWidth" winGetStringWidth :: CString -> CString -> Int -> Int -> OSPictContext -> IO Int

osGetFontMetrics :: Maybe OSPictContext -> Font -> IO (Int,Int,Int,Int)
osGetFontMetrics maybeHdc font = do
	let (fname,fstyles,fsize) = osFontGetImp font
	-- Marshal arguments:
	s1 <- newCString fname
	o1 <- malloc
	o2 <- malloc
	o3 <- malloc
	o4 <- malloc
	-- Call C:
	winGetFontInfo s1 fstyles fsize (fn maybeHdc) o1 o2 o3 o4
	-- Read/free:
	free s1
	ascent <- fpeek o1
	descent <- fpeek o2
	maxwidth <- fpeek o3
	leading <- fpeek o4	
	return (ascent,descent,leading,maxwidth)
foreign import ccall "cpicture_121.h WinGetFontInfo" winGetFontInfo :: CString -> Int -> Int -> OSPictContext -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()
	
osGetPicFontMetrics :: OSPictContext -> IO (Int,Int,Int,Int)
osGetPicFontMetrics a1 = do
	-- Marshal arguments:
	o1 <- malloc
	o2 <- malloc
	o3 <- malloc
	o4 <- malloc		
	-- Call C:
	winGetPicFontInfo a1 o1 o2 o3 o4
	-- Read/free:
	r1 <- fpeek o1
	r2 <- fpeek o2
	r3 <- fpeek o3
	r4 <- fpeek o4
	return (r1,r2,r3,r4)
foreign import ccall "cpicture_121.h WinGetPicFontInfo" winGetPicFontInfo :: OSPictContext -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()
