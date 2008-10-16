{-P:
import Prelude hiding (putStrLn,putStr,getLine)
-}
import Control.Monad
import Data.List(isSuffixOf)

import Kernel.Debug(v_defaultConsole)
import Kernel.Console
import Kernel.LineEditor
import Kernel.Driver.IA32.Screen
import Kernel.Driver.IA32.VbeGraphics as VBE
import Kernel.Driver.CMOS
import Kernel.Driver.Keyboard
import Kernel.Driver.Mouse(launchMouseDecoder)
import Kernel.Driver.PS2
import Kernel.Driver.IA32.Reboot(reboot)

import qualified Kernel.Driver.NE2000.Test as NE2000(testnet)
import qualified Kernel.Driver.Intel8255x.Test as Intel8255x(testnet)

import Data.Word(Word8,Word32)
import Data.Array.IArray(elems,listArray,(!))
import Data.Array.Unboxed(UArray)
-- import Data.Array.IO(IOUArray,newArray_,writeArray,freeze)

import Kernel.MemRegion

import GIFparser(parseGIFs)
import DescribeGIF


import Kernel.PCI.Probe(probe)
import Kernel.PCI.DevTree(drawTree,findDev)
import Kernel.PCI.ParseVendors

import Net.ClientInterface
import Net.TFTP_Client(tftpGet)
import qualified Net.Interface as Net
import Net.Utils(arraySize)
import Net.Shell

-- import AoutUser
-- import UProc
import Kernel.UserProc (execUProc,buildUProc)
import Demo(gadgetsDemo)
import Util.FontDecode(decodeFont)
--import Util.FixedFont(fonts)
import Kernel.GrubModules(pickModule,moduleCount,moduleName,moduleNames,moduleRegion)
import H.Grub as Grub

import H.Monad(H,runH{-trappedRunH-},trace)
import H.Concurrency
import H.Interrupts(enableInterrupts)

--import H.AdHocMem(peekByteOff,minusPtr,castPtr)
import H.MemRegion(peekByteOff)
--import Foreign.C.String(peekCStringLen) -- to be moved to H.AdHocMem

--import L4.H4(osker)
import qualified SimpleExec as DummyToGetItTypechecked
--import GfxBenchmark
import Monad.Util
import Util.CmdLineParser hiding ((!))
import qualified Util.CmdLineParser as P
       
import LwConc.Conc(startSystem)
import H.Monad(liftIO)
import Foreign.C(CString,withCString)

-- Jiffies test
import Foreign.C(CUInt)
foreign import ccall unsafe getourtimeofday :: IO CUInt

default(Int)

foreign import ccall unsafe "start.h c_print" c_print :: CString -> IO ()

cPrint :: String -> H ()
cPrint str = liftIO (withCString str c_print)

main :: IO ()
main = --trappedRunH mainH
       startSystem $ runH mainH

mainH :: H ()
mainH =
    do netState <- newMVar Nothing
       pciState <- newMVar (Nothing,Nothing) -- XXX
       trace "Tracing enabled"
       enableInterrupts
       shell (netState, pciState)

shell exestate =
  do optgfx <- VBE.initialize
     case optgfx of
       Just gfx ->
         do optfonts <- loadFonts
	    case optfonts of
	      Right fonts -> gfxShell2 exestate gfx fonts
	      _ -> initTextShell exestate -- pointless, not in text mode
       _ -> initTextShell exestate

loadFonts =
  do opti <- pickModule (const done) "/fonts.hf"
     case opti of
       Just i -> Right # (decodeFont =<< moduleRegion i)
       _ -> return (Left "Grub module /fonts.hf is not present")

loadPCITables = 
  do opti <- pickModule (const done) "/pci.ids.gz"
     case opti of
       Just i -> do arr <- regionBytes =<< moduleRegion i 
                    case Kernel.PCI.ParseVendors.parseTables arr of
                      Nothing -> return (Left "pci.ids is invalid.")
                      Just t  -> return (Right t)
       _ -> return (Left "Grub module /pci.ids.gz is not present")

welcome = "Welcome to the House shell! Enter help to see a list of commands."

initTextShell exestate =
    do console <- launchConsoleDriver
       putMVar v_defaultConsole console -- for possible debugging
       putString console (welcome++"\n\n")
       textShell console exestate =<< kbdChan

kbdChan = launchKeyboardInterpreter =<<
	          launchKeyboardDecoder =<<
		  launchKeyboardDriver

gfxShell2 exestate@(netstate,_) gfx fonts =
    do --b <- timeIt $ timeLimit 3000 $ gfxBenchmark gfx fonts
       --let msg = "Graphics benchmark result: "
	--	 ++show (1000*n `div` t)++"units/s."
       let msg = ""
       gifss <- loadGifs
       gadgetsDemo gfx fonts gifss (shellWindow msg) (runH . loadGIFs')
  where
    loadGifs =
       do gifpaths <- filter (".gif" `isSuffixOf`) # moduleNames
	  gifss <- mapM loadGIFs gifpaths
	  return [(path,gifs)|(path,Right gifs)<-zip gifpaths gifss]

    loadGIFs = fmap join . withModuleRegion (fmap parseGIFs .  regionBytes)
      where
	withModuleRegion action = withModule (action @@ moduleRegion)

	withModule action =
            maybe (return $ Left "File not found") (fmap Right . action)
            @@ pickModule quiet

        join (Left e) = Left e
        join (Right (Left e)) = Left e
        join (Right (Right a)) = Right a

    quiet _ = done :: H () -- opposite of putStrLn

    loadGIFs' path =
      case break (==':') path of
        (arg,':':path) ->
            withIPaddr' fail arg $ \ ip ->
	    netcmd' netstate fail $ \ Net{udp=udp} ->
	    either Left parse # tftpGet quiet udp ip path "octet"
          where
            fail = return . Left
	    parse bs = either (Left . (("("++show n++" bytes) ")++)) Right $
		       parseGIFs bytes
	      where
	        bytes = listArray (0,n-1) (concatMap elems bs)
		n = sum (map arraySize bs)
	_ -> loadGIFs path

    shellWindow msg =
      do dispCh <- newChan
	 kbdCh  <- newChan
	 --threadDelay 1000000 -- microseconds(?)
	 let putStr = writeChan dispCh
	     putStrLn s = putStr s >> putStr "\n"
	     putChar c = putStr [c]
	     getChar = readChan kbdCh
	     getLine = getLine' ""
	     getLine' l = do c <- getChar
			     let echo = putChar c
			     case c of
			       '\n' -> echo >> return (reverse l)
			       '\f' -> echo >> return ""
			       '\b' -> if null l
				       then getLine' l
				       else echo >> getLine' (tail l)
			       _    -> echo >> getLine' (c:l)
	     user = Net.Interface { Net.rx = getLine, Net.tx = putStr }
	     loop =  do putStr "> "
			cmd <- getLine
			execute exestate user (words cmd)
			loop
	     shell = do putStrLn $ welcome++msg
		        loop
         forkH shell
         return (dispCh,kbdCh)

    execute exestate user = execute2 extra exestate user
      where
        extra = cmd "vbeinfo" vbeinfo P.!
	        cmd "clear" (putStr "\f") P.!
	        cmd "draw" draw <@ number
        vbeinfo =
	    do print (screenSize gfx)
	       print (fbInfo gfx)
               print =<< vbeInfo gfx

        draw count =
          do --t0 <- getRawSeconds
             mapM_ putStr [show n++' ':['0'..'z']++"\n"|n<-[1..count]]
	     --t1 <- getRawSeconds
	     --putStr $ show (t1-t0)++" CMOS seconds\n"

        putStr = Net.tx user
        print x = putStr . (++"\n") . show $ x

--textShell :: Console -> Chan KeyPress -> H ()
textShell console exestate chan =
    do editor  <- newEditor chan console
       let user = Net.Interface { Net.rx=getLine editor "> ", Net.tx=putStr }
           loop =
	     do line <- getLine editor "> "
		putStrLn ""
		execute exestate user line
		loop
       loop
  where
    putStr =  putString console
    putStrLn = putStringLn console

    execute state user = execute2 extra state user . words
      where
        extra = cmd "testmouse" testmouse P.!
	        cmd "testmouse2" testmouse2 P.!
	        cmd "loadfonts" loadfonts

	testmouse = do forkH $ mapM_ (putStrLn.show) =<< getChanContents chan
		       mapM_ (putStrLn . show) =<< getChanContents
			 =<< launchMouseDecoder (1024,768)
			 =<< launchMouseDriver
	testmouse2 = do forkH $ mapM_ (putStrLn.show) =<< getChanContents chan
			testMouse2 putStr
	loadfonts = putStrLn . either id (show . map fst) =<< loadFonts

execute2 _ _ _ [] = done
execute2 extra exestate user cmd | last cmd=="&" =
  do forkH $ execute3 extra exestate user (init cmd)
     done
execute2 extra exestate user cmd = execute3 extra exestate user cmd

execute3 extra exestate@(netstate,pciState) user =
    either putStrLn id . parseAll grammar
  where
    grammar =
      commands P.!
      netcommands (execute2 extra exestate) runBytes findPCINet netstate user P.!
      extra P.!
      debugcommands

    commands =
      oneof
      [cmd "help" (putStrLn $ usage "" grammar),
       cmd "date" date,
       cmd "mem" (print =<< Grub.memInfo),
       cmd "lspci" lspci <@ flag "-t" -: "List PCI devices/load textual PCI info",
       cmd "ls" modules <@ flag "-l" -: "List Grub modules",
       cmd "run" run <@ path -: "Run a grub module",
       cmd "gif" gifinfo <@ path -: "Parse and show info about a GIF",
--       cmd "osker" ( \ ws->osker ws putStrLn putStr) <@ many args,
       cmd "reboot" reboot]

    debugcommands =
      oneof [cmd "lambda" (putStrLn "Too much to abstract!"),
             cmd "js" jiffies,
             -- tests
             cmd "killtest" killtest,
             cmd "killself" killself,
             cmd "delaytest" delaytest,
             cmd "delaykilltest" delaykilltest,
             -- benchmarks
             cmd "createmv" createmv, -- comparable
             cmd "takemv" takemv,     -- LwConc is 2x faster
             cmd "putmv" putmv,       -- LwConc is significantly faster
             cmd "forkbomb" forkbomb,
             cmd "forkbomb2" forkbomb2,
             cmd "block20k" block20k,
             cmd "hammer1" hammer1,
             cmd "chanpc10k" (chanpc 10000),
             cmd "chanpc100k" (chanpc 100000),
             -- preempt and friends
             cmd "preempt" preempt,
             cmd "preempt2" preempt2,
             cmd "preempt3" preempt3,
             cmd "preempt4" preempt4,
             cmd "wastemem" wasteMem <@ number]
      where
        killtest = do id <- forkH $ p2helper "A"
                      killH id
                      putStrLn "The other thread was about to print 'A's, but likely didn't get a chance."
        killself = do forkH $ do self <- myThreadId
                                 putStrLn (show self ++ " is about to print some dots, but not forever!")
                                 killH self
                                 p2helper "."
                      return ()
        delaytest = do putStrLn "Waiting 5 seconds..."
                       threadDelay 5000000
                       putStrLn "Welcome back!"
        delaykilltest = do tid <- forkH $ p2helper "o"
                           threadDelay 1000000
                           killH tid
                           putStrLn "Welcome back!"
        jiffies = do j <- liftIO getourtimeofday
                     putStrLn ("jiffies = " ++ show j ++ "; msec = " ++ show (j*20))
        {- PERFORMANCE TESTS -}
        createmv = mapM_ newMVar [1..10000000] -- ten million
        takemv = do mvars <- mapM newMVar [1..100000] -- only 100 thousand
                    return $! map takeMVar mvars
                    return ()
        putmv = do mvars <- sequence (replicate 100000 newEmptyMVar) -- only 100 thousand
                   return $! zipWith putMVar mvars [1..100000]
                   return ()
                   {- hits a blackhole...eventually :)
        fork100k = do waits <- sequence (replicate 100000 newEmptyMVar)
                      mapM_ forkH [putMVar done 1 | done <- waits]
                      mapM_ takeMVar waits
                      -}
        forkbomb = mapM_ forkH (replicate 100000 (return ())) -- only 100 thousand
        forkbomb2 = sequence_ [forkH (cPrint (show i ++ "\n")) >> cPrint ("Just forked " ++ show i ++ "\n") | i <- [1..100]] -- only 100 thousand
        block20k = do waits <- sequence $! (replicate 20000 newEmptyMVar)
                      mapM_ forkH [putMVar done 1 | done <- waits]
                      mapM_ takeMVar waits
        hammer1 = do cPrint "Wait for the 1. 2. 3. ...\n"
                     mv <- newMVar 42
                     forkH (sequence_ (replicate 100000 (readMVar mv)) >> cPrint "1.\n")
                     forkH (sequence_ (replicate 100000 (readMVar mv)) >> cPrint "2.\n")
                     forkH (sequence_ (replicate 100000 (readMVar mv)) >> cPrint "3.\n")
                     return ()
        chanpc n = do chan <- newChan
                      forkH (mapM_ (writeChan chan) [1..n])
                      sequence_ (replicate n (readChan chan))

        -- prints the number of invalid logs/aborted transactions
        --invalid = do i <- liftIO $ readIORef invalidLogs
                     --putStrLn (show i ++ " invalid logs")

	preempt = do forkH (putStrLn (repeat 'a'))
		     putStrLn (repeat 'b')
        p2helper s = do putStr s
                        t <- newMVar [1,2,3]
                        u <- readMVar t
                        p2helper s
	preempt2 = do putStrLn "This is a fair preempt: both allocate."
                      forkH (p2helper "A")
		      p2helper "B"
	preempt3 = do putStrLn "Unfair preempt: forked thread doesn't alloc"
                      forkH (putStr (repeat 'a'))
		      p2helper "B"
	preempt4 = do putStrLn "Unfair preempt: main thread doesn't alloc"
	              forkH (p2helper "A")
		      putStr (repeat 'b')

	wasteMem n = print $ sum (reverse [1..n::Integer])

    putStr = Net.tx user
    putStrLn s = putStr s >> putStr "\n"
    print x = putStrLn (show x)

    pciTables = do s <- takeMVar pciState
                   case s of
                     (_, Just _)  -> putMVar pciState s
                     (d, Nothing) -> do putStr "Parsing tables..."
                                        x <- loadPCITables
                                        case x of
                                          Left err -> do putStrLn err
                                                         putMVar pciState s
                                          Right t  -> 
                                            do putStrLn "done."
                                               putMVar pciState (d,Just t)
    probePCI  = do s <- takeMVar pciState
                   case s of
                     (Just _, _)  -> putMVar pciState s
                     (Nothing, t) -> do d <- Kernel.PCI.Probe.probe  
                                        putMVar pciState (Just d, t)

    getPCIdevs  = do s <- readMVar pciState
                     case s of
                       (Just d, _) -> return d
                       (Nothing,_) -> do probePCI
                                         getPCIdevs

    lspci parse = if parse then pciTables else lsPCI

    lsPCI     = do ds <- getPCIdevs
                   s <- readMVar pciState
                   let sh = case snd s of
                              Nothing -> (const Nothing, const Nothing) 
                              Just t  -> t
                   drawTree putStrLn sh ds

    findPCINet =
      do ds <- getPCIdevs
         let findEth init ven dev = (,) init # findDev ven dev ds
	 return $ msum [findEth NE2000.testnet 0x10ec 0x8029,
		        findEth Intel8255x.testnet 0x8086 0x1229,
		        findEth Intel8255x.testnet 0x8086 0x1031]

    date = do (year, month, day, hour, min, sec) <- getRawTime
	      putStrLn $
                show (2000+fromIntegral year)
                ++ "-" ++ show2 month ++ "-" ++ show2 day
		++ " " ++ show2 hour ++ ":" ++ show2 min ++ ":" ++ show2 sec
	      putStrLn ""
       where show2 = drop 1 . show . (100+)

    modules long =
      do n<- moduleCount
	 when long $ putStrLn $ "Module count: "++show n
	 mapM_ (showModule long) [0..n-1]

    showModule False i = putStrLn =<< (moduleName i)
    showModule True i =
      do n <- moduleName i
	 r <- moduleRegion i
	 putStrLn $ show i++": "++n++" "++show r

    run = withModuleRegion (runUProc @@ moduleUProc)

    withModuleRegion action = withModule (action @@ moduleRegion)

    withModule action = maybe notFound action @@ pickModule putStrLn
      where notFound = putStrLn "File not found"

    gifinfo = withModuleRegion (showGifInfo @@ regionBytes)

    showGifInfo = putStr .
		  either id (unlines . map describeGIF) .
		  parseGIFs

    runUProc uproc =
      do r <- execUProc uproc 
         case r of  
           Left uproc' -> 
              do yield
                 runUProc uproc'
           Right msg ->
              putStrLn ("Exit Status: " ++ show msg)

    moduleUProc = buildUProc putStr . peekByteOff

    bytesUProc bs = buildUProc putStr (return.(a!))
      where a = listArray (0,fromIntegral (length bs)-1) bs :: UArray Word32 Word8
    runBytes = runUProc @@ bytesUProc

withIPaddr' fail arg succeed = 
    case reads arg of
      [(ipaddr,"")] -> succeed ipaddr
      _ -> fail "Bad IPv4 address"

testMouse2 putStr =
    do putStr . unwords . map show =<< getChanContents
	 -- =<< launchMouseDecoder
	 =<< launchMouseDriver

