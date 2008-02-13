-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Window.Create
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Window.Create contains the window creation functions.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.Window.Create
		( openwindow
		, openModalWindow
		, bufferDelayedEvents
		, checkZeroWindowBound
		, decreaseWindowBound
		) where



import Graphics.UI.ObjectIO.CommonDef
import Graphics.UI.ObjectIO.Control.Create
import Graphics.UI.ObjectIO.Control.Pos(moveWindowViewFrame)
import Graphics.UI.ObjectIO.Process.IOState
import Graphics.UI.ObjectIO.Process.Scheduler(handleContextOSEvent)
import Graphics.UI.ObjectIO.StdWindowAttribute
import Graphics.UI.ObjectIO.Window.Access
import Graphics.UI.ObjectIO.Window.Handle
import Graphics.UI.ObjectIO.Window.Validate
import Graphics.UI.ObjectIO.Window.Update(updateWindow)
import Graphics.UI.ObjectIO.OS.Event
import Graphics.UI.ObjectIO.OS.Window
import Control.Monad(unless)
import System.IO(fixIO)



windowcreateFatalError :: String -> String -> x
windowcreateFatalError function error
    = dumpFatalError function "WindowCreate" error



{-  Open a modal dialogue.  -}

openModalWindow :: Id -> WindowLSHandle ls ps -> ps -> GUI ps (ps, (Maybe ls))
openModalWindow wId (WindowLSHandle {wlsState=wlsState,wlsHandle=wH}) ps = do
    (found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
    					-- This condition should never occur: WindowDevice must have been 'installed'
    unless found (windowcreateFatalError "openModalWindow" "could not retrieve WindowSystemState from IOSt")
    let windows = windowSystemStateGetWindowHandles wDevice
    osdinfo <- accIOEnv ioStGetOSDInfo
    wMetrics <- accIOEnv ioStGetOSWindowMetrics
    (_,_,_,_,wH) <- liftIO (validateWindow wMetrics osdinfo wH windows)
    let title = whTitle wH
    let closable = any isWindowClose (whAtts wH)
    let wlsH            = WindowLSHandle{wlsState=wlsState,wlsHandle=wH}
    let wIds            = WIDS{wId=wId,wPtr=0,wActive=True}             -- wPtr=0 is assumed by system
    let wsH             = WindowStateHandle wIds (Just wlsH)
    let modalWIDS       = getWindowHandlesActiveModalDialog windows
    let windows1       	= addWindowHandlesActiveWindow wsH windows  -- frontmost position is assumed by system
    appIOEnv (ioStSetDevice (WindowSystemState windows1))
    inputTrack <- ioStGetInputTrack
    ioStSetInputTrack Nothing         	-- clear input track information
    (noError,ps1) <- liftContextIO (\context -> osCreateModalDialog closable title osdinfo (fmap wPtr modalWIDS) (handleOSEvent context)) ps
    let (delayMouse,delayKey) = case inputTrack of      -- after handling modal dialog, generate proper (Mouse/Key)Lost events
	    Nothing -> ([],[])
	    Just it@(InputTrack {itWindow=itWindow,itControl=itControl,itKind=itKind}) ->
		( if (itkMouse itKind)    then [createOSLooseMouseEvent itWindow (if itControl==0 then itWindow else itControl)] else []
		, if (itkKeyboard itKind) then [createOSLooseKeyEvent   itWindow (if itControl==0 then itWindow else itControl)] else []
		)    
    ioStAppendEvents (delayMouse++delayKey)
    finalLS <- getFinalModalDialogLS noError (toWID wId)
    unless noError (throwGUI (OtherError "could not create modal dialog"))
    return (ps, finalLS)
    where
	    handleOSEvent :: Context -> OSEvent -> IO [Int]
	    handleOSEvent context osEvent = handleContextOSEvent context (ScheduleOSEvent osEvent [])

{-  getFinalModalDialogLS retrieves the final local state of the modal dialog. This value has been stored in the window handles.
    This MUST have been done by disposeWindow (windowdispose). 
-}
	    getFinalModalDialogLS :: Bool -> WID -> GUI ps (Maybe ls)
	    getFinalModalDialogLS False wid = return Nothing
	    getFinalModalDialogLS True  wid = do
		(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
		unless found (windowcreateFatalError "getFinalModalDialogLS" "could not retrieve WindowSystemState from IOSt")
		let windows = windowSystemStateGetWindowHandles wDevice
		let (final,windows1) = getFinalLS wid windows
		appIOEnv (ioStSetDevice (WindowSystemState windows1))
		(case final of
			Nothing    -> windowcreateFatalError "getFinalModalDialogLS" "final local modal dialog state not found"
			Just final -> return (getFinalModalLS wid final))
		where
			getFinalLS :: WID -> WindowHandles ps -> (Maybe FinalModalLS,WindowHandles ps)
			getFinalLS wid windows@(WindowHandles {whsFinalModalLS=whsFinalModalLS}) =
			    let (removed,finalLS,finalLSs)    = remove (\(FinalModalLS wids _)->identifyWIDS wid wids) undefined whsFinalModalLS
			    in
				if not removed then (Nothing,windows)
				else (Just finalLS,windows{whsFinalModalLS=finalLSs})

            
{-  Open a modeless window/dialogue.  -}
openwindow :: Id -> WindowLSHandle ls ps -> ps -> GUI ps ps
openwindow wId (WindowLSHandle {wlsState=wlsState,wlsHandle=wH}) ps
    = do {
        (found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice);
        if  not found      -- This condition should never occur: WindowDevice must have been 'installed'
        then    windowcreateFatalError "openwindow" "could not retrieve WindowSystemState from IOSt"
        else
        let windows        = windowSystemStateGetWindowHandles wDevice
        in
        do {
            (delayinfo,wPtr,index,wH1) <- openAnyWindow wId wH windows;
            let
                (windowInit,wH2) = getWindowHandleInit wH1
                wIds       = WIDS {wId=wId,wPtr=wPtr,wActive=False}
            in
            do {
                ps1 <- toGUI (doInitIO (windowInit (wlsState,ps))
                                       (\ls ioState -> ioStSetDevice
                                                           (WindowSystemState
                                                                (addWindowHandlesWindow index 
                                                                (WindowStateHandle wIds (Just (WindowLSHandle {wlsState=ls,wlsHandle=wH2})))
                                                                windows))
                                                           ioState));
                bufferDelayedEvents delayinfo;
                return ps1
            }
        };      
      }
    where
        doInitIO :: GUI ps (ls,ps) -> (ls -> IOSt ps -> IOSt ps) -> IOSt ps -> IO (ps,IOSt ps)
        doInitIO initGUI setWindowLS ioState
            = do {
                r <- fixIO (\st -> fromGUI initGUI (setWindowLS (fst (fst st)) ioState));
                let ((_,ps1),ioState1) = r
                in  return (ps1,ioState1)
              }
        
        getWindowHandleInit :: WindowHandle ls ps -> (GUIFun ls ps,WindowHandle ls ps)
        getWindowHandleInit wH
            = (getWindowInitFun (snd (cselect isWindowInit (WindowInit return) (whAtts wH))),wH)
    
    {-  openAnyWindow creates a window.
            After validating the window and its controls, the window and its controls are created.
            The return OSWindowPtr is the OSWindowPtr of the newly created window.
            The return Index is the proper insert position in the WindowHandles list.
    -}
        openAnyWindow :: Id -> WindowHandle ls ps -> WindowHandles ps
                      -> GUI ps ([DelayActivationInfo],OSWindowPtr,Index,WindowHandle ls ps)
        openAnyWindow wId wH windows
            = do {                
                osdinfo  <- accIOEnv ioStGetOSDInfo;
                wMetrics <- accIOEnv ioStGetOSWindowMetrics;
                (index,pos,size,originv,wH1) <- liftIO (validateWindow wMetrics osdinfo wH windows);
                let behindPtr = getStackBehindWindow index windows
                in  do {
                    (delayinfo,wPtr,osdinfo1,wH2) <- liftIO (createAnyWindow wMetrics behindPtr wId pos size originv osdinfo wH1);
                --  wH3                           <- validateWindowClipState wMetrics True wPtr wH2;
                    appIOEnv (ioStSetOSDInfo osdinfo1);
                    liftIO (osInvalidateWindow wPtr);
                    return (delayinfo,wPtr,index,wH2)
                    }
              }

{-  In this implementation, Windows are not taken into account. 
-}

createAnyWindow :: OSWindowMetrics -> OSWindowPtr -> Id -> Point2 -> Size -> Vector2 -> OSDInfo -> WindowHandle ls ps
                                                  -> IO ([DelayActivationInfo],OSWindowPtr,OSDInfo,WindowHandle ls ps)
createAnyWindow wMetrics behindPtr wId point' size' originv osdinfo wH
    | whKind wH==IsWindow = 
        let
	    isResizable			= True
	    windowInfo			= whWindowInfo wH
	    viewDomain			= windowDomain windowInfo
	    viewOrigin			= windowOrigin windowInfo
	    hScroll			= windowHScroll windowInfo
	    vScroll			= windowVScroll windowInfo
	    visScrolls			= osScrollbarsAreVisible wMetrics viewDomain size (isJust hScroll,isJust vScroll)
	    Rect{rright=w',rbottom=h'}	= getWindowContentRect wMetrics visScrolls (sizeToRect size')
	    hInfo			= toScrollbarInfo hScroll (rleft viewDomain,x viewOrigin,rright  viewDomain, w')
	    vInfo			= toScrollbarInfo vScroll (rtop viewDomain, y viewOrigin,rbottom viewDomain, h')
	    minSize			= osMinWindowSize
	    maxSize			= rectSize viewDomain
	    (_,cursorAtt)		= cselect isWindowCursor (WindowCursor StandardCursor) (whAtts wH)

	    toScrollbarInfo :: Maybe ScrollInfo -> (Int,Int,Int,Int) -> ScrollbarInfo
	    toScrollbarInfo Nothing scrollState =
		ScrollbarInfo{cbiHasScroll=False,cbiPos=undefined,cbiSize=undefined,cbiState=undefined}
	    toScrollbarInfo (Just (ScrollInfo{scrollItemPos=scrollItemPos,scrollItemSize=scrollItemSize})) (min,origin,max,size) =
		ScrollbarInfo{cbiHasScroll=True,cbiPos=toTuple scrollItemPos,cbiSize=toTuple scrollItemSize,cbiState=osScrollState}
		where
		    osScrollState		= toOSscrollbarRange (min,origin,max) size

	    setScrollInfoPtr :: Maybe ScrollInfo -> OSWindowPtr -> Maybe ScrollInfo
	    setScrollInfoPtr (Just info) scrollPtr	= Just info{scrollItemPtr=scrollPtr}
            setScrollInfoPtr Nothing     _		= Nothing
        in do
	      (delay_info,wPtr,hPtr,vPtr,osdinfo,wH) <- osCreateWindow wMetrics isResizable hInfo vInfo minSize (toTuple maxSize)
										      isClosable (whTitle wH) pos size getInitActiveControl (createWindowControls wMetrics)
										       (updateWindowControl wMetrics wId size)
										      osdinfo behindPtr wH
	      let windowInfo1 = windowInfo{windowHScroll = setScrollInfoPtr hScroll hPtr
					  ,windowVScroll = setScrollInfoPtr vScroll vPtr}
		  wH1         = wH{whWindowInfo=windowInfo1}
	      wH2 <- moveWindowViewFrame wMetrics originv (WIDS{wPtr=wPtr,wId=wId,wActive=False}) wH1	-- PA: check WIDS value
	      delay_info' <- osShowWindow wPtr False
	      osSetWindowCursor wPtr (toCursorCode (getWindowCursorAtt cursorAtt))
	      return (delay_info++delay_info',wPtr,osdinfo,wH2)

    | whKind wH==IsDialog
        = do {
            (delay_info,wPtr,wH1) <- osCreateDialog (whMode wH == Modal) isClosable (whTitle wH) pos size behindPtr
                                                    getInitActiveControl
                                                    (createWindowControls wMetrics)
                                                    (updateWindowControl wMetrics wId size)
                                                    osdinfo wH;
            return (delay_info,wPtr,osdinfo,wH1)
          }
    where
        isClosable = any isWindowClose (whAtts wH)
        pos        = toTuple point'
        size       = toTuple size'
        
    --  createWindowControls creates the controls.
        createWindowControls :: OSWindowMetrics -> OSWindowPtr -> WindowHandle ls ps -> IO (WindowHandle ls ps)
        createWindowControls wMetrics wPtr wH = do
            itemHs <- createControls wMetrics Nothing Nothing (whSelect wH) wPtr (whItems wH)
            return wH{whItems=itemHs}
        
    --  updateWindowControl updates customised controls.
        updateWindowControl :: OSWindowMetrics -> Id -> (Int,Int) -> OSWindowPtr -> OSWindowPtr -> OSPictContext -> WindowHandle ls ps
                                                                                                                 -> IO (WindowHandle ls ps)
--        updateWindowControl wMetrics wId (w,h) wPtr cPtr osPict wH
--            = return wH            
            
					       
	updateWindowControl wMetrics wId (w,h) wPtr cPtr osPict wH@(WindowHandle {whItems=itemHs}) =
	    let 
		(_,controls) = getUpdateControls cPtr (sizeToRect (Size{w=w,h=h})) itemHs
		wH1 = wH{whItems=itemHs}
		updateInfo = UpdateInfo{ updWIDS	= WIDS{wPtr=wPtr,wId=wId,wActive=False}	-- PA: check WIDS value
				       , updWindowArea	= zero
				       , updControls	= controls
				       , updGContext	= Just osPict
				       }
	    in updateWindow wMetrics updateInfo wH1
	    where
		getUpdateControls :: OSWindowPtr -> Rect -> [WElementHandle ls ps] -> (Bool,[ControlUpdateInfo])
		getUpdateControls cPtr clipRect (itemH:itemHs)			    
		    | found	= (found,controls)
		    | otherwise	= getUpdateControls cPtr clipRect itemHs
		    where
			(found,controls) = getUpdateControl cPtr clipRect itemH

			getUpdateControl :: OSWindowPtr -> Rect -> WElementHandle ls ps -> (Bool,[ControlUpdateInfo])
			getUpdateControl cPtr clipRect itemH@(WItemHandle {wItemPtr=wItemPtr})
			    | cPtr==wItemPtr = (True, [ControlUpdateInfo {cuItemNr=wItemNr itemH,cuItemPtr=wItemPtr,cuArea=clipRect1}])
			    | otherwise	 = getUpdateControls cPtr clipRect1 (wItems itemH)
			    where
				clipRect1 = intersectRects clipRect (posSizeToRect (wItemPos itemH) (wItemSize itemH))
			getUpdateControl cPtr clipRect (WListLSHandle itemHs) =
			    getUpdateControls cPtr clipRect itemHs
			getUpdateControl cPtr clipRect (WExtendLSHandle exLS itemHs) =
			    getUpdateControls cPtr clipRect itemHs
			getUpdateControl cPtr clipRect (WChangeLSHandle chLS itemHs) =
			    getUpdateControls cPtr clipRect itemHs
		getUpdateControls _ _ [] = (False,[])

getStackBehindWindow :: Index -> WindowHandles ps -> OSWindowPtr
getStackBehindWindow     0 wsHs = osNoWindowPtr
getStackBehindWindow index wsHs = wPtr (getWindowStateHandleWIDS wsH)
    where wsH = (whsWindows wsHs) !! (index-1)
        


{-  bufferDelayedEvents buffers the events in the OSEvents environment. -}
bufferDelayedEvents :: [DelayActivationInfo] -> GUI ps ()
bufferDelayedEvents delayinfo = ioStAppendEvents (map toOSEvent delayinfo)
    where
        toOSEvent :: DelayActivationInfo -> SchedulerEvent
        toOSEvent (DelayActivatedWindow wPtr)
            = createOSActivateWindowEvent wPtr
        toOSEvent (DelayDeactivatedWindow wPtr)
            = createOSDeactivateWindowEvent wPtr
        toOSEvent (DelayActivatedControl wPtr cPtr)
            = createOSActivateControlEvent wPtr cPtr
        toOSEvent (DelayDeactivatedControl wPtr cPtr)
            = createOSDeactivateControlEvent wPtr cPtr


{-  WindowBound-checks for normal windows. -}

checkZeroWindowBound :: IOSt ps -> Bool
checkZeroWindowBound ioState = found && checkZeroWindowHandlesBound wHs
    where
        (found,wDevice) = ioStGetDevice WindowDevice ioState
        wHs             = windowSystemStateGetWindowHandles wDevice

decreaseWindowBound :: IOSt ps -> IOSt ps
decreaseWindowBound ioState
    | not found
        = ioState
    | otherwise
        = ioStSetDevice (WindowSystemState wHs1) ioState
    where
        (found,wDevice) = ioStGetDevice WindowDevice ioState
        wHs             = windowSystemStateGetWindowHandles wDevice
        wHs1            = decreaseWindowHandlesBound wHs
