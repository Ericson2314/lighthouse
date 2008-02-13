module Notice where

--	**************************************************************************************************
--
--	A new instance of the Dialogs type constructor class to easily create simple notice dialogues.
--
--	This module has been written in Clean 1.3.1 and uses the Clean Standard Object I/O library 1.0.2
--	
--	**************************************************************************************************

import Graphics.UI.ObjectIO
import qualified Graphics.UI.ObjectIO.StdIOCommon as SC

--  The data type that defines a notice.

data Notice ls ps = Notice [String] (NoticeButton ls ps) [NoticeButton ls ps]
data NoticeButton ls ps = NoticeButton String (GUIFun ls ps)

--  Notices are defined as a new instance of the Dialogs type constructor class.

instance Dialogs Notice where    
    openDialog ls notice ps = do
        wId  <- openId
        okId <- openId
        openDialog ls (noticeToDialog wId okId notice) ps
    
    openModalDialog ls notice ps = do
        wId  <- openId
        okId <- openId
        openModalDialog ls (noticeToDialog wId okId notice) ps


--  noticeToDialog converts a Notice expression into a Dialog expression.

noticeToDialog wId okId (Notice texts (NoticeButton text f) buttons)
    = Dialog ""
        (   LayoutControl
		(ListLS [ TextControl text [ControlPos (SC.Left,zero)] | text <- texts ])
		[ControlHMargin 0 0, ControlVMargin 0 0, ControlItemSpace 3 3]
        :+: ButtonControl text
            	[ControlFunction (noticefun f), ControlPos (SC.Right,zero), ControlId okId]
        :+: ListLS
		[   ButtonControl text [ControlFunction (noticefun f),ControlPos (LeftOfPrev,zero)]
				|  (NoticeButton text f) <- buttons
		]
        )
        [ WindowId wId
        , WindowOk okId
        ]
    where
	noticefun f (ls,ps) = closeWindow wId ps >>= (\ps -> f (ls,ps))
