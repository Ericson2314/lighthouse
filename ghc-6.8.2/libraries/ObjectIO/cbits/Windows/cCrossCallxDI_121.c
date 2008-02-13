/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	This module contains the cross call implementations required for
	NDI, SDI, and MDI document interfaces.
********************************************************************************************/
#include "cCrossCallxDI_121.h"
#include "cCrossCall_121.h"
#include "cCCallWindows_121.h"
#include "cAcceleratorTable_121.h"


/*	Global data with external references:
*/
HWND ghTopDocWindow = NULL;
BOOL gInMouseDown   = FALSE;
BOOL gInKey         = FALSE;
int gCurChar;

/*	Global data with internal references only:
*/
static LONG stdMDIClientCallback = 0;				/* The standard internal Windows callback routine of MDI client windows. */


/*	Registered Windows class names:
*/
char SDIFrameClassName[]  = "__CleanSDIFrame";		/* Class for SDI frames.  */
char MDIFrameClassName[]  = "__CleanMDIFrame";		/* Class for MDI frames.  */
char SDIWindowClassName[] = "__CleanSDIWindow";		/* Class for SDI windows (must have same length as MDIWindowClassName). */
char MDIWindowClassName[] = "__CleanMDIWindow";		/* Class for MDI windows (must have same length as SDIWindowClassName). */


/*	GetSDIClientWindow finds the first SDI client window of the argument hwnd.
		This procedure assumes that hwnd is the handle of a SDI frame window.
		If no SDI client window could be found then GetSDIClientWindow returns NULL.
*/
static HWND GetSDIClientWindow (HWND hwndFrame)
{
	HWND client;
	char *clientclassname;
	int  classnamelength;

	client = GetWindow (hwndFrame,GW_CHILD);
	classnamelength = strlen (SDIWindowClassName) + 1;
	clientclassname = rmalloc (classnamelength);
	GetClassName (client, clientclassname, classnamelength);

	while (client != NULL && strcmp(clientclassname, SDIWindowClassName) != 0)
	{
		client = GetWindow (client,GW_HWNDNEXT);
		GetClassName (client,clientclassname,classnamelength);
	}
	rfree (clientclassname);
	return client;
}


static BOOL inDcSpaceTime (HWND curwin,  int curx,  int cury, UINT curtime, HWND prevwin, int prevx, int prevy, UINT prevtime)
{
	if (abs (curx - prevx) > GetSystemMetrics (SM_CXDOUBLECLK) / 2)
		return FALSE;

	if (abs (cury - prevy) > GetSystemMetrics (SM_CXDOUBLECLK) / 2)
		return FALSE;

	if (curtime - prevtime > GetDoubleClickTime ())
		return FALSE;

	if (curwin != prevwin)
		return FALSE;

	return TRUE;
}


/*	Sending keyboard events to Clean thread:
*/
void SendKeyDownToClean (HWND hwndParent, HWND hwndChild, int c)
{
	SendMessage5ToClean (CcWmKEYBOARD, hwndParent, hwndChild, c, KEYDOWN, GetModifiers ());
}

void SendKeyStillDownToClean (HWND hwndParent, HWND hwndChild, int c)
{
	SendMessage5ToClean (CcWmKEYBOARD, hwndParent, hwndChild, c, KEYREPEAT, GetModifiers ());
}

void SendKeyUpToClean (HWND hwndParent, HWND hwndChild, int c)
{
	SendMessage5ToClean (CcWmKEYBOARD, hwndParent, hwndChild, c, KEYUP, GetModifiers ());
}


/*	Sending mouse events to Clean thread:
*/
void SendMouseUpToClean (HWND hwndParent, HWND hwndChild, int x, int y)
{
	gInMouseDown = FALSE;
	SendMessage6ToClean (CcWmMOUSE, hwndParent, hwndChild, BUTTONUP, x, y, GetModifiers ());
}

void SendMouseStillDownToClean (HWND hwndParent, HWND hwndChild, int x, int y)
{
	SendMessage6ToClean (CcWmMOUSE, hwndParent, hwndChild, BUTTONSTILLDOWN, x, y, GetModifiers ());
}

void SendMouseStillUpToClean (HWND hwndParent, HWND hwndChild, int x, int y)
{
	SendMessage6ToClean (CcWmMOUSE, hwndParent, hwndChild, BUTTONSTILLUP, x, y, GetModifiers ());
}

void SendMouseDownToClean (HWND hwndParent, HWND hwndChild, int x, int y)
{
	static int gClicks = 0;
	static UINT gClTime = 0;
	static int gClX, gClY;
	static HWND gClHwnd;

	gInMouseDown = TRUE;
	SetCapture (hwndChild);
	SetFocus (hwndChild);		/* Pressing the mouse must also set the keyboard input to that object. */

	if (gClicks != 0 && !inDcSpaceTime (hwndChild, x, y, GetMessageTime (), gClHwnd, gClX, gClY, gClTime))
		gClicks = 0;

	if (gClicks == 0)
	{
		gClTime = GetMessageTime ();
		gClX = x;
		gClY = y;
		gClHwnd = hwndChild;
	}

	gClicks++;

	SendMessage6ToClean (CcWmMOUSE, hwndParent, hwndChild, gClicks, x, y, GetModifiers ());
	if (gClicks == 3)
		gClicks = 0;
}	/* SendMouseDownToClean */


/*	This routine should be applied only in case of WM_DROPFILES messages.
	It will copy a buffer of all involved file names into one string and send it
	to Clean.
*/
static void SendDropFilesToClean (HWND hWin,WPARAM wPara)
{
	int nrFiles, fileNr, nrChars, charCount;
	char * filenames;

	/* first retrieve the number of files. */
	nrFiles = DragQueryFile ((HANDLE)wPara, 0xFFFFFFFF, (LPSTR)NULL, 0);

	/* calculate the number of characters. */
	nrChars = 0;
	for (fileNr=0; fileNr<nrFiles; fileNr++)
	{
		nrChars += (int) DragQueryFile ((HANDLE)wPara, (UINT)fileNr, (LPSTR)NULL, 0);
		nrChars += 1;				/* account for newline char */
	}
	nrChars += 1;					/* and add a zero at the end of the string */

	filenames = rmalloc (nrChars);	/* This pointer is passed to and freed in the Clean code. (see also WM_DDE_EXECUTE)*/
	charCount = 0;
	for (fileNr=0; fileNr<nrFiles; fileNr++)
	{
		charCount += (int) DragQueryFile ((HANDLE)wPara, (UINT)fileNr, (LPTSTR)filenames+charCount, (UINT)nrChars-charCount);
		*(filenames+charCount) = '\n';
		charCount += 1;
	}
	*(filenames+nrChars-1) = 0;		/* terminate string with zero character. */

	DragFinish ((HANDLE)wPara);		/* free the internal memory required for DROPFILES. */
	SendMessage2ToClean (CcWmPROCESSDROPFILES, (int) hWin, (int) filenames);
}	/* SendDropFilesToClean */


/*********************************************************************************************
	Callback routine for SDI frame window procedure.
	This routine handles the SDI frame events. These concern menus, toolbar, resizes.
	Also the accelerator table is kept at the frame window (analogous to MDI frame window).
	When resized, also the SDI client window (see previous callback routine) is notified.
	Note that whenever Clean is informed about an event, the GetSDIClientWindow(hWin) value
	should be passed to Clean which identifies the Clean SDI client window! The only exception
	is the CcWmPROCESSCLOSE message.
*********************************************************************************************/
static LRESULT CALLBACK SDIFrameProcedure (HWND hWin,UINT uMess,WPARAM wPara,LPARAM lPara)
{
	switch (uMess)
	{
		case WM_COMMAND:
			{
				switch (HIWORD (wPara))
				{
					case 0:		/*	0: message originates from a menu or equals BN_CLICKED */
						{
							if (lPara != 0)		/* PA: it was BN_CLICKED. */
							{
								/*	hwndClient can't be NULL, because a button has been pressed. */
								HWND hwndClient = GetSDIClientWindow (hWin);
								/*	Send also modifiers to Clean */
								SendMessage4ToClean (CcWmBUTTONCLICKED, hwndClient, lPara, GetModifiers (), LOWORD (wPara));
							}
							else				/*	It was from a menu. */
							{
								SendMessage2ToClean (CcWmCOMMAND, LOWORD (wPara), GetModifiers ());
							}
						}
						break;
					case 1:		/*	1: message originates from an accelerator */
						{
							SendMessage2ToClean (CcWmCOMMAND, LOWORD (wPara), GetModifiers ());
						}
						break;
					case CBN_SELENDOK:
						{
							int newsel;
							HWND combo;

							combo = (HWND) lPara;
							newsel = SendMessage (combo, CB_GETCURSEL, 0, 0);

							if (newsel!=CB_ERR)
							{
								HWND hwndClient = GetSDIClientWindow (hWin);
								SendMessage3ToClean (CcWmITEMSELECT, hwndClient, combo, newsel);
							}
							return 1;
						}
						break;
				}
			} break;
		/*	WM_NOTIFY is handled identically as for MDI client windows (see MDIClientProcedure).
		*/
		case WM_NOTIFY:
			{
				LPNMHDR pnmh        = (LPNMHDR) lPara;
				LPTOOLTIPTEXT lpttt = (LPTOOLTIPTEXT) lPara;
				UINT from, flags;

				from = lpttt->hdr.idFrom;
				flags= lpttt->uFlags;

				if (pnmh->code == TTN_NEEDTEXT && from != 0 && flags != TTF_IDISHWND)
				{
					HWND hwndToolbar;

					hwndToolbar = (HWND)GetGWL_USERDATA (hWin);

					//	get tooltip text from Clean
					SendMessage2ToClean (CcWmGETTOOLBARTIPTEXT, hwndToolbar, from);
					lstrcpy (lpttt->szText,(LPSTR)gCci.p1);
					if (gCci.p1 != 0)
						rfree ((HGLOBAL) gCci.p1);

					return 0;
				}
				return (DefWindowProc (hWin, uMess, wPara, lPara));
			}
			break;
		case WM_CLOSE:		/*	The SDI frame window is requested to be closed. */
			{
				SendMessage1ToClean (CcWmPROCESSCLOSE, hWin);
				return 0;
			}
			break;
		case WM_DESTROY:	/*	The SDI frame window is in the act of being closed. */
			{
				ProcessShortcutTable shortcuts;

				ghTopDocWindow=NULL;

				shortcuts = (ProcessShortcutTable) GetWindowLong (hWin, 0);	// get the local shortcut table
				DestroyProcessShortcutTable (shortcuts);					// and destroy it.
				gAcceleratorTableIsUpToDate = FALSE;	// The active global accelerator table is not up to date

				DragAcceptFiles (hWin,FALSE);			/* Unregister for WM_DROPFILES events. */

				return 0;
			}
			break;
		/*	WM_ENTERIDLE message is used to let Clean evaluate the initialisation action
			of a modal dialog by sending the CcWmIDLEDIALOG message.
		*/
		case WM_ENTERIDLE:
			{
				HWND hwndModalDialog;

				hwndModalDialog = (HWND)lPara;

				if (wPara == MSGF_DIALOGBOX && hwndModalDialog != ghwndLastModalDialog)
				{
					SendMessage1ToClean (CcWmIDLEDIALOG,(int)hwndModalDialog);
					ghwndLastModalDialog = hwndModalDialog;
				}
				else
				{
					SendMessage0ToClean (CcWmIDLETIMER);
				}
				return 0;
			} break;
		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);

				if (hwndClient != NULL)
				{
					int c = CheckVirtualKeyCode ((int) wPara);

					if (!c)
					/* Ignore non-virtual keys, because they arrive as WM_SYSCHAR and WM_CHAR. */
					{
						return DefWindowProc (hWin, uMess, wPara, lPara);
					}
					/* Handle virtual keys analogously to keys received as WM_SYSCHAR and WM_CHAR. */
					if (gInKey)
					{
						if (gCurChar == c)
							SendKeyStillDownToClean (hwndClient, hwndClient, gCurChar);
						else
						{
							SendKeyUpToClean (hwndClient, hwndClient, gCurChar);
							gCurChar = c;
							SendKeyDownToClean (hwndClient, hwndClient, gCurChar);
						}
					}
					else
					{
						gCurChar = c;
						SendKeyDownToClean (hwndClient, hwndClient, gCurChar);
						gInKey = TRUE;
					}
				}
				return 0;
			}
			break;
		case WM_SYSCHAR:
		case WM_CHAR:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);

				if (hwndClient != NULL)
				{
					if (gInKey)
					{
						if (gCurChar == (int) wPara)
							SendKeyStillDownToClean (hwndClient, hwndClient, gCurChar);
						else
						{
							SendKeyUpToClean (hwndClient, hwndClient, gCurChar);
							gCurChar = wPara;
							SendKeyDownToClean (hwndClient, hwndClient, gCurChar);
						}
					}
					else
					{
						gCurChar = wPara;
						SendKeyDownToClean (hwndClient, hwndClient, gCurChar);
						gInKey = TRUE;
					}
				}
			}
			break;
		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);
				if (hwndClient != NULL && gInKey)
				{
					SendKeyUpToClean (hwndClient, hwndClient, gCurChar);
				}
				gInKey = FALSE;
				gCurChar = 0;
				return (DefWindowProc (hWin, uMess, wPara, lPara));
			}
			break;
		case WM_KILLFOCUS:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);
				if (hwndClient != NULL && gInKey)
				{
					SendKeyUpToClean (hwndClient, hwndClient, gCurChar);
				}
				gInKey = FALSE;
				gCurChar = 0;
			}
			break;
		case WM_LBUTTONDOWN:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);
				if (hwndClient != NULL)
				{
					SendMouseDownToClean (hwndClient, hwndClient, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
				}
			}
			break;
		case WM_MOUSEMOVE:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);
				if (hwndClient != NULL)
				{
					if (gInMouseDown)
					{
						SendMouseStillDownToClean (hwndClient, hwndClient, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
					}
					else
					{
						SendMouseStillUpToClean (hwndClient, hwndClient, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
					}
				}
			}
			break;
		case WM_LBUTTONUP:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
			}
			break;
		case WM_CANCELMODE:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return DefWindowProc (hWin, uMess, wPara, lPara);
			}
			break;
		case WM_CAPTURECHANGED:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);
				if (hwndClient != NULL && gInMouseDown)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hwndClient, &p);
					SendMouseUpToClean (hwndClient, hwndClient, p.x, p.y);
				}
			}
			break;
		case WM_ACTIVATE:
			{
				HWND hwndClient = GetSDIClientWindow (hWin);

				if (LOWORD (wPara) != WA_INACTIVE)
				{
					if (hwndClient != NULL)
						SendMessage1ToClean (CcWmACTIVATE, hwndClient);
					ghTopDocWindow       = hWin;		// PA: shouldn't this be hwndClient?
					ghActiveFrameWindow  = hWin;
					ghActiveClientWindow = NULL;		// PA: shouldn't this be hwndClient?
				}
				else if (LOWORD (wPara) == WA_INACTIVE)
				{
					if (hwndClient != NULL)
						SendMessage1ToClean (CcWmDEACTIVATE, hwndClient);
					ghTopDocWindow       = NULL;
					ghActiveFrameWindow  = NULL;
					ghActiveClientWindow = NULL;
					gAcceleratorTableIsUpToDate = FALSE; // The active global accelerator table is not up to date
				}
				return DefWindowProc (hWin, uMess, wPara, lPara);
			}
			break;
		/*	The WM_CREATE message should cause the SDI frame window only to create the accelerator table.
			The crosscall request from Clean to create the SDI window will create the SDI client. The
			SDI client will notify Clean that the controls can be created.
		*/
		case WM_CREATE:
			{
				ProcessShortcutTable shortcuts;

				shortcuts = AllocateProcessShortcutTable (MINSIZEPROCESSSHORTCUTTABLE);	// create a new shortcut table
				SetWindowLong (hWin, 0, (long) shortcuts);		//     and store it in the local memory of the window
				gAcceleratorTableIsUpToDate = FALSE;			// The active global accelerator table is not up to date

				ghActiveFrameWindow  = hWin;					// Keep track of the active frame window
				ghActiveClientWindow = NULL;					//  and client window
			}
			break;
		/*	The WM_SIZE message resizes the toolbar if present and makes sure that the SDI client
			window is also resized by sending it the same size message, but with the toolbar height
			subtracted.
		*/
		case WM_SIZE:
			{
				HWND hwndToolbar,hwndClient;
				RECT toolbarRect;
				int  toolbarHeight = 0;

				/*	Also resize the toolbar if present. */
				hwndToolbar = (HWND)GetGWL_USERDATA (hWin);

				if (hwndToolbar != NULL)
				{
					SendMessage (hwndToolbar, TB_AUTOSIZE, (WPARAM)0, (LPARAM)0);
					UpdateWindow (hwndToolbar);

					if (!GetWindowRect(hwndToolbar,&toolbarRect))
						rMessageBox (NULL,MB_APPLMODAL,"SDIFrameProcedure","GetWindowRect (hwndToolbar,_) failed");
					toolbarHeight = toolbarRect.bottom - toolbarRect.top;
				}

				hwndClient = GetSDIClientWindow (hWin);
				if (hwndClient != NULL)
				{
					SetWindowPos (hwndClient,												/* the SDI client */
								  HWND_BOTTOM,												/* this value is ignored (SWP_NOZORDER)  */
								  0,0,														/* these values are ignored (SWP_NOMOVE) */
								  (int)LOWORD (lPara),(int)HIWORD (lPara)-toolbarHeight,	/* new width and height */
								  SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOZORDER
								 );
				}
			}
			break;
		/*	Accept the user dropping file(s) in the frame window.
		*/
		case WM_DROPFILES:
			{
				SendDropFilesToClean (hWin,wPara);
			}
			return 0;
			break;
	}
	return DefWindowProc (hWin,uMess,wPara,lPara);
}	/* SDIFrameProcedure */


/*********************************************************************************************
	Callback routine for MDI frame window procedure.
*********************************************************************************************/
static LRESULT CALLBACK MDIFrameProcedure (HWND hWin,UINT uMess,WPARAM wPara,LPARAM lPara)
{
	switch (uMess)
	{
		/*	WM_ENTERIDLE message is used to let Clean evaluate the initialisation action
			of a modal dialog by sending the CcWmIDLEDIALOG message.
		*/
		case WM_ENTERIDLE:
			{
				HWND hwndModalDialog;

				hwndModalDialog = (HWND)lPara;

				if (wPara == MSGF_DIALOGBOX && hwndModalDialog != ghwndLastModalDialog)
				{
					SendMessage1ToClean (CcWmIDLEDIALOG,(int)hwndModalDialog);
					ghwndLastModalDialog = hwndModalDialog;
				}
				else
				{
					SendMessage0ToClean (CcWmIDLETIMER);
				}
				return 0;
			} break;
		case WM_COMMAND:
			{
				if (HIWORD (wPara)==0 && lPara!=0)
				{
					HWND hwndToolbar;

					hwndToolbar = (HWND)GetGWL_USERDATA (hWin);	// Obtain the toolbar handle
					if (hwndToolbar != 0)
						SendMessage4ToClean (CcWmBUTTONCLICKED, hWin, lPara, GetModifiers (), LOWORD (wPara));
				}
				else
				{
					switch (wPara)
					{
						case (OSMenuIDEnd+1):
							{
								SendMessage (GetWindow (hWin,GW_CHILD),WM_MDIICONARRANGE,0,0);
							} break;
						case (OSMenuIDEnd+2):
							{
								SendMessage (GetWindow (hWin,GW_CHILD),WM_MDITILE,(WPARAM) (UINT) MDITILE_VERTICAL,0);
							} break;
						case (OSMenuIDEnd+3):
							{
								SendMessage (GetWindow (hWin,GW_CHILD),WM_MDITILE,(WPARAM) (UINT) MDITILE_HORIZONTAL,0);
							} break;
						case (OSMenuIDEnd+4):
							{
								SendMessage (GetWindow (hWin,GW_CHILD),WM_MDICASCADE,0,0);
							} break;
						default:
							SendMessage2ToClean (CcWmCOMMAND, LOWORD (wPara), GetModifiers ());
					}
				}
			} break;
		/* WM_CREATE should create the client window, the menu bar, and the "Window" menu. */
		case WM_CREATE:
			{
				CLIENTCREATESTRUCT clientcreate;
				HMENU menuBar, windowMenu;				// The handle to the menu bar and the "Window" menu
				ProcessShortcutTable shortcuts;			// New
				HWND hwndClient;						// New

				menuBar = CreateMenu ();				// Create the menu bar
				SetMenu (hWin,menuBar);					// and associate it with the frame window
				windowMenu = CreatePopupMenu ();		// Create the "Window" menu
				InsertMenu (menuBar,					// add it to the menuBar
							0xFFFFFFFF,					// at the end
							MF_BYPOSITION | MF_POPUP,	// Flags
							(UINT) windowMenu,			// the "Window" menu
							"&Window"					// and set its title
					);
				InsertMenu (windowMenu,0,MF_BYPOSITION | MF_STRING,OSMenuIDEnd+1,"Arrange &Icons");		// Add "Arrange Icons" command
				InsertMenu (windowMenu,0,MF_BYPOSITION | MF_STRING,OSMenuIDEnd+2,"&Tile Vertically");	// Add "Tile Vertically" command
				InsertMenu (windowMenu,0,MF_BYPOSITION | MF_STRING,OSMenuIDEnd+3,"Tile &Horizontally");	// Add "Tile Horizontally" command
				InsertMenu (windowMenu,0,MF_BYPOSITION | MF_STRING,OSMenuIDEnd+4,"&Cascade");			// Add "Cascade" command

				clientcreate.hWindowMenu  = windowMenu;
				clientcreate.idFirstChild = OSMenuIDEnd+5;	// Window ids must be generated from OSMenuIDEnd+5

				hwndClient = CreateWindow (	"MDICLIENT",								// The MDICLIENT window class
											NULL,										// The window name
											WS_CHILD | WS_CLIPCHILDREN | WS_VISIBLE,	// Style parameters
											0,0,										// position (x,y)
											0,0,										// size (w,h)
											hWin,										// The frame window is the parent
											NULL,										// The menu (none at the moment)
											(HANDLE) ghInst,							// Instance that owns the window
											(LPSTR) &clientcreate						// The CLIENTCREATESTRUCT
											);

				shortcuts = AllocateProcessShortcutTable (MINSIZEPROCESSSHORTCUTTABLE);	// create a new shortcut table
				SetWindowLong (hWin, 0, (long) shortcuts);		//     and store it in the local memory of the window
				gAcceleratorTableIsUpToDate = FALSE;			// The active global accelerator table is not up to date

				ghActiveFrameWindow  = hWin;					// Keep track of the active frame window
				ghActiveClientWindow = hwndClient;				//	and client window

			} return 0;
		case WM_CLOSE:
			{
				SendMessage1ToClean (CcWmPROCESSCLOSE, hWin);
				return 0;
			}
			break;
		case WM_DESTROY:	/*	The frame is in the act of being closed. */
			{
				ProcessShortcutTable shortcuts;

				shortcuts = (ProcessShortcutTable) GetWindowLong (hWin, 0);	// get the local shortcut table
				DestroyProcessShortcutTable (shortcuts);					// and destroy it.
				gAcceleratorTableIsUpToDate = FALSE;	// The active global accelerator table is not up to date

				DragAcceptFiles (hWin,FALSE);			/* Unregister for WM_DROPFILES events. */
			}
			break;
		/*
		case WM_ACTIVATE:	// This alternative should only administer the current active frame/client window.
			{
				if (LOWORD (wPara)!=WA_INACTIVE)
				{
					ghActiveFrameWindow  = hWin;
					ghActiveClientWindow = GetWindow (ghActiveFrameWindow,GW_CHILD);
					ghTopDocWindow       = GetWindow (ghActiveClientWindow,GW_CHILD);
				}
				else
				{
					ghTopDocWindow       = NULL;
					ghActiveFrameWindow  = NULL;
					ghActiveClientWindow = NULL;
					gAcceleratorTableIsUpToDate = FALSE;	// The active global accelerator table is not up to date
				}
			}
			break;
		*/
		case WM_NOTIFY:
			{
				LPNMHDR pnmh        = (LPNMHDR) lPara;
				LPTOOLTIPTEXT lpttt = (LPTOOLTIPTEXT) lPara;
				UINT from, flags;

				from = lpttt->hdr.idFrom;
				flags= lpttt->uFlags;

				if (pnmh->code == TTN_NEEDTEXT && from != 0 && flags != TTF_IDISHWND)
				{
					HWND hwndToolbar;

					hwndToolbar = (HWND)GetGWL_USERDATA (hWin);

					//	get tooltip text from Clean
					SendMessage2ToClean (CcWmGETTOOLBARTIPTEXT, hwndToolbar, from);
					lstrcpy (lpttt->szText,(LPSTR)gCci.p1);
					if (gCci.p1 != 0)
						rfree ((HGLOBAL) gCci.p1);
				}
			}
			break;
		case WM_SIZE:
			{
				HWND hwndToolbar;

				hwndToolbar = (HWND)GetGWL_USERDATA (hWin);
				if (hwndToolbar != NULL)
					SendMessage ((HWND)GetGWL_USERDATA (hWin), TB_AUTOSIZE, (WPARAM)0, (LPARAM)0);
			}
			break;
		/*	Accept the user dropping file(s) in the frame window. */
		case WM_DROPFILES:
			{
				SendDropFilesToClean (hWin,wPara);
			}
			break;
		default:
			return DefFrameProc (hWin, GetWindow (hWin,GW_CHILD), uMess, wPara, lPara);
			break;
	}
	return DefFrameProc (hWin, GetWindow (hWin,GW_CHILD), uMess, wPara, lPara);
}	/* MDIFrameProcedure */


/*********************************************************************************************
	The callback routine for subclassing the client window of a MDI frame window.
	This routine catches only WM_WINDOWPOSCHANGING event.
*********************************************************************************************/
static LRESULT CALLBACK MDIClientProcedure (HWND hwnd,UINT uMess,WPARAM wParam,LPARAM lParam)
{
	switch (uMess)
	{
		case WM_WINDOWPOSCHANGING:
			{
				WINDOWPOS* wp;
				RECT tbRect;
				int tbHeight;
				HWND hwndFrame, hwndToolbar;

				wp = (LPWINDOWPOS)lParam;

				hwndFrame = GetParent (hwnd);
				hwndToolbar = (HWND)GetGWL_USERDATA (hwndFrame);

				if (hwndToolbar==0)
				{
					tbHeight = 0;
				} else
				{
					GetWindowRect (hwndToolbar,&tbRect);
					tbHeight = tbRect.bottom - tbRect.top;
				}
				wp->y  = tbHeight;
				wp->cy = wp->cy - tbHeight;

				return 0;
			}
			break;
	}
	return CallWindowProc ((WNDPROC) stdMDIClientCallback, hwnd, uMess, wParam, lParam);
}	/* MDIClientProcedure */


/*	Initialisation:
*/
static void InitialiseCrossCallxDI (void)
{
	WNDCLASSEX wclass;

	/* register clean SDI frame class */
    wclass.cbSize        = sizeof (WNDCLASSEX);
	wclass.style         = 0;
	wclass.lpfnWndProc   = (WNDPROC) SDIFrameProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = sizeof (HANDLE);						// Allocate local memory for shortcut table pointer
	wclass.hInstance     = ghInst;
	wclass.hIcon         = LoadIcon (ghInst, IDI_APPLICATION);
	wclass.hCursor       = LoadCursor (ghInst, IDC_ARROW);
	wclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH);
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = SDIFrameClassName;
	wclass.hIconSm       = NULL;
	RegisterClassEx (&wclass);

	/* register clean MDI frame class */
    wclass.cbSize        = sizeof (WNDCLASSEX);
	wclass.style         = 0;
	wclass.lpfnWndProc   = (WNDPROC) MDIFrameProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = sizeof (HANDLE);						// Allocate local memory for shortcut table pointer
	wclass.hInstance     = ghInst;
	wclass.hIcon         = LoadIcon (ghInst, IDI_APPLICATION);
	wclass.hCursor       = LoadCursor (ghInst, IDC_ARROW);
	wclass.hbrBackground = (HBRUSH) (COLOR_APPWORKSPACE+1);		// For best results (Petzold)
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = MDIFrameClassName;
	wclass.hIconSm       = NULL;
	RegisterClassEx (&wclass);
}


/*	Create a SDI frame window. */
void EvalCcRqCREATESDIFRAMEWINDOW (CrossCallInfo *pcci)	/* accept file open; frame ptr, menubar results. */
{
	HWND    hwndFrame;
	BOOL    acceptFileOpen;
	DWORD   styleFlags;
	HMENU   menuBar;

	acceptFileOpen = (BOOL) pcci->p1;	/*	respond to file open events. */

	/* The frame style flags equal the styleFlags with the scrollbar flags masked out. */
	styleFlags   = WS_SYSMENU | WS_OVERLAPPED | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_THICKFRAME;

	/* Create the menubar. */
	menuBar = CreateMenu ();

	/* Create the window. */
	hwndFrame = CreateWindow (	SDIFrameClassName,				/* Class name						*/
								NULL,							/* No title yet						*/
								styleFlags,						/* SDI frame style flags			*/
								CW_USEDEFAULT,CW_USEDEFAULT,	/* No position yet					*/
								CW_USEDEFAULT,CW_USEDEFAULT, 	/* no size yet						*/
								NULL,							/* Parent window					*/
								menuBar,						/* menu handle						*/
								(HANDLE) ghInst,				/* Instance that owns the window	*/
								0);
	ShowWindow (hwndFrame, SW_SHOWNORMAL);
	UpdateWindow (hwndFrame);

	if (acceptFileOpen)
		DragAcceptFiles (hwndFrame,TRUE);		/* register for WM_DROPFILES events. */

	MakeReturn2Cci (pcci, (int) hwndFrame, (int) menuBar);
}

/*	Create MDI frame window. */
void EvalCcRqCREATEMDIFRAMEWINDOW (CrossCallInfo *pcci)	/* show, accept file open; frame ptr, client ptr, menubar, windowmenu results. */
{
	BOOL show, acceptFileOpen;
	DWORD styleFlags;
	HWND hwndFrame, hwndClient;
	HMENU menuBar, windowMenu;

	show           = (BOOL) pcci->p1;
	acceptFileOpen = (BOOL) pcci->p2;	/*	respond to file open events. */

	styleFlags = WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN;
	if (show)
		styleFlags |= WS_MAXIMIZE;
	else
		styleFlags |= WS_MINIMIZE;

	hwndFrame = CreateWindow (	MDIFrameClassName			// Class name
							 ,	(LPCTSTR) gAppName			// Title is the application name
							 ,	styleFlags					// Style parameters
							 ,	0,0							// Default position (x,y)
							 ,	CW_USEDEFAULT,CW_USEDEFAULT	// Default size (w,h)
							 ,	NULL						// Every window should be top-level
							 ,	NULL
							 ,	(HANDLE) ghInst				// Instance that owns the window
							 ,	NULL
							 );
	hwndClient = GetWindow (hwndFrame,GW_CHILD);	// retrieve the "MDICLIENT" window
	menuBar    = GetMenu (hwndFrame);				// retrieve the menu bar of the frame window
	windowMenu = GetSubMenu (menuBar,0);			// retrieve the "Window" menu of the menu bar
	if (show)
	{
		ShowWindow (hwndFrame,SW_MAXIMIZE);			// show the frame window (SW_MAXIMIZE gives best result)
		UpdateWindow (hwndFrame);					// update the frame window
	}
	DrawMenuBar (hwndFrame);						// update the menu bar

	if (acceptFileOpen)
		DragAcceptFiles (hwndFrame,TRUE);			/* register for WM_DROPFILES events. */

	/*	Store the standard Windows callback routine adress in stdMDIClientCallback
		and subclass the MDI client window with MDIClientProcedure.
	*/
	stdMDIClientCallback = SetWindowLong (hwndClient, GWL_WNDPROC, (LONG) MDIClientProcedure);

	MakeReturn4Cci (pcci,(int) hwndFrame,(int) hwndClient,(int) menuBar,(int) windowMenu);
}

void EvalCcRqDESTROYWINDOW (CrossCallInfo *pcci) /* hwnd; no result. */
{
	BOOL noError;

	noError = DestroyWindow ((HWND) pcci->p1);
	if (!noError)
	{
		rMessageBox (NULL,MB_APPLMODAL,"CcRqDESTROYWINDOW","DestroyWindow failed");
	}
	MakeReturn0Cci (pcci);
}

void EvalCcRqGETWINDOWPOS (CrossCallInfo *pcci)	/* hwnd;   width, heigth result */
{
	RECT rect;

	GetWindowRect ((HWND) pcci->p1, &rect);

	MakeReturn2Cci (pcci, rect.left, rect.top);
}

void EvalCcRqGETCLIENTSIZE (CrossCallInfo *pcci) /* hwnd;		width, height result.  */
{
	RECT rect;

	GetClientRect ((HWND) pcci->p1, &rect);

	MakeReturn2Cci (pcci, rect.right - rect.left, rect.bottom - rect.top);
}

/*	Create a toolbar in a window. */
void EvalCcRqCREATEMDITOOLBAR (CrossCallInfo *pcci)			/* hwnd, width, height; toolbarptr, full toolbar height result; */
{
	HWND hwndToolbar;
	HWND hwndParent;
	int  bmpWidth, bmpHeight, tbHeight;
	RECT tbRect;

	hwndParent  = (HWND) pcci->p1;	// The parent is the frame window
	bmpWidth    = pcci->p2;
	bmpHeight   = pcci->p3;

	hwndToolbar = CreateWindow (TOOLBARCLASSNAME,
								NULL,
								WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | CCS_TOP | TBSTYLE_TOOLTIPS,
								0,0,0,0,
								hwndParent,
								(HMENU) NULL,
								(HANDLE) ghInst,
								0
								);
	SetGWL_USERDATA ((LONG)hwndToolbar, hwndParent);	// Administrate the toolbar handle in the MDI frame parent handle
	SendMessage (hwndToolbar, TB_SETBITMAPSIZE, (WPARAM)0, (LPARAM)MAKELONG(bmpWidth,bmpHeight));
	SendMessage (hwndToolbar, TB_AUTOSIZE, (WPARAM)0, (LPARAM)0);
	SendMessage (hwndToolbar, TB_BUTTONSTRUCTSIZE, (WPARAM) sizeof (TBBUTTON), (LPARAM) 0);

	/*	MDI windows that will get a toolbar are initially created with the WS_MINIMIZE flag
		(see CcRqCREATEMDIFRAMEWINDOW).
		This is needed to ensure that the toolbar, after creation, becomes visible.
	*/
	ShowWindow (hwndParent,SW_MAXIMIZE);

	if (!GetWindowRect (hwndToolbar,&tbRect))
		rMessageBox (NULL,MB_APPLMODAL,"CcRqCREATEMDITOOLBAR","GetWindowRect failed");
	tbHeight = tbRect.bottom - tbRect.top;

	MakeReturn2Cci (pcci,(int)hwndToolbar,tbHeight);
}

/*	Create a toolbar in a SDI window. */
void EvalCcRqCREATESDITOOLBAR (CrossCallInfo *pcci)			/* hwnd, width, height; toolbarptr, full toolbar height result; */
{
	HWND hwndParent,hwndClient,hwndToolbar;
	int  bmpWidth, bmpHeight, tbHeight;
	RECT tbRect;

	hwndParent  = (HWND) pcci->p1;
	bmpWidth    = pcci->p2;
	bmpHeight   = pcci->p3;
	hwndClient  = GetSDIClientWindow (hwndParent);
	hwndToolbar = CreateWindow (TOOLBARCLASSNAME,
								NULL,
								WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | CCS_TOP | TBSTYLE_TOOLTIPS,
								0,0,0,0,
								hwndParent,
								(HMENU) NULL,
								(HANDLE) ghInst,
								0
							   );
	SetGWL_USERDATA ((LONG)hwndToolbar,hwndParent);	// Administrate the toolbar handle in the SDI window handle
	SendMessage (hwndToolbar, TB_SETBITMAPSIZE, (WPARAM)0, (LPARAM)MAKELONG(bmpWidth,bmpHeight));
	SendMessage (hwndToolbar, TB_AUTOSIZE, (WPARAM)0, (LPARAM)0);
	SendMessage (hwndToolbar, TB_BUTTONSTRUCTSIZE, (WPARAM) sizeof (TBBUTTON), (LPARAM) 0);

	if (!GetWindowRect (hwndToolbar,&tbRect))
		rMessageBox (NULL,MB_APPLMODAL,"CcRqCREATESDITOOLBAR","GetWindowRect failed");
	tbHeight = tbRect.bottom - tbRect.top;

	/*	Before showing the new toolbar, move the client window down and update its scrollbars. */
	if (hwndClient != NULL)
	{
		SetWindowPos (hwndClient,HWND_TOP,0,tbHeight,0,0,SWP_NOSIZE | SWP_NOZORDER);
		UpdateWindowScrollbars (hwndClient);
	}

	ShowWindow (hwndToolbar,SW_SHOWNORMAL);
	UpdateWindow (hwndToolbar);

	MakeReturn2Cci (pcci, (int) hwndToolbar, tbHeight);
}

/*	Create a bitmap toolbar item. */
void EvalCcRqCREATETOOLBARITEM (CrossCallInfo *pcci)		// hwnd, hbmp, index; no results;
{
	HWND hwndToolbar;
	HBITMAP hbmp;
	int index;
	TBADDBITMAP tbab;
	TBBUTTON    tbb;
	int iBitmap;

	hwndToolbar= (HWND)    pcci->p1;
	hbmp       = (HBITMAP) pcci->p2;
	index      = pcci->p3;

	SendMessage (hwndToolbar, TB_BUTTONSTRUCTSIZE, (WPARAM) sizeof (TBBUTTON), (LPARAM) 0);

	tbab.hInst = NULL;
	tbab.nID   = (UINT) hbmp;

	iBitmap = SendMessage (hwndToolbar, TB_ADDBITMAP, (WPARAM)1, (LPARAM)(LPTBADDBITMAP)&tbab);

	tbb.iBitmap   = iBitmap;
	tbb.idCommand = index;
	tbb.fsState   = (BYTE)TBSTATE_ENABLED;
	tbb.fsStyle   = (BYTE)TBSTYLE_BUTTON;
	tbb.dwData    = (DWORD)0;
	tbb.iString   = 0;

	SendMessage (hwndToolbar, TB_ADDBUTTONS,(WPARAM)(UINT)1, (LPARAM)(LPTBBUTTON)&tbb);

	MakeReturn0Cci (pcci);
}

/*	Create a separator toolbar item. */
void EvalCcRqCREATETOOLBARSEPARATOR (CrossCallInfo *pcci)	// hwnd; no results;
{
	HWND hwndToolbar;
	TBBUTTON tbb;

	hwndToolbar = (HWND) pcci->p1;

	tbb.iBitmap   = 0;
	tbb.idCommand = 0;
	tbb.fsState   = (BYTE)TBSTATE_ENABLED;
	tbb.fsStyle   = (BYTE)TBSTYLE_SEP;
	tbb.dwData    = (DWORD)0;
	tbb.iString   = 0;

	SendMessage (hwndToolbar, TB_BUTTONSTRUCTSIZE, (WPARAM) sizeof (TBBUTTON), (LPARAM) 0);
	SendMessage (hwndToolbar, TB_ADDBUTTONS,(WPARAM)(UINT)1, (LPARAM)(LPTBBUTTON)&tbb);

	MakeReturn0Cci (pcci);
}

/*	Install the cross call procedures in the gCrossCallProcedureTable of cCrossCall_121.
*/
void InstallCrossCallxDI ()
{
	CrossCallProcedureTable newTable;

	InitialiseCrossCallxDI ();

	newTable = EmptyCrossCallProcedureTable ();
	AddCrossCallEntry (newTable, CcRqCREATESDIFRAMEWINDOW,   EvalCcRqCREATESDIFRAMEWINDOW);
	AddCrossCallEntry (newTable, CcRqCREATEMDIFRAMEWINDOW,   EvalCcRqCREATEMDIFRAMEWINDOW);
	AddCrossCallEntry (newTable, CcRqDESTROYWINDOW,          EvalCcRqDESTROYWINDOW);
	AddCrossCallEntry (newTable, CcRqGETWINDOWPOS,           EvalCcRqGETWINDOWPOS);
	AddCrossCallEntry (newTable, CcRqGETCLIENTSIZE,          EvalCcRqGETCLIENTSIZE);
	AddCrossCallEntry (newTable, CcRqCREATEMDITOOLBAR,       EvalCcRqCREATEMDITOOLBAR);
	AddCrossCallEntry (newTable, CcRqCREATESDITOOLBAR,       EvalCcRqCREATESDITOOLBAR);
	AddCrossCallEntry (newTable, CcRqCREATETOOLBARITEM,      EvalCcRqCREATETOOLBARITEM);
	AddCrossCallEntry (newTable, CcRqCREATETOOLBARSEPARATOR, EvalCcRqCREATETOOLBARSEPARATOR);
	AddCrossCallEntries (gCrossCallProcedureTable, newTable);
}
