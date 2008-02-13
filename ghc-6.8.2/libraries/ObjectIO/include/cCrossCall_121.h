#include "util_121.h"
#include "cCrossCallProcedureTable_121.h"
#include "cTCP_121.h"


//	Global data with external references:
extern CrossCallInfo gCci;									/* The global cross call information struct. */
extern int gClipboardCount;									/* Keeps track of changes of clipboard. */
extern CrossCallProcedureTable gCrossCallProcedureTable;	                                /* The cross call procedure table. */
extern OSWindowPtr gTooltip; 	                                                                /* The tooltip control. */

#if defined(mingw32_HOST_OS)
extern char *gAppName;										/* The application name. */
extern HINSTANCE ghInst;									/* The handle to the instance of the OS thread. */
extern HWND ghMainWindow;									/* The handle to the main HWND of the OS thread. */
extern HACCEL gAcceleratorTable;							/* Refers to the accelerator table of the active frame. */
extern BOOL gAcceleratorTableIsUpToDate;					/* Flag: TRUE iff accelerator table corresponds with active frame. */
extern HWND ghActiveFrameWindow;							/* The currently active frame window (MDI/SDI). */
extern HWND ghActiveClientWindow;							/* The currently active client window (MDI). */
extern HWND gActiveDialog;									/* The currently active dialogue. */
extern HWND ghwndLastModalDialog;							/* Keeps track of last modal dialog. */
extern HFONT gDlogFont;										/* The handle to the logical FONT that is used in dialogs. */
extern HFONT gControlFont;									/* The handle to the logical FONT that is used in all controls. */
extern HWND ghTCPWindow;									/* The handle to the TCP HWND of the OS thread. */


/*	Menu(item)IDs are not allowed to exceed OSMenuIDEnd.
	This is because window ids start at (OSMenuIDEnd+5), and need to be distinct from menu ids
	in case of MDI processes.
	The global gMenuItemID (initially 0) is incremented by NextMenuItemID each time a new
	menu(item)ID is required.
	This implementation does not reuse freed ids and is therefore not adequate!!
*/
#define OSMenuIDEnd		10000
extern UINT NextMenuItemID (void);
#endif


/*	GetModifiers returns the modifiers that are currently pressed.
*/
extern int GetModifiers (void);

/*	Translate virtual key codes to the codes shared with Clean.
	If the keycode could not be translated, zero is returned.
*/
extern int CheckVirtualKeyCode (int keycode);


extern void HandleCleanRequest( CrossCallInfo *pcci );
extern void WinStartOsThread ();
extern void WinKillOsThread ();
extern void WinKickOsThread (int,int,int,int,int,int,int,int*,int*,int*,int*,int*,int*,int*);
extern void KickCleanThread( CrossCallInfo *pcci );

extern void SendMessageToClean( int mess, int p1,int p2,int p3, int p4,int p5,int p6 );

//	Shorthands for SendMessageToClean:
#define SendMessage0ToClean(mess)                    SendMessageToClean((mess), 0,0,0,0,0,0)
#define SendMessage1ToClean(mess, p1)                SendMessageToClean((mess), (int)(p1),0,0,0,0,0)
#define SendMessage2ToClean(mess, p1,p2)             SendMessageToClean((mess), (int)(p1),(int)(p2),0,0,0,0)
#define SendMessage3ToClean(mess, p1,p2,p3)          SendMessageToClean((mess), (int)(p1),(int)(p2),(int)(p3),0,0,0)
#define SendMessage4ToClean(mess, p1,p2,p3,p4)       SendMessageToClean((mess), (int)(p1),(int)(p2),(int)(p3),(int)(p4),0,0)
#define SendMessage5ToClean(mess, p1,p2,p3,p4,p5)    SendMessageToClean((mess), (int)(p1),(int)(p2),(int)(p3),(int)(p4),(int)(p5),0)
#define SendMessage6ToClean(mess, p1,p2,p3,p4,p5,p6) SendMessageToClean((mess), (int)(p1),(int)(p2),(int)(p3),(int)(p4),(int)(p5),(int)(p6))

//	Prototypes of convenience functions that fill CrossCallInfo struct.
extern CrossCallInfo *MakeReturn0Cci (CrossCallInfo * pcci);
extern CrossCallInfo *MakeReturn1Cci (CrossCallInfo * pcci, int v);
extern CrossCallInfo *MakeReturn2Cci (CrossCallInfo * pcci, int v1, int v2);
extern CrossCallInfo *MakeReturn3Cci (CrossCallInfo * pcci, int v1, int v2, int v3);
extern CrossCallInfo *MakeReturn4Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4);
extern CrossCallInfo *MakeReturn5Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4, int v5);
extern CrossCallInfo *MakeReturn6Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4, int v5, int v6);

extern BOOL IsReturnCci( CrossCallInfo *pcci );
