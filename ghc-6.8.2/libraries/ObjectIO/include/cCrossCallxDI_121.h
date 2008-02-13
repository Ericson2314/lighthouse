#include "util_121.h"


//	Global data with external references:
extern OSWindowPtr ghTopDocWindow;
extern int gComboSelection;
extern BOOL gInMouseDown;
extern BOOL gInKey;
extern int gCurChar;

/*	Registered Windows class names:
*/
extern char SDIFrameClassName[];		/* Class for SDI frames.  */
extern char MDIFrameClassName[];		/* Class for MDI frames.  */
extern char SDIWindowClassName[];		/* Class for SDI windows (must have same length as MDIWindowClassName). */
extern char MDIWindowClassName[];		/* Class for MDI windows (must have same length as SDIWindowClassName). */

/*	Managing the double down distance.
*/
extern void WinSetDoubleDownDist (int dd);

/*	Sending keyboard events to Clean thread:
*/
extern void SendKeyDownToClean      (OSWindowPtr hwndParent, OSWindowPtr hwndChild, int c);
extern void SendKeyStillDownToClean (OSWindowPtr hwndParent, OSWindowPtr hwndChild, int c);
extern void SendKeyUpToClean        (OSWindowPtr hwndParent, OSWindowPtr hwndChild, int c);

/*	Sending mouse events to Clean thread:
*/
extern void SendMouseUpToClean        (OSWindowPtr hwndParent, OSWindowPtr hwndChild, int x, int y);
extern void SendMouseStillDownToClean (OSWindowPtr hwndParent, OSWindowPtr hwndChild, int x, int y);
extern void SendMouseStillUpToClean   (OSWindowPtr hwndParent, OSWindowPtr hwndChild, int x, int y);
extern void SendMouseDownToClean      (OSWindowPtr hwndParent, OSWindowPtr hwndChild, int x, int y);

//	InstallCrossCallxDI adds the proper cross call procedures to the
//	cross call procedures managed by cCrossCall_121.c.
extern void InstallCrossCallxDI ();
