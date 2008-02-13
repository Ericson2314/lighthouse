#include "util_121.h"

extern void WinInvalidateWindow (OSWindowPtr wnd);
extern void WinInvalidateRect (OSWindowPtr wnd, int left, int top, int right, int bottom);
extern void WinValidateRect (OSWindowPtr wnd, int left, int top, int right, int bottom);
extern void WinValidateRgn (OSWindowPtr wnd, OSRgnHandle rgn);

/*	Win(M/S)DIClientToOuterSizeDims returns the width and height needed to add/subtract
	from the client/outer size to obtain the outer/client size.
	These values must be the same as used by W95AdjustClean(M/S)DIWindowDimensions!
*/
extern void WinMDIClientToOuterSizeDims (int styleFlags, int *dw, int *dh);
extern void WinSDIClientToOuterSizeDims (int styleFlags, int *dw, int *dh);

/*	UpdateWindowScrollbars updates any window scrollbars and non-client area if present.
*/
extern void UpdateWindowScrollbars (OSWindowPtr hwnd);

/*	Access procedures to dimensions:
*/
extern int WinScreenYSize ();
extern int WinScreenXSize ();
extern void WinMinimumWinSize (int *mx, int *my);
extern void WinScrollbarSize (int *width, int *height);
extern void WinMaxFixedWindowSize (int *mx, int *my);
extern void WinMaxScrollWindowSize (int *mx, int *my);
