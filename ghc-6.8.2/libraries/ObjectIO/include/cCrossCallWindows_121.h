#include "util_121.h"

extern OSWindowPtr ghCaretWnd;

extern void DeleteCursors(); // Delete all created mouse cursors

//	InstallCrossCallFileSelectors adds the proper cross call procedures to the
//	cross call procedures managed by cCrossCall_121.c.
extern void InstallCrossCallWindows ();
