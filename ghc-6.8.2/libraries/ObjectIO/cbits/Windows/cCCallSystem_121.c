/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Routines related to system handling that is not part of standard cross call handling.
********************************************************************************************/
#include "cCCallSystem_121.h"

void WinBeep ()
{
	MessageBeep (MB_ICONASTERISK);
}

int WinGetBlinkTime()
{
	return (int) GetCaretBlinkTime();
}

int WinGetTickCount ()
{
	return GetTickCount ();
}

BOOL WinPlaySound (char *filename)
{
	return PlaySound(filename, NULL, SND_FILENAME | SND_SYNC);
}
