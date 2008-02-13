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
#include <sys/time.h>

void WinBeep ()
{
    gdk_beep();
}

int WinGetBlinkTime()
{
//	return (int) GetCaretBlinkTime();
    printf("WinGetBlinkTime -> not implemented\n");
    return 0;
}

int WinGetTickCount ()
{
    static struct timeval s;
    static gboolean f = TRUE;
    struct timeval r;
    
    if (f)
    {
	gettimeofday(&s,NULL);
	f = FALSE;
	return 0;
    }    
    
    gettimeofday(&r,NULL);
    return (r.tv_sec-s.tv_sec)*1000 + (r.tv_usec-s.tv_usec)/1000;
}

gboolean WinPlaySound (char *filename)
{
//	return PlaySound(filename, NULL, SND_FILENAME | SND_SYNC);
    printf("WinPlaySound -> not implemented");
    return 0;
}
