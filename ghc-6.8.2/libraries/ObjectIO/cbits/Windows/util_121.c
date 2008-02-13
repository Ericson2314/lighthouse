/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Generally applicable utility routines.
********************************************************************************************/
#include "util_121.h"
#include <stdarg.h>


//	Convenience procedure to fill in LOGFONT struct.
void SetLogFontData (LOGFONT * plf, char *fname, int style, int size)
{
	plf->lfHeight    = -size;
	plf->lfWeight    = (style & iBold) ? 700 : 400;
	plf->lfItalic    = (style & iItalic) ? TRUE : FALSE;
	plf->lfUnderline = (style & iUnderline) ? TRUE : FALSE;
	plf->lfStrikeOut = (style & iStrikeOut) ? TRUE : FALSE;

	strcpy(plf->lfFaceName, fname);

	plf->lfWidth          = 0;
	plf->lfEscapement     = 0;
	plf->lfOrientation    = 0;
	plf->lfCharSet        = DEFAULT_CHARSET;
	plf->lfOutPrecision   = OUT_DEFAULT_PRECIS;
	plf->lfClipPrecision  = CLIP_DEFAULT_PRECIS;
	plf->lfQuality        = DEFAULT_QUALITY;
	plf->lfPitchAndFamily = DEFAULT_PITCH | FF_DONTCARE;
}	/* SetLogFontData */


/*	since we don't use the C runtime library, here are some simple
	routines that would normally come from the C runtime lib.
*/

void *rmalloc (DWORD bytes)
{
	void *ptr = malloc (bytes);

	if (!ptr)
	{
		MessageBeep (0xFFFFFFFF);
		ExitProcess (255);
	}

	return ptr;
}

void rfree (HGLOBAL ptr)
{
	free(ptr);
}

int nCopyAnsiToWideChar (LPWORD lpWCStr, LPSTR lpAnsiIn)
{
	int nChar = 0;

	do
	{
		*lpWCStr++ = (WORD) * lpAnsiIn;
		nChar++;
	} while (*lpAnsiIn++);

	return nChar;
}	/* nCopyAnsiToWideChar */


/*	The following routines are used to write to the console, or convey runtime errors
	with message boxes.
*/
void rMessageBox (HWND owner, UINT style, char *title, char *format,...)
{
	va_list arglist;
	char mbuff[_RPRINTBUFSIZE];

	va_start (arglist, format);
	wvsprintf (mbuff, format, arglist);
	va_end (arglist);

	MessageBox (owner, mbuff, title, style);
}	/* rMessageBox */

void CheckF (BOOL theCheck, char *checkText, char *checkMess,
		char *filename, int linenum)
{
	if (!theCheck)
	{
		rMessageBox (NULL, MB_OK | MB_ICONSTOP,
			 "Internal check failed", "%s\n\ncheck: %s\nfile: %s\nline: %d",
					 checkMess, checkText, filename, linenum);
		ExitProcess (1);
	}
}	/* CheckF */

void ErrorExit (char *format,...)
{
	va_list arglist;
	char mbuff[_RPRINTBUFSIZE];

	va_start (arglist, format);
	wvsprintf (mbuff, format, arglist);
	va_end (arglist);

	MessageBox (NULL, mbuff, NULL, MB_OK | MB_ICONSTOP);
	ExitProcess (1);
}	/* ErrorExit */
