#ifndef _UTILH
#define _UTILH

#include "ghcconfig.h"

#if defined(mingw32_HOST_OS)

#include <windows.h>
#include <mmsystem.h>
#include <shlobj.h>

typedef HWND OSWindowPtr;
typedef struct
	{
		HDC hDC;
		HDC hBufferedDC;
		HBITMAP hBufferBitmap;

		HPEN theNormalPen;
		HPEN theBackPen;
		HBRUSH theNormalBrush;
		HBRUSH theBackBrush;
		HFONT theFont;

		int penSize;
		int penPat;
		int penMode;
		COLORREF penColor;
		COLORREF backColor;
		char curFont[LF_FACESIZE];
		int fontstyle;
		int fontsize;

		int lastActivity;
	} *OSPictContext;
typedef HRGN OSRgnHandle;
typedef HBITMAP OSBmpHandle;
typedef POINT *PointsArray;

#else

#include <gtk/gtk.h>
#include <gdk/gdk.h>

typedef GtkWidget   *OSWindowPtr;
typedef GdkDrawable *OSPictContext;
typedef GdkRegion   *OSRgnHandle;
typedef GdkPixbuf   *OSBmpHandle;
typedef GdkPoint    *PointsArray;

typedef gboolean BOOL;

#endif




#define SIGNEDLOWORD(i)  ((short) i)
#define SIGNEDHIWORD(i)  ((short) ((i)>>16))


/*  OS type, threading all calls from Clean.
*/

typedef int HITEM;

typedef struct
{   int  mess;
    int  p1;
    int  p2;
    int  p3;
    int  p4;
    int  p5;
    int  p6;
} CrossCallInfo;

#include "intrface_121.h"

/*  since we don't use the C runtime library, here are some simple
    routines that would normally come from the C runtime lib.
*/
// PA: extern added
extern void rfree(void *ptr);
extern void *rmalloc(unsigned long bytes);

/*  clean_strings don't have to end with 0, so we have to make
    copy the clean string and end it with a 0.
    global variables used for conversion from c strings to clean strings
*/

#if defined(mingw32_HOST_OS)

//	PA: extern added to the end
extern int nCopyAnsiToWideChar (LPWORD, LPSTR);

/*  The following routines are used to write to the console, or convey runtime errors
    with message boxes.
*/

#ifndef _RPRINTBUFSIZE
#define _RPRINTBUFSIZE 512
#endif

extern void rMessageBox(HWND owner, UINT style, char *title, char *format, ... );
extern void CheckF(BOOL theCheck, char *checkText, char *checkMess, char *filename, int linenum);
extern void ErrorExit(char *format, ...);

#define Check(check,mess) CheckF((check),(#check),(mess),__FILE__,__LINE__)

#endif

#endif
