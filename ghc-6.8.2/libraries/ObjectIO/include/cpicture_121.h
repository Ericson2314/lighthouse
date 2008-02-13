#include "util_121.h"
#include "intrface_121.h"
#include <math.h>


extern OSPictContext WinGetDC (OSWindowPtr);
extern void WinReleaseDC (OSWindowPtr,OSPictContext);
extern int OsMMtoVPixels(double);
extern int OsMMtoHPixels(double);

extern void WinInitPicture (int,int,int,int,int,int,int,int,char *,int,int,OSRgnHandle,BOOL,OSPictContext);
extern void WinDonePicture (OSPictContext);

extern void WinClipRgnPicture(OSRgnHandle,OSPictContext);
extern void WinSetClipRgnPicture (OSRgnHandle,OSPictContext);
extern OSRgnHandle WinGetClipRgnPicture (OSPictContext);

/*	Operations to create, modify, and destroy polygon shapes.
*/
extern PointsArray WinAllocPolyShape (int);
extern void   WinSetPolyPoint (int,int,int,PointsArray);
extern void   WinFreePolyShape (PointsArray);

/*	Operations to create, modify and destroy regions.
*/
extern OSRgnHandle WinCreateEmptyRgn();
extern OSRgnHandle WinCreateRectRgn(int,int,int,int);
extern OSRgnHandle WinCreatePolygonRgn(PointsArray,int,int);
extern OSRgnHandle WinSetRgnToRect(int,int,int,int,OSRgnHandle);
extern OSRgnHandle WinUnionRgn(OSRgnHandle rgn1, OSRgnHandle rgn2);
extern OSRgnHandle WinSectRgn(OSRgnHandle rgn1, OSRgnHandle rgn2);
extern OSRgnHandle WinDiffRgn(OSRgnHandle rgn1, OSRgnHandle rgn2);
extern OSRgnHandle WinXorRgn (OSRgnHandle rgn1, OSRgnHandle rgn2);
extern void WinGetRgnBox(OSRgnHandle,int*,int*,int*,int*,BOOL*);
extern BOOL WinIsEmptyRgn(OSRgnHandle rgn);
extern void WinDisposeRgn(OSRgnHandle rgn);

extern void WinSetPenSize (int,OSPictContext);
extern void WinSetPenColor (int,int,int,OSPictContext);
extern void WinSetBackColor (int,int,int,OSPictContext);
extern void WinSetMode (int,OSPictContext);
extern void WinSetPattern (int,OSPictContext);

extern void WinDrawPoint (int,int,OSPictContext);
extern void WinDrawLine (int,int,int,int,OSPictContext);
extern void WinUndrawLine(int,int,int,int,OSPictContext);
extern void WinDrawCurve (int,int,int,int,float,float,BOOL,OSPictContext);
extern void WinUndrawCurve (int,int,int,int,float,float,BOOL,OSPictContext);

extern void WinDrawChar (int,int,char,OSPictContext);
extern void WinUndrawChar (int,int,char,OSPictContext);
extern void WinDrawString (int,int,char*,OSPictContext);
extern void WinUndrawString (int,int,char*,OSPictContext);

extern void WinDrawRectangle (int,int,int,int,OSPictContext);
extern void WinUndrawRectangle (int,int,int,int,OSPictContext);
extern void WinFillRectangle (int,int,int,int,OSPictContext);
extern void WinEraseRectangle (int,int,int,int,OSPictContext);
extern void WinInvertRectangle (int,int,int,int,OSPictContext);
extern void WinMoveRectangleTo (int,int,int,int,int,int,OSPictContext);
extern void WinMoveRectangle (int,int,int,int,int,int,OSPictContext);
extern void WinCopyRectangleTo (int,int,int,int,int,int,OSPictContext);
extern void WinCopyRectangle (int,int,int,int,int,int,OSPictContext);
extern void WinScrollRectangle (int,int,int,int,int,int,OSPictContext,int*,int*,int*,int*);

extern void WinDrawOval (int,int,int,int,OSPictContext);
extern void WinUndrawOval (int,int,int,int,OSPictContext);
extern void WinFillOval (int,int,int,int,OSPictContext);
extern void WinEraseOval (int,int,int,int,OSPictContext);
extern void WinInvertOval (int,int,int,int,OSPictContext);

extern void WinFillWedge (int,int,int,int,float,float,BOOL,OSPictContext);
extern void WinEraseWedge (int,int,int,int,float,float,BOOL,OSPictContext);
extern void WinInvertWedge (int,int,int,int,float,float,BOOL,OSPictContext);

extern void WinStartPolygon (int);
extern void WinEndPolygon ();
extern void WinAddPolygonPoint (int,int);
extern void WinDrawPolygon (OSPictContext);
extern void WinUndrawPolygon (OSPictContext);
extern void WinFillPolygon (OSPictContext);
extern void WinErasePolygon (OSPictContext);
extern void WinInvertPolygon (OSPictContext);

//	Routines that temporarily create and destroy a DISPLAY OSPictContext. Use this OSPictContext only locally.
extern OSPictContext WinCreateScreenHDC ();
extern void WinDestroyScreenHDC (OSPictContext);

extern void WinDrawResizedBitmap (int,int,int,int,int,int,OSBmpHandle,OSPictContext);
extern void WinDrawBitmap (int,int,int,int,OSBmpHandle,OSPictContext);
extern OSBmpHandle WinCreateBitmap (char *, int *, int *);
extern void WinDisposeBitmap(OSBmpHandle);

extern void WinSetFont (char *,int,int,OSPictContext);
extern void WinGetFontInfo (char *,int,int,OSPictContext,int*,int*,int*,int*);
extern void WinGetPicFontInfo (OSPictContext,int*,int*,int*,int*);

extern int WinGetPicStringWidth (char *,OSPictContext);
extern int WinGetPicCharWidth (char,OSPictContext);
extern int WinGetStringWidth (char *,char *,int,int,OSPictContext);
extern int WinGetCharWidth (char,char *,int,int,OSPictContext);

//	Get the resolution of a picture
extern void getResolutionC(OSPictContext,int*,int*);

//	Get scaling factors, which have to be applied to coordinates for clipping regions in case
//	of emulating the screen resolution for printing (MM_ISOTROPIC)
extern void WinGetPictureScaleFactor(OSPictContext,int*,int*,int*,int*);

void WinDialogFontDef(char **fname, int *fstyle, int *fsize);
void WinDefaultFontDef(char **fname, int *fstyle, int *fsize);
void WinSerifFontDef(char **fname, int *fstyle, int *fsize);
void WinSansSerifFontDef(char **fname, int *fstyle, int *fsize);
void WinSmallFontDef(char **fname, int *fstyle, int *fsize);
void WinNonProportionalFontDef(char **fname, int *fstyle, int *fsize);
void WinSymbolFontDef(char **fname, int *fstyle, int *fsize);
