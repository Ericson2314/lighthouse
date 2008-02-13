/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Routines related to drawing.
********************************************************************************************/
#include "cpicture_121.h"
#include "cCrossCall_121.h"
#include "cCrossCallWindows_121.h"

OSPictContext WinGetDC (HWND hwnd)
{
	OSPictContext context;
	context = malloc(sizeof(*context));
	memset(context, 0, sizeof(*context));
	context->hDC = GetDC(hwnd);
	return context;
}	/* WinGetDC */

void WinReleaseDC (HWND hwnd, OSPictContext context)
{
	ReleaseDC(hwnd, context->hDC);
	free(context);
}	/* WinReleaseDC */

int WinGetVertResolution (void)
{
	static int res = 0;

	if (res == 0)
	{
		HDC screen;
		screen = CreateDC ("DISPLAY", NULL, NULL, NULL);
		if (screen==NULL)
			rMessageBox (NULL,MB_APPLMODAL,"WinGetVertResolution","CreateDC returned NULL.");
		res = GetDeviceCaps (screen, LOGPIXELSY);
		DeleteDC (screen);
	};

	return res;
}	/* WinGetVertResolution */

int WinGetHorzResolution (void)
{
	static int res = 0;

	if (res == 0)
	{
		HDC screen;
		screen = CreateDC ("DISPLAY", NULL, NULL, NULL);
		res = GetDeviceCaps (screen, LOGPIXELSX);
		DeleteDC (screen);
	};

	return res;
}	/* WinGetHorzResolution */

int OsMMtoVPixels(double mm)
{
	return (int) ((mm/25.4) * WinGetVertResolution());
}

int OsMMtoHPixels(double mm)
{
	return (int) ((mm/25.4) * WinGetHorzResolution());
}

/*------------------------------------*\
|									   |
|	   Helper functions 			   |
|									   |
\*------------------------------------*/

static POINT *thePolygon;
static int thePolygonIndex;

#define DRAWING    0
#define UNDRAWING  1
#define FILLING    2
#define INVERTING  3
#define ERASING    4

// MW...
static int PointsToPix(HDC hdc, int size)
{
	// convert font size in points to pixels (which depends on the device resolution)

    int vRes;
	int mapMode = GetMapMode(hdc);
	if (mapMode==MM_ISOTROPIC)
		vRes = WinGetVertResolution();
	  else
		vRes = GetDeviceCaps(hdc, LOGPIXELSY);
	return (size * vRes) / 72;
/*	MW: currently, the MM_ISOTROPIC mapping mode is only used for printing with the emulation
	of the screen resolution. For that purpose, points are not subject to the scaling, which
	MM_ISOTROPIC performs.
*/
}
// ...MW

void WinInitPicture (int size, int mode,
					 int pr, int pg, int pb,
					 int br, int bg, int bb,
					 char *fname, int fstyle, int fsize,
					 HRGN clipRgn,
					 BOOL bDoubleBuffered,
					 OSPictContext context
				    )
{
	LOGBRUSH lb;
	LOGFONT lf;
	DWORD style;

	context->penSize = size;
	context->penPat = iBlackPattern;
	context->penMode = mode;
	context->penColor = RGB (pr, pg, pb);
	context->backColor = RGB (br, bg, bb);
	context->lastActivity = FILLING;

	if (bDoubleBuffered)
	{
		RECT clipRect;

		if (GetClipBox(context->hDC,&clipRect) == ERROR)
		{
			printf("osInitPicture -> GetClipBox failed\n");
			exit(1);
		}

		context->hBufferedDC = context->hDC;
		context->hDC = CreateCompatibleDC(context->hBufferedDC);
		context->hBufferBitmap = CreateCompatibleBitmap(context->hBufferedDC,clipRect.right-clipRect.left,clipRect.bottom-clipRect.top);
		SelectObject(context->hDC, context->hBufferBitmap);

		if (!BitBlt(context->hDC, 0, 0, clipRect.right-clipRect.left, clipRect.bottom-clipRect.top, context->hBufferedDC, clipRect.left, clipRect.top, SRCCOPY))
		{
			printf("osDonePicture -> BitBlt failed\n");
			exit(1);
		}

		SetViewportOrgEx(context->hDC,-clipRect.left,-clipRect.top,NULL);
	}


	thePolygon = NULL;
	SetPolyFillMode (context->hDC, WINDING);

	strcpy (context->curFont, fname);
	context->fontstyle = fstyle;
	context->fontsize = PointsToPix(context->hDC,fsize);
				// PointsToPix by MW

	SetLogFontData (&lf, context->curFont, context->fontstyle, context->fontsize);

	if (context->penSize == 1)
		style = PS_COSMETIC | PS_SOLID;
	else
		style = PS_GEOMETRIC | PS_INSIDEFRAME;

	lb.lbStyle = BS_SOLID;
	lb.lbColor = context->penColor;
	lb.lbHatch = 0;
	context->theNormalPen = ExtCreatePen (style, context->penSize, &lb, 0, NULL);

	lb.lbStyle = BS_SOLID;
	lb.lbColor = context->backColor;
	lb.lbHatch = 0;
	context->theBackPen = ExtCreatePen (style, context->penSize, &lb, 0, NULL);

	SetBrushOrgEx (context->hDC,0,0,NULL);
	context->theNormalBrush = CreateSolidBrush (context->penColor);
	context->theBackBrush = CreateSolidBrush (context->backColor);
	context->theFont = CreateFontIndirect (&lf);

	SaveDC (context->hDC);

	SetWindowOrgEx (context->hDC, 0,0, NULL);

	SelectObject (context->hDC, GetStockObject (NULL_PEN));
	SelectObject (context->hDC, context->theNormalBrush);
	SelectObject (context->hDC, context->theFont);

	SetBkMode (context->hDC, TRANSPARENT);
	SetBkColor (context->hDC, context->backColor);
	SetTextAlign (context->hDC, TA_LEFT | TA_BASELINE);
	WinSetMode (context->penMode, context);
	SetStretchBltMode (context->hDC,COLORONCOLOR);		/* PA: when stretching bitmaps, use COLORONCOLOR mode. */

	if (ghCaretWnd)
	{
		int mess, p1, p2, p3, p4, p5, p6;
		WinKickOsThread (CcRqHIDECARET, (int) ghCaretWnd, 0, 0, 0, 0, 0,
						 &mess, &p1, &p2, &p3, &p4, &p5, &p6);
	}

	if (clipRgn != NULL)
		SelectClipRgn (context->hDC, clipRgn);
}	/* WinInitPicture */

void WinDonePicture (OSPictContext context)
{
	if (context->hBufferedDC)
	{
		RECT clipRect;

		if (GetClipBox(context->hBufferedDC,&clipRect) == ERROR)
		{
			printf("osInitPicture -> GetClipBox failed\n");
			exit(1);
		}

		if (!BitBlt(context->hBufferedDC, clipRect.left, clipRect.top, clipRect.right-clipRect.left, clipRect.bottom-clipRect.top, context->hDC, clipRect.left, clipRect.top, SRCCOPY))
		{
			printf("osDonePicture -> BitBlt failed\n");
			exit(1);
		}

		DeleteDC(context->hDC);
		context->hDC = context->hBufferedDC;
		context->hBufferedDC = NULL;

		DeleteObject(context->hBufferBitmap);
		context->hBufferBitmap = NULL;
	}

	RestoreDC (context->hDC, -1);
	DeleteObject (context->theNormalPen);
	DeleteObject (context->theBackPen);
	DeleteObject (context->theNormalBrush);
	DeleteObject (context->theBackBrush);
	DeleteObject (context->theFont);

	if (ghCaretWnd)
	{
		int mess, p1, p2, p3, p4, p5, p6;
		WinKickOsThread (CcRqSHOWCARET, (int) ghCaretWnd, 0, 0, 0, 0, 0,
						 &mess, &p1, &p2, &p3, &p4, &p5, &p6);
	}
}	/* WinDonePicture */

/*	PA: Set and get the clipping region of a picture:
		WinClipRgnPicture    takes the intersection of the argument clipRgn with the current clipping region.
		WinSetClipRgnPicture sets the argument clipRgn as the new clipping region.
		WinGetClipRgnPicture gets the current clipping region.
*/
void WinClipRgnPicture (HRGN cliprgn, OSPictContext context)
{
	int error;

	error = ExtSelectClipRgn (context->hDC, cliprgn, RGN_AND);

	if (error==ERROR)
		ErrorExit ("Fatal error in WinClipRgnPicture: ExtSelectClipRgn returned ERROR.");
}	/* WinClipRgnPicture */

void WinSetClipRgnPicture (HRGN cliprgn, OSPictContext context)
{
	int error;

	error = SelectClipRgn(context->hDC, cliprgn);

	if (error==ERROR)
		ErrorExit ("Fatal error in WinSetClipRgnPicture: SelectClipRgn returned ERROR.");
}	/* WinSetClipRgnPicture */

HRGN WinGetClipRgnPicture (OSPictContext context)
{
	HRGN theRegion;
	int error;

	theRegion = CreateRectRgn (0,0, 1,1);
	if (theRegion==NULL)
		ErrorExit ("Fatal error in WinGetClipRgnPicture: CreateRectRgn returned NULL.");

	error = GetClipRgn (context->hDC, theRegion);

	if (error==0)
	{
		DeleteObject (theRegion);
		theRegion = NULL;
	}
	if (error==-1)
		ErrorExit ("Fatal error in WinGetClipRgnPicture: GetClipRgn returned -1.");

	return theRegion;
}	/* WinGetClipRgnPicture */


/*	Operations to create, modify, and destroy polygon shapes.
*/

POINT *WinAllocPolyShape (int size)
{
	return (POINT *) rmalloc (size * sizeof (POINT));
}	/* WinAllocPolyShape */

void WinSetPolyPoint (int i, int x, int y, POINT *shape)
{
	shape[i].x = x;
	shape[i].y = y;
}	/* WinSetPolyPoint */

void WinFreePolyShape (POINT *shape)
{
	rfree (shape);
}	/* WinFreePolyShape */


/*	Operations to create, modify and destroy regions.
*/
HRGN WinCreateEmptyRgn()
{
	return CreateRectRgn (0, 0, 1, 1);
}	/* WinCreateRectRgn */

HRGN WinCreateRectRgn(int nLeftRect, int nTopRect, int nRightRect, int nBottomRect)
{
	HRGN theRegion;

	theRegion = CreateRectRgn (nLeftRect, nTopRect, nRightRect, nBottomRect);
	if (theRegion==NULL)
		ErrorExit ("Fatal error in WinCreateRectRgn: CreateRectRgn returned NULL.");

	return theRegion;
}	/* WinCreateRectRgn */

HRGN WinCreatePolygonRgn (POINT * lppt, int cPoints, int fnPolyFillMode)
{
	HRGN theRegion;

	theRegion = CreatePolygonRgn (lppt, cPoints, fnPolyFillMode == 1 ? ALTERNATE : WINDING);
	if (theRegion==NULL)
		ErrorExit ("Fatal error in WinCreatePolygonRgn: CreatePolygonRgn returned NULL.");

	return theRegion;
}	/* WinCreatePolygonRgn */

HRGN WinUnionRgn (HRGN hrgnSrc1, HRGN hrgnSrc2)
{
	HRGN hrgnDest = CreateRectRgn(0, 0, 1, 1);
	CombineRgn(hrgnDest, hrgnSrc1, hrgnSrc2, RGN_OR);
	return hrgnDest;
}	/* WinUnionRgn */

HRGN WinSectRgn (HRGN hrgnSrc1, HRGN hrgnSrc2)
{
	HRGN hrgnDest = CreateRectRgn(0, 0, 1, 1);
	CombineRgn(hrgnDest, hrgnSrc1, hrgnSrc2, RGN_AND);
	return hrgnDest;
}	/* WinSectRgn */

HRGN WinDiffRgn (HRGN hrgnSrc1, HRGN hrgnSrc2)
{
	HRGN hrgnDest = CreateRectRgn(0, 0, 1, 1);
	CombineRgn(hrgnDest, hrgnSrc1, hrgnSrc2, RGN_DIFF);
	return hrgnDest;
}	/* WinDiffRgn */

HRGN WinXorRgn (HRGN hrgnSrc1, HRGN hrgnSrc2)
{
	HRGN hrgnDest = CreateRectRgn(0, 0, 1, 1);
	CombineRgn(hrgnDest, hrgnSrc1, hrgnSrc2, RGN_XOR);
	return hrgnDest;
}	/* WinXorRgn */

void WinGetRgnBox (HRGN hrgn, int *left, int *top, int *right, int *bottom, BOOL *isrect)
{
	int result;
	RECT boundbox;

	result = GetRgnBox (hrgn, &boundbox);

	*left   = boundbox.left;
	*right  = boundbox.right;
	*top    = boundbox.top;
	*bottom = boundbox.bottom;

	*isrect  = result == SIMPLEREGION;
}	/* WinGetRgnBox */

BOOL WinIsEmptyRgn(HRGN hrgn)
{
	RECT boundbox;
	return GetRgnBox(hrgn, &boundbox) == NULLREGION;
}

void WinDisposeRgn(HRGN hrgn)
{
	DeleteObject (hrgn);
}

/*
lastActivity:
		DRAWING:   pen=theNormalPen	brush=NULL_BRUSH	 ROP2=if (penMode==iModeXor) R2_NOT R2_COPYPEN
		FILLING:   pen=NULL_PEN brush=theNormalBrush ROP2=if (penMode==iModeXor) R2_NOT R2_COPYPEN
		INVERTING: pen=NULL_PEN brush=theNormalBrush ROP2=R2_NOT
		ERASING:   pen=NULL_PEN brush=theBackBrush	 ROP2=if (penMode==iModeXor) R2_NOT R2_COPYPEN
*/

static void StartDrawing (OSPictContext context)
{
	switch (context->lastActivity)
	{
		case DRAWING:
			break;
		case UNDRAWING:
			SelectObject (context->hDC, context->theNormalPen);
			SetTextColor(context->hDC, context->penColor);
			break;
		case FILLING:
			SelectObject (context->hDC, context->theNormalPen);
			SetTextColor(context->hDC, context->penColor);
			SelectObject (context->hDC, GetStockObject (NULL_BRUSH));
			break;
		case INVERTING:
			SelectObject (context->hDC, context->theNormalPen);
			SetTextColor(context->hDC, context->penColor);
			SelectObject (context->hDC, GetStockObject (NULL_BRUSH));
			if (context->penMode != iModeXor)
				SetROP2 (context->hDC, R2_COPYPEN);
			break;
		case ERASING:
			SelectObject (context->hDC, context->theNormalPen);
			SetTextColor(context->hDC, context->penColor);
			SelectObject (context->hDC, GetStockObject (NULL_BRUSH));
			/* JVG */
			if (context->penMode == iModeXor)
				SetROP2 (context->hDC, R2_NOT);
			/**/
			break;
	}
	context->lastActivity = DRAWING;
}	/* StartDrawing */

static void StartFilling (OSPictContext context)
{
	switch (context->lastActivity)
	{
		case DRAWING:
			SelectObject (context->hDC, GetStockObject (NULL_PEN));
			SelectObject (context->hDC, context->theNormalBrush);
			break;
		case UNDRAWING:
			SelectObject (context->hDC, GetStockObject (NULL_PEN));
			SelectObject (context->hDC, context->theNormalBrush);
			break;
		case FILLING:
			break;
		case INVERTING:
			if (context->penMode != iModeXor)
				SetROP2 (context->hDC, R2_COPYPEN);
			break;
		case ERASING:
			SelectObject (context->hDC, context->theNormalBrush);
			/* JVG */
			if (context->penMode == iModeXor)
				SetROP2 (context->hDC, R2_NOT);
			/**/
			break;
	}
	context->lastActivity = FILLING;
}	/* StartFilling */

static void StartInverting (OSPictContext context)
{
	switch (context->lastActivity)
	{
		case UNDRAWING:
			SelectObject (context->hDC, GetStockObject (NULL_PEN));
			SelectObject (context->hDC, context->theNormalBrush);
			SetROP2 (context->hDC, R2_NOT);
			break;
		case DRAWING:
			SelectObject (context->hDC, GetStockObject (NULL_PEN));
			SelectObject (context->hDC, context->theNormalBrush);
			SetROP2 (context->hDC, R2_NOT);
			break;
		case FILLING:
			SetROP2 (context->hDC, R2_NOT);
			break;
		case INVERTING:
			break;
		case ERASING:
			SelectObject (context->hDC, context->theNormalBrush);
			SetROP2 (context->hDC, R2_NOT);
			break;
	}
	context->lastActivity = INVERTING;
}	/* StartInverting */

static void StartErasing (OSPictContext context)
{
	switch (context->lastActivity)
	{
		case UNDRAWING:
			SelectObject (context->hDC, GetStockObject (NULL_PEN));
			SelectObject (context->hDC, context->theBackBrush);
			/* JVG */
			if (context->penMode == iModeXor)
				SetROP2 (context->hDC, R2_COPYPEN);
			/**/
			break;
		case DRAWING:
			SelectObject (context->hDC, GetStockObject (NULL_PEN));
			SelectObject (context->hDC, context->theBackBrush);
			/* JVG */
			if (context->penMode == iModeXor)
				SetROP2 (context->hDC, R2_COPYPEN);
			/**/
			break;
		case FILLING:
			SelectObject (context->hDC, context->theBackBrush);
			/* JVG */
			if (context->penMode == iModeXor)
				SetROP2 (context->hDC, R2_COPYPEN);
			/**/
			break;
		case INVERTING:
			SelectObject (context->hDC, context->theBackBrush);
/* JVG			if( context->penMode != iModeXor ) */
			SetROP2 (context->hDC, R2_COPYPEN);
			break;
		case ERASING:
			break;
	}
	context->lastActivity = ERASING;
}	/* StartErasing */


static void StartUndrawing (OSPictContext context)
{
	switch (context->lastActivity)
	{
		case UNDRAWING:
			break;
		case DRAWING:
			SelectObject (context->hDC, context->theBackPen);
			SetTextColor (context->hDC, context->backColor);
			break;
		case FILLING:
			SelectObject (context->hDC, context->theBackPen);
			SetTextColor (context->hDC, context->backColor);
			SelectObject (context->hDC, GetStockObject (NULL_BRUSH));
			break;
		case INVERTING:
			SelectObject (context->hDC, context->theBackPen);
			SetTextColor (context->hDC, context->backColor);
			SelectObject (context->hDC, GetStockObject (NULL_BRUSH));
			if (context->penMode != iModeXor)
				SetROP2 (context->hDC, R2_COPYPEN);
			break;
		case ERASING:
			SelectObject (context->hDC, context->theBackPen);
			SetTextColor (context->hDC, context->backColor);
			SelectObject (context->hDC, GetStockObject (NULL_BRUSH));
			/* JVG */
			if (context->penMode == iModeXor)
				SetROP2 (context->hDC, R2_NOT);
			/**/
			break;
	}
	context->lastActivity = UNDRAWING;
}	/* StartUndrawing */

static void ChangeTheNormalPen (OSPictContext context)
{
	HPEN hp;
	LOGBRUSH lb;
	DWORD style;

	lb.lbStyle = BS_SOLID;
	lb.lbColor = context->penColor;
	lb.lbHatch = 0;

	if (context->penSize == 1)
		style = PS_COSMETIC | PS_SOLID;
	else
		style = PS_GEOMETRIC | PS_INSIDEFRAME;

	hp = ExtCreatePen (style, context->penSize, &lb, 0, NULL);

	if (context->lastActivity == DRAWING)
		SelectObject (context->hDC, hp);
	DeleteObject (context->theNormalPen);

	context->theNormalPen = hp;
}	/* ChangeTheNormalPen */

static void ChangeTheBackPen (OSPictContext context)
{
	HPEN hp;
	LOGBRUSH lb;
	DWORD style;

	lb.lbStyle = BS_SOLID;
	lb.lbColor = context->backColor;
	lb.lbHatch = 0;

	if (context->penSize == 1)
		style = PS_COSMETIC | PS_SOLID;
	else
		style = PS_GEOMETRIC | PS_INSIDEFRAME;

	hp = ExtCreatePen (style, context->penSize, &lb, 0, NULL);

	if (context->lastActivity == UNDRAWING)
		SelectObject (context->hDC, hp);
	DeleteObject (context->theBackPen);

	context->theBackPen = hp;
}	/* ChangeTheBackPen */

static void ChangeNormalBrush (OSPictContext context)
{
	HBRUSH hb;

	hb = CreateSolidBrush (context->penColor);

	if (context->lastActivity == FILLING || context->lastActivity == INVERTING)
		SelectObject (context->hDC, hb);
	DeleteObject (context->theNormalBrush);

	context->theNormalBrush = hb;
}	/* ChangeNormalBrush */

static void ChangeBackBrush (OSPictContext context)
{
	HBRUSH hb;

	hb = CreateSolidBrush (context->backColor);

	if (context->lastActivity == ERASING)
		SelectObject (context->hDC, hb);
	DeleteObject (context->theBackBrush);

	context->theBackBrush = hb;
}	/* ChangeBackBrush */


/*------------------------------------*\
|	   Interface functions			   |
\*------------------------------------*/

void WinSetPenSize (int size, OSPictContext context)
{
	context->penSize = size;
	ChangeTheNormalPen (context);
	ChangeTheBackPen (context);
}	/* WinSetPenSize */

void WinSetPenColor (int red, int green, int blue, OSPictContext context)
{
	context->penColor = RGB (red, green, blue);

	ChangeTheNormalPen (context);
	ChangeNormalBrush (context);

	if (context->lastActivity == DRAWING)
		SetTextColor(context->hDC, context->penColor);
}	/* WinSetPenColor */

void WinSetBackColor (int red, int green, int blue, OSPictContext context)
{
	context->backColor = RGB (red, green, blue);

	ChangeTheBackPen (context);
	ChangeBackBrush (context);

	SetBkColor (context->hDC, context->backColor);

	if (context->lastActivity == UNDRAWING)
		SetTextColor(context->hDC, context->backColor);
}	/* WinSetBackColor */

void WinSetMode (int mode, OSPictContext context)
{
	switch (mode)
	{
		case iModeCopy:
			context->penMode = iModeCopy;
			 /*JVG*/ SetROP2 (context->hDC, context->lastActivity == INVERTING ? R2_NOT : R2_COPYPEN);	/**/
			SetBkMode (context->hDC, OPAQUE);
			break;
		case iModeXor:
			context->penMode = iModeXor;
			 /*JVG*/ SetROP2 (context->hDC, context->lastActivity == ERASING ? R2_COPYPEN : R2_NOT); 	/**/
			SetBkMode (context->hDC, TRANSPARENT);
			break;
		case iModeOr:
		default:
			context->penMode = iModeOr;
			 /*JVG*/ SetROP2 (context->hDC, context->lastActivity == INVERTING ? R2_NOT : R2_COPYPEN);	/**/
			SetBkMode (context->hDC, TRANSPARENT);
			break;
	}
}	/* WinSetMode */

void WinSetPattern (int pattern, OSPictContext context)
{
}	/* WinSetPattern */


// changed by MW
void WinDrawPoint (int x, int y, OSPictContext context)
{
	SetPixelV (context->hDC, x, y, context->penColor);			// (for printing)
}	/* WinDrawPoint */

void WinDrawLine (int startx, int starty, int endx, int endy, OSPictContext context)
{
	StartDrawing (context);
	MoveToEx (context->hDC, startx, starty, NULL);
	LineTo (context->hDC, endx, endy);
}	/* WinDrawLine */

void WinUndrawLine (int startx, int starty, int endx, int endy, OSPictContext context)
{
	StartUndrawing(context);
	MoveToEx (context->hDC, startx, starty, NULL);
	LineTo (context->hDC, endx, endy);
}	/* WinDrawLine */

void WinDrawCurve (int x, int y, int rx, int ry, float from, float to, BOOL clockwise,OSPictContext context)
{
	int arx, ary;
	int cx, cy;
	int ex, ey;

	StartDrawing (context);

	arx	= abs(rx);
	ary	= abs(ry);
	cx	= x  - floor(cos(from)* arx);
	cy	= y  + floor(sin(from)* ary);
	ex	= cx + floor(cos(to)  * arx);
	ey	= cy - floor(sin(to)  * ary);

	if (clockwise)
		Arc (context->hDC, cx-rx, cy-ry, cx+rx, cy+ry, x, y, ex, ey);
	else
		Arc (context->hDC, cx-rx, cy-ry, cx+rx, cy+ry, ex, ey, x, y);
}	/* WinDrawCurve */

void WinUndrawCurve (int x, int y, int rx, int ry, float from, float to, BOOL clockwise,OSPictContext context)
{
	int arx, ary;
	int cx, cy;
	int ex, ey;

	StartUndrawing(context);

	arx	= abs(rx);
	ary	= abs(ry);
	cx	= x  - floor(cos(from)* arx);
	cy	= y  + floor(sin(from)* ary);
	ex	= cx + floor(cos(to)  * arx);
	ey	= cy - floor(sin(to)  * ary);

	if (clockwise)
		Arc (context->hDC, cx-rx, cy-ry, cx+rx, cy+ry, x, y, ex, ey);
	else
		Arc (context->hDC, cx-rx, cy-ry, cx+rx, cy+ry, ex, ey, x, y);
}	/* WinDrawCurve */

void WinDrawChar (int x, int y, char c, OSPictContext context)
{
	int oldmode;

	StartDrawing (context);

	oldmode = GetBkMode (context->hDC);
	SetBkMode (context->hDC, TRANSPARENT);
	TextOut (context->hDC, x, y, &c, 1);
	SetBkMode (context->hDC, oldmode);
}	/* WinDrawChar */

void WinUndrawChar (int x, int y, char c, OSPictContext context)
{
	int oldmode;

	StartUndrawing(context);

	oldmode = GetBkMode (context->hDC);
	SetBkMode (context->hDC, TRANSPARENT);
	TextOut (context->hDC, x, y, &c, 1);
	SetBkMode (context->hDC, oldmode);
}	/* WinEraseChar */

void WinDrawString (int x, int y, char *string, OSPictContext context)
{
	int oldmode;

	StartDrawing (context);

	oldmode = GetBkMode (context->hDC);
	if (context->penMode==iModeXor)					/* Check if currently in XOR mode */
	{
		SetBkMode (context->hDC, OPAQUE);			/* in that case background should be OPAQUE. */
	}
	else
	{
		SetBkMode (context->hDC, TRANSPARENT);		/* otherwise it should be TRANSPARENT. */
	}
	if (!TextOut (context->hDC, x, y, string, strlen(string)))
		rMessageBox (NULL,MB_APPLMODAL,"WinDrawString","TextOut failed.");
	SetBkMode (context->hDC, oldmode);
}	/* WinDrawString */

void WinUndrawString (int x, int y, char *string, OSPictContext context)
{
	int oldmode;

	StartUndrawing(context);

	oldmode = GetBkMode (context->hDC);
	if (context->penMode==iModeXor)					/* Check if currently in XOR mode */
	{
		SetBkMode (context->hDC, OPAQUE);			/* in that case background should be OPAQUE. */
	}
	else
	{
		SetBkMode (context->hDC, TRANSPARENT);		/* otherwise it should be TRANSPARENT. */
	}
	if (!TextOut (context->hDC, x, y, string, strlen(string)))
		rMessageBox (NULL,MB_APPLMODAL,"WinDrawString","TextOut failed.");
	SetBkMode (context->hDC, oldmode);
}	/* WinEraseString */


void WinDrawRectangle (int left, int top, int right, int bot, OSPictContext context)
{
	StartDrawing (context);
	Rectangle (context->hDC, left, top, right, bot);
}	/* WinDrawRectangle */

void WinUndrawRectangle (int left, int top, int right, int bot, OSPictContext context)
{
	StartUndrawing (context);
	Rectangle (context->hDC, left, top, right, bot);
}	/* WinDrawRectangle */

void WinFillRectangle (int left, int top, int right, int bot, OSPictContext context)
{
	StartFilling (context);
	Rectangle (context->hDC, left, top, right + 1, bot + 1);
}	/* WinFillRectangle */

void WinEraseRectangle (int left, int top, int right, int bot, OSPictContext context)
{
	StartErasing (context);
	Rectangle (context->hDC, left, top, right + 1, bot + 1);
}	/* WinEraseRectangle */

void WinInvertRectangle (int left, int top, int right, int bot, OSPictContext context)
{
	StartInverting (context);
	Rectangle (context->hDC, left, top, right + 1, bot + 1);
}	/* WinInvertRectangle */

void WinMoveRectangleTo (int left, int top, int right, int bot, int x, int y, OSPictContext context)
{
	WinMoveRectangle (left,top, right,bot, x-left, y-top, context);
}	/* WinMoveRectangleTo */

void WinMoveRectangle (int left, int top, int right, int bot, int dx, int dy, OSPictContext context)
{
	int w, h;
	HWND hwnd;

	hwnd = WindowFromDC (context->hDC);
	if (hwnd != NULL)
	{
		RECT r;
		POINT p;

		GetClientRect (hwnd, &r);
		GetWindowOrgEx (context->hDC, &p);
		left = max (left, r.left + p.x);
		top = max (top, r.top + p.y);
		right = min (right, r.right + p.x);
		bot = min (bot, r.bottom + p.y);
	}

	w = right - left;
	h = bot - top;

	WinCopyRectangle (left, top, right, bot, dx, dy, context);

	StartErasing (context);

	if (dx > w || dy > h)
	{
		Rectangle (context->hDC, left, top, right + 1, bot + 1);
		return;
	}

	if (dx < 0)
		Rectangle (context->hDC, right - dx, top, right + 1, bot + 1);
	else
		Rectangle (context->hDC, left, top, left + dx + 1, bot + 1);

	if (dy < 0)
		Rectangle (context->hDC, left, bot - dy, right + 1, bot + 1);
	else
		Rectangle (context->hDC, left, top, right + 1, top + dy + 1);
}	/* WinMoveRectangle */

void WinCopyRectangleTo (int left, int top, int right, int bot, int x, int y, OSPictContext context)
{
	WinCopyRectangle (left,top, right,bot, x-left,y-top, context);
}	/* WinCopyRectangleTo */

void WinCopyRectangle (int left, int top, int right, int bottom, int dx, int dy, OSPictContext context)
{
	RECT scrollRect;

	scrollRect.left   = left;
	scrollRect.top    = top;
	scrollRect.right  = right;
	scrollRect.bottom = bottom;

	if (!ScrollDC (context->hDC, dx,dy, &scrollRect, &scrollRect, NULL, NULL))
	{
		rMessageBox (NULL,MB_APPLMODAL,"WinCopyRectangle","ScrollDC failed");
	}
}	/* WinCopyRectangle */

/*	PA: new routine to scroll part of the content of a window.
		It is assumed that scrolling happens in one direction only (dx<>0 && dy==0 || dx==0 && dy<>0).
		The result rect (oleft,otop,oright,obottom) is the bounding box of the update area that
		remains to be updated. If all are zero, then nothing needs to be updated.
*/
void WinScrollRectangle (int left, int top, int right, int bottom, int dx, int dy, OSPictContext context,
						 int * oleft, int * otop, int * oright, int * obottom
					    )
{
	RECT scrollRect;
	HRGN hrgnUpdate, hrgnRect;

	scrollRect.left   = left;
	scrollRect.top    = top;
	scrollRect.right  = right;
	scrollRect.bottom = bottom;

	if (dx<0)
	{
		hrgnRect   = CreateRectRgn (right+dx-1,top-1,right+1,bottom+1);
	}
	else if (dx>0)
	{
		hrgnRect   = CreateRectRgn (left-1,top-1,left+dx+1,bottom+1);
	}
	else if (dy<0)
	{
		hrgnRect   = CreateRectRgn (left-1,bottom+dy-1,right+1,bottom+1);
	}
	else if (dy>0)
	{
		hrgnRect   = CreateRectRgn (left-1,top-1,right+1,top+dy+1);
	}
	else
	{
		hrgnRect   = CreateRectRgn (0,0,0,0);
	}
	hrgnUpdate = CreateRectRgn (0,0,1,1);

	if (!ScrollDC (context->hDC, dx,dy, &scrollRect, &scrollRect, hrgnUpdate, NULL))
	{
		rMessageBox (NULL,MB_APPLMODAL,"WinScrollRectangle","ScrollDC failed");
	}
	else
	{
		if (CombineRgn (hrgnUpdate, hrgnUpdate, hrgnRect, RGN_DIFF) == NULLREGION)
		{
			*oleft   = 0;
			*otop    = 0;
			*oright  = 0;
			*obottom = 0;
		}
		else
		{
			RECT box;
			GetRgnBox (hrgnUpdate,&box);
			*oleft   = box.left;
			*otop    = box.top;
			*oright  = box.right;
			*obottom = box.bottom;
		}
	}
	DeleteObject (hrgnUpdate);
	DeleteObject (hrgnRect);
}	/* WinScrollRectangle */


void WinDrawOval (int left, int top, int right, int bot, OSPictContext context)
{
	StartDrawing (context);
	Arc (context->hDC, left, top, right, bot, 0, 0, 0, 0);
}	/* WinDrawOval */

void WinUndrawOval (int left, int top, int right, int bot, OSPictContext context)
{
	StartUndrawing (context);
	Arc (context->hDC, left, top, right, bot, 0, 0, 0, 0);
}	/* WinDrawOval */

void WinFillOval (int left, int top, int right, int bot, OSPictContext context)
{
	StartFilling (context);
	Ellipse (context->hDC, left, top, right + 1, bot + 1);
}	/* WinFillOval */

void WinEraseOval (int left, int top, int right, int bot, OSPictContext context)
{
	StartErasing (context);
	Ellipse (context->hDC, left, top, right + 1, bot + 1);
}	/* WinEraseOval */

void WinInvertOval (int left, int top, int right, int bot, OSPictContext context)
{
	StartInverting (context);
	Ellipse (context->hDC, left, top, right + 1, bot + 1);
}	/* WinInvertOval */


void WinFillWedge (int x, int y, int rx, int ry, float from, float to, BOOL clockwise,OSPictContext context)
{
	int arx, ary;
	int cx, cy;
	int ex, ey;

	StartFilling (context);

	arx	= abs(rx);
	ary	= abs(ry);
	cx	= x  - floor(cos(from)* arx);
	cy	= y  + floor(sin(from)* ary);
	ex	= cx + floor(cos(to)  * arx);
	ey	= cy - floor(sin(to)  * ary);

	if (clockwise)
		Pie (context->hDC, cx-rx, cy-ry, cx+rx, cy+ry, x, y, ex, ey);
	else
		Pie (context->hDC, cx-rx, cy-ry, cx+rx, cy+ry, ex, ey, x, y);
}	/* WinFillWedge */

void WinEraseWedge (int x, int y, int rx, int ry, float from, float to, BOOL clockwise,OSPictContext context)
{
	int arx, ary;
	int cx, cy;
	int ex, ey;

	StartErasing (context);

	arx	= abs(rx);
	ary	= abs(ry);
	cx	= x  - floor(cos(from)* arx);
	cy	= y  + floor(sin(from)* ary);
	ex	= cx + floor(cos(to)  * arx);
	ey	= cy - floor(sin(to)  * ary);

	if (clockwise)
		Pie (context->hDC, cx-rx, cy-ry, cx+rx, cy+ry, x, y, ex, ey);
	else
		Pie (context->hDC, cx-rx, cy-ry, cx+rx, cy+ry, ex, ey, x, y);
}	/* WinEraseWedge */

void WinInvertWedge (int x, int y, int rx, int ry, float from, float to, BOOL clockwise,OSPictContext context)
{
	int arx, ary;
	int cx, cy;
	int ex, ey;

	StartInverting (context);

	arx	= abs(rx);
	ary	= abs(ry);
	cx	= x  - floor(cos(from)* arx);
	cy	= y  + floor(sin(from)* ary);
	ex	= cx + floor(cos(to)  * arx);
	ey	= cy - floor(sin(to)  * ary);

	if (clockwise)
		Pie (context->hDC, cx-rx, cy-ry, cx+rx, cy+ry, x, y, ex, ey);
	else
		Pie (context->hDC, cx-rx, cy-ry, cx+rx, cy+ry, ex, ey, x, y);
}	/* WinInvertWedge */


void WinStartPolygon (int size)
{
	thePolygon = rmalloc (size * sizeof (POINT));
	thePolygonIndex = 0;
}	/* WinStartPolygon */

void WinEndPolygon ()
{
	rfree (thePolygon);
	thePolygon = NULL;
}	/* WinEndPolygon */

void WinAddPolygonPoint (int x, int y)
{
	thePolygon[thePolygonIndex].x = x;
	thePolygon[thePolygonIndex].y = y;
	thePolygonIndex++;
}	/* WinAddPolygonPoint */

void WinDrawPolygon (OSPictContext context)
{
	StartDrawing (context);
	Polyline (context->hDC, thePolygon, thePolygonIndex);
}	/* WinDrawPolygon */

void WinUndrawPolygon (OSPictContext context)
{
	StartUndrawing (context);
	Polyline (context->hDC, thePolygon, thePolygonIndex);
}	/* WinDrawPolygon */

void WinFillPolygon (OSPictContext context)
{
	StartFilling (context);
	Polygon (context->hDC, thePolygon, thePolygonIndex);
}	/* WinFillPolygon */

void WinErasePolygon (OSPictContext context)
{
	StartErasing (context);
	Polygon (context->hDC, thePolygon, thePolygonIndex);
}	/* WinErasePolygon */

void WinInvertPolygon (OSPictContext context)
{
	StartInverting (context);
	Polygon (context->hDC, thePolygon, thePolygonIndex);
}	/* WinInvertPolygon */


/*	PA: two new routines that temporarily create and destroy a DISPLAY HDC.
		Use this HDC only for local use.
*/
OSPictContext WinCreateScreenHDC()
{
	OSPictContext context;
	context = malloc(sizeof(*context));
	memset(context, 0, sizeof(*context));
	context->hDC = CreateDC ("DISPLAY",NULL,NULL,NULL);
	return context;
}	/* WinCreateScreenHDC */

void WinDestroyScreenHDC (OSPictContext context)
{
	DeleteDC(context->hDC);
	free(context);
}	/* WinDestroyScreenHDC */


/*	WinDrawResizedBitmap draws a bitmap on screen. For reasons of efficiency it uses an
	already created bitmap handle.
*/
void WinDrawResizedBitmap (int sourcew, int sourceh, int destx, int desty, int destw, int desth,
						   HBITMAP hbmp, OSPictContext context
						  )
{
	HDC compatibleDC;
	POINT sourcesize, destsize, dest, origin;
	HGDIOBJ prevObj;

	sourcesize.x = sourcew;
	sourcesize.y = sourceh;
	origin.x     = 0;
	origin.y     = 0;
	destsize.x   = destw;
	destsize.y   = desth;
	dest.x       = destx;
	dest.y       = desty;

	//	Create a compatible device context
	compatibleDC = CreateCompatibleDC (context->hDC);
	if (compatibleDC == NULL)
		rMessageBox (NULL,MB_APPLMODAL,"WinDrawResizedBitmap","CreateCompatibleDC failed");

	//	Select bitmap into compatible device context
	prevObj = SelectObject (compatibleDC, hbmp);
	SetMapMode (compatibleDC, GetMapMode (context->hDC));
	DPtoLP (context->hDC, &destsize, 1);
	DPtoLP (context->hDC, &dest, 1);
	DPtoLP (compatibleDC, &sourcesize, 1);
	DPtoLP (compatibleDC, &origin, 1);

	if (!StretchBlt (context->hDC, dest.x, dest.y, destsize.x, destsize.y, compatibleDC, origin.x, origin.y, sourcesize.x, sourcesize.y, SRCCOPY))
		rMessageBox (NULL,MB_APPLMODAL,"WinDrawResizedBitmap","StretchBlt failed");

	SelectObject (compatibleDC, prevObj);
	DeleteDC (compatibleDC);
}	/* WinDrawResizedBitmap */

// ... MW


/*	WinDrawBitmap must be used for drawing bitmaps on screen.
	For reasons of efficiency it uses memory device context, BitBlt, and bitmap handle.
*/
void WinDrawBitmap (int w, int h, int destx, int desty,
					HBITMAP hbmp, OSPictContext context
				   )
{
	HDC compatibleDC;
	POINT size, origin, dest;
	HGDIOBJ prevObj;

	size.x   = w;
	size.y   = h;
	origin.x = 0;
	origin.y = 0;
	dest.x   = destx;
	dest.y   = desty;

	//	Create a compatible device context
	compatibleDC = CreateCompatibleDC (context->hDC);
	if (compatibleDC == NULL)
		rMessageBox (NULL,MB_APPLMODAL,"WinDrawBitmap","CreateCompatibleDC failed");

	//	Select bitmap into compatible device context
	prevObj = SelectObject (compatibleDC, hbmp);
	SetMapMode (compatibleDC, GetMapMode (context->hDC));
	DPtoLP (context->hDC, &size, 1);
	DPtoLP (context->hDC, &dest, 1);
	DPtoLP (compatibleDC, &origin, 1);

	BitBlt (context->hDC, dest.x, dest.y, size.x, size.y, compatibleDC, origin.x, origin.y, SRCCOPY);

	SelectObject (compatibleDC, prevObj);
	DeleteDC (compatibleDC);
}	/* WinDrawBitmap */

HBITMAP WinCreateBitmap (char *lpszName, int *pWidth, int *pHeight)
{
	HBITMAP hbmp;
	BITMAPINFO bi;
	HDC hdc;

	*pWidth  = 0;
	*pHeight = 0;

	hbmp = (HBITMAP) LoadImage(ghInst, lpszName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE);

	if (!hbmp)
		return NULL;

	memset(&bi, 0, sizeof(bi));
	bi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);

	hdc = CreateDC ("DISPLAY",NULL,NULL,NULL);
	if (!GetDIBits(hdc, hbmp, 0, 0, NULL, &bi, DIB_RGB_COLORS))
		return NULL;
	DeleteDC(hdc);

	*pWidth  = bi.bmiHeader.biWidth;
	*pHeight = bi.bmiHeader.biHeight;

	return hbmp;
}	/* WinCreateBitmap */

void WinDisposeBitmap (HBITMAP hBmp)
{
	if (!DeleteObject (hBmp))
		ErrorExit ("Fatal error in WinDisposeBitmap: DeleteObject returned FALSE.");
}

/*-----------------------------
	   Font stuff
  -----------------------------*/

void WinSetFont (char *lfname, int style, int size, OSPictContext context)
{
	LOGFONT lf;
	HFONT hf;

	strcpy (context->curFont, lfname);
	context->fontstyle = style;
	context->fontsize = PointsToPix(context->hDC,size);
				// MW: PointsToPix

	SetLogFontData (&lf, context->curFont, context->fontstyle, context->fontsize);
	hf = CreateFontIndirect (&lf);

	if (hf==NULL)
	{
		ErrorExit ("Fatal error in WinSetFont: CreateFontIndirect returned NULL.");
	}

	SelectObject (context->hDC, hf);
	DeleteObject (context->theFont);

	context->theFont = hf;
}	/* WinSetFont */

void WinGetFontInfo (char *fontName, int style, int size, OSPictContext context, int *ascent, int *descent, int *maxwidth, int *leading)
{
	LOGFONT lf;
	TEXTMETRIC tm;
	HDC screen, dummy;
	HFONT of;
	int	pixSize;

	if (context != NULL)
		screen = context->hDC;
	  else
		screen = CreateDC ("DISPLAY", NULL, NULL, NULL);

	pixSize = PointsToPix(screen,size);
	// end MW

	SetLogFontData (&lf, "", style, pixSize);

	strcpy(lf.lfFaceName, fontName);

	of = SelectObject (screen, CreateFontIndirect (&lf));

	GetTextMetrics (screen, &tm);

	*ascent = tm.tmAscent - tm.tmInternalLeading;
	*descent = tm.tmDescent;
	*maxwidth = tm.tmMaxCharWidth;
	*leading = tm.tmInternalLeading + tm.tmExternalLeading;

	DeleteObject (SelectObject (screen, of));

	if (context == NULL)
		DeleteDC(screen);
}	/* WinGetFontInfo */

void WinGetPicFontInfo (OSPictContext context, int *ascent, int *descent, int *maxwidth, int *leading)
{
	TEXTMETRIC tm;

	GetTextMetrics (context->hDC, &tm);

	*ascent = tm.tmAscent - tm.tmInternalLeading;
	*descent = tm.tmDescent;
	*maxwidth = tm.tmMaxCharWidth;
	*leading = tm.tmInternalLeading + tm.tmExternalLeading;
}	/* WinGetPicFontInfo */

int WinGetPicStringWidth (char *string, OSPictContext context)
{
	SIZE sz;
	if (!GetTextExtentPoint32 (context->hDC, string, strlen(string), &sz))
		rMessageBox (NULL,MB_APPLMODAL,"WinGetPicStringWidth","GetTextExtentPoint32 failed");
	return sz.cx;
}	/* WinGetPicStringWidth */

int WinGetPicCharWidth (char ch, OSPictContext context)
{
	SIZE sz;
	char str[1];

	str[0] = ch;
	GetTextExtentPoint32 (context->hDC, str, 1, &sz);

	return sz.cx;
}	/* WinGetPicCharWidth */

int WinGetStringWidth (char *string, char *fontName, int style, int size, OSPictContext context)
{
	LOGFONT lf;
	HDC screen;
	HFONT of,hf;
	int pixSize;
	SIZE sz;

	if (context != NULL)
		screen = context->hDC;
	else
		screen = CreateDC ("DISPLAY", NULL, NULL, NULL);

	pixSize = PointsToPix(screen,size);
	// end MW

	SetLogFontData (&lf, "", style, pixSize);

	strcpy (lf.lfFaceName, fontName);

	hf = CreateFontIndirect (&lf);	// PA+++: hf added to test for NULL
	if (hf==NULL)
		rMessageBox (NULL,MB_APPLMODAL,"WinGetStringWidth","CreateFontIndirect returned NULL");
	of = SelectObject (screen, hf);	//CreateFontIndirect (&lf));
	if (of==NULL)
		rMessageBox (NULL,MB_APPLMODAL,"WinGetStringWidth","SelectObject of HFONT returned NULL");

	if (!GetTextExtentPoint32 (screen, string, strlen(string), &sz))
		rMessageBox (NULL,MB_APPLMODAL,"WinGetPicStringWidth","GetTextExtentPoint32 failed");

	DeleteObject (SelectObject (screen, of));

	if (context == NULL)
		DeleteDC(screen);

	return sz.cx;
}	/* WinGetStringWidth */

int WinGetCharWidth (char ch, char *fontName, int style, int size, OSPictContext context)
{
	LOGFONT lf;
	HDC screen;
	HFONT of;
	SIZE sz;
	int pixSize;

	if (context != NULL)
		screen = context->hDC;
	  else
		screen = CreateDC ("DISPLAY", NULL, NULL, NULL);

	pixSize = PointsToPix(screen,size);
	// end MW

	SetLogFontData (&lf, "", style, pixSize);

	strcpy(lf.lfFaceName, fontName);

	of = SelectObject (screen, CreateFontIndirect (&lf));

	GetTextExtentPoint32 (screen, &ch, 1, &sz);

	DeleteObject (SelectObject (screen, of));

	if (context == NULL)
		DeleteDC(screen);

	return sz.cx;
}	/* WinGetCharWidth */


// MW...

void getResolutionC(OSPictContext context, int *xResP, int *yResP)
{
	int mapMode = GetMapMode(context->hDC);
	if (mapMode==MM_ISOTROPIC)
		{	*xResP = WinGetHorzResolution();
			*yResP = WinGetVertResolution();
		}
	  else
		{	*xResP = GetDeviceCaps(context->hDC,LOGPIXELSX);
			*yResP = GetDeviceCaps(context->hDC,LOGPIXELSY);
		};
/*	MW: currently, the MM_ISOTROPIC mapping mode is only used for printing with the emulation
	of the screen resolution. In that case the screen resolution will be returned.
*/
}	/* getResolutionC */

void WinGetPictureScaleFactor(OSPictContext context, int *nh, int *dh, int *nv, int *dv)
{
	if (GetMapMode(context->hDC)==MM_TEXT)
	    {	*nh = 1;
			*dh = 1;
			*nv = 1;
			*dv = 1;
		}
	  else
		{	SIZE sRes,pRes;
			GetWindowExtEx  (context->hDC,&sRes);
			GetViewportExtEx(context->hDC,&pRes);
			*nh	= pRes.cx;
			*dh	= sRes.cx;
			*nv	= pRes.cy;
			*dv	= sRes.cy;
		};

	/*	MW: Microsoft decided, that most drawing operations should work well with the
		MM_ISOTROPIC mapping mode, but not the clipping operations. For these, the clipping
		coordinates have to be scaled
	*/
}	/* WinGetPictureScaleFactor */
// .. MW

void WinDefaultFontDef(char **fname, int *fstyle, int *fsize)
{
    *fname  = "Times";
    *fstyle = 0;
    *fsize  = 10;
}

void WinDialogFontDef(char **fname, int *fstyle, int *fsize)
{
    *fname  = "MS Sans Serif";
    *fstyle = 0;
    *fsize  = 8;
}

void WinSerifFontDef(char **fname, int *fstyle, int *fsize)
{
	*fname  = "Times New Roman";
	*fstyle = 0;
    *fsize  = 10;
};

void WinSansSerifFontDef(char **fname, int *fstyle, int *fsize)
{
	*fname  = "Arial";
	*fstyle = 0;
    *fsize  = 10;
};

void WinSmallFontDef(char **fname, int *fstyle, int *fsize)
{
	*fname  = "Small Fonts";
	*fstyle = 0;
    *fsize  = 7;
};

void WinNonProportionalFontDef(char **fname, int *fstyle, int *fsize)
{
	*fname  = "Courier New";
	*fstyle = 0;
    *fsize  = 10;
};

void WinSymbolFontDef(char **fname, int *fstyle, int *fsize)
{
	*fname  = "Symbol";
	*fstyle = 0;
    *fsize  = 10;
};