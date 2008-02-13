/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Routines related to drawing.
********************************************************************************************/
#include "util_121.h"
#include <pango/pango.h>
#include "cpicture_121.h"
#include "cCrossCall_121.h"
#include "cCrossCallWindows_121.h"

GdkDrawable *WinGetDC (GtkWidget *widget)
{
	GdkWindow *window = GTK_BIN(GTK_BIN(widget)->child)->child->window;
	gdk_window_ref(window);
	return GDK_DRAWABLE(window);
}	/* WinGetDC */

void WinReleaseDC(GtkWidget *widget, GdkDrawable *drawable)
{
	gdk_window_unref(GDK_WINDOW(drawable));
	return;
}	/* WinReleaseDC */

int OsMMtoVPixels(double mm)
{
  	InitGTK();
	return (int) ((mm*gdk_screen_height())/gdk_screen_height_mm());
}

int OsMMtoHPixels(double mm)
{
  	InitGTK();
	return (int) ((mm*gdk_screen_width())/gdk_screen_width_mm());
}

/*------------------------------------*\
|									   |
|	   Helper functions 			   |
|									   |
\*------------------------------------*/

static GdkGC *theDrawGC, *theEraseGC, *theInvertGC;
static GdkFont *theFont;
static PangoFontDescription *theFontDesc;
static gint penSize;
static gint penPat;
static gint penMode;
static GdkColor penColor;
static GdkColor backColor;
static GdkPoint *thePolygon;
static gint thePolygonIndex;
static GdkRegion *theClipRgn = NULL;


void WinInitPicture (int size, int mode,
					 int pr, int pg, int pb,
					 int br, int bg, int bb,
					 char *fname, int fstyle, int fsize,
					 GdkRegion *clipRgn,
					 GdkDrawable *drawable
				    )
{
	penColor.pixel = 0;
	penColor.red   = pr*257;
	penColor.green = pg*257;
	penColor.blue  = pb*257;

	backColor.pixel = 0;
	backColor.red   = br*257;
	backColor.green = bg*257;
	backColor.blue  = bb*257;

	penSize = size;
	penMode = mode;

	if (drawable)
	{
		gdk_colormap_alloc_color(gdk_drawable_get_colormap(drawable), &penColor, FALSE, FALSE);
		gdk_colormap_alloc_color(gdk_drawable_get_colormap(drawable), &backColor, FALSE, FALSE);

		theDrawGC = gdk_gc_new(drawable);
		gdk_gc_set_foreground(theDrawGC, &penColor);
		gdk_gc_set_background(theDrawGC, &backColor);
		gdk_gc_set_clip_origin(theDrawGC, 0, 0);
		gdk_gc_set_line_attributes(theDrawGC, size, GDK_LINE_SOLID, GDK_CAP_ROUND, GDK_JOIN_ROUND);

		theEraseGC = gdk_gc_new(drawable);
		gdk_gc_set_foreground(theEraseGC, &backColor);
		gdk_gc_set_background(theEraseGC, &penColor);
		gdk_gc_set_clip_origin(theEraseGC, 0, 0);
		gdk_gc_set_line_attributes(theEraseGC, size, GDK_LINE_SOLID, GDK_CAP_ROUND, GDK_JOIN_ROUND);

		theInvertGC = gdk_gc_new(drawable);
		gdk_gc_set_foreground(theInvertGC, &penColor);
		gdk_gc_set_background(theInvertGC, &backColor);
		gdk_gc_set_function(theInvertGC, GDK_INVERT);
		gdk_gc_set_clip_origin(theInvertGC, 0, 0);
	}
	else
	{
		theDrawGC = NULL;
		theEraseGC = NULL;
		theInvertGC = NULL;
	}

	theFontDesc = pango_font_description_new();
	pango_font_description_set_family(theFontDesc,fname);
	pango_font_description_set_weight(theFontDesc,(fstyle & iBold) ? PANGO_WEIGHT_BOLD : PANGO_WEIGHT_NORMAL);
	pango_font_description_set_style(theFontDesc,(fstyle & iItalic) ? PANGO_STYLE_ITALIC : PANGO_STYLE_NORMAL);
	//	plf->lfUnderline = (style & iUnderline) ? TRUE : FALSE;
	//	plf->lfStrikeOut = (style & iStrikeOut) ? TRUE : FALSE;
	pango_font_description_set_size(theFontDesc, fsize*PANGO_SCALE);
	theFont = gdk_font_from_description(theFontDesc);

  	theClipRgn = NULL;
  	if (clipRgn)
  	{
		theClipRgn = gdk_region_copy(clipRgn);
  		if (theDrawGC)   gdk_gc_set_clip_region(theDrawGC,   theClipRgn);
		if (theEraseGC)  gdk_gc_set_clip_region(theEraseGC,  theClipRgn);
		if (theInvertGC) gdk_gc_set_clip_region(theInvertGC, theClipRgn);
	}
}	/* WinInitPicture */

void WinDonePicture (GdkDrawable *drawable)
{
	if (drawable)
	{
		gdk_colormap_free_colors(gdk_drawable_get_colormap(drawable), &penColor, 1);
		gdk_colormap_free_colors(gdk_drawable_get_colormap(drawable), &backColor, 1);
	}

	gdk_font_unref(theFont);
	pango_font_description_free(theFontDesc);

	if (theDrawGC)   gdk_gc_destroy(theDrawGC);
	if (theEraseGC)  gdk_gc_destroy(theEraseGC);
	if (theInvertGC) gdk_gc_destroy(theInvertGC);

	if (theClipRgn)
	{
	   	gdk_region_destroy(theClipRgn);
	   	theClipRgn = NULL;
	}
}	/* WinDonePicture */

/*	PA: Set and get the clipping region of a picture:
		WinClipRgnPicture    takes the intersection of the argument clipRgn with the current clipping region.
		WinSetClipRgnPicture sets the argument clipRgn as the new clipping region.
		WinGetClipRgnPicture gets the current clipping region.
*/
void WinClipRgnPicture (GdkRegion *region, GdkDrawable *drawable)
{
	GdkRectangle *rectangles;
	gint n_rectangles, i;

	if (theClipRgn != NULL)
		gdk_region_intersect(theClipRgn, region);
	else
	{
		if (region)
			theClipRgn = gdk_region_copy(region);
	}

	if (theDrawGC)   gdk_gc_set_clip_region(theDrawGC,   theClipRgn);
	if (theEraseGC)  gdk_gc_set_clip_region(theEraseGC,  theClipRgn);
	if (theInvertGC) gdk_gc_set_clip_region(theInvertGC, theClipRgn);
}	/* WinClipRgnPicture */

void WinSetClipRgnPicture (GdkRegion *region, GdkDrawable *drawable)
{
	GdkRectangle *rectangles;
	gint n_rectangles, i;

	if (theClipRgn != NULL)
		gdk_region_destroy(theClipRgn);

	theClipRgn = region ? gdk_region_copy(region) : NULL;

	if (theDrawGC)   gdk_gc_set_clip_region(theDrawGC,   theClipRgn);
	if (theEraseGC)  gdk_gc_set_clip_region(theEraseGC,  theClipRgn);
	if (theInvertGC) gdk_gc_set_clip_region(theInvertGC, theClipRgn);
}	/* WinSetClipRgnPicture */

GdkRegion *WinGetClipRgnPicture (GdkDrawable *drawable)
{
	GdkRegion *r = NULL;

	if (theClipRgn)
	{
		r = gdk_region_copy(theClipRgn);
	};

	return r;
}	/* WinGetClipRgnPicture */


/*	Operations to create, modify, and destroy polygon shapes.
*/

GdkPoint *WinAllocPolyShape (int size)
{
	return (GdkPoint *) rmalloc (size * sizeof (GdkPoint));
}	/* WinAllocPolyShape */

void WinSetPolyPoint (int i, int x, int y, GdkPoint *shape)
{
	shape[i].x = x;
	shape[i].y = y;
}	/* WinSetPolyPoint */

void WinFreePolyShape (GdkPoint *shape)
{
	rfree (shape);
}	/* WinFreePolyShape */


/*	Operations to create, modify and destroy regions.
*/
GdkRegion *WinCreateEmptyRgn()
{
	return gdk_region_new();
}	/* WinCreateRectRgn */

GdkRegion *WinCreateRectRgn (int nLeftRect, int nTopRect, int nRightRect, int nBottomRect)
{
	GdkRectangle rectangle;
	rectangle.x = nLeftRect;
	rectangle.y = nTopRect;
	rectangle.width  = nRightRect-nLeftRect;
	rectangle.height = nBottomRect-nTopRect;
	return gdk_region_rectangle(&rectangle);
}	/* WinCreateRectRgn */

GdkRegion *WinCreatePolygonRgn (GdkPoint *points, int nPoints, int fnPolyFillMode)
{
	return gdk_region_polygon(points,nPoints, fnPolyFillMode == 1 ? GDK_EVEN_ODD_RULE : GDK_WINDING_RULE);
}	/* WinCreatePolygonRgn */

GdkRegion *WinUnionRgn (GdkRegion *src1, GdkRegion *src2)
{
	GdkRegion *dst = NULL;

	if (src1)
	{
		dst = gdk_region_copy(src1);
		gdk_region_union(dst, src2);
	}

	return dst;
}	/* WinUnionRgn */

GdkRegion *WinSectRgn (GdkRegion *src1, GdkRegion *src2)
{
	GdkRegion *dst = src2;

	if (src1)
	{
		dst = gdk_region_copy(src1);
		gdk_region_intersect(dst, src2);
	}

	return dst;
}	/* WinSectRgn */

GdkRegion *WinDiffRgn (GdkRegion *src1, GdkRegion *src2)
{
	GdkRegion *dst = NULL;

	if (src1)
	{
		dst = gdk_region_copy(src1);
		gdk_region_subtract(dst, src2);
	};

	return dst;
}	/* WinDiffRgn */

GdkRegion *WinXorRgn (GdkRegion *src1, GdkRegion *src2)
{
	GdkRegion *dst = NULL;

	if (src1)
	{
		dst = gdk_region_copy(src1);
		gdk_region_xor(dst, src2);
	};

	return dst;
}	/* WinXorRgn */

void WinGetRgnBox (GdkRegion *region, int *left, int *top, int *right, int *bottom, gboolean *isrect)
{
	GdkRegion *tempRegion;
	GdkRectangle rectangle;
	gdk_region_get_clipbox(region,&rectangle);
	tempRegion = gdk_region_rectangle(&rectangle);

	*left   = rectangle.x;
	*top    = rectangle.y;
	*right  = rectangle.x+rectangle.width;
	*bottom = rectangle.y+rectangle.height;
	*isrect  = gdk_region_equal(region, tempRegion);

	gdk_region_destroy(tempRegion);
}	/* WinGetRgnBox */

gboolean WinIsEmptyRgn(GdkRegion *region)
{
	return gdk_region_empty(region);
}

void WinDisposeRgn (GdkRegion *region)
{
	gdk_region_destroy(region);
}

/*------------------------------------*\
|	   Interface functions			   |
\*------------------------------------*/

void WinSetPenSize (int size, GdkDrawable *drawable)
{
	if (theDrawGC)  gdk_gc_set_line_attributes(theDrawGC,  size, GDK_LINE_SOLID, GDK_CAP_ROUND, GDK_JOIN_ROUND);
	if (theEraseGC) gdk_gc_set_line_attributes(theEraseGC, size, GDK_LINE_SOLID, GDK_CAP_ROUND, GDK_JOIN_ROUND);
	penSize = size;
}	/* WinSetPenSize */

void WinSetPenColor (int red, int green, int blue, GdkDrawable *drawable)
{
	if (drawable)
	{
		gdk_colormap_free_colors(gdk_drawable_get_colormap(drawable), &backColor, 1);
		penColor.pixel = 0;
		penColor.red   = red*257;
		penColor.green = green*257;
		penColor.blue  = blue*257;
		gdk_colormap_alloc_color(gdk_drawable_get_colormap(drawable), &penColor, FALSE, FALSE);

		gdk_gc_set_foreground(theDrawGC, &penColor);
		gdk_gc_set_background(theEraseGC, &penColor);
		gdk_gc_set_foreground(theInvertGC, &penColor);
	}
}	/* WinSetPenColor */

void WinSetBackColor (int red, int green, int blue, GdkDrawable *drawable)
{
	if (drawable)
	{
		gdk_colormap_free_colors(gdk_drawable_get_colormap(drawable), &backColor, 1);
		backColor.pixel = 0;
		backColor.red   = red*257;
		backColor.green = green*257;
		backColor.blue  = blue*257;
		gdk_colormap_alloc_color(gdk_drawable_get_colormap(drawable), &backColor, FALSE, FALSE);

		gdk_gc_set_background(theDrawGC, &backColor);
		gdk_gc_set_foreground(theEraseGC, &backColor);
		gdk_gc_set_background(theInvertGC, &backColor);
	}
}	/* WinSetBackColor */

void WinSetMode (int mode, GdkDrawable *drawable)
{
	switch (mode)
	{
		case iModeCopy:
			penMode = iModeCopy;
			if (theDrawGC)   gdk_gc_set_function(theDrawGC,   GDK_COPY);
			if (theEraseGC)  gdk_gc_set_function(theEraseGC,  GDK_COPY);
			if (theInvertGC) gdk_gc_set_function(theInvertGC, GDK_COPY);
			break;
		case iModeXor:
			penMode = iModeXor;
			if (theDrawGC)   gdk_gc_set_function(theDrawGC,   GDK_XOR);
			if (theEraseGC)  gdk_gc_set_function(theEraseGC,  GDK_XOR);
			if (theInvertGC) gdk_gc_set_function(theInvertGC, GDK_XOR);
			break;
		case iModeOr:
		default:
			if (theDrawGC)   gdk_gc_set_function(theDrawGC,   GDK_OR);
			if (theEraseGC)  gdk_gc_set_function(theEraseGC,  GDK_OR);
			if (theInvertGC) gdk_gc_set_function(theInvertGC, GDK_OR);
			break;
	}
}	/* WinSetMode */

void WinSetPattern (int pattern, GdkDrawable *drawable)
{
}	/* WinSetPattern */


// changed by MW
void WinDrawPoint (int x, int y, GdkDrawable *drawable)
{
	if (drawable) gdk_draw_point(drawable, theDrawGC, x, y);
}	/* WinDrawPoint */

void WinDrawLine (int startx, int starty, int endx, int endy, GdkDrawable *drawable)
{
	if (drawable) gdk_draw_line(drawable, theDrawGC, startx, starty, endx, endy);
}	/* WinDrawLine */

void WinUndrawLine (int startx, int starty, int endx, int endy, GdkDrawable *drawable)
{
	if (drawable) gdk_draw_line(drawable, theEraseGC, startx, starty, endx, endy);
}	/* WinDrawLine */

static float PI = 3.1415926535897932384626433832795;

void WinDrawCurve (int x, int y, int rx, int ry, float from, float to, gboolean clockwise,GdkDrawable *drawable)
{
	int cx, cy;

	if (drawable)
	{
		cx	= x  - floor(cos(from)* abs(rx));
		cy	= y  + floor(sin(from)* abs(ry));

		from = (32*360*from)/PI;
		to   = (32*360*to)/PI;

		if (clockwise)
			gdk_draw_arc(drawable, theDrawGC, FALSE,
						 cx-rx, cy-ry, 2*rx, 2*ry,
						 floor(from-PI/2),floor(from-to));
		else
			gdk_draw_arc(drawable, theDrawGC, FALSE,
						 cx-rx, cy-ry, 2*rx, 2*ry,
						 floor(to-PI/2),floor(to-from));
	}
}	/* WinDrawCurve */

void WinUndrawCurve(int x, int y, int rx, int ry, float from, float to, gboolean clockwise,GdkDrawable *drawable)
{
	int cx, cy;

	if (drawable)
	{
		cx	= x  - floor(cos(from)* abs(rx));
		cy	= y  + floor(sin(from)* abs(ry));

		from = (32*360*from)/PI;
		to   = (32*360*to)/PI;

		if (clockwise)
			gdk_draw_arc(drawable, theEraseGC, FALSE,
						 cx-rx, cy-ry, 2*rx, 2*ry,
						 floor(from-PI/2),floor(from-to));
		else
			gdk_draw_arc(drawable, theEraseGC, FALSE,
						 cx-rx, cy-ry, 2*rx, 2*ry,
						 floor(to-PI/2),floor(to-from));
	}
}	/* WinDrawCurve */

void WinDrawChar (int x, int y, char c, GdkDrawable *drawable)
{
	if (drawable) gdk_draw_text(drawable,theFont,theDrawGC,x,y,&c,1);
}	/* WinDrawChar */

void WinUndrawChar(int x, int y, char c, GdkDrawable *drawable)
{
	if (drawable) gdk_draw_text(drawable,theFont,theEraseGC,x,y,&c,1);
}	/* WinDrawChar */

void WinDrawString (int x, int y, char *string, GdkDrawable *drawable)
{
	if (drawable) gdk_draw_string(drawable,theFont,theDrawGC,x,y,string);
}	/* WinDrawString */

void WinUndrawString (int x, int y, char *string, GdkDrawable *drawable)
{
	if (drawable) gdk_draw_string(drawable,theFont,theEraseGC,x,y,string);
}	/* WinUndrawString */

void WinDrawRectangle (int left, int top, int right, int bot, GdkDrawable *drawable)
{
	if (drawable)
		gdk_draw_rectangle(drawable, theDrawGC, FALSE,
	                   left, top,
	                   right-left-1,
	                   bot-top-1);
}	/* WinDrawRectangle */

void WinUndrawRectangle (int left, int top, int right, int bot, GdkDrawable *drawable)
{
	if (drawable)
		gdk_draw_rectangle(drawable, theEraseGC, FALSE,
	                   left, top,
	                   right-left-1,
	                   bot-top-1);
}	/* WinDrawRectangle */

void WinFillRectangle (int left, int top, int right, int bot, GdkDrawable *drawable)
{
	if (drawable)
		gdk_draw_rectangle(drawable, theDrawGC, TRUE,
	                   left, top,
	                   right-left,
	                   bot-top);
}	/* WinFillRectangle */

void WinEraseRectangle (int left, int top, int right, int bot, GdkDrawable *drawable)
{
	if (drawable)
		gdk_draw_rectangle(drawable, theEraseGC, TRUE,
					   left, top,
					   right-left,
	                   bot-top);
}	/* WinEraseRectangle */

void WinInvertRectangle (int left, int top, int right, int bot, GdkDrawable *drawable)
{
	if (drawable)
		gdk_draw_rectangle(drawable, theInvertGC, TRUE,
					   left, top,
					   right-left,
	                   bot-top);
}	/* WinInvertRectangle */

void WinMoveRectangleTo (int left, int top, int right, int bot, int x, int y, GdkDrawable *drawable)
{
	printf("WinMoveRectangleTo is not implemented\n");
	//WinMoveRectangle (left,top, right,bot, x-left, y-top, ihdc);
}	/* WinMoveRectangleTo */

void WinMoveRectangle (int left, int top, int right, int bot, int dx, int dy, GdkDrawable *drawable)
{
	printf("WinMoveRectangle is not implemented\n");
/*	int w, h;
	HWND hwnd;

	hwnd = WindowFromDC (ihdc);
	if (hwnd != NULL)
	{
		RECT r;
		POINT p;

		GetClientRect (hwnd, &r);
		GetWindowOrgEx (ihdc, &p);
		left = max (left, r.left + p.x);
		top = max (top, r.top + p.y);
		right = min (right, r.right + p.x);
		bot = min (bot, r.bottom + p.y);
	}

	w = right - left;
	h = bot - top;

	WinCopyRectangle (left, top, right, bot, dx, dy, ihdc);

//	StartErasing (ihdc);

	if (dx > w || dy > h)
	{
		Rectangle (ihdc, left, top, right + 1, bot + 1);
		return;
	}

	if (dx < 0)
		Rectangle (ihdc, right - dx, top, right + 1, bot + 1);
	else
		Rectangle (ihdc, left, top, left + dx + 1, bot + 1);

	if (dy < 0)
		Rectangle (ihdc, left, bot - dy, right + 1, bot + 1);
	else
		Rectangle (ihdc, left, top, right + 1, top + dy + 1);*/
}	/* WinMoveRectangle */

void WinCopyRectangleTo (int left, int top, int right, int bot, int x, int y, GdkDrawable *drawable)
{
//	WinCopyRectangle (left,top, right,bot, x-left,y-top, ihdc);
	printf("WinCopyRectangleTo is not implemented\n");
}	/* WinCopyRectangleTo */

void WinCopyRectangle (int left, int top, int right, int bottom, int dx, int dy, GdkDrawable *drawable)
{
/*	RECT scrollRect;

	scrollRect.left   = left;
	scrollRect.top    = top;
	scrollRect.right  = right;
	scrollRect.bottom = bottom;

	if (!ScrollDC (ihdc, dx,dy, &scrollRect, &scrollRect, NULL, NULL))
	{
		rMessageBox (NULL,MB_APPLMODAL,"WinCopyRectangle","ScrollDC failed");
	}*/
	printf("WinCopyRectangle is not implemented\n");
}	/* WinCopyRectangle */

/*	PA: new routine to scroll part of the content of a window.
		It is assumed that scrolling happens in one direction only (dx<>0 && dy==0 || dx==0 && dy<>0).
		The result rect (oleft,otop,oright,obottom) is the bounding box of the update area that
		remains to be updated. If all are zero, then nothing needs to be updated.
*/
void WinScrollRectangle (int left, int top, int right, int bottom, int dx, int dy, GdkDrawable *drawable,
						 int * oleft, int * otop, int * oright, int * obottom
					    )
{
/*	RECT scrollRect;
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

	if (!ScrollDC (ihdc, dx,dy, &scrollRect, &scrollRect, hrgnUpdate, NULL))
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
*/
	printf("WinScrollRectangle is not implemented\n");
}	/* WinScrollRectangle */


void WinUndrawOval (int left, int top, int right, int bot, GdkDrawable *drawable)
{
	if (drawable) gdk_draw_arc(drawable,theEraseGC,FALSE,left,top,right-left,bot-top,0,64*360);
}	/* WinDrawOval */

void WinDrawOval (int left, int top, int right, int bot, GdkDrawable *drawable)
{
	if (drawable) gdk_draw_arc(drawable,theDrawGC,FALSE,left,top,right-left,bot-top,0,64*360);
}	/* WinDrawOval */

void WinFillOval (int left, int top, int right, int bot, GdkDrawable *drawable)
{
	if (drawable) gdk_draw_arc(drawable,theDrawGC,TRUE,left,top,right-left,bot-top,0,64*360);
}	/* WinFillOval */

void WinEraseOval (int left, int top, int right, int bot, GdkDrawable *drawable)
{
	if (drawable) gdk_draw_arc(drawable,theEraseGC,TRUE,left,top,right-left,bot-top,0,64*360);
}	/* WinEraseOval */

void WinInvertOval (int left, int top, int right, int bot, GdkDrawable *drawable)
{
	if (drawable) gdk_draw_arc(drawable,theInvertGC,TRUE,left,top,right-left,bot-top,0,64*360);
}	/* WinInvertOval */


void WinFillWedge (int x, int y, int rx, int ry, float from, float to, gboolean clockwise,GdkDrawable *drawable)
{
	int cx, cy;

	if (drawable)
	{
		cx	= x  - floor(cos(from)* abs(rx));
		cy	= y  + floor(sin(from)* abs(ry));

		from = (32*360*from)/PI;
		to   = (32*360*to)/PI;

		if (clockwise)
			gdk_draw_arc(drawable, theDrawGC, TRUE,
						 cx-rx, cy-ry, 2*rx, 2*ry,
						 floor(from-PI/2),floor(from-to));
		else
			gdk_draw_arc(drawable, theDrawGC, TRUE,
						 cx-rx, cy-ry, 2*rx, 2*ry,
						 floor(to-PI/2),floor(to-from));
	}
}	/* WinFillWedge */

void WinEraseWedge (int x, int y, int rx, int ry, float from, float to, gboolean clockwise,GdkDrawable *drawable)
{
	int cx, cy;

	if (drawable)
	{
		cx	= x  - floor(cos(from)* abs(rx));
		cy	= y  + floor(sin(from)* abs(ry));

		from = (32*360*from)/PI;
		to   = (32*360*to)/PI;

		if (clockwise)
			gdk_draw_arc(drawable, theEraseGC, TRUE,
						 cx-rx, cy-ry, 2*rx, 2*ry,
						 floor(from-PI/2),floor(from-to));
		else
			gdk_draw_arc(drawable, theEraseGC, TRUE,
						 cx-rx, cy-ry, 2*rx, 2*ry,
					 	floor(to-PI/2),floor(to-from));
	}
}	/* WinEraseWedge */

void WinInvertWedge (int x, int y, int rx, int ry, float from, float to, gboolean clockwise,GdkDrawable *drawable)
{
	int cx, cy;

	if (drawable)
	{
		cx	= x  - floor(cos(from)* abs(rx));
		cy	= y  + floor(sin(from)* abs(ry));

		from = (32*360*from)/PI;
		to   = (32*360*to)/PI;

		if (clockwise)
			gdk_draw_arc(drawable, theInvertGC, TRUE,
						 cx-rx, cy-ry, 2*rx, 2*ry,
						 floor(from-PI/2),floor(from-to));
		else
			gdk_draw_arc(drawable, theInvertGC, TRUE,
						 cx-rx, cy-ry, 2*rx, 2*ry,
						 floor(to-PI/2),floor(to-from));
	}
}	/* WinInvertWedge */


void WinStartPolygon (int size)
{
	thePolygon = rmalloc (size * sizeof(GdkPoint));
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

void WinDrawPolygon(GdkDrawable *drawable)
{
	if (drawable) gdk_draw_polygon(drawable,theDrawGC,FALSE,thePolygon,thePolygonIndex);
}	/* WinDrawPolygon */

void WinUndrawPolygon (GdkDrawable *drawable)
{
	if (drawable) gdk_draw_polygon(drawable,theEraseGC,FALSE,thePolygon,thePolygonIndex);
}	/* WinDrawPolygon */

void WinFillPolygon (GdkDrawable *drawable)
{
	if (drawable) gdk_draw_polygon(drawable,theDrawGC,TRUE,thePolygon,thePolygonIndex);
}	/* WinFillPolygon */

void WinErasePolygon (GdkDrawable *drawable)
{
	if (drawable) gdk_draw_polygon(drawable,theEraseGC,TRUE,thePolygon,thePolygonIndex);
}	/* WinErasePolygon */

void WinInvertPolygon (GdkDrawable *drawable)
{
	if (drawable) gdk_draw_polygon(drawable,theInvertGC,TRUE,thePolygon,thePolygonIndex);
}	/* WinInvertPolygon */

GdkDrawable *WinCreateScreenHDC()
{
	InitGTK();
	return NULL;
}	/* WinCreateScreenHDC */

void WinDestroyScreenHDC (GdkDrawable *drawable)
{
}	/* WinDestroyScreenHDC */


/*	WinDrawResizedBitmap draws a bitmap on screen. For reasons of efficiency it uses an
	already created bitmap handle.
*/
void WinDrawResizedBitmap (int sourcew, int sourceh, int destx, int desty, int destw, int desth,
						   GdkPixbuf *pixbuf, GdkDrawable *drawable
						  )
{
/*	HDC compatibleDC;
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
	compatibleDC = CreateCompatibleDC (hdc);
	if (compatibleDC == NULL)
		rMessageBox (NULL,MB_APPLMODAL,"WinDrawResizedBitmap","CreateCompatibleDC failed");

	//	Select bitmap into compatible device context
	prevObj = SelectObject (compatibleDC, hbmp);
	SetMapMode (compatibleDC, GetMapMode (hdc));
	DPtoLP (hdc, &destsize, 1);
	DPtoLP (hdc, &dest, 1);
	DPtoLP (compatibleDC, &sourcesize, 1);
	DPtoLP (compatibleDC, &origin, 1);

	if (!StretchBlt (hdc, dest.x, dest.y, destsize.x, destsize.y, compatibleDC, origin.x, origin.y, sourcesize.x, sourcesize.y, SRCCOPY))
		rMessageBox (NULL,MB_APPLMODAL,"WinDrawResizedBitmap","StretchBlt failed");

	SelectObject (compatibleDC, prevObj);
	DeleteDC (compatibleDC);*/
	printf("WinDrawResizedBitmap is not implemented\n");
}	/* WinDrawResizedBitmap */

// ... MW


void WinDrawBitmap (int w, int h, int destx, int desty, GdkPixbuf *pixbuf, GdkDrawable *drawable)
{
  //	if (drawable) gdk_draw_drawable(drawable,theDrawGC,GDK_DRAWABLE(pixbuf),0,0,destx,desty,w,h);
  if (drawable)
    {
      gdk_pixbuf_render_to_drawable   (pixbuf, drawable, theDrawGC, 0, 0, destx, desty, w, h,
                                              GDK_RGB_DITHER_NONE, 0, 0);
    }
}	/* WinDrawBitmap */

GdkPixbuf *WinCreateBitmap (char *filename, int *pWidth, int *pHeight)
{
	GError *err = NULL;
	GdkPixbuf *pixbuf;

	InitGTK();
	pixbuf = gdk_pixbuf_new_from_file(filename, &err);

	if (!pixbuf)
		return NULL;

	*pWidth  = gdk_pixbuf_get_width(pixbuf);
	*pHeight  = gdk_pixbuf_get_height(pixbuf);

	return pixbuf;
}	/* WinCreateBitmap */

void WinDisposeBitmap (GdkPixbuf *pixbuf)
{
	gdk_pixbuf_unref(pixbuf);
}


/*-----------------------------
	   Font stuff
  -----------------------------*/

void WinSetFont (char *lfname, int style, int size, GdkDrawable *drawable)
{
	gdk_font_unref(theFont);

	pango_font_description_set_family(theFontDesc,lfname);
	pango_font_description_set_weight(theFontDesc,(style & iBold) ? PANGO_WEIGHT_BOLD : PANGO_WEIGHT_NORMAL);
	pango_font_description_set_style(theFontDesc,(style & iItalic) ? PANGO_STYLE_ITALIC : PANGO_STYLE_NORMAL);
	//	plf->lfUnderline = (style & iUnderline) ? TRUE : FALSE;
	//	plf->lfStrikeOut = (style & iStrikeOut) ? TRUE : FALSE;
	pango_font_description_set_size(theFontDesc, size*PANGO_SCALE);
	theFont = gdk_font_from_description(theFontDesc);
}	/* WinSetFont */

void WinGetFontInfo (char *fontName, int style, int size, GdkDrawable *drawable, int *ascent, int *descent, int *maxwidth, int *leading)
{
	PangoFontset *fontset;
	PangoFontMetrics *metrics;
	PangoFontDescription *fontDesc;

	fontDesc = pango_font_description_new();
	pango_font_description_set_family(fontDesc,fontName);
	pango_font_description_set_weight(fontDesc,(style & iBold) ? PANGO_WEIGHT_BOLD : PANGO_WEIGHT_NORMAL);
	pango_font_description_set_style(fontDesc,(style & iItalic) ? PANGO_STYLE_ITALIC : PANGO_STYLE_NORMAL);
	//	plf->lfUnderline = (style & iUnderline) ? TRUE : FALSE;
	//	plf->lfStrikeOut = (style & iStrikeOut) ? TRUE : FALSE;
	pango_font_description_set_size(fontDesc, size*PANGO_SCALE);
	fontset = pango_font_map_load_fontset
				(pango_ft2_font_map_for_display(),
				 gdk_pango_context_get(),
				 fontDesc,
				 pango_language_from_string("EN"));
	metrics = pango_fontset_get_metrics(fontset);
	*ascent = pango_font_metrics_get_ascent(metrics)/PANGO_SCALE;
	*descent = pango_font_metrics_get_descent(metrics)/PANGO_SCALE;
	*maxwidth = pango_font_metrics_get_approximate_char_width(metrics)/PANGO_SCALE;
	*leading = 2; /* FIXME */
	pango_font_metrics_unref(metrics);
	pango_font_description_free(fontDesc);
}	/* WinGetFontInfo */

void WinGetPicFontInfo (GdkDrawable *drawable, int *ascent, int *descent, int *maxwidth, int *leading)
{
	PangoFontset *fontset;
	PangoFontMetrics *metrics;

	fontset = pango_font_map_load_fontset
				(pango_ft2_font_map_for_display(),
				 gdk_pango_context_get(),
				 theFontDesc,
				 pango_language_from_string("EN"));
	metrics = pango_fontset_get_metrics(fontset);
	*ascent = pango_font_metrics_get_ascent(metrics);
	*descent = pango_font_metrics_get_descent(metrics);
	*maxwidth = pango_font_metrics_get_approximate_char_width(metrics);
	*leading = 2; /* FIXME */
	pango_font_metrics_unref(metrics);
}	/* WinGetPicFontInfo */

int WinGetPicStringWidth (char *string, GdkDrawable *drawable)
{
	return gdk_string_width(theFont, string);
}	/* WinGetPicStringWidth */

int WinGetPicCharWidth (char ch, GdkDrawable *drawable)
{
	return gdk_char_width(theFont, ch);
}	/* WinGetPicCharWidth */

int WinGetStringWidth (char *string, char *fontName, int style, int size, GdkDrawable *drawable)
{
	int width;
	GdkFont *font;
	PangoFontDescription *fontDesc;

	fontDesc = pango_font_description_new();
	pango_font_description_set_family(fontDesc,fontName);
	pango_font_description_set_weight(fontDesc,(style & iBold) ? PANGO_WEIGHT_BOLD : PANGO_WEIGHT_NORMAL);
	pango_font_description_set_style(fontDesc,(style & iItalic) ? PANGO_STYLE_ITALIC : PANGO_STYLE_NORMAL);
	//	plf->lfUnderline = (style & iUnderline) ? TRUE : FALSE;
	//	plf->lfStrikeOut = (style & iStrikeOut) ? TRUE : FALSE;
	pango_font_description_set_size(fontDesc, size*PANGO_SCALE);
  	font = gdk_font_from_description(fontDesc);

  	width = gdk_string_width(font, string);

	gdk_font_unref(font);
  	pango_font_description_free(fontDesc);

	return width;
}	/* WinGetStringWidth */

int WinGetCharWidth (char ch, char *fontName, int style, int size, GdkDrawable *drawable)
{
	int width;
	GdkFont *font;
	PangoFontDescription *fontDesc;

	fontDesc = pango_font_description_new();
	pango_font_description_set_family(fontDesc,fontName);
	pango_font_description_set_weight(fontDesc,(style & iBold) ? PANGO_WEIGHT_BOLD : PANGO_WEIGHT_NORMAL);
	pango_font_description_set_style(fontDesc,(style & iItalic) ? PANGO_STYLE_ITALIC : PANGO_STYLE_NORMAL);
	//	plf->lfUnderline = (style & iUnderline) ? TRUE : FALSE;
	//	plf->lfStrikeOut = (style & iStrikeOut) ? TRUE : FALSE;
	pango_font_description_set_size(fontDesc, size*PANGO_SCALE);
  	font = gdk_font_from_description(fontDesc);

  	width = gdk_char_width(font, ch);

	gdk_font_unref(font);
  	pango_font_description_free(fontDesc);

	return width;
}	/* WinGetCharWidth */


void getResolutionC(GdkDrawable *drawable, int *xResP, int *yResP)
{
	*xResP = gdk_screen_width();
	*yResP = gdk_screen_height();
}	/* getResolutionC */

void WinGetPictureScaleFactor(GdkDrawable *drawable, int *nh, int *dh, int *nv, int *dv)
{
	*nh = 1;
	*dh = 1;
	*nv = 1;
	*dv = 1;
}	/* WinGetPictureScaleFactor */

void WinDefaultFontDef(char **fname, int *fstyle, int *fsize)
{
  *fname  = "helvetica";
  *fstyle = 0;
  *fsize  = 12;
}

void WinDialogFontDef(char **fname, int *fstyle, int *fsize)
{
  *fname  = "helvetica";
  *fstyle = 0;
  *fsize  = 12;
}

void WinSerifFontDef(char **fname, int *fstyle, int *fsize)
{
	*fname  = "times";
	*fstyle = 0;
	*fsize  = 10;
};

void WinSansSerifFontDef(char **fname, int *fstyle, int *fsize)
{
	*fname  = "helvetica";
	*fstyle = 0;
	*fsize  = 10;
};

void WinSmallFontDef(char **fname, int *fstyle, int *fsize)
{
	*fname  = "helvetica";
	*fstyle = 0;
	*fsize  = 7;
};

void WinNonProportionalFontDef(char **fname, int *fstyle, int *fsize)
{
	*fname  = "fixed";
	*fstyle = 0;
	*fsize  = 10;
};

void WinSymbolFontDef(char **fname, int *fstyle, int *fsize)
{
	*fname  = "adobe-symbol";
	*fstyle = 0;
	*fsize  = 10;
};
