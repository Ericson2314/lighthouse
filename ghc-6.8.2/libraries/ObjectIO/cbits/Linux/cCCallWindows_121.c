/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Routines related to window/dialog handling.
********************************************************************************************/
#include "cCCallWindows_121.h"
#include "cCrossCallWindows_121.h"

void WinInvalidateWindow (GtkWidget *widget)
{
	gtk_widget_queue_draw(widget);
}

void WinInvalidateRect (GtkWidget *widget, int left, int top, int right, int bottom)
{
	gtk_widget_queue_draw_area(widget,left,top,right-left,bottom-top);
}

void WinValidateRect (GtkWidget *widget, int left, int top, int right, int bottom)
{
/*	RECT rect;

	rect.left   = left;
	rect.top    = top;
	rect.right  = right;
	rect.bottom = bottom;
	ValidateRect ((HWND) hwnd, &rect);*/
	printf("WinValidateRect -> not implemented\n");
}

void WinValidateRgn (GtkWidget *widget, GdkRegion *region)
{
//	ValidateRgn ((HWND) hwnd, (HRGN) rgn);
	printf("WinValidateRgn -> not implemented\n");
}

/*	Win(M/S)DIClientToOuterSizeDims returns the width and height needed to add/subtract
	from the client/outer size to obtain the outer/client size.
	These values must be the same as used by W95AdjustClean(M/S)DIWindowDimensions!
*/
void WinMDIClientToOuterSizeDims (int styleFlags, int *dw, int *dh)
{
/*	if ((styleFlags&WS_THICKFRAME) != 0)
	{	// resizable window
		*dw = 2 * GetSystemMetrics (SM_CXSIZEFRAME);
		*dh = 2 * GetSystemMetrics (SM_CYSIZEFRAME) + GetSystemMetrics (SM_CYCAPTION);
	} else
	{	// fixed size window
		*dw = 2 * GetSystemMetrics (SM_CXFIXEDFRAME);
		*dh = 2 * GetSystemMetrics (SM_CYFIXEDFRAME) + GetSystemMetrics (SM_CYCAPTION);
	}
*/
	*dw = 0;
        *dh = 0;
	printf("WinMDIClientOuterSizeDims -> not implemented\n");
}

void WinSDIClientToOuterSizeDims (int styleFlags, int *dw, int *dh)
{
	*dw = 0; //2 * GetSystemMetrics (SM_CXSIZEFRAME);
	*dh = 0; //2 * GetSystemMetrics (SM_CYSIZEFRAME) + GetSystemMetrics (SM_CYCAPTION);
	printf("WinSDIClientOuterSizeDims -> not implemented\n");
}


/*	UpdateWindowScrollbars updates any window scrollbars and non-client area if present.
	Uses the following access procedures to the GWL_STYLE of a windowhandle:
		GetGWL_STYLE (hwnd) returns the GWL_STYLE value of hwnd;
		WindowHasHScroll (hwnd) returns TRUE iff hwnd has a horizontal scrollbar;
		WindowHasVScroll (hwnd) returns TRUE iff hwnd has a vertical scrollbar;
*/

void UpdateWindowScrollbars (GtkWidget *widget)
{
/*	int w,h;
	RECT rect;

	GetWindowRect (hwnd, &rect);
	w = rect.right -rect.left;
	h = rect.bottom-rect.top;

	if (WindowHasHScroll (hwnd))
	{
		rect.left   = 0;
		rect.top    = h-GetSystemMetrics (SM_CYHSCROLL);
		rect.right  = w;
		rect.bottom = h;
		InvalidateRect (hwnd,&rect,FALSE);
		RedrawWindow (hwnd,&rect,NULL,RDW_FRAME | RDW_VALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN);
		ValidateRect (hwnd,&rect);
	}
	if (WindowHasVScroll (hwnd))
	{
		rect.left   = w-GetSystemMetrics (SM_CXVSCROLL);
		rect.top    = 0;
		rect.right  = w;
		rect.bottom = h;
		InvalidateRect (hwnd,&rect,FALSE);
		RedrawWindow (hwnd,&rect,NULL,RDW_FRAME | RDW_VALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN);
		ValidateRect (hwnd,&rect);
	}
*/
	printf("UpdateWindowScrollbars -> not implemented\n");
}


int WinScreenYSize ()
{
	return gdk_screen_height();
}

int WinScreenXSize ()
{
	return gdk_screen_width();
}

void WinMinimumWinSize (int *mx, int *my)
{
	*mx = 48;
	*my = 0;
}

/*	WinScrollbarSize determines system metrics of width and height of scrollbars.
*/
void WinScrollbarSize (int *width, int *height)
{
    printf ("WinScrollbarSize -> not implemented\n");
	*width  = 0; //GetSystemMetrics (SM_CXVSCROLL);
	*height = 0; //GetSystemMetrics (SM_CYHSCROLL);
}

void WinMaxFixedWindowSize (int *mx, int *my)
{
    *mx = gdk_screen_width();
    *my = gdk_screen_height();
}

void WinMaxScrollWindowSize (int *mx, int *my)
{
    *mx = gdk_screen_width();
    *my = gdk_screen_height();
}
