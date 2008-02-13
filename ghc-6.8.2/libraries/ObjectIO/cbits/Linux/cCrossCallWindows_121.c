/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Routines related to window/dialog handling.
********************************************************************************************/

#include "cCrossCallWindows_121.h"
#include "cCCallWindows_121.h"
#include "cCCallSystem_121.h"
#include "cCrossCall_121.h"
#include "cCrossCallxDI_121.h"
#include <gdk/gdkkeysyms.h>

/*	Global data:
*/
//static PAINTSTRUCT gPaintStruct;
//static LONG stdEditCallback      = 0;			/* The standard internal Windows callback routine of edit controls. */
//static LONG stdPopUpCallback     = 0;			/* The standard internal Windows callback routine of pop up controls. */

//HWND ghCaretWnd = NULL;

GdkCursor *gArrowCursor = NULL;
GdkCursor *gBusyCursor = NULL;
GdkCursor *gIBeamCursor = NULL;
GdkCursor *gCrossCursor = NULL;
GdkCursor *gFatCrossCursor = NULL;
GdkCursor *gHiddenCursor = NULL;

GtkWidget *gFirstRadioButton = NULL;


//	Find the first non CompoundControl parent window of the argument.
static GtkWidget *GetControlParent (GtkWidget *widget)
{
  GtkWidget *parent = widget, *res = NULL, *last = NULL;

	while (parent != NULL)
	{
		if (GTK_IS_SCROLLED_WINDOW(parent))
			res = parent;

		last = parent;
		parent = gtk_widget_get_parent (parent);
	}

	return (GTK_IS_DIALOG(last)) ? last : res;
}

static GtkFixed *GetFixed (GtkWidget *widget)
{
	if (GTK_IS_DIALOG(widget))
	{
		return GTK_FIXED((GtkWidget *) gtk_container_children(GTK_CONTAINER(GTK_DIALOG(widget)->vbox))->data);
	}
	else
	{
		return GTK_FIXED(GTK_BIN(GTK_BIN(widget)->child)->child);
	}
}


/*********************************************************************************************
	The callback routine for a compound control.
*********************************************************************************************/
#if 0
static LRESULT CALLBACK CompoundControlProcedure (HWND hwnd, UINT uMess, WPARAM wParam, LPARAM lParam)
{
	switch (uMess)
	{
		case WM_COMMAND:
			{
				switch (HIWORD (wParam))
				{
					case BN_CLICKED:
						{
							if (lParam != 0)
							{
								/*	Send also modifiers to Clean */
								SendMessage4ToClean (CcWmBUTTONCLICKED, GetControlParent (hwnd), lParam, GetModifiers (), LOWORD (wParam));
							}
							return 0;
						}
						break;
					case CBN_SETFOCUS:
						{
							gComboSelection = SendMessage ((HWND) lParam, CB_GETCURSEL, 0, 0);
							return 0;
						}
						break;
					case CBN_KILLFOCUS:
						{
							gComboSelection = -1;
							return 0;
						}
						break;
					case CBN_SELENDOK:
						{
							char text[256];
							int newsel;
							HWND combo;

							combo = (HWND) lParam;
							newsel = SendMessage (combo, CB_GETCURSEL, 0, 0);
							SendMessage (combo, CB_GETLBTEXT, newsel, (LPARAM) text);
							if (!SendMessage (combo, CB_GETITEMDATA, newsel, 0))
							{
								SendMessage (combo, CB_SETCURSEL, gComboSelection, (LPARAM) text);
								MessageBeep (0xFFFFFFFF);
								return 0;
							}
							else
							{
								gComboSelection = newsel;
								if (newsel!=CB_ERR)
									SendMessage3ToClean (CcWmITEMSELECT, GetControlParent (hwnd), combo, newsel);
								return 1;
							}
						}
						break;
				}
				return 0;
			} break;
		case WM_PAINT:
			{
				HWND parentwindow;
				HDC hdc;
				PAINTSTRUCT ps;

				if (GetUpdateRect(hwnd,NULL,FALSE))	// determine if there is really an update area.
				{
					parentwindow = GetControlParent (hwnd);
					hdc = BeginPaint (hwnd, &ps);
					SendMessage3ToClean (CcWmDRAWCONTROL, parentwindow, hwnd, hdc);
					EndPaint (hwnd, &ps);
				}
				return 0;
			} break;
		case WM_HSCROLL:
			{
				int nPos,nScrollCode,controlkind;
				HWND parentwindow, hwndScrollBar;

				nScrollCode = LOWORD (wParam);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wParam);
					parentwindow  = GetControlParent (hwnd);
					hwndScrollBar = (HWND) lParam;

					if (hwndScrollBar==0)
					{
						controlkind = SB_HORZ;		/* lParam==0 in case of Compound scrollbars. */
						hwndScrollBar = hwnd;		/* pass the compound control handle to Clean. */
						UpdateWindow (hwnd);		/* but first ensure that compound control is updated. */
					}
					else
					{
						controlkind = SB_CTL;		/* lParam!==0 in case of SliderControls. */
					}
					SendMessage5ToClean (CcWmSCROLLBARACTION, parentwindow, hwndScrollBar, controlkind, nScrollCode, nPos);
				}
				return 0;
			}
			break;
		case WM_VSCROLL:
			{
				int nPos,nScrollCode,controlkind;
				HWND parentwindow, hwndScrollBar;

				nScrollCode = LOWORD (wParam);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wParam);
					parentwindow  = GetControlParent (hwnd);
					hwndScrollBar = (HWND) lParam;

					if (hwndScrollBar==0)
					{
						controlkind = SB_VERT;		/* lParam==0 in case of Compound scrollbars. */
						hwndScrollBar = hwnd;		/* pass the compound control handle to Clean. */
						UpdateWindow (hwnd);		/* but first ensure that compound control is updated. */
					}
					else
					{
						controlkind = SB_CTL;		/* lParam!==0 in case of SliderControls. */
					}
					SendMessage5ToClean (CcWmSCROLLBARACTION, parentwindow, hwndScrollBar, controlkind, nScrollCode, nPos);
				}
				return 0;
			}
			break;
		/*	The following cases concerning mouse events
				(WM_LBUTTONDOWN upto WM_TIMER) have been copied from CustomControlProcedure.
		*/
		case WM_LBUTTONDOWN:
			{
//				SendMouseDownToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				return 0;
			} break;
		case WM_MOUSEMOVE:
			{
				if (gInMouseDown)
				{
//					SendMouseStillDownToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				}
				else
				{
//					SendMouseStillUpToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				}
				return 0;
			} break;
		case WM_LBUTTONUP:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return 0;
			} break;
		case WM_CANCELMODE:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return DefWindowProc (hwnd, uMess, wParam, lParam);
			} break;
		case WM_CAPTURECHANGED:
			{
				if (gInMouseDown)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hwnd, &p);
//					SendMouseUpToClean (GetControlParent (hwnd), hwnd, p.x, p.y);
				}
				return 0;
			} break;
		/*	The following cases concerning key events and focus events
				(WM_SYSKEYDOWN upto WM_GETDLGCODE) have been copied from CustomControlProcedure.
		*/
		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			{
				int c = 0;
				HWND hwndParent;

				c = CheckVirtualKeyCode ((int) wParam);

				if (!c)
				/* Ignore non-virtual keys, because they arrive as WM_SYSCHAR and WM_CHAR. */
				{
					return DefWindowProc (hwnd, uMess, wParam, lParam);
				}
				/* Handle virtual keys analogously to keys received as WM_SYSCHAR and WM_CHAR. */
				hwndParent = GetControlParent (hwnd);
//				if (gInKey)
//				{
//					if (gCurChar == c)
//						SendKeyStillDownToClean (hwndParent, hwnd, gCurChar);
//					else
//					{
//						SendKeyUpToClean (hwndParent, hwnd, gCurChar);
//						gCurChar = c;
//						SendKeyDownToClean (hwndParent, hwnd, gCurChar);
//					}
//				}
//				else
//				{
//					gCurChar = c;
//					SendKeyDownToClean (hwndParent, hwnd, gCurChar);
//					gInKey = TRUE;
//				}
				return 0;
			}
			break;
		case WM_SYSCHAR:
		case WM_CHAR:
			{
				HWND hwndParent = GetControlParent (hwnd);

//				if (gInKey)
//				{
//					if (gCurChar == (int) wParam)
//						SendKeyStillDownToClean (hwndParent, hwnd, gCurChar);
//					else
//					{
//						SendKeyUpToClean (hwndParent, hwnd, gCurChar);
//						gCurChar = wParam;
//						SendKeyDownToClean (hwndParent, hwnd, gCurChar);
//					}
//				}
//				else
//				{
//					gCurChar = wParam;
//					SendKeyDownToClean (hwndParent, hwnd, gCurChar);
//					gInKey = TRUE;
//				}
				return 0;
			}
			break;
		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				if (gInKey)
					SendKeyUpToClean (GetControlParent (hwnd), hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				return DefWindowProc (hwnd, uMess, wParam, lParam);
			}
			break;
		case WM_KILLFOCUS:
			{
				HWND hwndParent = GetControlParent (hwnd);
				if (gInKey)
					SendKeyUpToClean (hwndParent, hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				/*	WM_KILLFOCUS now also sends the CcWmKILLFOCUS message to
					Clean (because of the ControlDeactivate attribute).
				*/
				SendMessage2ToClean (CcWmKILLFOCUS, hwndParent, hwnd);
				return 0;
			}
			break;
		case WM_SETFOCUS:
			{
				/*	WM_SETFOCUS sends the CcWmSETFOCUS message to Clean because
					of the ControlActivate attribute.
				*/
				SendMessage2ToClean (CcWmSETFOCUS, GetControlParent (hwnd), hwnd);
				return 0;
			}
			break;
		/*	The WM_CLOSE event is generated when a user presses escape inside an EditControl that exists
			within the CompoundControl which exists within a Dialog.
		*/
		case WM_CLOSE:
			{
				SendMessage1ToClean (CcWmCLOSE, GetControlParent (hwnd));
				return 0;
			}
			break;
		case WM_GETDLGCODE:		/*	Inform dialog procedure to pass all keyboard input to the control. */
			return (DLGC_WANTCHARS | DLGC_WANTARROWS);
			break;
		case WM_DRAWITEM:
			{
				LPDRAWITEMSTRUCT lpdis;
				lpdis = (LPDRAWITEMSTRUCT) lParam;

				switch (lpdis->CtlType)
				{
					case ODT_COMBOBOX:
						{
							char text[256];
							COLORREF forecolor, bkcolor;
							SendMessage (lpdis->hwndItem, CB_GETLBTEXT, lpdis->itemID, (LPARAM) text);
							if (lpdis->itemState & ODS_DISABLED)
							{
								forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_3DFACE));
							}
							else if (lpdis->itemState & ODS_SELECTED)
							{
								if (lpdis->itemData)
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHTTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHT));
								}
								else
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
								}
							}
							else
							{
								if (lpdis->itemData)
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_WINDOWTEXT));
								else
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
							}

							ExtTextOut (lpdis->hDC,					/* device context         */
										lpdis->rcItem.left + 2,		/* ref point x            */
										lpdis->rcItem.top + 1,		/* ref point y            */
										ETO_CLIPPED | ETO_OPAQUE,	/* options                */
										&lpdis->rcItem,				/* clipping rect          */
										text,						/* text to draw           */
										lstrlen (text),				/* length of text to draw */
										NULL						/* no kerning array       */
								);

							SetTextColor (lpdis->hDC, forecolor);
							SetBkColor (lpdis->hDC, bkcolor);

							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);
							return 0;
						} break;
					case ODT_BUTTON:
						{
							HWND parentwindow;
							parentwindow  = GetControlParent (hwnd);

							SendMessage3ToClean (CcWmDRAWCONTROL, parentwindow, lpdis->hwndItem, lpdis->hDC);

							if (lpdis->itemState & ODS_SELECTED)
								InvertRect (lpdis->hDC, &lpdis->rcItem);

							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);
							return 0;
						} break;
				}
				return 0;
			}
			break;
		default:
			return DefWindowProc (hwnd, uMess, wParam, lParam);
			break;
	}
	ErrorExit ("Fatal error: case leak in CompoundControlProcedure (%d).",uMess);
}	/* CompoundControlProcedure */
#endif

/*********************************************************************************************
	Cross call procedure implementations.
	Eval<nr> corresponds with a CrossCallEntry generated by NewCrossCallEntry (nr,Eval<nr>).
*********************************************************************************************/
void EvalCcRqBEGINPAINT (CrossCallInfo *pcci)	/* hwnd; HDC result. */
{
//	HDC hdc;
//	hdc = BeginPaint ((HWND) pcci->p1, &gPaintStruct);

	printf("EvalCcRqBEGINPAINT -> not implemented\n");
	MakeReturn1Cci (pcci, (int) NULL /*hdc*/);
}

void EvalCcRqENDPAINT (CrossCallInfo *pcci)		/* hwnd; no result.  */
{
//	EndPaint ((HWND) pcci->p1, &gPaintStruct);
	printf("EvalCcRqEndPaint -> not implemented\n");
	MakeReturn0Cci (pcci);
}

void EvalCcRqFAKEPAINT (CrossCallInfo *pcci)		/* hwnd; no result. */
{
//	HWND hwnd = (HWND) pcci->p1;
//
//	BeginPaint (hwnd, &gPaintStruct);
//	EndPaint (hwnd,&gPaintStruct);
//	InvalidateRect (hwnd, NULL, FALSE);

	printf("EvalCcRqFAKEPAINT -> not implemented\n");
	MakeReturn0Cci (pcci);
}

void EvalCcRqDESTROYMODALDIALOG (CrossCallInfo *pcci) /* hwnd; no result. */
{
	GtkWidget *dialog = (GtkWidget *) pcci->p1;
	gtk_dialog_response (GTK_DIALOG(dialog), 0);
	MakeReturn0Cci (pcci);
}

void EvalCcRqDESTROYMDIDOCWINDOW (CrossCallInfo *pcci)	/* hwndFrame, hwndClient, wPtr; no result. */
{
	gint page_num;
	GtkWidget *frame, *client, *window;

	frame  = (GtkWidget *) pcci->p1;
	client = (GtkWidget *) pcci->p2;
	window = (GtkWidget *) pcci->p3;

	page_num = gtk_notebook_page_num(GTK_NOTEBOOK(client), window);
  	gtk_notebook_remove_page(GTK_NOTEBOOK(client), page_num);

	MakeReturn0Cci (pcci);
}

static gint client_expose_handler(GtkWidget *widget, GdkEventExpose *event, gpointer user_data)
{
	SendMessage6ToClean(CcWmPAINT, (int)gtk_widget_get_parent(gtk_widget_get_parent(widget)),
		event->area.x,
		event->area.y,
		event->area.x+event->area.width,
		event->area.y+event->area.height,
		(int) GDK_DRAWABLE(event->window));

	return GTK_WIDGET_GET_CLASS(widget)->expose_event(widget,event);
};

static void sw_focus_out_handler(GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
	if (gInKey)
		SendKeyUpToClean (widget, widget, gCurChar);

	gInKey = FALSE;
	gCurChar = 0;
}

static gboolean sw_button_press_handler(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
	if (event->button == 1)
	{
		GtkWidget *client = gtk_widget_get_parent(widget);

		gInMouseDown = TRUE;

		switch (event->type)
		{
		case GDK_BUTTON_PRESS:
			SendMessage6ToClean (CcWmMOUSE, client, client, BUTTONDOWN, event->x, event->y, GetModifiers());
			break;
		case GDK_2BUTTON_PRESS:
			SendMessage6ToClean (CcWmMOUSE, client, client, BUTTONDOUBLEDOWN, event->x, event->y, GetModifiers());
			break;
		case GDK_3BUTTON_PRESS:
			SendMessage6ToClean (CcWmMOUSE, client, client, BUTTONTRIPLEDOWN, event->x, event->y, GetModifiers());
			break;
		}
	}
};

static gboolean sw_button_release_handler(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
	if (event->button == 1)
	{
		GtkWidget *client = gtk_widget_get_parent(widget);

		gInMouseDown = FALSE;
		SendMessage6ToClean (CcWmMOUSE, client, client, BUTTONUP, event->x, event->y, GetModifiers());
	}
};

static gboolean sw_motion_notify_handler(GtkWidget *widget, GdkEventMotion *event, gpointer user_data)
{
	GtkWidget *client = gtk_widget_get_parent(widget);

	if (gInMouseDown)
		SendMessage6ToClean(CcWmMOUSE, client, client, BUTTONSTILLDOWN, event->x, event->y, GetModifiers());
	else
		SendMessage6ToClean (CcWmMOUSE, client, client, BUTTONSTILLUP, event->x, event->y, GetModifiers());
};

static void client_size_allocate(GtkWidget *widget, GtkAllocation *allocation, gpointer user_data)
{
	GtkWidget *sw = (GtkWidget *) user_data;
	SendMessage4ToClean (CcWmSIZE, sw, allocation->width, allocation->height, (int)FALSE);
};

static void client_size_request(GtkWidget *widget, GtkRequisition *requisition, gpointer user_data)
{
	*requisition = *((GtkRequisition *) user_data);
	printf("client_size_request(%d,%d)\n", requisition->width, requisition->height);
};

static gboolean client_delete_handler(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
	printf("client_delete_handler(%d,%d)\n", ((GtkRequisition *) user_data)->width, ((GtkRequisition *) user_data)->height);
	g_free((GtkRequisition *) user_data);
	return gtk_true();
}

static void compute_height(GtkWidget *widget, gpointer data)
{
	GtkRequisition requisition;
	gtk_widget_size_request(widget,&requisition);
	*((int *) data) += requisition.height;
};

/*	Create a SDI document window. */
void EvalCcRqCREATESDIDOCWINDOW (CrossCallInfo *pcci)	/* textptr, frameptr, packed pos, w,h, flags; client ptr result. */
{
	GtkWidget *window, *fixed, *box, *sw;
	const gchar *pwintitle;
	gint left, top, width, height;
	GtkRequisition *requisition;

	pwintitle    = (const gchar *) pcci->p1;
	window       = (GtkWidget *) pcci->p2;
	left		 = pcci->p3>>16;
	top		 = (pcci->p3<<16)>>16;
	width		 = pcci->p4;
	height		 = pcci->p5;

	requisition = g_new(GtkRequisition, 1);
	requisition->width = 0;
	requisition->height = 0;

	/* Adjust the pos and size of the frame window. */
	gtk_widget_set_uposition(window, left, top);

	if (pwintitle)
		gtk_window_set_title(GTK_WINDOW(window), pwintitle);

	/* Create a Scrolled Window */
	sw = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
				      GTK_POLICY_AUTOMATIC,
				      GTK_POLICY_AUTOMATIC);
	box = gtk_bin_get_child(GTK_BIN(window));
	gtk_box_pack_end (GTK_BOX (box), sw, TRUE, TRUE, 0);

	/* Create a Fixed Container */
	fixed = gtk_fixed_new();
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(sw), fixed);

	/* Signals */
	gtk_signal_connect (GTK_OBJECT(fixed), "expose-event",
			GTK_SIGNAL_FUNC(client_expose_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(GTK_BIN(sw)->child), "focus-out-event",
			GTK_SIGNAL_FUNC(sw_focus_out_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(GTK_BIN(sw)->child), "button-press-event",
			GTK_SIGNAL_FUNC(sw_button_press_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(GTK_BIN(sw)->child), "button-release-event",
			GTK_SIGNAL_FUNC(sw_button_release_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(GTK_BIN(sw)->child), "motion_notify_event",
			GTK_SIGNAL_FUNC(sw_motion_notify_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(fixed), "size-allocate",
			GTK_SIGNAL_FUNC(client_size_allocate),
			sw);
	gtk_signal_connect (GTK_OBJECT(fixed), "size-request",
			GTK_SIGNAL_FUNC(client_size_request),
			requisition);
	gtk_signal_connect (GTK_OBJECT(fixed), "delete-event",
			GTK_SIGNAL_FUNC(client_delete_handler),
			requisition);

	gtk_object_set_user_data(GTK_OBJECT(sw), requisition);

	SendMessage1ToClean (CcWmCREATE, sw);

	gtk_widget_realize(window);

	{
		int depth;
		GdkRectangle ext_rect, rect;

		gdk_window_get_geometry(window->window, &rect.x, &rect.y, &rect.width, &rect.height, &depth);
		gdk_window_get_frame_extents (window->window,&ext_rect);

		gtk_container_foreach(GTK_CONTAINER(GTK_BIN(window)->child), compute_height, &ext_rect.height);

		gtk_window_set_default_size(GTK_WINDOW(window),	width+(ext_rect.width - rect.width), height+(ext_rect.height - rect.height));
	}

	gtk_widget_show_all(window);

	gdk_window_set_events(GTK_BIN(sw)->child->window,
	    gdk_window_get_events(GTK_BIN(sw)->child->window) | GDK_BUTTON_RELEASE_MASK | GDK_POINTER_MOTION_MASK);

	MakeReturn1Cci (pcci, (int) sw);
}

/*	Create MDI child window. */
void EvalCcRqCREATEMDIDOCWINDOW (CrossCallInfo *pcci)		/* textptr, clientPtr, behindPtr, packed pos, packed size, flags; HWND result. */
{
	GtkWidget *window, *fixed, *client, *behind, *sw;
	const gchar *pwintitle;
	gint left, top, width, height;
	GtkRequisition *requisition;
	gint index;

	pwintitle = (const gchar *) pcci->p1;
	client    = (GtkWidget *) pcci->p2;
	behind	  = (GtkWidget *) pcci->p3;
	left	  = pcci->p4>>16;
	top		  = (pcci->p4<<16)>>16;
	width     = pcci->p5>>16;
	height    = (pcci->p5<<16)>>16;

	window = gtk_widget_get_parent(gtk_widget_get_parent(client));

	requisition = g_new(GtkRequisition, 1);
	requisition->width = 0;
	requisition->height = 0;

	/* Create a Scrolled Window */
	sw = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
					GTK_POLICY_AUTOMATIC,
					GTK_POLICY_AUTOMATIC);
	index = gtk_notebook_page_num(GTK_NOTEBOOK(client), behind);
	gtk_notebook_insert_page(GTK_NOTEBOOK(client), sw,
					pwintitle ? gtk_label_new(pwintitle) : NULL,
    				index);

	/* Create a Fixed Container */
	fixed = gtk_fixed_new();
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(sw), fixed);

	/* Signals */
	gtk_signal_connect (GTK_OBJECT(fixed), "expose-event",
			GTK_SIGNAL_FUNC(client_expose_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(GTK_BIN(sw)->child), "focus-out-event",
			GTK_SIGNAL_FUNC(sw_focus_out_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(GTK_BIN(sw)->child), "button-press-event",
			GTK_SIGNAL_FUNC(sw_button_press_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(GTK_BIN(sw)->child), "button-release-event",
			GTK_SIGNAL_FUNC(sw_button_release_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(GTK_BIN(sw)->child), "motion_notify_event",
			GTK_SIGNAL_FUNC(sw_motion_notify_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(fixed), "size-allocate",
			GTK_SIGNAL_FUNC(client_size_allocate),
			sw);
	gtk_signal_connect (GTK_OBJECT(fixed), "size-request",
			GTK_SIGNAL_FUNC(client_size_request),
			requisition);
	gtk_signal_connect (GTK_OBJECT(fixed), "delete-event",
			GTK_SIGNAL_FUNC(client_delete_handler),
			requisition);

	gtk_object_set_user_data(GTK_OBJECT(sw), requisition);

	SendMessage1ToClean (CcWmCREATE, sw);

	gtk_widget_show_all(sw);
	gtk_notebook_set_current_page(GTK_NOTEBOOK(client), gtk_notebook_page_num(GTK_NOTEBOOK(client),sw));

	gdk_window_set_events(GTK_BIN(sw)->child->window,
	    gdk_window_get_events(GTK_BIN(sw)->child->window) | GDK_BUTTON_RELEASE_MASK | GDK_POINTER_MOTION_MASK);

	MakeReturn1Cci (pcci, (int) sw);
}

void EvalCcRqSETWINDOWTITLE (CrossCallInfo *pcci)		/* hwnd, textptr		no result. */
{
	GtkWidget *window = (GtkWidget *) pcci->p1;
	gchar *title = (gchar *) pcci->p2;

	if (GTK_IS_WINDOW(window))
		gtk_window_set_title(GTK_WINDOW(window), title);
	else
		if (GTK_IS_LABEL(window))
			gtk_label_set_text(GTK_LABEL(window), title);
		else
			if (GTK_IS_BUTTON(window))
			{
				title = createMnemonicString(title);
				gtk_button_set_label(GTK_BUTTON(window), title);
				rfree(title);
			}
			else
				if (GTK_IS_ENTRY(window))
					gtk_entry_set_text(GTK_ENTRY(window), title);
				else
					if (GTK_IS_TEXT_VIEW(window))
					{
						GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(window));
						gtk_text_buffer_set_text (buffer, title, strlen(title));
					}
					else
						printf("EvalCcRqSETWINDOWTITLE -> unknown widget type");

	MakeReturn0Cci (pcci);
}

void EvalCcRqGETWINDOWTEXT (CrossCallInfo *pcci) /* hwnd;  textptr result. */
{
	G_CONST_RETURN gchar *title = NULL;
	char *textptr;

	GtkWidget *window = (GtkWidget *) pcci->p1;

	if (GTK_IS_WINDOW(window))
			title = gtk_window_get_title(GTK_WINDOW(window));
		else
			if (GTK_IS_LABEL(window))
				title = gtk_label_get_text(GTK_LABEL(window));
			else
				if (GTK_IS_BUTTON(window))
					title = gtk_button_get_label(GTK_BUTTON(window));
				else
					if (GTK_IS_ENTRY(window))
						title = gtk_entry_get_text(GTK_ENTRY(window));
					else
						if (GTK_IS_TEXT_VIEW(window))
						{
							GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(window));
							GtkTextIter start, end;

							gtk_text_buffer_get_start_iter(buffer, &start);
							gtk_text_buffer_get_end_iter(buffer, &end);
							title = gtk_text_buffer_get_text (buffer, &start, &end, gtk_true());
						}
						else
							printf("EvalCcRqSETWINDOWTITLE -> unknown widget type");

	if (GTK_IS_BUTTON(window))
		textptr = createMnemonicString(title);
	else
		textptr =  g_strdup(title);

	MakeReturn1Cci (pcci, (int) textptr);
}

/*	Update rect part of a window. */
void EvalCcRqUPDATEWINDOWRECT (CrossCallInfo *pcci) /* hwnd, left,top,right,bottom; no result. */
{
/*	RECT rect;
	HWND hwnd;

	hwnd       = (HWND) pcci->p1;
	rect.left  = pcci->p2;
	rect.top   = pcci->p3;
	rect.right = pcci->p4;
	rect.bottom= pcci->p5;

	InvalidateRect (hwnd,&rect,FALSE);
	UpdateWindow (hwnd);
	RedrawWindow (hwnd,&rect,NULL,RDW_FRAME | RDW_VALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN);
*/
	printf("EvalCcRqUPDATEWINDOWRECT -> not implemented\n");
	MakeReturn0Cci (pcci);
}

/*	Set the ClientRect. */
void EvalCcRqSETCLIENTSIZE (CrossCallInfo *pcci) /* hwnd, width, height; no result. */
{
/*	HWND hwnd;
	int w,h,curw,curh,clientw,clienth;
	UINT flags;
	RECT clientRect,windowRect;

	hwnd  = (HWND) pcci->p1;
	w     = pcci->p2;
	h     = pcci->p3;
	flags = SWP_NOMOVE			// retain position
		  | SWP_NOZORDER;		// retain Z order

	GetClientRect (hwnd, &clientRect);
	GetWindowRect (hwnd, &windowRect);
	clientw = clientRect.right - clientRect.left;
	clienth = clientRect.bottom- clientRect.top;
	curw    = windowRect.right - windowRect.left;
	curh    = windowRect.bottom- windowRect.top;

	SetWindowPos (hwnd, HWND_TOP, 0,0, curw+w-clientw,curh+h-clienth, flags);*/
	printf("EvalCcRqSETCLIENTSIZE -> not implemented\n");
	MakeReturn0Cci (pcci);
}

/*	(En/Dis)able windows/dialogues. */
void EvalCcRqSETSELECTWINDOW (CrossCallInfo *pcci)	/* hwnd, hasHScroll, hasVScroll, toAble, modalContext; no result. */
{
#if 0
	HWND window;
	BOOL hasHScroll, hasVScroll, toAble, modalContext;

	window       = (HWND) pcci->p1;
	hasHScroll   = (BOOL) pcci->p2;
	hasVScroll   = (BOOL) pcci->p3;
	toAble       = (BOOL) pcci->p4;
	modalContext = (BOOL) pcci->p5;

	if (modalContext)					/*	if not a modal context, then do not disable window	*/
		EnableWindow (window,toAble);	/*  because it can't be moved, or closed. */
	if (hasHScroll)
		EnableScrollBar (window,SB_HORZ,toAble ? ESB_ENABLE_BOTH : ESB_DISABLE_BOTH);
	if (hasVScroll)
		EnableScrollBar (window,SB_VERT,toAble ? ESB_ENABLE_BOTH : ESB_DISABLE_BOTH);
#endif
	printf("EvalCcRqSETSELECTWINDOW -> not implemented\n");
	MakeReturn0Cci (pcci);
}

/*	Set the position of windows/controls. */
void EvalCcRqSETWINDOWPOS (CrossCallInfo *pcci)	/* hwnd, x,y, update, include scrollbars ; no result. */
{
	GtkWidget *widget, *parent;
	int x,y;
	gboolean update,inclScrollbars;

	widget = (GtkWidget *) pcci->p1;
	x              = pcci->p2;
	y              = pcci->p3;
	update         = pcci->p4;
	inclScrollbars = pcci->p5;
	parent         = gtk_widget_get_parent(widget);

	if (parent)
	  gtk_fixed_move(GTK_FIXED(parent), widget, x, y);
	else
	  gtk_widget_set_uposition(widget, x, y);

	if (GTK_WIDGET_VISIBLE(widget) && update!=0)
	{	// only if window is visible and update is requested, proceed to enforce update.
		if (inclScrollbars)
		  {
		  }
		else
		{
			gtk_widget_queue_draw(widget);
		}
	}

	MakeReturn0Cci (pcci);
}

/*	Get the size of the bounding rectangle of windows/controls. */
void EvalCcRqGETWINDOWSIZE (CrossCallInfo *pcci) /* hwnd; width,height result. */
{
	GtkAllocation *alloc = &(((GtkWidget *) pcci->p1)->allocation);
	MakeReturn2Cci (pcci, alloc->width, alloc->height);
}

/*	Set the size of windows/controls. */
void EvalCcRqSETWINDOWSIZE (CrossCallInfo *pcci) /* hwnd, w,h, update; no result. */
{
#if 0
	HWND hwnd;
	int w,h;
	BOOL update;
	UINT flags;

	hwnd   = (HWND)pcci->p1;
	w      = pcci->p2;
	h      = pcci->p3;
	update = pcci->p4;
	flags  = SWP_NOMOVE			/* retain position */
		   | SWP_NOZORDER;		/* retain Z order  */
	/* flags do not contain SWP_NOREDRAW because otherwise not all windows get updated properly. */

	SetWindowPos (hwnd, HWND_TOP, 0,0, w,h, flags);
	if (update!=0)				/* still, updates are not sufficient using SetWindowPos only. */
		UpdateWindowScrollbars (hwnd);
#endif
	printf("EvalCcRqSETWINDOWSIZE -> not implemented\n");
	MakeReturn0Cci (pcci);
}

/*	Activate control. */
void EvalCcRqACTIVATECONTROL (CrossCallInfo *pcci)	/* controlPtr; no result. */
{
	gtk_widget_grab_focus((GtkWidget *) pcci->p1);
	MakeReturn0Cci (pcci);
}

/*	Activate window. */
void EvalCcRqACTIVATEWINDOW (CrossCallInfo *pcci)	/* isMDI, clientPtr, thisWindow; no result. */
{
	gboolean isMDI;
	GtkWidget *client, *thisWindow;

	isMDI       = (gboolean) pcci->p1;
	client      = (GtkWidget *) pcci->p2;
	thisWindow  = (GtkWidget *) pcci->p3;

	if (isMDI)
		gtk_notebook_set_page(GTK_NOTEBOOK(client), gtk_notebook_page_num(GTK_NOTEBOOK(client), thisWindow));
	else
		 gtk_window_activate_focus (GTK_WINDOW(thisWindow));
	MakeReturn0Cci (pcci);
}

static unsigned char hidden_cursor_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

static GdkColor black_color = { 0, 0, 0, 0 };

void EvalCcRqCHANGEWINDOWCURSOR (CrossCallInfo *pcci)	/* hwnd, cursor code; no result. It is assumed that the hwnd argument	*/
														/* corresponds to either a SDI/MDI window (and not frame).				*/
{
	GtkWidget *widget;
	int cursorcode;

	widget = GTK_BIN(GTK_BIN((GtkWidget *) pcci->p1)->child)->child;
	cursorcode = pcci->p2;

	switch (cursorcode)
	{
		case CURSARROW:
			if (!gArrowCursor)
			{
				gArrowCursor = gdk_cursor_new(GDK_ARROW);
			};

			gdk_window_set_cursor(widget->window, gArrowCursor);
			break;
		case CURSBUSY:
			if (!gBusyCursor)
			{
				gBusyCursor = gdk_cursor_new(GDK_CLOCK);
			};

			gdk_window_set_cursor(widget->window, gBusyCursor);
			break;
		case CURSIBEAM:
			if (!gIBeamCursor)
			{
			    gIBeamCursor = gdk_cursor_new(GDK_XTERM);
			};

			gdk_window_set_cursor(widget->window, gIBeamCursor);
			break;
		case CURSCROSS:
			if (!gCrossCursor)
			{
				gCrossCursor = gdk_cursor_new(GDK_CROSSHAIR);
			};

			gdk_window_set_cursor(widget->window, gCrossCursor);
			break;
		case CURSFATCROSS:
			if (!gFatCrossCursor)
			{
				gFatCrossCursor = gdk_cursor_new(GDK_CROSS);
			};

			gdk_window_set_cursor(widget->window, gFatCrossCursor);
			break;
		case CURSHIDDEN:
			if (!gHiddenCursor)
			{
				GdkPixmap *pixmap;

				pixmap = gdk_bitmap_create_from_data (NULL, hidden_cursor_bits, 16, 16);
				gHiddenCursor = gdk_cursor_new_from_pixmap (pixmap, pixmap, &black_color, &black_color, 8, 8);
				gdk_pixmap_unref (pixmap);
			};

			gdk_window_set_cursor(widget->window, gHiddenCursor);
			break;
 	}

	MakeReturn0Cci (pcci);
}

void EvalCcRqOBSCURECURSOR (CrossCallInfo *pcci) /* no params; no result. */
{
    GtkWidget *widget;

    widget = (GtkWidget *) pcci->p1;

    if (!gHiddenCursor)
    {
		GdkPixmap *pixmap;

		pixmap = gdk_bitmap_create_from_data (NULL, hidden_cursor_bits, 16, 16);
		gHiddenCursor = gdk_cursor_new_from_pixmap (pixmap, pixmap, &black_color, &black_color, 8, 8);
		gdk_pixmap_unref (pixmap);
    };

    gdk_window_set_cursor(widget->window, gHiddenCursor);
    MakeReturn0Cci (pcci);
}

void DeleteCursors()
{
	if (gArrowCursor)
	{
		gdk_cursor_destroy(gArrowCursor);
		gArrowCursor = NULL;
	};

	if (gBusyCursor)
	{
		gdk_cursor_destroy(gBusyCursor);
		gIBeamCursor = NULL;
	};

	if (gIBeamCursor)
	{
		gdk_cursor_destroy(gIBeamCursor);
		gIBeamCursor = NULL;
	};

	if (gCrossCursor)
	{
		gdk_cursor_destroy(gCrossCursor);
		gCrossCursor = NULL;
	};

	if (gFatCrossCursor)
	{
		gdk_cursor_destroy(gFatCrossCursor);
		gFatCrossCursor = NULL;
	};

	if (gHiddenCursor)
	{
		gdk_cursor_destroy(gHiddenCursor);
		gHiddenCursor = NULL;
	};
}

/*	Set range of scrollbars. */
void EvalCcRqSETSCROLLRANGE (CrossCallInfo *pcci)	/* hwnd, iBar, min, max, redraw, no result */
{
	GtkWidget *widget;
	GtkAdjustment *adj;
	GtkRequisition *requisition;
	int iBar, min, max;
	gboolean redraw;

	widget = (GtkWidget *) pcci->p1;
	iBar   = pcci->p2;
	min    = pcci->p3;		// maxx   is the right-most  x coordinate of the enclosing rectangle of the scrollbar
	max    = pcci->p4;		// maxy   is the bottom-most y coordinate of the enclosing rectangle of the scrollbar
	redraw = pcci->p5;		// extent is the width (height) of the vertical (horizontal) scrollbar

	requisition = (GtkRequisition *) gtk_object_get_user_data(GTK_OBJECT(widget));

	if (iBar==0)
	{
		requisition->width = max-min;
		adj = gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(widget));
	}
	else
	{
		requisition->height = max-min;
		adj = gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(widget));
	}

	printf("client_size_request(%d,%d)\n", requisition->width, requisition->height);

	adj->lower = min;
	adj->upper = max;
	gtk_adjustment_changed(adj);

	MakeReturn0Cci (pcci);
}

/*	Set pos of scrollbars. */
void EvalCcRqSETSCROLLPOS (CrossCallInfo *pcci)	/* hwnd, iBar, thumb, maxx, maxy, extent, no result */
{
	GtkWidget *widget;
	int thumb, iBar, maxx, maxy, extent;

	widget = (GtkWidget *) pcci->p1;
	iBar   = pcci->p2;
	thumb  = pcci->p3;
	maxx   = pcci->p4;		// maxx   is the right-most  x coordinate of the enclosing rectangle of the scrollbar
	maxy   = pcci->p5;		// maxy   is the bottom-most y coordinate of the enclosing rectangle of the scrollbar
	extent = pcci->p6;		// extent is the width (height) of the vertical (horizontal) scrollbar

	if (iBar==0)
		gtk_adjustment_set_value(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(widget)), thumb);
	else
		gtk_adjustment_set_value(gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(widget)), thumb);

	MakeReturn0Cci (pcci);
}

/*	Set thumb size of scrollbars. */
void EvalCcRqSETSCROLLSIZE (CrossCallInfo *pcci)	/* hwnd, iBar, size, maxx, maxy, extent, no result */
{
	GtkWidget *widget;
	GtkAdjustment *adj;
	int size, iBar, maxx, maxy, extent;

	widget = (GtkWidget *) pcci->p1;
	iBar   = pcci->p2;
	size   = pcci->p3;
	maxx   = pcci->p4;		// maxx   is the right-most  x coordinate of the enclosing rectangle of the scrollbar
	maxy   = pcci->p5;		// maxy   is the bottom-most y coordinate of the enclosing rectangle of the scrollbar
	extent = pcci->p6;		// extent is the width (height) of the vertical (horizontal) scrollbar

	if (iBar==0)
		adj = gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(widget));
	else
		adj = gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(widget));

	adj->page_size = size;
	gtk_adjustment_changed(adj);

	MakeReturn0Cci (pcci);
}

/*	Set selection of edit controls. */
void EvalCcRqSETEDITSELECTION (CrossCallInfo *pcci)	/* hwnd, first, last, no result. */
{
/*	HWND hwnd;
	int first,last;

	hwnd  = (HWND) pcci->p1;
	first = pcci->p2;
	last  = pcci->p3;

	SendMessage (hwnd, EM_SETSEL, (WPARAM) first, (LPARAM) last);		// Set the selection of the edit control.
	SendMessage (hwnd, EM_SCROLLCARET, 0,0);		// Let the caret be displayed - (w/l)Param MUST be 0.
*/
	printf("EvalCcRqSETEDITSELECTION -> not implemented\n");
	MakeReturn0Cci (pcci);
}

extern GtkWidget *gActiveTopLevelWindow;

static void dialog_focus_in_handler(GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
	SendMessage1ToClean (CcWmACTIVATE, widget);
	GTK_WIDGET_GET_CLASS(widget)->focus_in_event(widget, event);
	gActiveTopLevelWindow = widget;
}

static void dialog_focus_out_handler(GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
	SendMessage1ToClean (CcWmDEACTIVATE, widget);
	GTK_WIDGET_GET_CLASS(widget)->focus_out_event(widget, event);
	gActiveTopLevelWindow = NULL;
}

static gboolean dialog_close_handler(GtkWidget *dialog, GdkEvent *event, gpointer user_data)
{
	SendMessage1ToClean(CcWmCLOSE, (int) dialog);
	gtk_signal_emit_stop_by_name(GTK_OBJECT(dialog), "delete-event");
	return gtk_true();
}

/*	EvalCcRqCREATEDIALOG is now restricted to modeless dialogues only. */
void EvalCcRqCREATEDIALOG (CrossCallInfo *pcci)	// textptr,parentptr,behindPtr; HWND result.
{
	GtkWidget *dialog, *fixed, *defctrl;
	const gchar *pwintitle;
	int x, y, w, h;

	pwintitle    = (const gchar *) pcci->p1;

	dialog = gtk_dialog_new();
	gtk_dialog_set_has_separator(GTK_DIALOG(dialog), gtk_false());
	gtk_window_set_resizable(GTK_WINDOW(dialog), gtk_false());
	gtk_signal_connect (GTK_OBJECT(dialog), "delete-event",
		GTK_SIGNAL_FUNC(dialog_close_handler),
		NULL);
	gtk_signal_connect (GTK_OBJECT(dialog), "focus-in-event",
		GTK_SIGNAL_FUNC(dialog_focus_in_handler),
		NULL);
	gtk_signal_connect (GTK_OBJECT(dialog), "focus-out-event",
		GTK_SIGNAL_FUNC(dialog_focus_out_handler),
		NULL);

	if (pwintitle)
		gtk_window_set_title(GTK_WINDOW(dialog), pwintitle);

	/* Create a Fixed Container */
	fixed = gtk_fixed_new();
	gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), fixed, TRUE, TRUE, 0);
	gtk_widget_show(fixed);

	SendMessage1ToClean (CcWmINITDIALOG, (int) dialog);

	x = gCci.p1;
	y = gCci.p2;
	w = gCci.p3;
	h = gCci.p4;
	defctrl = (GtkWidget *) gCci.p5;

  	w += GTK_WINDOW(dialog)->frame_left + GTK_WINDOW(dialog)->frame_right;
	h += GTK_WINDOW(dialog)->frame_top  + GTK_WINDOW(dialog)->frame_bottom;

	/* Adjust the pos and size of the frame window. */
	gtk_widget_set_usize(dialog, w, h);
	if (x == -1 && y == -1)
		gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
	else
	  {
	    gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_NONE);
		gtk_widget_set_uposition(dialog, x, y);
	  }

	if (defctrl != NULL)
	{
		gtk_widget_grab_focus(defctrl);
	}

	gtk_widget_show(dialog);

	MakeReturn1Cci (pcci, (int) dialog);
}

//	Create modal dialogues.
void EvalCcRqCREATEMODALDIALOG (CrossCallInfo *pcci)	/* textptr,parentptr; error code result. */
{
	GtkWidget *dialog, *fixed, *defctrl, *parent;
	const gchar *pwintitle;
	int x, y, w, h;
	guint delete_handler;

	pwintitle    = (const gchar *) pcci->p1;
	parent		 = (GtkWidget *) pcci->p2;

	dialog = gtk_dialog_new();
	gtk_dialog_set_has_separator(GTK_DIALOG(dialog), gtk_false());
	gtk_window_set_resizable(GTK_WINDOW(dialog), gtk_false());
	gtk_signal_connect (GTK_OBJECT(dialog), "focus-in-event",
		GTK_SIGNAL_FUNC(dialog_focus_in_handler),
		NULL);
	gtk_signal_connect (GTK_OBJECT(dialog), "focus-out-event",
		GTK_SIGNAL_FUNC(dialog_focus_out_handler),
		NULL);
	gtk_signal_connect (GTK_OBJECT(dialog), "delete-event",
		GTK_SIGNAL_FUNC(dialog_close_handler),
		NULL);

	if (pwintitle)
		gtk_window_set_title(GTK_WINDOW(dialog), pwintitle);

	/* Create a Fixed Container */
	fixed = gtk_fixed_new();
	gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), fixed, TRUE, TRUE, 0);
	gtk_widget_show(fixed);

	SendMessage1ToClean (CcWmINITDIALOG, (int) dialog);

	x = gCci.p1;
	y = gCci.p2;
	w = gCci.p3;
	h = gCci.p4;
	defctrl = (GtkWidget *) gCci.p5;

  	w += GTK_WINDOW(dialog)->frame_left + GTK_WINDOW(dialog)->frame_right;
	h += GTK_WINDOW(dialog)->frame_top  + GTK_WINDOW(dialog)->frame_bottom;

	/* Adjust the pos and size of the frame window. */
	gtk_widget_set_usize(dialog, w, h);
	if (x == -1 && y == -1)
		gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
	else
	  {
	    gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_NONE);
		gtk_widget_set_uposition(dialog, x, y);
	  }

	if (defctrl != NULL)
	{
		gtk_widget_grab_focus(defctrl);
	}

	while (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_DELETE_EVENT);
	gtk_widget_destroy(dialog);

	MakeReturn1Cci (pcci,0/*errorcode*/);
}

static gboolean widget_focus_in_handler(GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
	GtkWidget *my_widget = (GtkWidget *) user_data;
	GTK_WIDGET_GET_CLASS(widget)->focus_in_event(widget, event);
	SendMessage2ToClean (CcWmSETFOCUS, GetControlParent(my_widget), my_widget);
	return gtk_false();
}

static gboolean widget_focus_out_handler(GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
	GtkWidget *my_widget = (GtkWidget *) user_data;
	GtkWidget *parent = GetControlParent(my_widget);

	GTK_WIDGET_GET_CLASS(widget)->focus_in_event(widget, event);

	if (gInKey)
	{
		SendKeyUpToClean (parent, my_widget, gCurChar);
		gInKey = FALSE;
		gCurChar = 0;
	}

	SendMessage2ToClean (CcWmKILLFOCUS, parent, my_widget);
	return gtk_false();
}

static gboolean widget_key_press_handler(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
	GtkWidget *my_widget = (GtkWidget *) user_data;
	GtkWidget *parent = GetControlParent(my_widget);
	int c = (event->length > 0) ? event->string[0] : CheckVirtualKeyCode (event->keyval);
	if (!c) return gtk_false();

	if (event->keyval == GDK_Tab)
		return gtk_false();

	GTK_WIDGET_GET_CLASS(widget)->key_press_event(widget, event);

	if (gInKey)
	{
		if (gCurChar == c)
			SendKeyStillDownToClean (parent, my_widget, gCurChar);
		else
		{
			SendKeyUpToClean (parent, my_widget, gCurChar);
			gCurChar = c;
			SendKeyDownToClean (parent, my_widget, gCurChar);
		}
	}
	else
	{
		gCurChar = c;
		SendKeyDownToClean (parent, my_widget, gCurChar);
		gInKey = TRUE;
	}

	return gtk_false();
};

static gboolean widget_key_release_handler(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
	GtkWidget *my_widget = (GtkWidget *) user_data;

	if (event->keyval == GDK_Tab)
		return gtk_false();

	GTK_WIDGET_GET_CLASS(widget)->key_press_event(widget, event);

	if (gInKey)
	{
		SendKeyUpToClean (GetControlParent(my_widget), my_widget, gCurChar);
		gInKey = FALSE;
		gCurChar = 0;
	}

	return gtk_false();
};

static gboolean widget_button_press_handler(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
	if (event->button == 1)
	{
		GtkWidget *parent = GetControlParent(widget);

		gtk_widget_grab_focus(widget);

		gInMouseDown = TRUE;

		switch (event->type)
		{
		case GDK_BUTTON_PRESS:
			SendMessage6ToClean (CcWmMOUSE, parent, widget, BUTTONDOWN, event->x, event->y, GetModifiers());
			break;
		case GDK_2BUTTON_PRESS:
			SendMessage6ToClean (CcWmMOUSE, parent, widget, BUTTONDOUBLEDOWN, event->x, event->y, GetModifiers());
			break;
		case GDK_3BUTTON_PRESS:
			SendMessage6ToClean (CcWmMOUSE, parent, widget, BUTTONTRIPLEDOWN, event->x, event->y, GetModifiers());
			break;
		}
	}
};

static gboolean widget_button_release_handler(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
	if (event->button == 1)
	{
		GtkWidget *parent = GetControlParent(widget);

		gInMouseDown = FALSE;
		SendMessage6ToClean (CcWmMOUSE, parent, widget, BUTTONUP, event->x, event->y, GetModifiers());
	}
};

static gboolean widget_motion_notify_handler(GtkWidget *widget, GdkEventMotion *event, gpointer user_data)
{
	GtkWidget *parent = GetControlParent(widget);

	if (gInMouseDown)
		SendMessage6ToClean(CcWmMOUSE, parent, widget, BUTTONSTILLDOWN, event->x, event->y, GetModifiers());
	else
		SendMessage6ToClean (CcWmMOUSE, parent, widget, BUTTONSTILLUP, event->x, event->y, GetModifiers());
};

/*	Create compound controls (window in window) */
void EvalCcRqCREATECOMPOUND (CrossCallInfo *pcci)	/* hwnd, packed pos,w,h, scrollbars, transparent; HWND result. */
{
#if 0
	HWND parentwindow, compoundhandle;
	int left,top, width,height;
	int compoundstyle;
	BOOL transparent;
	DWORD compoundExStyle;

	parentwindow  = (HWND) pcci->p1;
	left          = pcci->p2>>16;
	top           = (pcci->p2<<16)>>16;
	width         = pcci->p3;
	height        = pcci->p4;
	compoundstyle = pcci->p5;
	transparent   = (BOOL) pcci->p6;

	compoundExStyle = WS_EX_CONTROLPARENT;
	if (transparent)
		 compoundExStyle |= WS_EX_TRANSPARENT;

	compoundstyle |= WS_CHILD;// | WS_CLIPSIBLINGS;

	/* create the compound window */
	compoundhandle
		= CreateWindowEx (compoundExStyle,				/* Extended style				 */
						  CompoundControlClassName,		/* Class name					 */
						  "",							/* Window title 				 */
						  compoundstyle,				/* style flags					 */
						  left, top,					/* x, y 						 */
						  width, height,		 		/* width, height				 */
						  parentwindow,					/* Parent window				 */
						  NULL,							/* menu handle					 */
						  (HANDLE) ghInst,				/* Instance that owns the window */
						  0);
	SetWindowPos (compoundhandle, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack
#endif
	printf("EvalCcRqCREATECOMPOUND -> not implemented\n");
	MakeReturn1Cci (pcci, (int) NULL /*compoundhandle*/);
}

static void scrollbar_value_changed(GtkAdjustment *adjustment, gpointer user_data)
{
}

/*	Create scrollbars. */
void EvalCcRqCREATESCROLLBAR (CrossCallInfo *pcci)	/* hwnd, x,y,w,h bool; HWND result. */
{
	int x, y, w, h;
	GtkWidget *scroll;
	GtkWidget *parent;
	gboolean ishorizontal;

	parent = (GtkWidget *) pcci->p1;
	x = pcci->p2;
	y = pcci->p3;
	w = pcci->p4;
	h = pcci->p5;
	ishorizontal = pcci->p6;

	if (ishorizontal)
		scroll = gtk_hscrollbar_new(NULL);
	else
		scroll = gtk_vscrollbar_new(NULL);

	gtk_signal_connect(GTK_OBJECT(gtk_range_get_adjustment(GTK_RANGE(scroll))), "value-changed",
			GTK_SIGNAL_FUNC(scrollbar_value_changed),
			NULL);
	gtk_widget_set_usize(scroll, w, h);
	gtk_fixed_put(GetFixed(parent), scroll, x, y);

	MakeReturn1Cci (pcci, (int) scroll);
}

static void button_clicked (GtkButton *button, gpointer user_data)
{
	GtkWidget *wbutton = GTK_WIDGET(button);
	GtkWidget *window  = GetControlParent(wbutton);

	switch (GPOINTER_TO_INT(user_data))
	{
		case ISOKBUTTON:
			SendMessage2ToClean (CcWmSPECIALBUTTON, window, ISOKBUTTON);
			return;
		case ISCANCELBUTTON:
			SendMessage2ToClean (CcWmSPECIALBUTTON, window, ISCANCELBUTTON);
			return;
		default:
			SendMessage4ToClean (CcWmBUTTONCLICKED, window, wbutton, GetModifiers (), 0);
			return;
	};
};

static gint button_expose_handler(GtkWidget *widget, GdkEventExpose *event, gpointer user_data)
{
  GtkWidget *button = gtk_widget_get_parent(widget);
	GtkWidget *parent = gtk_widget_get_parent(gtk_widget_get_parent(gtk_widget_get_parent(button)));
	SendMessage3ToClean(CcWmDRAWCONTROL, (int) parent, (int) button, (int) GDK_DRAWABLE(event->window));
	return 0;
};

void EvalCcRqCREATEBUTTON (CrossCallInfo *pcci)	/* hwnd, x,y,w,h, kind;  HWND result. */
{
	GtkWidget *button, *parent;
	int x, y, w, h, kind;

	parent	= (GtkWidget *) pcci->p1;
	x		= pcci->p2;
	y		= pcci->p3;
	w		= pcci->p4;
	h		= pcci->p5;
	kind	= pcci->p6;

	if (kind==ISOKBUTTON)
	{
		button = gtk_button_new_from_stock("gtk-ok");
	}
	else
	{
		if (kind==ISCANCELBUTTON)
		{
			button = gtk_button_new_from_stock("gtk-quit");
		}
		else
		{
			button = gtk_button_new();
			gtk_button_set_use_underline(GTK_BUTTON(button), gtk_true());
		}
	}

	gtk_signal_connect (GTK_OBJECT (button), "clicked",
			GTK_SIGNAL_FUNC(button_clicked),
			GINT_TO_POINTER(kind));
	gtk_widget_set_usize(button, w, h);
	gtk_fixed_put (GetFixed(parent), button, x, y);

 	MakeReturn1Cci (pcci, (int) button);
}

void EvalCcRqCREATEICONBUT (CrossCallInfo *pcci)	/* hwnd, x,y,w,h,kind; HWND result. */
{
	GtkWidget *button, *parent, *drawing_area;
	int x, y, w, h, kind;

	parent	= (GtkWidget *) pcci->p1;
	x		= pcci->p2;
	y		= pcci->p3;
	w		= pcci->p4;
	h		= pcci->p5;
	kind	= pcci->p6;

	button = gtk_button_new();
	drawing_area = gtk_drawing_area_new();
	gtk_container_add(GTK_CONTAINER(button), drawing_area);
	gtk_signal_connect(GTK_OBJECT (button), "clicked",
			GTK_SIGNAL_FUNC(button_clicked),
			GINT_TO_POINTER(kind));
	gtk_signal_connect (GTK_OBJECT(drawing_area), "expose-event",
			GTK_SIGNAL_FUNC(button_expose_handler),
			NULL);

	gtk_widget_set_usize(button, w, h);
	gtk_fixed_put(GetFixed(parent), button, x, y);

 	MakeReturn1Cci(pcci, (int) button);
}

static gint custom_expose_handler(GtkWidget *widget, GdkEventExpose *event, gpointer user_data)
{
	GtkWidget *parent = gtk_widget_get_parent(gtk_widget_get_parent(gtk_widget_get_parent(widget)));
	SendMessage3ToClean(CcWmDRAWCONTROL, (int) parent, (int) widget, (int) GDK_DRAWABLE(event->window));
	return 0;
};

void EvalCcRqCREATECUSTOM (CrossCallInfo *pcci)	/* hwnd, x,y,w,h; HWND result. */
{
	GtkWidget *ctrl, *parent;
	int x, y, w, h;

	parent	= (GtkWidget *) pcci->p1;
	x		= pcci->p2;
	y		= pcci->p3;
	w		= pcci->p4;
	h		= pcci->p5;

	ctrl = gtk_drawing_area_new();
	GTK_WIDGET_SET_FLAGS(ctrl, GTK_CAN_FOCUS);
	gtk_signal_connect(GTK_OBJECT(ctrl), "expose-event",
			GTK_SIGNAL_FUNC(custom_expose_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(ctrl), "focus-in-event",
			GTK_SIGNAL_FUNC(widget_focus_in_handler),
			ctrl);
	gtk_signal_connect (GTK_OBJECT(ctrl), "focus-out-event",
			GTK_SIGNAL_FUNC(widget_focus_out_handler),
			ctrl);
	gtk_signal_connect (GTK_OBJECT(ctrl), "key-press-event",
			GTK_SIGNAL_FUNC(widget_key_press_handler),
			ctrl);
	gtk_signal_connect (GTK_OBJECT(ctrl), "key-release-event",
			GTK_SIGNAL_FUNC(widget_key_release_handler),
			ctrl);
	gtk_signal_connect (GTK_OBJECT(ctrl), "button-press-event",
			GTK_SIGNAL_FUNC(widget_button_press_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(ctrl), "button-release-event",
			GTK_SIGNAL_FUNC(widget_button_release_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(ctrl), "motion_notify_event",
			GTK_SIGNAL_FUNC(widget_motion_notify_handler),
			NULL);
	gtk_widget_set_usize(ctrl, w, h);
	gtk_fixed_put (GetFixed(parent), ctrl, x, y);

	gtk_widget_realize(ctrl);
	gtk_widget_add_events(ctrl, GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK | GDK_POINTER_MOTION_MASK | GDK_KEY_PRESS_MASK | GDK_KEY_RELEASE_MASK);

	MakeReturn1Cci (pcci, (int) ctrl);
}

void EvalCcRqCREATESTATICTXT (CrossCallInfo *pcci)		/* hwnd, x,y,w,h; HWND result. */
{
	int x, y, w, h;
	GtkWidget *parent, *label;

	parent = (GtkWidget *) pcci->p1;
	x = pcci->p2;
	y = pcci->p3;
	w = pcci->p4;
	h = pcci->p5;

	label =  gtk_label_new(NULL);
	gtk_widget_set_usize(label, w, h);
	gtk_fixed_put (GetFixed(parent), label, x, y);

	MakeReturn1Cci (pcci, (int) label);
}

void EvalCcRqCREATEEDITTXT (CrossCallInfo *pcci) /* hwnd, x,y,w,h, flags; HWND result. */
{
	GtkWidget *edit;
	GtkWidget *parent;
	int x, y, w, h, flags;

	parent			= (GtkWidget *) pcci->p1;
	x				= pcci->p2;
	y				= pcci->p3;
	w				= pcci->p4;
	h				= pcci->p5;
	flags			= pcci->p6;

	if (flags & EDITISMULTILINE)
		edit = gtk_text_view_new();
	else
		edit = gtk_entry_new();

	gtk_widget_set_usize(edit, w, h);
	gtk_fixed_put (GetFixed(parent), edit, x, y);


	gtk_signal_connect (GTK_OBJECT(edit), "focus-in-event",
			GTK_SIGNAL_FUNC(widget_focus_in_handler),
			edit);
	gtk_signal_connect (GTK_OBJECT(edit), "focus-out-event",
			GTK_SIGNAL_FUNC(widget_focus_out_handler),
			edit);
	if (flags & EDITISKEYSENSITIVE)
	{
	  gtk_signal_connect (GTK_OBJECT(edit), "key-press-event",
			GTK_SIGNAL_FUNC(widget_key_press_handler),
			edit);
	  gtk_signal_connect (GTK_OBJECT(edit), "key-release-event",
			GTK_SIGNAL_FUNC(widget_key_release_handler),
			edit);
	}

	MakeReturn1Cci (pcci, (int) edit);
}

static void radio_button_clicked (GtkButton *button, gpointer user_data)
{
	GtkWidget *wbutton = GTK_WIDGET(button);
	GtkWidget *window  = GetControlParent(wbutton);

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wbutton)))
		SendMessage4ToClean (CcWmBUTTONCLICKED, window, wbutton, GetModifiers (), 0);

};

void EvalCcRqCREATERADIOBUT (CrossCallInfo *pcci)		/* hwnd, x,y,w,h, isfirst;	HWND result. */
{
	GtkWidget *radio_btn;
	GtkWidget *parent;
	int x, y, w, h, first;

	parent	= (GtkWidget *) pcci->p1;
	x	= pcci->p2;
	y	= pcci->p3;
	w	= pcci->p4;
	h	= pcci->p5;
	first	= pcci->p6;

	if (first || !gFirstRadioButton)
	{
		radio_btn = gtk_radio_button_new(NULL);
		gFirstRadioButton = radio_btn;
	}
	else
		radio_btn = gtk_radio_button_new_from_widget(GTK_RADIO_BUTTON(gFirstRadioButton));

	gtk_button_set_use_underline(GTK_BUTTON(radio_btn), gtk_true());
	gtk_widget_set_usize(radio_btn, w, h);
	gtk_fixed_put (GetFixed(parent), radio_btn, x, y);

	gtk_signal_connect (GTK_OBJECT (radio_btn), "clicked",
			GTK_SIGNAL_FUNC(radio_button_clicked),
			NULL);

	MakeReturn1Cci (pcci, (int) radio_btn);
}

void EvalCcRqCREATECHECKBOX (CrossCallInfo *pcci)		/* hwnd, x,y,w,h, isfirst; HWND result. */
{
	GtkWidget *check_btn;
	GtkWidget *parent;
	int x, y, w, h, first;

	parent	= (GtkWidget *) pcci->p1;
	x	= pcci->p2;
	y	= pcci->p3;
	w	= pcci->p4;
	h	= pcci->p5;
	first	= pcci->p6;


	check_btn = gtk_check_button_new();
	gtk_button_set_use_underline(GTK_BUTTON(check_btn), gtk_true());
	gtk_widget_set_usize(check_btn, w, h);
	gtk_fixed_put (GetFixed(parent), check_btn, x, y);

	gtk_signal_connect (GTK_OBJECT (check_btn), "toggled",
			GTK_SIGNAL_FUNC(button_clicked),
			NULL);

	MakeReturn1Cci (pcci, (int) check_btn);
}

void EvalCcRqSETITEMCHECK (CrossCallInfo *pcci)	/* hwnd, bool; no result. */
{
	GtkWidget *widget;
	gboolean  on;

	widget = (GtkWidget *) pcci->p1;
	on     = (gboolean) pcci->p2;

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), on);

	MakeReturn0Cci (pcci);
}

void EvalCcRqENABLECONTROL (CrossCallInfo *pcci) /* hwnd, bool; no result. */
{
	GtkWidget *widget = (GtkWidget *) pcci->p1;
	gboolean newSelect = (gboolean) pcci->p2;

	gtk_widget_set_sensitive(widget, newSelect);
	MakeReturn0Cci (pcci);
}

void EvalCcRqSHOWCONTROL (CrossCallInfo *pcci)	// hwnd, bool; no result.
{
	GtkWidget *control;
	gboolean show;

	control = (GtkWidget *) pcci->p1;
	show    = (gboolean) pcci->p2;

	if (!show)
		gtk_widget_hide(control);
	else
		gtk_widget_show(control);

	MakeReturn0Cci (pcci);
}

/* Hide/show windows. */
void EvalCcRqSHOWWINDOW (CrossCallInfo *pcci)	/* hwnd, show, activate; no result. */
{
	GtkWidget *window;
	gboolean show, activate;

	window   = (GtkWidget *) pcci->p1;
	show     = (gboolean) pcci->p2;
	activate = (gboolean) pcci->p3;

	if (!show)
		gtk_widget_hide(window);
	else
		gtk_widget_show(window);

	if (activate)
		gtk_window_activate_default(GTK_WINDOW(window));

	MakeReturn0Cci (pcci);
}

static void combo_changed_handler(GtkWidget *entry, gpointer user_data)
{
	int newsel = 0;
	GtkWidget *combo = (GtkWidget *) user_data;
	GList *child = GTK_LIST(GTK_COMBO(combo)->list)->children;
	while (child)
	{
		GtkWidget *item = (GtkWidget *) child->data;
		if (item->state == GTK_STATE_SELECTED)
		{
			SendMessage3ToClean (CcWmITEMSELECT, (int) GetControlParent(combo), (int) combo, newsel);
			return;
		}

		child = child->next;
		newsel++;
	}
};

void EvalCcRqCREATEPOPUP (CrossCallInfo *pcci)	/* hwnd, x,y,w,h,isEditable;  HWND hwndPopUp,hwndEdit (if isEditable). */
{
	GtkWidget *combo;
	GtkWidget *parent;
	int x, y, w, h;
	gboolean isEditable;

	parent	= (GtkWidget *) pcci->p1;
	x	= pcci->p2;
	y	= pcci->p3;
	w	= pcci->p4;
	h	= pcci->p5;
	isEditable = (gboolean) pcci->p6;

	combo = gtk_combo_new();
	gtk_combo_set_use_arrows_always(GTK_COMBO(combo), gtk_true());
	gtk_widget_set_usize(combo, w, h);

	gtk_fixed_put (GetFixed(parent), combo, x, y);

	gtk_signal_connect(GTK_OBJECT(GTK_COMBO(combo)->entry), "changed",
			GTK_SIGNAL_FUNC(combo_changed_handler),
			combo);

	if (isEditable)
	{
		gtk_signal_connect (GTK_OBJECT (GTK_COMBO(combo)->entry), "focus-in-event",
				GTK_SIGNAL_FUNC(widget_focus_in_handler),
				combo);

		gtk_signal_connect (GTK_OBJECT (GTK_COMBO(combo)->entry), "focus-out-event",
				GTK_SIGNAL_FUNC(widget_focus_out_handler),
				combo);

		gtk_signal_connect (GTK_OBJECT (GTK_COMBO(combo)->entry), "key-press-event",
				GTK_SIGNAL_FUNC(widget_key_press_handler),
				combo);

		gtk_signal_connect (GTK_OBJECT (GTK_COMBO(combo)->entry), "key-release-event",
				GTK_SIGNAL_FUNC(widget_key_release_handler),
				combo);
	}
	else
	{
		gtk_entry_set_editable(GTK_ENTRY(GTK_COMBO(combo)->entry), gtk_false());
	}

	MakeReturn2Cci (pcci, (int) combo, (int) GTK_COMBO(combo)->entry);
}

void EvalCcRqADDTOPOPUP (CrossCallInfo *pcci)	/* hwnd, textptr, enabled, selected, index; Pos result. */
{
	int pos;
	GtkWidget *combo, *li;
	gchar *text;
	BOOL selected;

	combo    = (GtkWidget *) pcci->p1;
	text     = (char *) pcci->p2;
	selected = (gboolean) pcci->p3;

	li = gtk_list_item_new_with_label(text);
	gtk_widget_show (li);
	gtk_container_add(GTK_CONTAINER(GTK_COMBO(combo)->list), li);

	pos = gtk_list_child_position(GTK_LIST(GTK_COMBO(combo)->list), li);

	if (selected)
	 	  gtk_list_select_item(GTK_LIST(GTK_COMBO(combo)->list), pos);

	MakeReturn1Cci (pcci, pos);
}

void EvalCcRqSELECTPOPUPITEM (CrossCallInfo *pcci)		/* hwnd, pos; no result */
{
	GtkWidget *combo;
	int pos;

	combo = (GtkWidget *) pcci->p1;
	pos = pcci->p2;

	gtk_list_select_item(GTK_LIST(GTK_COMBO(combo)->list), pos);

	MakeReturn0Cci (pcci);
}

void EvalCcRqRESTACKWINDOW (CrossCallInfo *pcci)		/* thewindow,behind; no result. */
{
/*	HWND thePtr, behindPtr;
	UINT uflags = SWP_NOMOVE + SWP_NOSIZE;	//	Do not change current size or location

	thePtr    = (HWND) pcci->p1;
	behindPtr = (HWND) pcci->p2;

	SetWindowPos (thePtr, behindPtr, 0, 0, 0, 0, uflags);
*/
	printf("EvalCcRqRESTACKWINDOW -> not implemented\n");
	MakeReturn0Cci (pcci);
}

/*	Add controls to tooltip area. */
void EvalCcRqADDCONTROLTIP (CrossCallInfo *pcci) /* parentPtr, controlPtr, textPtr; no result. */
{
	GtkWidget *parent  = (GtkWidget *) pcci->p1;
	GtkWidget *control = (GtkWidget *) pcci->p2;
	gchar *text = (gchar *)pcci->p3;

	gtk_tooltips_set_tip(GTK_TOOLTIPS(gTooltip), control, text, text);

	MakeReturn0Cci (pcci);
}

/*	Remove controls from tooltip area. */
void EvalCcRqDELCONTROLTIP (CrossCallInfo *pcci) /* parentPtr, controlPtr; no result. */
{
	GtkWidget *parent  = (GtkWidget *) pcci->p1;
	GtkWidget *control = (GtkWidget *) pcci->p2;

	gtk_tooltips_set_tip(GTK_TOOLTIPS(gTooltip), control, NULL, NULL);

	MakeReturn0Cci (pcci);
}

void EvalCcRqCREATECARET(CrossCallInfo *pcci)
{
/*
	HWND hWnd = (HWND) pcci->p1;
	int nWidth = max(max(GetSystemMetrics(SM_CYBORDER), GetSystemMetrics(SM_CXBORDER)) * 2, pcci->p2);
	int nHeight = pcci->p3;

	ghCaretWnd = hWnd;
	CreateCaret(hWnd, NULL, nWidth, nHeight);
	ShowCaret(hWnd);
*/
	printf("EvalCcRqCREATECARET -> not implemented\n");
	MakeReturn0Cci (pcci);
}

void EvalCcRqSETCARETPOS(CrossCallInfo *pcci)
{
//	if (ghCaretWnd == (HWND) pcci->p1)
//	{
//		SetCaretPos(pcci->p2, pcci->p3);
//	};
	printf("EvalCcRqSETCARETPOS -> not implemented\n");
	MakeReturn0Cci (pcci);
}

void EvalCcRqDESTROYCARET(CrossCallInfo *pcci)
{
//	HWND hWnd = (HWND) pcci->p1;
//
///	HideCaret(hWnd);
//	DestroyCaret();
//	ghCaretWnd = NULL;
	printf("EvalCcRqDESTROYCARET -> not implemented\n");
	MakeReturn0Cci (pcci);
}

void EvalCcRqSHOWCARET(CrossCallInfo *pcci)
{
//	ShowCaret((HWND) pcci->p1);
	printf("EvalCcRqSHOWCARET -> not implemented\n");
	MakeReturn0Cci (pcci);
}

void EvalCcRqHIDECARET(CrossCallInfo *pcci)
{
//	HideCaret((HWND) pcci->p1);
	printf("EvalCcRqHIDECARET -> not implemented\n");
	MakeReturn0Cci (pcci);
}

/*	Install the cross call procedures in the gCrossCallProcedureTable of cCrossCall_121.
*/
void InstallCrossCallWindows ()
{
	CrossCallProcedureTable newTable;

	newTable = EmptyCrossCallProcedureTable ();
	AddCrossCallEntry (newTable, CcRqBEGINPAINT,             EvalCcRqBEGINPAINT);
	AddCrossCallEntry (newTable, CcRqENDPAINT,               EvalCcRqENDPAINT);
	AddCrossCallEntry (newTable, CcRqFAKEPAINT,              EvalCcRqFAKEPAINT);
	AddCrossCallEntry (newTable, CcRqDESTROYMODALDIALOG,     EvalCcRqDESTROYMODALDIALOG);
	AddCrossCallEntry (newTable, CcRqDESTROYMDIDOCWINDOW,    EvalCcRqDESTROYMDIDOCWINDOW);
	AddCrossCallEntry (newTable, CcRqCREATESDIDOCWINDOW,     EvalCcRqCREATESDIDOCWINDOW);
	AddCrossCallEntry (newTable, CcRqCREATEMDIDOCWINDOW,     EvalCcRqCREATEMDIDOCWINDOW);
	AddCrossCallEntry (newTable, CcRqSETWINDOWTITLE,         EvalCcRqSETWINDOWTITLE);
	AddCrossCallEntry (newTable, CcRqGETWINDOWTEXT,          EvalCcRqGETWINDOWTEXT);
	AddCrossCallEntry (newTable, CcRqUPDATEWINDOWRECT,       EvalCcRqUPDATEWINDOWRECT);
	AddCrossCallEntry (newTable, CcRqSETCLIENTSIZE,          EvalCcRqSETCLIENTSIZE);
	AddCrossCallEntry (newTable, CcRqSETSELECTWINDOW,        EvalCcRqSETSELECTWINDOW);
	AddCrossCallEntry (newTable, CcRqSETWINDOWPOS,           EvalCcRqSETWINDOWPOS);
	AddCrossCallEntry (newTable, CcRqGETWINDOWSIZE,          EvalCcRqGETWINDOWSIZE);
	AddCrossCallEntry (newTable, CcRqSETWINDOWSIZE,          EvalCcRqSETWINDOWSIZE);
	AddCrossCallEntry (newTable, CcRqACTIVATECONTROL,        EvalCcRqACTIVATECONTROL);
	AddCrossCallEntry (newTable, CcRqACTIVATEWINDOW,         EvalCcRqACTIVATEWINDOW);
	AddCrossCallEntry (newTable, CcRqCHANGEWINDOWCURSOR,     EvalCcRqCHANGEWINDOWCURSOR);
	AddCrossCallEntry (newTable, CcRqOBSCURECURSOR,          EvalCcRqOBSCURECURSOR);
	AddCrossCallEntry (newTable, CcRqSETSCROLLRANGE,         EvalCcRqSETSCROLLRANGE);
	AddCrossCallEntry (newTable, CcRqSETSCROLLPOS,           EvalCcRqSETSCROLLPOS);
	AddCrossCallEntry (newTable, CcRqSETSCROLLSIZE,          EvalCcRqSETSCROLLSIZE);
	AddCrossCallEntry (newTable, CcRqSETEDITSELECTION,       EvalCcRqSETEDITSELECTION);
	AddCrossCallEntry (newTable, CcRqCREATEDIALOG,           EvalCcRqCREATEDIALOG);
	AddCrossCallEntry (newTable, CcRqCREATEMODALDIALOG,      EvalCcRqCREATEMODALDIALOG);
	AddCrossCallEntry (newTable, CcRqCREATECOMPOUND,         EvalCcRqCREATECOMPOUND);
	AddCrossCallEntry (newTable, CcRqCREATESCROLLBAR,        EvalCcRqCREATESCROLLBAR);
	AddCrossCallEntry (newTable, CcRqCREATEBUTTON,           EvalCcRqCREATEBUTTON);
	AddCrossCallEntry (newTable, CcRqCREATEICONBUT,          EvalCcRqCREATEICONBUT);
	AddCrossCallEntry (newTable, CcRqCREATECUSTOM,           EvalCcRqCREATECUSTOM);
	AddCrossCallEntry (newTable, CcRqCREATESTATICTXT,        EvalCcRqCREATESTATICTXT);
	AddCrossCallEntry (newTable, CcRqCREATEEDITTXT,          EvalCcRqCREATEEDITTXT);
	AddCrossCallEntry (newTable, CcRqCREATERADIOBUT,         EvalCcRqCREATERADIOBUT);
	AddCrossCallEntry (newTable, CcRqCREATECHECKBOX,         EvalCcRqCREATECHECKBOX);
	AddCrossCallEntry (newTable, CcRqSETITEMCHECK,           EvalCcRqSETITEMCHECK);
	AddCrossCallEntry (newTable, CcRqENABLECONTROL,          EvalCcRqENABLECONTROL);
	AddCrossCallEntry (newTable, CcRqSHOWCONTROL,            EvalCcRqSHOWCONTROL);
	AddCrossCallEntry (newTable, CcRqSHOWWINDOW,             EvalCcRqSHOWWINDOW);
	AddCrossCallEntry (newTable, CcRqCREATEPOPUP,            EvalCcRqCREATEPOPUP);
	AddCrossCallEntry (newTable, CcRqADDTOPOPUP,             EvalCcRqADDTOPOPUP);
	AddCrossCallEntry (newTable, CcRqSELECTPOPUPITEM,        EvalCcRqSELECTPOPUPITEM);
	AddCrossCallEntry (newTable, CcRqRESTACKWINDOW,          EvalCcRqRESTACKWINDOW);
	AddCrossCallEntry (newTable, CcRqADDCONTROLTIP,          EvalCcRqADDCONTROLTIP);
	AddCrossCallEntry (newTable, CcRqDELCONTROLTIP,          EvalCcRqDELCONTROLTIP);
	AddCrossCallEntry (newTable, CcRqCREATECARET,			 EvalCcRqCREATECARET);
	AddCrossCallEntry (newTable, CcRqSETCARETPOS,			 EvalCcRqSETCARETPOS);
	AddCrossCallEntry (newTable, CcRqDESTROYCARET, 			 EvalCcRqDESTROYCARET);
	AddCrossCallEntry (newTable, CcRqHIDECARET, 			 EvalCcRqHIDECARET);
	AddCrossCallEntry (newTable, CcRqSHOWCARET, 			 EvalCcRqSHOWCARET);
	AddCrossCallEntries (gCrossCallProcedureTable, newTable);
}
