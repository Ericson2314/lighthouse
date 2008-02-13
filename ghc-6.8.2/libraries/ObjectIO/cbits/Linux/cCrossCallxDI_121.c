/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	This module contains the cross call implementations required for
	NDI, SDI, and MDI document interfaces.
********************************************************************************************/
#include "util_121.h"
#include <gdk/gdkkeysyms.h>
#include "cCrossCallxDI_121.h"
#include "cCrossCall_121.h"
#include "cCCallWindows_121.h"


/*	Global data with external references:
*/
GtkWidget *gActiveTopLevelWindow = NULL;
gboolean gInMouseDown = FALSE;
gboolean gInKey       = FALSE;
int gCurChar;


/*	GetSDIClientWindow finds the first SDI client window of the argument hwnd.
		This procedure assumes that hwnd is the handle of a SDI frame window.
		If no SDI client window could be found then GetSDIClientWindow returns NULL.
*/
/*
static HWND GetSDIClientWindow (HWND hwndFrame)
{
	HWND client;
	char *clientclassname;
	int  classnamelength;

	client = GetWindow (hwndFrame,GW_CHILD);
	classnamelength = strlen (SDIWindowClassName) + 1;
	clientclassname = rmalloc (classnamelength);
	GetClassName (client, clientclassname, classnamelength);

	while (client != NULL && strcmp(clientclassname, SDIWindowClassName) != 0)
	{
		client = GetWindow (client,GW_HWNDNEXT);
		GetClassName (client,clientclassname,classnamelength);
	}
	rfree (clientclassname);
	return client;
}
*/

/*	Sending keyboard events to Clean thread:
*/
void SendKeyDownToClean (GtkWidget *parent, GtkWidget *child, int c)
{
	SendMessage5ToClean (CcWmKEYBOARD, parent, child, c, KEYDOWN, GetModifiers ());
}

void SendKeyStillDownToClean (GtkWidget *parent, GtkWidget *child, int c)
{
	SendMessage5ToClean (CcWmKEYBOARD, parent, child, c, KEYREPEAT, GetModifiers ());
}

void SendKeyUpToClean (GtkWidget *parent, GtkWidget *child, int c)
{
	SendMessage5ToClean (CcWmKEYBOARD, parent, child, c, KEYUP, GetModifiers ());
}

static void prcs(GtkWidget *widget, gpointer data)
{
	if (GTK_IS_SCROLLED_WINDOW(widget))
	{
		*((GtkWidget **) data) = widget;
	}
}

static GtkWidget *get_client(GtkWidget *widget)
{
	GtkWidget *box = gtk_bin_get_child(GTK_BIN(widget));
	if (box)
	{
		GtkWidget *client = NULL;
		gtk_container_foreach(GTK_CONTAINER(box), prcs, (gpointer) &client);
		return client;
	}

	return NULL;
};

static void frame_focus_in_handler(GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
	SendMessage1ToClean (CcWmACTIVATE, get_client(widget));
	GTK_WIDGET_GET_CLASS(widget)->focus_in_event(widget, event);
	gActiveTopLevelWindow = widget;
}

static void frame_focus_out_handler(GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
	GtkWidget *client = get_client(widget);

	if (gInKey)
		SendKeyUpToClean (client, client, gCurChar);

	SendMessage1ToClean (CcWmDEACTIVATE, client);
	GTK_WIDGET_GET_CLASS(widget)->focus_out_event(widget, event);
	gActiveTopLevelWindow = NULL;
}


static gboolean frame_delete_handler(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
	if (gActiveTopLevelWindow == widget)
		gActiveTopLevelWindow = NULL;

	if (gtk_object_get_data(GTK_OBJECT (widget), "gtk-drag-dest") != NULL)
		gtk_drag_dest_unset(widget);

	SendMessage1ToClean (CcWmPROCESSCLOSE, widget);
	return gtk_true();
}

static void frame_drag_data_handler
			(GtkWidget 			*widget,
			 GdkDragContext     *context,
			 gint                x,
			 gint                y,
			 GtkSelectionData   *data,
			 guint               info,
			 guint               time)
{
  	if ((data->length >= 0) && (data->format == 8))
    {
		char *filenames = malloc(data->length);
		if (filenames)
		{
			guchar *s = data->data;
			guchar *e = s + data->length - 2;
			char *d = filenames;

			while (s < e)
			{
				if (*s != '\r') *(d++) = *s;
				s++;
			}
			*d = 0;

			gtk_drag_finish (context, TRUE, FALSE, time);
			SendMessage2ToClean (CcWmPROCESSDROPFILES, (int) widget, (int) filenames);
		}
    }
	else
  		gtk_drag_finish (context, FALSE, FALSE, time);
}

static gboolean frame_key_press_handler(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
	GtkWidget *client = get_client(widget);
	int c = (event->length > 0) ? event->string[0] : CheckVirtualKeyCode (event->keyval);
	if (!c) return gtk_false();

	if (event->keyval == GDK_Tab)
		return gtk_false();

	GTK_WIDGET_GET_CLASS(widget)->key_press_event(widget, event);

	if (gInKey)
	{
		if (gCurChar == c)
			SendKeyStillDownToClean (client, client, gCurChar);
		else
		{
			SendKeyUpToClean (client, client, gCurChar);
			gCurChar = c;
			SendKeyDownToClean (client, client, gCurChar);
		}
	}
	else
	{
		gCurChar = c;
		SendKeyDownToClean (client, client, gCurChar);
		gInKey = TRUE;
	}

	return gtk_true();
};

static gboolean frame_key_release_handler(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
	GtkWidget *client = get_client(widget);

	if (event->keyval == GDK_Tab)
		return gtk_false();

	GTK_WIDGET_GET_CLASS(widget)->key_press_event(widget, event);

	if (gInKey)
	{
		SendKeyUpToClean (client, client, gCurChar);
		gInKey = FALSE;
		gCurChar = 0;
	}

	return gtk_true();
};

/*	Create a SDI frame window. */
void EvalCcRqCREATESDIFRAMEWINDOW (CrossCallInfo *pcci)	/* accept file open; frame ptr, menubar results. */
{
	GtkWidget *window, *menuBar, *box;

	/* Create the menubar. */
	menuBar = gtk_menu_bar_new();

	/* Create the window. */
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_signal_connect (GTK_OBJECT (window), "focus-in-event",
				GTK_SIGNAL_FUNC(frame_focus_in_handler),
				NULL);
	gtk_signal_connect (GTK_OBJECT (window), "focus-out-event",
				GTK_SIGNAL_FUNC(frame_focus_out_handler),
				NULL);
	gtk_signal_connect (GTK_OBJECT (window), "delete-event",
				GTK_SIGNAL_FUNC(frame_delete_handler),
				NULL);
	gtk_signal_connect (GTK_OBJECT(window), "key-press-event",
				GTK_SIGNAL_FUNC(frame_key_press_handler),
				NULL);
	gtk_signal_connect (GTK_OBJECT(window), "key-release-event",
				GTK_SIGNAL_FUNC(frame_key_release_handler),
				NULL);

	if ((gboolean) pcci->p1)	/*	respond to file open events. */
	{
		static GtkTargetEntry target_table = { "text/uri-list", 0, 0 };

		gtk_drag_dest_set (window,
					GTK_DEST_DEFAULT_ALL,
					&target_table, 1, /* no rootwin */
					GDK_ACTION_COPY | GDK_ACTION_MOVE);

		gtk_signal_connect(GTK_OBJECT(window), "drag_data_received",
		      		GTK_SIGNAL_FUNC(frame_drag_data_handler), NULL);
	}

	gtk_window_add_accel_group (GTK_WINDOW (window), gtk_accel_group_new());

	box = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(window), box);
	gtk_box_pack_start(GTK_BOX(box), menuBar, FALSE, FALSE, 0);

	MakeReturn2Cci (pcci, (int) window, (int) menuBar);
}

static void frame_close_page_handler(GtkWidget *client)
{
  GtkWidget *window = gtk_notebook_get_nth_page(GTK_NOTEBOOK(client),  gtk_notebook_get_current_page(GTK_NOTEBOOK(client)));
	SendMessage1ToClean(CcWmCLOSE, window);
}

static void frame_notebook_top_handler(GtkWidget *client)
{
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(client), GTK_POS_TOP);
}

static void frame_notebook_bottom_handler(GtkWidget *client)
{
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(client), GTK_POS_BOTTOM);
}

static void frame_notebook_left_handler(GtkWidget *client)
{
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(client), GTK_POS_LEFT);
}

static void frame_notebook_right_handler(GtkWidget *client)
{
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(client), GTK_POS_RIGHT);
}

static void frame_switch_page_handler(GtkNotebook *notebook,GtkNotebookPage *page,gint page_num,gpointer user_data)
{
  // send deactivate message for old
	gint old_page_num = g_list_index(notebook->children, notebook->cur_page);
	SendMessage1ToClean (CcWmDEACTIVATE, gtk_notebook_get_nth_page(notebook, old_page_num));

	// send activate message for new
	SendMessage1ToClean (CcWmACTIVATE, gtk_notebook_get_nth_page(notebook, page_num));
	gActiveTopLevelWindow = gtk_widget_get_parent(gtk_widget_get_parent(GTK_WIDGET(notebook)));

};

/*	Create MDI frame window. */
void EvalCcRqCREATEMDIFRAMEWINDOW (CrossCallInfo *pcci)	/* show, accept file open; frame ptr, client ptr, menubar, windowmenu results. */
{
	GtkWidget *window, *client, *menuBar, *box;
	GtkWidget *notebook_menu, *menu_item, *pages_menu;
	GtkAccelGroup *accel_group;
	GList *group;

	/* Create the window. */
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_signal_connect (GTK_OBJECT(window), "delete-event",
				GTK_SIGNAL_FUNC(frame_delete_handler),
				NULL);
	gtk_signal_connect (GTK_OBJECT(window), "key-press-event",
				GTK_SIGNAL_FUNC(frame_key_press_handler),
				NULL);
	gtk_signal_connect (GTK_OBJECT(window), "key-release-event",
				GTK_SIGNAL_FUNC(frame_key_release_handler),
				NULL);


	if ((gboolean) pcci->p2)	/*	respond to file open events. */
	{
		static GtkTargetEntry target_table = { "text/uri-list", 0, 0 };

		gtk_drag_dest_set (window,
					GTK_DEST_DEFAULT_ALL,
					&target_table, 1, /* no rootwin */
					GDK_ACTION_COPY | GDK_ACTION_MOVE);

		gtk_signal_connect(GTK_OBJECT(window), "drag_data_received",
		      		GTK_SIGNAL_FUNC(frame_drag_data_handler), NULL);
	}

	/* Create accel_group */
	accel_group = gtk_accel_group_new();
	gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);

	box = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(window), box);

	/* Create the menubar. */
	menuBar = gtk_menu_bar_new();
	gtk_box_pack_start(GTK_BOX(box), menuBar, FALSE, FALSE, 0);

	/* Create client(notebook) */
	client = gtk_notebook_new();
	gtk_notebook_set_scrollable(GTK_NOTEBOOK(client), gtk_true());
	gtk_signal_connect (GTK_OBJECT(client), "switch-page",
					GTK_SIGNAL_FUNC(frame_switch_page_handler),
					NULL);
	gtk_box_pack_end(GTK_BOX(box), client, TRUE, TRUE, 0);

	if ((gboolean) pcci->p1)
		gtk_window_maximize(GTK_WINDOW(window));
	gtk_widget_show_all(window);

	/* Create "Pages" menu */
	pages_menu = gtk_menu_new();
	gtk_menu_set_accel_group(GTK_MENU(pages_menu), accel_group);

	menu_item = gtk_menu_item_new_with_label("Pages");
	gtk_menu_item_set_submenu(GTK_MENU_ITEM (menu_item), pages_menu);
	gtk_widget_show_all(menu_item);

	gtk_menu_bar_insert(GTK_MENU_BAR(menuBar), menu_item, 0);

	notebook_menu = gtk_menu_new();
	gtk_menu_set_accel_group(GTK_MENU(notebook_menu), accel_group);

	menu_item = gtk_radio_menu_item_new_with_label(NULL, "Top");
	gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu_item), gtk_true());
	group = gtk_radio_menu_item_group(GTK_RADIO_MENU_ITEM(menu_item));
	gtk_signal_connect_object (GTK_OBJECT (menu_item), "activate",
		GTK_SIGNAL_FUNC (frame_notebook_top_handler), client);
	gtk_menu_append(GTK_MENU(notebook_menu), menu_item);

	menu_item = gtk_radio_menu_item_new_with_label(group, "Bottom");
	group = gtk_radio_menu_item_group(GTK_RADIO_MENU_ITEM(menu_item));
	gtk_signal_connect_object (GTK_OBJECT (menu_item), "activate",
		GTK_SIGNAL_FUNC (frame_notebook_bottom_handler), client);
	gtk_menu_append(GTK_MENU(notebook_menu), menu_item);

	menu_item = gtk_radio_menu_item_new_with_label(group, "Left");
	group = gtk_radio_menu_item_group(GTK_RADIO_MENU_ITEM(menu_item));
	gtk_signal_connect_object (GTK_OBJECT (menu_item), "activate",
		GTK_SIGNAL_FUNC (frame_notebook_left_handler), client);
	gtk_menu_append(GTK_MENU(notebook_menu), menu_item);

	menu_item = gtk_radio_menu_item_new_with_label(group, "Right");
	group = gtk_radio_menu_item_group(GTK_RADIO_MENU_ITEM(menu_item));
	gtk_signal_connect_object (GTK_OBJECT (menu_item), "activate",
		GTK_SIGNAL_FUNC (frame_notebook_right_handler), client);
	gtk_menu_append(GTK_MENU(notebook_menu), menu_item);

	menu_item = gtk_menu_item_new_with_label("Notebook");
	gtk_menu_item_set_submenu(GTK_MENU_ITEM (menu_item), notebook_menu);
	gtk_menu_append(GTK_MENU(pages_menu), menu_item);

	menu_item = gtk_menu_item_new();
	gtk_menu_append(GTK_MENU(pages_menu), menu_item);

	menu_item = gtk_menu_item_new_with_label("Close page");
	gtk_signal_connect_object (GTK_OBJECT (menu_item), "activate",
		GTK_SIGNAL_FUNC (frame_close_page_handler), client);
	gtk_menu_append(GTK_MENU(pages_menu), menu_item);

	gtk_widget_show_all(pages_menu);

	MakeReturn4Cci (pcci, (int) window, (int) client, (int) menuBar, (int) pages_menu);
}

void EvalCcRqDESTROYWINDOW (CrossCallInfo *pcci) /* hwnd; no result. */
{
	gtk_widget_destroy((GtkWidget *) pcci->p1);
	MakeReturn0Cci (pcci);
}

void EvalCcRqGETWINDOWPOS (CrossCallInfo *pcci)	/* hwnd;   width, heigth result */
{
//	RECT rect;

//	GetWindowRect ((HWND) pcci->p1, &rect);

	printf("EvalCcRqGETWINDOWPOS -> not implemented\n");
	MakeReturn2Cci (pcci, 0 /*rect.left*/, 0 /*rect.top*/);
}

void EvalCcRqGETCLIENTSIZE (CrossCallInfo *pcci) /* hwnd;		width, height result.  */
{
	GtkWidget *frame = (GtkWidget *) pcci->p1;
	GtkWidget *vbox  = GTK_BIN(frame)->child;

	MakeReturn2Cci (pcci, vbox->allocation.width, vbox->allocation.height);
}

static void toolbar_handler(GtkWidget *widget, gpointer data)
{
	GtkWidget *toolbar, *parent;

	toolbar = gtk_widget_get_parent(widget);
	parent  = gtk_widget_get_parent(gtk_widget_get_parent(toolbar));
	SendMessage4ToClean (CcWmBUTTONCLICKED, parent, toolbar, GetModifiers(), (int) data);
};

/*	Create a toolbar in a window. */
void EvalCcRqCREATEMDITOOLBAR (CrossCallInfo *pcci)			/* hwnd, width, height; toolbarptr, full toolbar height result; */
{
	GtkWidget *parent,*box,*toolbar;

	parent  = (GtkWidget *) pcci->p1;

	box = gtk_bin_get_child(GTK_BIN(parent));
	toolbar = gtk_toolbar_new();

	gtk_box_pack_start (GTK_BOX (box), toolbar, FALSE, FALSE, 0);
	gtk_widget_show(toolbar);

	gtk_window_maximize(GTK_WINDOW(parent));

	MakeReturn2Cci (pcci, (int) toolbar, pcci->p3);
}

/*	Create a toolbar in a SDI window. */
void EvalCcRqCREATESDITOOLBAR (CrossCallInfo *pcci)			/* hwnd, width, height; toolbarptr, full toolbar height result; */
{
	GtkWidget *parent,*box,*toolbar;

	parent  = (GtkWidget *) pcci->p1;

	box = gtk_bin_get_child(GTK_BIN(parent));
	toolbar = gtk_toolbar_new();

	gtk_box_pack_start (GTK_BOX (box), toolbar, FALSE, FALSE, 0);
	gtk_widget_show(toolbar);

	MakeReturn2Cci (pcci, (int) toolbar, pcci->p3);
}

/*	Create a bitmap toolbar item. */
void EvalCcRqCREATETOOLBARITEM (CrossCallInfo *pcci)		// hwnd, hbmp, index; no results;
{
	GtkWidget *toolbar;
	GdkPixbuf *pixbuf;
	int index;

	toolbar = (GtkWidget *) pcci->p1;
	pixbuf  = (GdkPixbuf *) pcci->p2;
	index   = pcci->p3;

	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar), NULL, NULL, NULL, gtk_image_new_from_pixbuf(pixbuf), GTK_SIGNAL_FUNC(toolbar_handler), (gpointer) index);

	MakeReturn0Cci (pcci);
}

/*	Create a separator toolbar item. */
void EvalCcRqCREATETOOLBARSEPARATOR (CrossCallInfo *pcci)	// hwnd; no results;
{
	GtkWidget *toolbar;

	toolbar = (GtkWidget *) pcci->p1;
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));

	MakeReturn0Cci (pcci);
}

/*	Install the cross call procedures in the gCrossCallProcedureTable of cCrossCall_121.
*/
void InstallCrossCallxDI ()
{
	CrossCallProcedureTable newTable;

	newTable = EmptyCrossCallProcedureTable ();
	AddCrossCallEntry (newTable, CcRqCREATESDIFRAMEWINDOW,   EvalCcRqCREATESDIFRAMEWINDOW);
	AddCrossCallEntry (newTable, CcRqCREATEMDIFRAMEWINDOW,   EvalCcRqCREATEMDIFRAMEWINDOW);
	AddCrossCallEntry (newTable, CcRqDESTROYWINDOW,          EvalCcRqDESTROYWINDOW);
	AddCrossCallEntry (newTable, CcRqGETWINDOWPOS,           EvalCcRqGETWINDOWPOS);
	AddCrossCallEntry (newTable, CcRqGETCLIENTSIZE,          EvalCcRqGETCLIENTSIZE);
	AddCrossCallEntry (newTable, CcRqCREATEMDITOOLBAR,       EvalCcRqCREATEMDITOOLBAR);
	AddCrossCallEntry (newTable, CcRqCREATESDITOOLBAR,       EvalCcRqCREATESDITOOLBAR);
	AddCrossCallEntry (newTable, CcRqCREATETOOLBARITEM,      EvalCcRqCREATETOOLBARITEM);
	AddCrossCallEntry (newTable, CcRqCREATETOOLBARSEPARATOR, EvalCcRqCREATETOOLBARSEPARATOR);
	AddCrossCallEntries (gCrossCallProcedureTable, newTable);
}
