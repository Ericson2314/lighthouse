/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Routines related to standard file selector dialogues.
********************************************************************************************/
#include "cCrossCallFileSelectors_121.h"
#include "util_121.h"
#include "cCrossCall_121.h"

extern GtkWidget *gActiveTopLevelWindow;

void EvalCcRqDIRECTORYDIALOG (CrossCallInfo *pcci)		/* no params;  bool, textptr result; */
{
	GtkWidget *file_selector = gtk_file_selection_new("Select directory");

	if (gActiveTopLevelWindow)
	{
		gtk_window_set_transient_for(GTK_WINDOW(file_selector), GTK_WINDOW(gActiveTopLevelWindow));
	}

	for (;;)
	{
		if (gtk_dialog_run(GTK_DIALOG(file_selector)) == GTK_RESPONSE_OK)
		{
			gchar *file_name;
			G_CONST_RETURN gchar *fname = gtk_file_selection_get_filename(GTK_FILE_SELECTION(file_selector));
			if (!g_file_test(fname, G_FILE_TEST_EXISTS | G_FILE_TEST_IS_DIR))
			{
				GtkWidget *dialog =
						gtk_message_dialog_new (GTK_WINDOW(file_selector),
												GTK_DIALOG_DESTROY_WITH_PARENT,
												GTK_MESSAGE_ERROR,
												GTK_BUTTONS_OK,
												"%s directory not found",
												fname);
				 gtk_dialog_run (GTK_DIALOG (dialog));
				 gtk_widget_destroy (dialog);
				 continue;
			}

			file_name = g_strdup(fname);
			gtk_widget_destroy(file_selector);
			MakeReturn2Cci(pcci, gtk_true(), (int) file_name);
			return;
		}
		else
		{
			gtk_widget_destroy(file_selector);
			MakeReturn2Cci(pcci, gtk_false(), (int) NULL);
			return;
		}
	}
}

void EvalCcRqFILEOPENDIALOG (CrossCallInfo *pcci)		/* no params;  bool, textptr result; */
{
	GtkWidget *file_selector = gtk_file_selection_new("Open");

	if (gActiveTopLevelWindow)
	{
		gtk_widget_set_parent(file_selector, gActiveTopLevelWindow);
		gtk_window_set_transient_for(GTK_WINDOW(file_selector), GTK_WINDOW(gActiveTopLevelWindow));
	}

	for (;;)
	{
		if (gtk_dialog_run(GTK_DIALOG(file_selector)) == GTK_RESPONSE_OK)
		{
			gchar *file_name;
			G_CONST_RETURN gchar *fname = gtk_file_selection_get_filename(GTK_FILE_SELECTION(file_selector));
			if (!g_file_test(fname, G_FILE_TEST_EXISTS))
			{
				GtkWidget *dialog =
						gtk_message_dialog_new (GTK_WINDOW(file_selector),
												GTK_DIALOG_DESTROY_WITH_PARENT,
												GTK_MESSAGE_ERROR,
												GTK_BUTTONS_OK,
												"%s file not found",
												fname);
				 gtk_dialog_run (GTK_DIALOG (dialog));
				 gtk_widget_destroy (dialog);
				 continue;
			}

			file_name = g_strdup(fname);
			gtk_widget_destroy(file_selector);
			MakeReturn2Cci(pcci, gtk_true(), (int) file_name);
			return;
		}
		else
		{
			gtk_widget_destroy(file_selector);
			MakeReturn2Cci(pcci, gtk_false(), (int) NULL);
			return;
		}
	}
}

void EvalCcRqFILESAVEDIALOG (CrossCallInfo *pcci)		/* promptptr, nameptr; bool, textptr result; */
{
	GtkWidget *file_selector = gtk_file_selection_new((gchar *) pcci->p1);

	if (gActiveTopLevelWindow)
	{
		gtk_widget_set_parent(file_selector, gActiveTopLevelWindow);
		gtk_window_set_transient_for(GTK_WINDOW(file_selector), GTK_WINDOW(gActiveTopLevelWindow));
	}

	gtk_file_selection_set_filename(file_selector, (gchar *) pcci->p2);

	for (;;)
	{
		if (gtk_dialog_run(GTK_DIALOG(file_selector)) == GTK_RESPONSE_OK)
		{
			gchar *file_name;
			G_CONST_RETURN gchar *fname = gtk_file_selection_get_filename(GTK_FILE_SELECTION(file_selector));
			if (g_file_test(fname, G_FILE_TEST_EXISTS))
			{
				gint res;
				GtkWidget *dialog =
						gtk_message_dialog_new (GTK_WINDOW(file_selector),
												GTK_DIALOG_DESTROY_WITH_PARENT,
												GTK_MESSAGE_WARNING,
												GTK_BUTTONS_YES_NO,
												"%s already exists. Do you want to replace id?",
												fname);
				 res = gtk_dialog_run (GTK_DIALOG (dialog));
				 gtk_widget_destroy (dialog);

				 if (res == GTK_RESPONSE_NO) continue;
			}

			file_name = g_strdup(fname);
			gtk_widget_destroy(file_selector);
			MakeReturn2Cci(pcci, gtk_true(), (int) file_name);
			return;
		}
		else
		{
			gtk_widget_destroy(file_selector);
			MakeReturn2Cci(pcci, gtk_false(), (int) NULL);
			return;
		}
	}
}


/*	Install the cross call procedures in the gCrossCallProcedureTable of cCrossCall_121.
*/
void InstallCrossCallFileSelectors()
{
	CrossCallProcedureTable newTable;

	newTable = EmptyCrossCallProcedureTable ();
	AddCrossCallEntry (newTable, CcRqDIRECTORYDIALOG,EvalCcRqDIRECTORYDIALOG);
	AddCrossCallEntry (newTable, CcRqFILEOPENDIALOG, EvalCcRqFILEOPENDIALOG);
	AddCrossCallEntry (newTable, CcRqFILESAVEDIALOG, EvalCcRqFILESAVEDIALOG);
	AddCrossCallEntries (gCrossCallProcedureTable, newTable);
}
