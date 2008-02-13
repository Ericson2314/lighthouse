/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	cCrossCall_121 defines the infrastructure required by the Object I/O library to call
	system procedures that interact with the Windows callback mechanism.

	The basic principle in cCrossCall_121 is to have a minimal cross call kernel. If Clean
	code requires extension of the functionality of the OS thread, then this functionality
	must be	registered before being applicable.

	In this version the request codes are still statically fixed and are assumed to be
	globally available both in the OS thread and the Clean thread. In a future version this
	will probably be replaced by a dynamic allocation of cross call request codes.
********************************************************************************************/


/********************************************************************************************
	Include section.
********************************************************************************************/

#include "cCrossCall_121.h"
#include "cCrossCallWindows_121.h"		// Contains the implementation of cursors.
#include <gdk/gdkkeysyms.h>
#include <pthread.h>
#include <Rts.h>
#include <RtsAPI.h>

/**********************************************************************************************
	External global data section.
**********************************************************************************************/
CrossCallInfo gCci;									/* The global cross call information struct. */
GtkWidget *gTooltip              = NULL;			/* The tooltip control. */
CrossCallProcedureTable gCrossCallProcedureTable;

/**********************************************************************************************
	Internal global data section.
**********************************************************************************************/

static pthread_mutex_t gHaskellMutex;
static pthread_mutex_t gOSMutex;
static pthread_t gOSThread;
static gboolean gOSThreadIsRunning = FALSE;


static CrossCallInfo *MakeQuitCci (CrossCallInfo * pcci);


/*	GetModifiers returns the modifiers that are currently pressed.
*/
int GetModifiers (void)
{
	int mods = 0;
	GdkModifierType state;

	gdk_event_get_state(gtk_get_current_event(), &state);

	if (state & GDK_SHIFT_MASK)
		mods |= SHIFTBIT;
	if (state & GDK_CONTROL_MASK)
		mods |= CTRLBIT;
	if (state & GDK_MOD1_MASK)
		mods |= ALTBIT;

	return mods;
}


/*	Translate virtual key codes to the codes shared with Clean.
	This procedure has been filtered from TranslateKeyboardMessage.
	If the keycode could not be translated, zero is returned.
*/
int CheckVirtualKeyCode (int keycode)
{
	int c = 0;
	switch (keycode)
	{
		case GDK_Up:
			c = WinUpKey;
			break;
		case GDK_Down:
			c = WinDownKey;
			break;
		case GDK_Left:
			c = WinLeftKey;
			break;
		case GDK_Right:
			c = WinRightKey;
			break;
		case GDK_Page_Up:
			c = WinPgUpKey;
			break;
		case GDK_Page_Down:
			c = WinPgDownKey;
			break;
		case GDK_End:
			c = WinEndKey;
			break;
		case GDK_Begin:
			c = WinBeginKey;
			break;
		case GDK_BackSpace:
			c = WinBackSpKey;
			break;
		case GDK_Delete:
			c = WinDelKey;
			break;
		case GDK_Tab:
			c = WinTabKey;
			break;
		case GDK_Return:
			c = WinReturnKey;
			break;
		case GDK_Escape:
			c = WinEscapeKey;
			break;
		case GDK_Help:
			c = WinHelpKey;
			break;
		case GDK_F1:
			c = WinF1Key;
			break;
		case GDK_F2:
			c = WinF2Key;
			break;
		case GDK_F3:
			c = WinF3Key;
			break;
		case GDK_F4:
			c = WinF4Key;
			break;
		case GDK_F5:
			c = WinF5Key;
			break;
		case GDK_F6:
			c = WinF6Key;
			break;
		case GDK_F7:
			c = WinF7Key;
			break;
		case GDK_F8:
			c = WinF8Key;
			break;
		case GDK_F9:
			c = WinF9Key;
			break;
		case GDK_F10:
			c = WinF10Key;
			break;
		case GDK_F11:
			c = WinF11Key;
			break;
		case GDK_F12:
			c = WinF12Key;
			break;
	}
	return c;
}

static void TimerCallback (gpointer data)
{
	SendMessage0ToClean (CcWmIDLETIMER);
};

void HandleCleanRequest (CrossCallInfo * pcci)
{
	switch (pcci->mess)
	{
		case CcRqDOMESSAGE: 	// idleTimerOn, sleeptime; no result.
			{
				gboolean gIdleTimerOn = (gboolean) pcci->p1;
				gint interval = (gint) pcci->p2;

				if (gIdleTimerOn)
				{
					GSource *source = g_timeout_source_new(interval);
					g_source_set_callback(source,TimerCallback,NULL,NULL);
					g_source_attach(source,NULL);

					gtk_main_iteration();

					g_source_destroy(source);
				}
				else
				{
					gtk_main_iteration();
				}

				MakeReturn0Cci (pcci);
			}
			break;
		default:
			{
				CrossCallProcedure action;

				action = FindCrossCallEntry (gCrossCallProcedureTable, pcci->mess);

				if (action == NULL)
				{	// Cross call request code not installed.
					printf("\'HandleCleanRequest\' got uninstalled CcRq request code from Haskell: %d\n", pcci->mess);
					exit(1);
				}
				else
				{	// Cross call request code found. Apply it to pcci.
					action (pcci);
				}
			}
	}
	KickCleanThread (pcci);
}	/* HandleCleanRequest */

void InitGTK()
{
	static gboolean gInitiated = FALSE;

	if (!gInitiated)
	{
		int margc;
		char **margv;

		gtk_set_locale();
		getProgArgv(&margc, &margv);
		gtk_init(&margc,&margv);
		setProgArgv(margc, margv);
	};

	gInitiated = TRUE;
}	/* InitGTK */

static gpointer OsThreadFunction (gpointer param);

void WinStartOsThread()
{
	pthread_attr_t attr;

	InitGTK();

	//	The cross call procedure table is set to the empty table.
	gCrossCallProcedureTable = EmptyCrossCallProcedureTable ();

	pthread_mutex_init(&gHaskellMutex,NULL);
	pthread_mutex_lock(&gHaskellMutex);
	pthread_mutex_init(&gOSMutex,NULL);
	pthread_mutex_lock(&gOSMutex);
	gOSThreadIsRunning = TRUE;

	pthread_attr_init(&attr);
	pthread_create(&gOSThread,&attr,OsThreadFunction,NULL);
	pthread_attr_destroy(&attr);
}	/* WinStartOsThread */

void WinKillOsThread ()
{
	if (gOSThread != NULL)
	{
		gOSThreadIsRunning = FALSE;
		gOSThread = NULL;

		DeleteCursors();

		if (gCrossCallProcedureTable)
			FreeCrossCallProcedureTable (gCrossCallProcedureTable);
	};
}	/*WinKillOsThread*/

void WinKickOsThread (int imess,
					  int ip1, int ip2, int ip3,
					  int ip4, int ip5, int ip6,
					  int *omess,
					  int *op1, int *op2, int *op3,
					  int *op4, int *op5, int *op6
					 )
{
	gCci.mess = imess;
	gCci.p1 = ip1;
	gCci.p2 = ip2;
	gCci.p3 = ip3;
	gCci.p4 = ip4;
	gCci.p5 = ip5;
	gCci.p6 = ip6;

	if (gOSThread != NULL)
	{
		pthread_mutex_unlock(&gHaskellMutex);
		pthread_mutex_lock(&gOSMutex);

		*omess = gCci.mess;
		*op1 = gCci.p1;
		*op2 = gCci.p2;
		*op3 = gCci.p3;
		*op4 = gCci.p4;
		*op5 = gCci.p5;
		*op6 = gCci.p6;
	}
	else
	{
		*omess = CcWASQUIT;
		*op1 = 0;
		*op2 = 0;
		*op3 = 0;
		*op4 = 0;
		*op5 = 0;
		*op6 = 0;
	}
}	/* WinKickOsThread */

void KickCleanThread (CrossCallInfo * pcci)
{
	if (pcci != &gCci)
		gCci = *pcci;

	pthread_mutex_unlock(&gOSMutex);
	pthread_mutex_lock(&gHaskellMutex);

	if (pcci != &gCci)
	    *pcci = gCci;
}	/* KickCleanThread */

void SendMessageToClean (int mess, int p1, int p2, int p3, int p4, int p5, int p6)
{
	gCci.mess = mess;
	gCci.p1 = p1;
	gCci.p2 = p2;
	gCci.p3 = p3;
	gCci.p4 = p4;
	gCci.p5 = p5;
	gCci.p6 = p6;

	KickCleanThread (&gCci);
	while (!IsReturnCci (&gCci))
	{
		HandleCleanRequest (&gCci);
	}
}

CrossCallInfo *MakeReturn0Cci (CrossCallInfo * pcci)
{
	pcci->mess = CcRETURN0;
	return pcci;
}

CrossCallInfo *MakeReturn1Cci (CrossCallInfo * pcci, int v1)
{
	pcci->mess = CcRETURN1;
	pcci->p1 = v1;
	return pcci;
}

CrossCallInfo *MakeReturn2Cci (CrossCallInfo * pcci, int v1, int v2)
{
	pcci->mess = CcRETURN2;
	pcci->p1 = v1;
	pcci->p2 = v2;
	return pcci;
}

CrossCallInfo *MakeReturn3Cci (CrossCallInfo * pcci, int v1, int v2, int v3)
{
	pcci->mess = CcRETURN3;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	return pcci;
}

CrossCallInfo *MakeReturn4Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4)
{
	pcci->mess = CcRETURN4;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	pcci->p4 = v4;
	return pcci;
}

CrossCallInfo *MakeReturn5Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4, int v5)
{
	pcci->mess = CcRETURN5;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	pcci->p4 = v4;
	pcci->p5 = v5;
	return pcci;
}

CrossCallInfo *MakeReturn6Cci (CrossCallInfo * pcci, int v1, int v2, int v3, int v4, int v5, int v6)
{
	pcci->mess = CcRETURN6;
	pcci->p1 = v1;
	pcci->p2 = v2;
	pcci->p3 = v3;
	pcci->p4 = v4;
	pcci->p5 = v5;
	pcci->p6 = v6;
	return pcci;
}

gboolean IsReturnCci (CrossCallInfo * pcci)
{
	if (pcci->mess >= CcRETURNmin && pcci->mess <= CcRETURNmax)
		return TRUE;

	return FALSE;
}


static gpointer OsThreadFunction (gpointer param)
{
	gTooltip = gtk_tooltips_new();

	pthread_mutex_lock(&gHaskellMutex);

	while (gOSThreadIsRunning)
	{
	    HandleCleanRequest (&gCci);
	}

	pthread_mutex_unlock(&gHaskellMutex);

	pthread_mutex_destroy(&gOSMutex);
	pthread_mutex_destroy(&gHaskellMutex);

	return NULL;
}	/* OsThreadFunction */
