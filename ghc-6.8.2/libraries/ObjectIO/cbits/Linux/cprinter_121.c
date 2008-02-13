/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library and Clean 0.8 I/O library
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Routines related to printing.
********************************************************************************************/
#if 0
#include "util_121.h"
#include "cpicture_121.h"
#include "cprinter_121.h"

BOOL	bUserAbort;
HWND	hDlgPrint;
HWND	hwndButton,hwndText;
int		semaphor=0;

extern HWND   ghMainWindow;
extern HINSTANCE ghInst;


#define tachtig 80
void getDevmodeSizeC(int *size, HANDLE *phPrinter,
					 char **device, char **driver,  char **output)
{
	char	szPrinter[80];
	char	*szDevice, *szDriver, *szOutput;

	GetProfileString("windows", "device", ",,,", szPrinter, 80);
	szDevice = strtok(szPrinter,",");
	szDriver = strtok(NULL,", ");
	szOutput = strtok(NULL,", ");
	*device	= g_strdup(szDevice);
	*driver	= g_strdup(szDriver);
	*output	= g_strdup(szOutput);
	if (*szDevice=='\0' || *szDriver=='\0' || *szOutput=='\0')
	{
		*size	= 0;
		return;
	};
	OpenPrinter(szDevice,phPrinter,NULL);
	*size = DocumentProperties(NULL,*phPrinter,szDevice,NULL,NULL,0);
}

void getDefaultDevmodeC(char *printSetup, LPHANDLE phPrinter,  char **device)
{
	int		size,r1;

	size		= ((int*)printSetup)[0];
	printSetup	+=4;
	r1 = DocumentProperties(NULL,phPrinter,((char*)device)+4,
							(DEVMODE*)printSetup,NULL,DM_OUT_BUFFER);
	ClosePrinter(phPrinter);
}

static HDC myCreateIC(LPCTSTR driver, LPCTSTR device, DEVMODE *devmode)
{
	HDC	icPrint;

	icPrint	= CreateIC(driver,device,NULL,devmode);
	if (!icPrint)
		icPrint	= CreateIC(driver,device,NULL,devmode);
		// try once again. Adobe printer drivers sometimes need to be told everything twice
	return icPrint;
}

#define GetDeviceCapsWithDefault(icPrint, index, defaullt) (icPrint ? GetDeviceCaps(icPrint, index) : defaullt)

void os_getpagedimensionsC(	DEVMODE *devmode, char *device, char *driver,
							int emulateScreenRes,
							int *maxX, int *maxY,
							int *leftPaper, int *topPaper,
							int *rightPaper, int *bottomPaper,
							int	*xRes, int *yRes
						  )
{
	HDC	icPrint;
	int horPaperPixels, verPaperPixels,
		xResolution,yResolution,
		scNX, scNY, scDX, scDY;


	icPrint	= myCreateIC(driver,device, devmode);

	xResolution = GetDeviceCapsWithDefault(icPrint, LOGPIXELSX, 300);
	yResolution = GetDeviceCapsWithDefault(icPrint, LOGPIXELSY, 300);
	if (emulateScreenRes)						// for emulation of the screen resolution
		{	scNX = WinGetHorzResolution();		// all the deviceCaps will be scaled
			scNY = WinGetVertResolution();
			scDX = xResolution;
			scDY = yResolution;
		}
	  else
		{	scNX = 1; scNY = 1; scDX = 1; scDY = 1;	};

	horPaperPixels	= (GetDeviceCapsWithDefault(icPrint, PHYSICALWIDTH, 2246)*scNX)/scDX;
	verPaperPixels	= (GetDeviceCapsWithDefault(icPrint, PHYSICALHEIGHT, 3250)*scNY)/scDY;

	*maxX			= (GetDeviceCapsWithDefault(icPrint, HORZRES, 2241)*scNX)/scDX;
	*maxY			= (GetDeviceCapsWithDefault(icPrint, VERTRES, 3254)*scNY)/scDY;

    *leftPaper		= (-GetDeviceCapsWithDefault(icPrint, PHYSICALOFFSETX, 116)*scNX)/scDX;
	*topPaper		= (-GetDeviceCapsWithDefault(icPrint, PHYSICALOFFSETY, 129)*scNY)/scDY;
	*rightPaper		= horPaperPixels - *leftPaper;
	*bottomPaper	= verPaperPixels - *topPaper;

	if (emulateScreenRes)
		{	*xRes = scNX;	*yRes = scNY; }
	  else
		{	*xRes = xResolution ;	*yRes = yResolution; };
	DeleteDC(icPrint);
}

static HANDLE setupDevnames(int deviceLength,int driverLength,int outputLength,
							char *device,char *driver,char *output)
{
	HANDLE		hDevnames;
	DEVNAMES	*pDevnames;
	hDevnames	= (HANDLE) LocalAlloc(LMEM_MOVEABLE, 16+deviceLength+driverLength+outputLength);
	pDevnames	= LocalLock(hDevnames);
	pDevnames->wDriverOffset	= 16;
	pDevnames->wDeviceOffset	= 16+driverLength;
	pDevnames->wOutputOffset	= 16+driverLength+deviceLength;
	pDevnames->wDefault			= 0;
	strcpy(((char*)pDevnames)+pDevnames->wDriverOffset, driver);
	strcpy(((char*)pDevnames)+pDevnames->wDeviceOffset, device);
	strcpy(((char*)pDevnames)+pDevnames->wOutputOffset, output);
	LocalUnlock(hDevnames);
	return hDevnames;
}

static HANDLE setupDevmode(int size, char *pData)
{
	HANDLE	hDevmode;
	DEVMODE	*pDevmode;

	hDevmode	= (HANDLE) LocalAlloc(LMEM_MOVEABLE, size);
	pDevmode	= LocalLock(hDevmode);
	memcpy((char*)pDevmode, pData, size);
	LocalUnlock(hDevmode);
	return hDevmode;
}

void get_printSetup_with_PRINTDLG(PRINTDLG *pd, char **o_devmode,
						char **o_device, char **o_driver, char **o_output)
{
	char		*newDriver, *newDevice, *newOutput;
	DEVMODE		*pDevmode;
	DEVNAMES	*pDevnames;

	pDevmode	= LocalLock(pd->hDevMode);
	*o_devmode	= g_strdup(pDevmode->dmDeviceName);
	LocalUnlock(pd->hDevMode);
	pDevnames	= LocalLock(pd->hDevNames);
	newDriver	= ((char*)pDevnames)+(pDevnames->wDriverOffset);
	newDevice	= ((char*)pDevnames)+(pDevnames->wDeviceOffset);
	newOutput	= ((char*)pDevnames)+(pDevnames->wOutputOffset);
	*o_driver	= g_strdup(newDriver);
	*o_device	= g_strdup(newDevice);
	*o_output	= g_strdup(newOutput);
	LocalUnlock(pd->hDevNames);
}

/*	PA: called in Clean. */
int release_memory_handles(PRINTDLG *pd, int os) {
	LocalFree(pd->hDevNames);
	LocalFree(pd->hDevMode);
	return os;
	}

static UINT APIENTRY DialogToFrontHook(HWND hdl, UINT msg, WPARAM wParam, LPARAM lParam)
// This function hooks the Print dialog. It's purpose is to set the dialog in the
// foreground.
{
     if (msg==WM_INITDIALOG)
		{ SetForegroundWindow(hdl);
		};
	 return FALSE;
}

void printSetup(int calledFromCleanThread, int devmodeSize,
			   char *devmode, char *device, char *driver, char *output,
			   int *ok, PRINTDLG **pdPtr)
{
  	int			deviceLength, driverLength, outputLength;
	HANDLE		hDevnames,hDevmode;
	static PRINTDLG	pd;

	// Set up DEVNAMES structure

	//rMessageBox(NULL, MB_APPLMODAL, "in printSetup", "");
	deviceLength	= strlen(device)+1;
	driverLength	= strlen(driver)+1;
	outputLength	= strlen(output)+1;

	hDevnames	= setupDevnames(deviceLength,driverLength,outputLength,device,driver,output);

	// Set up DEVMODE structure
	hDevmode	= setupDevmode(devmodeSize,devmode);

	// Set up print dialog record
	pd.lStructSize = sizeof(PRINTDLG);
	pd.hwndOwner = calledFromCleanThread ? NULL : ghMainWindow;	// (NULL = desktop)
//	pd.hwndOwner = NULL;	// (NULL = desktop)
		// the handle must belong to the active thread, otherwise PrintDlg will crash
		// When this function is called from the Clean thread, ghMainWindow will not
		// belong to the active thread.
	pd.hDevMode = hDevmode;
	pd.hDevNames = hDevnames;
	pd.hDC = NULL;
	pd.Flags =	PD_PRINTSETUP | PD_ENABLESETUPHOOK;
	pd.nFromPage = 1;
	pd.nToPage = 1;
	pd.nMinPage = 1;
	pd.nMaxPage = USHRT_MAX;
	pd.nCopies = 1;
	pd.hInstance = NULL;
	pd.lCustData = 0L;
	pd.lpfnPrintHook = NULL;
	pd.lpfnSetupHook =  DialogToFrontHook;
	pd.lpPrintTemplateName = NULL;
	pd.lpSetupTemplateName = NULL;
	pd.hPrintTemplate = NULL;
	pd.hSetupTemplate = NULL;

	// Open print dialog
	*ok	= PrintDlg(&pd);
	*pdPtr = &pd;

	if (hDevnames!=pd.hDevNames) LocalFree(hDevnames);
	if (hDevmode!=pd.hDevMode) LocalFree(hDevmode);

}


int startPage(int hdc)
{
	//rMessageBox(NULL, MB_APPLMODAL, "in startPage", "");
	return StartPage((HDC) hdc) > 0;
}

int endPage(int hdc)
{
	//rMessageBox(NULL, MB_APPLMODAL, "in endPage", "");
	return EndPage((HDC) hdc) > 0;
}

int startDoc(int hdc)
			// err code: >0:no error, <=0: user cancelled file dialog
{
	static DOCINFO docInfo = { sizeof (DOCINFO), "Clean", NULL, NULL, 0 } ;

	//rMessageBox(NULL, MB_APPLMODAL, "in startDoc", "");
	bUserAbort = FALSE ;

	return StartDoc((HDC) hdc, &docInfo);
}

void endDoc(int hdc)
{
	//rMessageBox(NULL, MB_APPLMODAL, "in endDoc", "");
	if (bUserAbort)
		AbortDoc((HDC) hdc);
	  else
		EndDoc((HDC) hdc);
}

void deleteDC(int hdc)
{
	//rMessageBox(NULL, MB_APPLMODAL, "in deleteDC", "");
	DeleteDC((HDC) hdc);
}

int wasCanceled(void)
{
	//rMessageBox(NULL, MB_APPLMODAL, "in wasCanceled", "");
	return bUserAbort;
}

/* getDC opens the print job dialog and
 * lets the user change various settings or gets the default printer
 */

// c-strings are passed to this function !
void getDC( int doDialog, int emulateScreen, int calledFromCleanThread, int devmodeLength,
			char *devmode,char *device,char *driver,char *output,
			int *err,
			int *first, int *last, int *copies,
			PRINTDLG	**ppPrintDlg,
			int *deviceContext
	 		)
					// err code: -1:no error, others: non fatal error
{
	static PRINTDLG pd;
	HDC hdcPrint;
	int ok;

	*err = -1;

	if (doDialog)
	  {	// Set up print dialog record
		HANDLE	hDevnames, hDevmode;
		int deviceLength,driverLength,outputLength;

		deviceLength	= strlen(device)+1;
		driverLength	= strlen(driver)+1;
		outputLength	= strlen(output)+1;

		hDevnames	= setupDevnames(deviceLength,driverLength,outputLength,
									device,driver,output);
		hDevmode	= setupDevmode(devmodeLength,devmode);

		pd.lStructSize = sizeof(PRINTDLG);
		pd.hwndOwner = calledFromCleanThread ? NULL : ghMainWindow;	// (NULL = desktop)
			// the handle must belong to the active thread, otherwise PrintDlg will crash
			// When this function is called from the Clean thread, ghMainWindow will not
			// belong to the active thread.
		pd.hDevMode = hDevmode;
		pd.hDevNames = hDevnames;
		pd.hDC = NULL;
		pd.Flags = PD_ALLPAGES | PD_COLLATE | PD_RETURNDC | PD_NOSELECTION
				 | PD_ENABLEPRINTHOOK;
		      // hide some options from print dialog
		pd.nFromPage = 1;
		pd.nToPage = 1;
		pd.nMinPage = 1;
		pd.nMaxPage = USHRT_MAX;
		pd.nCopies = 1;
		pd.hInstance = NULL;
		pd.lCustData = 0L;
		pd.lpfnPrintHook = DialogToFrontHook;
		pd.lpfnSetupHook = NULL;
		pd.lpPrintTemplateName = NULL;
		pd.lpSetupTemplateName = NULL;
		pd.hPrintTemplate = NULL;
		pd.hSetupTemplate = NULL;

		// Open print dialog

		ok = PrintDlg(&pd);

		if (hDevnames!=pd.hDevNames) LocalFree(hDevnames);
		if (hDevmode!=pd.hDevMode) LocalFree(hDevmode);

		if (!ok)
			{
			*err = CommDlgExtendedError();	// will return 0 iff user canceled, otherwise positive value
			release_memory_handles(&pd, 0);
			return;
			}


		if (pd.Flags & PD_PAGENUMS)
			{ *first	= pd.nFromPage;
			  *last		= pd.nToPage;
			}
		  else
			{ *first	= 1;
			  *last		= 9999;
			};
		*copies			= pd.nCopies;
		*ppPrintDlg		= &pd;
		hdcPrint		= pd.hDC;
	  }

	else

		{
		hdcPrint = CreateDC(driver, device, output, NULL);
		if (hdcPrint==NULL)
		  { *err = 0;	// non fatal error, iff e.g. no printer driver is installed
	 	    return;
		  };
		*first	= 1;
		*last	= 9999;
		*copies	= 1;
		*ppPrintDlg		= NULL;
		};

	if (emulateScreen)
		{	int pXdpi,pYdpi,sXdpi,sYdpi;
			pXdpi = GetDeviceCaps(hdcPrint, LOGPIXELSX);
			pYdpi = GetDeviceCaps(hdcPrint, LOGPIXELSY);
			sXdpi = WinGetHorzResolution();
			sYdpi = WinGetVertResolution();
			SetMapMode(hdcPrint, MM_ISOTROPIC);
			SetWindowExtEx  (hdcPrint,sXdpi, sYdpi, NULL);
			SetViewportExtEx(hdcPrint,pXdpi, pYdpi, NULL);
		};

	*deviceContext	= (int) hdcPrint;
	//rMessageBox(NULL, MB_APPLMODAL, "leaving getDC","");

}



BOOL CALLBACK PrintDlgProc (HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam)
     {
     switch (msg)
          {
          case WM_INITDIALOG :
               EnableMenuItem (GetSystemMenu (hDlg, FALSE), SC_CLOSE,
                                                            MF_GRAYED) ;
               return TRUE ;
          case WM_COMMAND :
               bUserAbort = TRUE ;
               EnableWindow (ghMainWindow, TRUE) ;
               DestroyWindow (hDlg) ;
               hDlgPrint = 0 ;
               return TRUE ;
          }
     return FALSE;
	 }

BOOL CALLBACK AbortProc (HDC hdcPrn, int iCode)
     {
     MSG   msg ;

     while (!bUserAbort && PeekMessage (&msg, NULL, 0, 0, PM_REMOVE))
          {
          if (!hDlgPrint || !IsDialogMessage (hDlgPrint, &msg))
               {
               TranslateMessage (&msg) ;
               DispatchMessage (&msg) ;
               }
          }
     return !bUserAbort ;
     }


#define DIALOG_WIDTH 100
#define DIALOG_HEIGHT 60
	// in dialog units

HWND CreateCancelDialog(void)
{
	HWND hwndButton,dlgHdl;

	WORD *p, *pdlgtemplate,baseunitX,baseunitY;
	int nchar;
	int scrnWidth,scrnHeight;
	int buttonX, buttonY, buttonWidth, buttonHeight;
	int textX, textY, textWidth, textHeight;
	DWORD lStyle,baseunits;
	HDC screen;
	LOGFONT lf;

	/* allocate some memory to play with  */
	pdlgtemplate = p = (PWORD) rmalloc (1000);

	screen = CreateDC ("DISPLAY", NULL, NULL, NULL);
	scrnWidth  = GetDeviceCaps (screen, HORZRES);
	scrnHeight = GetDeviceCaps (screen, VERTRES);
	DeleteDC (screen);
	baseunits = GetDialogBaseUnits();

	/* start to fill in the dlgtemplate information.  addressing by WORDs */
	lStyle = WS_CAPTION | DS_MODALFRAME | WS_SYSMENU;

	baseunitX=LOWORD(baseunits);
	baseunitY=HIWORD(baseunits);

	*p++ = LOWORD (lStyle);
	*p++ = HIWORD (lStyle);
	*p++ = 0;		/* LOWORD (lExtendedStyle) */
	*p++ = 0;		/* HIWORD (lExtendedStyle) */
	*p++ = 0;		/* NumberOfItems */
	*p++ = ((scrnWidth*4)/3)/baseunitX;		// x
	*p++ = ((scrnHeight*8)/3)/baseunitY;	// y
	*p++ = DIALOG_WIDTH;	/* cx */
	*p++ = DIALOG_HEIGHT;	/* cy */
	*p++ = 0;		/* Menu */
	*p++ = 0;		/* Class */

	/* copy the title of the dialog */
	nchar = NULL; //nCopyAnsiToWideChar (p, (char *) "Printing in Progress");
	p += nchar;

	dlgHdl = CreateDialogIndirectParam (ghInst, (LPDLGTEMPLATE) pdlgtemplate, ghMainWindow,
										(DLGPROC) PrintDlgProc, (LPARAM) 0);

	rfree(pdlgtemplate);

	// Add a text field
	textWidth = 19*baseunitX;
	textHeight = baseunitY;
	textX =    (((DIALOG_WIDTH*baseunitX)/4) - textWidth)
		       / 2;
	textY =    (((DIALOG_HEIGHT*baseunitY)/8) - textHeight)
			   / 4;
	hwndText = CreateWindow ("static", "",WS_VISIBLE | WS_CHILD | SS_CENTER,
									textX, textY, textWidth, textHeight,
									dlgHdl, (HMENU) 0, ghInst, 0);


	// Add a Cancel button:
	buttonWidth = 10*baseunitX;
	buttonHeight = (3*baseunitY)/2;
	buttonX =    (((DIALOG_WIDTH*baseunitX)/4) - buttonWidth)
		       / 2;
	buttonY =  (3 *  (((DIALOG_HEIGHT*baseunitY)/8) - buttonHeight))
			   / 5;
	hwndButton = CreateWindow ("button", "Cancel", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON,
									buttonX, buttonY, buttonWidth, buttonHeight,
									dlgHdl, (HMENU) 0, ghInst, 0);
//	WinSetFont (&lf,"MS Sans Serif",0,8);
	SendMessage(hwndButton,WM_SETFONT,(WPARAM)CreateFontIndirect (&lf),MAKELPARAM (TRUE,0));
	SendMessage(hwndText,WM_SETFONT,(WPARAM)CreateFontIndirect (&lf),MAKELPARAM (TRUE,0));

	ShowWindow (dlgHdl,SW_SHOWNORMAL);

	return dlgHdl;
}

/*	PA: Called in Clean. */
int addSemaphor(int add)
{
	int old=semaphor;
	semaphor+=add;
	return old;
}

int os_printsetupvalidC(DEVMODE *devmode, char *device, char *driver)
{
	HDC	icPrint;

	icPrint	= myCreateIC(driver,device,devmode);
	if (icPrint)
		DeleteDC(icPrint);
	return icPrint!=NULL;
}
#endif
