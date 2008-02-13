#ifndef _CPRINTER
#define _CPRINTER

#if defined(mingw32_HOST_OS)
//	PA: all made extern
extern int startPage(int hdc);
extern int endPage  (int hdc);
extern int startDoc (int hdc);
			// returns err code: >0:no error, <=0: user cancelled file dialog
extern void endDoc  (int hdc);
extern void deleteDC(int hdc);
extern int wasCanceled(void);
extern void printSetup (int calledFromCleanThread, int devmodeSize,
						char *devmode, char *device, char *driver, char *output,
						int *ok, PRINTDLG **pdPtr
					   );
extern void getDC( int doDialog, int emulateScreen, int calledFromCleanThread, int devmodeLength,
				   char *devmode,char *device,char *driver,char *output,
				   int *err,
				   int *first, int *last, int *copies,
				   PRINTDLG	**ppPrintDlg,
				   int *deviceContext
	 			  );
					// err code: -1:no error, others: non fatal error
extern void get_printSetup_with_PRINTDLG(PRINTDLG *pd, char **o_devmode,
										 char **o_device, char **o_driver, char **o_output);
extern void getCaps(HDC hdcPrint, int unq,
					int *maxX, int *maxY,
					int *leftPaper, int *topPaper,
					int *rightPaper, int *bottomPaper,
					int *unqReturn
				   );

extern BOOL CALLBACK AbortProc (HDC hdcPrn, int iCode);
extern BOOL CALLBACK PrintDlgProc (HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam);
extern HWND CreateCancelDialog(void);
#endif

#endif
