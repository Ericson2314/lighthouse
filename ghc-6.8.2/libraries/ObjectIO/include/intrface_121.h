/* C module intrface */

//#include "util_121.h"

#define MaxRand									32767
#define iWhitePattern							4
#define iLtGreyPattern							3
#define iGreyPattern							2
#define iDkGreyPattern							1
#define iBlackPattern							0
#define iModeNotBic								7
#define iModeNotXor								6
#define iModeNotOr								5
#define iModeNotCopy							4
#define iModeBic								3
#define iModeXor								2
#define iModeOr									1
#define iModeCopy								0
#define iStrikeOut								8
#define iUnderline								4
#define iItalic									2
#define iBold									1

#define WinEscapeKey							27
#define WinReturnKey							13
#define WinTabKey								9
#define WinBackSpKey							8
#define WinF1Key								1001
#define WinF2Key								1002
#define WinF3Key								1003
#define WinF4Key								1004
#define WinF5Key								1005
#define WinF6Key								1006
#define WinF7Key								1007
#define WinF8Key								1008
#define WinF9Key								1009
#define WinF10Key								1010
#define WinF11Key								1011
#define WinF12Key								1012
#define WinHelpKey								1013
#define WinDelKey								1014
#define WinEndKey								1015
#define WinBeginKey								1016
#define WinPgDownKey							1017
#define WinPgUpKey								1018
#define WinRightKey								1019
#define WinLeftKey								1020
#define WinDownKey								1021
#define WinUpKey								1022

#define CTRLBIT									4
#define ALTBIT									2
#define SHIFTBIT								1
#define KEYREPEAT								4
#define KEYUP									2
#define KEYDOWN									1
#define BUTTONSTILLUP							0			/* PA: new constant for mouse handling. */
#define BUTTONUP								50
#define BUTTONSTILLDOWN							40
#define BUTTONTRIPLEDOWN						3
#define BUTTONDOUBLEDOWN						2
#define BUTTONDOWN								1
#define EDITISMULTILINE							1			/* PA: flag value: edit control is multi-line. */
#define EDITISKEYSENSITIVE						2			/* PA: flag value: edit control sends keyboard events to Clean. */

/*	Constants that are passed when creating (custom)button controls.
*/
#define ISNORMALBUTTON							0			/* The button is a normal button.   */
#define ISOKBUTTON								1			/* The button is the OK button.     */
#define ISCANCELBUTTON							2			/* The button is the CANCEL button. */

/*	Game cross call codes. */
#define CcRqUSERGAMEEVENT						1905        /* send user event to other objects */
#define CcRqCREATEGAMEOBJECT					1904        /* create a new game object */
#define CcRqPLAYSOUNDSAMPLE						1903        /* initialize sound sample */
#define CcRqRUNGAME								1901		/* run the game engine */
#define CcRqCREATEGAMEWINDOW					1900		/* create a game window */

/*	Print cross call codes. */
#define CcRqDO_PRINT_SETUP						1828
#define CcRqDO_HTML_HELP						1827
#define CcRqGET_PRINTER_DC						1824
#define CcRqDISPATCH_MESSAGES_WHILE_PRINTING	1823
#define CcRqENDDOC								1822
#define CcRqSTARTDOC							1821

#define CcRqCREATETCPWINDOW						1820		/* create TCP window */
#define CcRqDESTROYMDIDOCWINDOW					1817		/* destroy MDI document window */
#define CcRqCREATESDIDOCWINDOW					1816		/* create SDI document window */
#define CcRqCREATEMDIDOCWINDOW					1815		/* create MDI document window */
#define CcRqCREATEMDIFRAMEWINDOW				1814		/* create MDI frame window */
#define CcRqCREATESDIFRAMEWINDOW				1813		/* create SDI frame window */
#define CcRqCLIPBOARDHASTEXT					1812
#define CcRqGETCLIPBOARDTEXT					1811
#define CcRqSETCLIPBOARDTEXT					1810
#define CcRqDIRECTORYDIALOG						1802		/* create directory selector dialog. */
#define CcRqFILESAVEDIALOG						1801
#define CcRqFILEOPENDIALOG						1800
#define CcRqSHOWCONTROL							1755
#define CcRqSELECTPOPUPITEM						1754
#define CcRqENABLEPOPUPITEM						1753
#define CcRqADDTOPOPUP							1752
#define CcRqSETITEMCHECK						1751
#define CcRqENABLECONTROL						1750
#define CcRqCREATECOMPOUND						1729
#define CcRqCREATESCROLLBAR						1728
#define CcRqCREATECUSTOM						1727
#define CcRqCREATEICONBUT						1726
#define CcRqCREATEPOPUP							1725
#define CcRqCREATECHECKBOX						1724
#define CcRqCREATERADIOBUT						1723
#define CcRqCREATEEDITTXT						1722
#define CcRqCREATESTATICTXT						1721
#define CcRqCREATEBUTTON						1720
#define CcRqCREATEMODALDIALOG					1701		/* create modal dialog. */
#define CcRqCREATEDIALOG						1700
#define CcRqCREATETOOLBARSEPARATOR				1603		/* create a toolbar separator item. */
#define CcRqCREATETOOLBARITEM					1602		/* create a toolbar bitmap item. */
#define CcRqCREATEMDITOOLBAR					1601		/* create a toolbar for a MDI process. */
#define CcRqCREATESDITOOLBAR					1600		/* create a toolbar. */
#define CcCbFONTSIZE							1530
#define CcCbFONTNAME							1520
#define CcRqGETFONTSIZES						1510
#define CcRqGETFONTNAMES						1500

#define CcRqSETCLIENTSIZE						1438		/* set client size. */
#define CcRqDELCONTROLTIP						1437		/* remove controls from tooltip areas. */
#define CcRqADDCONTROLTIP						1436		/* add controls to tooltip areas. */
#define CcRqGETWINDOWSIZE						1435
#define CcRqRESTACKWINDOW						1434
#define CcRqSHOWWINDOW							1433
#define CcRqSETWINDOWSIZE						1432
#define CcRqSETSELECTWINDOW						1431
#define CcRqSETWINDOWPOS						1430
#define CcRqSETEDITSELECTION					1428
#define CcRqSETSCROLLSIZE						1427
#define CcRqSETSCROLLPOS						1426
#define CcRqSETSCROLLRANGE						1425
#define CcRqOBSCURECURSOR						1422
#define CcRqCHANGEWINDOWCURSOR					1421
#define CcRqACTIVATEWINDOW						1420		/* activating window. */
#define CcRqACTIVATECONTROL						1419		/* activating controls. */
#define CcRqCREATECARET							1610
#define CcRqSETCARETPOS							1611
#define CcRqDESTROYCARET						1612
#define CcRqHIDECARET							1613
#define CcRqSHOWCARET							1614
#define CcRqGETWINDOWPOS						1416
#define CcRqGETCLIENTSIZE						1415
#define CcRqUPDATEWINDOWRECT					1412		/* updating rect part of a window/control. */
#define CcRqGETWINDOWTEXT						1411
#define CcRqSETWINDOWTITLE						1410
#define CcRqFAKEPAINT							1405		/* combination of BeginPaint; EndPaint; InvalidateRect; */
#define CcRqENDPAINT							1404
#define CcRqBEGINPAINT							1403
#define CcRqDESTROYWINDOW						1402
#define CcRqDESTROYMODALDIALOG					1401		/* destroy modal dialog. */
#define CcRqDRAWMBAR							1265
#define CcRqTRACKPOPMENU						1256		/* handling pop up menu. */
#define CcRqCREATEPOPMENU						1255
#define CcRqINSERTSEPARATOR						1245
#define CcRqMENUENABLE							1235
#define CcRqMODIFYMENU							1230
#define CcRqINSERTMENU							1226		/* inserting a menu in the menu bar. */
#define CcRqITEMENABLE							1220
#define CcRqREMOVEMENUSHORTKEY					1217		/* removing a shortkey of a menu item. */
#define CcRqMODIFYMENUITEM						1215
#define CcRqDELETEMENU							1213		/* deleting a menu */
#define CcRqREMOVEMENUITEM						1212
#define CcRqCHECKMENUITEM						1210
#define CcRqINSERTMENUITEM						1205
#define CcRqCREATELISTBOX 						1206
#define CcRqADDTOLISTBOX 						1207
#define CcRqSELECTLISTBOXITEM					1208
#define CcRqMARKLISTBOXITEM						1209
#define CcRqDOMESSAGE							1100


/*	Game OS to Clean codes: 500-599 */
#define CcWmCHECKQUIT							513         /* check user's quit function */
#define CcWmUSEREVENT							512         /* user defined event */
#define CcWmSTATISTICS							511         /* request for statistics */
#define CcWmOBJECTKEYUP							510         /* key released */
#define CcWmOBJECTKEYDOWN						509         /* key pressed for object */
#define CcWmOBJECTTIMER							508			/* framecounter reached 0 */
#define CcWmANIMATION							507			/* animation sequence ended */
#define CcWmCOLLISION							506         /* collision of two objects */
#define CcWmTOUCHBOUND							505			/* object touches bound */
#define CcWmOBJECTDONE							504			/* object is destroyed */
#define CcWmMOVEOBJECT							503			/* move object */
#define CcWmINITOBJECT							502			/* initialize new object */
#define CcWmSCROLL								501			/* calculate layer positions */
#define CcWmGAMEKEYBOARD						500			/* keyboard input for game */

/*	TCP OS to Clean codes: */
#define CcWmINETEVENT							140

#define CcWmSPECIALBUTTON						133			/* info about OK/CANCEL button selected. */
#define CcWmPROCESSDROPFILES					132			/* requesting opening of files. */
#define CcWmGETTOOLBARTIPTEXT					131			/* getting tooltip text. */
#define CcWmSETFOCUS							130			/* notifying obtaining keyboard input focus. */
#define CcWmKILLFOCUS							129			/* notifying loss of keyboard input focus. */
#define CcWmPROCESSCLOSE						127			/* requesting closing of process. */
#define	CcWmDRAWCLIPBOARD						126			/* clipboard handling. Copied from Ronny. */
#define CcWmGETSCROLLBARINFO					125			/* info about scrollbars. */
#define CcWmSCROLLBARACTION						124			/* scrollbar handling. */
#define CcWmDDEEXECUTE							123
#define CcWmIDLEDIALOG							121			/* initialising modal dialogues. */
#define CcWmDRAWCONTROL							120
#define CcWmITEMSELECT							119
#define CcWmBUTTONCLICKED						118
#define CcWmINITDIALOG							117
#define CcWmIDLETIMER							116
#define CcWmTIMER								115
#define CcWmNEWVTHUMB							114
#define CcWmNEWHTHUMB							113
#define CcWmGETVSCROLLVAL						112
#define CcWmGETHSCROLLVAL						111
#define CcWmSIZE								110			/* resize information. */
#define CcWmMOUSE								109
#define CcWmKEYBOARD							108
#define CcWmDEACTIVATE							107
#define CcWmACTIVATE							106
#define CcWmCLOSE								105
#define CcWmCOMMAND								103
#define CcWmCHAR								102
#define CcWmCREATE								101
#define CcWmPAINT								100
#define CcWmNOTIFY								78			/* notify events. */
#define CcWINMESSmax							999
#define CcWINMESSmin							100
#define CcRETURN6								16
#define CcRETURN5								15
#define CcRETURN4								14
#define CcRETURN3								13
#define CcRETURN2								12
#define CcRETURN1								11
#define CcRETURN0								10
#define CcRETURNmax								19
#define CcRETURNmin								10
#define CcWASQUIT								1

// MW: new convention: messages that are passed within the OS thread begin with PM
// They can be in range WM_USER (currently 0x0400) to 0x7FFF.

#define	PM_SOCKET_EVENT							0x0405
#define	PM_DNS_EVENT							0x0406

// Cursor types
#define CURSHIDDEN		6
#define CURSARROW		5
#define CURSFATCROSS	4
#define CURSCROSS		3
#define CURSIBEAM		2
#define CURSBUSY		1
