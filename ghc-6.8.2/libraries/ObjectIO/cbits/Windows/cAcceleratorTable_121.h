/*	Implementation of accelerator tables.
	Now for each MDI and SDI frame window a private accelerator table is stored.
	NDI processes have no frame window, and also don't need an accelerator table.

	For each accelerator key four entries are stored in the acceleratortable:
		Ctrl          +key
		Ctrl+Shift    +key
		Ctrl      +Alt+key
		Ctrl+Shift+Alt+key
	For this reason a minimum size of 4 (MINSIZEPROCESSSHORTCUTTABLE) has been set. 
	Deflating the table never decreases below this value. 
*/
#include <windows.h>

struct ProcessShortcutTable
{
	int pst_size;			// The current size of the table
	int pst_used;			// The current number of filled items
	ACCEL *pst_shortcuts;	// The current table, implemented as a pointer to ACCELs
};

typedef struct ProcessShortcutTable *ProcessShortcutTable;

#define MINSIZEPROCESSSHORTCUTTABLE 4	// The minimum size of an accelerator table is 4

/*	AllocateProcessShortcutTable (size) creates a shortcut table of the given size. 
*/
extern ProcessShortcutTable AllocateProcessShortcutTable (int size);

/*	DestroyProcessShortcutTable (table) frees the memory used by the table. 
*/
extern void DestroyProcessShortcutTable (ProcessShortcutTable table);

/*	InflateProcessShortcutTable (table) returns a new table double the size of the argument table. 
*/
extern ProcessShortcutTable InflateProcessShortcutTable (ProcessShortcutTable oldTable);

/*	DeflateProcessShortcutTable (table) returns a new table half the size of the argument table. 
	In case the table already is at its minimum size (MINSIZEPROCESSSHORTCUTTABLE) the argument
	table is returned.
*/
extern ProcessShortcutTable DeflateProcessShortcutTable (ProcessShortcutTable oldTable);

/*	AddProcessShortcut (key,id,table) returns a new table in which the shortkey (id,key) has been added.
	The new table may have been inflated to accomodate the new shortkey.
	For each shortkey the following four accelerator flags are entered as separate entries:
		FVIRTKEY | FCONTROL
		FVIRTKEY | FCONTROL | FSHIFT
		FVIRTKEY | FCONTROL          | FALT
		FVIRTKEY | FCONTROL | FSHIFT | FALT
*/
extern ProcessShortcutTable AddProcessShortcut (int key, int id, ProcessShortcutTable oldTable);

/*	RemoveProcessShortcut (id,table) returns a new table in which the shortkey (id,_) has been removed.
	The new table may have been deflated to use up less memory space.
	In case the entry (id,_) has been located, the consecutive 3 entries are also removed as these
	encode the accelerator flag versions.
*/
extern ProcessShortcutTable RemoveProcessShortcut (int id, ProcessShortcutTable oldTable);

/*	UpdateAcceleratorTable (table,frame) returns a new table that corresponds with the shortkeys
	administered in frame. It is assumed that frame contains a pointer to a shortcut table.
	The argument table should not be used anymore, because it might have been destroyed.
*/
extern HACCEL UpdateAcceleratorTable (HACCEL gAcceleratorTable,HWND ghActiveFrameWindow);
