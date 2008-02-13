/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1, 
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Implementation of accelerator tables.
	For each accelerator key four entries are stored in the acceleratortable:
		Ctrl          +key
		Ctrl+Shift    +key
		Ctrl      +Alt+key
		Ctrl+Shift+Alt+key
	For this reason a minimum size of 4 (MINSIZEPROCESSSHORTCUTTABLE) has been set. 
	Deflating the table never decreases below this value. 
********************************************************************************************/
#include "cAcceleratorTable_121.h"
#include "util_121.h"


void CopyACCEL (ACCEL *source, ACCEL *dest)
{
	dest->fVirt = source->fVirt;
	dest->key   = source->key;
	dest->cmd   = source->cmd;
}

void SetACCELentry (ACCEL *entry, BYTE flags, int key, int id)
{
	entry->fVirt = flags;
	entry->key   = (WORD) ((BYTE) VkKeyScan ((char) key));
	entry->cmd   = (WORD) id;
}


/*	AllocateProcessShortcutTable (size) creates a shortcut table of the given size. 
*/
ProcessShortcutTable AllocateProcessShortcutTable (int size)
{
	ProcessShortcutTable table;
	ACCEL *shortcuts;

	table = (ProcessShortcutTable) rmalloc (sizeof (struct ProcessShortcutTable));
	shortcuts = (ACCEL *) rmalloc (size * sizeof (ACCEL));

	table->pst_size = size;
	table->pst_used = 0;
	table->pst_shortcuts = shortcuts;

	return (table);
}

/*	DestroyProcessShortcutTable (table) frees the memory used by the table. 
*/
void DestroyProcessShortcutTable (ProcessShortcutTable table)
{
	rfree (table->pst_shortcuts);
	rfree (table);
}

/*	InflateProcessShortcutTable (table) returns a new table double the size of the argument table. 
*/
ProcessShortcutTable InflateProcessShortcutTable (ProcessShortcutTable oldTable)
{
	int i, oldSize, newSize;
	ProcessShortcutTable newTable;

	oldSize = oldTable->pst_size;
	newSize = oldSize*2;

	newTable = (ProcessShortcutTable) AllocateProcessShortcutTable (newSize);

	for (i=0; i<oldSize; i++)
		CopyACCEL (&oldTable->pst_shortcuts[i], &newTable->pst_shortcuts[i]);

	newTable->pst_used = oldTable->pst_used;

	rfree (oldTable);
	return (newTable);
}

/*	DeflateProcessShortcutTable (table) returns a new table half the size of the argument table. 
	In case the table already is at its minimum size (MINSIZEPROCESSSHORTCUTTABLE) the argument
	table is returned.
*/
ProcessShortcutTable DeflateProcessShortcutTable (ProcessShortcutTable oldTable)
{
	int i, oldSize, newSize;
	ProcessShortcutTable newTable;

	oldSize = oldTable->pst_size;
	newSize = oldSize/2;

	if (newSize < MINSIZEPROCESSSHORTCUTTABLE)
	{
		return (oldTable);
	}
	else
	{
		newTable = (ProcessShortcutTable) AllocateProcessShortcutTable (newSize);

		for (i=0; i<newSize; i++)
			CopyACCEL (&oldTable->pst_shortcuts[i], &newTable->pst_shortcuts[i]);
		
		newTable->pst_used = min (oldTable->pst_used, newSize);

		rfree (oldTable);
		return (newTable);
	}
}

/*	AddProcessShortcut (key,id,table) returns a new table in which the shortkey (id,key) has been added.
	The new table may have been inflated to accomodate the new shortkey.
	For each shortkey the following four accelerator flags are entered as separate entries:
		FVIRTKEY | FCONTROL
		FVIRTKEY | FCONTROL | FSHIFT
		FVIRTKEY | FCONTROL          | FALT
		FVIRTKEY | FCONTROL | FSHIFT | FALT
*/
ProcessShortcutTable AddProcessShortcut (int key, int id, ProcessShortcutTable oldTable)
{
	ACCEL *newentry;
	ProcessShortcutTable newTable;
	int used = oldTable->pst_used;

	// first make sure that the table has the proper size.
	if (used+4 >= oldTable->pst_size)
	{
		newTable = InflateProcessShortcutTable (oldTable);
	}
	else
	{
		newTable = oldTable;
	}

	// Add Ctrl+key:
	newentry = &newTable->pst_shortcuts[used];
	SetACCELentry (newentry, (BYTE) FCONTROL | FVIRTKEY, key, id);
	// Add Ctrl+Shift+key:
	newentry = &newTable->pst_shortcuts[used+1];
	SetACCELentry (newentry, (BYTE) FCONTROL | FSHIFT | FVIRTKEY, key, id);
	// Add Ctrl+Alt+key:
	newentry = &newTable->pst_shortcuts[used+2];
	SetACCELentry (newentry, (BYTE) FCONTROL | FALT | FVIRTKEY, key, id);
	// Add Ctrl+Shift+Alt+key:
	newentry = &newTable->pst_shortcuts[used+3];
	SetACCELentry (newentry, (BYTE) FCONTROL |FSHIFT | FALT | FVIRTKEY, key, id);

	newTable->pst_used = used+4;

	return(newTable);
}

/*	RemoveProcessShortcut (id,table) returns a new table in which the shortkey (id,_) has been removed.
	The new table may have been deflated to use up less memory space.
	In case the entry (id,_) has been located, the consecutive 3 entries are also removed as these
	encode the accelerator flag versions.
*/
ProcessShortcutTable RemoveProcessShortcut (int id, ProcessShortcutTable oldTable)
{
	int i;
	int foundat = 0;
	int used    = oldTable->pst_used;

	// search for the element to be deleted.
	while (foundat<used && oldTable->pst_shortcuts[foundat].cmd != id)
	{
		foundat++;
	}

	if (foundat>=used)
	{
		return (oldTable);			// the element was not found, so return argument table
	}
	
	used -= 4;
	for (i=foundat; i<used; i++)	// otherwise shift remaining entries to the left
		CopyACCEL (&oldTable->pst_shortcuts[i+4], &oldTable->pst_shortcuts[i]);
	oldTable->pst_used = used;
	
	if (used < oldTable->pst_size/2 && used > MINSIZEPROCESSSHORTCUTTABLE)	// check if table needs to be deflated
	{
		return (DeflateProcessShortcutTable (oldTable));
	}
	else
	{
		return (oldTable);
	}
}

/*	UpdateAcceleratorTable (table,frame) returns a new table that corresponds with the shortkeys
	administered in frame. It is assumed that frame contains a pointer to a shortcut table.
	The argument table should not be used anymore, because it might have been destroyed.
*/
HACCEL UpdateAcceleratorTable (HACCEL gAcceleratorTable,HWND ghActiveFrameWindow)
{
	ProcessShortcutTable table;

	if (gAcceleratorTable!=NULL)		// Destroy the previous version if exists
	{
		DestroyAcceleratorTable (gAcceleratorTable);
	}
	gAcceleratorTable = NULL;

	if (ghActiveFrameWindow!=NULL)		// The current frame is an (MDI/SDI) frame
	{
		table = (ProcessShortcutTable) GetWindowLong (ghActiveFrameWindow, 0);
		if (table!=NULL)
		{
			gAcceleratorTable = CreateAcceleratorTable (table->pst_shortcuts,table->pst_used);
		}
	}
	return gAcceleratorTable;
}
