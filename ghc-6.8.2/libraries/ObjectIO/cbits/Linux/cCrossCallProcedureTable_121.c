/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Implementation of cross call procedure table.
	In this table a linked list of cross call entries is kept.
	A cross call entry is a pair of a cross call code (CcRq...) number (see intrface_121.h)
	and a pointer to a cross call procedure.
	Routines related to printer handling.
********************************************************************************************/
#include "cCrossCallProcedureTable_121.h"

//	A CrossCallEntry contains a CcRq number and a pointer to a CrossCallProcedure.
struct _crosscallentry
{	int					cce_code;		// CcRq... number
	CrossCallProcedure	cce_proc;		// The procedure to be called in case of cce_code
	CrossCallEntry		cce_next;		// The next entry in the list
};
//	A CrossCallProcedureTable contains a linked list of CrossCallEntries.
struct _crosscallproceduretable
{	int                 ccpt_size;		// nr of entries
	CrossCallEntry		ccpt_first;		// first entry
	CrossCallEntry		ccpt_last;		// last entry
};


//	NewCrossCallEntry creates a CrossCallEntry with cce_next field NULL.
CrossCallEntry NewCrossCallEntry (int cce_code, CrossCallProcedure cce_proc)
{
	CrossCallEntry cce = rmalloc (sizeof (struct _crosscallentry));

	cce->cce_code = cce_code;
	cce->cce_proc = cce_proc;
	cce->cce_next = NULL;

	return cce;
}

//	FreeCrossCallEntry frees a CrossCallEntry.
void FreeCrossCallEntry (CrossCallEntry cce)
{
	rfree (cce);
}

//	EmptyCrossCallProcedureTable creates an empty table.
CrossCallProcedureTable EmptyCrossCallProcedureTable (void)
{
	CrossCallProcedureTable ccpt = rmalloc (sizeof (struct _crosscallproceduretable));

	ccpt->ccpt_size  = 0;
	ccpt->ccpt_first = NULL;
	ccpt->ccpt_last  = NULL;

	return ccpt;
}

//	GetCrossCallProcedureTableSize returns the current number of installed cross call procedures.
int GetCrossCallProcedureTableSize (CrossCallProcedureTable ccpt)
{
	return ccpt->ccpt_size;
}

//	FreeCrossCallProcedureTable frees a CrossCallProcedureTable.
void FreeCrossCallProcedureTable (CrossCallProcedureTable ccpt)
{
	rfree (ccpt);
}

/*	SearchCrossCallEntry (nr,entry) returns the first CrossCallEntry
	following/including entry that either:
		matches nr, or
		is the entry after which a new entry with nr should be added, or
		NULL in case nr should be placed before entry.
*/
static CrossCallEntry SearchCrossCallEntry (int nr,CrossCallEntry entry)
{
	if (nr == entry->cce_code)
		return entry;				// entry found
	if (nr < entry->cce_code)
		return NULL;				// no entry found
	if (entry->cce_next == NULL)
		return entry;				// last entry; should insert new entry after this one
	if (nr < entry->cce_next->cce_code)
		return entry;				// next entry exceeds nr; should insert new entry after this one
	return SearchCrossCallEntry (nr,entry->cce_next);
}

//	AddCrossCallEntry (table,nr,proc) adds a new entry (nr,proc) if an entry with nr is not already present.
void AddCrossCallEntry (CrossCallProcedureTable ccpt, int cce_code, CrossCallProcedure cce_proc)
{
	CrossCallEntry entry = NewCrossCallEntry (cce_code,cce_proc);

	if (ccpt->ccpt_size == 0)
	{	// table is empty; create entry and add it
		ccpt->ccpt_size  = 1;
		ccpt->ccpt_first = entry;
		ccpt->ccpt_last  = entry;
	}
	else if (cce_code < ccpt->ccpt_first->cce_code)
	{	// entry should be inserted before first entry
		ccpt->ccpt_size += 1;
		entry->cce_next = ccpt->ccpt_first;
		ccpt->ccpt_first= entry;
	}
	else if (cce_code > ccpt->ccpt_first->cce_code)
	{	// entry could be in table; look for it and add it if not present
		CrossCallEntry searchCCE;
		searchCCE = SearchCrossCallEntry (cce_code,ccpt->ccpt_first);

		if (searchCCE == NULL)
		{
			printf("\'AddCrossCallEntry\' SearchCrossCallEntry returned NULL CrossCallEntry");
			exit(1);
		}
		if (searchCCE->cce_code != cce_code)
		{	// entry not in table but should be linked after searchCCE
			gboolean appendLast = (ccpt->ccpt_last == searchCCE);
			ccpt->ccpt_size     += 1;
			entry->cce_next     = searchCCE->cce_next;
			searchCCE->cce_next = entry;

			if (appendLast)
				ccpt->ccpt_last  = entry;	// adjust last if entry is appended at end
		}
	}
}

//	AddCrossCallEntries (table,entries) adds the entries to table
void AddCrossCallEntries (CrossCallProcedureTable theTable, CrossCallProcedureTable entries)
{
	CrossCallEntry cce = entries->ccpt_first;

	while (cce != NULL)
	{
		AddCrossCallEntry (theTable, cce->cce_code, cce->cce_proc);
		cce = cce->cce_next;
	}
}

//	FindCrossCallEntry returns the found CrossCallProcedure or NULL if not found.
CrossCallProcedure FindCrossCallEntry (CrossCallProcedureTable ccpt, int cce_code)
{
	if (ccpt->ccpt_size == 0)	// table is empty, return NULL
		return NULL;
	else
	{
		CrossCallEntry searchCCE;
		searchCCE = SearchCrossCallEntry (cce_code,ccpt->ccpt_first);
		if (searchCCE && searchCCE->cce_code == cce_code)
			return searchCCE->cce_proc;
		else
			return NULL;
	}
}

