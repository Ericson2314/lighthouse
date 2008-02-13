/*	Implementation of cross call procedure table.
	In this table a linked list of cross call entries is kept.
	A cross call entry is a pair of a cross call code (CcRq...) number (see intrface_121.h)
	and a pointer to a cross call procedure.
*/
#ifndef CROSSCALLPROCEDURETABLE_H
#define CROSSCALLPROCEDURETABLE_H

#include "util_121.h"

//	A CrossCallProcedure is a procedure that modifies a CrossCallInfo struct.
typedef void (*CrossCallProcedure)(CrossCallInfo *);

typedef struct _crosscallentry *CrossCallEntry;
typedef struct _crosscallproceduretable *CrossCallProcedureTable;

//	NewCrossCallEntry creates a CrossCallEntry with cce_next field NULL.
extern CrossCallEntry NewCrossCallEntry (int cce_code, CrossCallProcedure cce_proc);

//	FreeCrossCallEntry frees a CrossCallEntry.
extern void FreeCrossCallEntry (CrossCallEntry cce);

//	EmptyCrossCallProcedureTable creates an empty table.
extern CrossCallProcedureTable EmptyCrossCallProcedureTable (void);

//	GetCrossCallProcedureTableSize returns the current number of installed cross call procedures.
extern int GetCrossCallProcedureTableSize (CrossCallProcedureTable ccpt);

//	FreeCrossCallProcedureTable frees a CrossCallProcedureTable.
extern void FreeCrossCallProcedureTable (CrossCallProcedureTable ccpt);

//	AddCrossCallEntry adds the given entry if not already present.
extern void AddCrossCallEntry (CrossCallProcedureTable ccpt, int cce_code, CrossCallProcedure cce_proc);

//	AddCrossCallEntries (table,entries) adds the entries to table.
extern void AddCrossCallEntries (CrossCallProcedureTable theTable, CrossCallProcedureTable entries);

//	FindCrossCallEntry returns the found CrossCallProcedure or NULL if not found.
extern CrossCallProcedure FindCrossCallEntry (CrossCallProcedureTable ccpt, int cce_code);

#endif
