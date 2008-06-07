/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2001-2008
 *
 * Support for the lightweight concurrency system.
 * 
 * -------------------------------------------------------------------------*/

#include "Rts.h"

StgPtr rts_findRelocatedTSO(StgPtr tso)
{
    StgTSO *p = (StgTSO*) tso;
    while (p->what_next == ThreadRelocated)
        p = p->link;
    return (StgPtr) p;
}

