/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2001-2008
 *
 * Support for the lightweight concurrency system.
 * 
 * -------------------------------------------------------------------------*/

#include "Rts.h"

/* Walk the stack pointed to by sp, looking for the ATOMICALLY frame */
StgPtr findAtomicallyFrame (StgPtr sp)
{
    StgPtr           next;
    StgRetInfoTable *info;

    while (1) {
        info = get_ret_itbl((StgClosure*) sp);
        next = sp + stack_frame_sizeW((StgClosure*) sp);

        if (info->i.type == ATOMICALLY_FRAME)
            return sp;

        ASSERT(info->i.type != CATCH_FRAME);
        ASSERT(info->i.type != STOP_FRAME);
        sp = next;
    }
}

StgPtr rts_findRelocatedTSO(StgPtr tso)
{
    StgTSO *p = (StgTSO*) tso;
    while (p->what_next == ThreadRelocated)
        p = p->link;
    return (StgPtr) p;
}

