/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2003-2006
 *
 * Capabilities
 *
 * A Capability represent the token required to execute STG code,
 * and all the state an OS thread/task needs to run Haskell code:
 * its STG registers, a pointer to its TSO, a nursery etc. During
 * STG execution, a pointer to the capabilitity is kept in a
 * register (BaseReg; actually it is a pointer to cap->r).
 *
 * Only in an THREADED_RTS build will there be multiple capabilities,
 * for non-threaded builds there is only one global capability, namely
 * MainCapability.
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "OSThreads.h"
#include "Capability.h"
#include "Schedule.h"
#include "Sparks.h"
#include "Trace.h"

// one global capability, this is the Capability for non-threaded
// builds, and for +RTS -N1
Capability MainCapability;

nat n_capabilities;
Capability *capabilities = NULL;

// Holds the Capability which last became free.  This is used so that
// an in-call has a chance of quickly finding a free Capability.
// Maintaining a global free list of Capabilities would require global
// locking, so we don't do that.
Capability *last_free_capability;

/* ----------------------------------------------------------------------------
 * Initialisation
 *
 * The Capability is initially marked not free.
 * ------------------------------------------------------------------------- */

static void
initCapability( Capability *cap, nat i )
{
    nat g;

    cap->no = i;
    cap->in_haskell        = rtsFalse;

    cap->f.stgGCEnter1     = (F_)__stg_gc_enter_1;
    cap->f.stgGCFun        = (F_)__stg_gc_fun;

    cap->mut_lists  = stgMallocBytes(sizeof(bdescr *) *
				     RtsFlags.GcFlags.generations,
				     "initCapability");

    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
	cap->mut_lists[g] = NULL;
    }
}

/* ---------------------------------------------------------------------------
 * Function:  initCapabilities()
 *
 * Purpose:   set up the Capability handling. For the THREADED_RTS build,
 *            we keep a table of them, the size of which is
 *            controlled by the user via the RTS flag -N.
 *
 * ------------------------------------------------------------------------- */
void
initCapabilities( void )
{
    n_capabilities = 1;
    capabilities = &MainCapability;
    initCapability(&MainCapability, 0);

    last_free_capability = &capabilities[0];
}

void
freeCapability (Capability *cap) {
    stgFree(cap->mut_lists);
#if defined(THREADED_RTS) || defined(PARALLEL_HASKELL)
    freeSparkPool(&cap->r.rSparks);
#endif
}

