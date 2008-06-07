/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2006
 *
 * Capabilities
 *
 * The notion of a capability is used when operating in multi-threaded
 * environments (which the THREADED_RTS build of the RTS does), to
 * hold all the state an OS thread/task needs to run Haskell code:
 * its STG registers, a pointer to its  TSO, a nursery etc. During
 * STG execution, a pointer to the capabilitity is kept in a 
 * register (BaseReg).
 *
 * Only in an THREADED_RTS build will there be multiple capabilities,
 * in the non-threaded builds there is one global capability, namely
 * MainCapability.
 *
 * This header file contains the functions for working with capabilities.
 * (the main, and only, consumer of this interface is the scheduler).
 * 
 * --------------------------------------------------------------------------*/

#ifndef CAPABILITY_H
#define CAPABILITY_H

#include "RtsFlags.h"

struct Capability_ {
    // State required by the STG virtual machine when running Haskell
    // code.  During STG execution, the BaseReg register always points
    // to the StgRegTable of the current Capability (&cap->r).
    StgFunTable f;
    StgRegTable r;

    nat no;  // capability number.

    // true if this Capability is running Haskell code, used for
    // catching unsafe call-ins.
    rtsBool in_haskell;

    // One mutable list per generation, so we don't need to take any
    // locks when updating an old-generation thunk.  These
    // mini-mut-lists are moved onto the respective gen->mut_list at
    // each GC.
    bdescr **mut_lists;
}; // typedef Capability, defined in RtsAPI.h

// Converts a *StgRegTable into a *Capability.
//
INLINE_HEADER Capability *
regTableToCapability (StgRegTable *reg)
{
    return (Capability *)((void *)((unsigned char*)reg - sizeof(StgFunTable)));
}

// Initialise the available capabilities.
//
void initCapabilities (void);

#if !IN_STG_CODE
// one global capability
extern Capability MainCapability; 
#endif

// Array of all the capabilities
//
extern nat n_capabilities;
extern Capability *capabilities;

// The Capability that was last free.  Used as a good guess for where
// to assign new threads.
//
extern Capability *last_free_capability;

INLINE_HEADER void recordMutableCap (StgClosure *p, Capability *cap, nat gen);

// Grab a capability.  (Only in the non-threaded RTS; in the threaded
// RTS one of the waitFor*Capability() functions must be used).
//
extern void grabCapability (Capability **pCap);

// Free a capability on exit
void freeCapability (Capability *cap);

/* -----------------------------------------------------------------------------
 * INLINE functions... private below here
 * -------------------------------------------------------------------------- */

INLINE_HEADER void
recordMutableCap (StgClosure *p, Capability *cap, nat gen)
{
    bdescr *bd;

    bd = cap->mut_lists[gen];
    if (bd->free >= bd->start + BLOCK_SIZE_W) {
	bdescr *new_bd;
	new_bd = allocBlock_lock();
	new_bd->link = bd;
	bd = new_bd;
	cap->mut_lists[gen] = bd;
    }
    *bd->free++ = (StgWord)p;
}

#endif /* CAPABILITY_H */
