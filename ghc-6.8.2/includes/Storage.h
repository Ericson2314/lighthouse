/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * External Storage Manger Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef STORAGE_H
#define STORAGE_H

#include <stddef.h>
#include "OSThreads.h"

/* -----------------------------------------------------------------------------
 * Generational GC
 *
 * We support an arbitrary number of generations, with an arbitrary number
 * of steps per generation.  Notes (in no particular order):
 *
 *       - all generations except the oldest should have two steps.  This gives
 *         objects a decent chance to age before being promoted, and in
 *         particular will ensure that we don't end up with too many
 *         thunks being updated in older generations.
 *
 *       - the oldest generation has one step.  There's no point in aging
 *         objects in the oldest generation.
 *
 *       - generation 0, step 0 (G0S0) is the allocation area.  It is given
 *         a fixed set of blocks during initialisation, and these blocks
 *         are never freed.
 *
 *       - during garbage collection, each step which is an evacuation
 *         destination (i.e. all steps except G0S0) is allocated a to-space.
 *         evacuated objects are allocated into the step's to-space until
 *         GC is finished, when the original step's contents may be freed
 *         and replaced by the to-space.
 *
 *       - the mutable-list is per-generation (not per-step).  G0 doesn't 
 *         have one (since every garbage collection collects at least G0).
 * 
 *       - block descriptors contain pointers to both the step and the
 *         generation that the block belongs to, for convenience.
 *
 *       - static objects are stored in per-generation lists.  See GC.c for
 *         details of how we collect CAFs in the generational scheme.
 *
 *       - large objects are per-step, and are promoted in the same way
 *         as small objects, except that we may allocate large objects into
 *         generation 1 initially.
 *
 * ------------------------------------------------------------------------- */

typedef struct step_ {
  unsigned int         no;		/* step number */
  bdescr *             blocks;		/* blocks in this step */
  unsigned int         n_blocks;	/* number of blocks */
  struct step_ *       to;		/* destination step for live objects */
  struct generation_ * gen;		/* generation this step belongs to */
  unsigned int         gen_no;          /* generation number (cached) */
  bdescr *             large_objects;	/* large objects (doubly linked) */
  unsigned int         n_large_blocks;  /* no. of blocks used by large objs */
  int                  is_compacted;	/* compact this step? (old gen only) */

  /* During GC, if we are collecting this step, blocks and n_blocks
   * are copied into the following two fields.  After GC, these blocks
   * are freed. */
  bdescr *     old_blocks;	        /* bdescr of first from-space block */
  unsigned int n_old_blocks;		/* number of blocks in from-space */

  /* temporary use during GC: */
  StgPtr       hp;			/* next free locn in to-space */
  StgPtr       hpLim;			/* end of current to-space block */
  bdescr *     hp_bd;			/* bdescr of current to-space block */
  StgPtr       scavd_hp;		/* ... same as above, but already */
  StgPtr       scavd_hpLim;		/*     scavenged.  */
  bdescr *     scan_bd;			/* block currently being scanned */
  StgPtr       scan;			/* scan pointer in current block */
  bdescr *     new_large_objects;    	/* large objects collected so far */
  bdescr *     scavenged_large_objects; /* live large objs after GC (d-link) */
  unsigned int n_scavenged_large_blocks;/* size of above */
  bdescr *     bitmap;  		/* bitmap for compacting collection */
} step;

typedef struct generation_ {
  unsigned int   no;			/* generation number */
  step *         steps;			/* steps */
  unsigned int   n_steps;		/* number of steps */
  unsigned int   max_blocks;		/* max blocks in step 0 */
  bdescr        *mut_list;      	/* mut objects in this gen (not G0)*/

  /* temporary use during GC: */
  bdescr        *saved_mut_list;

  /* stats information */
  unsigned int collections;
  unsigned int failed_promotions;
} generation;

extern generation * RTS_VAR(generations);

extern generation * RTS_VAR(g0);
extern step * RTS_VAR(g0s0);
extern generation * RTS_VAR(oldest_gen);

/* -----------------------------------------------------------------------------
   Initialisation / De-initialisation
   -------------------------------------------------------------------------- */

extern void initStorage(void);
extern void exitStorage(void);
extern void freeStorage(void);

/* -----------------------------------------------------------------------------
   Generic allocation

   StgPtr allocate(nat n)       Allocates a chunk of contiguous store
   				n words long, returning a pointer to
				the first word.  Always succeeds.
				
   StgPtr allocateLocal(Capability *cap, nat n)
                                Allocates memory from the nursery in
				the current Capability.  This can be
				done without taking a global lock,
                                unlike allocate().

   StgPtr allocatePinned(nat n) Allocates a chunk of contiguous store
   				n words long, which is at a fixed
				address (won't be moved by GC).  
				Returns a pointer to the first word.
				Always succeeds.
				
				NOTE: the GC can't in general handle
				pinned objects, so allocatePinned()
				can only be used for ByteArrays at the
				moment.

				Don't forget to TICK_ALLOC_XXX(...)
				after calling allocate or
				allocatePinned, for the
				benefit of the ticky-ticky profiler.

   rtsBool doYouWantToGC(void)  Returns True if the storage manager is
   				ready to perform a GC, False otherwise.

   lnat  allocatedBytes(void)  Returns the number of bytes allocated
                                via allocate() since the last GC.
				Used in the reporting of statistics.

   -------------------------------------------------------------------------- */

extern StgPtr  allocate        ( nat n );
extern StgPtr  allocateLocal   ( Capability *cap, nat n );
extern StgPtr  allocatePinned  ( nat n );
extern lnat    allocatedBytes  ( void );

extern bdescr * RTS_VAR(small_alloc_list);
extern bdescr * RTS_VAR(large_alloc_list);
extern bdescr * RTS_VAR(pinned_object_block);

extern StgPtr RTS_VAR(alloc_Hp);
extern StgPtr RTS_VAR(alloc_HpLim);

extern nat RTS_VAR(alloc_blocks);
extern nat RTS_VAR(alloc_blocks_lim);

INLINE_HEADER rtsBool
doYouWantToGC( void )
{
  return (alloc_blocks >= alloc_blocks_lim);
}

/* memory allocator for executable memory */
extern void *allocateExec (nat bytes);
extern void freeExec (void *p);

/* -----------------------------------------------------------------------------
   Performing Garbage Collection

   GarbageCollect(get_roots)    Performs a garbage collection.  
				'get_roots' is called to find all the 
				roots that the system knows about.

   StgClosure 			Called by get_roots on each root.	
   MarkRoot(StgClosure *p)	Returns the new location of the root.
   -------------------------------------------------------------------------- */

extern void GarbageCollect(rtsBool force_major_gc);

/* -----------------------------------------------------------------------------
   Generational garbage collection support

   recordMutable(StgPtr p)       Informs the garbage collector that a
				 previously immutable object has
				 become (permanently) mutable.  Used
				 by thawArray and similar.

   updateWithIndirection(p1,p2)  Updates the object at p1 with an
				 indirection pointing to p2.  This is
				 normally called for objects in an old
				 generation (>0) when they are updated.

   updateWithPermIndirection(p1,p2)  As above but uses a permanent indir.

   -------------------------------------------------------------------------- */

/*
 * Storage manager mutex
 */
#if defined(THREADED_RTS)
extern Mutex sm_mutex;
extern Mutex atomic_modify_mutvar_mutex;
#endif

#if defined(THREADED_RTS)
#define ACQUIRE_SM_LOCK   ACQUIRE_LOCK(&sm_mutex);
#define RELEASE_SM_LOCK   RELEASE_LOCK(&sm_mutex);
#define ASSERT_SM_LOCK()  ASSERT_LOCK_HELD(&sm_mutex);
#else
#define ACQUIRE_SM_LOCK
#define RELEASE_SM_LOCK
#define ASSERT_SM_LOCK()
#endif

INLINE_HEADER void
recordMutableGen(StgClosure *p, generation *gen)
{
    bdescr *bd;

    bd = gen->mut_list;
    if (bd->free >= bd->start + BLOCK_SIZE_W) {
	bdescr *new_bd;
	new_bd = allocBlock();
	new_bd->link = bd;
	bd = new_bd;
	gen->mut_list = bd;
    }
    *bd->free++ = (StgWord)p;

}

INLINE_HEADER void
recordMutableGenLock(StgClosure *p, generation *gen)
{
    ACQUIRE_SM_LOCK;
    recordMutableGen(p,gen);
    RELEASE_SM_LOCK;
}

INLINE_HEADER void
recordMutable(StgClosure *p)
{
    bdescr *bd;
    ASSERT(closure_MUTABLE(p));
    bd = Bdescr((P_)p);
    if (bd->gen_no > 0) recordMutableGen(p, &RTS_DEREF(generations)[bd->gen_no]);
}

INLINE_HEADER void
recordMutableLock(StgClosure *p)
{
    ACQUIRE_SM_LOCK;
    recordMutable(p);
    RELEASE_SM_LOCK;
}

/* -----------------------------------------------------------------------------
   The CAF table - used to let us revert CAFs in GHCi
   -------------------------------------------------------------------------- */

/* set to disable CAF garbage collection in GHCi. */
/* (needed when dynamic libraries are used). */
extern rtsBool keepCAFs;

/* -----------------------------------------------------------------------------
   This is the write barrier for MUT_VARs, a.k.a. IORefs.  A
   MUT_VAR_CLEAN object is not on the mutable list; a MUT_VAR_DIRTY
   is.  When written to, a MUT_VAR_CLEAN turns into a MUT_VAR_DIRTY
   and is put on the mutable list.
   -------------------------------------------------------------------------- */

void dirty_MUT_VAR(StgRegTable *reg, StgClosure *p);

/* -----------------------------------------------------------------------------
   DEBUGGING predicates for pointers

   LOOKS_LIKE_INFO_PTR(p)    returns False if p is definitely not an info ptr
   LOOKS_LIKE_CLOSURE_PTR(p) returns False if p is definitely not a closure ptr

   These macros are complete but not sound.  That is, they might
   return false positives.  Do not rely on them to distinguish info
   pointers from closure pointers, for example.

   We don't use address-space predicates these days, for portability
   reasons, and the fact that code/data can be scattered about the
   address space in a dynamically-linked environment.  Our best option
   is to look at the alleged info table and see whether it seems to
   make sense...
   -------------------------------------------------------------------------- */

#define LOOKS_LIKE_INFO_PTR(p) \
   (p && ((StgInfoTable *)(INFO_PTR_TO_STRUCT(p)))->type != INVALID_OBJECT && \
    ((StgInfoTable *)(INFO_PTR_TO_STRUCT(p)))->type < N_CLOSURE_TYPES)

#define LOOKS_LIKE_CLOSURE_PTR(p) \
  (LOOKS_LIKE_INFO_PTR((UNTAG_CLOSURE((StgClosure *)(p)))->header.info))

/* -----------------------------------------------------------------------------
   Macros for calculating how big a closure will be (used during allocation)
   -------------------------------------------------------------------------- */

INLINE_HEADER StgOffset PAP_sizeW   ( nat n_args )
{ return sizeofW(StgPAP) + n_args; }

INLINE_HEADER StgOffset AP_sizeW   ( nat n_args )
{ return sizeofW(StgAP) + n_args; }

INLINE_HEADER StgOffset AP_STACK_sizeW ( nat size )
{ return sizeofW(StgAP_STACK) + size; }

INLINE_HEADER StgOffset CONSTR_sizeW( nat p, nat np )
{ return sizeofW(StgHeader) + p + np; }

INLINE_HEADER StgOffset THUNK_SELECTOR_sizeW ( void )
{ return sizeofW(StgSelector); }

INLINE_HEADER StgOffset BLACKHOLE_sizeW ( void )
{ return sizeofW(StgHeader)+MIN_PAYLOAD_SIZE; }

/* --------------------------------------------------------------------------
   Sizes of closures
   ------------------------------------------------------------------------*/

INLINE_HEADER StgOffset sizeW_fromITBL( const StgInfoTable* itbl ) 
{ return sizeofW(StgClosure) 
       + sizeofW(StgPtr)  * itbl->layout.payload.ptrs 
       + sizeofW(StgWord) * itbl->layout.payload.nptrs; }

INLINE_HEADER StgOffset thunk_sizeW_fromITBL( const StgInfoTable* itbl ) 
{ return sizeofW(StgThunk) 
       + sizeofW(StgPtr)  * itbl->layout.payload.ptrs 
       + sizeofW(StgWord) * itbl->layout.payload.nptrs; }

INLINE_HEADER StgOffset ap_stack_sizeW( StgAP_STACK* x )
{ return AP_STACK_sizeW(x->size); }

INLINE_HEADER StgOffset ap_sizeW( StgAP* x )
{ return AP_sizeW(x->n_args); }

INLINE_HEADER StgOffset pap_sizeW( StgPAP* x )
{ return PAP_sizeW(x->n_args); }

INLINE_HEADER StgOffset arr_words_sizeW( StgArrWords* x )
{ return sizeofW(StgArrWords) + x->words; }

INLINE_HEADER StgOffset mut_arr_ptrs_sizeW( StgMutArrPtrs* x )
{ return sizeofW(StgMutArrPtrs) + x->ptrs; }

INLINE_HEADER StgWord tso_sizeW ( StgTSO *tso )
{ return TSO_STRUCT_SIZEW + tso->stack_size; }

#ifdef ALLOW_INTERPRETER
INLINE_HEADER StgWord bco_sizeW ( StgBCO *bco )
{ return bco->size; }
#endif // ALLOW_INTERPRETER

INLINE_HEADER nat
closure_sizeW_ (StgClosure *p, StgInfoTable *info)
{
    switch (info->type) {
    case THUNK_0_1:
    case THUNK_1_0:
	return sizeofW(StgThunk) + 1;
    case FUN_0_1:
    case CONSTR_0_1:
    case FUN_1_0:
    case CONSTR_1_0:
	return sizeofW(StgHeader) + 1;
    case THUNK_0_2:
    case THUNK_1_1:
    case THUNK_2_0:
	return sizeofW(StgThunk) + 2;
    case FUN_0_2:
    case CONSTR_0_2:
    case FUN_1_1:
    case CONSTR_1_1:
    case FUN_2_0:
    case CONSTR_2_0:
	return sizeofW(StgHeader) + 2;
    case THUNK:
	return thunk_sizeW_fromITBL(info);
    case THUNK_SELECTOR:
	return THUNK_SELECTOR_sizeW();
    case AP_STACK:
	return ap_stack_sizeW((StgAP_STACK *)p);
    case AP:
	return ap_sizeW((StgAP *)p);
    case PAP:
	return pap_sizeW((StgPAP *)p);
    case IND:
    case IND_PERM:
    case IND_OLDGEN:
    case IND_OLDGEN_PERM:
	return sizeofW(StgInd);
    case ARR_WORDS:
	return arr_words_sizeW((StgArrWords *)p);
    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
	return mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
    case TSO:
	return tso_sizeW((StgTSO *)p);
#ifdef ALLOW_INTERPRETER
    case BCO:
	return bco_sizeW((StgBCO *)p);
#endif
    case TVAR_WATCH_QUEUE:
        return sizeofW(StgTVarWatchQueue);
    case TVAR:
        return sizeofW(StgTVar);
    case TREC_CHUNK:
        return sizeofW(StgTRecChunk);
    case TREC_HEADER:
        return sizeofW(StgTRecHeader);
    case ATOMIC_INVARIANT:
        return sizeofW(StgAtomicInvariant);
    case INVARIANT_CHECK_QUEUE:
        return sizeofW(StgInvariantCheckQueue);
    default:
	return sizeW_fromITBL(info);
    }
}

// The definitive way to find the size, in words, of a heap-allocated closure
INLINE_HEADER nat
closure_sizeW (StgClosure *p)
{
    return closure_sizeW_(p, get_itbl(p));
}

/* -----------------------------------------------------------------------------
   Sizes of stack frames
   -------------------------------------------------------------------------- */

INLINE_HEADER StgWord stack_frame_sizeW( StgClosure *frame )
{
    StgRetInfoTable *info;

    info = get_ret_itbl(frame);
    switch (info->i.type) {

    case RET_DYN:
    {
	StgRetDyn *dyn = (StgRetDyn *)frame;
	return  sizeofW(StgRetDyn) + RET_DYN_BITMAP_SIZE + 
	    RET_DYN_NONPTR_REGS_SIZE +
	    RET_DYN_PTRS(dyn->liveness) + RET_DYN_NONPTRS(dyn->liveness);
    }
	    
    case RET_FUN:
	return sizeofW(StgRetFun) + ((StgRetFun *)frame)->size;

    case RET_BIG:
	return 1 + GET_LARGE_BITMAP(&info->i)->size;

#ifdef ALLOW_INTERPRETER
    case RET_BCO:
	return 2 + BCO_BITMAP_SIZE((StgBCO *)((P_)frame)[1]);
#endif

    default:
	return 1 + BITMAP_SIZE(info->i.layout.bitmap);
    }
}

/* -----------------------------------------------------------------------------
   Nursery manipulation
   -------------------------------------------------------------------------- */

extern void     allocNurseries       ( void );
extern void     resetNurseries       ( void );
extern void     resizeNurseries      ( nat blocks );
extern void     resizeNurseriesFixed ( nat blocks );
extern void     tidyAllocateLists    ( void );
extern lnat     countNurseryBlocks   ( void );

/* -----------------------------------------------------------------------------
   Functions from GC.c 
   -------------------------------------------------------------------------- */

typedef void (*evac_fn)(StgClosure **);

extern void         threadPaused ( Capability *cap, StgTSO * );
extern StgClosure * isAlive      ( StgClosure *p );
extern void         markCAFs     ( evac_fn evac );

/* -----------------------------------------------------------------------------
   Stats 'n' DEBUG stuff
   -------------------------------------------------------------------------- */

extern ullong RTS_VAR(total_allocated);

extern lnat calcAllocated  ( void );
extern lnat calcLive       ( void );
extern lnat calcNeeded     ( void );

#if defined(DEBUG)
extern void memInventory(void);
extern void checkSanity(void);
extern nat  countBlocks(bdescr *);
extern void checkNurserySanity( step *stp );
#endif

#if defined(DEBUG)
void printMutOnceList(generation *gen);
void printMutableList(generation *gen);
#endif

/* ----------------------------------------------------------------------------
   Storage manager internal APIs and globals
   ------------------------------------------------------------------------- */

#define END_OF_STATIC_LIST stgCast(StgClosure*,1)

extern void newDynCAF(StgClosure *);

extern void move_TSO(StgTSO *src, StgTSO *dest);
extern StgTSO *relocate_stack(StgTSO *dest, ptrdiff_t diff);

extern StgClosure * RTS_VAR(scavenged_static_objects);
extern StgWeak    * RTS_VAR(old_weak_ptr_list);
extern StgWeak    * RTS_VAR(weak_ptr_list);
extern StgClosure * RTS_VAR(caf_list);
extern StgClosure * RTS_VAR(revertible_caf_list);
extern StgTSO     * RTS_VAR(resurrected_threads);

#endif /* STORAGE_H */
