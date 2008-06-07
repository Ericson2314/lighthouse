/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2006
 *
 * The scheduler and thread-related functionality
 *
 * --------------------------------------------------------------------------*/

#include "PosixSource.h"
#define KEEP_LOCKCLOSURE
#include "Rts.h"
#include "SchedAPI.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "OSThreads.h"
#include "Storage.h"
#include "StgRun.h"
#include "Hooks.h"
#include "Schedule.h"
#include "StgMiscClosures.h"
#include "Interpreter.h"
#include "Printer.h"
#include "RtsSignals.h"
#include "Sanity.h"
#include "Stats.h"
#include "Timer.h"
#include "TLS.h"
#include "Prelude.h"
#include "ThreadLabels.h"
#include "LdvProfile.h"
#include "Updates.h"
#include "Proftimer.h"
#include "ProfHeap.h"
#include "Sparks.h"
#include "Capability.h"
#include "AwaitEvent.h"
#if defined(mingw32_HOST_OS)
#include "win32/IOManager.h"
#endif
#include "Trace.h"
#include "RaiseAsync.h"
#include "Threads.h"
#include "ThrIOManager.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

// Turn off inlining when debugging - it obfuscates things
#ifdef DEBUG
# undef  STATIC_INLINE
# define STATIC_INLINE static
#endif

/* -----------------------------------------------------------------------------
 * Global variables
 * -------------------------------------------------------------------------- */

/* Threads blocked on blackholes.
 * LOCK: sched_mutex+capability, or all capabilities
 */
StgTSO *blackhole_queue = NULL;

/* The blackhole_queue should be checked for threads to wake up.  See
 * Schedule.h for more thorough comment.
 * LOCK: none (doesn't matter if we miss an update)
 */
rtsBool blackholes_need_checking = rtsFalse;

/* flag that tracks whether we have done any execution in this time slice.
 * LOCK: currently none, perhaps we should lock (but needs to be
 * updated in the fast path of the scheduler).
 */
nat recent_activity = ACTIVITY_YES;

/*  This is used in `TSO.h' and gcc 2.96 insists that this variable actually 
 *  exists - earlier gccs apparently didn't.
 *  -= chak
 */
StgTSO dummy_tso;

/*
 * Set to TRUE when entering a shutdown state (via shutdownHaskellAndExit()) --
 * in an MT setting, needed to signal that a worker thread shouldn't hang around
 * in the scheduler when it is out of work.
 */
rtsBool shutting_down_scheduler = rtsFalse;

#if !defined(mingw32_HOST_OS) && !defined(house_HOST_OS)
#error "FOO!"
#define FORKPROCESS_PRIMOP_SUPPORTED
#endif

/* -----------------------------------------------------------------------------
 * static function prototypes
 * -------------------------------------------------------------------------- */

//
// These function all encapsulate parts of the scheduler loop, and are
// abstracted only to make the structure and control flow of the
// scheduler clearer.
//
static void scheduleStartSignalHandlers (Capability *cap);
static void scheduleCheckBlackHoles (Capability *cap);
static Capability *scheduleHandleHeapOverflow(Capability *cap);
static void scheduleHandleStackOverflow(Capability *cap);
static void scheduleDoGC(Capability *cap, rtsBool force_major);

static rtsBool checkBlackHoles(Capability *cap);

static StgTSO *threadStackOverflow(Capability *cap, StgTSO *tso);

static void deleteThread (Capability *cap, StgTSO *tso);

#ifdef FORKPROCESS_PRIMOP_SUPPORTED
static void deleteThread_(Capability *cap, StgTSO *tso);
#endif

#ifdef DEBUG
static char *whatNext_strs[] = {
  "(unknown)",
  "ThreadRunGHC",
  "ThreadInterpret",
  "ThreadKilled",
  "ThreadRelocated",
  "ThreadComplete"
};
#endif

/* ----------------------------------------------------------------------------
 * Start any pending signal handlers
 * ------------------------------------------------------------------------- */
#if defined(RTS_USER_SIGNALS) && !defined(THREADED_RTS)
static void
scheduleStartSignalHandlers(Capability *cap)
{
    if (RtsFlags.MiscFlags.install_signal_handlers && signals_pending()) {
        // safe outside the lock
	startSignalHandlers(cap);
    }
}
#else
static void
scheduleStartSignalHandlers(Capability *cap STG_UNUSED)
{
}
#endif

/* ----------------------------------------------------------------------------
 * Check for threads blocked on BLACKHOLEs that can be woken up
 * ------------------------------------------------------------------------- */
static void
scheduleCheckBlackHoles (Capability *cap)
{
    if ( blackholes_need_checking ) // check without the lock first
    {
	ACQUIRE_LOCK(&sched_mutex);
	if ( blackholes_need_checking ) {
	    checkBlackHoles(cap);
	    blackholes_need_checking = rtsFalse;
	}
	RELEASE_LOCK(&sched_mutex);
    }
}

/* -----------------------------------------------------------------------------
 * Handle a thread that returned to the scheduler with ThreadHeepOverflow
 * -------------------------------------------------------------------------- */

static Capability *
scheduleHandleHeapOverflow(Capability *cap)
{
    StgTSO *t = cap->r.rCurrentTSO;
    // did the thread ask for a large block?
    if (cap->r.rHpAlloc > BLOCK_SIZE) {
	// if so, get one and push it on the front of the nursery.
	bdescr *bd;
	lnat blocks;
	
	blocks = (lnat)BLOCK_ROUND_UP(cap->r.rHpAlloc) / BLOCK_SIZE;
	
	debugTrace(DEBUG_sched,
		   "--<< thread %ld (%s) stopped: requesting a large block (size %ld)\n", 
		   (long)t->id, whatNext_strs[t->what_next], blocks);
    
	// don't do this if the nursery is (nearly) full, we'll GC first.
	if (cap->r.rCurrentNursery->link != NULL ||
	    cap->r.rNursery->n_blocks == 1) {  // paranoia to prevent infinite loop
	                                       // if the nursery has only one block.
	    
	    ACQUIRE_SM_LOCK
	    bd = allocGroup( blocks );
	    RELEASE_SM_LOCK
	    cap->r.rNursery->n_blocks += blocks;
	    
	    // link the new group into the list
	    bd->link = cap->r.rCurrentNursery;
	    bd->u.back = cap->r.rCurrentNursery->u.back;
	    if (cap->r.rCurrentNursery->u.back != NULL) {
		cap->r.rCurrentNursery->u.back->link = bd;
	    } else {
#if !defined(THREADED_RTS)
		ASSERT(g0s0->blocks == cap->r.rCurrentNursery &&
		       g0s0 == cap->r.rNursery);
#endif
		cap->r.rNursery->blocks = bd;
	    }		  
	    cap->r.rCurrentNursery->u.back = bd;
	    
	    // initialise it as a nursery block.  We initialise the
	    // step, gen_no, and flags field of *every* sub-block in
	    // this large block, because this is easier than making
	    // sure that we always find the block head of a large
	    // block whenever we call Bdescr() (eg. evacuate() and
	    // isAlive() in the GC would both have to do this, at
	    // least).
	    { 
		bdescr *x;
		for (x = bd; x < bd + blocks; x++) {
		    x->step = cap->r.rNursery;
		    x->gen_no = 0;
		    x->flags = 0;
		}
	    }
	    
	    // This assert can be a killer if the app is doing lots
	    // of large block allocations.
	    IF_DEBUG(sanity, checkNurserySanity(cap->r.rNursery));
	    
	    // now update the nursery to point to the new block
	    cap->r.rCurrentNursery = bd;
	    
	    return cap; /* not actually GC'ing */
	}
    }
    
    debugTrace(DEBUG_sched,
	       "--<< thread %ld (%s) stopped: HeapOverflow\n", 
	       (long)t->id, whatNext_strs[t->what_next]);

    scheduleDoGC(cap, rtsFalse);
    return cap;
}

/* -----------------------------------------------------------------------------
 * Handle a thread that returned to the scheduler with ThreadStackOverflow
 * -------------------------------------------------------------------------- */

static void
scheduleHandleStackOverflow (Capability *cap)
{
    StgTSO *t = cap->r.rCurrentTSO;
    debugTrace (DEBUG_sched,
		"--<< thread %ld (%s) stopped, StackOverflow", 
		(long)t->id, whatNext_strs[t->what_next]);

    /* enlarge the stack */
    cap->r.rCurrentTSO = threadStackOverflow(cap, t);
}

/* -----------------------------------------------------------------------------
 * Perform a garbage collection if necessary
 * -------------------------------------------------------------------------- */

static void
scheduleDoGC (Capability *cap, rtsBool force_major)
{
    // so this happens periodically:
    if (cap) scheduleCheckBlackHoles(cap);
    
    /* everybody back, start the GC.
     * Could do it in this thread, or signal a condition var
     * to do it in another thread.  Either way, we need to
     * broadcast on gc_pending_cond afterward.
     */
    GarbageCollect(force_major);
    
    return cap;
}

void
runTheWorld (HaskellObj closure)
{
    Capability *cap = &MainCapability;

    cap->r.rCurrentTSO = createIOThread(cap, RtsFlags.GcFlags.initialStkSize, closure);

    cap->r.rCurrentTSO->cap = cap;

    while (rtsTrue) {
        StgThreadReturnCode ret;

        dirtyTSO(cap->r.rCurrentTSO);
        switch (cap->r.rCurrentTSO->what_next) {
            case ThreadKilled:
            case ThreadComplete:
                debugTrace(DEBUG_sched, "RTW[%x]: what_next was ThreadKilled or ThreadComplete\n", cap->r.rCurrentTSO);
                ret = ThreadFinished;
                break;
            case ThreadRunGHC:
            {
                debugTrace(DEBUG_sched, "RTW[%x]: what_next was ThreadRunGHC\n", cap->r.rCurrentTSO);
                StgRegTable *r;
                r = StgRun((StgFunPtr) stg_returnToStackTop, &cap->r);
                cap = regTableToCapability(r);
                ret = r->rRet;
                break;
            }
            default:
                barf("runTheWorld: invalid what_next field\n");
        }

        switch (ret)
        {
            case HeapOverflow:
                debugTrace(DEBUG_sched, "RTW[%x]: ret was HeapOverflow\n", cap->r.rCurrentTSO);
                cap = scheduleHandleHeapOverflow(cap);
                break;
            case StackOverflow:
                debugTrace(DEBUG_sched, "RTW[%x]: ret was StackOverflow -> ", cap->r.rCurrentTSO);
                scheduleHandleStackOverflow(cap);
                debugTrace(DEBUG_sched, "handled: TSO is now %x\n; ", cap->r.rCurrentTSO);
                break;
            case ThreadFinished:
                // Returning with ThreadFinished is a very dangerous thing...
                // Unlike in the old world, we don't get a new thread from a
                // run queue and schedule it...we just stop.  The Haskell land
                // scheduler should always take care of switching to a new
                // thread when a forked thread is about to die.
                //
                // For now, this corresponds to shutting down...but it gives
                // a warning because some code (like exceptions) likely hasn't
                // been properly converted yet.
                barf("WARNING: Thread %x returned with ThreadFinished\n", cap->r.rCurrentTSO);
                //exit(0);
            case ThreadBlocked:
                barf("runTheWorld: got ThreadBlocked return status...should never happen!\n");
            default:
                barf("runTheWorld: invalid thread return code %d", (int)ret);
        }
    }
}

void *
suspendThread (StgRegTable *reg)
{
    debugTrace(DEBUG_sched, "HEC %p makes an out-call, deactivated\n", regTableToCapability(reg));
    return reg;
}

StgRegTable *
resumeThread (void *reg)
{
    dirtyTSO(((StgRegTable*)reg)->rCurrentTSO);
    debugTrace(DEBUG_sched, "HEC %p returns from an out-call, activated\n", regTableToCapability(reg));
    return reg;
}

/* ---------------------------------------------------------------------------
 * initScheduler()
 *
 * Initialise the scheduler.  This resets all the queues - if the
 * queues contained any threads, they'll be garbage collected at the
 * next pass.
 *
 * ------------------------------------------------------------------------ */

void 
initScheduler(void)
{
  blackhole_queue   = END_TSO_QUEUE;

  recent_activity = ACTIVITY_YES;

  ACQUIRE_LOCK(&sched_mutex);

  /* A capability holds the state a native thread needs in
   * order to execute STG code. At least one capability is
   * floating around (only THREADED_RTS builds have more than one).
   */
  initCapabilities();

  trace(TRACE_sched, "start: %d capabilities", n_capabilities);

  RELEASE_LOCK(&sched_mutex);
}

void
exitScheduler(
    rtsBool wait_foreign
#if !defined(THREADED_RTS)
                         __attribute__((unused))
#endif
)
               /* see Capability.c, shutdownCapability() */
{
    scheduleDoGC(NULL,rtsFalse);

    freeCapability(&MainCapability);
}

/* ---------------------------------------------------------------------------
   Where are the roots that we know about?

	- all the thread currently executing a _ccall_GC
        - all the "main threads"
     
   ------------------------------------------------------------------------ */

/* This has to be protected either by the scheduler monitor, or by the
	garbage collection monitor (probably the latter).
	KH @ 25/10/99
*/

void
GetRoots( evac_fn evac )
{
    nat i;
    Capability *cap;

    // Evacuate the TLS
    GetTLSRoots(evac);

    // Evacuate the interrupt handler...ensures top-level globals like the
    // run queue don't get garbage collected. :)
    StgClosure* temp = &KernelziInterrupts_interruptHandler_closure;
    evac(&temp);

    for (i = 0; i < n_capabilities; i++) {
	cap = &capabilities[i];
        temp = &cap->r.rCurrentTSO;
        evac(temp);
    }
}

/* -----------------------------------------------------------------------------
   performGC

   This is the interface to the garbage collector from Haskell land.
   We provide this so that external C code can allocate and garbage
   collect when called from Haskell via _ccall_GC.
   -------------------------------------------------------------------------- */

void
performGC(void)
{
    scheduleDoGC(NULL, rtsFalse);
}

void
performMajorGC(void)
{
    scheduleDoGC(NULL, rtsTrue);
}

/* -----------------------------------------------------------------------------
   Stack overflow

   If the thread has reached its maximum stack size, then raise the
   StackOverflow exception in the offending thread.  Otherwise
   relocate the TSO into a larger chunk of memory and adjust its stack
   size appropriately.
   -------------------------------------------------------------------------- */

static StgTSO *
threadStackOverflow(Capability *cap, StgTSO *tso)
{
  nat new_stack_size, stack_words;
  lnat new_tso_size;
  StgPtr new_sp;
  StgTSO *dest;

  IF_DEBUG(sanity,checkTSO(tso));

  // don't allow throwTo() to modify the blocked_exceptions queue
  // while we are moving the TSO:
  lockClosure((StgClosure *)tso);

  if (tso->stack_size >= tso->max_stack_size && !(tso->flags & TSO_BLOCKEX)) {
      // NB. never raise a StackOverflow exception if the thread is
      // inside Control.Exceptino.block.  It is impractical to protect
      // against stack overflow exceptions, since virtually anything
      // can raise one (even 'catch'), so this is the only sensible
      // thing to do here.  See bug #767.

      debugTrace(DEBUG_gc,
		 "threadStackOverflow of TSO %ld (%p): stack too large (now %ld; max is %ld)",
		 (long)tso->id, tso, (long)tso->stack_size, (long)tso->max_stack_size);
      IF_DEBUG(gc,
	       /* If we're debugging, just print out the top of the stack */
	       printStackChunk(tso->sp, stg_min(tso->stack+tso->stack_size, 
						tso->sp+64)));

      debugBelch("Out of stack space...trying to throw exception\n");
      // Send this thread the StackOverflow exception
      unlockTSO(tso);
      throwToSingleThreaded(cap, tso, (StgClosure *)stackOverflow_closure);
      return tso;
  }

  /* Try to double the current stack size.  If that takes us over the
   * maximum stack size for this thread, then use the maximum instead.
   * Finally round up so the TSO ends up as a whole number of blocks.
   */
  new_stack_size = stg_min(tso->stack_size * 2, tso->max_stack_size);
  new_tso_size   = (lnat)BLOCK_ROUND_UP(new_stack_size * sizeof(W_) + 
				       TSO_STRUCT_SIZE)/sizeof(W_);
  new_tso_size = round_to_mblocks(new_tso_size);  /* Be MBLOCK-friendly */
  new_stack_size = new_tso_size - TSO_STRUCT_SIZEW;

  debugTrace(DEBUG_sched, 
	     "increasing stack size from %ld words to %d.",
	     (long)tso->stack_size, new_stack_size);

  dest = (StgTSO *)allocate(new_tso_size);
  TICK_ALLOC_TSO(new_stack_size,0);

  /* copy the TSO block and the old stack into the new area */
  memcpy(dest,tso,TSO_STRUCT_SIZE);
  stack_words = tso->stack + tso->stack_size - tso->sp;
  new_sp = (P_)dest + new_tso_size - stack_words;
  memcpy(new_sp, tso->sp, stack_words * sizeof(W_));

  /* relocate the stack pointers... */
  dest->sp         = new_sp;
  dest->stack_size = new_stack_size;
	
  /* Mark the old TSO as relocated.  We have to check for relocated
   * TSOs in the garbage collector and any primops that deal with TSOs.
   *
   * It's important to set the sp value to just beyond the end
   * of the stack, so we don't attempt to scavenge any part of the
   * dead TSO's stack.
   */
  tso->what_next = ThreadRelocated;
  tso->link = dest;
  tso->sp = (P_)&(tso->stack[tso->stack_size]);
  tso->why_blocked = NotBlocked;

  IF_PAR_DEBUG(verbose,
	       debugBelch("@@ threadStackOverflow of TSO %d (now at %p): stack size increased to %ld\n",
		     tso->id, tso, tso->stack_size);
	       /* If we're debugging, just print out the top of the stack */
	       printStackChunk(tso->sp, stg_min(tso->stack+tso->stack_size, 
						tso->sp+64)));
  
  unlockTSO(dest);
  unlockTSO(tso);

  IF_DEBUG(sanity,checkTSO(dest));
#if 0
  IF_DEBUG(scheduler,printTSO(dest));
#endif

  return dest;
}

/* -----------------------------------------------------------------------------
 * checkBlackHoles()
 *
 * Check the blackhole_queue for threads that can be woken up.  We do
 * this periodically: before every GC, and whenever the run queue is
 * empty.
 *
 * An elegant solution might be to just wake up all the blocked
 * threads with awakenBlockedQueue occasionally: they'll go back to
 * sleep again if the object is still a BLACKHOLE.  Unfortunately this
 * doesn't give us a way to tell whether we've actually managed to
 * wake up any threads, so we would be busy-waiting.
 *
 * -------------------------------------------------------------------------- */

static rtsBool
checkBlackHoles (Capability *cap)
{
    StgTSO **prev, *t;
    rtsBool any_woke_up = rtsFalse;
    StgHalfWord type;

    // blackhole_queue is global:
    ASSERT_LOCK_HELD(&sched_mutex);

    debugTrace(DEBUG_sched, "checking threads blocked on black holes");

    // ASSUMES: sched_mutex
    prev = &blackhole_queue;
    t = blackhole_queue;
    while (t != END_TSO_QUEUE) {
	ASSERT(t->why_blocked == BlockedOnBlackHole);
	type = get_itbl(t->block_info.closure)->type;
	if (type != BLACKHOLE && type != CAF_BLACKHOLE) {
	    IF_DEBUG(sanity,checkTSO(t));
	    t = unblockOne(cap, t);
	    // urk, the threads migrate to the current capability
	    // here, but we'd like to keep them on the original one.
	    *prev = t;
	    any_woke_up = rtsTrue;
	} else {
	    prev = &t->link;
	    t = t->link;
	}
    }

    return any_woke_up;
}

/* -----------------------------------------------------------------------------
   raiseExceptionHelper
   
   This function is called by the raise# primitve, just so that we can
   move some of the tricky bits of raising an exception from C-- into
   C.  Who knows, it might be a useful re-useable thing here too.
   -------------------------------------------------------------------------- */

StgWord
raiseExceptionHelper (StgRegTable *reg, StgTSO *tso, StgClosure *exception)
{
    Capability *cap = regTableToCapability(reg);
    StgThunk *raise_closure = NULL;
    StgPtr p, next;
    StgRetInfoTable *info;
    //
    // This closure represents the expression 'raise# E' where E
    // is the exception raise.  It is used to overwrite all the
    // thunks which are currently under evaluataion.
    //

    // OLD COMMENT (we don't have MIN_UPD_SIZE now):
    // LDV profiling: stg_raise_info has THUNK as its closure
    // type. Since a THUNK takes at least MIN_UPD_SIZE words in its
    // payload, MIN_UPD_SIZE is more approprate than 1.  It seems that
    // 1 does not cause any problem unless profiling is performed.
    // However, when LDV profiling goes on, we need to linearly scan
    // small object pool, where raise_closure is stored, so we should
    // use MIN_UPD_SIZE.
    //
    // raise_closure = (StgClosure *)RET_STGCALL1(P_,allocate,
    // 				       sizeofW(StgClosure)+1);
    //

    //
    // Walk up the stack, looking for the catch frame.  On the way,
    // we update any closures pointed to from update frames with the
    // raise closure that we just built.
    //
    p = tso->sp;
    while(1) {
	info = get_ret_itbl((StgClosure *)p);
	next = p + stack_frame_sizeW((StgClosure *)p);
	switch (info->i.type) {
	    
	case UPDATE_FRAME:
	    // Only create raise_closure if we need to.
	    if (raise_closure == NULL) {
		raise_closure = 
		    (StgThunk *)allocateLocal(cap,sizeofW(StgThunk)+1);
		SET_HDR(raise_closure, &stg_raise_info, CCCS);
		raise_closure->payload[0] = exception;
	    }
	    UPD_IND(((StgUpdateFrame *)p)->updatee,(StgClosure *)raise_closure);
	    p = next;
	    continue;

	case CATCH_FRAME:
	    tso->sp = p;
	    return CATCH_FRAME;

	case STOP_FRAME:
	    tso->sp = p;
	    return STOP_FRAME;

	default:
	    p = next; 
	    continue;
	}
    }
}

