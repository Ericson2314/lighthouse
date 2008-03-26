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
#include "STM.h"
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
#include "Task.h"
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

/* Linked list of all threads.
 * Used for detecting garbage collected threads.
 * LOCK: sched_mutex+capability, or all capabilities
 */
StgTSO *all_threads = NULL;

/* flag set by signal handler to precipitate a context switch
 * LOCK: none (just an advisory flag)
 */
int context_switch = 0; /* KAYDEN: Should rename to something indicating
                           "please do a timer interrupt" */

/* flag that tracks whether we have done any execution in this time slice.
 * LOCK: currently none, perhaps we should lock (but needs to be
 * updated in the fast path of the scheduler).
 */
nat recent_activity = ACTIVITY_YES;

/* if this flag is set as well, give up execution
 * LOCK: none (changes once, from false->true)
 */
rtsBool sched_state = SCHED_RUNNING;

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

static Capability *schedule (Capability *initialCapability, Task *task);

//
// These function all encapsulate parts of the scheduler loop, and are
// abstracted only to make the structure and control flow of the
// scheduler clearer.
//
static void scheduleStartSignalHandlers (Capability *cap);
static void scheduleCheckBlackHoles (Capability *cap);
static rtsBool scheduleHandleHeapOverflow( Capability *cap, StgTSO *t );
static void scheduleHandleStackOverflow( Capability *cap, Task *task, 
					 StgTSO *t);
static rtsBool scheduleHandleThreadFinished( Capability *cap, Task *task,
					     StgTSO *t );
static Capability *scheduleDoGC(Capability *cap, Task *task,
				rtsBool force_major);

static rtsBool checkBlackHoles(Capability *cap);

static StgTSO *threadStackOverflow(Capability *cap, StgTSO *tso);

static void deleteThread (Capability *cap, StgTSO *tso);
static void deleteAllThreads (Capability *cap);

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

/* ---------------------------------------------------------------------------
   Main scheduling loop.

   We use round-robin scheduling, each thread returning to the
   scheduler loop when one of these conditions is detected:

      * out of heap space
      * timer expires (thread yields)
      * thread blocks
      * thread ends
      * stack overflow

   GUM version:
     GUM iterates over incoming messages.
     It starts with nothing to do (thus CurrentTSO == END_TSO_QUEUE),
     and sends out a fish whenever it has nothing to do; in-between
     doing the actual reductions (shared code below) it processes the
     incoming messages and deals with delayed operations 
     (see PendingFetches).
     This is not the ugliest code you could imagine, but it's bloody close.

   ------------------------------------------------------------------------ */

static Capability *
schedule (Capability *initialCapability, Task *task)
{
  StgTSO *t;
  Capability *cap;
  StgThreadReturnCode ret;
  nat prev_what_next;
  rtsBool ready_to_gc;
  
  cap = initialCapability;

  // Pre-condition: this task owns initialCapability.
  // The sched_mutex is *NOT* held
  // NB. on return, we still hold a capability.

  debugTrace (DEBUG_sched, 
	      "### NEW SCHEDULER LOOP (task: %p, cap: %p)",
	      task, initialCapability);

  // -----------------------------------------------------------
  // Scheduler loop starts here:

  while (rtsTrue) {

    // Check whether we have re-entered the RTS from Haskell without
    // going via suspendThread()/resumeThread (i.e. a 'safe' foreign
    // call).
    if (cap->in_haskell) {
    	  errorBelch("schedule: re-entered unsafely.\n"
    		     "   Perhaps a 'foreign import unsafe' should be 'safe'?");
    	  stg_exit(EXIT_FAILURE);
    }

    // The interruption / shutdown sequence.
    // 
    // In order to cleanly shut down the runtime, we want to:
    //   * make sure that all main threads return to their callers
    //     with the state 'Interrupted'.
    //   * clean up all OS threads assocated with the runtime
    //   * free all memory etc.
    //
    // So the sequence for ^C goes like this:
    //
    //   * ^C handler sets sched_state := SCHED_INTERRUPTING and
    //     arranges for some Capability to wake up
    //
    //   * all threads in the system are halted, and the zombies are
    //     placed on the run queue for cleaning up.  We acquire all
    //     the capabilities in order to delete the threads, this is
    //     done by scheduleDoGC() for convenience (because GC already
    //     needs to acquire all the capabilities).  We can't kill
    //     threads involved in foreign calls.
    // 
    //   * somebody calls shutdownHaskell(), which calls exitScheduler()
    //
    //   * sched_state := SCHED_SHUTTING_DOWN
    //
    //   * all workers exit when the run queue on their capability
    //     drains.  All main threads will also exit when their TSO
    //     reaches the head of the run queue and they can return.
    //
    //   * eventually all Capabilities will shut down, and the RTS can
    //     exit.
    //
    //   * We might be left with threads blocked in foreign calls, 
    //     we should really attempt to kill these somehow (TODO);
    
    switch (sched_state) {
    case SCHED_RUNNING:
	break;
    case SCHED_INTERRUPTING:
	debugTrace(DEBUG_sched, "SCHED_INTERRUPTING");
	/* scheduleDoGC() deletes all the threads */
	cap = scheduleDoGC(cap,task,rtsFalse);
	break;
    case SCHED_SHUTTING_DOWN:
	debugTrace(DEBUG_sched, "SCHED_SHUTTING_DOWN");
	// If we are a worker, just exit.  If we're a bound thread
	// then we will exit below when we've removed our TSO from
	// the run queue.
	//if (task->tso == NULL && emptyRunQueue(cap)) {
	    return cap;
	//}
	break;
    default:
	barf("sched_state: %d", sched_state);
    }

    scheduleStartSignalHandlers(cap);

    // Normally, the only way we can get here with no threads to
    // run is if a keyboard interrupt received during 
    // scheduleCheckBlockedThreads() or scheduleDetectDeadlock().
    // Additionally, it is not fatal for the
    // threaded RTS to reach here with no threads to run.
    ASSERT(cap->r.rCurrentTSO && sched_state >= SCHED_INTERRUPTING);

run_thread:
    t = cap->r.rCurrentTSO;
    debugTrace(DEBUG_sched, "-->> running thread %ld %s ...", 
			      (long)t->id, whatNext_strs[t->what_next]);

    startHeapProfTimer();

    // Check for exceptions blocked on this thread
    maybePerformBlockedException (cap, t);

    // ----------------------------------------------------------------------
    // Run the current thread 

    ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);
    ASSERT(t->cap == cap);

    prev_what_next = t->what_next;

    errno = t->saved_errno;
#if mingw32_HOST_OS
    SetLastError(t->saved_winerror);
#endif

    cap->in_haskell = rtsTrue;

    dirtyTSO(t);

    switch (prev_what_next) {
	
    case ThreadKilled:
    case ThreadComplete:
	/* Thread already finished, return to scheduler. */
	ret = ThreadFinished;
	break;
	
    case ThreadRunGHC:
    {
	StgRegTable *r;
	r = StgRun((StgFunPtr) stg_returnToStackTop, &cap->r);
	cap = regTableToCapability(r);
	ret = r->rRet;
	break;
    }
   
#ifdef ALLOW_INTERPRETER
    case ThreadInterpret:
	cap = interpretBCO(cap);
	ret = cap->r.rRet;
	break;
#endif
	
    default:
	barf("schedule: invalid what_next field");
    }

    cap->in_haskell = rtsFalse;

    // The TSO might have moved, eg. if it re-entered the RTS and a GC
    // happened.  So find the new location:
    t = cap->r.rCurrentTSO;

    // We have run some Haskell code: there might be blackhole-blocked
    // threads to wake up now.
    // Lock-free test here should be ok, we're just setting a flag.
    if ( blackhole_queue != END_TSO_QUEUE ) {
	blackholes_need_checking = rtsTrue;
    }
    
    // And save the current errno in this thread.
    // XXX: possibly bogus for SMP because this thread might already
    // be running again, see code below.
    t->saved_errno = errno;
#if mingw32_HOST_OS
    // Similarly for Windows error code
    t->saved_winerror = GetLastError();
#endif

    ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);
    ASSERT(t->cap == cap);

    // ----------------------------------------------------------------------
    
    // Costs for the scheduler are assigned to CCS_SYSTEM
    stopHeapProfTimer();
#if defined(PROFILING)
    CCCS = CCS_SYSTEM;
#endif
    
    ready_to_gc = rtsFalse;

    switch (ret) {
    case HeapOverflow:
	ready_to_gc = scheduleHandleHeapOverflow(cap,t);
	break;

    case StackOverflow:
	scheduleHandleStackOverflow(cap,task,t);
	break;

    case ThreadFinished:
	if (scheduleHandleThreadFinished(cap, task, t)) return cap;
	ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);
	break;

    case ThreadBlocked:
    default:
      barf("schedule: invalid thread return code %d", (int)ret);
    }

    if (ready_to_gc) {
      cap = scheduleDoGC(cap,task,rtsFalse);
    }
  } /* end of while() */

  debugTrace(PAR_DEBUG_verbose,
	     "== Leaving schedule() after having received Finish");
}

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

static rtsBool
scheduleHandleHeapOverflow( Capability *cap, StgTSO *t )
{
    // did the task ask for a large block?
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
	    
	    // we might be unlucky and have another thread get on the
	    // run queue before us and steal the large block, but in that
	    // case the thread will just end up requesting another large
	    // block.
	    //pushOnRunQueue(cap,t);
	    return rtsFalse;  /* not actually GC'ing */
	}
    }
    
    debugTrace(DEBUG_sched,
	       "--<< thread %ld (%s) stopped: HeapOverflow\n", 
	       (long)t->id, whatNext_strs[t->what_next]);

    //pushOnRunQueue(cap,t);
    return rtsTrue;
    /* actual GC is done at the end of the while loop in schedule() */
}

/* -----------------------------------------------------------------------------
 * Handle a thread that returned to the scheduler with ThreadStackOverflow
 * -------------------------------------------------------------------------- */

static void
scheduleHandleStackOverflow (Capability *cap, Task *task, StgTSO *t)
{
    debugTrace (DEBUG_sched,
		"--<< thread %ld (%s) stopped, StackOverflow", 
		(long)t->id, whatNext_strs[t->what_next]);

    /* just adjust the stack for this thread, then pop it back
     * on the run queue.
     */
    { 
	/* enlarge the stack */
	StgTSO *new_t = threadStackOverflow(cap, t);
	
	/* The TSO attached to this Task may have moved, so update the
	 * pointer to it.
	 */
	if (task->tso == t) {
	    task->tso = new_t;
	}
        cap->r.rCurrentTSO = new_t;
    }
}

/* -----------------------------------------------------------------------------
 * Handle a thread that returned to the scheduler with ThreadFinished
 * -------------------------------------------------------------------------- */

static rtsBool
scheduleHandleThreadFinished (Capability *cap STG_UNUSED, Task *task, StgTSO *t)
{
    /* Need to check whether this was a main thread, and if so,
     * return with the return value.
     *
     * We also end up here if the thread kills itself with an
     * uncaught exception, see Exception.cmm.
     */
    debugTrace(DEBUG_sched, "--++ thread %lu (%s) finished", 
	       (unsigned long)t->id, whatNext_strs[t->what_next]);

      // Check whether the thread that just completed was a bound
      // thread, and if so return with the result.  
      //
      // There is an assumption here that all thread completion goes
      // through this point; we need to make sure that if a thread
      // ends up in the ThreadKilled state, that it stays on the run
      // queue so it can be dealt with here.
      //

      if (t->bound) {
	  ASSERT(t->bound == task);
	  ASSERT(task->tso == t);

	  if (t->what_next == ThreadComplete) {
	      if (task->ret) {
		  // NOTE: return val is tso->sp[1] (see StgStartup.hc)
		  *(task->ret) = (StgClosure *)task->tso->sp[1]; 
	      }
	      task->stat = Success;
	  } else {
	      if (task->ret) {
		  *(task->ret) = NULL;
	      }
	      if (sched_state >= SCHED_INTERRUPTING) {
		  task->stat = Interrupted;
	      } else {
		  task->stat = Killed;
	      }
	  }
#ifdef DEBUG
	  removeThreadLabel((StgWord)task->tso->id);
#endif
	  return rtsTrue; // tells schedule() to return
      }

      return rtsFalse;
}

/* -----------------------------------------------------------------------------
 * Perform a garbage collection if necessary
 * -------------------------------------------------------------------------- */

static Capability *
scheduleDoGC (Capability *cap, Task *task USED_IF_THREADS, rtsBool force_major)
{
    StgTSO *t;
    rtsBool heap_census;

    /* Kick any transactions which are invalid back to their
     * atomically frames.  When next scheduled they will try to
     * commit, this commit will fail and they will retry.
     */
    { 
	StgTSO *next;

	for (t = all_threads; t != END_TSO_QUEUE; t = next) {
	    if (t->what_next == ThreadRelocated) {
		next = t->link;
	    } else {
		next = t->global_link;
		
		// This is a good place to check for blocked
		// exceptions.  It might be the case that a thread is
		// blocked on delivering an exception to a thread that
		// is also blocked - we try to ensure that this
		// doesn't happen in throwTo(), but it's too hard (or
		// impossible) to close all the race holes, so we
		// accept that some might get through and deal with
		// them here.  A GC will always happen at some point,
		// even if the system is otherwise deadlocked.
		maybePerformBlockedException (&capabilities[0], t);

		if (t -> trec != NO_TREC && t -> why_blocked == NotBlocked) {
		    if (!stmValidateNestOfTransactions (t -> trec)) {
			debugTrace(DEBUG_sched | DEBUG_stm,
				   "trec %p found wasting its time", t);
			
			// strip the stack back to the
			// ATOMICALLY_FRAME, aborting the (nested)
			// transaction, and saving the stack of any
			// partially-evaluated thunks on the heap.
			throwToSingleThreaded_(&capabilities[0], t, 
					       NULL, rtsTrue, NULL);
			
#ifdef REG_R1
			ASSERT(get_itbl((StgClosure *)t->sp)->type == ATOMICALLY_FRAME);
#endif
		    }
		}
	    }
	}
    }
    
    // so this happens periodically:
    if (cap) scheduleCheckBlackHoles(cap);
    
    IF_DEBUG(scheduler, printAllThreads());

    /*
     * We now have all the capabilities; if we're in an interrupting
     * state, then we should take the opportunity to delete all the
     * threads in the system.
     */
    if (sched_state >= SCHED_INTERRUPTING) {
	deleteAllThreads(&capabilities[0]);
	sched_state = SCHED_SHUTTING_DOWN;
    }
    
    /* everybody back, start the GC.
     * Could do it in this thread, or signal a condition var
     * to do it in another thread.  Either way, we need to
     * broadcast on gc_pending_cond afterward.
     */
    GarbageCollect(force_major || heap_census);
    
    return cap;
}

/* ---------------------------------------------------------------------------
 * Singleton fork(). Do not copy any running threads.
 * ------------------------------------------------------------------------- */

pid_t
forkProcess(HsStablePtr *entry
#ifndef FORKPROCESS_PRIMOP_SUPPORTED
	    STG_UNUSED
#endif
           )
{
#ifdef FORKPROCESS_PRIMOP_SUPPORTED
    Task *task;
    pid_t pid;
    StgTSO* t,*next;
    Capability *cap;
    
    debugTrace(DEBUG_sched, "forking!");
    
    // ToDo: for SMP, we should probably acquire *all* the capabilities
    cap = rts_lock();
    
    // no funny business: hold locks while we fork, otherwise if some
    // other thread is holding a lock when the fork happens, the data
    // structure protected by the lock will forever be in an
    // inconsistent state in the child.  See also #1391.
    ACQUIRE_LOCK(&sched_mutex);
    ACQUIRE_LOCK(&cap->lock);
    ACQUIRE_LOCK(&cap->running_task->lock);

    pid = fork();
    
    if (pid) { // parent
	
        RELEASE_LOCK(&sched_mutex);
        RELEASE_LOCK(&cap->lock);
        RELEASE_LOCK(&cap->running_task->lock);

	// just return the pid
	rts_unlock(cap);
	return pid;
	
    } else { // child
	
	// Now, all OS threads except the thread that forked are
	// stopped.  We need to stop all Haskell threads, including
	// those involved in foreign calls.  Also we need to delete
	// all Tasks, because they correspond to OS threads that are
	// now gone.

	for (t = all_threads; t != END_TSO_QUEUE; t = next) {
	    if (t->what_next == ThreadRelocated) {
		next = t->link;
	    } else {
		next = t->global_link;
		// don't allow threads to catch the ThreadKilled
		// exception, but we do want to raiseAsync() because these
		// threads may be evaluating thunks that we need later.
		deleteThread_(cap,t);
	    }
	}
	
	// Any suspended C-calling Tasks are no more, their OS threads
	// don't exist now:
	cap->suspended_ccalling_tasks = NULL;

	// Empty the all_threads list.  Otherwise, the garbage
	// collector may attempt to resurrect some of these threads.
	all_threads = END_TSO_QUEUE;

	// Wipe the task list, except the current Task.
	ACQUIRE_LOCK(&sched_mutex);
	for (task = all_tasks; task != NULL; task=task->all_link) {
	    if (task != cap->running_task) {
		discardTask(task);
	    }
	}
	RELEASE_LOCK(&sched_mutex);

        // On Unix, all timers are reset in the child, so we need to start
        // the timer again.
        initTimer();
        startTimer();

	cap = rts_evalStableIO(cap, entry, NULL);  // run the action
	rts_checkSchedStatus("forkProcess",cap);
	
	rts_unlock(cap);
	hs_exit();                      // clean up and exit
	stg_exit(EXIT_SUCCESS);
    }
#else /* !FORKPROCESS_PRIMOP_SUPPORTED */
    barf("forkProcess#: primop not supported on this platform, sorry!\n");
    return -1;
#endif
}

/* ---------------------------------------------------------------------------
 * Delete all the threads in the system
 * ------------------------------------------------------------------------- */
   
static void
deleteAllThreads ( Capability *cap )
{
    debugBelch("/!\ deleteAllThreads() called!\n");
    // NOTE: only safe to call if we own all capabilities.

    StgTSO* t, *next;
    debugTrace(DEBUG_sched,"deleting all threads");
    for (t = all_threads; t != END_TSO_QUEUE; t = next) {
	if (t->what_next == ThreadRelocated) {
	    next = t->link;
	} else {
	    next = t->global_link;
	    deleteThread(cap,t);
	}
    }      

    // The run queue now contains a bunch of ThreadKilled threads.  We
    // must not throw these away: the main thread(s) will be in there
    // somewhere, and the main scheduler loop has to deal with it.
    // Also, the run queue is the only thing keeping these threads from
    // being GC'd, and we don't want the "main thread has been GC'd" panic.
}

/* -----------------------------------------------------------------------------
   Managing the suspended_ccalling_tasks list.
   Locks required: sched_mutex
   -------------------------------------------------------------------------- */

STATIC_INLINE void
suspendTask (Capability *cap, Task *task)
{
    ASSERT(task->next == NULL && task->prev == NULL);
    task->next = cap->suspended_ccalling_tasks;
    task->prev = NULL;
    if (cap->suspended_ccalling_tasks) {
	cap->suspended_ccalling_tasks->prev = task;
    }
    cap->suspended_ccalling_tasks = task;
}

STATIC_INLINE void
recoverSuspendedTask (Capability *cap, Task *task)
{
    if (task->prev) {
	task->prev->next = task->next;
    } else {
	ASSERT(cap->suspended_ccalling_tasks == task);
	cap->suspended_ccalling_tasks = task->next;
    }
    if (task->next) {
	task->next->prev = task->prev;
    }
    task->next = task->prev = NULL;
}

/* ---------------------------------------------------------------------------
 * Suspending & resuming Haskell threads.
 * 
 * When making a "safe" call to C (aka _ccall_GC), the task gives back
 * its capability before calling the C function.  This allows another
 * task to pick up the capability and carry on running Haskell
 * threads.  It also means that if the C call blocks, it won't lock
 * the whole system.
 *
 * The Haskell thread making the C call is put to sleep for the
 * duration of the call, on the susepended_ccalling_threads queue.  We
 * give out a token to the task, which it can use to resume the thread
 * on return from the C function.
 * ------------------------------------------------------------------------- */
   
void *
suspendThread (StgRegTable *reg)
{
  Capability *cap;
  int saved_errno;
  StgTSO *tso;
  Task *task;
#if mingw32_HOST_OS
  StgWord32 saved_winerror;
#endif

  saved_errno = errno;
#if mingw32_HOST_OS
  saved_winerror = GetLastError();
#endif

  /* assume that *reg is a pointer to the StgRegTable part of a Capability.
   */
  cap = regTableToCapability(reg);

  task = cap->running_task;
  tso = cap->r.rCurrentTSO;

  debugTrace(DEBUG_sched, 
	     "thread %lu did a safe foreign call", 
	     (unsigned long)cap->r.rCurrentTSO->id);

  // XXX this might not be necessary --SDM
  tso->what_next = ThreadRunGHC;

  threadPaused(cap,tso);

  if ((tso->flags & TSO_BLOCKEX) == 0)  {
      tso->why_blocked = BlockedOnCCall;
      tso->flags |= TSO_BLOCKEX;
      tso->flags &= ~TSO_INTERRUPTIBLE;
  } else {
      tso->why_blocked = BlockedOnCCall_NoUnblockExc;
  }

  // Hand back capability
  task->suspended_tso = tso;

  ACQUIRE_LOCK(&cap->lock);

  suspendTask(cap,task);
  cap->in_haskell = rtsFalse;
  releaseCapability_(cap);
  
  RELEASE_LOCK(&cap->lock);

  errno = saved_errno;
#if mingw32_HOST_OS
  SetLastError(saved_winerror);
#endif
  return task;
}

StgRegTable *
resumeThread (void *task_)
{
    StgTSO *tso;
    Capability *cap;
    Task *task = task_;
    int saved_errno;
#if mingw32_HOST_OS
    StgWord32 saved_winerror;
#endif

    saved_errno = errno;
#if mingw32_HOST_OS
    saved_winerror = GetLastError();
#endif

    cap = task->cap;
    // Wait for permission to re-enter the RTS with the result.
    waitForReturnCapability(&cap,task);
    // we might be on a different capability now... but if so, our
    // entry on the suspended_ccalling_tasks list will also have been
    // migrated.

    // Remove the thread from the suspended list
    recoverSuspendedTask(cap,task);

    tso = task->suspended_tso;
    task->suspended_tso = NULL;
    tso->link = END_TSO_QUEUE;
    debugTrace(DEBUG_sched, "thread %lu: re-entering RTS", (unsigned long)tso->id);
    
    if (tso->why_blocked == BlockedOnCCall) {
	awakenBlockedExceptionQueue(cap,tso);
	tso->flags &= ~(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
    }
    
    /* Reset blocking status */
    tso->why_blocked  = NotBlocked;
    
    cap->r.rCurrentTSO = tso;
    cap->in_haskell = rtsTrue;
    errno = saved_errno;
#if mingw32_HOST_OS
    SetLastError(saved_winerror);
#endif

    /* We might have GC'd, mark the TSO dirty again */
    dirtyTSO(tso);

    IF_DEBUG(sanity, checkTSO(tso));

    return &cap->r;
}

Capability *
scheduleWaitThread (StgTSO* tso, /*[out]*/HaskellObj* ret, Capability *cap)
{
    Task *task;

    // We already created/initialised the Task
    task = cap->running_task;

    // This TSO is now a bound thread; make the Task and TSO
    // point to each other.
    tso->bound = task;
    tso->cap = cap;

    task->tso = tso;
    task->ret = ret;
    task->stat = NoStatus;

    cap->r.rCurrentTSO = tso;

    debugTrace(DEBUG_sched, "new bound thread (%lu)", (unsigned long)tso->id);

    cap = schedule(cap,task);

    ASSERT(task->stat != NoStatus);
    ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);

    debugTrace(DEBUG_sched, "bound thread (%lu) finished", (unsigned long)task->tso->id);
    return cap;
}

/* ----------------------------------------------------------------------------
 * Starting Tasks
 * ------------------------------------------------------------------------- */

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
  all_threads       = END_TSO_QUEUE;

  context_switch = 0;
  sched_state    = SCHED_RUNNING;
  recent_activity = ACTIVITY_YES;

  ACQUIRE_LOCK(&sched_mutex);

  /* A capability holds the state a native thread needs in
   * order to execute STG code. At least one capability is
   * floating around (only THREADED_RTS builds have more than one).
   */
  initCapabilities();

  initTaskManager();

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
    Task *task = NULL;

    // If we haven't killed all the threads yet, do it now.
    if (sched_state < SCHED_SHUTTING_DOWN) {
	sched_state = SCHED_INTERRUPTING;
	scheduleDoGC(NULL,task,rtsFalse);    
    }
    sched_state = SCHED_SHUTTING_DOWN;

    freeCapability(&MainCapability);
}

void
freeScheduler( void )
{
    freeTaskManager();
    if (n_capabilities != 1) {
        stgFree(capabilities);
    }
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
    Task *task;

    // Evacuate the TLS
    GetTLSRoots(evac);

    for (i = 0; i < n_capabilities; i++) {
	cap = &capabilities[i];
	for (task = cap->suspended_ccalling_tasks; task != NULL; 
	     task=task->next) {
	    debugTrace(DEBUG_sched,
		       "evac'ing suspended TSO %lu", (unsigned long)task->suspended_tso->id);
	    evac((StgClosure **)(void *)&task->suspended_tso);
	}

    }

    // evac((StgClosure **)&blackhole_queue);

#if defined(RTS_USER_SIGNALS)
    // mark the signal handlers (signals should be already blocked)
    if (RtsFlags.MiscFlags.install_signal_handlers) {
        markSignalHandlers(evac);
    }
#endif
}

/* -----------------------------------------------------------------------------
   performGC

   This is the interface to the garbage collector from Haskell land.
   We provide this so that external C code can allocate and garbage
   collect when called from Haskell via _ccall_GC.
   -------------------------------------------------------------------------- */

static void
performGC_(rtsBool force_major)
{
    Task *task;
    // We must grab a new Task here, because the existing Task may be
    // associated with a particular Capability, and chained onto the 
    // suspended_ccalling_tasks queue.
    ACQUIRE_LOCK(&sched_mutex);
    task = newBoundTask();
    RELEASE_LOCK(&sched_mutex);
    scheduleDoGC(NULL,task,force_major);
    boundTaskExiting(task);
}

void
performGC(void)
{
    performGC_(rtsFalse);
}

void
performMajorGC(void)
{
    performGC_(rtsTrue);
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

/* ---------------------------------------------------------------------------
   Interrupt execution
   - usually called inside a signal handler so it mustn't do anything fancy.   
   ------------------------------------------------------------------------ */

void
interruptStgRts(void)
{
    sched_state = SCHED_INTERRUPTING;
    context_switch = 1;
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
   Deleting threads

   This is used for interruption (^C) and forking, and corresponds to
   raising an exception but without letting the thread catch the
   exception.
   -------------------------------------------------------------------------- */

static void 
deleteThread (Capability *cap, StgTSO *tso)
{
    // NOTE: must only be called on a TSO that we have exclusive
    // access to, because we will call throwToSingleThreaded() below.
    // The TSO must be on the run queue of the Capability we own, or 
    // we must own all Capabilities.

    if (tso->why_blocked != BlockedOnCCall &&
	tso->why_blocked != BlockedOnCCall_NoUnblockExc) {
	throwToSingleThreaded(cap,tso,NULL);
    }
}

#ifdef FORKPROCESS_PRIMOP_SUPPORTED
static void 
deleteThread_(Capability *cap, StgTSO *tso)
{ // for forkProcess only:
  // like deleteThread(), but we delete threads in foreign calls, too.

    if (tso->why_blocked == BlockedOnCCall ||
	tso->why_blocked == BlockedOnCCall_NoUnblockExc) {
	unblockOne(cap,tso);
	tso->what_next = ThreadKilled;
    } else {
	deleteThread(cap,tso);
    }
}
#endif

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

        case ATOMICALLY_FRAME:
	    debugTrace(DEBUG_stm, "found ATOMICALLY_FRAME at %p", p);
            tso->sp = p;
            return ATOMICALLY_FRAME;
	    
	case CATCH_FRAME:
	    tso->sp = p;
	    return CATCH_FRAME;

        case CATCH_STM_FRAME:
	    debugTrace(DEBUG_stm, "found CATCH_STM_FRAME at %p", p);
            tso->sp = p;
            return CATCH_STM_FRAME;
	    
	case STOP_FRAME:
	    tso->sp = p;
	    return STOP_FRAME;

	default:
	    p = next; 
	    continue;
	}
    }
}

/* -----------------------------------------------------------------------------
   resurrectThreads is called after garbage collection on the list of
   threads found to be garbage.  Each of these threads will be woken
   up and sent a signal: BlockedOnDeadMVar if the thread was blocked
   on an MVar, or NonTermination if the thread was blocked on a Black
   Hole.

   Locks: assumes we hold *all* the capabilities.
   -------------------------------------------------------------------------- */

void
resurrectThreads (StgTSO *threads)
{
    StgTSO *tso, *next;
    Capability *cap;

    for (tso = threads; tso != END_TSO_QUEUE; tso = next) {
	next = tso->global_link;
	tso->global_link = all_threads;
	all_threads = tso;
	debugTrace(DEBUG_sched, "resurrecting thread %lu", (unsigned long)tso->id);
	
	// Wake up the thread on the Capability it was last on
	cap = tso->cap;
	
	switch (tso->why_blocked) {
	case BlockedOnMVar:
	case BlockedOnException:
	    /* Called by GC - sched_mutex lock is currently held. */
	    throwToSingleThreaded(cap, tso,
				  (StgClosure *)BlockedOnDeadMVar_closure);
	    break;
	case BlockedOnBlackHole:
	    throwToSingleThreaded(cap, tso,
				  (StgClosure *)NonTermination_closure);
	    break;
	case BlockedOnSTM:
	    throwToSingleThreaded(cap, tso,
				  (StgClosure *)BlockedIndefinitely_closure);
	    break;
	case NotBlocked:
	    /* This might happen if the thread was blocked on a black hole
	     * belonging to a thread that we've just woken up (raiseAsync
	     * can wake up threads, remember...).
	     */
	    continue;
	default:
	    barf("resurrectThreads: thread blocked in a strange way");
	}
    }
}
