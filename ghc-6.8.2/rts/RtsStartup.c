/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2002
 *
 * Main function for a standalone Haskell program.
 *
 * ---------------------------------------------------------------------------*/

// PAPI uses caddr_t, which is not POSIX
// #include "PosixSource.h"

#include "Rts.h"
#include "RtsAPI.h"
#include "RtsUtils.h"
#include "RtsFlags.h"  
#include "OSThreads.h"
#include "Schedule.h"   /* initScheduler */
#include "Stats.h"      /* initStats */
#include "STM.h"        /* initSTM */
#include "Signals.h"
#include "RtsSignals.h"
#include "ThrIOManager.h"
#include "Timer.h"      /* startTimer, stopTimer */
#include "Weak.h"
#include "Ticky.h"
#include "StgRun.h"
#include "Prelude.h"		/* fixupRTStoPreludeRefs */
#include "HsFFI.h"
#include "Linker.h"
#include "ThreadLabels.h"
#include "BlockAlloc.h"
#include "Trace.h"
#include "RtsTypeable.h"
#include "Stable.h"
#ifndef house_HOST_OS
#include "Hpc.h"
#endif
#include "FileLock.h"

#if defined(RTS_GTK_FRONTPANEL)
#include "FrontPanel.h"
#endif

# include "Profiling.h"

#if defined(PROFILING)
# include "ProfHeap.h"
# include "RetainerProfile.h"
#endif

#if defined(GRAN)
# include "GranSimRts.h"
#endif

#if defined(GRAN) || defined(PAR)
# include "ParallelRts.h"
#endif

#if defined(PAR)
# include "Parallel.h"
# include "LLC.h"
#endif

#if defined(mingw32_HOST_OS) && !defined(THREADED_RTS)
#include "win32/AsyncIO.h"
#endif

#include <stdlib.h>

#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#if USE_PAPI
#include "Papi.h"
#endif

// Count of how many outstanding hs_init()s there have been.
static int hs_init_count = 0;

// Here we save the terminal settings on the standard file
// descriptors, if we need to change them (eg. to support NoBuffering
// input).
static void *saved_termios[3] = {NULL,NULL,NULL};

void*
__hscore_get_saved_termios(int fd)
{
  return (0 <= fd && fd < (int)(sizeof(saved_termios) / sizeof(*saved_termios))) ?
    saved_termios[fd] : NULL;
}

void
__hscore_set_saved_termios(int fd, void* ts)
{
  if (0 <= fd && fd < (int)(sizeof(saved_termios) / sizeof(*saved_termios))) {
    saved_termios[fd] = ts;
  }
}

/* -----------------------------------------------------------------------------
   Initialise floating point unit on x86 (currently disabled. why?)
   (see comment in ghc/compiler/nativeGen/MachInstrs.lhs).
   -------------------------------------------------------------------------- */

#define X86_INIT_FPU 0

#if X86_INIT_FPU
static void
x86_init_fpu ( void )
{
  __volatile unsigned short int fpu_cw;

  // Grab the control word
  __asm __volatile ("fnstcw %0" : "=m" (fpu_cw));

#if 0
  printf("fpu_cw: %x\n", fpu_cw);
#endif

  // Set bits 8-9 to 10 (64-bit precision).
  fpu_cw = (fpu_cw & 0xfcff) | 0x0200;

  // Store the new control word back
  __asm __volatile ("fldcw %0" : : "m" (fpu_cw));
}
#endif

/* -----------------------------------------------------------------------------
   Starting up the RTS
   -------------------------------------------------------------------------- */

void
hs_init(int *argc, char **argv[])
{
    hs_init_count++;
    if (hs_init_count > 1) {
	// second and subsequent inits are ignored
	return;
    }

#if defined(DEBUG)
    /* Start off by initialising the allocator debugging so we can
     * use it anywhere */
    initAllocator();
#endif

    /* Next we do is grab the start time...just in case we're
     * collecting timing statistics.
     */
    stat_startInit();

#ifdef PAR
    /*
     * The parallel system needs to be initialised and synchronised before
     * the program is run.  
     */ 
    startupParallelSystem(argv);
     
    if (*argv[0] == '-') { /* Strip off mainPE flag argument */
      argv++; 
      argc--;			
    }

    argv[1] = argv[0];   /* ignore the nPEs argument */
    argv++; argc--;
#endif

    /* Initialise the performance tracking library */
#ifdef USE_PAPI
    {
	int ver;
	if ((ver = PAPI_library_init(PAPI_VER_CURRENT)) != PAPI_VER_CURRENT) {
	    if (ver > 0) {
		errorBelch("PAPI_library_init: wrong version: %x", ver);
		stg_exit(EXIT_FAILURE);
	    } else {
		sysErrorBelch("PAPI_library_init");
		stg_exit(EXIT_FAILURE);
	    }
	}
    }
#ifdef THREADED_RTS
    {
	int err;
	if ((err = PAPI_thread_init(osThreadId)) < 0) {
	    barf("PAPI_thread_init: %d",err);
	}
    }
#endif
#endif

    /* Set the RTS flags to default values. */

    initRtsFlagsDefaults();

    /* Call the user hook to reset defaults, if present */
    defaultsHook();

    /* Parse the flags, separating the RTS flags from the programs args */
    if (argc != NULL && argv != NULL) {
	setFullProgArgv(*argc,*argv);
	setupRtsFlags(argc, *argv, &rts_argc, rts_argv);
	setProgArgv(*argc,*argv);
    }

    /* initTracing must be after setupRtsFlags() */
    initTracing();

#if defined(PAR)
    /* NB: this really must be done after processing the RTS flags */
    IF_PAR_DEBUG(verbose,
                 debugBelch("==== Synchronising system (%d PEs)\n", nPEs));
    synchroniseSystem();             // calls initParallelSystem etc
#endif	/* PAR */

    /* initialise scheduler data structures (needs to be done before
     * initStorage()).
     */
    initScheduler();

#if defined(GRAN)
    /* And start GranSim profiling if required: */
    if (RtsFlags.GranFlags.GranSimStats.Full)
      init_gr_simulation(rts_argc, rts_argv, prog_argc, prog_argv);
#elif defined(PAR)
    /* And start GUM profiling if required: */
    if (RtsFlags.ParFlags.ParStats.Full)
      init_gr_simulation(rts_argc, rts_argv, prog_argc, prog_argv);
#endif	/* PAR || GRAN */

    /* initialize the storage manager */
    initStorage();

    /* initialise the stable pointer table */
    initStablePtrTable();

    /* initialise the shared Typeable store */
    initTypeableStore();

    /* initialise file locking, if necessary */
#if !defined(mingw32_HOST_OS)    
    initFileLocking();
#endif

#if defined(DEBUG)
    /* initialise thread label table (tso->char*) */
    initThreadLabelTable();
#endif

    initProfiling1();

    /* start the virtual timer 'subsystem'. */
    initTimer();
    startTimer();

    /* Initialise the stats department */
    initStats();

#if defined(RTS_USER_SIGNALS)
    if (RtsFlags.MiscFlags.install_signal_handlers) {
        /* Initialise the user signal handler set */
        initUserSignals();
        /* Set up handler to run on SIGINT, etc. */
        initDefaultHandlers();
    }
#endif
 
#if defined(mingw32_HOST_OS) && !defined(THREADED_RTS)
    startupAsyncIO();
#endif

#ifdef RTS_GTK_FRONTPANEL
    if (RtsFlags.GcFlags.frontpanel) {
	initFrontPanel();
    }
#endif

#if X86_INIT_FPU
    x86_init_fpu();
#endif

#if defined(THREADED_RTS)
    ioManagerStart();
#endif

    /* Record initialization times */
    stat_endInit();
}

// Compatibility interface
void
startupHaskell(int argc, char *argv[], void (*init_root)(void))
{
    hs_init(&argc, &argv);
    if(init_root)
        hs_add_root(init_root);
}


/* -----------------------------------------------------------------------------
   Per-module initialisation

   This process traverses all the compiled modules in the program
   starting with "Main", and performing per-module initialisation for
   each one.

   So far, two things happen at initialisation time:

      - we register stable names for each foreign-exported function
        in that module.  This prevents foreign-exported entities, and
	things they depend on, from being garbage collected.

      - we supply a unique integer to each statically declared cost
        centre and cost centre stack in the program.

   The code generator inserts a small function "__stginit_<module>" in each
   module and calls the registration functions in each of the modules it
   imports.

   The init* functions are compiled in the same way as STG code,
   i.e. without normal C call/return conventions.  Hence we must use
   StgRun to call this stuff.
   -------------------------------------------------------------------------- */

/* The init functions use an explicit stack... 
 */
#define INIT_STACK_BLOCKS  4
static F_ *init_stack = NULL;

void
hs_add_root(void (*init_root)(void))
{
    bdescr *bd;
    nat init_sp;
    Capability *cap = &MainCapability;

    if (hs_init_count <= 0) {
	barf("hs_add_root() must be called after hs_init()");
    }

    /* The initialisation stack grows downward, with sp pointing 
       to the last occupied word */
    init_sp = INIT_STACK_BLOCKS*BLOCK_SIZE_W;
    bd = allocGroup_lock(INIT_STACK_BLOCKS);
    init_stack = (F_ *)bd->start;
    init_stack[--init_sp] = (F_)stg_init_finish;
    if (init_root != NULL) {
	init_stack[--init_sp] = (F_)init_root;
    }
    
    cap->r.rSp = (P_)(init_stack + init_sp);
    StgRun((StgFunPtr)stg_init, &cap->r);

    freeGroup_lock(bd);
#ifndef house_HOST_OS
    startupHpc();
#endif
    // This must be done after module initialisation.
    // ToDo: make this work in the presence of multiple hs_add_root()s.
    initProfiling2();
}

/* ----------------------------------------------------------------------------
 * Shutting down the RTS
 *
 * The wait_foreign parameter means:
 *       True  ==> wait for any threads doing foreign calls now.
 *       False ==> threads doing foreign calls may return in the
 *                 future, but will immediately block on a mutex.
 *                 (capability->lock).
 * 
 * If this RTS is a DLL that we're about to unload, then you want
 * safe=True, otherwise the thread might return to code that has been
 * unloaded.  If this is a standalone program that is about to exit,
 * then you can get away with safe=False, which is better because we
 * won't hang on exit if there is a blocked foreign call outstanding.
 *
 ------------------------------------------------------------------------- */

static void
hs_exit_(rtsBool wait_foreign)
{
    if (hs_init_count <= 0) {
	errorBelch("warning: too many hs_exit()s");
	return;
    }
    hs_init_count--;
    if (hs_init_count > 0) {
	// ignore until it's the last one
	return;
    }

    /* start timing the shutdown */
    stat_startExit();
    
#if defined(RTS_USER_SIGNALS)
    if (RtsFlags.MiscFlags.install_signal_handlers) {
        freeSignalHandlers();
    }
#endif

#if defined(THREADED_RTS)
    ioManagerDie();
#endif

    /* stop all running tasks */
    exitScheduler(wait_foreign);
    
#if defined(GRAN)
    /* end_gr_simulation prints global stats if requested -- HWL */
    if (!RtsFlags.GranFlags.GranSimStats.Suppressed)
	end_gr_simulation();
#endif
    
    /* stop the ticker */
    stopTimer();
    exitTimer();

    /* reset the standard file descriptors to blocking mode */
    resetNonBlockingFd(0);
    resetNonBlockingFd(1);
    resetNonBlockingFd(2);

#if HAVE_TERMIOS_H
    // Reset the terminal settings on the standard file descriptors,
    // if we changed them.  See System.Posix.Internals.tcSetAttr for
    // more details, including the reason we termporarily disable
    // SIGTTOU here.
    { 
	int fd;
	sigset_t sigset, old_sigset;
	sigemptyset(&sigset);
	sigaddset(&sigset, SIGTTOU);
	sigprocmask(SIG_BLOCK, &sigset, &old_sigset);
	for (fd = 0; fd <= 2; fd++) {
	    struct termios* ts = (struct termios*)__hscore_get_saved_termios(fd);
	    if (ts != NULL) {
		tcsetattr(fd,TCSANOW,ts);
	    }
	}
	sigprocmask(SIG_SETMASK, &old_sigset, NULL);
    }
#endif

#if defined(PAR)
    /* controlled exit; good thread! */
    shutdownParallelSystem(0);
    
    /* global statistics in parallel system */
    PAR_TICKY_PAR_END();
#endif

    /* stop timing the shutdown, we're about to print stats */
    stat_endExit();
   
#ifndef house_HOST_OS
    /* shutdown the hpc support (if needed) */
    exitHpc();
#endif

    // clean up things from the storage manager's point of view.
    // also outputs the stats (+RTS -s) info.
    exitStorage();
    
    /* free the tasks */
    freeScheduler();

    /* free shared Typeable store */
    exitTypeableStore();

    /* free file locking tables, if necessary */
#if !defined(mingw32_HOST_OS)    
    freeFileLocking();
#endif

    /* free the stable pointer table */
    exitStablePtrTable();

#if defined(DEBUG)
    /* free the thread label table */
    freeThreadLabelTable();
#endif

#ifdef RTS_GTK_FRONTPANEL
    if (RtsFlags.GcFlags.frontpanel) {
	stopFrontPanel();
    }
#endif

#if defined(PROFILING) 
    reportCCSProfiling();
#endif

    endProfiling();
    freeProfiling1();

#ifdef PROFILING
    // Originally, this was in report_ccs_profiling().  Now, retainer
    // profiling might tack some extra stuff on to the end of this file
    // during endProfiling().
    fclose(prof_file);
#endif

#if defined(TICKY_TICKY)
    if (RtsFlags.TickyFlags.showTickyStats) PrintTickyInfo();
#endif

#if defined(mingw32_HOST_OS) && !defined(THREADED_RTS)
    shutdownAsyncIO(wait_foreign);
#endif

    /* free hash table storage */
    exitHashTable();

    // Finally, free all our storage
    freeStorage();

#if defined(DEBUG)
    /* and shut down the allocator debugging */
    shutdownAllocator();
#endif

}

// The real hs_exit():
void
hs_exit(void)
{
    hs_exit_(rtsTrue);
    // be safe; this might be a DLL
}

// Compatibility interfaces
void
shutdownHaskell(void)
{
    hs_exit();
}

void
shutdownHaskellAndExit(int n)
{
    if (hs_init_count == 1) {
	OnExitHook();
	hs_exit_(rtsFalse);
        // we're about to exit(), no need to wait for foreign calls to return.
#if defined(PAR)
	/* really exit (stg_exit() would call shutdownParallelSystem() again) */
	exit(n);
#else
	stg_exit(n);
#endif
    }
}

/* 
 * called from STG-land to exit the program
 */

#ifdef PAR
static int exit_started=rtsFalse;
#endif

void (*exitFn)(int) = 0;

void  
stg_exit(int n)
{ 
#ifdef PAR
  /* HACK: avoid a loop when exiting due to a stupid error */
  if (exit_started) 
    return;
  exit_started=rtsTrue;

  IF_PAR_DEBUG(verbose, debugBelch("==-- stg_exit %d on [%x]...", n, mytid));
  shutdownParallelSystem(n);
#endif
  if (exitFn)
    (*exitFn)(n);
  exit(n);
}
