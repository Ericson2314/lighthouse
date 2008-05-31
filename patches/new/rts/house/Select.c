/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1995-2002
 *
 * Support for concurrent non-blocking I/O and thread waiting.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "Schedule.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Timer.h"
#include "Signals.h"
#include "Capability.h"
#include "posix/Select.h"

# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif

# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif

#include <string.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

extern unsigned int getourtimeofday(void);

#if !defined(THREADED_RTS)
/* last timestamp */
lnat timestamp = 0;

/* 
 * The threaded RTS uses an IO-manager thread in Haskell instead (see GHC.Conc) 
 */

/* There's a clever trick here to avoid problems when the time wraps
 * around.  Since our maximum delay is smaller than 31 bits of ticks
 * (it's actually 31 bits of microseconds), we can safely check
 * whether a timer has expired even if our timer will wrap around
 * before the target is reached, using the following formula:
 *
 *        (int)((uint)current_time - (uint)target_time) < 0
 *
 * if this is true, then our time has expired.
 * (idea due to Andy Gill).
 */
static rtsBool
wakeUpSleepingThreads(lnat ticks)
{
    StgTSO *tso;
    rtsBool flag = rtsFalse;

    while (sleeping_queue != END_TSO_QUEUE &&
	   (int)(ticks - sleeping_queue->block_info.target) > 0) {
	tso = sleeping_queue;
	sleeping_queue = tso->link;
	tso->why_blocked = NotBlocked;
	tso->link = END_TSO_QUEUE;
	IF_DEBUG(scheduler,debugBelch("Waking up sleeping thread %lu\n", (unsigned long)tso->id));
	// MainCapability: this code is !THREADED_RTS
	pushOnRunQueue(&MainCapability,tso);
	flag = rtsTrue;
    }
    return flag;
}

/* Argument 'wait' says whether to wait for I/O to become available,
 * or whether to just check and return immediately.  If there are
 * other threads ready to run, we normally do the non-waiting variety,
 * otherwise we wait (see Schedule.c).
 *
 * SMP note: must be called with sched_mutex locked.
 *
 * Windows: select only works on sockets, so this doesn't really work,
 * though it makes things better than before. MsgWaitForMultipleObjects
 * should really be used, though it only seems to work for read handles,
 * not write handles.
 *
 */
void
awaitEvent(rtsBool wait)
{
    StgTSO *tso, *prev, *next;
    rtsBool ready;
    fd_set rfd,wfd;
    int numFound;
    int maxfd = -1;
    rtsBool select_succeeded = rtsTrue;
    rtsBool unblock_all = rtsFalse;
    struct timeval tv;
    lnat min, ticks;

    tv.tv_sec  = 0;
    tv.tv_usec = 0;
    
    IF_DEBUG(scheduler,
	     debugBelch("scheduler: checking for threads blocked on I/O");
	     if (wait) {
		 debugBelch(" (waiting)");
	     }
	     debugBelch("\n");
	     );

    /* loop until we've woken up some threads.  This loop is needed
     * because the select timing isn't accurate, we sometimes sleep
     * for a while but not long enough to wake up a thread in
     * a threadDelay.
     */
    do {

      ticks = timestamp = getourtimeofday();
      if (wakeUpSleepingThreads(ticks)) { 
	  return;
      }

      if (!wait) {
	  min = 0;
      } else if (sleeping_queue != END_TSO_QUEUE) {
	  min = (sleeping_queue->block_info.target - ticks) 
	      * RtsFlags.MiscFlags.tickInterval * 1000;
      } else {
	  min = 0x7ffffff;
      }

      /* 
       * Collect all of the fd's that we're interested in
       */
      FD_ZERO(&rfd);
      FD_ZERO(&wfd);

      for(tso = blocked_queue_hd; tso != END_TSO_QUEUE; tso = next) {
	next = tso->link;

	switch (tso->why_blocked) {
	case BlockedOnRead:
	  { 
	    int fd = tso->block_info.fd;
	    if (fd >= FD_SETSIZE) {
		barf("awaitEvent: descriptor out of range");
	    }
	    maxfd = (fd > maxfd) ? fd : maxfd;
	    FD_SET(fd, &rfd);
	    continue;
	  }

	case BlockedOnWrite:
	  { 
	    int fd = tso->block_info.fd;
	    if (fd >= FD_SETSIZE) {
		barf("awaitEvent: descriptor out of range");
	    }
	    maxfd = (fd > maxfd) ? fd : maxfd;
	    FD_SET(fd, &wfd);
	    continue;
	  }

	default:
	  barf("AwaitEvent");
	}
      }

      /* Check for any interesting events */
      
      tv.tv_sec  = min / 1000000;
      tv.tv_usec = min % 1000000;

      while ((numFound = select(maxfd+1, &rfd, &wfd, NULL, &tv)) < 0) {
	  /* We got a signal; could be one of ours.  If so, we need
	   * to start up the signal handler straight away, otherwise
	   * we could block for a long time before the signal is
	   * serviced.
	   */
#if defined(RTS_USER_SIGNALS)
	  if (RtsFlags.MiscFlags.install_signal_handlers && signals_pending()) {
	      startSignalHandlers(&MainCapability);
	      return; /* still hold the lock */
	  }
#endif

	  /* we were interrupted, return to the scheduler immediately.
	   */
	  if (sched_state >= SCHED_INTERRUPTING) {
	      return; /* still hold the lock */
	  }
	  
	  /* check for threads that need waking up 
	   */
	  wakeUpSleepingThreads(getourtimeofday());
	  
	  /* If new runnable threads have arrived, stop waiting for
	   * I/O and run them.
	   */
	  if (!emptyRunQueue(&MainCapability)) {
	      return; /* still hold the lock */
	  }
      }

      /* Step through the waiting queue, unblocking every thread that now has
       * a file descriptor in a ready state.
       */

      prev = NULL;
      if (select_succeeded || unblock_all) {
	  for(tso = blocked_queue_hd; tso != END_TSO_QUEUE; tso = next) {
	      next = tso->link;
	      switch (tso->why_blocked) {
	      case BlockedOnRead:
		  ready = unblock_all || FD_ISSET(tso->block_info.fd, &rfd);
		  break;
	      case BlockedOnWrite:
		  ready = unblock_all || FD_ISSET(tso->block_info.fd, &wfd);
		  break;
	      default:
		  barf("awaitEvent");
	      }
      
	      if (ready) {
		IF_DEBUG(scheduler,debugBelch("Waking up blocked thread %lu\n", (unsigned long)tso->id));
		  tso->why_blocked = NotBlocked;
		  tso->link = END_TSO_QUEUE;
		  pushOnRunQueue(&MainCapability,tso);
	      } else {
		  if (prev == NULL)
		      blocked_queue_hd = tso;
		  else
		      prev->link = tso;
		  prev = tso;
	      }
	  }

	  if (prev == NULL)
	      blocked_queue_hd = blocked_queue_tl = END_TSO_QUEUE;
	  else {
	      prev->link = END_TSO_QUEUE;
	      blocked_queue_tl = prev;
	  }
      }
      
    } while (wait && sched_state == SCHED_RUNNING
	     && emptyRunQueue(&MainCapability));
}

#endif /* !THREADED_RTS */

