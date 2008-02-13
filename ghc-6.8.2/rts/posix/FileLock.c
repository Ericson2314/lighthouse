/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2007
 *
 * File locking support as required by Haskell 98
 *
 * ---------------------------------------------------------------------------*/
 
#include "Rts.h"
#include "Hash.h"
#include "FileLock.h"
#include "RtsUtils.h"

#include <unistd.h>
#include <sys/stat.h>
#include <errno.h>

typedef struct {
    dev_t device;
    ino_t inode;
    int   readers; // >0 : readers,  <0 : writers
} Lock;

// Two hash tables.  The first maps objects (device/inode pairs) to
// Lock objects containing the number of active readers or writers.  The
// second maps file descriptors to lock objects, so that we can unlock
// by FD without needing to fstat() again.
static HashTable *obj_hash;
static HashTable *fd_hash;

static int cmpLocks(StgWord w1, StgWord w2)
{
    Lock *l1 = (Lock *)w1;
    Lock *l2 = (Lock *)w2;
    return (l1->device == l2->device && l1->inode == l2->inode);
}

static int hashLock(HashTable *table, StgWord w)
{
    Lock *l = (Lock *)w;
    // Just xor the dev_t with the ino_t, hope this is good enough.
    return hashWord(table, (StgWord)l->inode ^ (StgWord)l->device);
}

void
initFileLocking(void)
{
    obj_hash = allocHashTable_(hashLock, cmpLocks);
    fd_hash  = allocHashTable(); /* ordinary word-based table */
}

static void
freeLock(void *lock)
{
    stgFree(lock);
}

void
freeFileLocking(void)
{
    freeHashTable(obj_hash, freeLock);
    freeHashTable(fd_hash,  NULL);
}

int
lockFile(int fd, dev_t dev, ino_t ino, int for_writing)
{
    Lock key, *lock;

    key.device = dev;
    key.inode  = ino;

    lock = lookupHashTable(obj_hash, (StgWord)&key);

    if (lock == NULL)
    {
        lock = stgMallocBytes(sizeof(Lock), "lockFile");
        lock->device = dev;
        lock->inode  = ino;
        lock->readers = for_writing ? -1 : 1;
        insertHashTable(obj_hash, (StgWord)lock, (void *)lock);
        insertHashTable(fd_hash, fd, lock);
        return 0;
    }
    else
    {
        // single-writer/multi-reader locking:
        if (for_writing || lock->readers < 0) {
            return -1;
        }
        lock->readers++;
        return 0;
    }
}

int
unlockFile(int fd)
{
    Lock *lock;

    lock = lookupHashTable(fd_hash, fd);
    if (lock == NULL) {
        // errorBelch("unlockFile: fd %d not found", fd); 
        // This is normal: we didn't know when calling unlockFile
        // whether this FD referred to a locked file or not.
        return 1;
    }

    if (lock->readers < 0) {
        lock->readers++;
    } else {
        lock->readers--;
    }

    if (lock->readers == 0) {
        removeHashTable(obj_hash, (StgWord)lock, NULL);
        stgFree(lock);
    }
    removeHashTable(fd_hash, fd, NULL);
    return 0;
}
