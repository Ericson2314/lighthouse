/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Block Allocator Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef BLOCK_ALLOC_H
#define BLOCK_ALLOC_H

/* Debugging  -------------------------------------------------------------- */

#if defined(DEBUG) || defined(PERFORM_SANITY_CHECKS)
extern void checkFreeListSanity(void);
nat         countFreeList(void);
#endif

#endif /* BLOCK_ALLOC_H */
