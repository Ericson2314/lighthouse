#ifndef TLS_H
#define TLS_H

#include "Rts.h"

extern StgClosure * tls_default_slots[MAX_TLS_SLOTS];
int newTLSKeyHelper(StgClosure *d);

void expandTLSHelper(StgTSO *tso);

void GetTLSRoots(evac_fn evac);

#endif

