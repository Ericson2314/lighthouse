#include "Rts.h"
#include "TLS.h"

static volatile int tls_next_key = 0;
StgClosure * tls_default_slots[MAX_TLS_SLOTS];

int newTLSKeyHelper(StgClosure *d) {
    int key;
    key = tls_next_key++;
    tls_default_slots[key] = d;
    ASSERT(tls_next_key < MAX_TLS_SLOTS);
    return key;
}

void expandTLSHelper(StgTSO *tso) {
    int i;
    for (i = tso->tls_max; i < tls_next_key; i++)
        tso->tls_slots[i] = tls_default_slots[i];
    tso->tls_max = tls_next_key;
}

void GetTLSRoots(evac_fn evac) {
    int i;
    for (i = 0; i < tls_next_key; i++) {
        evac(&(tls_default_slots[i]));
    }
}
