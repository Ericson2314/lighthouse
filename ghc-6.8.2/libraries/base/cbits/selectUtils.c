
#include "HsBase.h"
#ifndef xen_HOST_OS
void hsFD_ZERO(fd_set *fds) { FD_ZERO(fds); }
#endif
