# -----------------------------------------------------------------------------

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

HS_PROG      = UserGuide$(exeext)
HC           = $(GHC_INPLACE)
MKDEPENDHS   = $(GHC_INPLACE)
SRC_HC_OPTS += -package parsec

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
