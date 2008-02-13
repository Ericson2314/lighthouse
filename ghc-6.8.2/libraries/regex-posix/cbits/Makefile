TOP = ../..
include $(TOP)/mk/boilerplate.mk
-include ../config.mk
PACKAGE=

HC = $(GHC_INPLACE)

UseGhcForCc = YES

SRC_CC_OPTS   += -I. -I../include

# engine.c is #included into regexec.c, so:
EXCLUDED_SRCS += engine.c

SRC_CC_OPTS += -Wall

LIBRARY = libHSregex_posix_cbits.a
LIBOBJS = $(C_OBJS)

include $(TOP)/mk/target.mk
