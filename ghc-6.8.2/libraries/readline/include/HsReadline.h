#ifndef HSREADLINE_H
#define HSREADLINE_H

#include "HsReadlineConfig.h"
#include <stdio.h>
#if defined(HAVE_FRAMEWORK_READLINE)
#include <GNUreadline/readline/readline.h>
#include <GNUreadline/readline/history.h>
#else   
#include <readline/readline.h>
#include <readline/history.h>
#endif
    
extern void hs_rl_message (const char *s);

#endif
