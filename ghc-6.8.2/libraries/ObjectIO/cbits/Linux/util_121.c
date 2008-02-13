/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1,
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Generally applicable utility routines.
********************************************************************************************/
#include "util_121.h"
#include "intrface_121.h"
#include <stdarg.h>


/*	since we don't use the C runtime library, here are some simple
	routines that would normally come from the C runtime lib.
*/

void *rmalloc (unsigned long bytes)
{
	void *ptr = malloc (bytes);

	if (!ptr)
	{
		printf("Out of memory\n");
		exit(1);
	}

	return ptr;
}

void rfree (void *ptr)
{
	free(ptr);
}

gchar *createMnemonicString(gchar *source)
{
	gchar *dest = (gchar *) rmalloc(strlen(source)*2+1);
	gchar *s = dest;

	while (*source)
	{
		switch (*source)
		{
		case '&':
			*(dest++) = '_';
			break;
		case '_':
			*(dest++) = '_';
			*(dest++) = '_';
		default:
			*(dest++) = *source;
		}

		source++;
	}

	*dest = 0;
	return s;
};
