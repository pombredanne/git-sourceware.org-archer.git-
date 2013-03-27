/* Shared general utility routines for GDB, the GNU debugger.

   Copyright (C) 1986-2013 Free Software Foundation, Inc.

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef COMMON_UTILS_H
#define COMMON_UTILS_H

#include "config.h"
#include "ansidecl.h"
#include <stddef.h>
#include <stdarg.h>

/* Static target-system-dependent parameters for GDB.  */

/* Number of bits in a char or unsigned char for the target machine.
   Just like CHAR_BIT in <limits.h> but describes the target machine.  */
#if !defined (TARGET_CHAR_BIT)
#define TARGET_CHAR_BIT 8
#endif

/* If we picked up a copy of CHAR_BIT from a configuration file
   (which may get it by including <limits.h>) then use it to set
   the number of bits in a host char.  If not, use the same size
   as the target.  */

#if defined (CHAR_BIT)
#define HOST_CHAR_BIT CHAR_BIT
#else
#define HOST_CHAR_BIT TARGET_CHAR_BIT
#endif

extern void malloc_failure (long size) ATTRIBUTE_NORETURN;
extern void internal_error (const char *file, int line, const char *, ...)
     ATTRIBUTE_NORETURN ATTRIBUTE_PRINTF (3, 4);

/* xmalloc(), xrealloc() and xcalloc() have already been declared in
   "libiberty.h". */

/* Like xmalloc, but zero the memory.  */
void *xzalloc (size_t);

void xfree (void *);

/* Like asprintf and vasprintf, but return the string, throw an error
   if no memory.  */
char *xstrprintf (const char *format, ...) ATTRIBUTE_PRINTF (1, 2);
char *xstrvprintf (const char *format, va_list ap)
     ATTRIBUTE_PRINTF (1, 0);

/* Like snprintf, but throw an error if the output buffer is too small.  */
int xsnprintf (char *str, size_t size, const char *format, ...)
     ATTRIBUTE_PRINTF (3, 4);

/* Make a copy of the string at PTR with LEN characters
   (and add a null character at the end in the copy).
   Uses malloc to get the space.  Returns the address of the copy.  */

char *savestring (const char *ptr, size_t len);

ULONGEST strtoulst (const char *num, const char **trailer, int base);

extern int fromhex (int a);

extern int tohex (int nib);

extern int hex2bin (const char *hex, gdb_byte *bin, int count);

extern int bin2hex (const gdb_byte *bin, char *hex, int count);

/* Skip leading whitespace characters in INP, returning an updated
   pointer.  If INP is NULL, return NULL.  */

extern char *skip_spaces (char *inp);

/* A const-correct version of the above.  */

extern const char *skip_spaces_const (const char *inp);

#endif
