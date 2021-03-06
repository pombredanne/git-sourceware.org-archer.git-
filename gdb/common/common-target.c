/* Utility target functions for GDB, and GDBserver.

   Copyright (C) 2013 Free Software Foundation, Inc.

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

#ifdef GDBSERVER
#include "server.h"
#else
#include "defs.h"
#endif

#include "common-target.h"
#include "gdb_assert.h"
#include "gdb_string.h"

LONGEST
read_alloc (gdb_byte **buf_p, int handle, read_alloc_pread_ftype *pread_func,
	    int padding, void **memory_to_free_ptr)
{
  size_t buf_alloc, buf_pos;
  gdb_byte *buf;
  LONGEST n;
  int target_errno;

  /* Start by reading up to 4K at a time.  The target will throttle
     this number down if necessary.  */
  buf_alloc = 4096;
  buf = xmalloc (buf_alloc);
  if (memory_to_free_ptr != NULL)
    {
      gdb_assert (*memory_to_free_ptr == NULL);
      *memory_to_free_ptr = buf;
    }
  buf_pos = 0;
  while (1)
    {
      n = pread_func (handle, &buf[buf_pos], buf_alloc - buf_pos - padding,
		      buf_pos, &target_errno);
      if (n <= 0)
	{
	  if (n < 0 || (n == 0 && buf_pos == 0))
	    xfree (buf);
	  else
	    *buf_p = buf;
	  if (memory_to_free_ptr != NULL)
	    *memory_to_free_ptr = NULL;
	  if (n < 0)
	    {
	      /* An error occurred.  */
	      return -1;
	    }
	  else
	    {
	      /* Read all there was.  */
	      return buf_pos;
	    }
	}

      buf_pos += n;

      /* If the buffer is filling up, expand it.  */
      if (buf_alloc < buf_pos * 2)
	{
	  buf_alloc *= 2;
	  buf = xrealloc (buf, buf_alloc);
	  if (memory_to_free_ptr != NULL)
	    *memory_to_free_ptr = buf;
	}
    }
}

char *
read_stralloc (const char *filename, read_stralloc_func_ftype *func)
{
  gdb_byte *buffer;
  char *bufstr;
  LONGEST i, transferred;

  transferred = func (filename, &buffer, 1);
  bufstr = (char *) buffer;

  if (transferred < 0)
    return NULL;

  if (transferred == 0)
    return xstrdup ("");

  bufstr[transferred] = 0;

  /* Check for embedded NUL bytes; but allow trailing NULs.  */
  for (i = strlen (bufstr); i < transferred; i++)
    if (bufstr[i] != 0)
      {
	warning (_("target file %s "
		   "contained unexpected null characters"),
		 filename);
	break;
      }

  return bufstr;
}
