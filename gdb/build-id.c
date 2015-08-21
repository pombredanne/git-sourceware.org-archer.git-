/* build-id-related functions.

   Copyright (C) 1991-2015 Free Software Foundation, Inc.

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

#include "defs.h"
#include "bfd.h"
#include "gdb_bfd.h"
#include "build-id.h"
#include "gdb_vecs.h"
#include "symfile.h"
#include "objfiles.h"
#include "filenames.h"
#include "gdbcore.h"
#include "gdbcmd.h"
#include "source.h"
#include "rsp-low.h"

/* Boolean for command 'set validate-build-id'.  */
int validate_build_id = 1;

/* Implement 'show validate-build-id'.  */

static void
show_validate_build_id (struct ui_file *file, int from_tty,
			struct cmd_list_element *c, const char *value)
{
  fprintf_filtered (file, _("Validation of build-id match when loading "
			    "a binary is %s.\n"),
		    value);
}

/* See build-id.h.  */

const struct bfd_build_id *
build_id_bfd_get (bfd *abfd)
{
  if (!bfd_check_format (abfd, bfd_object))
    return NULL;

  if (abfd->build_id != NULL)
    return abfd->build_id;

  /* No build-id */
  return NULL;
}

/* See build-id.h.  */

int
build_id_verify (bfd *abfd, size_t check_len, const bfd_byte *check, int advice)
{
  const struct bfd_build_id *found;
  char *message, *check_hex;
  struct cleanup *back_to;

  if (check_len == 0 || !validate_build_id)
    return 1;

  found = build_id_bfd_get (abfd);
  check_hex = alloca (check_len * 2 + 1);
  bin2hex (check, check_hex, check_len);

  if (found == NULL)
    message = xstrprintf (_("inferior build ID is %s but symbol file \"%s\" "
			    "does not have build ID"),
			  check_hex, bfd_get_filename (abfd));
  else if (found->size != check_len
           || memcmp (found->data, check, found->size) != 0)
    {
      char *abfd_hex = alloca (found->size * 2 + 1);

      bin2hex (found->data, abfd_hex, found->size);
      message = xstrprintf (_("inferior build ID %s is not identical to "
			      "symbol file \"%s\" build ID %s"),
			    check_hex, bfd_get_filename (abfd), abfd_hex);
    }
  else
    return 1;
  back_to = make_cleanup (xfree, message);

  if (!advice)
    warning ("%s", message);
  else
    warning (_("Symbol file \"%s\" could not be validated (%s) and "
	       "will be ignored; or use 'set validate-build-id off'."),
	     bfd_get_filename (abfd), message);
  do_cleanups (back_to);
  return 0;
}

/* Find and open a BFD given a build-id.  If no BFD can be found,
   return NULL.  Use "" or ".debug" for SUFFIX.  The returned reference to the
   BFD must be released by the caller.  */

struct file_location
build_id_to_file (size_t build_id_len, const bfd_byte *build_id,
		  const char *suffix, enum openp_flags opts)
{
  char *link, *debugdir;
  VEC (char_ptr) *debugdir_vec;
  struct cleanup *back_to;
  int ix;
  struct file_location result;

  /* DEBUG_FILE_DIRECTORY/.build-id/ab/cdef */
  link = alloca (strlen (debug_file_directory) + (sizeof "/.build-id/" - 1) + 1
		 + 2 * build_id_len + strlen (suffix) + 1);

  /* Keep backward compatibility so that DEBUG_FILE_DIRECTORY being "" will
     cause "/.build-id/..." lookups.  */

  debugdir_vec = dirnames_to_char_ptr_vec (debug_file_directory);
  back_to = make_cleanup_free_char_ptr_vec (debugdir_vec);

  for (ix = 0; VEC_iterate (char_ptr, debugdir_vec, ix, debugdir); ++ix)
    {
      size_t debugdir_len = strlen (debugdir);
      const gdb_byte *data = build_id;
      size_t size = build_id_len;
      char *s;

      memcpy (link, debugdir, debugdir_len);
      s = &link[debugdir_len];
      s += sprintf (s, "/.build-id/");
      if (size > 0)
	{
	  size--;
	  s += sprintf (s, "%02x", (unsigned) *data++);
	}
      if (size > 0)
	*s++ = '/';
      while (size-- > 0)
	s += sprintf (s, "%02x", (unsigned) *data++);
      strcpy (s, suffix);

      result = file_location_from_filename (link, opts | OPF_BFD_CANONICAL,
					    build_id_len, build_id);
      if (file_location_is_valid (&result))
	{
	  do_cleanups (back_to);
	  return result;
	}
      file_location_free (&result);
    }

  file_location_enoent (&result);
  return result;
}

/* See build-id.h.  */

bfd *
build_id_to_debug_bfd (size_t build_id_len, const bfd_byte *build_id)
{
  struct file_location result;
  bfd *retval;

  result = build_id_to_file (build_id_len, build_id, ".debug", OPF_IS_BFD);
  if (result.abfd == NULL)
    {
      file_location_free (&result);
      return NULL;
    }
  gdb_bfd_ref (result.abfd);
  retval = result.abfd;
  file_location_free (&result);
  return retval;
}

/* See build-id.h.  */

char *
find_separate_debug_file_by_buildid (struct objfile *objfile)
{
  const struct bfd_build_id *build_id;

  build_id = build_id_bfd_get (objfile->obfd);
  if (build_id != NULL)
    {
      bfd *abfd;

      abfd = build_id_to_debug_bfd (build_id->size, build_id->data);
      /* Prevent looping on a stripped .debug file.  */
      if (abfd != NULL
	  && filename_cmp (bfd_get_filename (abfd),
			   objfile_name (objfile)) == 0)
        {
	  warning (_("\"%s\": separate debug info file has no debug info"),
		   bfd_get_filename (abfd));
	  gdb_bfd_unref (abfd);
	}
      else if (abfd != NULL)
	{
	  char *result = xstrdup (bfd_get_filename (abfd));

	  gdb_bfd_unref (abfd);
	  return result;
	}
    }
  return NULL;
}

extern initialize_file_ftype _initialize_build_id; /* -Wmissing-prototypes */

void
_initialize_build_id (void)
{
  add_setshow_boolean_cmd ("validate-build-id", class_support,
			   &validate_build_id, _("\
Set whether to validate build-id match when loading a binary."), _("\
Show whether to validate build-id match when loading a binary."), _("\
Inferior binary and symbol file may contain unique build-id.\n\
If both build-ids are present, but they do not match, then this setting\n\
enables (off) or disables (on) loading of such symbol file.\n\
Loading non-matching symbol file may confuse debugging including breakage\n\
of backtrace output."),
			   NULL,
			   show_validate_build_id,
			   &setlist, &showlist);
}
