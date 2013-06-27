/* Very simple "bfd" target, for GDB, the GNU debugger.

   Copyright (C) 2003-2013 Free Software Foundation, Inc.

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
#include "target.h"
#include "bfd-target.h"
#include "exec.h"
#include "gdb_bfd.h"

/* The extra data that is stored in the gdb_target subclass has this
   type.  */
struct target_bfd_data
{
  /* The BFD we're wrapping.  */
  struct bfd *bfd;

  /* The section table build from the ALLOC sections in BFD.  Note
     that we can't rely on extracting the BFD from a random section in
     the table, since the table can be legitimately empty.  */
  struct target_section_table table;
};

/* A subclass of gdb_target that also carries the per-BFD-target
   data.  */

struct gdb_target_bfd_subclass
{
  /* The base class.  */

  struct gdb_target base;

  /* Local data.  */

  struct target_bfd_data data;
};

static struct target_ops target_bfd_ops;

/* Fetch the DATA field from gdb_target.  */

static struct target_bfd_data *
get_bfd_target_data (struct gdb_target *ops)
{
  struct gdb_target_bfd_subclass *self;

  /* Downcast.  */
  self = (struct gdb_target_bfd_subclass *) ops;
  return &self->data;
}

static LONGEST
target_bfd_xfer_partial (struct gdb_target *ops,
			 enum target_object object,
			 const char *annex, gdb_byte *readbuf,
			 const gdb_byte *writebuf,
			 ULONGEST offset, LONGEST len)
{
  switch (object)
    {
    case TARGET_OBJECT_MEMORY:
      {
	struct target_bfd_data *data = get_bfd_target_data (ops);

	return section_table_xfer_memory_partial (readbuf, writebuf,
						  offset, len,
						  data->table.sections,
						  data->table.sections_end,
						  NULL);
      }
    default:
      return -1;
    }
}

static struct target_section_table *
target_bfd_get_section_table (struct gdb_target *ops)
{
  struct target_bfd_data *data = get_bfd_target_data (ops);
  return &data->table;
}

static void
target_bfd_xclose (struct gdb_target *t)
{
  struct target_bfd_data *data = get_bfd_target_data (t);

  gdb_bfd_unref (data->bfd);
  xfree (data->table.sections);
  xfree (t);
}

struct gdb_target *
target_bfd_reopen (struct bfd *abfd)
{
  struct gdb_target_bfd_subclass *t;
  struct target_bfd_data *data;

  t = XZALLOC (struct gdb_target_bfd_subclass);

  t->base.ops = &target_bfd_ops; /* FIXME */
  t->data.bfd = abfd;
  gdb_bfd_ref (abfd);
  build_section_table (abfd, &t->data.table.sections,
		       &t->data.table.sections_end);

  return &t->base;
}

extern initialize_file_ftype _initialize_bfd_target;

void
_initialize_bfd_target (void)
{
  target_bfd_ops.to_shortname = "bfd";
  target_bfd_ops.to_longname = _("BFD backed target");
  target_bfd_ops.to_doc = _("You should never see this");
  target_bfd_ops.to_get_section_table = target_bfd_get_section_table;
  target_bfd_ops.to_xfer_partial = target_bfd_xfer_partial;
  target_bfd_ops.to_xclose = target_bfd_xclose;
  target_bfd_ops.to_magic = OPS_MAGIC;
}
