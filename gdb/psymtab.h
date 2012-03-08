/* Public partial symbol table definitions.

   Copyright (C) 2009-2012 Free Software Foundation, Inc.

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

#ifndef PSYMTAB_H
#define PSYMTAB_H

#include "symfile.h"

/* This structure holds all the information related to partial symbols
   for a given objfile.  This might be allocated in the per-BFD
   storage, or it might be allocated on the objfile obstack, depending
   on the particular objfile.  */

struct partial_symbol_info
{
  /* The obstack used for all psymbol allocation.  */
  struct obstack *psym_obstack;

  /* A linked list of partial symtabs derived from this file, one
     partial symtab structure for each compilation unit (source
     file).  */

  struct partial_symtab *psymtabs;

  /* Map addresses to the entries of PSYMTABS.  It would be more efficient to
     have a map per the whole process but ADDRMAP cannot selectively remove
     its items during FREE_OBJFILE.  This mapping is already present even for
     PARTIAL_SYMTABs which still have no corresponding full SYMTABs read.  */

  struct addrmap *psymtabs_addrmap;

  /* List of freed partial symtabs, available for re-use.  */

  struct partial_symtab *free_psymtabs;

  /* Byte cache for partial syms.  */
  struct psymbol_bcache *psymbol_cache;

  /* Vectors of all partial symbols read in from file.  The actual
     data is stored in the psym_obstack.  */

  struct psymbol_allocation_list global_psymbols;
  struct psymbol_allocation_list static_psymbols;
};

/* Allocate a new partial_symbol_info on OBSTACK and return it.  */

extern struct partial_symbol_info *new_partial_symbol_info (struct obstack *);

/* Free a partial_symbol_info.  */

extern void free_partial_symbol_info (struct partial_symbol_info *);

/* A bcache for partial symbols.  */

struct psymbol_bcache;

extern struct psymbol_bcache *psymbol_bcache_init (void);
extern void psymbol_bcache_free (struct psymbol_bcache *);
extern struct bcache *psymbol_bcache_get_bcache (struct psymbol_bcache *);

void expand_partial_symbol_names (int (*fun) (const char *, void *),
				  void *data);

void map_partial_symbol_filenames (symbol_filename_ftype *fun, void *data,
				   int need_fullname);

extern const struct quick_symbol_functions psym_functions;

extern const struct quick_symbol_functions dwarf2_gdb_index_functions;

/* Ensure that the partial symbols for OBJFILE have been loaded.  If
   VERBOSE is non-zero, then this will print a message when symbols
   are loaded.  This function always returns its argument, as a
   convenience.  */

extern struct objfile *require_partial_symbols (struct objfile *objfile,
						int verbose);

#endif /* PSYMTAB_H */
