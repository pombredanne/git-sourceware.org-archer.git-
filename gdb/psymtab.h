/* Public partial symbol table definitions.

   Copyright (C) 2009 Free Software Foundation, Inc.

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

struct symtab *find_last_source_symtab_from_partial (struct objfile *);

void forget_cached_source_info_partial (struct objfile *);

struct symtab *lookup_symtab_via_partial_symtab (struct objfile *objfile,
						 const char *name,
						 const char *full_path,
						 const char *real_path);

struct symbol *lookup_symbol_aux_psymtabs (struct objfile *objfile,
					   int block_index, const char *name,
					   const char *linkage_name,
					   const domain_enum domain);

struct symbol *lookup_global_symbol_from_objfile_via_partial
    (const struct objfile *objfile,
     const char *name,
     const char *linkage_name,
     const domain_enum domain);

struct type *basic_lookup_transparent_type_via_partial (struct objfile *objfile,
							const char *name,
							int kind);

void print_psymtab_stats_for_objfile (struct objfile *objfile);

void dump_psymtabs_for_objfile (struct objfile *objfile);

void relocate_psymtabs (struct objfile *objfile,
			struct section_offsets *new_offsets,
			struct section_offsets *delta);

void read_symtabs_for_function (struct objfile *objfile, const char *func_name);

void expand_partial_symbol_tables (struct objfile *objfile, int from_tty);

void find_pc_symtab_from_partial (CORE_ADDR pc);

struct symtab *find_pc_sect_symtab_from_partial (CORE_ADDR pc,
						 struct obj_section *section,
						 int warn_if_readin);

void read_psymtabs_with_filename (struct objfile *objfile,
				  const char *filename);

void map_partial_symbol_names (void (*) (const char *, void *), void *);

void map_partial_symbol_filenames (void (*) (const char *, const char *,
					     void *),
				   void *);

char *find_symbol_file_from_partial (struct objfile *, char *);

#endif /* PSYMTAB_H */
