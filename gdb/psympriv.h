/* Private partial symbol table definitions.

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

#ifndef PSYMPRIV_H
#define PSYMPRIV_H

#include "psymtab.h"

/* A partial_symbol records the name, domain, and address class of
   symbols whose types we have not parsed yet.  For functions, it also
   contains their memory address, so we can find them from a PC value.
   Each partial_symbol sits in a partial_symtab, all of which are chained
   on a  partial symtab list and which points to the corresponding 
   normal symtab once the partial_symtab has been referenced.  */

/* This structure is space critical.  See space comments at the top. */

struct partial_symbol
{

  /* The general symbol info required for all types of symbols. */

  struct general_symbol_info ginfo;

  /* Name space code.  */

  ENUM_BITFIELD(domain_enum_tag) domain : 6;

  /* Address class (for info_symbols) */

  ENUM_BITFIELD(address_class) aclass : 6;

};

#define PSYMBOL_DOMAIN(psymbol)	(psymbol)->domain
#define PSYMBOL_CLASS(psymbol)		(psymbol)->aclass

/* Each source file that has not been fully read in is represented by
   a partial_symtab.  This contains the information on where in the
   executable the debugging symbols for a specific file are, and a
   list of names of global symbols which are located in this file.
   They are all chained on partial symtab lists.

   Even after the source file has been read into a symtab, the
   partial_symtab remains around.  They are allocated on an obstack,
   objfile_obstack.  FIXME, this is bad for dynamic linking or VxWorks-
   style execution of a bunch of .o's.  */

struct partial_symtab
{

  /* Chain of all existing partial symtabs.  */

  struct partial_symtab *next;

  /* Name of the source file which this partial_symtab defines */

  char *filename;

  /* Full path of the source file.  NULL if not known.  */

  char *fullname;

  /* Directory in which it was compiled, or NULL if we don't know.  */

  char *dirname;

  /* Information about the object file from which symbols should be read.  */

  struct objfile *objfile;

  /* Set of relocation offsets to apply to each section.  */

  struct section_offsets *section_offsets;

  /* Range of text addresses covered by this file; texthigh is the
     beginning of the next section. */

  CORE_ADDR textlow;
  CORE_ADDR texthigh;

  /* Array of pointers to all of the partial_symtab's which this one
     depends on.  Since this array can only be set to previous or
     the current (?) psymtab, this dependency tree is guaranteed not
     to have any loops.  "depends on" means that symbols must be read
     for the dependencies before being read for this psymtab; this is
     for type references in stabs, where if foo.c includes foo.h, declarations
     in foo.h may use type numbers defined in foo.c.  For other debugging
     formats there may be no need to use dependencies.  */

  struct partial_symtab **dependencies;

  int number_of_dependencies;

  /* Global symbol list.  This list will be sorted after readin to
     improve access.  Binary search will be the usual method of
     finding a symbol within it. globals_offset is an integer offset
     within global_psymbols[].  */

  int globals_offset;
  int n_global_syms;

  /* Static symbol list.  This list will *not* be sorted after readin;
     to find a symbol in it, exhaustive search must be used.  This is
     reasonable because searches through this list will eventually
     lead to either the read in of a files symbols for real (assumed
     to take a *lot* of time; check) or an error (and we don't care
     how long errors take).  This is an offset and size within
     static_psymbols[].  */

  int statics_offset;
  int n_static_syms;

  /* Pointer to symtab eventually allocated for this source file, 0 if
     !readin or if we haven't looked for the symtab after it was readin.  */

  struct symtab *symtab;

  /* Pointer to function which will read in the symtab corresponding to
     this psymtab.  */

  void (*read_symtab) (struct partial_symtab *);

  /* Information that lets read_symtab() locate the part of the symbol table
     that this psymtab corresponds to.  This information is private to the
     format-dependent symbol reading routines.  For further detail examine
     the various symbol reading modules.  Should really be (void *) but is
     (char *) as with other such gdb variables.  (FIXME) */

  char *read_symtab_private;

  /* Non-zero if the symtab corresponding to this psymtab has been readin */

  unsigned char readin;
};

/* A fast way to get from a psymtab to its symtab (after the first time).  */
#define	PSYMTAB_TO_SYMTAB(pst)  \
    ((pst) -> symtab != NULL ? (pst) -> symtab : psymtab_to_symtab (pst))


void dump_psymtab (struct objfile *objfile, struct partial_symtab *psymtab,
		   struct ui_file *outfile);

struct objfile *require_partial_symbols (struct objfile *);

/* lookup partial symbol table by filename */

/* extern struct partial_symtab *lookup_partial_symtab (const char *); */

/* lookup partial symbol table by address and section */

extern struct partial_symtab *find_pc_sect_psymtab (CORE_ADDR,
						    struct obj_section *);

/* lookup partial symbol by address */

extern struct partial_symbol *find_pc_psymbol (struct partial_symtab *,
					       CORE_ADDR);

/* lookup partial symbol by address and section */

extern struct partial_symbol *find_pc_sect_psymbol (struct partial_symtab *,
						    CORE_ADDR,
						    struct obj_section *);

extern struct symtab *psymtab_to_symtab (struct partial_symtab *);

extern struct partial_symbol *fixup_psymbol_section (struct partial_symbol
						     *psym,
						     struct objfile *objfile);

extern void sort_pst_symbols (struct partial_symtab *);

extern char* psymtab_to_fullname (struct partial_symtab *ps);

/* Traverse all psymtabs in one objfile.  */

#define	ALL_OBJFILE_PSYMTABS(objfile, p) \
    for ((p) = (objfile) -> psymtabs; (p) != NULL; (p) = (p) -> next)

/* Traverse all psymtabs in all objfiles.  */

#define	ALL_PSYMTABS(objfile, p) \
  ALL_OBJFILES (objfile)	 \
    ALL_OBJFILE_PSYMTABS (objfile, p)

/* Like ALL_PSYMTABS, but ensure that partial symbols have been read
   before examining the objfile.  */

#define ALL_PSYMTABS_REQUIRED(objfile, p)			\
  ALL_OBJFILES (objfile)					\
    ALL_OBJFILE_PSYMTABS (require_partial_symbols (objfile), p)

#endif /* PSYMPRIV_H */
