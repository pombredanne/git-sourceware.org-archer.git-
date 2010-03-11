/* Private partial symbol table definitions.

   Copyright (C) 2009, 2010 Free Software Foundation, Inc.

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

/* This structure is space critical.  See space comments at the top of
   symtab.h. */

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
   objfile_obstack.  */

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
     the various symbol reading modules.  */

  void *read_symtab_private;

  /* Non-zero if the symtab corresponding to this psymtab has been readin */

  unsigned char readin;
};

/* Traverse all psymtabs in one objfile.  */

#define	ALL_OBJFILE_PSYMTABS(objfile, p) \
    for ((p) = (objfile)->psyms->psymtabs; (p) != NULL; (p) = (p) -> next)

/* Traverse all psymtabs in all objfiles.  */

#define	ALL_PSYMTABS(objfile, p) \
  ALL_OBJFILES (objfile)	 \
    ALL_OBJFILE_PSYMTABS (objfile, p)

/* Like ALL_PSYMTABS, but ensure that partial symbols have been read
   before examining the objfile.  */

#define ALL_PSYMTABS_REQUIRED(objfile, p)			\
  ALL_OBJFILES (objfile)					\
    ALL_OBJFILE_PSYMTABS (require_partial_symbols (objfile), p)


/* Partial symbols are stored in the psymbol_cache and pointers to
   them are kept in a dynamically grown array that is obtained from
   malloc and grown as necessary via realloc.  Each objfile typically
   has two of these, one for global symbols and one for static
   symbols.  Although this adds a level of indirection for storing or
   accessing the partial symbols, it allows us to throw away duplicate
   psymbols and set all pointers to the single saved instance.  */

struct psymbol_allocation_list
{

  /* Pointer to beginning of dynamically allocated array of pointers
     to partial symbols.  The array is dynamically expanded as
     necessary to accommodate more pointers.  */

  struct partial_symbol **list;

  /* Pointer to next available slot in which to store a pointer to a
     partial symbol.  */

  struct partial_symbol **next;

  /* Number of allocated pointer slots in current dynamic array (not
     the number of bytes of storage).  The "next" pointer will always
     point somewhere between list[0] and list[size], and when at
     list[size] the array will be expanded on the next attempt to
     store a pointer.  */

  int size;
};

/* Partial symbols and partial symbol tables are managed using an
   instance of this structure.  In normal use, a structure is
   allocated on an objfile's obstack and assigned to the objfile's
   'psyms' field.  */
struct psymtab_state
{
  int writeable;

  /* The obstack on which psymbols are allocated.  */
  struct obstack *obstack;

  /* Vectors of all partial symbols read in from file.  The actual data
     is stored in the objfile_obstack. */
  struct psymbol_allocation_list global_psymbols;
  struct psymbol_allocation_list static_psymbols;

  /* Each objfile points to a linked list of partial symtabs derived from
     this file, one partial symtab structure for each compilation unit
     (source file). */
  struct partial_symtab *psymtabs;

  /* Map addresses to the entries of PSYMTABS.  It would be more efficient to
     have a map per the whole process but ADDRMAP cannot selectively remove
     its items during FREE_OBJFILE.  This mapping is already present even for
     PARTIAL_SYMTABs which still have no corresponding full SYMTABs read.  */
  struct addrmap *psymtabs_addrmap;

  /* List of freed partial symtabs, available for re-use */
  struct partial_symtab *free_psymtabs;

  /* Byte cache for partial syms.  */
  struct bcache *psymbol_cache;
};


/* Sort the global symbols associated with PST which are held in
   STATE.  */
void sort_pst_symbols (struct psymtab_state *state,
		       struct partial_symtab *pst);

/* Allocate a partial symbol table in STATE.  FILENAME and OBJFILE are
   the file name and objfile associated with the new table.  */
struct partial_symtab *allocate_psymtab_full (struct psymtab_state *state,
					      char *filename,
					      struct objfile *objfile);

/* A legacy function that calls allocate_psymtab_full using the
   objfile's psymtab_state.  */
struct partial_symtab *allocate_psymtab (char *, struct objfile *);

/* Discard a partial symbol table.  */
void discard_psymtab (struct psymtab_state *state, struct partial_symtab *);

/* Add any kind of symbol to a psymbol_allocation_list.  */
const struct partial_symbol *add_psymbol_to_list_full
    (struct psymtab_state *state,
     char *name, int namelength, int copy_name, domain_enum domain,
     enum address_class class,
     long val,	/* Value as a long */
     CORE_ADDR coreaddr,	/* Value as a CORE_ADDR */
     enum language language, struct objfile *objfile,
     int is_global);

/* A legacy function that calls add_psymbol_to_list_full using the
   objfile's psymtab_state.  */
const struct partial_symbol *add_psymbol_to_list
    (char *, int, int, domain_enum,
     enum address_class,
     int,
     long, CORE_ADDR,
     enum language, struct objfile *);

/* Allocate and partially fill a partial symtab.  It will be
   completely filled at the end of the symbol list.

   STATE is the psymtab state object holding all the necessary state.
   OBJFILE is the objfile associated with the symbol table.
   FILENAME is the name of the symbol-file we are reading from.  */
struct partial_symtab *start_psymtab_common_full
    (struct psymtab_state *state,
     struct objfile *objfile,
     struct section_offsets *section_offsets,
     char *filename,
     CORE_ADDR textlow);

/* A legacy function that calls start_psymtab_common_full with the
   objfile's psymtab_state.  */
struct partial_symtab *start_psymtab_common
    (struct objfile *objfile,
     struct section_offsets *section_offsets, char *filename,
     CORE_ADDR textlow);

/* Set n_global_syms and n_static_syms on PSYMTAB.  */
void finalize_psymtab (struct psymtab_state *state,
		       struct partial_symtab *psymtab);

/* Change the obstack associated with a psymtab_state.  STATE is the
   psymtab_state, which must be writeable.  OBSTACK is the new
   obstack.  */
void switch_psymtab_state_obstack (struct psymtab_state *state,
				   struct obstack *new_obstack);

/* A readonly psymtab_state.  This is temporarily put into an
   objfile's psyms slot when reading psymtabs in a background
   thread.  */
extern struct psymtab_state readonly_psymtab_state;

#endif /* PSYMPRIV_H */
