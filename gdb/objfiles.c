/* GDB routines for manipulating objfiles.

   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003, 2004, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.

   Contributed by Cygnus Support, using pieces from other GDB modules.

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

/* This file contains support routines for creating, manipulating, and
   destroying objfile structures. */

#include "defs.h"
#include "bfd.h"		/* Binary File Description */
#include "symtab.h"
#include "symfile.h"
#include "objfiles.h"
#include "gdb-stabs.h"
#include "target.h"
#include "bcache.h"
#include "mdebugread.h"
#include "expression.h"
#include "parser-defs.h"

#include "gdb_assert.h"
#include <sys/types.h>
#include "gdb_stat.h"
#include <fcntl.h>
#include "gdb_obstack.h"
#include "gdb_string.h"
#include "hashtab.h"

#include "breakpoint.h"
#include "block.h"
#include "dictionary.h"
#include "source.h"
#include "addrmap.h"
#include "arch-utils.h"
#include "exec.h"
#include "observer.h"
#include "complaints.h"
#include "psymtab.h"
#include "solist.h"

/* Prototypes for local functions */

static void objfile_alloc_data (struct objfile *objfile);
static void objfile_free_data (struct objfile *objfile);
static struct objfile_list *unlink_objfile (struct objfile *objfile);

/* Externally visible variables that are owned by this module.
   See declarations in objfile.h for more info. */

struct objfile *rt_common_objfile;	/* For runtime common symbols */

struct objfile_pspace_info
{
  int objfiles_changed_p;
  struct obj_section **sections;
  int num_sections;
};

/* Per-program-space data key.  */
static const struct program_space_data *objfiles_pspace_data;

static void
objfiles_pspace_data_cleanup (struct program_space *pspace, void *arg)
{
  struct objfile_pspace_info *info;

  info = program_space_data (pspace, objfiles_pspace_data);
  if (info != NULL)
    {
      xfree (info->sections);
      xfree (info);
    }
}

/* Get the current svr4 data.  If none is found yet, add it now.  This
   function always returns a valid object.  */

static struct objfile_pspace_info *
get_objfile_pspace_data (struct program_space *pspace)
{
  struct objfile_pspace_info *info;

  info = program_space_data (pspace, objfiles_pspace_data);
  if (info == NULL)
    {
      info = XZALLOC (struct objfile_pspace_info);
      set_program_space_data (pspace, objfiles_pspace_data, info);
    }

  return info;
}

/* Records whether any objfiles appeared or disappeared since we last updated
   address to obj section map.  */

/* Locate all mappable sections of a BFD file. 
   objfile_p_char is a char * to get it through
   bfd_map_over_sections; we cast it back to its proper type.  */

/* Called via bfd_map_over_sections to build up the section table that
   the objfile references.  The objfile contains pointers to the start
   of the table (objfile->sections) and to the first location after
   the end of the table (objfile->sections_end). */

static void
add_to_objfile_sections (struct bfd *abfd, struct bfd_section *asect,
			 void *objfile_p_char)
{
  struct objfile *objfile = (struct objfile *) objfile_p_char;
  struct obj_section section;
  flagword aflag;

  aflag = bfd_get_section_flags (abfd, asect);

  if (!(aflag & SEC_ALLOC))
    return;

  if (0 == bfd_section_size (abfd, asect))
    return;
  section.objfile = objfile;
  section.the_bfd_section = asect;
  section.ovly_mapped = 0;
  obstack_grow (&OBJFILE_OBSTACK (objfile), (char *) &section, sizeof (section));
  OBJFILE_SECTIONS_END (objfile)
    = (struct obj_section *) (((size_t) OBJFILE_SECTIONS_END (objfile)) + 1);
}

/* Builds a section table for OBJFILE.
   Returns 0 if OK, 1 on error (in which case bfd_error contains the
   error).

   Note that while we are building the table, which goes into the
   psymbol obstack, we hijack the sections_end pointer to instead hold
   a count of the number of sections.  When bfd_map_over_sections
   returns, this count is used to compute the pointer to the end of
   the sections table, which then overwrites the count.

   Also note that the OFFSET and OVLY_MAPPED in each table entry
   are initialized to zero.

   Also note that if anything else writes to the psymbol obstack while
   we are building the table, we're pretty much hosed. */

int
build_objfile_section_table (struct objfile *objfile)
{
  /* objfile->sections can be already set when reading a mapped symbol
     file.  I believe that we do need to rebuild the section table in
     this case (we rebuild other things derived from the bfd), but we
     can't free the old one (it's in the objfile_obstack).  So we just
     waste some memory.  */

  OBJFILE_SECTIONS_END (objfile) = 0;
  bfd_map_over_sections (OBJFILE_OBFD (objfile),
			 add_to_objfile_sections, (void *) objfile);
  OBJFILE_SECTIONS (objfile) = obstack_finish (&OBJFILE_OBSTACK (objfile));
  OBJFILE_SECTIONS_END (objfile) = OBJFILE_SECTIONS (objfile) + (size_t) OBJFILE_SECTIONS_END (objfile);
  return (0);
}

/* Given a pointer to an initialized bfd (ABFD) and some flag bits
   allocate a new objfile struct, fill it in as best we can, link it
   into the list of all known objfiles, and return a pointer to the
   new objfile struct.

   The FLAGS word contains various bits (OBJF_*) that can be taken as
   requests for specific operations.  Other bits like OBJF_SHARED are
   simply copied through to the new objfile flags member. */

/* NOTE: carlton/2003-02-04: This function is called with args NULL, 0
   by jv-lang.c, to create an artificial objfile used to hold
   information about dynamically-loaded Java classes.  Unfortunately,
   that branch of this function doesn't get tested very frequently, so
   it's prone to breakage.  (E.g. at one time the name was set to NULL
   in that situation, which broke a loop over all names in the dynamic
   library loader.)  If you change this function, please try to leave
   things in a consistent state even if abfd is NULL.  */

struct objfile *
allocate_objfile (bfd *abfd, int flags)
{
  struct objfile *objfile;
  struct objfile_list *olist, **iter;

  objfile = (struct objfile *) xzalloc (sizeof (struct objfile));
  objfile->storage = XCNEW (struct objfile_storage);
  OBJFILE_REFC (objfile) = 1;
  OBJFILE_PSYMBOL_CACHE (objfile) = bcache_xmalloc ();
  OBJFILE_MACRO_CACHE (objfile) = bcache_xmalloc ();
  OBJFILE_FILENAME_CACHE (objfile) = bcache_xmalloc ();
  /* We could use obstack_specify_allocation here instead, but
     gdb_obstack.h specifies the alloc/dealloc functions.  */
  obstack_init (&OBJFILE_OBSTACK (objfile));
  terminate_minimal_symbol_table (objfile);

  objfile_alloc_data (objfile);

  /* Update the per-objfile information that comes from the bfd, ensuring
     that any data that is reference is saved in the per-objfile data
     region. */

  OBJFILE_OBFD (objfile) = gdb_bfd_ref (abfd);
  if (OBJFILE_NAME (objfile) != NULL)
    {
      xfree (OBJFILE_NAME (objfile));
    }
  if (abfd != NULL)
    {
      /* Look up the gdbarch associated with the BFD.  */
      OBJFILE_GDBARCH (objfile) = gdbarch_from_bfd (abfd);

      OBJFILE_NAME (objfile) = xstrdup (bfd_get_filename (abfd));
      OBJFILE_MTIME (objfile) = bfd_get_mtime (abfd);

      /* Build section table.  */

      if (build_objfile_section_table (objfile))
	{
	  error (_("Can't find the file sections in `%s': %s"),
		 OBJFILE_NAME (objfile), bfd_errmsg (bfd_get_error ()));
	}
    }
  else
    {
      OBJFILE_NAME (objfile) = xstrdup ("<<anonymous objfile>>");
    }

  /* Initialize the section indexes for this objfile, so that we can
     later detect if they are used w/o being properly assigned to. */

  OBJFILE_SECT_INDEX_TEXT (objfile) = -1;
  OBJFILE_SECT_INDEX_DATA (objfile) = -1;
  OBJFILE_SECT_INDEX_BSS (objfile) = -1;
  OBJFILE_SECT_INDEX_RODATA (objfile) = -1;

  /* We don't yet have a C++-specific namespace symtab.  */

  OBJFILE_CP_NAMESPACE_SYMTAB (objfile) = NULL;

  /* Add this file onto the tail of the linked list of other such files. */

  olist = XNEW (struct objfile_list);
  olist->next = NULL;
  olist->objfile = objfile;

  for (iter = &object_files; *iter != NULL; iter = &(*iter)->next)
    ;
  *iter = olist;

  /* Save passed in flag bits. */
  OBJFILE_FLAGS (objfile) |= flags;

  /* Rebuild section map next time we need it.  */
  get_objfile_pspace_data (current_program_space)->objfiles_changed_p = 1;

  return objfile;
}

/* Retrieve the gdbarch associated with OBJFILE.  */
struct gdbarch *
get_objfile_arch (struct objfile *objfile)
{
  return OBJFILE_GDBARCH (objfile);
}

/* Initialize entry point information for this objfile. */

void
init_entry_point_info (struct objfile *objfile)
{
  /* Save startup file's range of PC addresses to help blockframe.c
     decide where the bottom of the stack is.  */

  if (bfd_get_file_flags (OBJFILE_OBFD (objfile)) & EXEC_P)
    {
      /* Executable file -- record its entry point so we'll recognize
         the startup file because it contains the entry point.  */
      OBJFILE_EI (objfile).entry_point = bfd_get_start_address (OBJFILE_OBFD (objfile));
      OBJFILE_EI (objfile).entry_point_p = 1;
    }
  else if (bfd_get_file_flags (OBJFILE_OBFD (objfile)) & DYNAMIC
	   && bfd_get_start_address (OBJFILE_OBFD (objfile)) != 0)
    {
      /* Some shared libraries may have entry points set and be
	 runnable.  There's no clear way to indicate this, so just check
	 for values other than zero.  */
      OBJFILE_EI (objfile).entry_point = bfd_get_start_address (OBJFILE_OBFD (objfile));    
      OBJFILE_EI (objfile).entry_point_p = 1;
    }
  else
    {
      /* Examination of non-executable.o files.  Short-circuit this stuff.  */
      OBJFILE_EI (objfile).entry_point_p = 0;
    }
}

/* If there is a valid and known entry point, function fills *ENTRY_P with it
   and returns non-zero; otherwise it returns zero.  */

int
entry_point_address_query (CORE_ADDR *entry_p)
{
  struct gdbarch *gdbarch;
  CORE_ADDR entry_point;

  if (symfile_objfile == NULL || !OBJFILE_EI (symfile_objfile).entry_point_p)
    return 0;

  gdbarch = get_objfile_arch (symfile_objfile);

  entry_point = OBJFILE_EI (symfile_objfile).entry_point;

  /* Make certain that the address points at real code, and not a
     function descriptor.  */
  entry_point = gdbarch_convert_from_func_ptr_addr (gdbarch, entry_point,
						    &current_target);

  /* Remove any ISA markers, so that this matches entries in the
     symbol table.  */
  entry_point = gdbarch_addr_bits_remove (gdbarch, entry_point);

  *entry_p = entry_point;
  return 1;
}

/* Get current entry point address.  Call error if it is not known.  */

CORE_ADDR
entry_point_address (void)
{
  CORE_ADDR retval;

  if (!entry_point_address_query (&retval))
    error (_("Entry point address is not known."));

  return retval;
}

/* Create the terminating entry of OBJFILE's minimal symbol table.
   If OBJFILE->msymbols is zero, allocate a single entry from
   OBJFILE->objfile_obstack; otherwise, just initialize
   OBJFILE->msymbols[OBJFILE->minimal_symbol_count].  */
void
terminate_minimal_symbol_table (struct objfile *objfile)
{
  if (! OBJFILE_MSYMBOLS (objfile))
    OBJFILE_MSYMBOLS (objfile) = ((struct minimal_symbol *)
                         obstack_alloc (&OBJFILE_OBSTACK (objfile),
                                        sizeof (OBJFILE_MSYMBOLS (objfile)[0])));

  {
    struct minimal_symbol *m
      = &OBJFILE_MSYMBOLS (objfile)[OBJFILE_MINIMAL_SYMBOL_COUNT (objfile)];

    memset (m, 0, sizeof (*m));
    /* Don't rely on these enumeration values being 0's.  */
    MSYMBOL_TYPE (m) = mst_unknown;
    SYMBOL_INIT_LANGUAGE_SPECIFIC (m, language_unknown);
  }
}

/* Iterator on PARENT and every separate debug objfile of PARENT.
   The usage pattern is:
     for (objfile = parent;
          objfile;
          objfile = objfile_separate_debug_iterate (parent, objfile))
       ...
*/

struct objfile *
objfile_separate_debug_iterate (const struct objfile *parent,
                                const struct objfile *objfile)
{
  struct objfile *res;

  /* If any, return the first child.  */
  res = OBJFILE_SEPARATE_DEBUG_OBJFILE (objfile);
  if (res)
    return res;

  /* Common case where there is no separate debug objfile.  */
  if (objfile == parent)
    return NULL;

  /* Return the brother if any.  Note that we don't iterate on brothers of
     the parents.  */
  res = OBJFILE_SEPARATE_DEBUG_OBJFILE_LINK (objfile);
  if (res)
    return res;

  for (res = OBJFILE_SEPARATE_DEBUG_OBJFILE_BACKLINK (objfile);
       res != parent;
       res = OBJFILE_SEPARATE_DEBUG_OBJFILE_BACKLINK (res))
    {
      gdb_assert (res != NULL);
      if (OBJFILE_SEPARATE_DEBUG_OBJFILE_LINK (res))
        return OBJFILE_SEPARATE_DEBUG_OBJFILE_LINK (res);
    }
  return NULL;
}

/* Put one object file before a specified on in the global list.
   This can be used to make sure an object file is destroyed before
   another when using ALL_OBJFILES_SAFE to free all objfiles. */
static void
put_objfile_before (struct objfile *objfile, struct objfile *before_this)
{
  struct objfile_list *iter, *prev, *objfile_wrapper;

  objfile_wrapper = unlink_objfile (objfile);
  
  prev = NULL;
  for (iter = object_files; iter != NULL; prev = iter, iter = iter->next)
    {
      if (iter->objfile == before_this)
	{
	  if (prev)
	    prev->next = objfile_wrapper;
	  else
	    object_files = objfile_wrapper;
	  objfile_wrapper->next = iter;
	  return;
	}
    }
  
  internal_error (__FILE__, __LINE__,
		  _("put_objfile_before: before objfile not in list"));
}

/* Unlink OBJFILE from the list of known objfiles, if it is found in the
   list.

   If OBJFILE turns out to be in the list, we zap it's NEXT pointer after
   unlinking it, just to ensure that we have completely severed any linkages
   between the OBJFILE and the list. */

static struct objfile_list *
unlink_objfile (struct objfile *objfile)
{
  struct objfile_list *iter, *prev;

  prev = NULL;
  for (iter = object_files; iter != NULL; prev = iter, iter = iter->next)
    {
      if (iter->objfile == objfile)
	{
	  if (prev)
	    prev->next = iter->next;
	  else
	    object_files = iter->next;
	  return iter;
	}
    }

  internal_error (__FILE__, __LINE__,
		  _("unlink_objfile: objfile already unlinked"));
}

/* Add OBJFILE as a separate debug objfile of PARENT.  */

void
add_separate_debug_objfile (struct objfile *objfile, struct objfile *parent)
{
  gdb_assert (objfile && parent);

  /* Must not be already in a list.  */
  gdb_assert (OBJFILE_SEPARATE_DEBUG_OBJFILE_BACKLINK (objfile) == NULL);
  gdb_assert (OBJFILE_SEPARATE_DEBUG_OBJFILE_LINK (objfile) == NULL);

  OBJFILE_SEPARATE_DEBUG_OBJFILE_BACKLINK (objfile) = parent;
  OBJFILE_SEPARATE_DEBUG_OBJFILE_LINK (objfile) = OBJFILE_SEPARATE_DEBUG_OBJFILE (parent);
  OBJFILE_SEPARATE_DEBUG_OBJFILE (parent) = objfile;

  /* Put the separate debug object before the normal one, this is so that
     usage of the ALL_OBJFILES_SAFE macro will stay safe. */
  put_objfile_before (objfile, parent);
}

/* Free all separate debug objfile of OBJFILE, but don't free OBJFILE
   itself.  */

void
free_objfile_separate_debug (struct objfile *objfile)
{
  struct objfile *child;

  for (child = OBJFILE_SEPARATE_DEBUG_OBJFILE (objfile); child;)
    {
      struct objfile *next_child = OBJFILE_SEPARATE_DEBUG_OBJFILE_LINK (child);
      free_objfile (child);
      child = next_child;
    }
}

/* Destroy an objfile and all the symtabs and psymtabs under it.  Note
   that as much as possible is allocated on the objfile_obstack 
   so that the memory can be efficiently freed.

   Things which we do NOT free because they are not in malloc'd memory
   or not in memory specific to the objfile include:

   objfile -> sf

   FIXME:  If the objfile is using reusable symbol information (via mmalloc),
   then we need to take into account the fact that more than one process
   may be using the symbol information at the same time (when mmalloc is
   extended to support cooperative locking).  When more than one process
   is using the mapped symbol info, we need to be more careful about when
   we free objects in the reusable area. */

static void
do_free_objfile (struct objfile *objfile)
{
  if (OBJFILE_SEPARATE_DEBUG_OBJFILE_BACKLINK (objfile))
    {
      /* We freed the separate debug file, make sure the base objfile
	 doesn't reference it.  */
      struct objfile *child;

      child = OBJFILE_SEPARATE_DEBUG_OBJFILE (OBJFILE_SEPARATE_DEBUG_OBJFILE_BACKLINK (objfile));

      if (child == objfile)
        {
          /* OBJFILE is the first child.  */
          OBJFILE_SEPARATE_DEBUG_OBJFILE (OBJFILE_SEPARATE_DEBUG_OBJFILE_BACKLINK (objfile)) =
            OBJFILE_SEPARATE_DEBUG_OBJFILE_LINK (objfile);
        }
      else
        {
          /* Find OBJFILE in the list.  */
          while (1)
            {
              if (OBJFILE_SEPARATE_DEBUG_OBJFILE_LINK (child) == objfile)
                {
                  OBJFILE_SEPARATE_DEBUG_OBJFILE_LINK (child) =
                    OBJFILE_SEPARATE_DEBUG_OBJFILE_LINK (objfile);
                  break;
                }
              child = OBJFILE_SEPARATE_DEBUG_OBJFILE_LINK (child);
              gdb_assert (child);
            }
        }
    }
  
  /* Remove any references to this objfile in the global value
     lists.  */
  preserve_values (objfile);

  /* First do any symbol file specific actions required when we are
     finished with a particular symbol file.  Note that if the objfile
     is using reusable symbol information (via mmalloc) then each of
     these routines is responsible for doing the correct thing, either
     freeing things which are valid only during this particular gdb
     execution, or leaving them to be reused during the next one. */

  if (OBJFILE_SF (objfile) != NULL)
    {
      (*OBJFILE_SF (objfile)->sym_finish) (objfile);
    }

  /* Discard any data modules have associated with the objfile.  */
  objfile_free_data (objfile);

  gdb_bfd_unref (OBJFILE_OBFD (objfile));

  if (objfile == symfile_objfile)
    symfile_objfile = NULL;

  if (objfile == rt_common_objfile)
    rt_common_objfile = NULL;

  /* Before the symbol table code was redone to make it easier to
     selectively load and remove information particular to a specific
     linkage unit, gdb used to do these things whenever the monolithic
     symbol table was blown away.  How much still needs to be done
     is unknown, but we play it safe for now and keep each action until
     it is shown to be no longer needed. */

  /* Not all our callers call clear_symtab_users (objfile_purge_solibs,
     for example), so we need to call this here.  */
  clear_pc_function_cache ();

  /* Clear globals which might have pointed into a removed objfile.
     FIXME: It's not clear which of these are supposed to persist
     between expressions and which ought to be reset each time.  */
  expression_context_block = NULL;
  innermost_block = NULL;

  /* Check to see if the current_source_symtab belongs to this objfile,
     and if so, call clear_current_source_symtab_and_line. */

  {
    struct symtab_and_line cursal = get_current_source_symtab_and_line ();
    struct symtab *s;

    ALL_OBJFILE_SYMTABS (objfile, s)
      {
	if (s == cursal.symtab)
	  clear_current_source_symtab_and_line ();
      }
  }

  /* The last thing we do is free the objfile struct itself. */

  if (OBJFILE_NAME (objfile) != NULL)
    {
      xfree (OBJFILE_NAME (objfile));
    }
  if (OBJFILE_GLOBAL_PSYMBOLS (objfile).list)
    xfree (OBJFILE_GLOBAL_PSYMBOLS (objfile).list);
  if (OBJFILE_STATIC_PSYMBOLS (objfile).list)
    xfree (OBJFILE_STATIC_PSYMBOLS (objfile).list);
  /* Free the obstacks for non-reusable objfiles */
  bcache_xfree (OBJFILE_PSYMBOL_CACHE (objfile));
  bcache_xfree (OBJFILE_MACRO_CACHE (objfile));
  bcache_xfree (OBJFILE_FILENAME_CACHE (objfile));
  if (OBJFILE_DEMANGLED_NAMES_HASH (objfile))
    htab_delete (OBJFILE_DEMANGLED_NAMES_HASH (objfile));
  obstack_free (&OBJFILE_OBSTACK (objfile), 0);

  /* Rebuild section map next time we need it.  */
  get_objfile_pspace_data (current_program_space)->objfiles_changed_p = 1;

  xfree (objfile->section_offsets);
  xfree (objfile->storage);
  xfree (objfile);
}

void
free_objfile (struct objfile *objfile)
{
  struct objfile_list *wrapper;

  /* Free all separate debug objfiles.  */
  free_objfile_separate_debug (objfile);

  /* Remove it from the chain of all objfiles. */
  wrapper = unlink_objfile (objfile);
  xfree (wrapper);

  if (--OBJFILE_REFC (objfile) == 0)
    do_free_objfile (objfile);
}

static void
do_free_objfile_cleanup (void *obj)
{
  free_objfile (obj);
}

struct cleanup *
make_cleanup_free_objfile (struct objfile *obj)
{
  return make_cleanup (do_free_objfile_cleanup, obj);
}

/* Free all the object files at once and clean up their users.  */

void
free_all_objfiles (void)
{
  struct objfile *objfile;
  struct so_list *so;
  objfile_iterator_type iter, temp;

  /* Any objfile referencewould become stale.  */
  for (so = master_so_list (); so; so = so->next)
    gdb_assert (so->objfile == NULL);

  ALL_OBJFILES_SAFE (iter, objfile, temp)
  {
    free_objfile (objfile);
  }
  clear_symtab_users ();
}

/* Relocate OBJFILE to NEW_OFFSETS.  There should be OBJFILE->NUM_SECTIONS
   entries in new_offsets.  SEPARATE_DEBUG_OBJFILE is not touched here.
   Return non-zero iff any change happened.  */

static int
objfile_relocate1 (struct objfile *objfile, 
		   struct section_offsets *new_offsets)
{
  struct obj_section *s;
  struct section_offsets *delta =
    ((struct section_offsets *) 
     alloca (SIZEOF_N_SECTION_OFFSETS (OBJFILE_NUM_SECTIONS (objfile))));

  int i;
  int something_changed = 0;

  for (i = 0; i < OBJFILE_NUM_SECTIONS (objfile); ++i)
    {
      delta->offsets[i] =
	ANOFFSET (new_offsets, i) - ANOFFSET (OBJFILE_SECTION_OFFSETS (objfile), i);
      if (ANOFFSET (delta, i) != 0)
	something_changed = 1;
    }
  if (!something_changed)
    return 0;

  /* OK, get all the symtabs.  */
  {
    struct symtab *s;

    ALL_OBJFILE_SYMTABS (objfile, s)
    {
      struct linetable *l;
      struct blockvector *bv;
      int i;

      /* First the line table.  */
      l = LINETABLE (s);
      if (l)
	{
	  for (i = 0; i < l->nitems; ++i)
	    l->item[i].pc += ANOFFSET (delta, s->block_line_section);
	}

      /* Don't relocate a shared blockvector more than once.  */
      if (!s->primary)
	continue;

      bv = BLOCKVECTOR (s);
      if (BLOCKVECTOR_MAP (bv))
	addrmap_relocate (BLOCKVECTOR_MAP (bv),
			  ANOFFSET (delta, s->block_line_section));

      for (i = 0; i < BLOCKVECTOR_NBLOCKS (bv); ++i)
	{
	  struct block *b;
	  struct symbol *sym;
	  struct dict_iterator iter;

	  b = BLOCKVECTOR_BLOCK (bv, i);
	  SET_BLOCK_START (b, (BLOCK_START (b)
			       + ANOFFSET (delta, s->block_line_section)));
	  SET_BLOCK_END (b, (BLOCK_END (b)
			     + ANOFFSET (delta, s->block_line_section)));

	  ALL_BLOCK_SYMBOLS (b, iter, sym)
	    {
	      fixup_symbol_section (sym, objfile);

	      /* The RS6000 code from which this was taken skipped
	         any symbols in STRUCT_DOMAIN or UNDEF_DOMAIN.
	         But I'm leaving out that test, on the theory that
	         they can't possibly pass the tests below.  */
	      if ((SYMBOL_CLASS (sym) == LOC_LABEL
		   || SYMBOL_CLASS (sym) == LOC_STATIC)
		  && SYMBOL_SECTION (sym) >= 0)
		{
		  SET_SYMBOL_VALUE_ADDRESS (sym,
					    SYMBOL_VALUE_ADDRESS (sym)
					    + ANOFFSET (delta,
							SYMBOL_SECTION (sym)));
		}
	    }
	}
    }
  }

  if (OBJFILE_PSYMTABS_ADDRMAP (objfile))
    addrmap_relocate (OBJFILE_PSYMTABS_ADDRMAP (objfile),
		      ANOFFSET (delta, SECT_OFF_TEXT (objfile)));

  if (OBJFILE_SF (objfile))
    OBJFILE_SF (objfile)->qf->relocate (objfile, new_offsets, delta);

  {
    struct minimal_symbol *msym;

    ALL_OBJFILE_MSYMBOLS (objfile, msym)
      if (SYMBOL_SECTION (msym) >= 0)
	SET_SYMBOL_VALUE_ADDRESS (msym,
				  SYMBOL_VALUE_ADDRESS (msym)
				  + ANOFFSET (delta, SYMBOL_SECTION (msym)));
  }
  /* Relocating different sections by different amounts may cause the symbols
     to be out of order.  */
  msymbols_sort (objfile);

  if (OBJFILE_EI (objfile).entry_point_p)
    {
      /* Relocate ei.entry_point with its section offset, use SECT_OFF_TEXT
	 only as a fallback.  */
      struct obj_section *s;
      s = find_pc_section (OBJFILE_EI (objfile).entry_point);
      if (s)
        OBJFILE_EI (objfile).entry_point += ANOFFSET (delta, s->the_bfd_section->index);
      else
        OBJFILE_EI (objfile).entry_point += ANOFFSET (delta, SECT_OFF_TEXT (objfile));
    }

  {
    int i;

    for (i = 0; i < OBJFILE_NUM_SECTIONS (objfile); ++i)
      (OBJFILE_SECTION_OFFSETS (objfile))->offsets[i] = ANOFFSET (new_offsets, i);
  }

  /* Rebuild section map next time we need it.  */
  get_objfile_pspace_data (current_program_space)->objfiles_changed_p = 1;

  /* Update the table in exec_ops, used to read memory.  */
  ALL_OBJFILE_OSECTIONS (objfile, s)
    {
      int idx = s->the_bfd_section->index;

      exec_set_section_address (bfd_get_filename (OBJFILE_OBFD (objfile)), idx,
				obj_section_addr (s));
    }

  /* Data changed.  */
  return 1;
}

/* Relocate OBJFILE to NEW_OFFSETS.  There should be OBJFILE->NUM_SECTIONS
   entries in new_offsets.  Process also OBJFILE's SEPARATE_DEBUG_OBJFILEs.

   The number and ordering of sections does differ between the two objfiles.
   Only their names match.  Also the file offsets will differ (objfile being
   possibly prelinked but separate_debug_objfile is probably not prelinked) but
   the in-memory absolute address as specified by NEW_OFFSETS must match both
   files.  */

void
objfile_relocate (struct objfile *objfile, struct section_offsets *new_offsets)
{
  struct objfile *debug_objfile;
  int changed = 0;

  changed |= objfile_relocate1 (objfile, new_offsets);

  for (debug_objfile = OBJFILE_SEPARATE_DEBUG_OBJFILE (objfile);
       debug_objfile;
       debug_objfile = objfile_separate_debug_iterate (objfile, debug_objfile))
    {
      struct section_addr_info *objfile_addrs;
      struct section_offsets *new_debug_offsets;
      struct cleanup *my_cleanups;

      objfile_addrs = build_section_addr_info_from_objfile (objfile);
      my_cleanups = make_cleanup (xfree, objfile_addrs);

      /* Here OBJFILE_ADDRS contain the correct absolute addresses, the
	 relative ones must be already created according to debug_objfile.  */

      addr_info_make_relative (objfile_addrs, OBJFILE_OBFD (debug_objfile));

      gdb_assert (OBJFILE_NUM_SECTIONS (debug_objfile)
		  == bfd_count_sections (OBJFILE_OBFD (debug_objfile)));
      new_debug_offsets = 
	xmalloc (SIZEOF_N_SECTION_OFFSETS (OBJFILE_NUM_SECTIONS (debug_objfile)));
      make_cleanup (xfree, new_debug_offsets);
      relative_addr_info_to_section_offsets (new_debug_offsets,
					     OBJFILE_NUM_SECTIONS (debug_objfile),
					     objfile_addrs);

      changed |= objfile_relocate1 (debug_objfile, new_debug_offsets);

      do_cleanups (my_cleanups);
    }

  /* Relocate breakpoints as necessary, after things are relocated. */
  if (changed)
    breakpoint_re_set ();
}

/* Return non-zero if OBJFILE has partial symbols.  */

int
objfile_has_partial_symbols (struct objfile *objfile)
{
  return OBJFILE_SF (objfile) ? OBJFILE_SF (objfile)->qf->has_symbols (objfile) : 0;
}

/* Return non-zero if OBJFILE has full symbols.  */

int
objfile_has_full_symbols (struct objfile *objfile)
{
  return OBJFILE_SYMTABS (objfile) != NULL;
}

/* Return non-zero if OBJFILE has full or partial symbols, either directly
   or through a separate debug file.  */

int
objfile_has_symbols (struct objfile *objfile)
{
  struct objfile *o;

  for (o = objfile; o; o = objfile_separate_debug_iterate (objfile, o))
    if (objfile_has_partial_symbols (o) || objfile_has_full_symbols (o))
      return 1;
  return 0;
}


/* Many places in gdb want to test just to see if we have any partial
   symbols available.  This function returns zero if none are currently
   available, nonzero otherwise. */

int
have_partial_symbols (void)
{
  struct objfile *ofp;
  objfile_iterator_type iter;

  ALL_OBJFILES (iter, ofp)
  {
    if (objfile_has_partial_symbols (ofp))
      return 1;
  }
  return 0;
}

/* Many places in gdb want to test just to see if we have any full
   symbols available.  This function returns zero if none are currently
   available, nonzero otherwise. */

int
have_full_symbols (void)
{
  struct objfile *ofp;
  objfile_iterator_type iter;

  ALL_OBJFILES (iter, ofp)
  {
    if (objfile_has_full_symbols (ofp))
      return 1;
  }
  return 0;
}


/* This operations deletes all objfile entries that represent solibs that
   weren't explicitly loaded by the user, via e.g., the add-symbol-file
   command.
 */
void
objfile_purge_solibs (void)
{
  struct objfile *objf;
  objfile_iterator_type iter, temp;

  ALL_OBJFILES_SAFE (iter, objf, temp)
  {
    /* We assume that the solib package has been purged already, or will
       be soon.
     */
    if (!(OBJFILE_FLAGS (objf) & OBJF_USERLOADED) && (OBJFILE_FLAGS (objf) & OBJF_SHARED))
      free_objfile (objf);
  }
}


/* Many places in gdb want to test just to see if we have any minimal
   symbols available.  This function returns zero if none are currently
   available, nonzero otherwise. */

int
have_minimal_symbols (void)
{
  struct objfile *ofp;
  objfile_iterator_type iter;

  ALL_OBJFILES (iter, ofp)
  {
    if (OBJFILE_MINIMAL_SYMBOL_COUNT (ofp) > 0)
      {
	return 1;
      }
  }
  return 0;
}

/* Qsort comparison function.  */

static int
qsort_cmp (const void *a, const void *b)
{
  const struct obj_section *sect1 = *(const struct obj_section **) a;
  const struct obj_section *sect2 = *(const struct obj_section **) b;
  const CORE_ADDR sect1_addr = obj_section_addr (sect1);
  const CORE_ADDR sect2_addr = obj_section_addr (sect2);

  if (sect1_addr < sect2_addr)
    return -1;
  else if (sect1_addr > sect2_addr)
    return 1;
  else
    {
      /* Sections are at the same address.  This could happen if
	 A) we have an objfile and a separate debuginfo.
	 B) we are confused, and have added sections without proper relocation,
	 or something like that. */

      const struct objfile *const objfile1 = sect1->objfile;
      const struct objfile *const objfile2 = sect2->objfile;

      if (OBJFILE_SEPARATE_DEBUG_OBJFILE (objfile1) == objfile2
	  || OBJFILE_SEPARATE_DEBUG_OBJFILE (objfile2) == objfile1)
	{
	  /* Case A.  The ordering doesn't matter: separate debuginfo files
	     will be filtered out later.  */

	  return 0;
	}

      /* Case B.  Maintain stable sort order, so bugs in GDB are easier to
	 triage.  This section could be slow (since we iterate over all
	 objfiles in each call to qsort_cmp), but this shouldn't happen
	 very often (GDB is already in a confused state; one hopes this
	 doesn't happen at all).  If you discover that significant time is
	 spent in the loops below, do 'set complaints 100' and examine the
	 resulting complaints.  */

      if (objfile1 == objfile2)
	{
	  /* Both sections came from the same objfile.  We are really confused.
	     Sort on sequence order of sections within the objfile.  */

	  const struct obj_section *osect;

	  ALL_OBJFILE_OSECTIONS (objfile1, osect)
	    if (osect == sect1)
	      return -1;
	    else if (osect == sect2)
	      return 1;

	  /* We should have found one of the sections before getting here.  */
	  gdb_assert (0);
	}
      else
	{
	  /* Sort on sequence number of the objfile in the chain.  */

	  struct objfile *objfile;
	  objfile_iterator_type iter;

	  ALL_OBJFILES (iter, objfile)
	    if (objfile == objfile1)
	      return -1;
	    else if (objfile == objfile2)
	      return 1;

	  /* We should have found one of the objfiles before getting here.  */
	  gdb_assert (0);
	}
    }

  /* Unreachable.  */
  gdb_assert (0);
  return 0;
}

/* Select "better" obj_section to keep.  We prefer the one that came from
   the real object, rather than the one from separate debuginfo.
   Most of the time the two sections are exactly identical, but with
   prelinking the .rel.dyn section in the real object may have different
   size.  */

static struct obj_section *
preferred_obj_section (struct obj_section *a, struct obj_section *b)
{
  gdb_assert (obj_section_addr (a) == obj_section_addr (b));
  gdb_assert ((OBJFILE_SEPARATE_DEBUG_OBJFILE (a->objfile) == b->objfile)
	      || (OBJFILE_SEPARATE_DEBUG_OBJFILE (b->objfile) == a->objfile));
  gdb_assert ((OBJFILE_SEPARATE_DEBUG_OBJFILE_BACKLINK (a->objfile) == b->objfile)
	      || (OBJFILE_SEPARATE_DEBUG_OBJFILE_BACKLINK (b->objfile) == a->objfile));

  if (OBJFILE_SEPARATE_DEBUG_OBJFILE (a->objfile) != NULL)
    return a;
  return b;
}

/* Return 1 if SECTION should be inserted into the section map.
   We want to insert only non-overlay and non-TLS section.  */

static int
insert_section_p (const struct bfd *abfd,
		  const struct bfd_section *section)
{
  const bfd_vma lma = bfd_section_lma (abfd, section);

  if (lma != 0 && lma != bfd_section_vma (abfd, section)
      && (bfd_get_file_flags (abfd) & BFD_IN_MEMORY) == 0)
    /* This is an overlay section.  IN_MEMORY check is needed to avoid
       discarding sections from the "system supplied DSO" (aka vdso)
       on some Linux systems (e.g. Fedora 11).  */
    return 0;
  if ((bfd_get_section_flags (abfd, section) & SEC_THREAD_LOCAL) != 0)
    /* This is a TLS section.  */
    return 0;

  return 1;
}

/* Filter out overlapping sections where one section came from the real
   objfile, and the other from a separate debuginfo file.
   Return the size of table after redundant sections have been eliminated.  */

static int
filter_debuginfo_sections (struct obj_section **map, int map_size)
{
  int i, j;

  for (i = 0, j = 0; i < map_size - 1; i++)
    {
      struct obj_section *const sect1 = map[i];
      struct obj_section *const sect2 = map[i + 1];
      const struct objfile *const objfile1 = sect1->objfile;
      const struct objfile *const objfile2 = sect2->objfile;
      const CORE_ADDR sect1_addr = obj_section_addr (sect1);
      const CORE_ADDR sect2_addr = obj_section_addr (sect2);

      if (sect1_addr == sect2_addr
	  && (OBJFILE_SEPARATE_DEBUG_OBJFILE (objfile1) == objfile2
	      || OBJFILE_SEPARATE_DEBUG_OBJFILE (objfile2) == objfile1))
	{
	  map[j++] = preferred_obj_section (sect1, sect2);
	  ++i;
	}
      else
	map[j++] = sect1;
    }

  if (i < map_size)
    {
      gdb_assert (i == map_size - 1);
      map[j++] = map[i];
    }

  /* The map should not have shrunk to less than half the original size.  */
  gdb_assert (map_size / 2 <= j);

  return j;
}

/* Filter out overlapping sections, issuing a warning if any are found.
   Overlapping sections could really be overlay sections which we didn't
   classify as such in insert_section_p, or we could be dealing with a
   corrupt binary.  */

static int
filter_overlapping_sections (struct obj_section **map, int map_size)
{
  int i, j;

  for (i = 0, j = 0; i < map_size - 1; )
    {
      int k;

      map[j++] = map[i];
      for (k = i + 1; k < map_size; k++)
	{
	  struct obj_section *const sect1 = map[i];
	  struct obj_section *const sect2 = map[k];
	  const CORE_ADDR sect1_addr = obj_section_addr (sect1);
	  const CORE_ADDR sect2_addr = obj_section_addr (sect2);
	  const CORE_ADDR sect1_endaddr = obj_section_endaddr (sect1);

	  gdb_assert (sect1_addr <= sect2_addr);

	  if (sect1_endaddr <= sect2_addr)
	    break;
	  else
	    {
	      /* We have an overlap.  Report it.  */

	      struct objfile *const objf1 = sect1->objfile;
	      struct objfile *const objf2 = sect2->objfile;

	      const struct bfd *const abfd1 = OBJFILE_OBFD (objf1);
	      const struct bfd *const abfd2 = OBJFILE_OBFD (objf2);

	      const struct bfd_section *const bfds1 = sect1->the_bfd_section;
	      const struct bfd_section *const bfds2 = sect2->the_bfd_section;

	      const CORE_ADDR sect2_endaddr = obj_section_endaddr (sect2);

	      struct gdbarch *const gdbarch = get_objfile_arch (objf1);

	      complaint (&symfile_complaints,
			 _("unexpected overlap between:\n"
			   " (A) section `%s' from `%s' [%s, %s)\n"
			   " (B) section `%s' from `%s' [%s, %s).\n"
			   "Will ignore section B"),
			 bfd_section_name (abfd1, bfds1), OBJFILE_NAME (objf1),
			 paddress (gdbarch, sect1_addr),
			 paddress (gdbarch, sect1_endaddr),
			 bfd_section_name (abfd2, bfds2), OBJFILE_NAME (objf2),
			 paddress (gdbarch, sect2_addr),
			 paddress (gdbarch, sect2_endaddr));
	    }
	}
      i = k;
    }

  if (i < map_size)
    {
      gdb_assert (i == map_size - 1);
      map[j++] = map[i];
    }

  return j;
}


/* Update PMAP, PMAP_SIZE with sections from all objfiles, excluding any
   TLS, overlay and overlapping sections.  */

static void
update_section_map (struct program_space *pspace,
		    struct obj_section ***pmap, int *pmap_size)
{
  int alloc_size, map_size, i;
  struct obj_section *s, **map;
  struct objfile *objfile;
  objfile_iterator_type iter;

  gdb_assert (get_objfile_pspace_data (pspace)->objfiles_changed_p != 0);

  map = *pmap;
  xfree (map);

  alloc_size = 0;
  ALL_PSPACE_OBJFILES (pspace, iter, objfile)
    ALL_OBJFILE_OSECTIONS (objfile, s)
      if (insert_section_p (OBJFILE_OBFD (objfile), s->the_bfd_section))
	alloc_size += 1;

  /* This happens on detach/attach (e.g. in gdb.base/attach.exp).  */
  if (alloc_size == 0)
    {
      *pmap = NULL;
      *pmap_size = 0;
      return;
    }

  map = xmalloc (alloc_size * sizeof (*map));

  i = 0;
  ALL_PSPACE_OBJFILES (pspace, iter, objfile)
    ALL_OBJFILE_OSECTIONS (objfile, s)
      if (insert_section_p (OBJFILE_OBFD (objfile), s->the_bfd_section))
	map[i++] = s;

  qsort (map, alloc_size, sizeof (*map), qsort_cmp);
  map_size = filter_debuginfo_sections(map, alloc_size);
  map_size = filter_overlapping_sections(map, map_size);

  if (map_size < alloc_size)
    /* Some sections were eliminated.  Trim excess space.  */
    map = xrealloc (map, map_size * sizeof (*map));
  else
    gdb_assert (alloc_size == map_size);

  *pmap = map;
  *pmap_size = map_size;
}

/* Bsearch comparison function. */

static int
bsearch_cmp (const void *key, const void *elt)
{
  const CORE_ADDR pc = *(CORE_ADDR *) key;
  const struct obj_section *section = *(const struct obj_section **) elt;

  if (pc < obj_section_addr (section))
    return -1;
  if (pc < obj_section_endaddr (section))
    return 0;
  return 1;
}

/* Returns a section whose range includes PC or NULL if none found.   */

struct obj_section *
find_pc_section (CORE_ADDR pc)
{
  struct objfile_pspace_info *pspace_info;
  struct obj_section *s, **sp;

  /* Check for mapped overlay section first.  */
  s = find_pc_mapped_section (pc);
  if (s)
    return s;

  pspace_info = get_objfile_pspace_data (current_program_space);
  if (pspace_info->objfiles_changed_p != 0)
    {
      update_section_map (current_program_space,
			  &pspace_info->sections,
			  &pspace_info->num_sections);

      /* Don't need updates to section map until objfiles are added,
         removed or relocated.  */
      pspace_info->objfiles_changed_p = 0;
    }

  /* The C standard (ISO/IEC 9899:TC2) requires the BASE argument to
     bsearch be non-NULL.  */
  if (pspace_info->sections == NULL)
    {
      gdb_assert (pspace_info->num_sections == 0);
      return NULL;
    }

  sp = (struct obj_section **) bsearch (&pc,
					pspace_info->sections,
					pspace_info->num_sections,
					sizeof (*pspace_info->sections),
					bsearch_cmp);
  if (sp != NULL)
    return *sp;
  return NULL;
}


/* In SVR4, we recognize a trampoline by it's section name. 
   That is, if the pc is in a section named ".plt" then we are in
   a trampoline.  */

int
in_plt_section (CORE_ADDR pc, char *name)
{
  struct obj_section *s;
  int retval = 0;

  s = find_pc_section (pc);

  retval = (s != NULL
	    && s->the_bfd_section->name != NULL
	    && strcmp (s->the_bfd_section->name, ".plt") == 0);
  return (retval);
}


/* Keep a registry of per-objfile data-pointers required by other GDB
   modules.  */

struct objfile_data
{
  unsigned index;
  void (*save) (struct objfile *, void *);
  void (*free) (struct objfile *, void *);
};

struct objfile_data_registration
{
  struct objfile_data *data;
  struct objfile_data_registration *next;
};
  
struct objfile_data_registry
{
  struct objfile_data_registration *registrations;
  unsigned num_registrations;
};

static struct objfile_data_registry objfile_data_registry = { NULL, 0 };

const struct objfile_data *
register_objfile_data_with_cleanup (void (*save) (struct objfile *, void *),
				    void (*free) (struct objfile *, void *))
{
  struct objfile_data_registration **curr;

  /* Append new registration.  */
  for (curr = &objfile_data_registry.registrations;
       *curr != NULL; curr = &(*curr)->next);

  *curr = XMALLOC (struct objfile_data_registration);
  (*curr)->next = NULL;
  (*curr)->data = XMALLOC (struct objfile_data);
  (*curr)->data->index = objfile_data_registry.num_registrations++;
  (*curr)->data->save = save;
  (*curr)->data->free = free;

  return (*curr)->data;
}

const struct objfile_data *
register_objfile_data (void)
{
  return register_objfile_data_with_cleanup (NULL, NULL);
}

static void
objfile_alloc_data (struct objfile *objfile)
{
  gdb_assert (OBJFILE_DATA (objfile) == NULL);
  OBJFILE_NUM_DATA (objfile) = objfile_data_registry.num_registrations;
  OBJFILE_DATA (objfile) = XCALLOC (OBJFILE_NUM_DATA (objfile), void *);
}

static void
objfile_free_data (struct objfile *objfile)
{
  gdb_assert (OBJFILE_DATA (objfile) != NULL);
  clear_objfile_data (objfile);
  xfree (OBJFILE_DATA (objfile));
  OBJFILE_DATA (objfile) = NULL;
}

void
clear_objfile_data (struct objfile *objfile)
{
  struct objfile_data_registration *registration;
  int i;

  gdb_assert (OBJFILE_DATA (objfile) != NULL);

  /* Process all the save handlers.  */

  for (registration = objfile_data_registry.registrations, i = 0;
       i < OBJFILE_NUM_DATA (objfile);
       registration = registration->next, i++)
    if (OBJFILE_DATA (objfile)[i] != NULL && registration->data->save != NULL)
      registration->data->save (objfile, OBJFILE_DATA (objfile)[i]);

  /* Now process all the free handlers.  */

  for (registration = objfile_data_registry.registrations, i = 0;
       i < OBJFILE_NUM_DATA (objfile);
       registration = registration->next, i++)
    if (OBJFILE_DATA (objfile)[i] != NULL && registration->data->free != NULL)
      registration->data->free (objfile, OBJFILE_DATA (objfile)[i]);

  memset (OBJFILE_DATA (objfile), 0, OBJFILE_NUM_DATA (objfile) * sizeof (void *));
}

void
set_objfile_data (struct objfile *objfile, const struct objfile_data *data,
		  void *value)
{
  gdb_assert (data->index < OBJFILE_NUM_DATA (objfile));
  OBJFILE_DATA (objfile)[data->index] = value;
}

void *
objfile_data (struct objfile *objfile, const struct objfile_data *data)
{
  gdb_assert (data->index < OBJFILE_NUM_DATA (objfile));
  return OBJFILE_DATA (objfile)[data->index];
}

/* Set objfiles_changed_p so section map will be rebuilt next time it
   is used.  Called by reread_symbols.  */

void
objfiles_changed (void)
{
  /* Rebuild section map next time we need it.  */
  get_objfile_pspace_data (current_program_space)->objfiles_changed_p = 1;
}

/* Close ABFD, and warn if that fails.  */

int
gdb_bfd_close_or_warn (struct bfd *abfd)
{
  int ret;
  char *name = bfd_get_filename (abfd);

  ret = bfd_close (abfd);

  if (!ret)
    warning (_("cannot close \"%s\": %s"),
	     name, bfd_errmsg (bfd_get_error ()));

  return ret;
}

/* Add reference to ABFD.  Returns ABFD.  */
struct bfd *
gdb_bfd_ref (struct bfd *abfd)
{
  int *p_refcount;

  if (abfd == NULL)
    return NULL;

  p_refcount = bfd_usrdata (abfd);

  if (p_refcount != NULL)
    {
      *p_refcount += 1;
      return abfd;
    }

  p_refcount = xmalloc (sizeof (*p_refcount));
  *p_refcount = 1;
  bfd_usrdata (abfd) = p_refcount;

  return abfd;
}

/* Unreference and possibly close ABFD.  */
void
gdb_bfd_unref (struct bfd *abfd)
{
  int *p_refcount;
  char *name;

  if (abfd == NULL)
    return;

  p_refcount = bfd_usrdata (abfd);

  /* Valid range for p_refcount: a pointer to int counter, which has a
     value of 1 (single owner) or 2 (shared).  */
  gdb_assert (*p_refcount == 1 || *p_refcount == 2);

  *p_refcount -= 1;
  if (*p_refcount > 0)
    return;

  xfree (p_refcount);
  bfd_usrdata (abfd) = NULL;  /* Paranoia.  */

  name = bfd_get_filename (abfd);
  gdb_bfd_close_or_warn (abfd);
  xfree (name);
}

/* Provide a prototype to silence -Wmissing-prototypes.  */
extern initialize_file_ftype _initialize_objfiles;

void
_initialize_objfiles (void)
{
  objfiles_pspace_data
    = register_program_space_data_with_cleanup (objfiles_pspace_data_cleanup);
}
