/* Cleanups.
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

#ifndef CLEANUPS_H
#define CLEANUPS_H

/* Outside of cleanups.c, this is an opaque type.  */
struct cleanup;

/* NOTE: cagney/2000-03-04: This typedef is strictly for the
   make_cleanup function declarations below.  Do not use this typedef
   as a cast when passing functions into the make_cleanup() code.
   Instead either use a bounce function or add a wrapper function.
   Calling a f(char*) function with f(void*) is non-portable.  */
typedef void (make_cleanup_ftype) (void *);

/* Function type for the dtor in make_cleanup_dtor.  */
typedef void (make_cleanup_dtor_ftype) (void *);

/* WARNING: The result of the "make cleanup" routines is not the intuitive
   choice of being a handle on the just-created cleanup.  Instead it is an
   opaque handle of the cleanup mechanism and represents all cleanups created
   from that point onwards.
   The result is guaranteed to be non-NULL though.  */

extern struct cleanup *make_cleanup (make_cleanup_ftype *, void *);

extern struct cleanup *make_cleanup_dtor (make_cleanup_ftype *, void *,
					  make_cleanup_dtor_ftype *);

extern struct cleanup *make_final_cleanup (make_cleanup_ftype *, void *);

/* A special value to pass to do_cleanups and do_final_cleanups
   to tell them to do all cleanups.  */
extern struct cleanup *all_cleanups (void);

extern void do_cleanups (struct cleanup *);
extern void do_final_cleanups (struct cleanup *);

extern void discard_cleanups (struct cleanup *);
extern void discard_final_cleanups (struct cleanup *);

extern struct cleanup *save_cleanups (void);
extern struct cleanup *save_final_cleanups (void);

extern void restore_cleanups (struct cleanup *);
extern void restore_final_cleanups (struct cleanup *);

/* A no-op cleanup.
   This is useful when you want to establish a known reference point
   to pass to do_cleanups.  */
extern void null_cleanup (void *);


/* You should continue to treat this as opaque.  It is defined here
   so that scoped cleanups can be stack-allocated and specially
   treated.  */

struct cleanup
{
  struct cleanup *next;

  /* True if this is a scoped cleanup.  */
  unsigned scoped : 1;

  /* True if this is scoped cleanup has been cleaned up or discarded.
     Not used for ordinary cleanups.  */

  unsigned cleaned_up : 1;
};

/* This is used for scoped cleanups.  It should be treated as
   opaque.  */

struct scoped_cleanup
{
  struct cleanup base;
};

extern struct cleanup *init_scoped_cleanup (struct scoped_cleanup *);

#if defined (__GNUC__) && __GNUC__ >= 4

/* Define this to consolidate #if checking with the
   implementation.  */

#define SCOPED_CLEANUP_CHECKING

#define SCOPED_CLEANUP_ATTRIBUTE \
  __attribute__ ((cleanup (cleanup_close_scope)))

extern void cleanup_close_scope (struct scoped_cleanup *);

#else

#define SCOPED_CLEANUP_ATTRIBUTE

#endif

/* Use this to declare a scoped cleanup.  A scoped cleanup must be
   cleaned up or discarded whenever the scope is exited.  When
   possible, this is checked at runtime.  */

#define SCOPED_CLEANUP(name)						\
  struct scoped_cleanup name ## __LINE__				\
    SCOPED_CLEANUP_ATTRIBUTE;						\
  struct cleanup *name = init_scoped_cleanup (& (name ## __LINE__))

#endif /* CLEANUPS_H */
