/* itset.h - Inferior/Thread sets.
   Copyright (C) 2010 Free Software Foundation, Inc.

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

#ifndef ITSET_H
#define ITSET_H

struct inferior;

/* This is an opaque type representing an I/T set.  An I/T set is
   simply a set of inferiors and/or threads.  A set may be dynamic
   (the members are enumerated at the time of use) or static (the
   members are enumerated at the time of construction); but this
   distinction is hidden from the callers.  An I/T set object is
   reference counted.  */

struct itset;

/* Create a new I/T set from a user specification.  The valid forms of
   a specification are documented in the manual.  *SPEC is the input
   specification, and it is updated to point to the first non-space
   character after the end of the specification.  */

struct itset *itset_create (const char **spec);

/* Create a new I/T set which represents the current inferior, at the
   time that this call is made.  */

struct itset *itset_create_current (void);

/* Return true if the inferior is contained in the I/T set.  */

int itset_contains_inferior (struct itset *itset, struct inferior *inf);

/* Acquire a new reference to an I/T set.  Returns the I/T set, for
   convenience.  */

struct itset *itset_reference (struct itset *itset);

/* Release a reference to an I/T set.  */

void itset_free (struct itset *itset);

/* A cleanup function that calls itset_free.  */

void itset_cleanup (void *itset);

/* Like iterate_over_inferiors, but iterate over only those inferiors
   in ITSET.  */

struct inferior *iterate_over_itset (struct itset *itset,
				     int (*callback) (struct inferior *,
						      void *),
				     void *data);

#endif /* ITSET_H */
