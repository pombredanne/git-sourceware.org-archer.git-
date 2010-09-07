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

struct itset;
struct inferior;

struct itset *itset_create (const char *spec);

int itset_contains_inferior (struct itset *, struct inferior *);

void itset_free (struct itset *);

#endif /* ITSET_H */
