/* Language independent support for printing types for GDB, the GNU debugger.
   Copyright (C) 1986, 1988, 1989, 1991-1993, 1999, 2000, 2007, 2008, 2009,
   2010, 2011 Free Software Foundation, Inc.

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

#ifndef TYPEPRINT_H
#define TYPEPRINT_H

enum language;
struct ui_file;

/* An enumeration for specifying the type of a computed symbol name.  */
enum name_kind
  {
    NAME_KIND_FULL,	/* The fullname (methods do not
			   contain any formal parameters).  */
    NAME_KIND_PHYS,	/* The physname used to lookup symbols.  */
    NAME_KIND_PRINT	/* The name used when printing the symbol.  */
  };

void print_type_scalar (struct type * type, LONGEST, struct ui_file *);

void c_type_print_varspec_suffix (struct type *, struct ui_file *, int,
				  int, int);

void c_type_print_args (struct type *, struct ui_file *,
			enum name_kind, enum language);
#endif
