# In-scope function.

# Copyright (C) 2008 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import gdb

class InScope (gdb.Function):
    """Check if all the given variables or macros are in scope.
Receives as argument a list of names separated by whitespace."""

    def __init__ (self):
	super (InScope, self).__init__ ("in_scope")

    def invoke (self, var):
	vars = set (var.string().split())
	found = set ()
	block = gdb.block_for_pc (gdb.selected_frame ().pc ())
	while block:
	    for sym in block:
		if (sym.is_argument () or sym.is_constant ()
		      or sym.is_function () or sym.is_variable ()):
		    sym_name = sym.name
		    if sym_name in vars:
			found.add (sym_name)

	    block = block.superblock

	return vars == found

InScope ()
