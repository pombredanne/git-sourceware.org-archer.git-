# Copyright (C) 2011 Free Software Foundation, Inc.

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

# This file is part of the GDB testsuite.  It tests python pretty
# printers.

class mi_bp (gdb.Breakpoint):

    def print_mention (self):
        return "MI Testcase Breakpoint " + str (self.number) + " at " + str (self.location)

    def print_stop_action (self):
        return ("MI Testcase Breakpoint, ", gdb.PRINT_SRC_AND_LOC)

    def print_one (self, address):
        return (address,"MI Testcase Breakpoint")

    def print_one_detail (self):
        return "MI Testcase Breakpoint at " + str (self.location) + "\n"

