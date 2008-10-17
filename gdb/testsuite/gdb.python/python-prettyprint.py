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

# This file is part of the GDB testsuite.  It tests python pretty
# printers.

python

def pp_s(val):
  a = val["a"]
  b = val["b"]
  if a.address() != b:
    raise Exception("&a(%s) != b(%s)" % (str(a.address()), str(b)))
  return " a=<" + str(val["a"]) + "> b=<" + str(val["b"]) + ">"

def pp_ss(val):
  return "a=<" + str(val["a"]) + "> b=<" + str(val["b"]) + ">"

gdb.cli_pretty_printers['^struct s$']   = pp_s
gdb.cli_pretty_printers['^struct ss$']  = pp_ss
