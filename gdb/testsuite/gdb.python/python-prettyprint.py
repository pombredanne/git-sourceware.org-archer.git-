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

# Test returning a Value from a printer.
def string_print (val):
    return val['whybother']['contents']

# Test a class-based printer.
class ContainerPrinter:
    class _iterator:
        def __init__ (self, pointer, len):
            self.start = pointer
            self.pointer = pointer
            self.end = pointer + len

        def __iter__(self):
            return self

        def next(self):
            if self.pointer == self.end:
                raise StopIteration
            result = self.pointer
            self.pointer = self.pointer + 1
            return ('[%d]' % int (result - self.start), result.dereference())

    def header(self, val):
        return 'container %s with %d elements' % (val['name'], val['len'])

    def children(self, val):
        return self._iterator(val['elements'], val['len'])

def pp_s(val):
  a = val["a"]
  b = val["b"]
  if a.address() != b:
    raise Exception("&a(%s) != b(%s)" % (str(a.address()), str(b)))
  return " a=<" + str(val["a"]) + "> b=<" + str(val["b"]) + ">"

def pp_ss(val):
  return "a=<" + str(val["a"]) + "> b=<" + str(val["b"]) + ">"

def pp_sss(val):
    return "a=<" + str(val['a']) + "> b=<" + str(val["b"]) + ">"

gdb.cli_pretty_printers['^struct s$']   = pp_s
gdb.cli_pretty_printers['^s$']   = pp_s
gdb.cli_pretty_printers['^S$']   = pp_s

gdb.cli_pretty_printers['^struct ss$']  = pp_ss
gdb.cli_pretty_printers['^ss$']  = pp_ss

gdb.cli_pretty_printers['^const S &$']   = pp_s
gdb.cli_pretty_printers['^SSS$']  = pp_sss

# Note that we purposely omit the typedef names here.
# Printer lookup is based on canonical name.
# However, we do need both tagged and untagged variants, to handle
# both the C and C++ cases.
gdb.cli_pretty_printers['^struct string_repr$'] = string_print
gdb.cli_pretty_printers['^struct container$'] = ContainerPrinter()
gdb.cli_pretty_printers['^string_repr$'] = string_print
gdb.cli_pretty_printers['^container$'] = ContainerPrinter()
