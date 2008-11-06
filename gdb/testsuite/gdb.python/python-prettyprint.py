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
class string_print:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        return self.val['whybother']['contents']

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

    def __init__(self, val):
        self.val = val

    def to_string(self):
        return 'container %s with %d elements' % (self.val['name'], self.val['len'])

    def children(self):
        return self._iterator(self.val['elements'], self.val['len'])

class pp_s:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        a = self.val["a"]
        b = self.val["b"]
        if a.address() != b:
            raise Exception("&a(%s) != b(%s)" % (str(a.address()), str(b)))
        return " a=<" + str(self.val["a"]) + "> b=<" + str(self.val["b"]) + ">"

class pp_ss:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        return "a=<" + str(self.val["a"]) + "> b=<" + str(self.val["b"]) + ">"

class pp_sss:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        return "a=<" + str(self.val['a']) + "> b=<" + str(self.val["b"]) + ">"

gdb.pretty_printers['^struct s$']   = pp_s
gdb.pretty_printers['^s$']   = pp_s
gdb.pretty_printers['^S$']   = pp_s

gdb.pretty_printers['^struct ss$']  = pp_ss
gdb.pretty_printers['^ss$']  = pp_ss

gdb.pretty_printers['^const S &$']   = pp_s
gdb.pretty_printers['^SSS$']  = pp_sss

# Note that we purposely omit the typedef names here.
# Printer lookup is based on canonical name.
# However, we do need both tagged and untagged variants, to handle
# both the C and C++ cases.
gdb.pretty_printers['^struct string_repr$'] = string_print
gdb.pretty_printers['^struct container$'] = ContainerPrinter
gdb.pretty_printers['^string_repr$'] = string_print
gdb.pretty_printers['^container$'] = ContainerPrinter
