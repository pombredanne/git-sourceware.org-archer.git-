# Copyright (C) 2012 Free Software Foundation, Inc.

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

# This file is part of the GDB testsuite.  It tests Python-based
# frame-filters.

class Main_filter:
    "Example main () filter"

    def __init__ (self, frame, what, level, args):
        self.frame = frame
        self.what = what
        self.lvl = level
        self.args = args

    def omit (self):
        fname = str (self.frame.function())
        if fname == "func2":
            return True
        else:
            return False

    def elide (self):
        fname = str (self.frame.function())
        frame = self.frame

        if fname == "func3":
            frame = frame.older()
            frame = frame.older()
            frame = frame.older()

        return frame

    def function (self):
        fname = str (self.frame.function())
        if fname == "func3":
            return "Composite frame " + str(self.frame.function())
        else:
            return str (self.frame.function())

    def level (self, level):
        return level

    def address (self):
        return self.frame.pc()

    def filename (self):
        sal = self.frame.find_sal()
        if (sal):
            return sal.symtab.filename
        else:
            return "unknown"

    def frame_args (self):
        func = self.frame.function()
        args = []
        block = self.frame.block()

        if not func:
            return

        for sym in block:
            if not sym.is_argument:
                continue;

            if len (sym.linkage_name):
                nsym, is_field_of_this = gdb.lookup_symbol (sym.linkage_name, block)
                if nsym.addr_class != gdb.SYMBOL_LOC_REGISTER:
                    sym = nsym
            try:
                val = sym.value(self.frame)
                if val != None:
                    val = val
                else:
                    val="???"
            except RuntimeError, text:
                val = text

            atuple = (sym.print_name, val)
            args.append (atuple)

        return args

    def line (self):
        sal = self.frame.find_sal()
        if (sal):
            return sal.line        
        else:
            return "<unknown line>"


def register_frame_filters (frame, what, level, args):

#    if (frame.name() == "main"):
        x = Main_filter (frame, what, level, args)
        return x
