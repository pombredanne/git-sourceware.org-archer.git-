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

flevel = 0

class Main_filter:
    "Example main () filter"

    def __init__ (self, frame, what, level, args):
        self.frame = frame
        self.what = what
        self.lvl = level
        self.args = args
        

    def function (self):
        return str (self.frame.function())

    def level (self):
        global flevel
        rlevel = flevel
        flevel = flevel + 1
        return rlevel

    def address (self):
        return self.frame.pc()

    def filename (self):
        sal = self.frame.find_sal()
        if (sal):
            return sal.symtab.filename
        else:
            return "unknown"

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
