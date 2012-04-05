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

class TestFilter:
    "Testcase filter"

    def __init__ (self, frame, what, level, args):
        self.frame = frame
        self.what = what
        self.lvl = level
        self.args = args

    def __new__ (self):
        fname = str (self.frame.function())
        if fname == "main":
            return None
        else:
            return self

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
        args = self.frame.arguments()
        args_list = []
        if args != None:
            for arg in args:
                value = arg.value(self.frame)
                args_list.append((arg, value))

        return args_list

    def frame_locals (self):
        frame_locals = self.frame.locals()
        frame_locals_list = []
        if frame_locals != None:
            for frame_local in frame_locals:
                value = frame_local.value(self.frame)
                frame_locals_list.append((frame_local, value))

        return frame_locals_list

    def line (self):
        sal = self.frame.find_sal()
        if (sal):
            return sal.line        
        else:
            return "<unknown line>"

