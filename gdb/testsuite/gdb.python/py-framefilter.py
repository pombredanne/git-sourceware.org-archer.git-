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
import gdb
import itertools
from gdb.FrameWrapper import FrameWrapper


class Reverse_Function (FrameWrapper):

    def __init__(self, fobj):
        super(Reverse_Function, self).__init__(fobj)
        self.fobj = fobj
 
    def function (self):
        fname = str (self.fobj.function())
        if (fname == ""):
            return "<unknown function>"
        else:
            fname = fname[::-1]
        return fname

    def elide (self):
        fname = str (self.fobj.function())
        if (fname == "func2" or fname == "func3"):
            return True
        else:
            return False

class Dummy ():

    def function (self):
        return "Dummy function"

    def address (self):
        return 0x123

    def filename (self):
        return "Dummy filename"

    def frame_args (self):
        return [("Foo",gdb.Value(12)),("Bar","Stuff"), ("FooBar",42)]

    def frame_locals (self):
        return []

    def line (self):
        return 0

    def inferior_frame (self):
        return None

class FrameAdd ():

    def __init__ (self):
        self.name = "Dummy"
        self.priority = 10
        self.enabled = True
        gdb.frame_filters [self.name] = self

    def filter (self, frame_iter):
        dummies = [Dummy()]
        frame_iter = itertools.chain (dummies,
                                      frame_iter)
        return frame_iter

class FrameFilter ():

    def __init__ (self):
        self.name = "Reverse"
        self.priority = 100
        self.enabled = True
        gdb.frame_filters [self.name] = self

    def filter (self, frame_iter):
        frame_iter = itertools.imap (Reverse_Function,
                                     frame_iter)
        return frame_iter

FrameAdd()
FrameFilter()
