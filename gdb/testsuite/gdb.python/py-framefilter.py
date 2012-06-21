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

    def function (self):
        fname = str (self.frame.function())
        if (fname == ""):
            return "<unknown function>"
        else:
            fname = fname[::-1]
        return fname

    def elide (self):
        fname = str (self.frame.function())
        if (fname == "func2" or fname == "func3"):
            return True
        else:
            return False

class Dummy:
    def __init__(self, nextframe):
        self.nextframe = nextframe

    def omit (self):
        return False

    def elide (self):
        return False

    def function (self):
        return "Dummy function"

    def address (self):
        return 0x123

    def filename (self):
        return "Dummy filename"

    def frame_args (self):
        return []

    def frame_locals (self):
        return []

    def line (self):
        return 0

    def older (self):
        return self.nextframe

    def inferior_frame (self):
        return None

class FrameAdd ():

    def __init__ (self):
        self.name = "Add"
        self.priority = 10
        self.enabled = True
        gdb.frame_filters [self.name] = self

    def filter (self, frame_iter):
        for f in frame_iter:
            nextv = f
            break
        dummies = [Dummy(nextv)]
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
