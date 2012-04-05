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

class FrameFilter ():

    def __init__ (self, iterator, limit):
        self.iterator = iterator
        self.limit = limit

    def __new__ (self):
        return self;

    def invoke (self):
        iter = itertools.imap (Reverse_Function,
                               self.iterator)
        return iter
