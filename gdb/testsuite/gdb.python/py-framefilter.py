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
from gdb.BaseFrameWrapper import BaseFrameWrapper
import copy

class Reverse_Function (BaseFrameWrapper):

    def __init__(self, fobj):
        super(Reverse_Function, self).__init__(fobj)
        self.fobj = fobj
 
    def function (self):
        fname = str (self.fobj.function())
        if (fname == None or fname == ""):
            return None
        else:
            fname = fname[::-1]
        return fname

class ElideFrames (BaseFrameWrapper):
    def __init__(self,fobj,fiter):
        super(ElideFrames, self).__init__(fobj)
        self.elide_iter = fiter

    def elide (self):
        return iter(self.elide_iter)


class Dummy (BaseFrameWrapper):

    def __init__(self, fobj):
        super(Dummy, self).__init__(fobj)
        self.fobj = fobj

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

    def elide (self):
        return None
    
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

class FrameElider ():

    def __init__ (self):
        self.name = "Elider"
        self.priority = 1000
        self.enabled = True
        gdb.frame_filters [self.name] = self

    def interp_b (self, it):
        fn = str(it.function())
        if fn == "func1":
            return True

        return False

    def elide_b (self, it):
        f = str(it.function())
        if f == "func2" or f == "func3":
            return True
        
        return False

    def elider (self,frame):
        if str(frame.function()) == "func1":
            return ElideFrames(frame, self.fr)
        else:
            return frame
        
    def filter (self, frame_iter):

        # First find if the functions we are interested in exist in
        # the iterable.  Copy the iterator pointer to two copies, also
        # over the reference of frame_iter so that will remain
        # uncorrupted.
        frame_iter, f1, f2 = itertools.tee(frame_iter,3)

        # Is there a function we are interested in?  If not just
        # return the iterator.
        check = itertools.ifilter (self.interp_b, f1)
        try:
            f = next(check)
        except StopIteration:
            return frame_iter
            
        # And are there frames that we want to elide into the previous
        # function?  If so we need to copy the frames, as later we
        # will filter them out of the main iterator.
        elide_iter = itertools.ifilter (self.elide_b, f2)
        self.fr = []
        for fri in elide_iter:
            self.fr.append (fri)

        # Add in a synthetic frame to the elided frames group
        self.fr.append (Dummy(f))
        
        frame_iter = itertools.imap (self.elider, frame_iter)

        # Filter out elided frames from main iterator
        frame_iter = itertools.ifilterfalse  (self.elide_b, frame_iter)
        return frame_iter

FrameFilter()
FrameElider()
