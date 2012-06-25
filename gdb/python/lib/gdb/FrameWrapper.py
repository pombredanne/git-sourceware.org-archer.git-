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

import gdb

class FrameWrapper (object):
    "Base Frame Wrapper"

    # frame her can refer to a gdb.Frame or another frame like object
    # conforming to the interface in this class.  As we can have frame
    # wrappers wrapping frame wrappers, we should defer to that
    # object's method.
    def __init__(self, frame):
        super(FrameWrapper, self).__init__()
        self.frame = frame

    def omit (self):
        return False

    def elide (self):
        return False

    def function (self):
        if hasattr(self.frame, "function"):
            return str(self.frame.function())

        fname = str (self.frame.function())
        if (fname == ""):
            return "<unknown function>"
        else:
            return fname

    def level (self, level):
        if hasattr(self.frame, "level"):
            return self.frame.level()

        return level

    def address (self):
        if hasattr(self.frame, "address"):
            return self.frame.address()

        return self.frame.pc()

    def filename (self):
        if hasattr(self.frame, "filename"):
            return self.frame.filename()

        sal = self.frame.find_sal()
        if (sal):
            return sal.symtab.filename
        else:
            return "<unknown filename>"

    def frame_args (self):
        if hasattr(self.frame, "frame_args"):
            return self.frame.frame_args()

        args = self.frame.arguments()
        args_list = []
        if args != None:
            for arg in args:
                value = arg.value(self.frame)
                args_list.append((arg, value))

        return args_list

    def frame_locals (self):
        if hasattr(self.frame, "frame_locals"):
            return self.frame.frame_locals()

        frame_locals = self.frame.locals()
        frame_locals_list = []
        if frame_locals != None:
            for frame_local in frame_locals:
                value = frame_local.value(self.frame)
                frame_locals_list.append((frame_local, value))

        return frame_locals_list

    def line (self):
        if hasattr(self.frame, "line"):
            return self.frame.line()

        sal = self.frame.find_sal()
        if (sal):
            return sal.line        
        else:
            return "<unknown line>"

    def inferior_frame (self):
        if hasattr(self.frame, "inferior_frame"):
            return self.frame.inferior_frame()

        return self.frame

    def older(self):
        return self.frame.older()
