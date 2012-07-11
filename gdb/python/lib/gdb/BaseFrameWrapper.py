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
from gdb.FrameWrapper import FrameWrapper

class BaseFrameWrapper (FrameWrapper):
    "Base Frame Wrapper"

    # 'base' here can refer to a gdb.Frame or another frame like
    # object conforming to the interface of the FrameWrapper class.
    # As we can have frame wrappers wrapping frame wrappers, we should
    # defer to that object's method.
    def __init__(self, base):
        super(BaseFrameWrapper, self).__init__(base)
        self.base = base

    def elide (self):
        if hasattr(self.base, "elide"):
            return self.base.elide()

        return None

    def function (self):
        if hasattr(self.base, "function"):
            return str(self.base.function())

        fname = str (self.base.function())
        if (fname == ""):
            return None
        else:
            return fname

    def address (self):
        if hasattr(self.base, "address"):
            return self.base.address()

        return self.base.pc()

    def filename (self):
        if hasattr(self.base, "filename"):
            return self.base.filename()

        sal = self.base.find_sal()
        if (sal):
            return sal.symtab.filename
        else:
            return None

    def frame_args (self):
        if hasattr(self.base, "frame_args"):
            return self.base.frame_args()

        args = self.base.arguments()
        args_list = []
        if args != None:
            for arg in args:
                value = arg.value(self.base)
                args_list.append((arg, value))

        return args_list

    def frame_locals (self):
        if hasattr(self.base, "frame_locals"):
            return self.base.frame_locals()

        frame_locals = self.base.locals()
        frame_locals_list = []
        if frame_locals != None:
            for frame_local in frame_locals:
                value = frame_local.value(self.base)
                frame_locals_list.append((frame_local, value))

        return frame_locals_list

    def line (self):
        if hasattr(self.base, "line"):
            return self.base.line()

        sal = self.base.find_sal()
        if (sal):
            return sal.line        
        else:
            return None

    def inferior_frame (self):
        if hasattr(self.base, "inferior_frame"):
            return self.base.inferior_frame()

        return self.base
