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

    def elided (self):
        if hasattr(self.base, "elided"):
            return self.base.elided()

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

        args = FrameVars (self.base)
        return args.fetch_frame_args()

    def frame_locals (self):
        if hasattr(self.base, "frame_locals"):
            return self.base.frame_locals()

        args = FrameVars (self.base)
        return args.fetch_frame_locals()

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

class BaseSymValueWrapper ():

    def __init__(self, symbol, value):
        self.sym = symbol
        self.val = value

    def value (self):
        return self.val

    def symbol (self):
        return self.sym

class FrameVars ():

    def __init__(self,frame):
        self.frame = frame

    def fetch_frame_locals (self):
        lvars = []
        block = self.frame.block()

        for sym in block:
            if sym.is_argument:
                continue;

            lvars.append(BaseSymValueWrapper(sym, self.get_value(sym,block)))

        if len(lvars) == 0:
            return None

        return iter (lvars)

    def fetch_frame_args (self):
        args = []
        block = self.frame.block()

        for sym in block:
            if not sym.is_argument:
                continue;

            args.append(BaseSymValueWrapper(sym,self.get_value (sym,block)))

        if len(args) == 0:
            return None

        return iter (args)

    def get_value (self, sym, block):
        if len (sym.linkage_name):
            nsym, is_field_of_this = gdb.lookup_symbol (sym.linkage_name, block)

            if nsym.addr_class != gdb.SYMBOL_LOC_REGISTER:
                sym = nsym

        try:
            val = sym.value (self.frame)

        except RuntimeError, text:
            val = text
        if val == None:
            val = "???"

        return val
